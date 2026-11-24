/* rodaja.cl — barra de auth (Explorador Shiny) */
(function (global) {
  "use strict";

  var favoritesSynced = false;
  var authBootDone = false;
  var authListenerRegistered = false;

  function escapeHtml(s) {
    return String(s)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function tierLabel(tier) {
    var t = String(tier || "gratuito").toLowerCase();
    if (t === "enterprise") return "Institucional";
    if (t === "pro" || t === "premium") return "Pro";
    if (t === "gratuito") return "Gratuito";
    return "Gratuito";
  }

  function tierCssClass(tier) {
    var t = String(tier || "gratuito").toLowerCase();
    if (t === "enterprise") return "ec-auth-tier ec-auth-tier--enterprise";
    if (t === "pro" || t === "premium") return "ec-auth-tier ec-auth-tier--premium";
    return "ec-auth-tier ec-auth-tier--gratuito";
  }

  function isPaidTier(tier) {
    var t = String(tier || "gratuito").toLowerCase();
    return t === "pro" || t === "premium" || t === "enterprise";
  }

  function emailOf(user) {
    if (!user) return "";
    return (user.primaryEmailAddress && user.primaryEmailAddress.emailAddress) ||
      (user.emailAddresses && user.emailAddresses[0] && user.emailAddresses[0].emailAddress) ||
      "";
  }

  function toolsHost() {
    return document.getElementById("ec-map-tools") || document.getElementById("ec-map-toolbar");
  }

  function applyTierBadge(bar, tier) {
    if (!bar) return;
    var t = String(tier || "gratuito").toLowerCase();
    var tierEl = bar.querySelector(".ec-plan-name") || bar.querySelector(".ec-auth-tier");
    if (tierEl) {
      tierEl.textContent = tierLabel(t);
      if (tierEl.classList.contains("ec-auth-tier")) tierEl.className = tierCssClass(t);
    }
    var up = bar.querySelector(".ec-plan-upgrade");
    if (up) up.style.display = isPaidTier(t) ? "none" : "";
  }

  function loadUserSubscription(bar) {
    if (!bar || typeof rodajaApiFetch !== "function") return;
    rodajaApiFetch("/me").then(function (r) {
      return r.ok ? r.json() : null;
    }).then(function (me) {
      if (me) applyTierBadge(bar, me.tier || me.tipo_usuario || "gratuito");
    });
    loadQuotaIntoBar(bar);
  }

  function formatResetDate(iso) {
    if (!iso) return "";
    try {
      return new Date(iso + "T12:00:00").toLocaleDateString("es-CL", { day: "numeric", month: "short" });
    } catch (e) {
      return iso;
    }
  }

  function formatBytes(n) {
    n = Number(n);
    if (!n || isNaN(n) || n < 0) return "0 B";
    var units = ["B", "KB", "MB", "GB"];
    var i = 0;
    var v = n;
    while (v >= 1024 && i < units.length - 1) {
      v /= 1024;
      i++;
    }
    var decimals = i > 0 && v < 10 ? 1 : 0;
    return v.toFixed(decimals) + " " + units[i];
  }

  function quotaBarHtml(cuota) {
    if (!cuota) return "";
    var usado = cuota.bytes_usados || 0;
    var limite = cuota.limite_bytes || (100 * 1024 * 1024);
    var pct = limite > 0 ? Math.min(100, (usado / limite) * 100) : 0;
    var fillClass = "ec-auth-quota-fill";
    if (pct >= 100) fillClass += " ec-auth-quota-over";
    else if (pct >= 80) fillClass += " ec-auth-quota-warn";
    return (
      '<span class="ec-auth-quota-wrap" title="L\u00edmite de descarga: ' +
        formatBytes(usado) + " / " + formatBytes(limite) +
        " \u00b7 se renueva el " + formatResetDate(cuota.reset_en) +
      '">' +
        '<span class="ec-auth-quota-text">' +
          formatBytes(usado) + " / " + formatBytes(limite) +
        "</span>" +
        '<span class="ec-auth-quota-track">' +
          '<span class="' + fillClass + '" style="width:' + pct.toFixed(1) + '%"></span>' +
        "</span>" +
      "</span>"
    );
  }

  function loadQuotaIntoBar(bar) {
    if (!bar || typeof rodajaApiFetch !== "function") return;
    var user = typeof rodajaGetCurrentUser === "function" ? rodajaGetCurrentUser() : null;
    var userEmail = user && ((user.primaryEmailAddress && user.primaryEmailAddress.emailAddress) ||
      (user.emailAddresses && user.emailAddresses[0] && user.emailAddresses[0].emailAddress) ||
      "");
    var cuotaPath = "/me/cuota";
    if (userEmail) cuotaPath += "?" + new URLSearchParams({ email: userEmail }).toString();
    rodajaApiFetch(cuotaPath).then(function (r) {
      return r.ok ? r.json() : null;
    }).then(function (cuota) {
      if (!cuota) return;
      var wrap = bar.querySelector(".ec-auth-quota-wrap");
      if (wrap) wrap.outerHTML = quotaBarHtml(cuota);
      else {
        var slot = bar.querySelector(".ec-auth-quota-slot");
        if (slot) slot.innerHTML = quotaBarHtml(cuota);
        else {
          var status = bar.querySelector(".ec-auth-status");
          if (status) status.insertAdjacentHTML("beforeend", quotaBarHtml(cuota));
        }
      }
      var tierEl = bar.querySelector(".ec-auth-tier");
      if (tierEl) applyTierBadge(bar, cuota.tier || "");
      attachFavoritesToAuthBar(toolsHost());
    });
  }

  function displayUserName(user) {
    if (!user) return "Mi cuenta";
    if (user.fullName) return user.fullName;
    var first = user.firstName || "";
    var last = user.lastName || "";
    var combined = (first + " " + last).trim();
    if (combined) return combined;
    if (user.username) return user.username;
    var email = (user.primaryEmailAddress && user.primaryEmailAddress.emailAddress) ||
      (user.emailAddresses && user.emailAddresses[0] && user.emailAddresses[0].emailAddress) ||
      "";
    if (email) {
      var at = email.indexOf("@");
      if (at > 0) return email.substring(0, at);
      return email;
    }
    return "Mi cuenta";
  }

  function syncTokenToShiny(token) {
    if (typeof Shiny === "undefined" || !Shiny.setInputValue) return;
    Shiny.setInputValue("ec_auth_token", token || "", { priority: "event" });
  }

  function mergeCloudFavorites(cloud) {
    if (!cloud || typeof cloud !== "object") return;
    var local = typeof rodajaGetFavorites === "function" ? rodajaGetFavorites() : { primary: null, recents: [] };
    var hasLocal = !!(local.primary || (local.recents && local.recents.length));
    if (hasLocal && !favoritesSynced && typeof rodajaSyncFavoritesToCloud === "function") {
      rodajaSyncFavoritesToCloud(local).then(function (ok) {
        if (ok) favoritesSynced = true;
      });
      return;
    }
    if (!cloud.primary && !(cloud.recents && cloud.recents.length)) return;
    if (hasLocal) return;
    try {
      var next = { primary: cloud.primary || null, recents: cloud.recents || [] };
      if (typeof rodajaSaveFavorites === "function") {
        rodajaSaveFavorites(next);
      } else {
        localStorage.setItem(
          global.EC_FAVORITES_STORAGE_KEY || "rodaja_favorites_v1",
          JSON.stringify(next)
        );
        if (next.primary && next.primary.eid) {
          localStorage.setItem("rodaja_app_reporte_v1", JSON.stringify({
            eid: next.primary.eid,
            variableId: next.primary.variableId || null,
            nombre: next.primary.alias || ""
          }));
        }
        if (typeof global.dispatchEvent === "function") {
          global.dispatchEvent(new CustomEvent("rodaja:favorites-changed", { detail: cloud }));
        }
      }
      if (typeof ecInitFavoritesUi === "function") ecInitFavoritesUi({ sync: true });
    } catch (e) { /* ignore */ }
  }

  function createFavGroup() {
    var group = document.createElement("div");
    group.className = "btn-group ec-nav-fav-group";
    group.innerHTML =
      '<button type="button" id="ec-btn-mi-estacion" class="btn btn-default ec-btn-mi-estacion" ' +
        'title="Mi estaci\u00f3n favorita" onclick="return ecNavIrMiEstacion();">' +
        '<i class="fa fa-star" role="presentation" aria-label="star icon"></i>' +
        '<span class="ec-btn-mi-estacion-label"> Mi estaci\u00f3n</span>' +
      "</button>";
    return group;
  }

  function resolveFavGroup(preserved) {
    if (preserved && preserved.querySelector && preserved.querySelector("#ec-btn-mi-estacion")) {
      return preserved;
    }
    var inBar = document.querySelector("#ec-map-tools .ec-nav-fav-group") ||
      document.querySelector("#ec-auth-bar .ec-nav-fav-group");
    if (inBar) return inBar;
    var btn = document.getElementById("ec-btn-mi-estacion");
    if (btn) {
      var fromBtn = btn.closest ? btn.closest(".ec-nav-fav-group") : null;
      if (fromBtn) return fromBtn;
    }
    var any = document.querySelector(".ec-nav-fav-group");
    if (any) return any;
    return createFavGroup();
  }

  function dedupeFavGroups(keep) {
    document.querySelectorAll(".ec-nav-fav-group").forEach(function (group) {
      if (group !== keep && group.parentNode) group.parentNode.removeChild(group);
    });
    document.querySelectorAll("li.ec-nav-fav-item").forEach(function (li) {
      if (!li.querySelector(".ec-nav-fav-group") && li.parentNode) li.parentNode.removeChild(li);
    });
  }

  function detachFavGroup(favGroup) {
    if (favGroup && favGroup.parentNode) favGroup.parentNode.removeChild(favGroup);
    return favGroup;
  }

  function onAuthChanged(user, token) {
    syncTokenToShiny(token);
    attachMapTools();
    if (typeof ecAlertasOnAuthChange === "function") {
      ecAlertasOnAuthChange(user, token);
    }
    if (token && typeof rodajaLoadFavoritesFromCloud === "function") {
      rodajaLoadFavoritesFromCloud().then(mergeCloudFavorites);
    }
  }

  function attachMapTools(preservedFavGroup) {
    var tools = toolsHost();
    attachFavoritesToAuthBar(tools, preservedFavGroup);
  }

  function attachFavoritesToAuthBar(bar, preservedFavGroup, attempt) {
    attempt = attempt || 0;
    if (!bar) bar = toolsHost();
    if (bar && bar.id === "ec-auth-bar") bar = toolsHost();
    if (!bar) return;

    var favGroup = resolveFavGroup(preservedFavGroup);
    detachFavGroup(favGroup);
    dedupeFavGroups(favGroup);

    var slot = bar.querySelector(".ec-auth-fav-slot");
    if (slot) slot.appendChild(favGroup);
    else if (attempt < 40) {
      clearTimeout(bar._favAttachT);
      bar._favAttachT = setTimeout(function () {
        attachFavoritesToAuthBar(bar, favGroup, attempt + 1);
      }, 100);
      return;
    } else {
      var anchor = bar.querySelector(".ec-auth-logout") || bar.querySelector(".ec-auth-signup");
      if (anchor) anchor.insertAdjacentElement("afterend", favGroup);
      else bar.insertBefore(favGroup, bar.firstChild);
    }

    if (typeof ecInitFavoritesUi === "function") ecInitFavoritesUi({});

    if (!bar.querySelector("#ec-btn-mi-estacion") && attempt < 40) {
      clearTimeout(bar._favVerifyT);
      bar._favVerifyT = setTimeout(function () {
        attachFavoritesToAuthBar(bar, favGroup, attempt + 1);
      }, 100);
    }
  }
  function syncTokenNow() {
    if (typeof rodajaGetAuthToken !== "function") return;
    rodajaGetAuthToken().then(function (token) {
      syncTokenToShiny(token);
    });
  }

  function boot() {
    var bar = document.getElementById("ec-auth-bar");
    if (bar && typeof rodajaAuthEnabled === "function" && !rodajaAuthEnabled()) {
      bar.style.display = "none";
    }
    if (typeof rodajaMountInstallButton === "function") rodajaMountInstallButton();
    if (!authListenerRegistered && typeof rodajaOnAuthChange === "function") {
      authListenerRegistered = true;
      rodajaOnAuthChange(onAuthChanged);
    }
    if (authBootDone) {
      attachMapTools();
      syncTokenNow();
      return;
    }
    authBootDone = true;
    attachMapTools();
    if (typeof rodajaInitAuth === "function") {
      rodajaInitAuth().then(function () {
        return rodajaGetAuthToken();
      }).then(function (token) {
        onAuthChanged(rodajaGetCurrentUser(), token);
      });
    }
    document.addEventListener("visibilitychange", function () {
      if (document.visibilityState === "visible") attachMapTools();
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    boot();
  }

  if (typeof jQuery !== "undefined") {
    jQuery(document).on("shiny:connected", boot);
    jQuery(document).on("shiny:connected", function () {
      attachMapTools();
    });
  } else {
    document.addEventListener("shiny:connected", boot);
    document.addEventListener("shiny:connected", function () {
      attachMapTools();
    });
  }

  global.addEventListener("rodaja:favorites-changed", function () {
    attachMapTools();
    if (typeof ecInitFavoritesUi === "function") ecInitFavoritesUi({});
  });
})(window);
