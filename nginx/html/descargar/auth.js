/* Auth + cuota header (PWA descargar) */
(function (global) {
  "use strict";

  function escapeHtml(s) {
    return String(s)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function formatDate(iso) {
    if (!iso) return "";
    try {
      var d = new Date(iso + "T12:00:00");
      return d.toLocaleDateString("es-CL", { day: "numeric", month: "short" });
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

  function renderQuotaBar(cuota) {
    var bar = document.getElementById("dl-quota-bar");
    if (!bar || !cuota) {
      if (bar) bar.hidden = true;
      return;
    }
    var usado = cuota.bytes_usados || 0;
    var limite = cuota.limite_bytes || (100 * 1024 * 1024);
    var pct = limite > 0 ? Math.min(100, Math.round((usado / limite) * 100)) : 0;
    var fillClass = "dl-quota-fill";
    if (pct >= 90) fillClass += " dl-quota-over";
    else if (pct >= 70) fillClass += " dl-quota-warn";
    bar.hidden = false;
    bar.innerHTML =
      '<span class="dl-quota-text">' + formatBytes(usado) + " / " + formatBytes(limite) +
      " \u00b7 se renueva el " + formatDate(cuota.reset_en) + "</span>" +
      '<div class="dl-quota-track"><div class="' + fillClass + '" style="width:' + pct + '%"></div></div>';
  }

  function renderAuthBar(user, token) {
    var bar = document.getElementById("dl-auth-bar");
    if (!bar) return;
    if (!rodajaAuthEnabled || !rodajaAuthEnabled()) {
      bar.innerHTML = "";
      return;
    }
    if (token && user) {
      var email = (user.primaryEmailAddress && user.primaryEmailAddress.emailAddress) ||
        (user.emailAddresses && user.emailAddresses[0] && user.emailAddresses[0].emailAddress) ||
        "Cuenta";
      bar.innerHTML =
        '<span class="dl-auth-email">' + escapeHtml(email) + "</span>" +
        '<button type="button" class="dl-link dl-auth-logout" style="border:none;background:none;cursor:pointer;">Salir</button>';
      bar.querySelector(".dl-auth-logout").addEventListener("click", function () {
        rodajaSignOut();
      });
      if (typeof rodajaApiFetch === "function") {
        var userEmail = (user.primaryEmailAddress && user.primaryEmailAddress.emailAddress) ||
          (user.emailAddresses && user.emailAddresses[0] && user.emailAddresses[0].emailAddress) ||
          "";
        var cuotaPath = "/me/cuota";
        if (userEmail) cuotaPath += "?" + new URLSearchParams({ email: userEmail }).toString();
        rodajaApiFetch(cuotaPath).then(function (r) {
          return r.ok ? r.json() : null;
        }).then(renderQuotaBar);
      }
      return;
    }
    bar.innerHTML =
      '<a class="dl-link" href="#" data-rodaja-signin>Iniciar sesi\u00f3n</a>';
    renderQuotaBar(null);
  }

  global.dlRenderAuthBar = renderAuthBar;
  global.dlRenderQuotaBar = renderQuotaBar;

  function boot() {
    if (typeof rodajaOnAuthChange === "function") {
      rodajaOnAuthChange(renderAuthBar);
    }
    if (typeof rodajaInitAuth === "function") {
      rodajaInitAuth().then(function () {
        return rodajaGetAuthToken();
      }).then(function (token) {
        renderAuthBar(rodajaGetCurrentUser(), token);
      });
    }
    document.addEventListener("visibilitychange", function () {
      if (document.visibilityState === "visible" && rodajaIsSignedIn && rodajaIsSignedIn()) {
        rodajaApiFetch("/me/cuota").then(function (r) {
          return r.ok ? r.json() : null;
        }).then(renderQuotaBar);
      }
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    boot();
  }
})(window);
