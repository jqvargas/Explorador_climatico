/* rodaja.cl â€” alertas top 10 (Explorador navbar) */
(function (global) {
  "use strict";

  var state = {
    tier: "gratuito",
    maxSubs: 1,
    email: "",
    abono: null,
    signedIn: false
  };
  var loadPromise = null;

  function openPanelUrl(opts) {
    opts = opts || {};
    try {
      sessionStorage.setItem("rodaja_alertas_handoff", JSON.stringify(opts));
    } catch (e) { /* ignore */ }
    var url = panelUrl();
    if (opts.scope) url += "?scope=" + encodeURIComponent(opts.scope);
    else if (opts.eid) url += "?e=" + encodeURIComponent(opts.eid);
    var w = global.open(url, "_blank");
    if (!w) global.location.href = url;
  }

  function esc(s) {
    var d = document.createElement("div");
    d.textContent = s == null ? "" : String(s);
    return d.innerHTML;
  }

  function tierLabel(tier) {
    if (tier === "pro") return "Pro";
    if (tier === "enterprise") return "Institucional";
    return "Gratuito";
  }

  function toast(msg, ok) {
    if (typeof global.rodajaToast === "function") global.rodajaToast(msg, ok);
  }

  function panelUrl() {
    return "/alertas/";
  }

  function openPanel(opts) { openPanelUrl(opts || {}); }

  function createAlertasWrap() {
    var wrap = document.createElement("div");
    wrap.className = "ec-nav-alertas-wrap";
    wrap.innerHTML =
      '<button type="button" id="ec-btn-alertas" class="btn btn-default ec-btn-alertas" ' +
        'title="Panel de alertas" aria-label="Abrir panel de alertas">' +
        '<i class="fa fa-bell" role="presentation" aria-label="alertas"></i>' +
        '<span class="ec-btn-alertas-label">Alertas</span>' +
        '<span id="ec-alertas-active-dot" class="ec-alertas-active-dot" hidden aria-hidden="true"></span>' +
      "</button>" +
      '<div id="ec-nav-alertas-panel" class="ec-nav-alertas-panel" role="dialog" aria-label="Alertas por correo">' +
        '<div class="ec-nav-alertas-header"><strong>Alertas</strong></div>' +
        '<p class="ec-alertas-desc">Te avisamos por correo cuando el \u00faltimo dato del d\u00eda est\u00e1 entre los 10 m\u00e1s extremos de esa estaci\u00f3n.</p>' +
        '<p id="ec-alertas-email" class="ec-alertas-email"></p>' +
        '<p id="ec-alertas-guest" class="ec-alertas-guest" hidden>' +
          'Inicia sesión para activar alertas. Usamos el correo de tu cuenta.' +
        "</p>" +
        '<div id="ec-alertas-toggles" class="ec-alertas-toggles" hidden>' +
          '<div class="ec-alertas-row">' +
            '<div class="ec-alertas-row-text">' +
              '<strong>Estación favorita</strong>' +
              '<span id="ec-alertas-fav-hint" class="ec-alertas-hint"></span>' +
            "</div>" +
            '<label class="ec-alertas-switch">' +
              '<input type="checkbox" id="ec-toggle-alerta-favorita" />' +
              '<span class="ec-alertas-slider"></span>' +
            "</label>" +
          "</div>" +
          '<div class="ec-alertas-row">' +
            '<div class="ec-alertas-row-text">' +
              '<strong>Mi grupo</strong>' +
              '<span id="ec-alertas-grupo-hint" class="ec-alertas-hint"></span>' +
            "</div>" +
            '<label class="ec-alertas-switch">' +
              '<input type="checkbox" id="ec-toggle-alerta-grupo" />' +
              '<span class="ec-alertas-slider"></span>' +
            "</label>" +
          "</div>" +
          '<p id="ec-alertas-tier-note" class="ec-alertas-tier-note"></p>' +
          '<p id="ec-alertas-status" class="ec-alertas-status" role="status"></p>' +
          '<a id="ec-alertas-historial" class="ec-alertas-historial" href="' + esc(panelUrl()) + '" target="_blank" rel="noopener">Abrir panel de alertas</a>' +
        "</div>" +
        '<div id="ec-alertas-guest-actions" class="ec-alertas-guest-actions" hidden>' +
          '<a class="btn btn-sm btn-primary ec-alertas-login" href="#">Iniciar sesión</a>' +
        "</div>" +
      "</div>";
    return wrap;
  }

  function resolveAlertasWrap(preserved) {
    if (preserved && preserved.querySelector && preserved.querySelector("#ec-btn-alertas") &&
        preserved.querySelector("#ec-nav-alertas-panel")) {
      return preserved;
    }
    var inBar = document.querySelector("#ec-map-tools .ec-nav-alertas-wrap") ||
      document.querySelector("#ec-auth-bar .ec-nav-alertas-wrap");
    if (inBar && !inBar.querySelector("#ec-nav-alertas-panel")) {
      if (inBar.parentNode) inBar.parentNode.removeChild(inBar);
      inBar = null;
    }
    if (inBar) return inBar;
    var btn = document.getElementById("ec-btn-alertas");
    if (btn && btn.closest) {
      var fromBtn = btn.closest(".ec-nav-alertas-wrap");
      if (fromBtn) return fromBtn;
    }
    return createAlertasWrap();
  }

  function detachAlertasWrap(wrap) {
    if (wrap && wrap.parentNode) wrap.parentNode.removeChild(wrap);
    return wrap;
  }

  function setStatus(msg, ok) {
    var el = document.getElementById("ec-alertas-status");
    if (!el) return;
    el.textContent = msg || "";
    el.className = "ec-alertas-status" + (msg ? (ok ? " ok" : " err") : "");
  }

  function updateBellBadge() {
    var dot = document.getElementById("ec-alertas-active-dot");
    var btn = document.getElementById("ec-btn-alertas");
    if (!dot || !btn) return;
    var fav = state.abono && state.abono.favorita;
    var grp = state.abono && state.abono.grupo;
    var active = !!(fav && fav.activa) || !!(grp && grp.activa);
    dot.hidden = !active;
    btn.classList.toggle("ec-alertas-on", active);
  }

  function updateToggleStates() {
    var fav = state.abono && state.abono.favorita;
    var grp = state.abono && state.abono.grupo;
    var favCb = document.getElementById("ec-toggle-alerta-favorita");
    var grpCb = document.getElementById("ec-toggle-alerta-grupo");
    var inlineGrp = document.getElementById("ec-toggle-alerta-grupo-inline");
    var favOn = !!(fav && fav.activa);
    var grpOn = !!(grp && grp.activa);
    var favOk = !!(fav && fav.scope_configurado);
    var grpOk = !!(grp && grp.scope_configurado);

    if (favCb) {
      favCb.checked = favOn;
      favCb.disabled = !state.signedIn || !favOk;
    }
    if (grpCb) {
      grpCb.checked = grpOn;
      grpCb.disabled = !state.signedIn || !grpOk;
    }
    if (inlineGrp) {
      inlineGrp.checked = grpOn;
      inlineGrp.disabled = !state.signedIn || !grpOk;
    }

    var favHint = document.getElementById("ec-alertas-fav-hint");
    var grpHint = document.getElementById("ec-alertas-grupo-hint");
    if (favHint) {
      favHint.textContent = favOk
        ? "Tu estación favorita guardada."
        : "Fija una estación favorita en el mapa.";
    }
    if (grpHint) {
      grpHint.textContent = grpOk
        ? "Todas las estaciones de tu grupo."
        : "Agrega estaciones al grupo desde el mapa.";
    }

    var maxSubs = state.maxSubs;
    if (maxSubs != null && maxSubs === 1) {
      if (favOn && grpOk) {
        if (grpCb) grpCb.disabled = true;
        if (inlineGrp) inlineGrp.disabled = true;
      }
      if (grpOn && favOk && favCb) favCb.disabled = true;
    }

    var note = document.getElementById("ec-alertas-tier-note");
    if (note) {
      if (maxSubs != null && maxSubs === 1) {
        note.textContent = "Plan " + tierLabel(state.tier) + ": 1 alerta activa (favorita o grupo).";
      } else {
        note.textContent = "Plan " + tierLabel(state.tier) + ": alertas ilimitadas.";
      }
    }

    updateBellBadge();
  }

  function updateGuestPanel() {
    var emailEl = document.getElementById("ec-alertas-email");
    var guestEl = document.getElementById("ec-alertas-guest");
    var toggles = document.getElementById("ec-alertas-toggles");
    var guestActions = document.getElementById("ec-alertas-guest-actions");
    var login = document.querySelector(".ec-alertas-login");

    if (state.signedIn) {
      if (emailEl) {
        emailEl.textContent = state.email
          ? "Correo: " + state.email
          : "Correo de tu cuenta registrada.";
        emailEl.hidden = false;
      }
      if (guestEl) guestEl.hidden = true;
      if (toggles) toggles.hidden = false;
      if (guestActions) guestActions.hidden = true;
    } else {
      if (emailEl) emailEl.hidden = true;
      if (guestEl) guestEl.hidden = false;
      if (toggles) toggles.hidden = true;
      if (guestActions) guestActions.hidden = false;
      if (login && typeof global.rodajaSignInUrl === "function") {
        login.href = global.rodajaSignInUrl(global.location.href);
      }
    }
    updateToggleStates();
  }

  function loadAlertas() {
    if (typeof global.rodajaApiFetch !== "function") return Promise.resolve();
    if (!global.rodajaIsSignedIn || !global.rodajaIsSignedIn()) {
      state.signedIn = false;
      state.abono = null;
      updateGuestPanel();
      return Promise.resolve();
    }
    state.signedIn = true;
    if (loadPromise) return loadPromise;
    loadPromise = global.rodajaApiFetch("/me/alertas").then(function (r) {
      if (!r.ok) throw new Error("HTTP " + r.status);
      return r.json();
    }).then(function (data) {
      state.tier = data.tier || "gratuito";
      state.maxSubs = data.max_alertas_suscripcion;
      state.email = data.email || "";
      state.abono = data.abono || {};
      updateGuestPanel();
    }).catch(function () {
      setStatus("No se pudieron cargar las alertas.", false);
    }).finally(function () {
      loadPromise = null;
    });
    return loadPromise;
  }

  function ensureScopeSynced(scopeType) {
    if (scopeType === "favorita") {
      if (typeof global.rodajaSyncFavoritesToCloud === "function" &&
          typeof global.rodajaGetFavorites === "function") {
        return global.rodajaSyncFavoritesToCloud(global.rodajaGetFavorites()).then(function (ok) {
          if (!ok) throw new Error("No se pudo sincronizar tu favorita. Reintenta.");
          return ok;
        });
      }
    } else if (scopeType === "grupo") {
      if (typeof global.rodajaSyncStationGroupToCloud === "function" &&
          typeof global.rodajaGetStationGroup === "function") {
        return global.rodajaSyncStationGroupToCloud(global.rodajaGetStationGroup()).then(function (ok) {
          if (!ok) throw new Error("No se pudo sincronizar tu grupo. Reintenta.");
          return ok;
        });
      }
    }
    return Promise.resolve(true);
  }

  function putAlerta(scopeType, activa) {
    setStatus("Guardando\u2026", true);
    var chain = activa ? ensureScopeSynced(scopeType) : Promise.resolve(true);
    return chain.then(function () {
      return global.rodajaApiFetch("/me/alertas", {
        method: "PUT",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ scope_type: scopeType, activa: activa })
      });
    }).then(function (r) {
      return r.json().then(function (body) {
        if (!r.ok) throw new Error((body && body.error) || ("HTTP " + r.status));
        return body;
      });
    }).then(function (body) {
      state.abono = body.abono || state.abono;
      updateToggleStates();
      var msg = activa ? "Alertas activadas." : "Alertas desactivadas.";
      setStatus(msg, true);
      toast(msg, true);
    }).catch(function (err) {
      var msg = err.message || "Error al guardar";
      setStatus(msg, false);
      toast(msg, false);
      return loadAlertas();
    });
  }

  function onToggle(scopeType, checked, sourceEl) {
    putAlerta(scopeType, checked).catch(function () {
      if (sourceEl) sourceEl.checked = !checked;
    });
  }

  function injectGrupoInlineToggle() {
    var panel = document.getElementById("ec-nav-grupo-panel");
    if (!panel || panel.querySelector("#ec-toggle-alerta-grupo-inline")) return;
    var actions = panel.querySelector(".ec-nav-grupo-actions");
    if (!actions) return;
    var row = document.createElement("div");
    row.className = "ec-nav-grupo-alertas";
    row.innerHTML =
      '<label class="ec-alertas-inline-row">' +
        '<span class="ec-alertas-inline-text">' +
          '<i class="fa fa-bell" aria-hidden="true"></i> Avisos del grupo por correo' +
        "</span>" +
        '<span class="ec-alertas-switch ec-alertas-switch--sm">' +
          '<input type="checkbox" id="ec-toggle-alerta-grupo-inline" />' +
          '<span class="ec-alertas-slider"></span>' +
        "</span>" +
      "</label>";
    panel.insertBefore(row, actions);
    var inline = document.getElementById("ec-toggle-alerta-grupo-inline");
    if (inline) {
      inline.addEventListener("change", function (e) {
        var main = document.getElementById("ec-toggle-alerta-grupo");
        if (main) main.checked = e.target.checked;
        onToggle("grupo", e.target.checked, e.target);
      });
    }
    updateToggleStates();
  }

  function wireAlertasUi() {
    var toggle = document.getElementById("ec-btn-alertas");
    var panel = document.getElementById("ec-nav-alertas-panel");

    if (toggle && toggle.getAttribute("data-ec-alertas-wired") === "1") {
      updateGuestPanel();
      injectGrupoInlineToggle();
      updateToggleStates();
      return;
    }
    if (toggle) toggle.setAttribute("data-ec-alertas-wired", "1");

    if (toggle && panel) {
      toggle.addEventListener("click", function (e) {
        e.preventDefault();
        e.stopPropagation();
        if (e.altKey) {
          var open = panel.classList.toggle("open");
          toggle.setAttribute("aria-expanded", open ? "true" : "false");
          if (open && state.signedIn) loadAlertas();
          return;
        }
        openPanel();
      });
      document.addEventListener("click", function (e) {
        if (!panel.contains(e.target) && e.target !== toggle && !toggle.contains(e.target)) {
          panel.classList.remove("open");
          toggle.setAttribute("aria-expanded", "false");
        }
      });
    }

    var favCb = document.getElementById("ec-toggle-alerta-favorita");
    var grpCb = document.getElementById("ec-toggle-alerta-grupo");
    if (favCb) {
      favCb.addEventListener("change", function (e) {
        onToggle("favorita", e.target.checked, e.target);
      });
    }
    if (grpCb) {
      grpCb.addEventListener("change", function (e) {
        var inline = document.getElementById("ec-toggle-alerta-grupo-inline");
        if (inline) inline.checked = e.target.checked;
        onToggle("grupo", e.target.checked, e.target);
      });
    }

    injectGrupoInlineToggle();
    updateGuestPanel();
  }

  function ecAttachAlertasToAuthBar(bar, preserved, attempt) {
    attempt = attempt || 0;
    if (!bar) bar = document.getElementById("ec-map-tools") || document.getElementById("ec-auth-bar");
    if (!bar) return;

    var wrap = resolveAlertasWrap(preserved);
    detachAlertasWrap(wrap);

    var slot = bar.querySelector(".ec-auth-alertas-slot");
    if (slot) {
      slot.appendChild(wrap);
    } else if (attempt < 40) {
      clearTimeout(bar._alertasAttachT);
      bar._alertasAttachT = setTimeout(function () {
        ecAttachAlertasToAuthBar(bar, wrap, attempt + 1);
      }, 100);
      return;
    } else {
      var favSlot = bar.querySelector(".ec-auth-fav-slot");
      if (favSlot && favSlot.parentNode) {
        favSlot.parentNode.insertBefore(wrap, favSlot.nextSibling);
      } else {
        bar.insertBefore(wrap, bar.firstChild);
      }
    }

    wireAlertasUi();
    loadAlertas();
  }

  function ecAlertasOnAuthChange(user, token) {
    state.signedIn = !!(token && user);
    if (!state.signedIn) {
      state.email = "";
      state.abono = null;
    }
    updateGuestPanel();
    injectGrupoInlineToggle();
    if (state.signedIn) loadAlertas();
  }

  global.ecAttachAlertasToAuthBar = ecAttachAlertasToAuthBar;
  global.ecAlertasOnAuthChange = ecAlertasOnAuthChange;
  global.ecAlertasReload = loadAlertas;

  global.rodajaOpenAlertasPanel = openPanelUrl;

  global.addEventListener("rodaja:favorites-changed", function () {
    if (state.signedIn) {
      ensureScopeSynced("favorita").finally(loadAlertas);
    } else {
      loadAlertas();
    }
  });

  global.addEventListener("rodaja:station-group-changed", function () {
    injectGrupoInlineToggle();
    if (state.signedIn) {
      ensureScopeSynced("grupo").finally(loadAlertas);
    } else {
      loadAlertas();
    }
  });

  if (typeof global.rodajaOnAuthChange === "function") {
    global.rodajaOnAuthChange(ecAlertasOnAuthChange);
  }
})(window);
