(function () {
  "use strict";

  var authReady = null;

  function $(id) { return document.getElementById(id); }

  function scalar(value) {
    return Array.isArray(value) ? value[0] : value;
  }

  function showBanner(text, kind) {
    var el = $("pr-banner");
    if (!el) return;
    el.textContent = text;
    el.className = "pr-banner pr-banner-" + (kind || "info");
    el.hidden = false;
  }

  function isExternalFlowUrl(url) {
    try {
      var host = new URL(url, location.href).hostname.toLowerCase();
      return host === "www.flow.cl" || host === "sandbox.flow.cl" ||
        (host.length > 8 && host.slice(-8) === ".flow.cl");
    } catch (e) {
      return false;
    }
  }

  function showCheckoutBlocked(url) {
    var el = $("pr-banner");
    if (!el) return;
    el.className = "pr-banner pr-banner-warn";
    el.hidden = false;
    el.textContent = "";
    el.appendChild(document.createTextNode("El navegador bloqueó la ventana de pago. "));
    var a = document.createElement("a");
    a.href = url;
    a.target = "_blank";
    a.rel = "noopener noreferrer";
    a.textContent = "Abrir Flow en una pestaña nueva";
    el.appendChild(a);
  }

  // ponytail: TWA _blank → CCT/Chrome con barra si flow.cl NO es extra origin (techo: popup blocker; upgrade = tap del link, nunca extra origin Flow).
  function openCheckoutUrl(url, btn) {
    if (!url) return false;
    if (!isExternalFlowUrl(url)) {
      window.location.assign(url);
      return true;
    }
    var w = window.open(url, "_blank", "noopener,noreferrer");
    if (btn) {
      btn.disabled = false;
      btn.textContent = btn.getAttribute("data-label") || btn.textContent;
    }
    if (w) return true;
    showCheckoutBlocked(url);
    return false;
  }

  function checkoutReturnUrl() {
    return location.origin + location.pathname + "?checkout=pro";
  }

  function signInTarget() {
    return typeof rodajaSignInUrl === "function"
      ? rodajaSignInUrl(checkoutReturnUrl())
      : "/cuenta/";
  }

  function ensureAuthReady() {
    if (authReady) return authReady;
    if (typeof rodajaInitAuth !== "function") {
      authReady = Promise.resolve();
      return authReady;
    }
    authReady = rodajaInitAuth().catch(function () {
      authReady = null;
    });
    return authReady;
  }

  function refreshAuth() {
    if (typeof rodajaRefreshAuth === "function") {
      return rodajaRefreshAuth().catch(function () { return null; });
    }
    return Promise.resolve(null);
  }

  function isSignedIn() {
    return typeof rodajaIsSignedIn === "function" && rodajaIsSignedIn();
  }

  function waitForSignedIn(maxMs) {
    var deadline = Date.now() + (maxMs || 8000);

    function attempt() {
      if (isSignedIn()) return Promise.resolve(true);
      if (Date.now() >= deadline) return Promise.resolve(false);
      return refreshAuth().then(function () {
        if (isSignedIn()) return true;
        return new Promise(function (resolve) {
          setTimeout(resolve, 250);
        }).then(attempt);
      });
    }

    return ensureAuthReady().then(attempt);
  }

  function renderAuthBar() {
    var bar = $("pr-auth-bar");
    if (!bar || typeof rodajaGetCurrentUser !== "function") return;
    var user = rodajaGetCurrentUser();
    if (!user) {
      bar.innerHTML = '<a href="' + signInTarget() + '">Iniciar sesión</a>';
      return;
    }
    var email = (user.primaryEmailAddress && user.primaryEmailAddress.emailAddress) ||
      (user.emailAddresses && user.emailAddresses[0] && user.emailAddresses[0].emailAddress) || "";
    bar.innerHTML = '<span style="font-size:13px;color:#8C9091">' + email + '</span> ' +
      '<button type="button" id="pr-signout" style="border:none;background:none;cursor:pointer;color:#394553;font-size:13px">Salir</button>';
    var btn = document.getElementById("pr-signout");
    if (btn) btn.addEventListener("click", function () { rodajaSignOut(); });
  }

  function runCheckout(btn) {
    if (typeof rodajaTrack === "function") {
      rodajaTrack("begin_checkout", { plan: "pro", currency: "CLP", value: 9990 });
    }
    if (typeof rodajaApiFetch !== "function") {
      btn.disabled = false;
      btn.textContent = btn.getAttribute("data-label") || "Suscribirse";
      showBanner("Autenticación no disponible. Recarga la página.", "warn");
      return;
    }

    btn.textContent = "Redirigiendo al pago…";
    rodajaApiFetch("/billing/checkout", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ plan: "pro" })
    }).then(function (r) {
      return r.text().then(function (text) {
        var body = null;
        if (text) {
          try { body = JSON.parse(text); } catch (e) { /* ignore */ }
        }
        if (!r.ok) {
          var errVal = body && (body.error || body.message);
          var msg = scalar(errVal) || ("Error " + r.status);
          throw new Error(msg);
        }
        return body;
      });
    }).then(function (data) {
      var url = scalar(data && data.url);
      if (url) {
        openCheckoutUrl(url, btn);
        return;
      }
      throw new Error("Flow no devolvió URL de checkout");
    }).catch(function (err) {
      btn.disabled = false;
      btn.textContent = btn.getAttribute("data-label") || "Suscribirse";
      showBanner(err.message || "No se pudo iniciar el pago.", "warn");
    });
  }

  function startCheckout(btn) {
    if (!btn || btn.disabled) return;
    var original = btn.textContent;
    btn.setAttribute("data-label", original);
    btn.disabled = true;
    btn.textContent = "Verificando sesión…";

    waitForSignedIn(8000).then(function (signedIn) {
      if (!signedIn) {
        window.location.assign(signInTarget());
        return;
      }
      runCheckout(btn);
    }).catch(function () {
      btn.disabled = false;
      btn.textContent = original;
      showBanner("No se pudo verificar la sesión. Usa «Iniciar sesión» arriba e intenta de nuevo.", "warn");
    });
  }

  function wireButtons() {
    var gratuito = $("pr-btn-gratuito");
    if (gratuito) {
      gratuito.addEventListener("click", function () {
        waitForSignedIn(5000).then(function (signedIn) {
          if (signedIn) {
            location.href = "/explorador/";
            return;
          }
          location.href = typeof rodajaSignUpUrl === "function"
            ? rodajaSignUpUrl(location.href)
            : signInTarget();
        });
      });
    }

    var pro = $("pr-btn-pro");
    if (pro) pro.addEventListener("click", function () { startCheckout(pro); });

    var toggle = $("pr-compare-toggle");
    var panel = $("pr-compare-panel");
    var label = $("pr-compare-label");
    if (toggle && panel) {
      toggle.addEventListener("click", function () {
        var open = toggle.getAttribute("aria-expanded") === "true";
        toggle.setAttribute("aria-expanded", open ? "false" : "true");
        panel.hidden = open;
        if (label) label.textContent = open ? "Ver comparación completa" : "Ocultar comparación";
      });
    }
  }

  function handleQueryBanner() {
    var params = new URLSearchParams(location.search);
    if (params.get("checkout") === "cancel") {
      showBanner("Pago cancelado. Puedes intentar de nuevo cuando quieras.", "warn");
    }
  }

  function maybeAutoCheckoutAfterSignIn() {
    var params = new URLSearchParams(location.search);
    if (params.get("checkout") !== "pro") return;
    var pro = $("pr-btn-pro");
    if (!pro) return;

    waitForSignedIn(12000).then(function (signedIn) {
      if (!signedIn) {
        showBanner("Inicia sesión para continuar con la suscripción Pro.", "warn");
        return;
      }
      if (history.replaceState) {
        history.replaceState({}, "", location.pathname);
      }
      startCheckout(pro);
    });
  }

  function loadBillingConfig() {
    if (typeof rodajaApiFetch !== "function") return;
    rodajaApiFetch("/billing/config").then(function (r) {
      return r.ok ? r.json() : null;
    }).then(function (cfg) {
      if (!cfg) return;
      var note = $("pr-pro-note");
      if (note && !scalar(cfg.checkout_enabled)) {
        note.hidden = false;
        note.textContent = "Checkout en configuración. Falta FLOW_API_KEY y FLOW_SECRET_KEY en el servidor.";
      }
    }).catch(function () { /* ignore */ });
  }

  function boot() {
    wireButtons();
    handleQueryBanner();
    ensureAuthReady().then(function () {
      renderAuthBar();
      loadBillingConfig();
      maybeAutoCheckoutAfterSignIn();
    });
    if (typeof rodajaOnAuthChange === "function") {
      rodajaOnAuthChange(function () { renderAuthBar(); });
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    boot();
  }
})();
