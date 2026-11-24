/* rodaja.cl — auth UI (PWA mi-reporte) */
(function (global) {
  "use strict";

  function escapeHtml(s) {
    return String(s)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function applyCloudFavorites(cloud) {
    if (!cloud) return;
    var hasCloud = !!(cloud.primary || (cloud.recents && cloud.recents.length));
    if (!hasCloud) return;
    var local = typeof rodajaGetFavorites === "function" ? rodajaGetFavorites() : null;
    if (local && local.primary) return;
    try {
      var next = {
        primary: cloud.primary || null,
        recents: cloud.recents || []
      };
      localStorage.setItem("rodaja_favorites_v1", JSON.stringify(next));
      if (next.primary && next.primary.eid) {
        localStorage.setItem("rodaja_app_reporte_v1", JSON.stringify({
          eid: next.primary.eid,
          variableId: next.primary.variableId || null,
          nombre: next.primary.alias || ""
        }));
      }
      global.dispatchEvent(new CustomEvent("rodaja:favorites-changed", { detail: cloud }));
    } catch (e) { /* ignore */ }
  }

  function renderHeaderAuth(user, token) {
    var nav = document.querySelector(".mr-nav");
    if (!nav) return;
    var existing = document.getElementById("mr-auth-bar");
    if (!rodajaAuthEnabled || !rodajaAuthEnabled()) {
      if (existing) existing.remove();
      return;
    }
    if (!existing) {
      existing = document.createElement("div");
      existing.id = "mr-auth-bar";
      existing.className = "mr-auth-bar";
      nav.insertBefore(existing, nav.firstChild);
    }
    if (token && user) {
      var email = (user.primaryEmailAddress && user.primaryEmailAddress.emailAddress) ||
        (user.emailAddresses && user.emailAddresses[0] && user.emailAddresses[0].emailAddress) ||
        "Cuenta";
      existing.innerHTML =
        '<span class="mr-auth-email">' + escapeHtml(email) + '</span>' +
        '<button type="button" class="mr-btn mr-btn-ghost mr-auth-logout">Salir</button>';
      existing.querySelector(".mr-auth-logout").addEventListener("click", function () {
        rodajaSignOut();
      });
      rodajaLoadFavoritesFromCloud().then(applyCloudFavorites);
      return;
    }
    existing.innerHTML =
      '<a class="mr-link" href="#" data-rodaja-signin>Iniciar sesi\u00f3n</a>';
  }

  function boot() {
    if (typeof rodajaOnAuthChange === "function") {
      rodajaOnAuthChange(renderHeaderAuth);
    }
    if (typeof rodajaInitAuth === "function") {
      rodajaInitAuth().then(function () {
        return rodajaGetAuthToken();
      }).then(function (token) {
        renderHeaderAuth(rodajaGetCurrentUser(), token);
      });
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    boot();
  }
})(window);
