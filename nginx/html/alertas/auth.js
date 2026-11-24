/* Auth header (panel alertas) */
(function (global) {
  "use strict";

  function escapeHtml(s) {
    return String(s)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function renderAuthBar(user, token) {
    var bar = document.getElementById("al-auth-bar");
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
        '<span class="al-auth-email">' + escapeHtml(email) + "</span>" +
        '<button type="button" class="al-link al-auth-logout" style="border:none;background:none;cursor:pointer;">Salir</button>';
      bar.querySelector(".al-auth-logout").addEventListener("click", function () {
        var p = (typeof rodajaPushDisable === "function") ? rodajaPushDisable() : Promise.resolve();
        Promise.resolve(p).then(function () { rodajaSignOut(); }, function () { rodajaSignOut(); });
      });
      return;
    }
    bar.innerHTML =
      '<a class="al-link" href="#" data-rodaja-signin>Iniciar sesi\u00f3n</a>';
  }

  global.alRenderAuthBar = renderAuthBar;

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
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    boot();
  }
})(window);
