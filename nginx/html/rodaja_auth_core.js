/* rodaja.cl — auth core (Clerk + API) */
(function (global) {
  "use strict";

  var clerkLoaded = false;
  var clerkLoading = false;
  var authInitialized = false;
  var sessionToken = null;
  var currentUser = null;
  var authListeners = [];
  var hadUser = false;

  function config() {
    return global.RODAJA_CONFIG || {};
  }

  function apiBase() {
    var b = (config().apiBase || "/api").replace(/\/$/, "");
    return b || "/api";
  }

  function apiPath(path) {
    var p = path.charAt(0) === "/" ? path : "/" + path;
    return apiBase() + p;
  }

  function publishableKey() {
    return (config().clerkPublishableKey || "").trim();
  }

  function accountUrl() {
    var u = (config().accountUrl || "https://accounts.rodaja.cl").replace(/\/$/, "");
    return u;
  }

  function authEnabled() {
    return publishableKey().length > 0;
  }

  function onAuthChange(fn) {
    if (typeof fn === "function") authListeners.push(fn);
  }

  function notifyAuthChange() {
    var wasSignedIn = hadUser;
    authListeners.forEach(function (fn) {
      try { fn(currentUser, sessionToken); } catch (e) { /* ignore */ }
    });
    try {
      global.dispatchEvent(new CustomEvent("rodaja:auth-changed", {
        detail: { user: currentUser, token: sessionToken }
      }));
    } catch (e) { /* ignore */ }
    var signedIn = !!(currentUser && sessionToken);
    if (signedIn && !wasSignedIn && typeof global.rodajaTrack === "function") {
      global.rodajaTrack("login", { method: "clerk" });
    }
    hadUser = signedIn;
    if (signedIn) sendFirstTouchOrigin();
  }

  /* Atribucion: envia una vez por cuenta el first-touch guardado (rodaja-config.js / arriba). */
  var origenEnviando = false;
  function sendFirstTouchOrigin() {
    try {
      if (origenEnviando || !currentUser || !sessionToken) return;
      var ls = global.localStorage;
      var uid = String(currentUser.id || "");
      if (!uid || ls.getItem("rodaja_ft_sent") === uid) return;
      var ft = {};
      try { ft = JSON.parse(ls.getItem("rodaja_ft") || "{}") || {}; } catch (e) { ft = {}; }
      origenEnviando = true;
      fetch(apiPath("/me/origen"), {
        method: "POST",
        headers: { Accept: "application/json", "Content-Type": "application/json", Authorization: "Bearer " + sessionToken },
        body: JSON.stringify({
          referrer: ft.referrer || "", utm_source: ft.utm_source || "", utm_medium: ft.utm_medium || "",
          utm_campaign: ft.utm_campaign || "", landing_path: ft.landing_path || "", first_touch_at: ft.t || null
        })
      }).then(function (r) {
        if (r.ok) ls.setItem("rodaja_ft_sent", uid);
      }).catch(function () { /* reintenta en la proxima sesion */ }).then(function () { origenEnviando = false; });
    } catch (e) { origenEnviando = false; }
  }


  function withTimeout(promise, ms, message) {
    return new Promise(function (resolve, reject) {
      var settled = false;
      var timer = setTimeout(function () {
        if (settled) return;
        settled = true;
        reject(new Error(message || "timeout"));
      }, ms);
      Promise.resolve(promise).then(function (value) {
        if (settled) return;
        settled = true;
        clearTimeout(timer);
        resolve(value);
      }, function (err) {
        if (settled) return;
        settled = true;
        clearTimeout(timer);
        reject(err);
      });
    });
  }

  function loadClerkScript() {
    return new Promise(function (resolve, reject) {
      if (!authEnabled()) {
        resolve(null);
        return;
      }
      if (global.Clerk) {
        resolve(global.Clerk);
        return;
      }
      if (clerkLoading) {
        var tries = 0;
        var t = setInterval(function () {
          tries++;
          if (global.Clerk) {
            clearInterval(t);
            resolve(global.Clerk);
          } else if (tries > 50) {
            clearInterval(t);
            reject(new Error("Clerk timeout"));
          }
        }, 100);
        return;
      }
      clerkLoading = true;
      var s = document.createElement("script");
      s.async = true;
      s.crossOrigin = "anonymous";
      s.setAttribute("data-clerk-publishable-key", publishableKey());
      s.src = "https://cdn.jsdelivr.net/npm/@clerk/clerk-js@5/dist/clerk.browser.js";
      s.onload = function () {
        clerkLoaded = true;
        var loaded = (global.Clerk && global.Clerk.load)
          ? withTimeout(global.Clerk.load(), 10000, "Clerk load timeout")
          : Promise.resolve(global.Clerk || null);
        loaded.then(function () {
          clerkLoading = false;
          resolve(global.Clerk);
        }).catch(function (err) {
          clerkLoading = false;
          reject(err);
        });
      };
      s.onerror = function () {
        clerkLoading = false;
        reject(new Error("No se pudo cargar Clerk"));
      };
      document.head.appendChild(s);
    });
  }

  function refreshSession() {
    return loadClerkScript().then(function (Clerk) {
      if (!Clerk) {
        sessionToken = null;
        currentUser = null;
        notifyAuthChange();
        return null;
      }

      function tryToken(attempt) {
        if (Clerk.session) {
          return Clerk.session.getToken().then(function (token) {
            sessionToken = token || null;
            currentUser = Clerk.user || null;
            notifyAuthChange();
            return token;
          });
        }
        if (attempt >= 30) {
          sessionToken = null;
          currentUser = Clerk.user || null;
          notifyAuthChange();
          return null;
        }
        return new Promise(function (resolve) {
          setTimeout(function () { resolve(tryToken(attempt + 1)); }, 100);
        });
      }

      return tryToken(0);
    }).catch(function () {
      sessionToken = null;
      currentUser = null;
      notifyAuthChange();
      return null;
    });
  }

  function getAuthToken() {
    if (!authEnabled()) return Promise.resolve(null);
    if (sessionToken) return Promise.resolve(sessionToken);
    return loadClerkScript().then(function (Clerk) {
      if (Clerk && Clerk.session && Clerk.session.getToken) {
        return Clerk.session.getToken().then(function (token) {
          sessionToken = token || null;
          currentUser = Clerk.user || null;
          return token;
        });
      }
      // Signed-out: do not poll Clerk.session 30×100ms. Private calls 401-retry.
      return null;
    }).catch(function () { return null; });
  }

  function getCurrentUser() {
    return currentUser;
  }

  function isSignedIn() {
    return !!(currentUser && sessionToken);
  }

  function signInUrl(redirectUrl) {
    var base = accountUrl() + "/sign-in";
    if (redirectUrl) {
      base += "?redirect_url=" + encodeURIComponent(redirectUrl);
    }
    return base;
  }

  function signUpUrl(redirectUrl) {
    var base = accountUrl() + "/sign-up";
    if (redirectUrl) {
      base += "?redirect_url=" + encodeURIComponent(redirectUrl);
    }
    return base;
  }

  function openSignIn(redirectUrl) {
    var here = redirectUrl || (global.location && global.location.href) || "/";
    return loadClerkScript().then(function (Clerk) {
      if (Clerk && typeof Clerk.openSignIn === "function") {
        Clerk.openSignIn({ fallbackRedirectUrl: here });
        return true;
      }
      global.location.href = signInUrl(here);
      return false;
    }).catch(function () {
      global.location.href = signInUrl(here);
      return false;
    });
  }

  function openSignUp(redirectUrl) {
    var here = redirectUrl || (global.location && global.location.href) || "/";
    return loadClerkScript().then(function (Clerk) {
      if (Clerk && typeof Clerk.openSignUp === "function") {
        Clerk.openSignUp({ fallbackRedirectUrl: here });
        return true;
      }
      global.location.href = signUpUrl(here);
      return false;
    }).catch(function () {
      global.location.href = signUpUrl(here);
      return false;
    });
  }

  document.addEventListener("click", function (e) {
    var t = e.target && e.target.closest ? e.target.closest("[data-rodaja-signin], [data-rodaja-signup]") : null;
    if (!t) return;
    e.preventDefault();
    if (t.getAttribute("data-rodaja-signin") != null) openSignIn();
    else openSignUp();
  });

  function accountProfileUrl() {
    // ponytail: UserProfile embebido en /cuenta/ (custom page "Uso"); el portal hosted
    // accounts.rodaja.cl/user no admite pestañas custom de rodaja.
    return "/cuenta/";
  }

  function signOut() {
    return loadClerkScript().then(function (Clerk) {
      if (Clerk && Clerk.signOut) {
        return Clerk.signOut().then(function () {
          sessionToken = null;
          currentUser = null;
          notifyAuthChange();
        });
      }
      sessionToken = null;
      currentUser = null;
      notifyAuthChange();
    });
  }

  function rodajaDownloadFile(path, fallbackName) {
    var authRetried = false;
    var uuidFileRe = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\.(csv|zip|tar\.gz)$/i;
    function run() {
      return getAuthToken().then(function (token) {
        var headers = {
          Accept: "text/csv, application/zip, application/gzip, application/octet-stream, */*"
        };
        if (token) headers.Authorization = "Bearer " + token;
        return fetch(apiPath(path), { headers: headers });
      }).then(function (r) {
        if (r.status === 401 && !authRetried) {
          authRetried = true;
          sessionToken = null;
          return refreshSession().then(function (newToken) {
            if (!newToken) return r;
            return run();
          });
        }
        var ct = (r.headers.get("Content-Type") || "").toLowerCase();
        if (!r.ok) {
          return r.text().then(function (text) {
            var msg = text;
            try {
              var j = JSON.parse(text);
              if (j.error) msg = Array.isArray(j.error) ? j.error.join(", ") : j.error;
            } catch (e) { /* keep text */ }
            throw new Error(msg || ("HTTP " + r.status));
          });
        }
        if (ct.indexOf("json") >= 0) {
          return r.text().then(function () {
            throw new Error("El servidor devolvio una respuesta JSON en lugar del archivo.");
          });
        }
        return r.blob().then(function (blob) {
          var name = fallbackName || "descarga.dat";
          if (Array.isArray(name)) name = name.length ? name[0] : "descarga.dat";
          var disp = r.headers.get("Content-Disposition") || "";
          var m = /filename\*=UTF-8''([^;\s]+)|filename=\"?([^\";]+)\"?/i.exec(disp);
          if (m) {
            var fromHeader = decodeURIComponent(m[1] || m[2] || "");
            if (fromHeader && !uuidFileRe.test(fromHeader)) name = fromHeader;
          }
          return { blob: blob, filename: String(name) };
        });
      });
    }
    function triggerSave(result) {
      if (!result || !result.blob) return result;
      var a = document.createElement("a");
      var url = URL.createObjectURL(result.blob);
      a.href = url;
      a.download = result.filename || "descarga.dat";
      a.style.display = "none";
      document.body.appendChild(a);
      a.click();
      setTimeout(function () {
        URL.revokeObjectURL(url);
        if (a.parentNode) a.parentNode.removeChild(a);
      }, 250);
      return result;
    }
    return run().then(triggerSave);
  }

  function rodajaApiFetch(path, options) {
    options = options || {};
    var retried = options._authRetried;
    return getAuthToken().then(function (token) {
      var headers = Object.assign({ Accept: "application/json" }, options.headers || {});
      if (token) headers.Authorization = "Bearer " + token;
      var opts = Object.assign({}, options, { headers: headers });
      delete opts._authRetried;
      return fetch(apiPath(path), opts).then(function (r) {
        if (r.status === 401 && !retried) {
          sessionToken = null;
          return refreshSession().then(function (newToken) {
            if (!newToken) return r;
            var retryOpts = Object.assign({}, options, { _authRetried: true });
            return rodajaApiFetch(path, retryOpts);
          });
        }
        return r;
      });
    });
  }

  function syncFavoritesToCloud(localStore) {
    if (!authEnabled()) return Promise.resolve(false);
    return getAuthToken().then(function (token) {
      if (!token) return false;
      return fetch("/api/me/favoritos", {
        method: "PUT",
        headers: {
          Accept: "application/json",
          "Content-Type": "application/json",
          Authorization: "Bearer " + token
        },
        body: JSON.stringify(localStore || { primary: null, recents: [] })
      }).then(function (r) { return r.ok; }).catch(function () { return false; });
    });
  }

  function loadFavoritesFromCloud() {
    if (!authEnabled()) return Promise.resolve(null);
    return getAuthToken().then(function (token) {
      if (!token) return null;
      return fetch("/api/me/favoritos", {
        headers: { Accept: "application/json", Authorization: "Bearer " + token }
      }).then(function (r) {
        if (!r.ok) return null;
        return r.json();
      }).catch(function () { return null; });
    });
  }

  function initAuth() {
    if (!authEnabled()) return Promise.resolve(null);
    return loadClerkScript().then(function (Clerk) {
      if (!Clerk) return null;
      if (!authInitialized) {
        authInitialized = true;
        if (Clerk.addListener) {
          Clerk.addListener(function (evt) {
            if (evt && (evt.type === "session" || evt.type === "user")) {
              refreshSession();
            }
          });
        }
      }
      return refreshSession();
    });
  }

  global.rodajaAuthEnabled = authEnabled;
  global.rodajaGetAuthToken = getAuthToken;
  global.rodajaGetCurrentUser = getCurrentUser;
  global.rodajaIsSignedIn = isSignedIn;
  global.rodajaSignInUrl = signInUrl;
  global.rodajaSignUpUrl = signUpUrl;
  global.rodajaOpenSignIn = openSignIn;
  global.rodajaOpenSignUp = openSignUp;
  global.rodajaAccountProfileUrl = accountProfileUrl;
  global.rodajaSignOut = signOut;
  global.rodajaApiFetch = rodajaApiFetch;
  global.rodajaDownloadFile = rodajaDownloadFile;
  global.rodajaSyncFavoritesToCloud = syncFavoritesToCloud;
  global.rodajaLoadFavoritesFromCloud = loadFavoritesFromCloud;
  function syncStationGroupToCloud(localStore) {
    if (!authEnabled()) return Promise.resolve(false);
    return getAuthToken().then(function (token) {
      if (!token) return false;
      return fetch("/api/me/grupo-estaciones", {
        method: "PUT",
        headers: {
          Accept: "application/json",
          "Content-Type": "application/json",
          Authorization: "Bearer " + token
        },
        body: JSON.stringify(localStore || { nombre: "Mi grupo", ids: [] })
      }).then(function (r) { return r.ok; }).catch(function () { return false; });
    });
  }
  function loadStationGroupFromCloud() {
    if (!authEnabled()) return Promise.resolve(null);
    return getAuthToken().then(function (token) {
      if (!token) return null;
      return fetch("/api/me/grupo-estaciones", {
        headers: { Accept: "application/json", Authorization: "Bearer " + token }
      }).then(function (r) {
        if (!r.ok) return null;
        return r.json();
      }).catch(function () { return null; });
    });
  }
  global.rodajaSyncStationGroupToCloud = syncStationGroupToCloud;
  global.rodajaLoadStationGroupFromCloud = loadStationGroupFromCloud;
  global.rodajaOnAuthChange = onAuthChange;
  global.rodajaApiPath = apiPath;
  global.rodajaInitAuth = initAuth;
  global.rodajaRefreshAuth = refreshSession;
})(window);

/* rodaja.cl — atribucion first-touch (referrer + UTM + landing). 90 dias. Sin datos personales. */
(function (w) {
  try {
    var KEY = "rodaja_ft", TTL = 90 * 864e5, now = Date.now(), ls = w.localStorage;
    var cur = null;
    try { cur = JSON.parse(ls.getItem(KEY) || "null"); } catch (e) { cur = null; }
    if (cur && cur.t && (now - cur.t) < TTL) return;
    var q = new URLSearchParams(w.location.search || "");
    var cut = function (v) { return v ? String(v).slice(0, 255) : ""; };
    var us = cut(q.get("utm_source")), um = cut(q.get("utm_medium")), uc = cut(q.get("utm_campaign"));
    var ref = "", rh = "";
    try {
      if (document.referrer) {
        var r = new URL(document.referrer);
        rh = r.hostname.toLowerCase();
        ref = (r.protocol + "//" + rh + r.pathname).slice(0, 255);
      }
    } catch (e) { ref = ""; rh = ""; }
    var interno = rh && (rh === w.location.hostname.toLowerCase() || /(^|\.)rodaja\.cl$/.test(rh) || /clerk\./.test(rh));
    if (interno && !us) return; /* navegacion interna o vuelta del login: no es primera visita */
    ls.setItem(KEY, JSON.stringify({
      t: now, referrer: interno ? "" : ref, utm_source: us, utm_medium: um, utm_campaign: uc,
      landing_path: cut(w.location.pathname || "/")
    }));
  } catch (e) { /* localStorage bloqueado: sin atribucion */ }
})(window);
