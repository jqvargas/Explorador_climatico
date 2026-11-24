/* Avisos del celular: permiso solo con gesto, suscripcion Web Push. */
(function (global) {
  "use strict";

  function $(id) { return document.getElementById(id); }

  function pad(n) { return (n < 10 ? "0" : "") + n; }

  function horaValor(mins) {
    mins = parseInt(mins, 10);
    if (isNaN(mins)) mins = 450;
    if (mins < 420) mins = 420;
    if (mins > 1319) mins = 1319;
    return pad(Math.floor(mins / 60)) + ":" + pad(mins % 60);
  }

  function horaMinutos(value) {
    var p = String(value || "07:30").split(":");
    var h = parseInt(p[0], 10);
    var m = parseInt(p[1], 10);
    if (isNaN(h) || isNaN(m)) return 450;
    return h * 60 + m;
  }

  function one(v) {
    return (v && v.length != null && typeof v !== "string") ? v[0] : v;
  }

  function b64ToBuf(base64String) {
    var padding = "=".repeat((4 - base64String.length % 4) % 4);
    var base64 = (base64String + padding).replace(/-/g, "+").replace(/_/g, "/");
    var raw = atob(base64);
    var out = new Uint8Array(raw.length);
    for (var i = 0; i < raw.length; i++) out[i] = raw.charCodeAt(i);
    return out;
  }

  function permiso() {
    if (!("Notification" in global)) return "unsupported";
    return Notification.permission;
  }

  function html(prefs) {
    prefs = prefs || {};
    var p = permiso();
    var bloqueado = p === "denied";
    var listo = p === "granted";
    var hora = horaValor(prefs.briefing_hora);
    var nota = "El clima de tu estaci\u00f3n llega cada ma\u00f1ana. Con cuenta, tambi\u00e9n llegan las alertas.";
    var body = "";
    if (p === "unsupported") {
      body = '<p class="al-desc">Este navegador no muestra avisos en el celular.</p>';
    } else if (bloqueado) {
      body = '<p class="al-desc">' + nota + "</p>" +
        '<button type="button" class="al-btn al-btn-ghost" id="al-push-settings">Activar en ajustes de Android</button>';
    } else if (!listo) {
      body = '<p class="al-desc">' + nota + "</p>" +
        '<button type="button" class="al-btn al-btn-primary" id="al-push-enable">Activar avisos</button>';
    } else {
      body = '<p class="al-desc">' + nota + "</p>" +
        '<div class="al-row"><div class="al-row-t"><b>Avisos activados</b><small>Puedes apagarlos aqu\u00ed o en los ajustes de Android</small></div>' +
          '<label class="al-switch"><input type="checkbox" id="al-push-alertas"' + (prefs.push_alertas ? " checked" : "") + '><span class="al-slider"></span></label></div>' +
        '<div class="al-row"><div class="al-row-t"><b>Clima de la ma\u00f1ana</b><small>Hora del aviso</small></div>' +
          '<input type="time" id="al-push-hora" min="07:00" max="21:59" value="' + hora + '">' +
          '<label class="al-switch"><input type="checkbox" id="al-push-briefing"' + (prefs.push_briefing ? " checked" : "") + '><span class="al-slider"></span></label></div>';
    }
    return '<section class="al-card al-cfg" id="al-push-block">' +
      '<h2><svg class="i" viewBox="0 0 24 24"><rect x="7" y="3" width="10" height="18" rx="2"/><path d="M11 18h2"/></svg>Avisos en el celular</h2>' +
      body + "</section>";
  }

  function putPrefs(apiFetch, body) {
    return apiFetch("/me/alertas/preferencias", {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body)
    });
  }

  function suscribir(apiFetch) {
    return apiFetch("/me/alertas/push/clave").then(function (data) {
      var key = one(data && data.publicKey);
      if (!key) throw new Error("Avisos del celular no configurados");
      return navigator.serviceWorker.ready.then(function (reg) {
        return reg.pushManager.subscribe({
          userVisibleOnly: true,
          applicationServerKey: b64ToBuf(key)
        });
      });
    }).then(function (sub) {
      var json = sub.toJSON();
      return apiFetch("/me/alertas/push", {
        method: "PUT",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ endpoint: json.endpoint, keys: json.keys })
      }).then(function () { return sub; });
    });
  }

  function desuscribir(apiFetch) {
    var done = Promise.resolve();
    if ("serviceWorker" in navigator) {
      done = navigator.serviceWorker.ready.then(function (reg) {
        return reg.pushManager.getSubscription();
      }).then(function (sub) {
        if (!sub) return apiFetch("/me/alertas/push", { method: "DELETE" });
        var endpoint = sub.endpoint;
        return sub.unsubscribe().catch(function () { return null; }).then(function () {
          return apiFetch("/me/alertas/push", {
            method: "DELETE",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ endpoint: endpoint })
          });
        });
      });
    }
    return done.catch(function () { return null; });
  }

  function wire(apiFetch, setStatus, onSaved) {
    var enable = $("al-push-enable");
    if (enable) {
      enable.addEventListener("click", function () {
        if (permiso() === "denied") return;
        setStatus("Pidiendo permiso\u2026", true);
        Notification.requestPermission().then(function (res) {
          if (res !== "granted") {
            setStatus(res === "denied" ? "Avisos bloqueados. Activalos en los ajustes de Android." : "No se activaron los avisos.", false);
            if (onSaved) onSaved();
            return;
          }
          return suscribir(apiFetch).then(function () {
            setStatus("Celular listo para avisos.", true);
            if (onSaved) onSaved();
          });
        }).catch(function (err) { setStatus(err.message || "No se pudo activar", false); });
      });
    }
    var settings = $("al-push-settings");
    if (settings) {
      settings.addEventListener("click", function () {
        setStatus("Abre los ajustes de la app o del sitio y permite las notificaciones. No volvemos a pedir el permiso.", false);
      });
    }
    var al = $("al-push-alertas");
    var br = $("al-push-briefing");
    var hora = $("al-push-hora");
    function guardar(which) {
      var body = {};
      if (al) body.push_alertas = al.checked;
      if (br) body.push_briefing = br.checked;
      if (hora) body.briefing_hora = horaMinutos(hora.value);
      var necesita = (al && al.checked) || (br && br.checked);
      var chain = necesita ? suscribir(apiFetch) : Promise.resolve();
      setStatus("Guardando\u2026", true);
      chain.then(function () { return putPrefs(apiFetch, body); }).then(function (data) {
        setStatus("Avisos guardados.", true);
        if (data && data.preferencias && onSaved) onSaved(data.preferencias);
      }).catch(function (err) {
        setStatus(err.message || "No se pudo guardar", false);
        if (which) which.checked = !which.checked;
      });
    }
    if (al) al.addEventListener("change", function () { guardar(al); });
    if (br) br.addEventListener("change", function () { guardar(br); });
    if (hora) hora.addEventListener("change", function () { guardar(null); });
  }

  function offerOnStation(eid) {
    eid = parseInt(eid, 10);
    if (!eid || eid < 1) return;
    if (!("Notification" in global) || !("serviceWorker" in navigator) || !("PushManager" in global)) return;
    if (Notification.permission === "denied") return;
    var ask = Notification.permission === "granted"
      ? Promise.resolve("granted")
      : Notification.requestPermission();
    ask.then(function (res) {
      if (res !== "granted") return;
      return fetch("/api/alertas/push/clave").then(function (r) { return r.json(); }).then(function (data) {
        var key = one(data && data.publicKey);
        if (!key) return;
        return navigator.serviceWorker.ready.then(function (reg) {
          return reg.pushManager.subscribe({
            userVisibleOnly: true,
            applicationServerKey: b64ToBuf(key)
          });
        });
      }).then(function (sub) {
        if (!sub) return;
        var json = sub.toJSON();
        var headers = { "Content-Type": "application/json" };
        var tokenP = (typeof global.rodajaGetAuthToken === "function")
          ? global.rodajaGetAuthToken() : Promise.resolve("");
        return tokenP.then(function (token) {
          if (token) headers.Authorization = "Bearer " + token;
          return fetch("/api/alertas/push", {
            method: "POST",
            headers: headers,
            body: JSON.stringify({ endpoint: json.endpoint, keys: json.keys, eid: eid })
          });
        });
      });
    }).catch(function () { /* el panel de alertas sigue disponible */ });
  }

  global.rodajaPushOfferOnStation = offerOnStation;
  global.rodajaPushHtml = html;
  global.rodajaPushWire = wire;
  global.rodajaPushDisable = function () {
    if (typeof global.rodajaApiFetch !== "function") return Promise.resolve();
    return desuscribir(function (path, opts) {
      return global.rodajaApiFetch(path, opts || {}).then(function (r) {
        if (!r.ok) throw new Error("HTTP " + r.status);
        return {};
      });
    });
  };
})(window);
