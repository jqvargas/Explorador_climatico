/* #region agent log */
/* Instrumentacion temporal (debug 5e3e0f) - velo gris / mapa ausente. Quitar al cerrar el caso. */
(function (global) {
  "use strict";

  var EP = "http://localhost:7911/ingest/2d6a8a92-c9c8-483b-9bd9-66545d475d64";
  var SID = "5e3e0f";
  var t0 = Date.now();
  var lastRecv = 0;
  var lastInput = t0;
  var wsOpenAt = 0;
  var sock = null;
  var caps = { asset: 0, overlay: 0, silence: 0, close: 0, jsErr: 0 };

  function send(hyp, loc, msg, data) {
    try {
      global.fetch(EP, {
        method: "POST",
        headers: { "Content-Type": "application/json", "X-Debug-Session-Id": SID },
        body: JSON.stringify({
          sessionId: SID, runId: "run1", hypothesisId: hyp,
          location: loc, message: msg, data: data, timestamp: Date.now()
        }),
        keepalive: true
      }).catch(function () {});
    } catch (e) { /* ignore */ }
  }

  function ctx() {
    var now = Date.now();
    var diag = {};
    try { if (global.ecDiagOverlays) diag = global.ecDiagOverlays(); } catch (e) { /* ignore */ }
    var tiles = -1;
    try { tiles = document.querySelectorAll(".leaflet-tile-pane img.leaflet-tile").length; } catch (e2) { /* ignore */ }
    return {
      msSinceLoad: now - t0,
      msSinceWsRecv: lastRecv ? now - lastRecv : null,
      msSinceUserInput: now - lastInput,
      wsState: sock ? sock.readyState : null,
      wsAgeMs: wsOpenAt ? now - wsOpenAt : null,
      online: !!global.navigator.onLine,
      visibility: document.visibilityState,
      bootHealthy: diag.bootHealthy === undefined ? null : diag.bootHealthy,
      moduleAlive: typeof global.ecDiagOverlays === "function",
      leafletTiles: tiles,
      hasLeafletContainer: !!document.querySelector(".leaflet-container"),
      overlayPresent: !!document.getElementById("shiny-disconnected-overlay"),
      diag: diag
    };
  }

  /* H-A: assets del Shiny rechazados (429 por limit_req) o caídos → sin mapa.
     H-G: excepción JS no capturada que aborta un script de arranque. */
  global.addEventListener("error", function (ev) {
    var el = ev && ev.target;
    if (el && el.tagName && (el.src || el.href)) {
      if (caps.asset++ > 12) return;
      send("A", "ec_overlay_recovery.js:asset_error", "asset load failed", {
        url: String(el.src || el.href), tag: el.tagName, ctx: ctx()
      });
      return;
    }
    if (caps.jsErr++ > 12) return;
    send("G", "ec_overlay_recovery.js:js_exception", "excepción JS no capturada", {
      msg: String((ev && ev.message) || ""),
      file: String((ev && ev.filename) || ""),
      line: (ev && ev.lineno) || null,
      col: (ev && ev.colno) || null,
      stack: String((ev && ev.error && ev.error.stack) || "").slice(0, 900),
      ctx: ctx()
    });
  }, true);

  try {
    new global.PerformanceObserver(function (list) {
      list.getEntries().forEach(function (e) {
        if (typeof e.responseStatus !== "number" || e.responseStatus < 400) return;
        if (caps.asset++ > 12) return;
        send("A", "ec_overlay_recovery.js:http_status", "subresource non-2xx", {
          url: e.name, status: e.responseStatus, initiator: e.initiatorType, ctx: ctx()
        });
      });
    }).observe({ type: "resource", buffered: true });
  } catch (ePo) { /* ignore */ }

  /* H-O: protocolo de transporte de la navegación (h3/h2/http1.1) y cookie sticky. */
  function transportInfo() {
    var nav = {};
    try { nav = global.performance.getEntriesByType("navigation")[0] || {}; } catch (e) { /* ignore */ }
    var sticky = null, names = [];
    try {
      String(document.cookie || "").split(";").forEach(function (c) {
        var kv = c.split("=");
        var k = (kv[0] || "").trim();
        if (!k) return;
        names.push(k);
        if (k === "rodaja_shiny") sticky = (kv[1] || "").trim();
      });
    } catch (e2) { /* ignore */ }
    return {
      navProtocol: nav.nextHopProtocol || null,
      navDurationMs: nav.duration || null,
      stickyRoute: sticky,
      cookieNames: names
    };
  }

  /* H-K: ¿qué responde el origen por HTTP en el instante exacto del fallo del handshake? */
  function probeHttpAfterFailure(seq) {
    function one(url, opts) {
      return global.fetch(url, opts).then(function (r) {
        return { url: url, status: r.status, cfRay: r.headers.get("cf-ray"), server: r.headers.get("server") };
      }).catch(function (e) {
        return { url: url, error: String(e) };
      });
    }
    var stamp = Date.now();
    Promise.all([
      one("/websocket/?ecdbg=" + stamp, { method: "GET", cache: "no-store" }),
      one("/?ecdbg=" + stamp, { method: "HEAD", cache: "no-store" })
    ]).then(function (rs) {
      send("K", "ec_overlay_recovery.js:post_fail_http", "HTTP tras handshake fallido", {
        seq: seq, probes: rs, transport: transportInfo(), ctx: ctx()
      });
    });
  }

  /* H-B/H-H: ¿intenta Shiny abrir el websocket?, ¿a qué URL?, ¿con qué resultado? */
  try {
    var NativeWS = global.WebSocket;
    var wsSeq = 0;
    function PatchedWS(url, protocols) {
      var n = ++wsSeq;
      var openedAt = Date.now();
      var handshakeOk = false;
      var ws = protocols === undefined ? new NativeWS(url) : new NativeWS(url, protocols);
      send("B,H", "ec_overlay_recovery.js:ws_new", "new WebSocket()", {
        seq: n, url: String(url), transport: transportInfo(), ctx: ctx()
      });
      // #region agent log
      try {
        var swc = null;
        try { swc = !!(global.navigator.serviceWorker && global.navigator.serviceWorker.controller); } catch (eSw) { /* ignore */ }
        var ck = [];
        try {
          String(document.cookie || "").split(";").forEach(function (c) {
            var k = (c.split("=")[0] || "").trim();
            if (/^(rodaja_shiny|__cf_bm|cf_clearance|__cfruid)$/.test(k)) ck.push(k);
          });
        } catch (eCk) { /* ignore */ }
        send("I", "ec_overlay_recovery.js:ws_env", "entorno del handshake", {
          seq: n,
          swControlled: swc,
          cfCookies: ck,
          proto: (function () { try { return (performance.getEntriesByType("navigation")[0] || {}).nextHopProtocol || null; } catch (e) { return null; } })(),
          wsSeq: n
        });
      } catch (eEnv) { /* ignore */ }
      // #endregion
      try {
        ws.addEventListener("open", function () {
          handshakeOk = true;
          send("B,H", "ec_overlay_recovery.js:ws_handshake_ok", "websocket abierto", {
            seq: n, url: String(url), waitMs: Date.now() - openedAt,
            transport: transportInfo(), ctx: ctx()
          });
        });
        ws.addEventListener("close", function (ev) {
          if (caps.close++ > 10) return;
          send("B,C,D,H", "ec_overlay_recovery.js:ws_closed", "websocket cerrado", {
            seq: n, url: String(url), code: ev && ev.code, reason: (ev && ev.reason) || "",
            wasClean: !!(ev && ev.wasClean), handshakeOk: handshakeOk,
            ageMs: Date.now() - openedAt, transport: transportInfo(), ctx: ctx()
          });
          if (!handshakeOk) probeHttpAfterFailure(n);
        });
      } catch (e) { /* ignore */ }
      return ws;
    }
    PatchedWS.prototype = NativeWS.prototype;
    PatchedWS.CONNECTING = 0; PatchedWS.OPEN = 1; PatchedWS.CLOSING = 2; PatchedWS.CLOSED = 3;
    global.WebSocket = PatchedWS;
  } catch (eWs) { /* ignore */ }

  function attachSocket() {
    var s = null;
    try { s = global.Shiny && global.Shiny.shinyapp && global.Shiny.shinyapp.$socket; } catch (e) { return; }
    if (!s || s === sock) return;
    sock = s;
    wsOpenAt = Date.now();
    lastRecv = wsOpenAt;
    /* H-B: apertura del websocket y edad del boot. */
    send("B", "ec_overlay_recovery.js:ws_open", "shiny websocket connected", ctx());
    try {
      s.addEventListener("message", function () { lastRecv = Date.now(); });
      s.addEventListener("close", function (ev) {
        if (caps.close++ > 8) return;
        /* H-B/C/D: code 1000/1005 = cierre del servidor (idle kill); 1006 = corte de red/túnel. */
        send("B,C,D", "ec_overlay_recovery.js:ws_close", "shiny websocket closed", {
          code: ev && ev.code, reason: (ev && ev.reason) || "", wasClean: !!(ev && ev.wasClean),
          ctx: ctx()
        });
      });
    } catch (e2) { /* ignore */ }
  }

  /* H-D: silencio prolongado del websocket (timeout idle de Cloudflare/túnel ~100s). */
  global.setInterval(function () {
    if (!sock || sock.readyState !== 1 || !lastRecv) return;
    if (Date.now() - lastRecv < 45000) return;
    if (caps.silence++ > 20) return;
    send("D", "ec_overlay_recovery.js:ws_silence", "websocket sin tráfico >45s", ctx());
  }, 30000);

  /* H-E/F: aparición del velo (overlay de desconexión u otro overlay a pantalla completa). */
  try {
    new global.MutationObserver(function (muts) {
      for (var i = 0; i < muts.length; i++) {
        var added = muts[i].addedNodes || [];
        for (var j = 0; j < added.length; j++) {
          var n = added[j];
          if (!n || n.nodeType !== 1) continue;
          var id = n.id || "";
          var cls = String(n.className || "");
          var isVeil = id === "shiny-disconnected-overlay" ||
            /modal-backdrop|driver-overlay|ec-app-backdrop/.test(cls);
          if (!isVeil) continue;
          if (caps.overlay++ > 10) return;
          var cs = {};
          try {
            var c = global.getComputedStyle(n);
            cs = { position: c.position, background: c.backgroundColor, opacity: c.opacity, zIndex: c.zIndex };
          } catch (e) { /* ignore */ }
          send("E,F", "ec_overlay_recovery.js:overlay_added", "overlay gris insertado", {
            id: id, cls: cls, style: cs, ctx: ctx()
          });
        }
      }
    }).observe(document.documentElement, { childList: true, subtree: true });
  } catch (eMo) { /* ignore */ }

  function wire() {
    if (typeof global.jQuery === "undefined") { global.setTimeout(wire, 100); return; }
    var $ = global.jQuery;
    $(document).on("shiny:connected", function () { global.setTimeout(attachSocket, 0); });
    $(document).on("shiny:inputchanged", function (ev) {
      var nm = (ev && ev.name) || "";
      if (/^(\.|shiny)/.test(nm) || /ec_user_active|app_profile/.test(nm)) return;
      lastInput = Date.now();
    });
  }
  wire();

  /* H-A/E: estado al terminar la carga inicial (¿faltan librerías por 429?). */
  global.addEventListener("load", function () {
    global.setTimeout(function () {
      var nav = {};
      try { nav = global.performance.getEntriesByType("navigation")[0] || {}; } catch (e) { /* ignore */ }
      var reloadFlag = null;
      try { reloadFlag = global.sessionStorage.getItem("ec_boot_reload_once"); } catch (e2) { /* ignore */ }
      send("A,E", "ec_overlay_recovery.js:boot_state", "estado tras load", {
        navType: nav.type || null,
        transport: transportInfo(),
        hasJQuery: typeof global.jQuery !== "undefined",
        hasShiny: typeof global.Shiny !== "undefined",
        shinyConnected: !!(global.Shiny && global.Shiny.shinyapp && global.Shiny.shinyapp.isConnected && global.Shiny.shinyapp.isConnected()),
        hasLeaflet: typeof global.L !== "undefined",
        hasHTMLWidgets: typeof global.HTMLWidgets !== "undefined",
        reloadFlag: reloadFlag,
        ctx: ctx()
      });
      attachSocket();
    }, 4000);
  });
})(window);
/* #endregion */

/* rodaja.cl — overlays grises + fail-closed mid-boot + reseñal Leaflet */
(function (global) {
  "use strict";

  var RELOAD_KEY = "ec_boot_reload_once";
  var RELOAD_AT_KEY = "ec_boot_reload_at";
  var MAX_BOOT_RELOADS = 3;
  var bootHealthy = false;
  var mapReadySent = false;
  var mapReadyPollTimer = null;
  var mapInvalidateTimer = null;
  var lastMapReadyAt = 0;
  var overlayHandlersRegistered = false;
  var reloadScheduled = false;

  function ecDiagOverlays() {
    var modalVisible = !!document.querySelector(".modal.in, .modal.show");
    return {
      drawerOpen: !!document.querySelector(".ec-drawer-overlay.ec-drawer-open"),
      modalBackdrop: document.querySelectorAll(".modal-backdrop").length,
      modalVisible: modalVisible,
      shinyDisconnected: !!document.getElementById("shiny-disconnected-overlay"),
      shinyConnected: shinyIsConnected(),
      bootHealthy: bootHealthy
    };
  }

  /* Shiny.isConnected() lee $socket.readyState sin guardas: tras un handshake
     fallido $socket es null y lanza, abortando la recuperación. */
  function shinyIsConnected() {
    try {
      if (typeof Shiny === "undefined" || !Shiny.shinyapp) return false;
      if (typeof Shiny.shinyapp.isConnected !== "function") return false;
      return !!Shiny.shinyapp.isConnected();
    } catch (e) {
      return false;
    }
  }

  function ecCleanupStaleOverlays(opts) {
    opts = opts || {};
    var diag = ecDiagOverlays();
    var cleaned = { drawer: false, backdrop: 0 };

    if (diag.drawerOpen && typeof global.ecCloseDrawer === "function") {
      global.ecCloseDrawer();
      cleaned.drawer = true;
    }

    if (!diag.modalVisible) {
      var backs = document.querySelectorAll(".modal-backdrop");
      backs.forEach(function (el) {
        el.parentNode.removeChild(el);
        cleaned.backdrop += 1;
      });
      if (cleaned.backdrop > 0) {
        document.body.classList.remove("modal-open");
        document.body.style.removeProperty("padding-right");
      }
    }

    if (opts.log && (cleaned.drawer || cleaned.backdrop > 0)) {
      console.info("[ec_overlay_recovery] cleaned", cleaned, ecDiagOverlays());
    }
    return cleaned;
  }

  function ecRemoveShinyDisconnectedOverlay() {
    var el = document.getElementById("shiny-disconnected-overlay");
    if (!el || !el.parentNode) return false;
    el.parentNode.removeChild(el);
    return true;
  }

  function profileMark(name, attrs) {
    try {
      if (typeof global.__ecAppProfileMark === "function") {
        global.__ecAppProfileMark(name, attrs || { lane: "browser" });
      }
    } catch (e) { /* ignore */ }
  }

  function clearReloadFlag() {
    try {
      sessionStorage.removeItem(RELOAD_KEY);
      sessionStorage.removeItem(RELOAD_AT_KEY);
    } catch (e) { /* ignore */ }
  }

  function markBootHealthy() {
    bootHealthy = true;
    clearReloadFlag();
  }

  /* Mid-boot WS death → reintento de carga. Soft reconnect solo tras healthy.
     El handshake WebSocket contra el edge falla de forma intermitente e
     independiente entre intentos, así que un solo reintento no alcanza. */
  function failClosedReloadIfNeeded(src) {
    if (bootHealthy || reloadScheduled) return false;
    if (reloadAttempts() >= MAX_BOOT_RELOADS) return false;
    reloadScheduled = true;
    var intento = bumpReloadAttempts();
    profileMark("browser.boot_hard_reload", {
      lane: "browser",
      phase: "fail_closed",
      attempt: intento,
      src: src || "unknown"
    });
    // #region agent log
    try {
      global.fetch("http://localhost:7911/ingest/2d6a8a92-c9c8-483b-9bd9-66545d475d64", {
        method: "POST",
        headers: { "Content-Type": "application/json", "X-Debug-Session-Id": "5e3e0f" },
        body: JSON.stringify({
          sessionId: "5e3e0f", runId: "post-fix", hypothesisId: "H-reload",
          location: "ec_overlay_recovery.js:failClosedReloadIfNeeded",
          message: "programando recarga fail-closed",
          data: { src: src || "unknown", intento: intento, overlay: !!document.getElementById("shiny-disconnected-overlay") },
          timestamp: Date.now()
        }),
        keepalive: true
      }).catch(function () {});
    } catch (eLog) { /* ignore */ }
    // #endregion
    /* Backoff corto: el reintento inmediato suele reusar la conexión que falló. */
    global.setTimeout(function () {
      try {
        global.location.reload();
      } catch (eRel) { /* ignore */ }
    }, 250 * intento);
    return true;
  }

  function reloadAttempts() {
    try {
      var at = parseInt(sessionStorage.getItem(RELOAD_AT_KEY), 10);
      if (isFinite(at) && Date.now() - at > 20000) {
        sessionStorage.removeItem(RELOAD_KEY);
        sessionStorage.removeItem(RELOAD_AT_KEY);
        return 0;
      }
      var raw = parseInt(sessionStorage.getItem(RELOAD_KEY), 10);
      return isFinite(raw) && raw > 0 ? raw : 0;
    } catch (e) {
      return 0;
    }
  }

  function bumpReloadAttempts() {
    var n = reloadAttempts() + 1;
    try {
      sessionStorage.setItem(RELOAD_KEY, String(n));
      sessionStorage.setItem(RELOAD_AT_KEY, String(Date.now()));
    } catch (e) { /* ignore */ }
    return n;
  }

  function ecRecoverOverlays() {
    var diag = ecDiagOverlays();
    ecCleanupStaleOverlays({ log: false });

    if (diag.shinyConnected) {
      ecRemoveShinyDisconnectedOverlay();
      return;
    }

    if (document.visibilityState !== "visible") return;

    if (!bootHealthy) {
      if (document.getElementById("shiny-disconnected-overlay")) {
        failClosedReloadIfNeeded("lifecycle");
      }
      return;
    }

    if (
      typeof Shiny !== "undefined" &&
      Shiny.shinyapp &&
      typeof Shiny.shinyapp.reconnect === "function"
    ) {
      try {
        Shiny.shinyapp.reconnect();
      } catch (e) { /* ignore */ }
    }
  }

  function onLifecycleRecover() {
    ecRecoverOverlays();
    scheduleLeafletInvalidate();
  }

  function onShinyDisconnected() {
    mapReadySent = false;
    lastMapReadyAt = 0;
    if (mapReadyPollTimer) {
      clearInterval(mapReadyPollTimer);
      mapReadyPollTimer = null;
    }
    if (!bootHealthy) {
      failClosedReloadIfNeeded("shiny_disconnected");
      return;
    }
    onLifecycleRecover();
  }

  function onShinyConnected() {
    mapReadySent = false;
    scheduleMapReadyAfterIdle();
    onLifecycleRecover();
  }

  function mapHost() {
    var mount = document.getElementById("mapa_mount");
    if (!mount) return null;
    return mount.querySelector("[id$='-mapa']") || mount.querySelector(".leaflet-container");
  }

  function leafletMap() {
    var root = mapHost();
    if (!root) return null;
    if (window.jQuery) {
      var viaJq = window.jQuery(root).data("leaflet-map");
      if (viaJq) return viaJq;
    }
    if (window.HTMLWidgets && typeof window.HTMLWidgets.find === "function" && root.id) {
      try {
        var w = window.HTMLWidgets.find("#" + root.id);
        if (w && typeof w.getMap === "function") {
          var viaHw = w.getMap();
          if (viaHw) return viaHw;
        }
      } catch (eHw) { /* ignore */ }
    }
    if (root._leaflet_map) return root._leaflet_map;
    return null;
  }

  function leafletMounted() {
    return !!leafletMap() || !!(mapHost() && mapHost().classList.contains("leaflet-container"));
  }

  function invalidateLeafletSize() {
    var map = leafletMap();
    if (!map || typeof map.invalidateSize !== "function") return;
    if (map._animatingZoom) return;
    try {
      map.invalidateSize({ animate: false, pan: false });
    } catch (e) { /* ignore */ }
  }

  function scheduleLeafletInvalidate() {
    if (mapInvalidateTimer) clearTimeout(mapInvalidateTimer);
    mapInvalidateTimer = setTimeout(function () {
      mapInvalidateTimer = null;
      invalidateLeafletSize();
    }, 80);
  }

  function signalMapReady() {
    var now = Date.now();
    /* Una señal por montaje. Reenviar cada 400ms hacía que Shiny creyera remount
       y el choropleth fitBounds a Chile (zoom in → zoom out). Reset en onMapaWidgetValue. */
    if (mapReadySent) return;
    mapReadySent = true;
    lastMapReadyAt = now;
    if (mapReadyPollTimer) {
      clearInterval(mapReadyPollTimer);
      mapReadyPollTimer = null;
    }
    if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
      /* textInput: numericInput + Date.now() a veces no propaga en Shiny 1.7. */
      Shiny.setInputValue("ec_map_ready", String(now), { priority: "event" });
    }
  }

  function trySignalMapReady() {
    registerOverlayHandlers();
    if (leafletMounted()) {
      if (!mapReadySent) scheduleLeafletInvalidate();
      signalMapReady();
      return true;
    }
    return false;
  }

  /* shiny:value del widget = prueba de montaje; no depender solo de L.map/jQuery data. */
  function onMapaWidgetValue() {
    mapReadySent = false;
    signalMapReady();
    scheduleLeafletInvalidate();
    scheduleMapReadyAfterIdle();
  }

  function scheduleMapReadyAfterIdle() {
    if (trySignalMapReady()) return;
    if (mapReadyPollTimer) return;
    var attempts = 0;
    mapReadyPollTimer = setInterval(function () {
      attempts++;
      if (trySignalMapReady() || attempts >= 120) {
        clearInterval(mapReadyPollTimer);
        mapReadyPollTimer = null;
      }
    }, 100);
  }

  function registerOverlayHandlers() {
    if (overlayHandlersRegistered) return true;
    if (typeof Shiny === "undefined" || !Shiny.addCustomMessageHandler) return false;
    overlayHandlersRegistered = true;
    Shiny.addCustomMessageHandler("ec_report_first", function (on) {
      document.body.classList.toggle("ec-report-first", !!on);
    });
    /* Shiny exige handlers de aridad 1: con 0 argumentos lanza y aborta el script. */
    Shiny.addCustomMessageHandler("ec_boot_healthy", function (on) {
      markBootHealthy();
      profileMark("browser.boot_healthy", { lane: "browser", phase: "healthy" });
    });
    return true;
  }

  function wireShinyLifecycle() {
    if (typeof jQuery === "undefined") {
      setTimeout(wireShinyLifecycle, 50);
      return;
    }
    jQuery(document).on("shiny:connected", onShinyConnected);
    jQuery(document).on("shiny:disconnected", onShinyDisconnected);
    jQuery(document).on("shiny:idle", scheduleMapReadyAfterIdle);
    jQuery(document).on("shiny:value", function (ev) {
      try {
        var nm = (ev && ev.name) || "";
        if (/mapa-mapa$|(^|-)mapa$/.test(nm)) onMapaWidgetValue();
      } catch (eVal) { /* ignore */ }
    });
    /* Backup: zoom del mapa implica widget vivo. */
    jQuery(document).on("shiny:inputchanged", function (ev) {
      try {
        var nm = (ev && ev.name) || "";
        if (!/(^|-)mapa-mapa_zoom$/.test(nm) && !/mapa-mapa_zoom$/.test(nm)) return;
        if (!mapReadySent) {
          signalMapReady();
          scheduleLeafletInvalidate();
        }
      } catch (eIn) { /* ignore */ }
    });
    registerOverlayHandlers();
  }

  /* El overlay de Shiny aparece aunque shiny:disconnected no se dispare
     (handshake que nunca abre). Recargar al verlo, no esperar el evento. */
  function watchDisconnectedOverlay() {
    var overlaySince = 0;
    function maybeReload(src) {
      if (bootHealthy || reloadScheduled) return;
      if (document.visibilityState !== "visible") return;
      if (shinyIsConnected()) {
        overlaySince = 0;
        return;
      }
      if (!document.getElementById("shiny-disconnected-overlay")) {
        overlaySince = 0;
        return;
      }
      var now = Date.now();
      if (!overlaySince) overlaySince = now;
      if (now - overlaySince < 400) return;
      failClosedReloadIfNeeded(src);
    }
    try {
      new global.MutationObserver(function (muts) {
        for (var i = 0; i < muts.length; i++) {
          var added = muts[i].addedNodes || [];
          for (var j = 0; j < added.length; j++) {
            var n = added[j];
            if (!n || n.nodeType !== 1) continue;
            if (n.id === "shiny-disconnected-overlay") {
              maybeReload("overlay_observer");
              return;
            }
            if (n.querySelector && n.querySelector("#shiny-disconnected-overlay")) {
              maybeReload("overlay_observer");
              return;
            }
          }
        }
      }).observe(document.documentElement, { childList: true, subtree: true });
    } catch (eMo) { /* ignore */ }
    var ticks = 0;
    var poll = global.setInterval(function () {
      ticks++;
      maybeReload("overlay_poll");
      if (bootHealthy || reloadScheduled || ticks >= 40) {
        global.clearInterval(poll);
      }
    }, 200);
  }

  if (typeof Shiny !== "undefined") {
    registerOverlayHandlers();
  }

  global.ecDiagOverlays = ecDiagOverlays;
  global.ecCleanupStaleOverlays = ecCleanupStaleOverlays;

  global.addEventListener("pageshow", onLifecycleRecover);
  global.addEventListener("resize", scheduleLeafletInvalidate);
  if (global.visualViewport) {
    global.visualViewport.addEventListener("resize", scheduleLeafletInvalidate);
  }

  document.addEventListener("visibilitychange", function () {
    if (document.visibilityState === "visible") {
      onLifecycleRecover();
    }
  });

  document.addEventListener("keydown", function (e) {
    if (e.key !== "Escape" && e.keyCode !== 27) return;
    if (!document.querySelector(".ec-drawer-overlay.ec-drawer-open")) return;
    if (typeof global.ecCloseDrawer === "function") global.ecCloseDrawer();
  });

  watchDisconnectedOverlay();
  wireShinyLifecycle();
})(window);
