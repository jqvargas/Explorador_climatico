/* rodaja.cl — favoritos + deep links (Explorador + Mi reporte) */
(function (global) {
  "use strict";

  var STORAGE_KEY = global.EC_FAVORITES_STORAGE_KEY || "rodaja_favorites_v1";
  var URL_PARAMS = {
    estacion: "estacion",
    variable: "variable",
    operador: "operador",
    reporte: "reporte"
  };
  var shinyHandlersRegistered = false;
  var bootRetries = 0;
  var lastSyncedJson = null;

  function parseJson(s, fallback) {
    try { return JSON.parse(s); } catch (e) { return fallback; }
  }

  function emptyStore() {
    return { primary: null, recents: [] };
  }

  function coerceScalar(v) {
    if (v == null || v === "") return null;
    if (Array.isArray(v)) v = v.length ? v[0] : null;
    return v;
  }

  function unwrapShinyMsg(msg) {
    if (msg == null) return null;
    if (typeof msg === "string") {
      var parsed = parseJson(msg, null);
      return parsed ? unwrapShinyMsg(parsed) : null;
    }
    if (Array.isArray(msg)) return msg.length ? unwrapShinyMsg(msg[0]) : null;
    if (typeof msg === "object" && msg.data && typeof msg.data === "object") return unwrapShinyMsg(msg.data);
    return msg;
  }

  function normalizeFav(f) {
    if (!f || typeof f !== "object") return null;
    var eid = parseInt(coerceScalar(f.eid), 10);
    if (!eid || eid < 1) return null;
    var vid = coerceScalar(f.variableId);
    var fid = coerceScalar(f.fuenteId);
    var lat = coerceScalar(f.lat);
    var lon = coerceScalar(f.lon);
    var aliasRaw = coerceScalar(f.alias);
    var alias = aliasRaw != null ? String(aliasRaw).trim() : "";
    var vidNum = vid != null && vid !== "" ? parseInt(vid, 10) : null;
    var fidNum = fid != null && fid !== "" ? parseInt(fid, 10) : null;
    var out = {
      eid: eid,
      variableId: vidNum != null && !isNaN(vidNum) ? vidNum : null,
      fuenteId: fidNum != null && !isNaN(fidNum) ? fidNum : null,
      alias: alias,
      lat: lat != null && !isNaN(parseFloat(lat)) ? parseFloat(lat) : null,
      lon: lon != null && !isNaN(parseFloat(lon)) ? parseFloat(lon) : null
    };
    return out;
  }

  function rodajaGetFavorites() {
    var raw;
    try {
      raw = localStorage.getItem(STORAGE_KEY);
    } catch (e) {
      return emptyStore();
    }
    if (!raw) return emptyStore();
    var o = parseJson(raw, emptyStore());
    if (!o || typeof o !== "object") return emptyStore();
    o.primary = normalizeFav(o.primary);
    o.recents = Array.isArray(o.recents) ? o.recents.map(normalizeFav).filter(Boolean) : [];
    return o;
  }

  function rodajaSaveFavorites(store) {
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(store));
    } catch (e) {
      toast("No se pudo guardar en el navegador (almacenamiento bloqueado)", false);
      return false;
    }
    try {
      var p = store && store.primary;
      if (p && p.eid) {
        localStorage.setItem("rodaja_app_reporte_v1", JSON.stringify({
          eid: p.eid,
          variableId: p.variableId || null,
          nombre: p.alias || ""
        }));
      } else {
        localStorage.removeItem("rodaja_app_reporte_v1");
      }
    } catch (eMir) { /* ignore */ }
    try {
      global.dispatchEvent(new CustomEvent("rodaja:favorites-changed", { detail: store }));
    } catch (e) { /* IE */ }
    if (typeof global.rodajaSyncFavoritesToCloud === "function" && typeof global.rodajaIsSignedIn === "function" && global.rodajaIsSignedIn()) {
      global.rodajaSyncFavoritesToCloud(store);
    }
    return true;
  }

  function rodajaSetPrimary(fav) {
    var f = normalizeFav(fav);
    if (!f) return false;
    var store = rodajaGetFavorites();
    store.primary = f;
    var rec = store.recents.filter(function (r) { return r.eid !== f.eid; });
    rec.unshift(f);
    store.recents = rec.slice(0, 5);
    return rodajaSaveFavorites(store);
  }

  function rodajaClearPrimary() {
    var store = rodajaGetFavorites();
    store.primary = null;
    rodajaSaveFavorites(store);
  }

  function exploradorBasePath() {
    var configured = global.EC_EXPLORADOR_BASE_PATH;
    if (configured != null && String(configured).length > 0) {
      var c = String(configured);
      return c.charAt(c.length - 1) === "/" ? c : c + "/";
    }
    var p = global.location.pathname || "/";
    if (p === "/explorador" || p.indexOf("/explorador/") === 0) {
      var idx = p.indexOf("/explorador");
      return p.substring(0, idx + "/explorador".length) + "/";
    }
    if (p.endsWith("/explorador")) {
      return p + "/";
    }
    return "/";
  }

  function buildExploradorUrl(opts) {
    opts = opts || {};
    var base = exploradorBasePath();
    var q = [];
    if (opts.eid) q.push(URL_PARAMS.estacion + "=" + encodeURIComponent(opts.eid));
    if (opts.variableId) q.push(URL_PARAMS.variable + "=" + encodeURIComponent(opts.variableId));
    if (opts.fuenteId) q.push(URL_PARAMS.operador + "=" + encodeURIComponent(opts.fuenteId));
    return base + (q.length ? "?" + q.join("&") : "");
  }

  function buildMiReporteUrl(opts) {
    opts = opts || {};
    var q = [];
    if (opts.eid) q.push("e=" + encodeURIComponent(opts.eid));
    if (opts.variableId) q.push("v=" + encodeURIComponent(opts.variableId));
    if (opts.fuenteId) q.push("f=" + encodeURIComponent(opts.fuenteId));
    return "/mi-reporte/" + (q.length ? "?" + q.join("&") : "");
  }

  function parseExploradorSearch(search) {
    var out = { reporte: false };
    if (!search || search.length < 2) return out;
    var sp = new URLSearchParams(search.charAt(0) === "?" ? search.slice(1) : search);
    if (sp.has(URL_PARAMS.estacion)) out.eid = parseInt(sp.get(URL_PARAMS.estacion), 10);
    if (sp.has(URL_PARAMS.variable)) out.variableId = parseInt(sp.get(URL_PARAMS.variable), 10);
    if (sp.has(URL_PARAMS.operador)) out.fuenteId = parseInt(sp.get(URL_PARAMS.operador), 10);
    out.reporte = sp.get(URL_PARAMS.reporte) === "1";
    return out;
  }

  function syncExploradorUrl(opts) {
    if (!global.history || !global.history.replaceState) return;
    var url = buildExploradorUrl(opts);
    try {
      global.history.replaceState({}, "", url);
    } catch (e) { /* ignore */ }
  }

  function clearReportUrlParams() {
    if (!global.history || !global.history.replaceState) return;
    var sp = new URLSearchParams(global.location.search);
    sp.delete(URL_PARAMS.estacion);
    sp.delete(URL_PARAMS.reporte);
    var qs = sp.toString();
    var path = global.location.pathname;
    global.history.replaceState({}, "", path + (qs ? "?" + qs : ""));
  }

  function readShinySelectValue(id) {
    var el = document.getElementById(id);
    if (!el || !el.value || el.value === "" || el.value === "0") return null;
    var n = parseInt(el.value, 10);
    return isNaN(n) || n < 1 ? null : n;
  }

  function getActiveReportTabEid() {
    var activeTab = document.querySelector(".ec-report-tab.active .ec-report-tab-label");
    if (!activeTab) return null;
    var oc = activeTab.getAttribute("onclick") || "";
    var m = oc.match(/reporte_tab_select',(\d+)/);
    return m ? parseInt(m[1], 10) : null;
  }

  function getReportLinkOpts() {
    var parsed = parseExploradorSearch(global.location.search);
    var eid = parsed.eid || getActiveReportTabEid() || readShinySelectValue("estacion");
    if (!eid) return null;
    return {
      eid: eid,
      variableId: parsed.variableId || readShinySelectValue("variable"),
      fuenteId: parsed.fuenteId || readShinySelectValue("operador"),
      reporte: true
    };
  }

  function copyReportLink(opts, onDone) {
    opts = opts || {};
    if (!opts.eid) {
      if (onDone) onDone(false, "");
      return;
    }
    var url = global.location.origin + buildMiReporteUrl(opts);
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(url).then(function () {
        if (onDone) onDone(true, url);
      }).catch(function () {
        fallbackCopy(url, onDone);
      });
    } else {
      fallbackCopy(url, onDone);
    }
  }

  function fallbackCopy(text, onDone) {
    var ta = document.createElement("textarea");
    ta.value = text;
    ta.style.position = "fixed";
    ta.style.left = "-9999px";
    document.body.appendChild(ta);
    ta.select();
    try {
      document.execCommand("copy");
      if (onDone) onDone(true, text);
    } catch (e) {
      if (onDone) onDone(false, text);
    }
    document.body.removeChild(ta);
  }

  function toast(msg, ok) {
    var el = document.getElementById("ec-fav-toast");
    if (!el) {
      el = document.createElement("div");
      el.id = "ec-fav-toast";
      el.className = "ec-fav-toast";
      document.body.appendChild(el);
    }
    el.textContent = msg;
    el.className = "ec-fav-toast" + (ok === false ? " ec-fav-toast-err" : " ec-fav-toast-ok");
    el.style.display = "block";
    clearTimeout(el._t);
    el._t = setTimeout(function () { el.style.display = "none"; }, 2800);
  }

  function shinyReadyForFavSync() {
    if (typeof Shiny === "undefined" || !Shiny.setInputValue) return false;
    if (!document.getElementById("ec_favorites_json")) return false;
    if (Shiny.shinyapp && Shiny.shinyapp.isConnected && !Shiny.shinyapp.isConnected()) return false;
    return true;
  }

  function syncFavoritesToShiny() {
    if (!shinyReadyForFavSync()) return false;
    var store = rodajaGetFavorites();
    var json = JSON.stringify(store);
    if (json === lastSyncedJson) return true;
    Shiny.setInputValue("ec_favorites_json", json, { priority: "event" });
    lastSyncedJson = json;
    return true;
  }

  function syncFavoritesToShinyWithRetry(attempt, maxAttempts, delayMs) {
    attempt = attempt || 0;
    maxAttempts = maxAttempts || 30;
    delayMs = delayMs || 100;
    updateNavbarFavoriteBtn(false);
    if (syncFavoritesToShiny()) return;
    if (attempt >= maxAttempts) return;
    setTimeout(function () {
      syncFavoritesToShinyWithRetry(attempt + 1, maxAttempts, delayMs);
    }, delayMs);
  }

  function updateNavbarFavoriteBtn(flash) {
    var btn = document.getElementById("ec-btn-mi-estacion");
    if (!btn) return;
    var store = rodajaGetFavorites();
    var labelEl = btn.querySelector(".ec-btn-mi-estacion-label");
    if (labelEl) labelEl.textContent = " Mi estaci\u00f3n";
    if (store.primary) {
      var name = store.primary.alias || ("Estaci\u00f3n " + store.primary.eid);
      btn.title = "Ir al mapa y ver datos: " + name;
    } else {
      btn.title = "Configurar mi estaci\u00f3n favorita";
    }
    btn.classList.toggle("ec-has-favorite", !!store.primary);
    if (flash && store.primary) {
      btn.classList.add("ec-nav-fav-flash");
      clearTimeout(btn._flashT);
      btn._flashT = setTimeout(function () {
        btn.classList.remove("ec-nav-fav-flash");
      }, 1500);
    }
  }

  function initFavoritesUi(opts) {
    opts = opts || {};
    updateNavbarFavoriteBtn(!!opts.flash);
    if (opts.sync) syncFavoritesToShinyWithRetry();
  }

  function syncFavoritesAfterIdle() {
    syncFavoritesToShinyWithRetry();
  }

  function triggerShinyInput(name) {
    syncFavoritesToShiny();
    if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
      Shiny.setInputValue(name, Date.now(), { priority: "event" });
    }
  }

  global.ecNavIrMiEstacion = function () {
    triggerShinyInput("abrir_mi_estacion");
    return false;
  };

  global.ecNavVerReporteMiEstacion = function () {
    triggerShinyInput("abrir_reporte_mi_estacion");
    return false;
  };

  global.ecNavVerPronosticoMiEstacion = function () {
    var fav = rodajaGetFavorites().primary;
    if (!fav || !fav.eid) {
      toast("Fija una estaci\u00f3n favorita primero", false);
      return false;
    }
    var url = "/mi-reporte/?view=pronostico&e=" + encodeURIComponent(fav.eid);
    if (fav.variableId) url += "&v=" + encodeURIComponent(fav.variableId);
    window.open(url, "_blank");
    return false;
  };

  global.ecFijarDesdeMapa = function (eid, alias) {
    if (typeof global.ecPopupRequireAuthOrRun === "function") {
      var ok = global.ecPopupRequireAuthOrRun(function () { return true; });
      if (ok === false) return false;
    } else if (typeof global.rodajaIsSignedIn === "function" && !global.rodajaIsSignedIn()) {
      if (typeof global.rodajaSignInUrl === "function") {
        window.location.href = global.rodajaSignInUrl(window.location.href);
      }
      return false;
    }
    eid = parseInt(eid, 10);
    if (!eid || eid < 1) {
      toast("No se pudo guardar la estaci\u00f3n", false);
      return false;
    }
    var prevEid = (rodajaGetFavorites().primary || {}).eid;
    var payload = { eid: eid, alias: alias != null ? String(alias) : "" };
    if (rodajaSetPrimary(payload)) {
      initFavoritesUi({ flash: prevEid !== eid, sync: true });
      if (prevEid !== eid) {
        toast("Estaci\u00f3n fijada como favorita", true);
      }
    } else {
      toast("No se pudo guardar la estaci\u00f3n", false);
      return false;
    }
    if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
      Shiny.setInputValue("fijar_favorito_mapa", eid, { priority: "event" });
    }
    return false;
  };

  global.rodajaGetFavorites = rodajaGetFavorites;
  global.rodajaSetPrimary = rodajaSetPrimary;
  global.rodajaClearPrimary = rodajaClearPrimary;
  global.rodajaBuildExploradorUrl = buildExploradorUrl;
  global.rodajaBuildMiReporteUrl = buildMiReporteUrl;
  global.rodajaParseExploradorSearch = parseExploradorSearch;
  global.rodajaSyncExploradorUrl = syncExploradorUrl;
  global.rodajaClearReportUrlParams = clearReportUrlParams;
  global.rodajaCopyReportLink = copyReportLink;
  global.rodajaToast = toast;
  global.ecInitFavoritesUi = initFavoritesUi;

  global.rodajaCopiarEnlaceReporte = function () {
    var opts = getReportLinkOpts();
    if (!opts || !opts.eid) {
      toast("Abre un reporte de estaci\u00f3n antes de copiar el enlace", false);
      return false;
    }
    copyReportLink(opts, function (ok) {
      toast(ok ? "Enlace copiado al portapapeles" : "No se pudo copiar", ok);
    });
    return false;
  };

  function applyFijarFavoritoMsg(msg, showToast) {
    var fav = unwrapShinyMsg(msg);
    var prevEid = (rodajaGetFavorites().primary || {}).eid;
    var norm = normalizeFav(fav);
    if (rodajaSetPrimary(fav)) {
      initFavoritesUi({ flash: !!(norm && norm.eid !== prevEid), sync: true });
      if (showToast !== false && norm && norm.eid !== prevEid) {
        toast("Estaci\u00f3n fijada como favorita", true);
      }
      return true;
    }
    if (showToast !== false) {
      toast("No se pudo guardar la estaci\u00f3n", false);
    }
    return false;
  }

  function safeAddHandler(name, fn) {
    try {
      Shiny.addCustomMessageHandler(name, fn);
      return true;
    } catch (err) {
      var msg = String(err && err.message ? err.message : err);
      if (msg.indexOf("Duplicate") >= 0) return true;
      throw err;
    }
  }

  var favMapMarker = null;
  var favMapBound = null;
  var lastFavPaint = null;
  var favRemountTimer = null;
  var FAV_PANE = "ecFavGeo";

  function favMap() {
    var mount = document.getElementById("mapa_mount");
    if (!mount) return null;
    var root = mount.querySelector("[id$='-mapa']") || mount.querySelector(".leaflet-container");
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

  function dropFavMapMarker() {
    if (favMapMarker && favMapBound) {
      try { favMapBound.removeLayer(favMapMarker); } catch (eD) { /* ignore */ }
    }
    favMapMarker = null;
    favMapBound = null;
  }

  function paintFavMapMarker(msg) {
    msg = unwrapShinyMsg(msg) || {};
    if (msg.clear) {
      lastFavPaint = null;
      dropFavMapMarker();
      return;
    }
    var lat = parseFloat(msg.lat);
    var lon = parseFloat(msg.lon);
    if (!isFinite(lat) || !isFinite(lon)) {
      lastFavPaint = null;
      dropFavMapMarker();
      return;
    }
    lastFavPaint = { lat: lat, lon: lon };
    var map = favMap();
    if (!map || typeof L === "undefined" || !map.getContainer || !document.body.contains(map.getContainer())) {
      if (!favRemountTimer) {
        favRemountTimer = setTimeout(function () {
          favRemountTimer = null;
          if (lastFavPaint) paintFavMapMarker(lastFavPaint);
        }, 250);
      }
      return;
    }
    if (!map.getPane(FAV_PANE)) {
      map.createPane(FAV_PANE);
      map.getPane(FAV_PANE).style.zIndex = "430";
    }
    var ll = L.latLng(lat, lon);
    if (favMapBound !== map || !favMapMarker) {
      dropFavMapMarker();
      favMapBound = map;
      favMapMarker = L.marker(ll, {
        pane: FAV_PANE,
        keyboard: false,
        interactive: false,
        title: "Estaci\u00f3n favorita",
        alt: "Estaci\u00f3n favorita",
        zIndexOffset: 500,
        icon: L.divIcon({
          className: "ec-fav-geo-icon",
          html: '<div class="ec-fav-geo-wrap" title="Estaci\u00f3n favorita">' +
            '<span class="ec-fav-geo-pulse" aria-hidden="true"></span>' +
            '<span class="ec-fav-geo-dot" aria-hidden="true"></span></div>',
          iconSize: [48, 48],
          iconAnchor: [24, 24]
        })
      }).addTo(map);
    } else {
      favMapMarker.setLatLng(ll);
    }
  }

  function paintFavFromStore() {
    var p = rodajaGetFavorites().primary;
    if (!p || p.lat == null || p.lon == null) return;
    paintFavMapMarker({ lat: p.lat, lon: p.lon });
  }

  function registerShinyFavHandlers() {
    if (shinyHandlersRegistered) return true;
    if (typeof Shiny === "undefined" || !Shiny.addCustomMessageHandler) return false;

    try {
    safeAddHandler("ec_favorites_refresh", function () {
      updateNavbarFavoriteBtn(false);
    });

    safeAddHandler("ec_url_sync", function (msg) {
      if (!msg || msg.reporte !== false) return;
      syncExploradorUrl({
        eid: msg.eid,
        variableId: msg.variableId,
        fuenteId: msg.fuenteId,
        reporte: false
      });
    });

    safeAddHandler("ec_url_clear_report", function () {
      clearReportUrlParams();
    });

    safeAddHandler("ec_copy_report_link", function (msg) {
      var m = unwrapShinyMsg(msg) || {};
      if (!m.eid) m = getReportLinkOpts() || m;
      if (!m || !m.eid) {
        toast("No hay estaci\u00f3n para copiar en el enlace", false);
        return;
      }
      m.reporte = m.reporte !== false;
      copyReportLink(m, function (ok) {
        toast(ok ? "Enlace copiado al portapapeles" : "No se pudo copiar", ok);
      });
    });

    safeAddHandler("ec_fav_toast", function (msg) {
      toast(msg && msg.text ? msg.text : "Listo", msg && msg.ok !== false);
    });

    safeAddHandler("ec_fijar_favorito", function (msg) {
      applyFijarFavoritoMsg(msg, true);
    });

    safeAddHandler("ec_pintar_favorito", function (msg) {
      paintFavMapMarker(msg);
    });

    safeAddHandler("ec_update_mi_reporte_link", function (msg) {
      var a = document.getElementById("ec_link_mi_reporte");
      if (a && msg) a.href = buildMiReporteUrl(msg);
    });

    safeAddHandler("ec_wizard_open", function () {
      if (Shiny.setInputValue) {
        Shiny.setInputValue("wizard_open", Date.now(), { priority: "event" });
      }
    });

    safeAddHandler("ec_wizard_request_geo", function (msg) {
      var req = global.rodajaRequestGeolocation;
      if (typeof req !== "function") {
        toast("Geolocalizaci\u00f3n no disponible", false);
        return;
      }
      req(function (ok, data) {
        if (ok && data && data.lat != null) {
          Shiny.setInputValue("wizard_geo_lat", data.lat, { priority: "event" });
          Shiny.setInputValue("wizard_geo_lon", data.lon, { priority: "event" });
        } else {
          toast(typeof data === "string" ? data : "No se pudo obtener ubicaci\u00f3n", false);
        }
      });
    });

    shinyHandlersRegistered = true;
    } catch (err) {
      return false;
    }

    return true;
  }

  function bootShinyFavorites() {
    if (!registerShinyFavHandlers() && bootRetries < 30) {
      bootRetries++;
      setTimeout(bootShinyFavorites, 100);
      return;
    }
    updateNavbarFavoriteBtn(false);
    paintFavFromStore();
  }

  global.addEventListener("rodaja:favorites-changed", function () {
    initFavoritesUi({ sync: true });
    paintFavFromStore();
  });

  global.addEventListener("pageshow", function (ev) {
    if (ev && ev.persisted) shinyHandlersRegistered = false;
    updateNavbarFavoriteBtn(false);
    if (shinyReadyForFavSync()) syncFavoritesToShinyWithRetry();
  });

  if (typeof jQuery !== "undefined") {
    jQuery(document).on("shiny:connected", bootShinyFavorites);
    jQuery(document).one("shiny:idle", syncFavoritesAfterIdle);
  } else {
    document.addEventListener("shiny:connected", bootShinyFavorites);
    document.addEventListener("shiny:idle", syncFavoritesAfterIdle, { once: true });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () {
      updateNavbarFavoriteBtn(false);
    });
  } else {
    updateNavbarFavoriteBtn(false);
  }
})(window);
