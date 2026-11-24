/* rodaja.cl — grupo de estaciones (persistencia local + sync liviano) */
(function (global) {
  "use strict";

  var STORAGE_KEY = "rodaja_station_group_v1";
  var SNAPSHOT_KEY = "rodaja_dl_group_snapshot";
  var DEFAULT_GROUP_NAME = "Mi grupo";

  function parseJson(s, fallback) {
    try { return JSON.parse(s); } catch (e) { return fallback; }
  }

  function emptyGroup() {
    return { nombre: DEFAULT_GROUP_NAME, ids: [], updated_at: null };
  }

  function normalizeIds(ids) {
    if (!ids) return [];
    var arr = Array.isArray(ids) ? ids : [ids];
    var out = [];
    var seen = {};
    arr.forEach(function (x) {
      var n = parseInt(x, 10);
      if (!n || n < 1 || seen[n]) return;
      seen[n] = true;
      out.push(n);
    });
    return out;
  }

  function rodajaGetStationGroup() {
    try {
      var raw = localStorage.getItem(STORAGE_KEY);
      if (!raw) return emptyGroup();
      var o = parseJson(raw, emptyGroup());
      if (!o || typeof o !== "object") return emptyGroup();
      return {
        nombre: String(o.nombre || DEFAULT_GROUP_NAME),
        ids: normalizeIds(o.ids),
        updated_at: o.updated_at || null
      };
    } catch (e) {
      return emptyGroup();
    }
  }

  function rodajaSaveStationGroup(store) {
    store = store || emptyGroup();
    store.ids = normalizeIds(store.ids);
    store.updated_at = new Date().toISOString();
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(store));
    } catch (e) {
      return false;
    }
    try {
      global.dispatchEvent(new CustomEvent("rodaja:station-group-changed", { detail: store }));
    } catch (e2) { /* ignore */ }
    if (typeof global.rodajaSyncStationGroupToCloud === "function" &&
        typeof global.rodajaIsSignedIn === "function" && global.rodajaIsSignedIn()) {
      global.rodajaSyncStationGroupToCloud(store);
    }
    if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
      Shiny.setInputValue("ec_station_group_json", JSON.stringify(store), { priority: "event" });
    }
    return true;
  }

  function rodajaSetStationGroup(store) {
    return rodajaSaveStationGroup(store);
  }

  function rodajaAddToStationGroup(ids) {
    var g = rodajaGetStationGroup();
    var merged = normalizeIds(g.ids.concat(ids || []));
    g.ids = merged;
    return rodajaSaveStationGroup(g);
  }

  function rodajaRemoveFromStationGroup(id) {
    var g = rodajaGetStationGroup();
    var n = parseInt(id, 10);
    g.ids = g.ids.filter(function (x) { return x !== n; });
    return rodajaSaveStationGroup(g);
  }

  function rodajaClearStationGroup() {
    var g = rodajaGetStationGroup();
    g.ids = [];
    return rodajaSaveStationGroup(g);
  }

  function rodajaSnapshotGroupForDownload() {
    try {
      sessionStorage.setItem(SNAPSHOT_KEY, JSON.stringify(rodajaGetStationGroup()));
    } catch (e) { /* ignore */ }
  }

  function rodajaOpenDownloadWithGroup() {
    rodajaSnapshotGroupForDownload();
    global.open("/descargar/index.html?from=grupo", "_blank");
  }

  function rodajaLoadStationGroupFromCloudIfNeeded() {
    if (typeof global.rodajaLoadStationGroupFromCloud !== "function" ||
        typeof global.rodajaIsSignedIn !== "function" || !global.rodajaIsSignedIn()) {
      return Promise.resolve(false);
    }
    return global.rodajaLoadStationGroupFromCloud().then(function (cloud) {
      if (!cloud) return false;
      var local = rodajaGetStationGroup();
      var cloudTs = cloud.updated_at ? Date.parse(cloud.updated_at) : 0;
      var localTs = local.updated_at ? Date.parse(local.updated_at) : 0;
      if (cloudTs > localTs) {
        rodajaSaveStationGroup({
          nombre: cloud.nombre || local.nombre,
          ids: cloud.ids || local.ids,
          updated_at: cloud.updated_at
        });
        return true;
      }
      return false;
    });
  }

  function stripGrupoMapWidget() {
    document.querySelectorAll(".ec-nav-grupo-wrap, .ec-auth-grupo-slot").forEach(function (el) {
      if (el && el.parentNode) el.parentNode.removeChild(el);
    });
    var leftover = document.getElementById("ec-btn-grupo-estaciones");
    if (leftover && leftover.parentNode) leftover.parentNode.removeChild(leftover);
  }

  function ecAgregarAlGrupoDesdeMapa(eid) {
    if (typeof global.ecPopupRequireAuthOrRun === "function") {
      var ok = global.ecPopupRequireAuthOrRun(function () { return true; });
      if (ok === false) return;
    } else if (typeof global.rodajaIsSignedIn === "function" && !global.rodajaIsSignedIn()) {
      if (typeof global.rodajaSignInUrl === "function") {
        window.location.href = global.rodajaSignInUrl(window.location.href);
      }
      return;
    }
    rodajaAddToStationGroup([parseInt(eid, 10)]);
  }

  global.ecAgregarAlGrupoDesdeMapa = ecAgregarAlGrupoDesdeMapa;
  global.rodajaGetStationGroup = rodajaGetStationGroup;
  global.rodajaSetStationGroup = rodajaSetStationGroup;
  global.rodajaAddToStationGroup = rodajaAddToStationGroup;
  global.rodajaRemoveFromStationGroup = rodajaRemoveFromStationGroup;
  global.rodajaClearStationGroup = rodajaClearStationGroup;
  global.rodajaSnapshotGroupForDownload = rodajaSnapshotGroupForDownload;
  global.rodajaOpenDownloadWithGroup = rodajaOpenDownloadWithGroup;
  global.ecAttachGrupoToAuthBar = stripGrupoMapWidget;

  function initBboxSelect() {
    if (typeof L === "undefined" || !L.Draw || !L.drawLocal) return;
    L.drawLocal.draw.handlers.rectangle.tooltip.start = "Arrastra en el mapa para seleccionar estaciones";
    L.drawLocal.draw.handlers.simpleshape.tooltip.end = "Suelta para confirmar";

    var active = false;

    function getMap() {
      var mount = document.getElementById("mapa_mount");
      if (!mount) return null;
      var root = mount.querySelector(".leaflet-container") || mount.querySelector("[id$='-mapa']");
      if (!root || !root._leaflet_map) return null;
      return root._leaflet_map;
    }

    function stationPointsInBbox(bounds) {
      var pts = global.ecMapStationPoints || [];
      return pts.filter(function (p) {
        return bounds.contains([p.lat, p.lng]);
      }).map(function (p) { return p.id; });
    }

    function startRectDraw(m) {
      active = true;
      var drawer = new L.Draw.Rectangle(m, {
        shapeOptions: { color: "#DF9531", weight: 2, fillOpacity: 0.15 }
      });
      drawer.enable();
      m.once(L.Draw.Event.CREATED, function (e) {
        active = false;
        var layer = e.layer;
        var bounds = layer.getBounds();
        m.removeLayer(layer);
        var pts = global.ecMapStationPoints || [];
        if (!pts.length) {
          alert("No hay estaciones cargadas en el mapa. Aplica filtros primero.");
          return;
        }
        var ids = stationPointsInBbox(bounds);
        if (!ids.length) {
          alert("No hay estaciones en el \u00e1rea seleccionada.");
          return;
        }
        if (confirm("Agregar " + ids.length + " estaci\u00f3n(es) al grupo?")) {
          rodajaAddToStationGroup(ids);
        }
        if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
          Shiny.setInputValue("mapa_bbox_select", {
            ids: ids,
            bounds: {
              north: bounds.getNorth(),
              south: bounds.getSouth(),
              east: bounds.getEast(),
              west: bounds.getWest()
            }
          }, { priority: "event" });
        }
      });
      m.once(L.Draw.Event.DRAWSTOP, function () { active = false; });
    }

    function enableRect(attempt) {
      if (active) return;
      attempt = attempt || 0;
      var m = getMap();
      if (!m) {
        if (attempt < 12) return setTimeout(function () { enableRect(attempt + 1); }, 250);
        alert("El mapa a\u00fan no est\u00e1 listo. Espera unos segundos e intenta de nuevo.");
        return;
      }
      if (typeof global.ecCloseDrawer === "function") global.ecCloseDrawer();
      setTimeout(function () { startRectDraw(m); }, 320);
    }

    var btn = document.getElementById("ec-btn-seleccion-rectangulo");
    if (!btn || btn.getAttribute("data-ec-bbox-wired") === "1") return;
    btn.setAttribute("data-ec-bbox-wired", "1");
    btn.addEventListener("click", function (e) {
      e.preventDefault();
      enableRect();
    });
  }

  function syncMapAlertaHorizonteCheckbox(checked) {
    wireMapAlertaHorizonte();
    var cb = document.getElementById("ec_map_alerta_horizonte_ext");
    if (!cb) return;
    cb.checked = !!checked;
  }

  function wireMapAlertaHorizonte() {
    var cb = document.getElementById("ec_map_alerta_horizonte_ext");
    if (!cb || cb.getAttribute("data-ec-horizonte-wired") === "1") return;
    cb.setAttribute("data-ec-horizonte-wired", "1");
    cb.addEventListener("change", function () {
      if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
        Shiny.setInputValue("mapa-alerta_horizonte_ext", cb.checked, { priority: "event" });
      }
    });
  }

  var alertaFilterKey = "";

  function getAlertaLegendPanel() {
    return document.getElementById("ec_map_legend_alerta");
  }

  function getAlertaLegendControl() {
    var panel = getAlertaLegendPanel();
    if (!panel) return null;
    return panel.closest(".ec-map-legend-alerta-ctrl") || panel.parentElement;
  }

  function placeAlertaLegendControl(active) {
    var ctrl = getAlertaLegendControl();
    if (!ctrl) return;
    var map = ctrl.closest(".leaflet-container");
    if (!map) return;
    var topRight = map.querySelector(".leaflet-top.leaflet-right");
    var bottomRight = map.querySelector(".leaflet-bottom.leaflet-right");
    if (active && topRight) {
      if (ctrl.parentElement !== topRight) topRight.appendChild(ctrl);
    } else if (bottomRight) {
      if (ctrl.parentElement !== bottomRight) bottomRight.appendChild(ctrl);
      // Mantener alertas encima de calidad, sin compartir caja.
      var quality = bottomRight.querySelector(".ec-map-legend-ctrl");
      if (quality && quality !== ctrl) bottomRight.insertBefore(ctrl, quality);
    }
  }

  function applyAlertaMarkerFilter(key) {
    var markers = document.querySelectorAll(
      ".ec-map-alerta-marker, .ec-om-pt-icon.has-alerta"
    );
    markers.forEach(function (el) {
      var match = !key || el.classList.contains("ec-map-alerta-bucket-" + key);
      el.classList.toggle("ec-map-alerta-dimmed", !match);
    });
  }

  function setAlertaFilter(key) {
    alertaFilterKey = key || "";
    var panel = getAlertaLegendPanel();
    if (panel) panel.setAttribute("data-alerta-filter", alertaFilterKey);
    var clearBtn = document.getElementById("ec_map_alerta_clear");
    if (clearBtn) clearBtn.hidden = !alertaFilterKey;
    document.querySelectorAll(".ec-map-legend-alerta-chip").forEach(function (chip) {
      var k = chip.getAttribute("data-alerta-key") || "";
      var on = !!alertaFilterKey && k === alertaFilterKey;
      chip.classList.toggle("is-filtered", on);
      chip.classList.toggle("is-dimmed", !!alertaFilterKey && !on);
      chip.setAttribute("aria-pressed", on ? "true" : "false");
    });
    applyAlertaMarkerFilter(alertaFilterKey);
    if (global.ecOmPoints && typeof global.ecOmPoints.refreshRange === "function") {
      global.ecOmPoints.refreshRange();
    }
  }

  function updateAlertaLegendPriority(counts) {
    var panel = getAlertaLegendPanel();
    if (!panel) return;
    var total = 0;
    Object.keys(counts || {}).forEach(function (k) {
      var n = parseInt(counts[k], 10);
      if (!isNaN(n) && n > 0) total += n;
    });
    var active = total > 0;
    panel.classList.toggle("ec-map-legend-alerta--active", active);
    placeAlertaLegendControl(active);
    if (active) panel.classList.add("ec-map-legend-alerta--expanded");
  }

  function applyAlertaLegendCounts(msg) {
    if (!msg || typeof msg !== "object") return;
    var known = { pp: 1, cold: 1, heat: 1, wind: 1 };
    Object.keys(msg).forEach(function (key) {
      if (!known[key]) return;
      var el = document.querySelector(".ec-map-legend-alerta-n[data-alerta-key=\"" + key + "\"]");
      if (!el) return;
      var n = parseInt(msg[key], 10);
      if (isNaN(n) || n < 0) n = 0;
      el.textContent = String(n);
      el.classList.toggle("ec-map-legend-alerta-n--zero", n === 0);
      el.classList.toggle("ec-map-legend-alerta-n--on", n > 0);
      var chip = el.closest(".ec-map-legend-alerta-chip");
      if (chip) chip.classList.toggle("has-alerts", n > 0);
    });
    updateAlertaLegendPriority(msg);
    // Reaplica filtro tras repintado de marcadores.
    applyAlertaMarkerFilter(alertaFilterKey);
  }

  function wireMapAlertaLegend() {
    var panel = getAlertaLegendPanel();
    if (!panel || panel.getAttribute("data-ec-alerta-legend-wired") === "1") return;
    panel.setAttribute("data-ec-alerta-legend-wired", "1");

    panel.addEventListener("click", function (e) {
      var expand = e.target.closest("#ec_map_alerta_expand");
      if (expand) {
        e.preventDefault();
        var open = panel.classList.toggle("ec-map-legend-alerta--expanded");
        expand.setAttribute("aria-expanded", open ? "true" : "false");
        return;
      }
      var clear = e.target.closest("#ec_map_alerta_clear");
      if (clear) {
        e.preventDefault();
        setAlertaFilter("");
        return;
      }
      var chip = e.target.closest(".ec-map-legend-alerta-chip");
      if (!chip || !panel.contains(chip)) return;
      e.preventDefault();
      var key = chip.getAttribute("data-alerta-key") || "";
      if (!key) return;
      setAlertaFilter(alertaFilterKey === key ? "" : key);
    });
  }

  function initMapAlertaHorizonte(attempt) {
    attempt = attempt || 0;
    wireMapAlertaHorizonte();
    wireMapAlertaLegend();
    if (!document.getElementById("ec_map_alerta_horizonte_ext") && attempt < 24) {
      setTimeout(function () { initMapAlertaHorizonte(attempt + 1); }, 500);
    }
  }

  function boot() {
    stripGrupoMapWidget();
    if (typeof Shiny !== "undefined" && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler("ec_map_station_points", function (msg) {
        global.ecMapStationPoints = msg || [];
        if (global.ecOmPoints && typeof global.ecOmPoints.sync === "function") {
          global.ecOmPoints.sync({});
        }
      });
      Shiny.addCustomMessageHandler("ec_map_alerta_legend_counts", function (msg) {
        wireMapAlertaLegend();
        applyAlertaLegendCounts(msg);
      });
      Shiny.addCustomMessageHandler("ec_map_alerta_horizonte_sync", function (msg) {
        syncMapAlertaHorizonteCheckbox(msg && msg.checked);
      });
      Shiny.addCustomMessageHandler("ec_open_download_filtered", function (msg) {
        var ids = normalizeIds(msg && msg.ids);
        if (!ids.length) return;
        try {
          sessionStorage.setItem(SNAPSHOT_KEY, JSON.stringify({
            nombre: (msg && msg.nombre) || "Explorador filtrado",
            ids: ids
          }));
        } catch (e) { return; }
        global.open("/descargar/index.html?from=grupo", "_blank");
      });
      Shiny.addCustomMessageHandler("ec_station_group_add_ids", function (ids) {
        rodajaAddToStationGroup(ids || []);
      });
    }
    rodajaLoadStationGroupFromCloudIfNeeded();
    setTimeout(initBboxSelect, 1500);
    initMapAlertaHorizonte();
  }

  if (typeof global.rodajaOnAuthChange === "function") {
    global.rodajaOnAuthChange(function (user) {
      if (user) rodajaLoadStationGroupFromCloudIfNeeded();
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    boot();
  }
  document.addEventListener("shiny:connected", function () {
    stripGrupoMapWidget();
    rodajaLoadStationGroupFromCloudIfNeeded();
    setTimeout(initBboxSelect, 2000);
    initMapAlertaHorizonte();
  });
  document.addEventListener("ec-leaflet-draw-ready", function () {
    setTimeout(initBboxSelect, 0);
  });
})(window);
