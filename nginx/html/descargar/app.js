(function (global) {
  "use strict";

  var JOB_POLL_MS = 2000;
  var JOB_MAX_WAIT_MS = 600000;
  var SNAPSHOT_KEY = "rodaja_dl_group_snapshot";

  var state = {
    step: 1,
    eids: [],
    vids: [],
    granularidades: ["anual"],
    variablesCatalog: [],
    stationsCatalog: [],
    stationsById: {},
    regionsCatalog: [],
    stationFilters: {
      macrozona: "",
      region: "",
      comuna: "",
      cuenca: "",
      subcuenca: "",
      fuenteId: "",
      variableId: ""
    },
    stationSearch: "",
    groupSearch: "",
    varSearch: "",
    varOpen: {},
    varFocus: "",
    varSpecs: {},
    exploreTab: "lista",
    explorePage: 0,
    groupPage: 0,
    pageSize: 10,
    advancedOpen: false,
    yearStart: null,
    yearEnd: null,
    formatoDescarga: "liviano",
    formatos: ["liviano", "completo"],
    granularidadesAllowed: ["anual", "mensual", "diario", "horaria"],
    modoCobertura: "solo_con_datos",
    cobertura: null,
    cuota: null,
    tier: "gratuito",
    estimate: null,
    estimateTimer: null,
    estimateSeq: 0,
    estimateAbort: null,
    coberturaTimer: null,
    coberturaSeq: 0,
    varStations: null,
    nombreLote: "Mi descarga"
  };

  var bootPromise = null;

  function qs(name) {
    return new URLSearchParams(global.location.search).get(name);
  }

  function escapeHtml(s) {
    return String(s)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  /** Plumber/R a veces serializa escalares como arrays JSON, ej. estado: ["listo"] */
  function apiScalar(v) {
    if (v == null) return v;
    return Array.isArray(v) ? (v.length ? v[0] : null) : v;
  }

  function show(el) { if (el) el.hidden = false; }
  function hide(el) { if (el) el.hidden = true; }

  function apiUrl(path) {
    var base = ((global.RODAJA_CONFIG || {}).apiBase || "/api").replace(/\/$/, "");
    return base + (path.charAt(0) === "/" ? path : "/" + path);
  }

  function apiFetchResponse(path) {
    if (typeof rodajaApiFetch === "function") return rodajaApiFetch(path);
    return fetch(apiUrl(path), { headers: { Accept: "application/json" } });
  }

  function parseJsonResponse(label, r) {
    if (!r || typeof r.text !== "function") return Promise.resolve(r);
    return r.text().then(function (text) {
      if (!r.ok) {
        var msg = "HTTP " + r.status;
        try {
          var errBody = JSON.parse(text);
          msg = (errBody && errBody.error) || msg;
        } catch (e) { /* HTML o texto plano */ }
        throw new Error(String(msg) + " (" + label + ")");
      }
      try {
        return JSON.parse(text);
      } catch (e) {
        throw new Error("Respuesta no JSON en " + label);
      }
    });
  }

  function formatBytes(n) {
    n = Number(n);
    if (!n || isNaN(n) || n < 0) return "0 B";
    var units = ["B", "KB", "MB", "GB"];
    var i = 0;
    var v = n;
    while (v >= 1024 && i < units.length - 1) { v /= 1024; i++; }
    return v.toFixed(i > 0 && v < 10 ? 1 : 0) + " " + units[i];
  }

  function fechaFromYears(yStart, yEnd) {
    return { inicio: yStart + "-01-01", fin: yEnd + "-12-31" };
  }

  function normalizeIds(arr) {
    if (!arr) return [];
    var out = [];
    var seen = {};
    (Array.isArray(arr) ? arr : String(arr).split(/[,\s]+/)).forEach(function (x) {
      var n = parseInt(x, 10);
      if (!n || n < 1 || seen[n]) return;
      seen[n] = true;
      out.push(n);
    });
    return out;
  }

  var PAGE_SIZES = [10, 25, 50, 100];
  // Misma partición que Inicio: Atacama y Coquimbo son Centro Norte, no Norte ni Centro.
  var MACROZONAS = [
    { id: "Norte", cods: [15, 1, 2] },
    { id: "Centro Norte", cods: [3, 4] },
    { id: "Centro", cods: [5, 13, 6, 7] },
    { id: "Sur", cods: [16, 8, 9, 14, 10] },
    { id: "Austral", cods: [11, 12] }
  ];
  var MACROZONA_CODE = { N: "Norte", C: "Centro", S: "Sur", A: "Austral" };

  function rebuildStationMap() {
    state.stationsById = {};
    state.stationsCatalog.forEach(function (s) {
      state.stationsById[s.id] = s;
    });
  }

  function stationById(id) {
    return state.stationsById[id] || null;
  }

  function persistGroupSnapshot() {
    try {
      sessionStorage.setItem(SNAPSHOT_KEY, JSON.stringify({
        nombre: state.nombreLote,
        ids: state.eids
      }));
    } catch (e) { /* ignore */ }
  }

  function addStations(ids) {
    state.eids = normalizeIds(state.eids.concat(ids || []));
    persistGroupSnapshot();
  }

  function removeStation(id) {
    state.eids = state.eids.filter(function (x) { return x !== id; });
    persistGroupSnapshot();
  }

  function clearGroup() {
    state.eids = [];
    persistGroupSnapshot();
  }

  function toggleStationInGroup(id, add) {
    if (add) {
      if (state.eids.indexOf(id) >= 0) return;
      state.eids.push(id);
    } else {
      removeStation(id);
      return;
    }
    persistGroupSnapshot();
  }

  function uniqSorted(arr) {
    var seen = {};
    var out = [];
    (arr || []).forEach(function (v) {
      v = String(v || "").trim();
      if (!v || seen[v]) return;
      seen[v] = true;
      out.push(v);
    });
    return out.sort(function (a, b) { return a.localeCompare(b, "es"); });
  }

  function textOf(v) {
    v = apiScalar(v);
    if (v == null) return "";
    return String(v).trim();
  }

  function foldText(s) {
    return String(s || "").toLowerCase().normalize("NFD").replace(/[\u0300-\u036f]/g, "");
  }

  // Nivel 2 = variable f\u00edsica. Nivel 3 (2 m, 10 m) solo si hay m\u00e1s de una medici\u00f3n.
  var VAR_GROUPS = [
    { id: "temperatura", label: "Temperatura", icon: "heat", nombres: ["temperatura_media", "temperatura_maxima", "temperatura_minima", "horas_frio"] },
    { id: "precipitacion", label: "Precipitaci\u00f3n", icon: "rain", nombres: ["precipitacion"] },
    { id: "viento", label: "Viento", icon: "wind", nombres: ["viento_medio", "viento_maximo", "viento_minimo", "viento_direccion_dga", "viento_velocidad", "viento_direccion", "viento_maximo_dmc", "viento_rafaga_horaria"] },
    { id: "atmosfera", label: "Atm\u00f3sfera", icon: "atm", nombres: ["humedad_relativa_media", "presion", "radiacion_solar_media", "cloud_octas", "Condicion meteorologica"] },
    { id: "hidrologia", label: "Hidrolog\u00eda", icon: "drop", nombres: ["caudal", "niveles_pozos"] },
    { id: "aire", label: "Calidad del aire", icon: "leaf", nombres: ["material_particulado_2_5"] }
  ];

  var VAR_FACE = {
    precipitacion: { label: "Precipitaci\u00f3n", chip: "Precipitaci\u00f3n" },
    temperatura_media: { label: "Temperatura media", chip: "Temperatura media" },
    temperatura_maxima: { label: "Temperatura m\u00e1xima", chip: "Temperatura m\u00e1xima" },
    temperatura_minima: { label: "Temperatura m\u00ednima", chip: "Temperatura m\u00ednima" },
    horas_frio: { label: "Horas de fr\u00edo", chip: "Horas de fr\u00edo" },
    caudal: { label: "Caudal", chip: "Caudal" },
    niveles_pozos: { label: "Nivel de pozos", chip: "Nivel de pozos" },
    viento_medio: { label: "Velocidad media", chip: "Viento \u00b7 media" },
    viento_maximo: { label: "Velocidad m\u00e1xima", chip: "Viento \u00b7 m\u00e1xima" },
    viento_minimo: { label: "Velocidad m\u00ednima", chip: "Viento \u00b7 m\u00ednima" },
    viento_maximo_dmc: { label: "Velocidad m\u00e1xima", chip: "Viento \u00b7 m\u00e1xima" },
    viento_velocidad: { label: "Direcci\u00f3n predominante", chip: "Direcci\u00f3n predominante" },
    viento_direccion: { label: "Direcci\u00f3n predominante", chip: "Direcci\u00f3n predominante", unit: "\u00b0" },
    viento_direccion_dga: { label: "Direcci\u00f3n predominante", chip: "Direcci\u00f3n predominante", unit: "\u00b0" },
    viento_rafaga_horaria: { label: "R\u00e1faga horaria", chip: "R\u00e1faga horaria", unit: "km/h" },
    humedad_relativa_media: { label: "Humedad relativa media", chip: "Humedad relativa media" },
    presion: { label: "Presi\u00f3n atmosf\u00e9rica", chip: "Presi\u00f3n atmosf\u00e9rica", unit: "hPa" },
    radiacion_solar_media: { label: "Radiaci\u00f3n solar media", chip: "Radiaci\u00f3n solar media" },
    cloud_octas: { label: "Nubosidad", chip: "Nubosidad", unit: "octas" },
    "Condicion meteorologica": { label: "Condici\u00f3n meteorol\u00f3gica", chip: "Condici\u00f3n meteorol\u00f3gica", unit: "\u2014" },
    material_particulado_2_5: { label: "Material particulado 2,5", chip: "Material particulado 2,5" }
  };

  // phys agrupa la variable f\u00edsica. variant es el tercer nivel (altura u otra medici\u00f3n).
  var VAR_PHYS = {
    viento_medio: { phys: "v_media", variant: "2 m" },
    viento_minimo: { phys: "v_min", variant: "2 m" },
    viento_maximo: { phys: "v_max", variant: "2 m" },
    viento_maximo_dmc: { phys: "v_max", variant: "10 m" },
    viento_direccion_dga: { phys: "v_dir", variant: "2 m" },
    viento_direccion: { phys: "v_dir", variant: "10 m" },
    viento_velocidad: { phys: "v_dir", variant: "10 m" }
  };

  function prettyUnit(raw) {
    var u = String(raw || "").trim();
    if (!u || u === "s/u") return "";
    if (u === "m3/s") return "m\u00b3/s";
    if (u === "watt/m2") return "W/m\u00b2";
    if (u === "\u00b5g/m3" || u === "ug/m3") return "\u00b5g/m\u00b3";
    if (u === "grados") return "\u00b0";
    if (u === "wmo") return "\u2014";
    return u;
  }

  function varKey(v) {
    return String((v && (v.nombre || v.nombre_abr)) || "");
  }

  function varFace(v) {
    var key = varKey(v);
    var face = VAR_FACE[key] || null;
    var label = face ? face.label : (key || ("Variable " + (v && v.id)));
    return {
      label: label,
      chip: face && face.chip ? face.chip : label,
      unit: face && face.unit != null ? face.unit : prettyUnit(v && v.unidad)
    };
  }

  function seriesUnit(v) {
    var face = varFace(v);
    return face.unit || prettyUnit(v && v.unidad);
  }

  function pushUnit(list, unit) {
    if (unit && list.indexOf(unit) < 0) list.push(unit);
  }

  function itemMatches(item, catLabel, q) {
    if (!q) return true;
    var parts = [item.face.label, item.face.chip, item.face.unit, catLabel];
    item.variants.forEach(function (slot) { parts.push(slot.label, slot.unit); });
    item.vars.forEach(function (v) { parts.push(varKey(v), v.nombre_abr || ""); });
    return foldText(parts.join(" ")).indexOf(q) >= 0;
  }

  function blankItem(phys, face, v) {
    return { phys: phys, face: face, vars: [], variants: [], v: v };
  }

  function addSeries(item, v, variantLabel) {
    item.vars.push(v);
    var key = variantLabel || "";
    var slot = null;
    var i;
    for (i = 0; i < item.variants.length; i++) if (item.variants[i].key === key) slot = item.variants[i];
    if (!slot) {
      slot = { key: key, label: key, units: [], unit: "", vars: [] };
      item.variants.push(slot);
    }
    slot.vars.push(v);
    pushUnit(slot.units, seriesUnit(v));
    slot.unit = slot.units.join(" \u00b7 ");
  }

  function finishItem(item) {
    var units = [];
    item.variants.forEach(function (slot) { pushUnit(units, slot.unit); });
    if (units.length === 1) item.face.unit = units[0];
    item.v = item.vars[0];
    return item;
  }

  function groupVariables(catalog, query) {
    var q = foldText(query).trim();
    var used = {};
    function take(nombre, items) {
      (catalog || []).forEach(function (v) {
        if (varKey(v) !== nombre || used[v.id]) return;
        used[v.id] = true;
        var spec = VAR_PHYS[nombre];
        var face = varFace(v);
        var phys = spec ? spec.phys : ("id:" + v.id);
        var existing = null;
        var i;
        for (i = 0; i < items.length; i++) if (items[i].phys === phys) existing = items[i];
        if (!existing) {
          existing = blankItem(phys, { label: face.label, chip: face.chip, unit: "" }, v);
          items.push(existing);
        }
        addSeries(existing, v, spec ? spec.variant : "");
      });
    }
    function pack(items, catLabel) {
      var out = [];
      items.forEach(function (it) {
        finishItem(it);
        if (itemMatches(it, catLabel, q)) out.push(it);
      });
      return out;
    }
    var groups = VAR_GROUPS.map(function (g) {
      var items = [];
      g.nombres.forEach(function (nombre) { take(nombre, items); });
      return { id: g.id, label: g.label, icon: g.icon, items: pack(items, g.label) };
    }).filter(function (g) { return g.items.length; });
    var otras = [];
    (catalog || []).forEach(function (v) {
      if (used[v.id]) return;
      var item = blankItem("id:" + v.id, { label: varFace(v).label, chip: varFace(v).chip, unit: "" }, v);
      addSeries(item, v, "");
      otras.push(item);
    });
    otras = pack(otras, "Otras");
    if (otras.length) groups.push({ id: "otras", label: "Otras", icon: "atm", items: otras });
    return groups;
  }

  function eachPhysical(fn) {
    groupVariables(state.variablesCatalog, "").forEach(function (g) {
      g.items.forEach(fn);
    });
  }

  function variableFilterOptions() {
    var out = [];
    eachPhysical(function (it) {
      out.push({ id: it.vars.map(function (v) { return v.id; }).join(","), nombre: it.face.label });
    });
    return out;
  }

  function varPickGroups() {
    var selected = {};
    state.vids.forEach(function (id) { selected[id] = true; });
    var out = [];
    eachPhysical(function (it) {
      var ids = [];
      var labels = [];
      it.variants.forEach(function (slot) {
        var on = slot.vars.filter(function (v) { return selected[v.id]; });
        if (!on.length) return;
        on.forEach(function (v) { ids.push(v.id); });
        if (slot.label) labels.push(slot.label);
      });
      if (!ids.length) return;
      var all = it.vars.every(function (v) { return selected[v.id]; });
      var chip = it.face.chip;
      if (!all && it.variants.length > 1 && labels.length) chip += " \u00b7 " + labels.join(", ");
      out.push({ ids: ids, chip: chip });
    });
    return out;
  }

  function parseVidList(raw) {
    return String(raw || "").split(",").map(function (x) { return parseInt(x, 10); }).filter(function (n) { return n > 0; });
  }

  function syncVarBoxes(root) {
    if (!root) return;
    root.querySelectorAll(".dl-var-cb").forEach(function (cb) {
      var ids = parseVidList(cb.getAttribute("data-vid"));
      var n = ids.filter(function (id) { return state.vids.indexOf(id) >= 0; }).length;
      var parent = cb.classList.contains("is-parent");
      cb.checked = n === ids.length && n > 0;
      cb.indeterminate = parent && n > 0 && n < ids.length;
      var row = cb.closest(".dl-var-sub") || cb.closest(".dl-var-opt");
      if (row) row.classList.toggle("is-on", n > 0);
    });
  }

  function varIcon(id) {
    var paths = {
      heat: '<circle cx="12" cy="12" r="4"/><path d="M12 2v2M12 20v2M4.9 4.9l1.4 1.4M17.7 17.7l1.4 1.4M2 12h2M20 12h2M4.9 19.1l1.4-1.4M17.7 6.3l1.4-1.4"/>',
      rain: '<path d="M7 14a4 4 0 0 1 .8-7.9A5 5 0 0 1 17 8a3.5 3.5 0 0 1-.6 7H7z"/><path d="M8 18l-1 2M12 18l-1 2M16 18l-1 2"/>',
      wind: '<path d="M3 8h11a3 3 0 1 0-3-3M3 16h13a3 3 0 1 1-3 3M3 12h7"/>',
      atm: '<path d="M6 16a4 4 0 0 1 .6-8A5 5 0 0 1 17 9.5 3.2 3.2 0 0 1 16.5 16H6z"/>',
      drop: '<path d="M12 3s5 6 5 9.5a5 5 0 1 1-10 0C7 9 12 3 12 3z"/>',
      leaf: '<path d="M5 19c8 0 14-5 14-14C11 5 5 10 5 19z"/><path d="M5 19c2-5 5-8 10-10"/>'
    };
    return '<svg class="dl-var-ico" viewBox="0 0 24 24" aria-hidden="true">' + (paths[id] || paths.atm) + "</svg>";
  }

  function applyStationFilters(catalog, filters, search) {
    var q = foldText(search).trim();
    return catalog.filter(function (s) {
      if (filters.macrozona && stationMacrozona(s) !== filters.macrozona) return false;
      if (filters.region && textOf(s.cod_reg) !== String(filters.region)) return false;
      if (filters.comuna && textOf(s.nom_com) !== filters.comuna) return false;
      if (filters.cuenca && textOf(s.cuenca) !== filters.cuenca) return false;
      if (filters.subcuenca && textOf(s.subcuenca) !== filters.subcuenca) return false;
      if (filters.fuenteId && stationFuenteId(s) !== String(filters.fuenteId)) return false;
      if (filters.variableId && state.varStations) {
        var allowIds = String(filters.variableId).split(",");
        var stationOk = false;
        var ai;
        for (ai = 0; ai < allowIds.length; ai++) {
          var allow = state.varStations[allowIds[ai]];
          if (allow && allow[s.id]) stationOk = true;
        }
        if (!stationOk) return false;
      }
      if (q) {
        var hay = foldText([s.nombre, s.nom_reg, s.nom_com, s.cuenca, s.subcuenca, stationMacrozona(s), fuenteLabel(s), s.id].join(" "));
        if (hay.indexOf(q) < 0) return false;
      }
      return true;
    });
  }

  function filtersCopy(filters, overrides) {
    var copy = {
      macrozona: filters.macrozona || "",
      region: filters.region || "",
      comuna: filters.comuna || "",
      cuenca: filters.cuenca || "",
      subcuenca: filters.subcuenca || "",
      fuenteId: filters.fuenteId || "",
      variableId: filters.variableId || ""
    };
    if (overrides) Object.keys(overrides).forEach(function (k) { copy[k] = overrides[k]; });
    return copy;
  }

  function poolStations(catalog, filters, overrides) {
    return applyStationFilters(catalog, filtersCopy(filters, overrides), "");
  }

  function reconcileStationFilters() {
    var f = state.stationFilters;
    var catalog = state.stationsCatalog;
    function contains(rows, pred) {
      for (var i = 0; i < rows.length; i++) if (pred(rows[i])) return true;
      return false;
    }
    if (f.region && !contains(poolStations(catalog, f, { region: "", comuna: "", cuenca: "", subcuenca: "" }), function (s) {
      return textOf(s.cod_reg) === String(f.region);
    })) {
      f.region = "";
      f.comuna = "";
    }
    if (f.comuna && !contains(poolStations(catalog, f, { comuna: "" }), function (s) {
      return textOf(s.nom_com) === f.comuna;
    })) f.comuna = "";
    if (f.cuenca && !contains(poolStations(catalog, f, { cuenca: "", subcuenca: "" }), function (s) {
      return textOf(s.cuenca) === f.cuenca;
    })) {
      f.cuenca = "";
      f.subcuenca = "";
    }
    if (f.subcuenca && !contains(poolStations(catalog, f, { subcuenca: "" }), function (s) {
      return textOf(s.subcuenca) === f.subcuenca;
    })) f.subcuenca = "";
    if (f.fuenteId && !contains(poolStations(catalog, f, { fuenteId: "" }), function (s) {
      return stationFuenteId(s) === String(f.fuenteId);
    })) f.fuenteId = "";
  }

  function deriveFilterOptions(catalog, filters) {
    var regionIds = {};
    poolStations(catalog, filters, { region: "", comuna: "", cuenca: "", subcuenca: "" }).forEach(function (s) {
      var id = textOf(s.cod_reg);
      if (id) regionIds[id] = true;
    });
    return {
      regions: (state.regionsCatalog || []).filter(function (r) { return regionIds[textOf(r.id)]; }),
      comunas: uniqSorted(poolStations(catalog, filters, { comuna: "" }).map(function (s) { return textOf(s.nom_com); })),
      cuencas: uniqSorted(poolStations(catalog, filters, { cuenca: "", subcuenca: "" }).map(function (s) { return textOf(s.cuenca); })),
      subcuencas: uniqSorted(poolStations(catalog, filters, { subcuenca: "" }).map(function (s) { return textOf(s.subcuenca); })),
      fuentes: uniqFuentes(poolStations(catalog, filters, { fuenteId: "" }))
    };
  }

  function stationCodReg(s) {
    var n = parseInt(apiScalar(s && s.cod_reg), 10);
    return n > 0 ? n : 0;
  }

  function stationMacrozona(s) {
    var cod = stationCodReg(s);
    var i;
    if (cod) {
      for (i = 0; i < MACROZONAS.length; i++) {
        if (MACROZONAS[i].cods.indexOf(cod) >= 0) return MACROZONAS[i].id;
      }
    }
    var mz = apiScalar(s && s.macrozona);
    if (mz == null || mz === "") return "";
    mz = String(mz).trim();
    return MACROZONA_CODE[mz] || mz;
  }

  function stationFuenteId(s) {
    var id = apiScalar(s && s.id_fuente);
    if (id == null || id === "") return "";
    var n = parseInt(id, 10);
    return n > 0 ? String(n) : "";
  }

  function fuenteLabel(s) {
    var nombre = apiScalar(s && s.fuente);
    if (nombre) return String(nombre).toUpperCase();
    return "";
  }

  function uniqFuentes(catalog) {
    var seen = {};
    var out = [];
    (catalog || []).forEach(function (s) {
      var id = stationFuenteId(s);
      if (!id || seen[id]) return;
      seen[id] = true;
      out.push({ id: id, nombre: fuenteLabel(s) || ("Fuente " + id) });
    });
    out.sort(function (a, b) { return a.nombre.localeCompare(b.nombre, "es"); });
    return out;
  }

  function buildStationsCatalogPath() {
    var p = new URLSearchParams();
    p.set("catalog", "1");
    p.set("chile_only", "1");
    p.set("con_datos", "1");
    return "/estaciones?" + p.toString();
  }

  function selectOptionsHtml(options, selected, emptyLabel) {
    var html = '<option value="">' + escapeHtml(emptyLabel) + "</option>";
    options.forEach(function (opt) {
      var val = typeof opt === "object" ? String(opt.id != null ? opt.id : opt.value) : String(opt);
      var label = typeof opt === "object" ? (opt.nombre || opt.label || val) : opt;
      var sel = String(selected) === val ? " selected" : "";
      html += '<option value="' + escapeHtml(val) + '"' + sel + ">" + escapeHtml(label) + "</option>";
    });
    return html;
  }

  function formatMiles(n) {
    n = Number(n);
    if (!n || isNaN(n)) n = 0;
    return new Intl.NumberFormat("es-CL", { maximumFractionDigits: 0 }).format(n);
  }

  function seleccionadasLabel(n) {
    n = Number(n) || 0;
    return formatMiles(n) + (n === 1 ? " seleccionada" : " seleccionadas");
  }

  function stationNameHtml(s) {
    var nombre = escapeHtml((s && s.nombre) || ("Estacion " + (s && s.id)));
    return nombre + ' <span class="dl-station-code">#' + escapeHtml(s && s.id) + "</span>";
  }

  function stationMetaHtml(s) {
    var bits = [];
    var fuente = fuenteLabel(s);
    if (fuente) bits.push('<span class="dl-fuente">' + escapeHtml(fuente) + "</span>");
    if (s && s.nom_reg) bits.push(escapeHtml(textOf(s.nom_reg)));
    if (s && s.nom_com) bits.push(escapeHtml(textOf(s.nom_com)));
    if (s && s.cuenca) bits.push("cuenca " + escapeHtml(textOf(s.cuenca)));
    return bits.join(' <span class="dl-meta-dot">\u00b7</span> ');
  }

  function filteredExploreStations() {
    return applyStationFilters(state.stationsCatalog, state.stationFilters, state.stationSearch);
  }

  function filteredGroupMembers() {
    var q = (state.groupSearch || "").toLowerCase().trim();
    var members = state.eids.map(function (id) {
      return stationById(id) || { id: id, nombre: "Estacion " + id };
    });
    if (!q) return members;
    return members.filter(function (s) {
      var hay = foldText([s.nombre, s.nom_reg, s.nom_com, s.cuenca, s.subcuenca, fuenteLabel(s), s.id].join(" "));
      return hay.indexOf(foldText(q)) >= 0;
    });
  }

  function loadStationsCatalog() {
    return apiFetchResponse(buildStationsCatalogPath())
      .then(function (r) { return parseJsonResponse("estaciones", r); })
      .then(function (data) {
        state.stationsCatalog = Array.isArray(data) ? data : (data && data.data) || [];
        rebuildStationMap();
      });
  }

  function clerkEmail() {
    var user = typeof rodajaGetCurrentUser === "function" ? rodajaGetCurrentUser() : null;
    if (!user) return "";
    return (user.primaryEmailAddress && user.primaryEmailAddress.emailAddress) ||
      (user.emailAddresses && user.emailAddresses[0] && user.emailAddresses[0].emailAddress) || "";
  }

  function loadCuota() {
    var path = "/me/cuota";
    var email = clerkEmail();
    if (email) path += "?" + new URLSearchParams({ email: email }).toString();
    return apiFetchResponse(path).then(function (r) { return r.ok ? parseJsonResponse("cuota", r) : null; }).then(function (c) {
      if (!c) return;
      state.cuota = c;
      state.tier = c.tier || "gratuito";
      state.granularidadesAllowed = c.granularidades || ["anual", "mensual", "diario", "horaria"];
      state.formatos = c.formatos || ["liviano", "completo"];
      if (typeof dlRenderQuotaBar === "function") dlRenderQuotaBar(c);
    });
  }

  function loadVarStations() {
    state.varStations = null;
    return apiFetchResponse("/descarga/pares-variable").then(function (r) {
      if (!r || !r.ok) return null;
      return parseJsonResponse("pares", r);
    }).then(function (data) {
      if (!data || data.disponible === false || !data.por_variable) return;
      var map = {};
      var por = data.por_variable;
      Object.keys(por).forEach(function (vid) {
        var set = {};
        var arr = por[vid];
        if (!Array.isArray(arr)) arr = arr == null ? [] : [arr];
        var i;
        for (i = 0; i < arr.length; i++) set[arr[i]] = true;
        map[String(vid)] = set;
      });
      state.varStations = map;
    }).catch(function () { state.varStations = null; });
  }

  function loadCatalogs() {
    return Promise.all([
      loadStationsCatalog(),
      apiFetchResponse("/regiones?chile_only=1").then(function (r) { return parseJsonResponse("regiones", r); }),
      apiFetchResponse("/variables").then(function (r) { return parseJsonResponse("variables", r); }),
      loadVarStations()
    ]).then(function (res) {
      state.regionsCatalog = Array.isArray(res[1]) ? res[1] : (res[1] && res[1].data) || [];
      state.variablesCatalog = Array.isArray(res[2]) ? res[2] : (res[2] && res[2].data) || [];
      if (!state.variablesCatalog.length) {
        throw new Error("No se pudo cargar el cat\u00e1logo de variables.");
      }
    });
  }

  function showWizardMessage(html) {
    var gate = document.getElementById("dl-gate");
    var wizard = document.getElementById("dl-wizard");
    hide(gate);
    show(wizard);
    wizard.innerHTML = html;
  }

  function initEidsFromBoot() {
    var fromGrupo = qs("from") === "grupo";
  if (fromGrupo) {
      try {
        var snap = sessionStorage.getItem(SNAPSHOT_KEY);
        if (snap) {
          var g = JSON.parse(snap);
          state.eids = normalizeIds(g.ids || []);
          state.nombreLote = g.nombre || state.nombreLote;
        }
      } catch (e) { /* ignore */ }
    }
    var eParam = qs("e") || qs("estacion");
    if (eParam) state.eids = normalizeIds(eParam.split(","));
    var vParam = qs("v");
    if (vParam) state.vids = normalizeIds(vParam.split(","));
    var now = new Date().getFullYear();
    state.yearEnd = state.yearEnd || now;
    state.yearStart = state.yearStart || (now - 10);
    if (state.eids.length) persistGroupSnapshot();
  }

  function hourlyIdSet() {
    var set = {};
    (state.variablesCatalog || []).forEach(function (v) {
      if (v && (v.nombre_abr === "u_rafaga_h" || v.nombre === "viento_rafaga_horaria")) set[v.id] = true;
    });
    return set;
  }

  function syncHourlyGranularity() {
    var hora = hourlyIdSet();
    var hasH = state.vids.some(function (id) { return hora[id]; });
    var hasD = state.vids.some(function (id) { return !hora[id]; });
    if (hasH && !state.hadHourly && state.granularidades.indexOf("horaria") < 0) {
      state.granularidades.push("horaria");
    }
    if (!hasH && state.hadHourly) {
      state.granularidades = state.granularidades.filter(function (g) { return g !== "horaria"; });
    }
    state.hadHourly = hasH;
    if (!hasD) {
      state.granularidades = state.granularidades.filter(function (g) { return g === "horaria"; });
    } else if (!state.granularidades.some(function (g) { return g !== "horaria"; })) {
      state.granularidades.push("diario");
    }
    if (!state.granularidades.length) state.granularidades = ["anual"];
  }

  function granBase(g, y0, y1) {
    if (!(y1 >= y0)) return 0;
    var y;
    var d = 0;
    if (g === "horaria") return granBase("diario", y0, y1) * 24;
    if (g === "diario") {
      for (y = y0; y <= y1; y++) {
        d += (y % 4 === 0 && (y % 100 !== 0 || y % 400 === 0)) ? 366 : 365;
      }
      return d;
    }
    if (g === "mensual") return (y1 - y0 + 1) * 12;
    return y1 - y0 + 1;
  }

  function bytesForRegs(regs, fmt) {
    regs = Number(regs) || 0;
    if (regs < 1) return 0;
    var f = fmt || state.formatoDescarga || "liviano";
    var per = f === "completo" ? 55 : 30;
    var fijo = f === "completo" ? 800 : 20;
    return fijo + regs * per;
  }

  function applyLocalEstimate() {
    if (!(state.yearStart > 0) || !(state.yearEnd > 0) || state.yearStart > state.yearEnd) {
      state.estimate = {
        registros: 0,
        aproximado: true,
        error: "El a\u00f1o de inicio es posterior al a\u00f1o de fin.",
        bytes_estimados_label: "\u2014"
      };
      return;
    }
    var nEst = state.eids.length;
    var hora = hourlyIdSet();
    var nH = state.vids.filter(function (id) { return hora[id]; }).length;
    var nD = state.vids.length - nH;
    var desglose = state.granularidades.map(function (g) {
      var n = g === "horaria" ? nH : nD;
      var regs = granBase(g, state.yearStart, state.yearEnd) * nEst * n;
      return { granularidad: g, registros: regs, bytes: bytesForRegs(regs) };
    });
    var regs = 0;
    var bytes = 0;
    desglose.forEach(function (d) { regs += d.registros; bytes += d.bytes; });
    state.estimate = {
      registros: regs,
      bytes_estimados: bytes,
      bytes_estimados_label: formatBytes(bytes),
      desglose: desglose,
      aproximado: true,
      dentro_cuota: null
    };
  }

  function rescaleEstimateBytes() {
    var est = state.estimate;
    if (!est || est.error) return;
    var bytes = 0;
    (est.desglose || []).forEach(function (d) {
      d.bytes = bytesForRegs(d.registros);
      bytes += d.bytes;
    });
    if (!est.desglose || !est.desglose.length) bytes = bytesForRegs(est.registros);
    est.bytes_estimados = bytes;
    est.bytes_estimados_label = formatBytes(bytes);
  }

  function estimateBody(extra) {
    var fechas = fechaFromYears(state.yearStart, state.yearEnd);
    var body = {
      estacion_ids: state.eids,
      variable_ids: state.vids,
      granularidades: state.granularidades,
      fecha_inicio: fechas.inicio,
      fecha_fin: fechas.fin,
      formato_descarga: state.formatoDescarga,
      modo_cobertura: state.modoCobertura
    };
    if (extra) Object.keys(extra).forEach(function (k) { body[k] = extra[k]; });
    return body;
  }

  function paintSummary() {
    var est = state.estimate || {};
    var count = document.getElementById("dl-summary-count");
    if (count) {
      count.innerHTML = "Peso estimado: <span>" + escapeHtml(est.bytes_estimados_label || "\u2014") + "</span>";
    }
    var msg = document.getElementById("dl-summary-msg");
    if (msg) msg.innerHTML = renderSummaryMsg(est);
    var line = document.getElementById("dl-summary-line");
    if (line) {
      line.textContent = summaryEstacionesHtml() + " \u00b7 " + varPickGroups().length + " variables \u00b7 " +
        state.granularidades.length + " granularidad(es)";
    }
    var ul = document.getElementById("dl-desglose");
    if (ul) {
      ul.innerHTML = (est.desglose || []).map(function (d) {
        return "<li>" + escapeHtml(d.granularidad) + ": ~" + formatBytes(d.bytes) + "</li>";
      }).join("");
    }
    var btn = document.getElementById("dl-btn-submit");
    if (btn) btn.disabled = !canSubmit();
  }

  function fetchEstimate() {
    if (!state.eids.length || !state.vids.length || !state.granularidades.length) return Promise.resolve(null);
    if (state.yearStart > state.yearEnd) {
      applyLocalEstimate();
      paintSummary();
      return Promise.resolve(state.estimate);
    }
    var seq = (state.estimateSeq += 1);
    if (state.estimateAbort) state.estimateAbort.abort();
    var ctrl = typeof AbortController === "function" ? new AbortController() : null;
    state.estimateAbort = ctrl;
    var fmt = state.formatoDescarga;
    var opts = {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(estimateBody())
    };
    if (ctrl) opts.signal = ctrl.signal;
    return rodajaApiFetch("/descarga/estimar", opts).then(function (r) {
      return r.json().then(function (data) {
        return { ok: r.ok, status: r.status, data: data };
      }, function () {
        return { ok: false, status: r.status, data: {} };
      });
    }).then(function (res) {
      if (seq !== state.estimateSeq) return null;
      if (!res.ok) {
        if (res.status === 400 && res.data && res.data.error) {
          state.estimate = { registros: 0, error: res.data.error, aproximado: false, bytes_estimados_label: "\u2014" };
          paintSummary();
        }
        return state.estimate;
      }
      state.estimate = res.data;
      state.estimate.registros = Number(apiScalar(state.estimate.registros)) || 0;
      state.estimate.bytes_estimados = Number(apiScalar(state.estimate.bytes_estimados)) || 0;
      state.estimate.aproximado = apiScalar(state.estimate.aproximado) === true;
      var dentro = apiScalar(state.estimate.dentro_cuota);
      state.estimate.dentro_cuota = dentro === true || dentro === false ? dentro : null;
      var nCon = apiScalar(state.estimate.estaciones_con_datos);
      if (nCon != null && nCon !== "") state.estimate.estaciones_con_datos = Number(nCon);
      if (!state.estimate.bytes_estimados_label) {
        state.estimate.bytes_estimados_label = formatBytes(state.estimate.bytes_estimados);
      }
      if (state.formatoDescarga !== fmt) rescaleEstimateBytes();
      paintSummary();
      return state.estimate;
    }).catch(function (err) {
      if (err && err.name === "AbortError") return null;
      return state.estimate;
    });
  }

  function scheduleEstimate() {
    if (state.estimateTimer) clearTimeout(state.estimateTimer);
    state.estimateTimer = setTimeout(fetchEstimate, 400);
  }

  function fetchCobertura(opts) {
    opts = opts || {};
    if (!state.eids.length || !state.vids.length) return Promise.resolve(null);
    var seq = (state.coberturaSeq += 1);
    return rodajaApiFetch("/descarga/cobertura", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(estimateBody({ incluir_ids: !!opts.incluir_ids }))
    }).then(function (r) {
      return r.json().then(function (data) { return { ok: r.ok, data: data }; }, function () { return { ok: false, data: null }; });
    }).then(function (res) {
      if (!opts.incluir_ids && seq !== state.coberturaSeq) return null;
      if (res.ok) state.cobertura = res.data;
      return res.ok ? res.data : null;
    }).catch(function () { return null; });
  }

  function sinDatosCount(v) {
    if (!v) return 0;
    if (typeof v.sin_datos === "number") return v.sin_datos;
    var s = v.estaciones_sin_datos;
    if (Array.isArray(s)) return s.length;
    var n = Number(s);
    return isNaN(n) ? 0 : n;
  }

  function hasCoberturaGaps() {
    if (!state.cobertura || !state.cobertura.variables) return false;
    return state.cobertura.variables.some(function (v) { return sinDatosCount(v) > 0; });
  }

  function formatClp(n) {
    n = Number(n);
    if (isNaN(n)) n = 0;
    return new Intl.NumberFormat("es-CL", {
      style: "currency",
      currency: "CLP",
      maximumFractionDigits: 0
    }).format(n);
  }

  function estimateOnDemandOk(est) {
    if (!est || !est.on_demand_disponible) return false;
    var od = est.on_demand;
    if (od && od.enabled === true) return true;
    return !!est.on_demand_disponible;
  }

  function renderSummaryMsg(est) {
    if (est && est.error) return escapeHtml(String(est.error));
    if (!est || est.registros == null) return "Estimando\u2026";
    if (est.aproximado && est.dentro_cuota == null) return "Estimado por calendario. Afinando con la cobertura real\u2026";
    if (est.dentro_cuota == null) return "Afinando el peso\u2026";
    if (est.dentro_cuota === true) return "Cabe en tu l\u00edmite.";
    if (est.on_demand_disponible) {
      return "Supera el l\u00edmite de tu plan. Costo extra estimado: " +
        formatClp(est.costo_overage_clp) + " (pago por uso).";
    }
    return 'Supera tu l\u00edmite. <a href="/cuenta/">Activa el pago por uso o mejora tu plan</a>.';
  }

  function canSubmit() {
    if (!rodajaIsSignedIn || !rodajaIsSignedIn()) return false;
    if (!state.eids.length || !state.vids.length || !state.granularidades.length) return false;
    if (state.estimate && state.estimate.dentro_cuota === false && !estimateOnDemandOk(state.estimate)) return false;
    if (!state.estimate || !(Number(state.estimate.registros) >= 1)) return false;
    return true;
  }

  var STEP_KICKERS = [
    "Arma tu grupo de estaciones",
    "Elige las variables",
    "Elige la granularidad",
    "A\u00f1os y formato"
  ];

  function stepProgressHtml() {
    var total = STEP_KICKERS.length;
    var n = state.step;
    var dots = "";
    var i;
    for (i = 1; i <= total; i++) {
      var cls = "dl-step-dot";
      if (i < n) cls += " is-done";
      else if (i === n) cls += " is-current";
      dots += '<span class="' + cls + '"></span>';
    }
    return '<div class="dl-step-progress">' +
      '<p class="dl-step-kicker">Paso ' + n + " de " + total + " \u00b7 " + escapeHtml(STEP_KICKERS[n - 1] || "") + "</p>" +
      '<div class="dl-step-dots" aria-hidden="true">' + dots + "</div></div>";
  }

  function coberturaNombre(v) {
    var id = v && v.variable_id;
    var cat = (state.variablesCatalog || []).filter(function (x) { return String(x.id) === String(id); })[0];
    if (cat) return varFace(cat).label;
    return (v && v.nombre) || "";
  }

  function renderCoverageBanner() {
    if (!hasCoberturaGaps()) return "";
    var lines = (state.cobertura.variables || []).filter(function (v) {
      return sinDatosCount(v) > 0;
    }).map(function (v) {
      return "<li><strong>" + escapeHtml(coberturaNombre(v)) + "</strong>: sin datos en " +
        sinDatosCount(v) + " de " + state.cobertura.total_estaciones + " estaciones</li>";
    }).join("");
    return '<div class="dl-coverage-banner">' +
      "<p>Algunas estaciones no tienen todas las variables seleccionadas:</p><ul>" + lines + "</ul>" +
      '<div class="dl-coverage-actions">' +
      '<button type="button" class="dl-btn-secondary" id="dl-cov-solo">Descargar solo donde hay datos</button>' +
      '<button type="button" class="dl-btn-secondary" id="dl-cov-quitar">Quitar estaciones sin datos</button>' +
      "</div></div>";
  }

  function pageWindow(total, page, pageSize) {
    var pages = Math.max(1, Math.ceil((total || 0) / pageSize));
    if (page < 0 || isNaN(page)) page = 0;
    if (page >= pages) page = pages - 1;
    var start = total ? page * pageSize : 0;
    return { page: page, pages: pages, start: start, end: Math.min(total, start + pageSize) };
  }

  function pagerHtml(which, total, page, pageSize) {
    if (!total) return "";
    var w = pageWindow(total, page, pageSize);
    var sizeOpts = PAGE_SIZES.map(function (n) {
      return '<option value="' + n + '"' + (n === pageSize ? " selected" : "") + ">" + n + "</option>";
    }).join("");
    return '<div class="dl-pager">' +
      '<span class="dl-pager-meta">' + formatMiles(w.start + 1) + "\u2013" + formatMiles(w.end) + " de " + formatMiles(total) + "</span>" +
      '<label class="dl-pager-field">Por p\u00e1gina <select class="dl-page-size" data-pager="' + which + '">' + sizeOpts + "</select></label>" +
      '<div class="dl-pager-nav">' +
      '<button type="button" class="dl-pager-btn" data-pager="' + which + '" data-dir="-1"' + (w.page <= 0 ? " disabled" : "") + ">Anterior</button>" +
      '<span class="dl-pager-meta">' + (w.page + 1) + " / " + w.pages + "</span>" +
      '<button type="button" class="dl-pager-btn" data-pager="' + which + '" data-dir="1"' + (w.page >= w.pages - 1 ? " disabled" : "") + ">Siguiente</button>" +
      "</div></div>";
  }

  function renderMacrozonaPills() {
    var f = state.stationFilters;
    var pills = ['<button type="button" class="dl-filter-chip' + (!f.macrozona ? " active" : "") + '" data-mz="">Todas</button>'];
    MACROZONAS.forEach(function (mz) {
      pills.push('<button type="button" class="dl-filter-chip' + (f.macrozona === mz.id ? " active" : "") + '" data-mz="' + escapeHtml(mz.id) + '">' + escapeHtml(mz.id) + "</button>");
    });
    return pills.join("");
  }

  function renderStep1(wizard) {
    reconcileStationFilters();
    var list = filteredExploreStations();
    var pageSize = PAGE_SIZES.indexOf(state.pageSize) >= 0 ? state.pageSize : 10;
    state.pageSize = pageSize;
    var exploreWin = pageWindow(list.length, state.explorePage, pageSize);
    state.explorePage = exploreWin.page;
    var shown = list.slice(exploreWin.start, exploreWin.end);
    var opts = deriveFilterOptions(state.stationsCatalog, state.stationFilters);
    var f = state.stationFilters;
    var eidSet = {};
    var tab = state.exploreTab === "pegar" ? "pegar" : "lista";
    var canNext = state.eids.length > 0;
    var addLabel = list.length === 1
      ? "Agregar esta estaci\u00f3n"
      : (list.length ? ("Agregar las " + formatMiles(list.length)) : "Agregar");
    var removeLabel = list.length === 1
      ? "Quitar esta del grupo"
      : (list.length ? ("Quitar las " + formatMiles(list.length) + " del grupo") : "Quitar del grupo");
    state.eids.forEach(function (id) { eidSet[id] = true; });

    var checks = shown.map(function (s) {
      var inGroup = !!eidSet[s.id];
      var meta = stationMetaHtml(s);
      return '<label class="dl-station-row' + (inGroup ? " is-in-group" : "") + '">' +
        '<input type="checkbox" data-eid="' + s.id + '"' + (inGroup ? " checked" : "") + ">" +
        '<span class="dl-station-row-body">' +
        '<span class="dl-station-row-name">' + stationNameHtml(s) + "</span>" +
        (meta ? '<span class="dl-station-row-meta">' + meta + "</span>" : "") +
        "</span></label>";
    }).join("");

    var members = filteredGroupMembers();
    var groupWin = pageWindow(members.length, state.groupPage, pageSize);
    state.groupPage = groupWin.page;
    var shownMembers = members.slice(groupWin.start, groupWin.end);
    var groupRows = shownMembers.map(function (s) {
      var meta = stationMetaHtml(s);
      return '<div class="dl-group-member">' +
        '<div class="dl-group-member-info">' +
        '<span class="dl-group-member-name">' + stationNameHtml(s) + "</span>" +
        (meta ? '<span class="dl-group-member-meta">' + meta + "</span>" : "") +
        "</div>" +
        '<button type="button" class="dl-btn-icon dl-group-remove" data-eid="' + s.id + '" title="Quitar">&times;</button>' +
        "</div>";
    }).join("");

    var groupSearchHtml = state.eids.length
      ? '<input type="search" id="dl-group-search" class="dl-search" placeholder="Buscar en el grupo\u2026" value="' + escapeHtml(state.groupSearch) + '">'
      : "";
    var groupBody = groupRows
      ? '<div class="dl-group-list" id="dl-group-list">' + groupRows + "</div>"
      : '<div class="dl-group-empty" id="dl-group-list"><p>A\u00fan no agregas estaciones.<br>Selecci\u00f3nalas de la lista o agr\u00e9galas todas de una vez.</p></div>';

    wizard.innerHTML = stepProgressHtml() +
      '<h1 class="dl-step-title">\u00bfQu\u00e9 estaciones quieres descargar?</h1>' +
      '<div class="dl-card dl-step1-card">' +
      '<div class="dl-step1-grid">' +
      '<div class="dl-explore-panel">' +
      '<div class="dl-tabs">' +
      '<button type="button" class="dl-tab' + (tab === "lista" ? " active" : "") + '" data-tab="lista">Explorar</button>' +
      '<button type="button" class="dl-tab' + (tab === "pegar" ? " active" : "") + '" data-tab="pegar">Pegar IDs</button>' +
      "</div>" +
      '<div id="dl-tab-lista"' + (tab === "lista" ? "" : " hidden") + ">" +
      '<div class="dl-searchbar"><i class="fa fa-search" aria-hidden="true"></i>' +
      '<input type="search" id="dl-station-search" placeholder="Busca por nombre, comuna o c\u00f3digo de estaci\u00f3n" value="' + escapeHtml(state.stationSearch) + '" aria-label="Buscar estaci\u00f3n">' +
      "</div>" +
      '<p class="dl-zone-label">Zona</p>' +
      '<div class="dl-filter-chips">' + renderMacrozonaPills() + "</div>" +
      '<details class="dl-advanced" id="dl-advanced"' + (state.advancedOpen ? " open" : "") + ">" +
      '<summary><span class="dl-adv-chevron" aria-hidden="true"></span> Filtros avanzados <span class="dl-adv-count">6</span></summary>' +
      '<div class="dl-filter-selects">' +
      '<label>Regi\u00f3n<select id="dl-filter-region">' + selectOptionsHtml(opts.regions, f.region, "Todas") + "</select></label>" +
      '<label>Comuna<select id="dl-filter-comuna">' + selectOptionsHtml(opts.comunas, f.comuna, "Todas") + "</select></label>" +
      '<label>Cuenca<select id="dl-filter-cuenca">' + selectOptionsHtml(opts.cuencas, f.cuenca, "Todas") + "</select></label>" +
      '<label>Subcuenca<select id="dl-filter-subcuenca">' + selectOptionsHtml(opts.subcuencas, f.subcuenca, "Todas") + "</select></label>" +
      '<label>Fuente<select id="dl-filter-fuente">' + selectOptionsHtml(opts.fuentes, f.fuenteId, "Todas") + "</select></label>" +
      '<label>Variable<select id="dl-filter-variable">' + selectOptionsHtml(
        variableFilterOptions(),
        f.variableId, "Cualquiera"
      ) + "</select></label>" +
      "</div></details>" +
      '<div class="dl-result-line">' +
      "<span><strong>" + formatMiles(list.length) + "</strong> resultados</span>" +
      '<span class="dl-selected-near" id="dl-selected-near">' + seleccionadasLabel(state.eids.length) + "</span>" +
      "</div>" +
      '<div class="dl-explore-actions">' +
      '<button type="button" id="dl-add-visible" class="dl-btn-accent"' + (list.length ? "" : " disabled") + ">" + escapeHtml(addLabel) + "</button>" +
      '<button type="button" id="dl-remove-visible" class="dl-btn-outline"' + (list.length ? "" : " disabled") + ">" + escapeHtml(removeLabel) + "</button>" +
      "</div>" +
      '<div class="dl-station-list">' + (checks || '<p class="dl-empty-msg">Sin resultados con estos filtros</p>') + "</div>" +
      pagerHtml("explore", list.length, state.explorePage, pageSize) +
      "</div>" +
      '<div id="dl-tab-pegar"' + (tab === "pegar" ? "" : " hidden") + ">" +
      '<textarea id="dl-paste-ids" class="dl-textarea" placeholder="101, 102, 205"></textarea>' +
      '<button type="button" id="dl-apply-paste" class="dl-btn-accent">Aplicar IDs al grupo</button></div>' +
      "</div>" +
      '<aside class="dl-group-panel">' +
      '<label class="dl-group-name-label">Nombre del grupo<input type="text" id="dl-group-name" class="dl-group-name" value="' + escapeHtml(state.nombreLote) + '"></label>' +
      '<div class="dl-cap-row"><span>Estaciones en el grupo</span><strong id="dl-eid-count">' + formatMiles(state.eids.length) + "</strong></div>" +
      groupSearchHtml +
      groupBody +
      pagerHtml("group", members.length, state.groupPage, pageSize) +
      '<button type="button" id="dl-clear-group" class="dl-btn-danger"' + (state.eids.length ? "" : " disabled") + '><i class="fa fa-trash-o" aria-hidden="true"></i> Vaciar grupo</button>' +
      "</aside></div></div>" +
      '<div class="dl-step-cta"><div class="dl-step-cta-inner">' +
      '<button type="button" id="dl-next-1" class="dl-btn-continue"' + (canNext ? "" : " disabled") + ">" +
      (canNext ? "Continuar" : "Continuar \u00b7 agrega al menos 1 estaci\u00f3n") +
      "</button></div></div>";
  }

  function varCatsHtml() {
    var groups = groupVariables(state.variablesCatalog, state.varSearch);
    if (!groups.length) return '<p class="dl-empty-msg">Ninguna variable coincide con la b\u00fasqueda.</p>';
    return groups.map(function (g) {
      var rows = g.items.map(function (it) {
        var ids = it.vars.map(function (v) { return v.id; });
        var nOn = ids.filter(function (id) { return state.vids.indexOf(id) >= 0; }).length;
        var on = nOn > 0;
        var nested = it.variants.length > 1;
        var open = nested && (!!state.varOpen[it.phys] || !!foldText(state.varSearch).trim());
        var unit = it.face.unit ? '<span class="dl-var-unit">' + escapeHtml(it.face.unit) + "</span>" : "";
        var head = '<input type="checkbox" class="dl-var-cb is-parent" data-vid="' + ids.join(",") + '"' + (nOn === ids.length ? " checked" : "") + ">";
        var name = escapeHtml(it.face.label);
        var row;
        if (!nested) {
          row = '<label class="dl-var-opt' + (on ? " is-on" : "") + '">' + head +
            '<span class="dl-var-name">' + name + "</span>" + unit + "</label>";
        } else {
          var subs = it.variants.map(function (slot) {
            var sids = slot.vars.map(function (v) { return v.id; });
            var son = sids.every(function (id) { return state.vids.indexOf(id) >= 0; });
            var su = slot.unit ? '<span class="dl-var-unit">' + escapeHtml(slot.unit) + "</span>" : "";
            return '<label class="dl-var-sub' + (son ? " is-on" : "") + '">' +
              '<input type="checkbox" class="dl-var-cb" data-vid="' + sids.join(",") + '"' + (son ? " checked" : "") + ">" +
              "<span>" + escapeHtml(slot.label) + "</span>" + su + "</label>";
          }).join("");
          row = '<div class="dl-var-opt' + (on ? " is-on" : "") + '">' + head +
            '<button type="button" class="dl-var-hit" data-phys="' + escapeHtml(it.phys) + '"><span class="dl-var-name">' + name + "</span></button>" +
            unit +
            '<button type="button" class="dl-var-more" data-phys="' + escapeHtml(it.phys) + '" aria-expanded="' + (open ? "true" : "false") + '" aria-label="Ver mediciones">' + (open ? "\u25be" : "\u25b8") + "</button></div>" +
            '<div class="dl-var-subs"' + (open ? "" : " hidden") + ">" + subs + "</div>";
        }
        return '<div class="dl-var-block' + (nested ? " has-variants" : "") + (open ? " is-open" : "") + '">' + row + "</div>";
      }).join("");
      return '<section class="dl-var-cat">' +
        '<div class="dl-var-cathd">' +
        '<span class="dl-var-ic">' + varIcon(g.icon) + "</span>" +
        "<b>" + escapeHtml(g.label) + "</b>" +
        '<button type="button" class="dl-var-all" data-cat="' + g.id + '">Seleccionar todas</button>' +
        "</div>" +
        '<div class="dl-var-grid2">' + rows + "</div></section>";
    }).join("");
  }

  function varPicksHtml() {
    var chips = varPickGroups().map(function (pick) {
      return '<span class="dl-var-pick">' + escapeHtml(pick.chip) +
        '<button type="button" data-vid="' + pick.ids.join(",") + '" aria-label="Quitar">\u00d7</button></span>';
    }).join("");
    return chips || '<p class="dl-empty-msg">Todav\u00eda no eliges variables.</p>';
  }

  function pruneVarFocus() {
    var ids = parseVidList(state.varFocus);
    if (!ids.length || ids.some(function (id) { return state.vids.indexOf(id) < 0; })) state.varFocus = "";
  }

  function varSpecHtml() {
    pruneVarFocus();
    var ids = parseVidList(state.varFocus);
    if (!ids.length) return "";
    var wanted = {};
    ids.forEach(function (id) { wanted[id] = true; });
    var bits = [];
    groupVariables(state.variablesCatalog, "").forEach(function (g) {
      g.items.forEach(function (it) {
        it.variants.forEach(function (slot) {
          var sids = slot.vars.map(function (v) { return v.id; });
          if (!sids.length || !sids.every(function (id) { return wanted[id]; })) return;
          var texts = [];
          slot.vars.forEach(function (v) {
            var t = state.varSpecs && state.varSpecs[String(v.id)];
            if (t) texts.push(t);
          });
          if (!texts.length) return;
          var grados = texts.filter(function (t) { return t.indexOf("grados") >= 0; });
          bits.push({ label: slot.label, text: grados.length ? grados[0] : texts[0] });
        });
      });
    });
    if (!bits.length) {
      ids.forEach(function (id) {
        var t = state.varSpecs && state.varSpecs[String(id)];
        if (t) bits.push({ label: "", text: t });
      });
    }
    return bits.map(function (b) {
      var head = bits.length > 1 && b.label ? "<b>" + escapeHtml(b.label) + ".</b> " : "";
      return "<p>" + head + escapeHtml(b.text) + "</p>";
    }).join("");
  }

  function paintVarSpec() {
    var el = document.getElementById("dl-var-spec");
    if (!el) return;
    var html = varSpecHtml();
    el.innerHTML = html;
    el.hidden = !html;
  }

  function loadVarSpecs() {
    return fetch("/descargar/var-specs.json", { cache: "no-cache", headers: { Accept: "application/json" } })
      .then(function (r) { return r.ok ? r.json() : null; })
      .then(function (data) {
        state.varSpecs = (data && data.por_id) || {};
        paintVarSpec();
      })
      .catch(function () { state.varSpecs = state.varSpecs || {}; });
  }

  function rememberVarFocus(cb) {
    if (!cb || !cb.checked) return;
    state.varFocus = cb.getAttribute("data-vid") || "";
    pruneVarFocus();
  }

  function varHintHtml() {
    var n = varPickGroups().length;
    var word = n === 1 ? "variable elegida" : "variables elegidas";
    var tail = " Si una estaci\u00f3n no mide alguna, no aparecer\u00e1 en su descarga.";
    if (!n) return "Ninguna variable elegida." + tail;
    return "<b>" + n + "</b> " + word + "." + tail;
  }

  function renderStep2(wizard) {
    wizard.innerHTML = stepProgressHtml() +
      '<h1 class="dl-step-title">Variables</h1>' +
      '<div class="dl-card">' +
      '<div class="dl-step2-grid">' +
      "<div>" +
      '<div class="dl-searchbar"><i class="fa fa-search" aria-hidden="true"></i>' +
      '<input type="search" id="dl-var-search" placeholder="Busca una variable, por ejemplo \u201cviento\u201d o \u201ctemperatura\u201d" value="' + escapeHtml(state.varSearch) + '" aria-label="Buscar variable">' +
      "</div>" +
      '<div id="dl-var-cats">' + varCatsHtml() + "</div>" +
      "</div>" +
      '<aside class="dl-group-panel dl-var-side">' +
      "<h3>Variables seleccionadas</h3>" +
      '<p class="dl-var-note">La fuente de la estaci\u00f3n se elige en el paso 1. Si una variable tiene varias mediciones (2 m, 10 m), \u00e1brela para elegir. Marcarla cerrada incluye todas.</p>' +
      '<div class="dl-var-spec" id="dl-var-spec"' + (varSpecHtml() ? "" : " hidden") + ">" + varSpecHtml() + "</div>" +
      '<div class="dl-var-picks" id="dl-var-picks">' + varPicksHtml() + "</div>" +
      '<div class="dl-var-hint" id="dl-var-hint">' + varHintHtml() + "</div>" +
      "</aside></div>" +
      '<div id="dl-coverage">' + renderCoverageBanner() + "</div></div>" +
      '<div class="dl-step-nav dl-var-nav">' +
      '<button type="button" id="dl-back-2" class="dl-btn-outline">Atr\u00e1s</button>' +
      '<button type="button" id="dl-next-2" class="dl-btn-continue"' + (state.vids.length ? "" : " disabled") + ">Continuar</button></div>";
  }

  function renderStep3(wizard) {
    var hora = hourlyIdSet();
    var hasH = state.vids.some(function (id) { return hora[id]; });
    var onlyH = hasH && state.vids.every(function (id) { return hora[id]; });
    var grans = [];
    if (!onlyH) {
      grans.push({ id: "anual", label: "Anual" }, { id: "mensual", label: "Mensual" }, { id: "diario", label: "Diario" });
    }
    if (hasH) grans.push({ id: "horaria", label: "Horaria" });
    var pills = grans.map(function (g) {
      var on = state.granularidades.indexOf(g.id) >= 0;
      return '<button type="button" class="dl-pill' + (on ? " active" : "") + '" data-gran="' + g.id + '">' + g.label + "</button>";
    }).join("");
    wizard.innerHTML = stepProgressHtml() +
      '<h1 class="dl-step-title">Granularidades</h1>' +
      '<div class="dl-card">' +
      '<div class="dl-pill-group">' + pills + "</div>" +
      (hasH ? '<p class="dl-var-note">La r\u00e1faga horaria sale en su propio archivo, hora por hora. No se mezcla con el m\u00e1ximo del d\u00eda.</p>' : "") +
      '<div class="dl-step-nav"><button type="button" id="dl-back-3" class="dl-btn-secondary">Atr\u00e1s</button>' +
      '<button type="button" id="dl-next-3" class="dl-btn-primary"' + (state.granularidades.length ? "" : " disabled") + ">Continuar</button></div></div>";
  }

  function summaryEstacionesHtml() {
    var nGrupo = state.eids.length;
    var nDatos = null;
    var est = state.estimate;
    if (est && est.aproximado !== true && est.estaciones_con_datos != null) {
      nDatos = Number(est.estaciones_con_datos);
    } else if (est && est.aproximado !== true && est.estacion_ids) {
      nDatos = normalizeIds(est.estacion_ids).length;
    }
    if (nDatos != null && !isNaN(nDatos) && nDatos !== nGrupo) {
      return formatMiles(nGrupo) + " estaciones (" + formatMiles(nDatos) + " con datos en el periodo)";
    }
    return formatMiles(nGrupo) + " estaciones";
  }

  function renderStep4(wizard) {
    var est = state.estimate || {};
    var fmtHtml = state.formatos.map(function (f) {
      var on = state.formatoDescarga === f;
      return '<button type="button" class="dl-pill' + (on ? " active" : "") + '" data-fmt="' + f + '">' + f + "</button>";
    }).join("");
    var desglose = (est.desglose || []).map(function (d) {
      return "<li>" + escapeHtml(d.granularidad) + ": ~" + formatBytes(d.bytes) + "</li>";
    }).join("");
    wizard.innerHTML = stepProgressHtml() +
      '<h1 class="dl-step-title">A\u00f1os y formato</h1>' +
      '<div class="dl-card">' +
      '<div class="dl-year-row"><label>A\u00f1o inicio<input type="number" id="dl-year-start" value="' + state.yearStart + '"></label>' +
      '<label>A\u00f1o fin<input type="number" id="dl-year-end" value="' + state.yearEnd + '"></label></div>' +
      '<p class="dl-card-title">Formato</p><div class="dl-pill-group">' + fmtHtml + "</div></div>" +
      '<div class="dl-card dl-summary"><p class="dl-card-title">Resumen</p>' +
      '<p id="dl-summary-line">' + summaryEstacionesHtml() + " \u00b7 " + varPickGroups().length + " variables \u00b7 " +
      state.granularidades.length + " granularidad(es)</p>" +
      '<p class="dl-summary-count" id="dl-summary-count">Peso estimado: <span>' +
      escapeHtml(est.bytes_estimados_label || "\u2014") + "</span></p>" +
      '<ul class="dl-desglose" id="dl-desglose">' + desglose + "</ul>" +
      '<p class="dl-summary-msg" id="dl-summary-msg">' + renderSummaryMsg(est) + "</p>" +
      '<div class="dl-step-nav"><button type="button" id="dl-back-4" class="dl-btn-secondary">Atr\u00e1s</button>' +
      '<button type="button" id="dl-btn-submit" class="dl-btn-primary"' + (canSubmit() ? "" : " disabled") + ">Solicitar descarga</button></div></div>";
  }

  function renderKeepingFocus(id) {
    var el = document.getElementById(id);
    var start = el ? el.selectionStart : null;
    var end = el ? el.selectionEnd : null;
    renderWizard();
    var next = document.getElementById(id);
    if (!next) return;
    next.focus();
    if (start != null && typeof next.setSelectionRange === "function") {
      try { next.setSelectionRange(start, end); } catch (e) { /* type=search */ }
    }
  }

  function paintCoverage() {
    var box = document.getElementById("dl-coverage");
    if (box) box.innerHTML = renderCoverageBanner();
    wireCoverageActions();
  }

  function scheduleCobertura() {
    if (state.coberturaTimer) clearTimeout(state.coberturaTimer);
    state.coberturaTimer = setTimeout(function () {
      fetchCobertura().then(function () { paintCoverage(); });
    }, 400);
  }

  function onQuitarSinDatos() {
    var btn = document.getElementById("dl-cov-quitar");
    if (btn) btn.disabled = true;
    fetchCobertura({ incluir_ids: true }).then(function (data) {
      if (data && data.estacion_ids) state.eids = normalizeIds(data.estacion_ids);
      state.modoCobertura = "solo_con_datos";
      return fetchCobertura();
    }).then(function () { renderWizard(); }).catch(function () { renderWizard(); });
  }

  function wireCoverageActions() {
    var covSolo = document.getElementById("dl-cov-solo");
    if (covSolo) covSolo.addEventListener("click", function () {
      state.modoCobertura = "solo_con_datos";
    });
    var covQuitar = document.getElementById("dl-cov-quitar");
    if (covQuitar) covQuitar.addEventListener("click", onQuitarSinDatos);
  }

  function wireStepEvents() {
    var wizard = document.getElementById("dl-wizard");
    if (!wizard) return;

    if (state.step === 1) {
      function syncStep1Ui() {
        var n = state.eids.length;
        var cnt = document.getElementById("dl-eid-count");
        if (cnt) cnt.textContent = formatMiles(n);
        var near = document.getElementById("dl-selected-near");
        if (near) near.textContent = seleccionadasLabel(n);
        var nxt = document.getElementById("dl-next-1");
        if (nxt) {
          nxt.disabled = !n;
          nxt.textContent = n ? "Continuar" : "Continuar \u00b7 agrega al menos 1 estaci\u00f3n";
        }
        var clr = document.getElementById("dl-clear-group");
        if (clr) clr.disabled = !n;
      }

      wizard.querySelectorAll(".dl-station-row input").forEach(function (cb) {
        cb.addEventListener("change", function () {
          var id = parseInt(cb.getAttribute("data-eid"), 10);
          if (cb.checked) {
            if (state.eids.indexOf(id) >= 0) return;
            state.eids.push(id);
            persistGroupSnapshot();
          } else {
            removeStation(id);
          }
          renderWizard();
        });
      });

      wizard.querySelectorAll(".dl-group-remove").forEach(function (btn) {
        btn.addEventListener("click", function () {
          removeStation(parseInt(btn.getAttribute("data-eid"), 10));
          renderWizard();
        });
      });

      wizard.querySelectorAll(".dl-filter-chip[data-mz]").forEach(function (btn) {
        btn.addEventListener("click", function () {
          state.stationFilters.macrozona = btn.getAttribute("data-mz") || "";
          state.explorePage = 0;
          renderWizard();
        });
      });

      function onFilterSelect(id, key, cascade) {
        var el = document.getElementById(id);
        if (!el) return;
        el.addEventListener("change", function () {
          state.stationFilters[key] = el.value || "";
          state.explorePage = 0;
          state.advancedOpen = true;
          if (cascade === "region") {
            state.stationFilters.comuna = "";
          }
          if (cascade === "cuenca") {
            state.stationFilters.subcuenca = "";
          }
          renderWizard();
        });
      }
      onFilterSelect("dl-filter-region", "region", "region");
      onFilterSelect("dl-filter-comuna", "comuna");
      onFilterSelect("dl-filter-cuenca", "cuenca", "cuenca");
      onFilterSelect("dl-filter-subcuenca", "subcuenca");
      onFilterSelect("dl-filter-fuente", "fuenteId");

      var varFilter = document.getElementById("dl-filter-variable");
      if (varFilter) {
        varFilter.addEventListener("change", function () {
          var next = varFilter.value || "";
          if (next === state.stationFilters.variableId) return;
          state.stationFilters.variableId = next;
          state.explorePage = 0;
          state.advancedOpen = true;
          renderWizard();
        });
      }

      var search = document.getElementById("dl-station-search");
      if (search) search.addEventListener("input", function () {
        state.stationSearch = search.value;
        state.explorePage = 0;
        renderKeepingFocus("dl-station-search");
      });

      var groupSearch = document.getElementById("dl-group-search");
      if (groupSearch) groupSearch.addEventListener("input", function () {
        state.groupSearch = groupSearch.value;
        state.groupPage = 0;
        renderKeepingFocus("dl-group-search");
      });

      var groupName = document.getElementById("dl-group-name");
      if (groupName) groupName.addEventListener("input", function () {
        state.nombreLote = groupName.value.trim() || "Mi descarga";
        persistGroupSnapshot();
      });

      wizard.querySelectorAll(".dl-pager-btn").forEach(function (btn) {
        btn.addEventListener("click", function () {
          var dir = parseInt(btn.getAttribute("data-dir"), 10) || 0;
          if (btn.getAttribute("data-pager") === "group") state.groupPage += dir;
          else state.explorePage += dir;
          renderWizard();
        });
      });
      wizard.querySelectorAll(".dl-page-size").forEach(function (sel) {
        sel.addEventListener("change", function () {
          var n = parseInt(sel.value, 10);
          if (PAGE_SIZES.indexOf(n) < 0) return;
          state.pageSize = n;
          state.explorePage = 0;
          state.groupPage = 0;
          renderWizard();
        });
      });

      var addVisible = document.getElementById("dl-add-visible");
      if (addVisible) addVisible.addEventListener("click", function () {
        addStations(filteredExploreStations().map(function (s) { return s.id; }));
        renderWizard();
      });

      var removeVisible = document.getElementById("dl-remove-visible");
      if (removeVisible) removeVisible.addEventListener("click", function () {
        var visible = {};
        filteredExploreStations().forEach(function (s) { visible[s.id] = true; });
        state.eids = state.eids.filter(function (id) { return !visible[id]; });
        persistGroupSnapshot();
        renderWizard();
      });

      var clearBtn = document.getElementById("dl-clear-group");
      if (clearBtn) clearBtn.addEventListener("click", function () {
        if (state.eids.length && confirm("\u00bfVaciar el grupo de estaciones?")) {
          clearGroup();
          renderWizard();
        }
      });

      var adv = document.getElementById("dl-advanced");
      if (adv) adv.addEventListener("toggle", function () {
        state.advancedOpen = adv.open;
      });

      wizard.querySelectorAll(".dl-tab").forEach(function (tab) {
        tab.addEventListener("click", function () {
          var t = tab.getAttribute("data-tab") || "lista";
          state.exploreTab = t;
          document.getElementById("dl-tab-lista").hidden = t !== "lista";
          document.getElementById("dl-tab-pegar").hidden = t !== "pegar";
          wizard.querySelectorAll(".dl-tab").forEach(function (x) { x.classList.toggle("active", x === tab); });
        });
      });

      var applyPaste = document.getElementById("dl-apply-paste");
      if (applyPaste) applyPaste.addEventListener("click", function () {
        var ta = document.getElementById("dl-paste-ids");
        addStations(normalizeIds(ta ? ta.value : ""));
        renderWizard();
      });

      var n1 = document.getElementById("dl-next-1");
      if (n1) n1.addEventListener("click", function () { state.step = 2; renderWizard(); });

      syncStep1Ui();
    }

    if (state.step === 2) {
      function setVids(raw, on) {
        parseVidList(raw).forEach(function (vid) {
          var i = state.vids.indexOf(vid);
          if (on && i < 0) state.vids.push(vid);
          if (!on && i >= 0) state.vids.splice(i, 1);
        });
      }
      function paintVarSelection() {
        syncHourlyGranularity();
        syncVarBoxes(wizard);
        var picks = document.getElementById("dl-var-picks");
        if (picks) picks.innerHTML = varPicksHtml();
        var hint = document.getElementById("dl-var-hint");
        if (hint) hint.innerHTML = varHintHtml();
        var nxt = document.getElementById("dl-next-2");
        if (nxt) nxt.disabled = !state.vids.length;
        if (!state.vids.length) {
          state.cobertura = null;
          paintCoverage();
          return;
        }
        scheduleCobertura();
      }
      var search = document.getElementById("dl-var-search");
      if (search) search.addEventListener("input", function () {
        state.varSearch = search.value;
        var box = document.getElementById("dl-var-cats");
        if (box) {
          box.innerHTML = varCatsHtml();
          syncVarBoxes(box);
        }
      });
      var cats = document.getElementById("dl-var-cats");
      if (cats) {
        cats.addEventListener("change", function (ev) {
          var cb = ev.target;
          if (!cb || !cb.classList || !cb.classList.contains("dl-var-cb")) return;
          setVids(cb.getAttribute("data-vid"), cb.checked);
          rememberVarFocus(cb);
          if (!cb.checked) pruneVarFocus();
          paintVarSelection();
          paintVarSpec();
        });
        cats.addEventListener("click", function (ev) {
          var t = ev.target;
          if (!t || !t.closest) return;
          var hit = t.closest(".dl-var-more, .dl-var-hit");
          if (hit) {
            var phys = hit.getAttribute("data-phys");
            state.varOpen[phys] = !state.varOpen[phys];
            var box = document.getElementById("dl-var-cats");
            if (box) {
              box.innerHTML = varCatsHtml();
              syncVarBoxes(box);
            }
            return;
          }
          var allBtn = t.closest(".dl-var-all");
          if (!allBtn) return;
          var sec = allBtn.closest(".dl-var-cat");
          var boxes = sec ? sec.querySelectorAll(".dl-var-cb.is-parent") : [];
          var turnOn = false;
          var i;
          for (i = 0; i < boxes.length; i++) if (!boxes[i].checked || boxes[i].indeterminate) turnOn = true;
          for (i = 0; i < boxes.length; i++) setVids(boxes[i].getAttribute("data-vid"), turnOn);
          if (!turnOn) pruneVarFocus();
          paintVarSelection();
          paintVarSpec();
        });
      }
      var picks = document.getElementById("dl-var-picks");
      if (picks) picks.addEventListener("click", function (ev) {
        var t = ev.target;
        if (!t || !t.closest) return;
        var rm = t.closest("button");
        if (!rm || !rm.getAttribute("data-vid")) return;
        setVids(rm.getAttribute("data-vid"), false);
        pruneVarFocus();
        paintVarSelection();
        paintVarSpec();
      });
      wireCoverageActions();
      document.getElementById("dl-back-2").addEventListener("click", function () { state.step = 1; renderWizard(); });
      document.getElementById("dl-next-2").addEventListener("click", function () {
        syncHourlyGranularity();
        state.step = 3;
        renderWizard();
      });
    }

    if (state.step === 3) {
      wizard.querySelectorAll(".dl-pill[data-gran]").forEach(function (btn) {
        btn.addEventListener("click", function () {
          var g = btn.getAttribute("data-gran");
          var i = state.granularidades.indexOf(g);
          if (i >= 0) {
            if (state.granularidades.length > 1) state.granularidades.splice(i, 1);
          } else state.granularidades.push(g);
          renderWizard();
        });
      });
      document.getElementById("dl-back-3").addEventListener("click", function () { state.step = 2; renderWizard(); });
      document.getElementById("dl-next-3").addEventListener("click", function () {
        state.step = 4;
        applyLocalEstimate();
        renderWizard();
        scheduleEstimate();
      });
    }

    if (state.step === 4) {
      wizard.querySelectorAll(".dl-pill[data-fmt]").forEach(function (btn) {
        btn.addEventListener("click", function () {
          state.formatoDescarga = btn.getAttribute("data-fmt");
          wizard.querySelectorAll(".dl-pill[data-fmt]").forEach(function (b) {
            b.classList.toggle("active", b.getAttribute("data-fmt") === state.formatoDescarga);
          });
          rescaleEstimateBytes();
          paintSummary();
        });
      });
      function onYear() {
        var y0 = parseInt(document.getElementById("dl-year-start").value, 10);
        var y1 = parseInt(document.getElementById("dl-year-end").value, 10);
        if (!y0 || !y1) return;
        state.yearStart = y0;
        state.yearEnd = y1;
        applyLocalEstimate();
        paintSummary();
        scheduleEstimate();
      }
      var ys = document.getElementById("dl-year-start");
      var ye = document.getElementById("dl-year-end");
      if (ys) ys.addEventListener("input", onYear);
      if (ye) ye.addEventListener("input", onYear);
      document.getElementById("dl-back-4").addEventListener("click", function () { state.step = 3; renderWizard(); });
      document.getElementById("dl-btn-submit").addEventListener("click", submitBulk);
    }
  }

  function renderWizard() {
    var wizard = document.getElementById("dl-wizard");
    if (!wizard) return;
    var main = document.getElementById("dl-main");
    if (main) main.classList.toggle("dl-has-cta", state.step === 1);
    if (state.step === 1) renderStep1(wizard);
    else if (state.step === 2) renderStep2(wizard);
    else if (state.step === 3) renderStep3(wizard);
    else renderStep4(wizard);
    wireStepEvents();
  }

  function renderGate() {
    var gate = document.getElementById("dl-gate");
    var wizard = document.getElementById("dl-wizard");
    hide(wizard);
    show(gate);
    gate.innerHTML =
      "<h1>Descarga de datos clim\u00e1ticos</h1>" +
      "<p>Cuenta gratuita: hasta 50 MB de descarga al mes. Selecciona estaciones, variables y granularidades seg\u00fan tu presupuesto.</p>" +
      '<a class="dl-btn-secondary" href="#" data-rodaja-signup>Crear cuenta gratis</a>';
  }

  function submitBulk() {
    var btn = document.getElementById("dl-btn-submit");
    if (btn) btn.disabled = true;
    var fechas = fechaFromYears(state.yearStart, state.yearEnd);
    rodajaApiFetch("/solicitar-descarga-bulk", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        estacion_ids: state.eids,
        variable_ids: state.vids,
        granularidades: state.granularidades,
        fecha_inicio: fechas.inicio,
        fecha_fin: fechas.fin,
        formato_descarga: state.formatoDescarga,
        nombre_lote: state.nombreLote,
        modo_cobertura: state.modoCobertura,
        email: clerkEmail()
      })
    }).then(function (r) { return r.json().then(function (d) { return { ok: r.ok, data: d }; }); })
      .then(function (res) {
        if (!res.ok) {
          alert((res.data && res.data.error) || "Error al solicitar descarga");
          if (btn) btn.disabled = false;
          return;
        }
        if (typeof rodajaTrack === "function") {
          rodajaTrack("download_request", {
            station_count: (state.eids && state.eids.length) || 0,
            format: state.formatoDescarga || "csv"
          });
        }
        pollLote(apiScalar(res.data.lote_id));
      }).catch(function () {
        alert("Error de red");
        if (btn) btn.disabled = false;
      });
  }

  function pollLote(loteId) {
    hide(document.getElementById("dl-wizard"));
    var status = document.getElementById("dl-status");
    show(status);
    status.innerHTML = '<div class="dl-progress-card"><h2>Preparando paquete bulk</h2><p id="dl-lote-msg">En cola\u2026</p></div>';

    var pollDone = false;

    function check() {
      if (pollDone) return;
      rodajaApiFetch("/descarga/lote/" + loteId).then(function (r) { return r.json(); }).then(function (data) {
        var estado = apiScalar(data.estado);
        var jobsListos = apiScalar(data.jobs_listos);
        var jobsTotal = apiScalar(data.jobs_total);
        var archivoNombre = apiScalar(data.archivo_nombre);
        var msg = document.getElementById("dl-lote-msg");
        if (msg) {
          msg.textContent = "Estado: " + estado + " (" + (jobsListos || 0) + "/" + (jobsTotal || 0) + " jobs)";
        }
        if (estado === "listo" || estado === "parcial") {
          pollDone = true;
          status.innerHTML = '<div class="dl-progress-card"><h2>Paquete listo</h2>' +
            '<button type="button" id="dl-btn-lote" class="dl-btn-primary">Descargar ZIP</button></div>';
          document.getElementById("dl-btn-lote").addEventListener("click", function () {
            var path = "/descarga/lote/" + loteId + "/archivo";
            var fname = archivoNombre || "rodaja_bulk.zip";
            if (typeof rodajaDownloadFile === "function") {
              rodajaDownloadFile(path, fname).catch(function (err) {
                alert((err && err.message) || "No se pudo descargar el archivo");
              });
            } else {
              rodajaApiFetch(path).then(function (r) { return r.blob(); }).then(function (blob) {
                var a = document.createElement("a");
                a.href = URL.createObjectURL(blob);
                a.download = fname;
                a.click();
              }).catch(function (err) {
                alert((err && err.message) || "No se pudo descargar el archivo");
              });
            }
          });
          loadCuota();
          return;
        }
        if (estado === "error") {
          status.innerHTML = "<div class=\"dl-progress-card\"><h2>Error en el lote</h2></div>";
          return;
        }
        setTimeout(check, JOB_POLL_MS);
      }).catch(function () { setTimeout(check, JOB_POLL_MS); });
    }
    check();
  }

  function boot() {
    if (bootPromise) {
      return bootPromise;
    }
    initEidsFromBoot();
    if (typeof rodajaInitAuth === "function") {
      bootPromise = rodajaInitAuth().then(function () {
        var signedIn = rodajaIsSignedIn && rodajaIsSignedIn();
        if (!signedIn) {
          renderGate();
          bootPromise = null;
          return;
        }
        hide(document.getElementById("dl-gate"));
        show(document.getElementById("dl-wizard"));
        showWizardMessage("<div class=\"dl-card\"><p>Cargando cat\u00e1logo\u2026</p></div>");
        loadVarSpecs();
        return loadCuota().then(loadCatalogs).then(function () {
          if (state.vids.length) state.step = 2;
          renderWizard();
          if (state.step >= 2) fetchCobertura().then(function () { paintCoverage(); });
        }).catch(function (err) {
          bootPromise = null;
          var msg = (err && err.message) ? err.message : "No se pudo cargar el cat\u00e1logo.";
          showWizardMessage(
            "<div class=\"dl-card\"><p>" + escapeHtml(msg) + "</p>" +
            "<p><button type=\"button\" class=\"dl-btn-secondary\" onclick=\"location.reload()\">Reintentar</button></p></div>"
          );
        });
      }).catch(function (err) {
        bootPromise = null;
        showWizardMessage(
          "<div class=\"dl-card\"><p>Error de autenticaci\u00f3n. <button type=\"button\" class=\"dl-btn-secondary\" onclick=\"location.reload()\">Reintentar</button></p></div>"
        );
      });
      return bootPromise;
    }
    renderGate();
    return Promise.resolve();
  }

  function selfCheckStep1() {
    function eq(got, exp, msg) {
      if (got !== exp) throw new Error(msg + ": " + JSON.stringify(got));
    }
    eq(stationMacrozona({ cod_reg: 15 }), "Norte", "arica");
    eq(stationMacrozona({ cod_reg: 2 }), "Norte", "antofagasta");
    eq(stationMacrozona({ cod_reg: 3 }), "Centro Norte", "atacama");
    eq(stationMacrozona({ cod_reg: 4, macrozona: "N" }), "Centro Norte", "cod gana");
    eq(stationMacrozona({ cod_reg: 13 }), "Centro", "rm");
    eq(stationMacrozona({ cod_reg: 16 }), "Sur", "nuble");
    eq(stationMacrozona({ cod_reg: 12 }), "Austral", "magallanes");
    eq(stationMacrozona({ macrozona: "C" }), "Centro", "letra");
    eq(formatMiles(2171), "2.171", "miles");
    eq(seleccionadasLabel(0), "0 seleccionadas", "cero");
    eq(seleccionadasLabel(1), "1 seleccionada", "una");

    var catalog = [];
    var i;
    for (i = 1; i <= 2171; i++) {
      var cod = i <= 3 ? 15 : (i <= 13 ? 3 : 13);
      catalog.push({
        id: i,
        nombre: i === 1 ? "Visviri" : ("Estacion " + i),
        cod_reg: cod,
        nom_reg: cod === 15 ? "Arica y Parinacota" : (cod === 3 ? "Atacama" : "Metropolitana de Santiago"),
        nom_com: cod === 15 ? "General Lagos" : (cod === 3 ? "Copiapo" : "Santiago"),
        cuenca: cod === 15 ? "Altiplanicas" : "Maipo",
        fuente: "DGA",
        id_fuente: 1
      });
    }
    state.stationsCatalog = catalog;
    state.variablesCatalog = [{ id: 1, nombre: "Precipitacion" }];
    state.regionsCatalog = [];
    rebuildStationMap();
    state.eids = [];
    state.stationFilters.macrozona = "";
    state.advancedOpen = false;
    var box = { innerHTML: "" };
    renderStep1(box);
    var html = box.innerHTML;
    eq(html.indexOf("Paso 1 de 4") >= 0, true, "paso");
    eq(html.indexOf("Centro Norte") >= 0, true, "chip");
    eq(html.indexOf("2.171") >= 0, true, "miles ui");
    eq(html.indexOf("1\u201310 de 2.171") >= 0, true, "pagina");
    eq(html.indexOf('value="10" selected') >= 0, true, "tamano");
    eq(html.split('type="checkbox" data-eid=').length - 1, 10, "diez filas");
    eq(html.indexOf("Estacion 11") >= 0, false, "fuera de pagina");
    eq(html.indexOf("Agregar las 2.171") >= 0, true, "boton");
    eq(html.indexOf("/ 200") >= 0, false, "sin tope");
    eq(html.indexOf("dl-btn-danger") >= 0, true, "vaciar");
    eq(html.indexOf("0 seleccionadas") >= 0, true, "cerca");
    eq(html.indexOf("agrega al menos 1") >= 0, true, "cta");
    eq(html.indexOf("dl-fuente") >= 0, true, "fuente");
    eq(html.indexOf('id="dl-advanced" open') >= 0, false, "colapsado");
    eq(html.indexOf("Visviri") >= 0, true, "nombre");
    state.stationFilters.macrozona = "Centro Norte";
    renderStep1(box);
    eq(box.innerHTML.indexOf("Agregar las 10") >= 0, true, "centro norte");
    eq(box.innerHTML.indexOf("mostrando") >= 0, false, "sin recorte");
    state.stationFilters.macrozona = "";
    state.explorePage = 1;
    renderStep1(box);
    eq(box.innerHTML.indexOf("Estacion 11") >= 0, true, "pagina 2");
    eq(box.innerHTML.indexOf("Visviri") >= 0, false, "pagina 2 sin primera");
    state.explorePage = 0;
    addStations(catalog.map(function (s) { return s.id; }));
    eq(state.eids.length, 2171, "agrega todas");
    renderStep1(box);
    eq(box.innerHTML.split('class="dl-group-member">').length - 1, 10, "grupo pagina");
    eq(box.innerHTML.indexOf("Estacion 2171") >= 0, false, "grupo no vuelca");
    if (typeof global.RODAJA_DL_HTML_OUT === "function") global.RODAJA_DL_HTML_OUT(html);

    var vcat = [
      { id: 1, nombre: "precipitacion", nombre_abr: "pp", unidad: "mm" },
      { id: 15, nombre: "viento_medio", nombre_abr: "u2_mean", unidad: "km/h" },
      { id: 16, nombre: "viento_maximo", nombre_abr: "u_max_diario", unidad: "km/h" },
      { id: 7, nombre: "viento_velocidad", nombre_abr: "u_10_kt", unidad: "km/h" },
      { id: 19, nombre: "viento_maximo_dmc", nombre_abr: "u_rafaga", unidad: "km/h" },
      { id: 5, nombre: "caudal", nombre_abr: "q", unidad: "m3/s" },
      { id: 10, nombre: "cloud_octas", nombre_abr: "co", unidad: "s/u" },
      { id: 12, nombre: "presion", nombre_abr: "pa", unidad: "s/u" },
      { id: 99, nombre: "algo_nuevo", nombre_abr: "xx", unidad: "foo" }
    ];
    var wind = groupVariables(vcat, "").filter(function (g) { return g.id === "viento"; })[0];
    eq(wind.items.length, 3, "viento fisico");
    var vmax = wind.items.filter(function (it) { return it.phys === "v_max"; })[0];
    eq(vmax.variants.length, 2, "max tiene 2m y 10m");
    eq(vmax.variants[0].label !== vmax.variants[1].label, true, "alturas distintas");
    eq(varFace(vcat[1]).label, "Velocidad media", "medio sin altura en el nombre");
    eq(wind.items.filter(function (it) { return it.phys === "v_dir"; })[0].face.label, "Direcci\u00f3n predominante", "predominante es direccion");
    eq(varFace(vcat[5]).unit, "m\u00b3/s", "caudal");
    eq(varFace(vcat[6]).unit, "octas", "nubosidad");
    eq(varFace(vcat[7]).unit, "hPa", "presion");
    eq(groupVariables(vcat, "").filter(function (g) { return g.id === "otras"; })[0].items.length, 1, "desconocida");
    var vcatH = vcat.concat([{ id: 21, nombre: "viento_rafaga_horaria", nombre_abr: "u_rafaga_h", unidad: "m/s" }]);
    var windH = groupVariables(vcatH, "").filter(function (g) { return g.id === "viento"; })[0];
    eq(windH.items.filter(function (it) { return it.face.label === "R\u00e1faga horaria"; }).length, 1, "rafaga horaria");
    eq(windH.items.filter(function (it) { return it.face.label === "R\u00e1faga horaria"; })[0].face.unit, "km/h", "rafaga horaria en km/h");
    eq(windH.items.filter(function (it) { return it.phys === "v_max"; })[0].vars.length, 2, "horaria no se mezcla con el maximo");
    eq(groupVariables(vcatH, "").filter(function (g) { return g.id === "otras"; })[0].items.length, 1, "horaria no cae en otras");
    eq(groupVariables(vcat, "viento_maximo").filter(function (g) { return g.id === "viento"; })[0].items.length, 1, "busca por nombre interno");
    var vdir = vcat.concat([
      { id: 8, nombre: "viento_direccion", nombre_abr: "u_10_dire", unidad: "s/u" },
      { id: 20, nombre: "viento_direccion_dga", nombre_abr: "u2_dire", unidad: "grados" }
    ]);
    var windDir = groupVariables(vdir, "").filter(function (g) { return g.id === "viento"; })[0];
    var dirRows = windDir.items.filter(function (it) { return it.phys === "v_dir"; });
    eq(dirRows.length, 1, "una direccion");
    eq(dirRows[0].variants.length, 2, "2 m y 10 m");
    eq(dirRows[0].variants.filter(function (s) { return s.key === "10 m"; })[0].vars.length, 2, "10 m junta direccion y velocidad");
    eq(dirRows[0].face.label, "Direcci\u00f3n predominante", "label direccion");
    state.variablesCatalog = vcat;
    state.vids = [15, 19];
    state.varSearch = "";
    state.step = 2;
    var vbox = { innerHTML: "" };
    renderStep2(vbox);
    eq(vbox.innerHTML.indexOf("Velocidad media") >= 0, true, "label visible");
    eq(vbox.innerHTML.indexOf("Predominante, 10 m") >= 0, false, "sin fila predominante");
    eq(vbox.innerHTML.indexOf("Direcci\u00f3n predominante") >= 0, true, "fila direccion");
    eq(vbox.innerHTML.indexOf(">10 m<") >= 0, true, "tercer nivel en el html");
    eq(vbox.innerHTML.indexOf("viento_maximo_dmc") >= 0, false, "sin snake");
    eq(vbox.innerHTML.indexOf("km/h") >= 0, true, "viento en km/h");
    eq(vbox.innerHTML.indexOf("2</b> variables elegidas") >= 0, true, "conteo");
    eq(vbox.innerHTML.indexOf("m\u00b3/s") >= 0, true, "caudal ui");
    state.varSpecs = { "15": "Media <b>x", "20": "Direccion a 2 m, en grados", "7": "Velocidad predominante", "8": "Direccion a 10 m, en grados" };
    state.variablesCatalog = vdir;
    state.vids = [15];
    state.varFocus = "15";
    eq(varSpecHtml().indexOf("Media") >= 0, true, "ficha");
    eq(varSpecHtml().indexOf("<b>") >= 0, false, "ficha escapa");
    state.vids = [];
    eq(varSpecHtml(), "", "sin seleccion no hay ficha");
    state.vids = [20, 7, 8];
    state.varFocus = "20,7,8";
    var fichaDir = varSpecHtml();
    eq(fichaDir.indexOf("2 m") >= 0, true, "ficha 2 m");
    eq(fichaDir.indexOf("10 m") >= 0, true, "ficha 10 m");
    eq(fichaDir.indexOf("Velocidad predominante") >= 0, false, "direccion no repite la velocidad");
    state.varFocus = "7,8";
    eq(varSpecHtml().indexOf("10 m, en grados") >= 0, true, "10 m solo direccion");
    eq(varSpecHtml().indexOf("2 m") >= 0, false, "10 m no arrastra 2 m");
    state.vids = [15];
    state.varFocus = "20,7,8";
    eq(varSpecHtml(), "", "ficha vieja no se queda");
  }

  if (global.RODAJA_DL_SELFCHECK) {
    selfCheckStep1();
    return;
  }

  if (typeof rodajaOnAuthChange === "function") {
    rodajaOnAuthChange(function (user, token) {
      if (user && token) boot();
      else {
        bootPromise = null;
        renderGate();
      }
    });
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", boot);
  else boot();
})(typeof window !== "undefined" ? window : global);
