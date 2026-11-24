(function (global) {
  "use strict";

  var SNAPSHOT_KEY = "rodaja_alertas_handoff";
  var TAB_KEY = "rodaja_alertas_tab";
  var activeTab = "resumen";
  var openRegions = {};
  var openMacrozones = {};
  var geoSearch = "";
  var geoAdvancedOpen = false;

  function macrozonasMeta() {
    return [
      { id: "norte", etiqueta: "Macrozona Norte", cod_regs: [1, 2, 15] },
      { id: "centro_norte", etiqueta: "Macrozona Centro Norte", cod_regs: [3, 4] },
      { id: "centro", etiqueta: "Macrozona Centro", cod_regs: [5, 6, 7, 13] },
      { id: "sur", etiqueta: "Macrozona Sur", cod_regs: [8, 9, 10, 14, 16] },
      { id: "austral", etiqueta: "Macrozona Austral", cod_regs: [11, 12] }
    ];
  }

  function codToMacrozonaId(cod) {
    var c = parseInt(cod, 10);
    if (isNaN(c)) return "sin_zona";
    var hit = null;
    macrozonasMeta().some(function (mz) {
      if (mz.cod_regs.indexOf(c) >= 0) { hit = mz.id; return true; }
      return false;
    });
    return hit || "sin_zona";
  }

  var state = {
    tier: "gratuito",
    email: "",
    maxSubs: 1,
    maxVars: 1,
    pruebaEnviadaHoy: false,
    scopes: ["favorita", "grupo", "estacion"],
    abono: {},
    reglas: [],
    umbrales: [],
    umbralContexto: {},
    preferencias: {},
    stats: {},
    variables: [],
    regions: [],
    stationsCatalog: [],
    comunas: [],
    cuencas: [],
    alerta_temprana_activa: false,
    alerta_tardia_activa: true
  };

  function $(id) { return document.getElementById(id); }
  function esc(s) {
    var d = document.createElement("div");
    d.textContent = s == null ? "" : String(s);
    return d.innerHTML;
  }
  function qs(name) { return new URLSearchParams(global.location.search).get(name); }
  function show(el) { if (el) el.hidden = false; }
  function hide(el) { if (el) el.hidden = true; }

  function setStatus(msg, ok) {
    var el = $("al-status");
    if (!el) return;
    el.textContent = msg || "";
    el.className = "al-status" + (msg ? (ok ? " ok" : " err") : "");
  }


  function apiScalar(v) {
    if (v == null) return v;
    return Array.isArray(v) ? (v.length ? v[0] : null) : v;
  }
  function apiBool(v) {
    var x = apiScalar(v);
    return x === true || x === 1 || x === "1" || x === "true" || x === "TRUE";
  }
  function normalizeRegla(r) {
    if (!r || typeof r !== "object") return r;
    return {
      id: apiScalar(r.id),
      nombre: apiScalar(r.nombre) || "",
      scope_type: apiScalar(r.scope_type) || "",
      scope_value: r.scope_value || {},
      activa: apiBool(r.activa),
      scope_configurado: apiBool(r.scope_configurado),
      n_estaciones: parseInt(apiScalar(r.n_estaciones), 10) || 0
    };
  }

  function normalizeSuscripciones(subs) {
    subs = subs || {};
    ["favorita", "grupo"].forEach(function (scope) {
      if (!subs[scope]) return;
      subs[scope].activa = apiBool(subs[scope].activa);
      subs[scope].scope_configurado = apiBool(subs[scope].scope_configurado);
      if (subs[scope].regla_id != null) subs[scope].regla_id = apiScalar(subs[scope].regla_id);
    });
    return subs;
  }  function syncSuscripcionFromRegla(r) {
    r = normalizeRegla(r);
    if (!r || !r.scope_type) return;
    if (r.scope_type !== "favorita" && r.scope_type !== "grupo") return;
    state.abono[r.scope_type] = state.abono[r.scope_type] || {};
    state.abono[r.scope_type].activa = r.activa;
    state.abono[r.scope_type].regla_id = r.id;
    state.abono[r.scope_type].scope_configurado = r.scope_configurado;
  }
  function normalizeList(val) {
    if (Array.isArray(val)) return val;
    if (!val || typeof val !== "object") return [];
    var keys = Object.keys(val);
    if (!keys.length) return [];
    if (Array.isArray(val[keys[0]])) {
      var n = val[keys[0]].length;
      var out = [];
      for (var i = 0; i < n; i++) {
        var row = {};
        keys.forEach(function (k) { row[k] = val[k][i]; });
        out.push(row);
      }
      return out;
    }
    return keys.map(function (k) { return val[k]; });
  }

  function normalizeScopes(val) {
    var rows = normalizeList(val);
    if (rows.length) {
      return rows.map(function (x) { return String(apiScalar(x)); }).filter(Boolean);
    }
    if (Array.isArray(val)) return val.map(function (x) { return String(apiScalar(x)); }).filter(Boolean);
    if (val && typeof val === "object") {
      return Object.keys(val).map(function (k) { return String(apiScalar(val[k])); }).filter(Boolean);
    }
    return [];
  }

  function applyMainPayload(main) {
    state.tier = apiScalar(main.tier) || "gratuito";
    state.email = apiScalar(main.email) || "";
    state.maxSubs = apiScalar(main.max_alertas_suscripcion);
    if (state.maxSubs === "" || typeof state.maxSubs === "undefined") state.maxSubs = null;
    state.maxVars = apiScalar(main.max_alertas_variables);
    if (state.maxVars === "" || typeof state.maxVars === "undefined") state.maxVars = null;
    state.pruebaEnviadaHoy = apiBool(main.prueba_enviada_hoy);
    state.scopes = normalizeScopes(main.alertas_scopes);
    if (main.variables_alertas && main.variables_alertas.length) {
      state.variables = normalizeList(main.variables_alertas);
    }
    if (!state.scopes.length) {
      state.scopes = isProTier() ? ["favorita", "grupo", "estacion"] : ["favorita"];
    }
    state.abono = normalizeSuscripciones(main.abono || {});
    state.reglas = normalizeList(main.reglas).map(normalizeRegla);
    state.umbrales = main.umbrales;
    state.umbralContexto = main.umbral_contexto || {};
    state.preferencias = main.preferencias || {};
    state.alerta_temprana_activa = apiBool(main.alerta_temprana_activa);
    state.alerta_tardia_activa = apiBool(main.alerta_tardia_activa);
    if (main.alerta_tardia_activa == null) state.alerta_tardia_activa = true;
    state.carta_devuelta = main.carta_devuelta || { bloqueado: false };
    if (!state.preferencias.email_efectivo) {
      state.preferencias.email_efectivo = main.email_efectivo || state.email;
    }
  }

  function applyStatsPayload(stats) {
    state.stats = stats || {};
    state.stats.por_mes = normalizeList(state.stats.por_mes);
    state.stats.por_dia = normalizeList(state.stats.por_dia);
  }

  function tierLabel(t) {
    t = apiScalar(t) || t;
    if (t === "pro") return "Pro";
    if (t === "enterprise") return "Institucional";
    return "Gratuito";
  }

  function apiFetch(path, opts) {
    if (typeof rodajaApiFetch !== "function") return Promise.reject(new Error("Auth no disponible"));
    return rodajaApiFetch(path, opts || {}).then(function (r) {
      return r.text().then(function (text) {
        var body = null;
        if (text) {
          try { body = JSON.parse(text); } catch (e) {
            if (r.status === 429) throw new Error("Demasiadas solicitudes; espera un momento e intenta de nuevo.");
            throw new Error((r.status === 500) ? "500 - Internal server error" : ("HTTP " + r.status));
          }
        }
        if (!r.ok) throw new Error((body && body.error) || ("HTTP " + r.status));
        return body;
      });
    });
  }

  function loadVariables() {
    if (state.variables && state.variables.length) return Promise.resolve();
    return apiFetch("/me/alertas/variables").then(function (data) {
      state.variables = normalizeList((data && data.variables) || data || []);
      if (!Array.isArray(state.variables)) state.variables = [];
    }).catch(function () { state.variables = []; });
  }

  function isProTier() {
    var t = apiScalar(state.tier) || state.tier;
    return t === "pro" || t === "enterprise";
  }

  function singleVariableOnly() {
    if (isProTier()) return false;
    var mv = state.maxVars;
    if (mv == null || mv === "") return true;
    return parseInt(mv, 10) === 1;
  }

  function loadCatalog() {
    return apiFetch("/estaciones?catalog=1&chile_only=1&con_datos=1").then(function (data) {
      var rows = (data && data.estaciones) || data || [];
      state.stationsCatalog = Array.isArray(rows) ? rows : [];
      var comSet = {};
      var cuenSet = {};
      state.stationsCatalog.forEach(function (s) {
        if (s.nom_com) comSet[String(s.nom_com).trim()] = true;
        var cu = s.cuenca || s.nom_cuen;
        if (cu) cuenSet[String(cu).trim()] = true;
      });
      state.comunas = Object.keys(comSet).sort();
      state.cuencas = Object.keys(cuenSet).sort();
    }).catch(function () {});
  }

  function loadRegions() {
    return apiFetch("/regiones").then(function (data) {
      state.regions = (data && data.regiones) || data || [];
      if (!Array.isArray(state.regions)) state.regions = [];
    }).catch(function () { state.regions = []; });
  }


  function ensureFavoritesSynced() {
    if (typeof rodajaGetFavorites !== "function" || typeof rodajaSyncFavoritesToCloud !== "function") return Promise.resolve();
    if (typeof rodajaIsSignedIn !== "function" || !rodajaIsSignedIn()) return Promise.resolve();
    var store = rodajaGetFavorites();
    if (!store || !store.primary) return Promise.resolve();
    return rodajaSyncFavoritesToCloud(store).catch(function () { return false; });
  }
  function loadCore() {
    return ensureFavoritesSynced().then(function () {
      return Promise.all([
      apiFetch("/me/alertas"),
      apiFetch("/me/alertas/estadisticas"),
      loadVariables(),
      loadRegions()
    ]).then(function (results) {
      applyMainPayload(results[0] || {});
      applyStatsPayload(results[1]);
    });
    }).catch(function (err) {
      throw err;
    });
  }

  function renderHeatmap() {
    var wrap = $("al-heatmap");
    if (!wrap) return;
    var porDia = (state.stats && state.stats.por_dia) || [];
    wrap.innerHTML = typeof rodajaAlertasHeatmapHtml === "function"
      ? rodajaAlertasHeatmapHtml(porDia)
      : '<p class="al-empty">Sin actividad registrada.</p>';
  }
  function renderLoading(msg) {
    var gate = $("al-gate");
    var panel = $("al-panel");
    if (!gate) return;
    hide(panel);
    gate.innerHTML =
      "<h1>Panel de alertas</h1>" +
      "<p>" + esc(msg || "Cargando\u2026") + "</p>";
    show(gate);
  }

  function renderGate() {
    var gate = $("al-gate");
    if (!gate) return;
    gate.innerHTML =
      "<h1>Panel de alertas</h1>" +
      "<p>Inicia sesi\u00f3n para configurar alertas por correo. Recibir\u00e1s como m\u00e1ximo <strong>un correo al d\u00eda</strong> con todas las alertas.</p>" +
      '<a class="al-btn al-btn-primary" href="#" data-rodaja-signin>Iniciar sesi\u00f3n</a> ' +
      '<a class="al-btn al-btn-ghost" href="#" data-rodaja-signup>Crear cuenta</a>';
    show(gate);
    hide($("al-panel"));
  }

  function varKind(v) {
    var abr = String((v && (v.nombre_abr || v.abr)) || "").toLowerCase();
    if (abr === "pp") return "rain";
    if (abr === "t_max") return "heat";
    if (abr === "t_min") return "cold";
    return "";
  }

  function varIcon(kind) {
    if (kind === "rain") return '<span class="al-pill-ic al-pill-ic--rain" aria-hidden="true"><svg class="i" viewBox="0 0 24 24"><path d="M7 15a5 5 0 0 1 1-9.9A6 6 0 0 1 19 8a4 4 0 0 1-1 8H7z"/></svg></span>';
    if (kind === "heat") return '<span class="al-pill-ic al-pill-ic--heat" aria-hidden="true"><svg class="i" viewBox="0 0 24 24"><circle cx="12" cy="12" r="5"/><path d="M12 1v3M12 20v3M4.2 4.2l2.1 2.1M17.7 17.7l2.1 2.1M1 12h3M20 12h3"/></svg></span>';
    if (kind === "cold") return '<span class="al-pill-ic al-pill-ic--cold" aria-hidden="true"><svg class="i" viewBox="0 0 24 24"><path d="M12 2v20M5 6l14 12M19 6L5 18M2 12h20"/></svg></span>';
    return "";
  }

  function variableLabel(v) {
    var abr = String((v && (v.nombre_abr || v.abr)) || "").toLowerCase();
    var byAbr = {
      pp: "Precipitaci\u00f3n",
      t_max: "Temperatura m\u00e1xima",
      t_min: "Temperatura m\u00ednima",
      u_rafaga: "R\u00e1faga"
    };
    if (byAbr[abr]) return byAbr[abr];
    var raw = String((v && (v.nombre || v.name)) || "");
    var bySnake = {
      precipitacion: "Precipitaci\u00f3n",
      temperatura_maxima: "Temperatura m\u00e1xima",
      temperatura_minima: "Temperatura m\u00ednima",
      viento_rafaga: "R\u00e1faga"
    };
    return bySnake[raw] || raw || ("Var " + ((v && (v.id || v.variable_id)) || ""));
  }

  function variableChipsHtml() {
    var selected = state.preferencias.variable_ids || null;
    var single = singleVariableOnly();
    var allOn = !single && (!selected || !selected.length);
    var html = "";
    if (!single) {
      html += '<button type="button" class="al-chip' + (allOn ? " on" : "") + '" data-vid="">Todas</button>';
    }
    state.variables.forEach(function (v) {
      var id = v.id || v.variable_id;
      var name = variableLabel(v);
      var on = single
        ? (selected && selected.length === 1 && selected[0] === id)
        : (!allOn && selected && selected.indexOf(id) >= 0);
      var kind = varKind(v);
      html += '<button type="button" class="al-chip' + (kind ? " al-chip--" + kind : "") + (on ? " on" : "") + '" data-vid="' + id + '">' +
        varIcon(kind) + esc(name) + "</button>";
    });
    return html;
  }

  function ruleCardHtml(r) {
    var nEst = parseInt(apiScalar(r.n_estaciones), 10) || 0;
    var hint = !r.scope_configurado
      ? "Configura el alcance en el Explorador"
      : (r.scope_type === "grupo" && !nEst
        ? "Sin estaciones agregadas todav\u00eda"
        : (nEst ? (nEst + " estaci\u00f3n" + (nEst === 1 ? "" : "es")) : "Alcance configurado"));
    var badge = r.scope_type === "favorita" ? '<span class="al-rec-badge">Recomendado</span> ' : "";
  var disabled = !r.scope_configurado ? " disabled" : "";
    return (
      '<div class="al-rule" data-regla-id="' + esc(r.id) + '" data-scope-type="' + esc(r.scope_type) + '">' +
        '<div class="al-rule-text">' +
          badge + "<strong>" + esc(r.nombre || r.scope_type) + "</strong>" +
          '<span class="al-rule-hint">' + esc(hint) + "</span>" +
        "</div>" +
        '<label class="al-switch">' +
          '<input type="checkbox" class="al-rule-toggle"' + (apiBool(r.activa) ? " checked" : "") + disabled + " />" +
          '<span class="al-slider"></span>' +
        "</label>" +
        (r.scope_type !== "favorita" && r.scope_type !== "grupo"
          ? '<button type="button" class="al-btn al-btn-ghost al-btn-sm al-rule-del">Eliminar</button>'
          : "") +
      "</div>"
    );
  }

  function legacyToggleHtml(scope, label) {
    var sub = state.abono[scope] || {};
    var ok = sub.scope_configurado;
    var hint = scope === "grupo"
      ? "Sin estaciones agregadas todav\u00eda"
      : (ok ? "1 estaci\u00f3n" : "Configura en el Explorador");
    var badge = scope === "favorita" ? '<span class="al-rec-badge">Recomendado</span> ' : "";
    return (
      '<div class="al-rule" data-legacy-scope="' + scope + '">' +
        '<div class="al-rule-text">' + badge + "<strong>" + esc(label) + "</strong>" +
        '<span class="al-rule-hint">' + esc(hint) + "</span></div>" +
        '<label class="al-switch">' +
          '<input type="checkbox" class="al-legacy-toggle" data-scope="' + scope + '"' +
          (apiBool(sub.activa) ? " checked" : "") + (ok ? "" : " disabled") + " />" +
          '<span class="al-slider"></span></label></div>'
    );
  }

  function sv(r, key) {
    return apiScalar((r && r.scope_value) ? r.scope_value[key] : null);
  }

  function normalizeComuna(s) {
    return String(s == null ? "" : s).trim().toLowerCase();
  }

  function geoTree() {
    var byReg = {};
    (state.regions || []).forEach(function (r) {
      var cod = parseInt(r.cod_reg != null ? r.cod_reg : r.id, 10);
      if (isNaN(cod)) return;
      byReg[cod] = { cod: cod, nombre: r.nom_reg || r.nombre || ("Regi\u00f3n " + cod), comunas: {} };
    });
    (state.stationsCatalog || []).forEach(function (s) {
      var cod = parseInt(s.cod_reg, 10);
      if (isNaN(cod)) return;
      if (!byReg[cod]) byReg[cod] = { cod: cod, nombre: s.nom_reg || ("Regi\u00f3n " + cod), comunas: {} };
      var com = s.nom_com ? String(s.nom_com).trim() : "";
      if (com) byReg[cod].comunas[com] = true;
    });
    var regList = Object.keys(byReg).map(function (k) { return byReg[k]; })
      .filter(function (r) { return Object.keys(r.comunas).length > 0; })
      .map(function (r) {
        r.comunaList = Object.keys(r.comunas).sort(function (a, b) { return a.localeCompare(b); });
        r.mzId = codToMacrozonaId(r.cod);
        return r;
      })
      .sort(function (a, b) { return a.nombre.localeCompare(b.nombre); });

    var buckets = {};
    macrozonasMeta().forEach(function (mz) {
      buckets[mz.id] = { id: mz.id, etiqueta: mz.etiqueta, regions: [] };
    });
    buckets.sin_zona = { id: "sin_zona", etiqueta: "Sin zona", regions: [] };
    regList.forEach(function (rg) {
      var bid = buckets[rg.mzId] ? rg.mzId : "sin_zona";
      buckets[bid].regions.push(rg);
    });
    return macrozonasMeta().map(function (mz) { return buckets[mz.id]; })
      .concat(buckets.sin_zona.regions.length ? [buckets.sin_zona] : [])
      .filter(function (mz) { return mz.regions.length > 0; });
  }

  function selectedRegions() {
    var set = {};
    (state.reglas || []).forEach(function (r) {
      if (r.scope_type !== "region") return;
      var cod = parseInt(sv(r, "cod_reg"), 10);
      if (!isNaN(cod)) set[cod] = r.id;
    });
    return set;
  }

  function selectedComunas() {
    var set = {};
    (state.reglas || []).forEach(function (r) {
      if (r.scope_type !== "comuna") return;
      var nom = sv(r, "nom_com");
      if (nom) set[normalizeComuna(nom)] = r.id;
    });
    return set;
  }

  function sourceToggleHtml(scope, label) {
    var r = (state.reglas || []).filter(function (x) { return x.scope_type === scope; })[0];
    if (r) return ruleCardHtml(r);
    return legacyToggleHtml(scope, label);
  }

  function geoReglas() {
    return (state.reglas || []).filter(function (r) {
      return r.scope_type === "region" || r.scope_type === "comuna";
    });
  }

  function advancedOn() {
    if (geoAdvancedOpen) return true;
    if (geoReglas().some(function (r) { return r.activa; })) {
      geoAdvancedOpen = true;
      return true;
    }
    return false;
  }

  function advancedHint(on) {
    var nReg = Object.keys(selectedRegions()).length;
    var nCom = Object.keys(selectedComunas()).length;
    if (!nReg && !nCom) return "Por macrozona, regi\u00f3n o comuna";
    var bits = [];
    if (nReg) bits.push(nReg + " regi\u00f3n" + (nReg === 1 ? "" : "es"));
    if (nCom) bits.push(nCom + " comuna" + (nCom === 1 ? "" : "s"));
    var txt = bits.join(" \u00b7 ");
    return on ? txt : (txt + " guardadas");
  }

  function advancedScopeHtml() {
    var on = advancedOn();
    return (
      '<div class="al-rule" data-scope-mode="avanzada">' +
        '<div class="al-rule-text">' +
          "<strong>Selecci\u00f3n avanzada por regi\u00f3n y comuna</strong>" +
          '<span class="al-rule-hint">' + esc(advancedHint(on)) + "</span>" +
        "</div>" +
        '<label class="al-switch">' +
          '<input type="checkbox" class="al-advanced-toggle"' + (on ? " checked" : "") + " />" +
          '<span class="al-slider"></span>' +
        "</label>" +
      "</div>" +
      '<div class="al-geo-filters"' + (on ? "" : " hidden") + ">" +
        '<p class="al-tier-note">Abre una zona para ver sus regiones. Marca una regi\u00f3n completa o elige comunas.</p>' +
        '<input type="text" id="al-geo-search" class="al-input" placeholder="Buscar zona, regi\u00f3n o comuna\u2026" value="' + esc(geoSearch) + '">' +
        '<div class="al-geo-tree" id="al-geo-tree">' + (on ? geoTreeHtml() : "") + "</div>" +
      "</div>"
    );
  }

  function quickSourcesHtml() {
    var html = sourceToggleHtml("favorita", "Estaci\u00f3n favorita");
    if (isProTier() && (state.scopes || []).indexOf("grupo") >= 0) {
      html += sourceToggleHtml("grupo", "Mi grupo de estaciones");
    }
    return html;
  }

  function geoMatches(mzName, regName, comName) {
    if (!geoSearch) return true;
    var q = geoSearch.toLowerCase();
    if ((mzName || "").toLowerCase().indexOf(q) >= 0) return true;
    if (regName.toLowerCase().indexOf(q) >= 0) return true;
    return (comName || "").toLowerCase().indexOf(q) >= 0;
  }

  function macrozonaSelectionBadge(mz, selReg, selCom) {
    var nReg = 0;
    var nCom = 0;
    mz.regions.forEach(function (rg) {
      if (selReg[rg.cod]) nReg++;
      rg.comunaList.forEach(function (c) {
        if (selCom[normalizeComuna(c)] && !selReg[rg.cod]) nCom++;
      });
    });
    if (!nReg && !nCom) return "";
    var bits = [];
    if (nReg) bits.push(nReg + " regi\u00f3n" + (nReg === 1 ? "" : "es"));
    if (nCom) bits.push(nCom + " comuna" + (nCom === 1 ? "" : "s"));
    return bits.join(" \u00b7 ");
  }

  function geoRegionHtml(rg, selReg, selCom, forceOpen) {
    var comunasVis = rg.comunaList.filter(function (c) { return geoMatches("", rg.nombre, c); });
    var regNameMatch = !geoSearch || rg.nombre.toLowerCase().indexOf(geoSearch.toLowerCase()) >= 0;
    if (geoSearch && !regNameMatch && !comunasVis.length) return "";
    var comunasToShow = (geoSearch && !regNameMatch) ? comunasVis : rg.comunaList;
    var regChecked = !!selReg[rg.cod];
    var open = !!forceOpen || !!openRegions[rg.cod] || (geoSearch && comunasVis.length > 0);
    var nSelCom = rg.comunaList.filter(function (c) { return selCom[normalizeComuna(c)]; }).length;
    var badge = regChecked ? "Toda la regi\u00f3n" : (nSelCom ? (nSelCom + " comuna" + (nSelCom === 1 ? "" : "s")) : "");
    var comHtml = comunasToShow.map(function (c) {
      var on = regChecked || !!selCom[normalizeComuna(c)];
      var dis = regChecked ? " disabled" : "";
      return '<label class="al-geo-com">' +
        '<input type="checkbox" class="al-geo-com-cb" data-com="' + esc(c) + '"' + (on ? " checked" : "") + dis + ">" +
        "<span>" + esc(c) + "</span></label>";
    }).join("");
    return '<div class="al-geo-reg' + (open ? " open" : "") + '" data-cod="' + rg.cod + '">' +
      '<div class="al-geo-reg-hd">' +
        '<label class="al-geo-reg-check" title="Vigilar toda la regi\u00f3n">' +
          '<input type="checkbox" class="al-geo-reg-cb" data-cod="' + rg.cod + '"' + (regChecked ? " checked" : "") + ">" +
        "</label>" +
        '<button type="button" class="al-geo-reg-toggle" aria-expanded="' + (open ? "true" : "false") + '">' +
          '<span class="al-geo-reg-name">' + esc(rg.nombre) + "</span>" +
          (badge ? '<span class="al-geo-reg-badge">' + esc(badge) + "</span>" : "") +
          '<span class="al-geo-chevron" aria-hidden="true"></span>' +
        "</button>" +
      "</div>" +
      '<div class="al-geo-coms">' + comHtml + "</div>" +
    "</div>";
  }

  function geoTreeHtml() {
    var macrozonas = geoTree();
    if (!macrozonas.length) return '<p class="al-empty">A\u00fan no hay estaciones disponibles para elegir.</p>';
    var selReg = selectedRegions();
    var selCom = selectedComunas();
    var out = "";
    macrozonas.forEach(function (mz) {
      var visibleRegs = [];
      var anyMatch = !geoSearch;
      mz.regions.forEach(function (rg) {
        var comunasVis = rg.comunaList.filter(function (c) { return geoMatches(mz.etiqueta, rg.nombre, c); });
        var regNameMatch = !geoSearch || rg.nombre.toLowerCase().indexOf(geoSearch.toLowerCase()) >= 0;
        var mzNameMatch = !geoSearch || mz.etiqueta.toLowerCase().indexOf(geoSearch.toLowerCase()) >= 0;
        if (geoSearch && !mzNameMatch && !regNameMatch && !comunasVis.length) return;
        anyMatch = true;
        visibleRegs.push({ rg: rg, forceOpen: geoSearch && (regNameMatch || comunasVis.length > 0) });
      });
      if (!visibleRegs.length) return;
      var mzOpen = !!openMacrozones[mz.id] || (geoSearch && anyMatch);
      var mzBadge = macrozonaSelectionBadge(mz, selReg, selCom);
      var regHtml = visibleRegs.map(function (item) {
        return geoRegionHtml(item.rg, selReg, selCom, item.forceOpen);
      }).join("");
      out +=
        '<div class="al-geo-mz' + (mzOpen ? " open" : "") + '" data-mz="' + esc(mz.id) + '">' +
          '<button type="button" class="al-geo-mz-toggle" aria-expanded="' + (mzOpen ? "true" : "false") + '">' +
            '<span class="al-geo-mz-name">' + esc(mz.etiqueta) + "</span>" +
            (mzBadge ? '<span class="al-geo-mz-badge">' + esc(mzBadge) + "</span>" : "") +
            '<span class="al-geo-mz-count">' + esc(visibleRegs.length) + " regi\u00f3n" + (visibleRegs.length === 1 ? "" : "es") + "</span>" +
            '<span class="al-geo-chevron" aria-hidden="true"></span>' +
          "</button>" +
          '<div class="al-geo-mz-body">' + regHtml + "</div>" +
        "</div>";
    });
    if (!out) return '<p class="al-empty">Sin coincidencias para "' + esc(geoSearch) + '".</p>';
    return out;
  }

  function misAlertasBodyHtml() {
    var html = quickSourcesHtml();
    if (!isProTier()) {
      return html + '<p class="al-tier-note">Plan gratis: tu estaci\u00f3n favorita. Con Pro puedes seguir regiones y comunas.</p>';
    }
    html += advancedScopeHtml();
    var otras = (state.reglas || []).filter(function (r) {
      return ["favorita", "grupo", "region", "comuna"].indexOf(r.scope_type) < 0;
    });
    if (otras.length) {
      html += '<div class="al-src-group"><h3 class="al-subh">Otras alertas</h3><div id="al-otras-list">';
      otras.forEach(function (r) { html += ruleCardHtml(r); });
      html += "</div></div>";
    }
    return html;
  }

  function refreshGeoTreeOnly() {
    var t = $("al-geo-tree");
    if (t) t.innerHTML = geoTreeHtml();
  }

  function refreshMisAlertas() {
    var el = $("al-misalertas-body");
    if (!el) return;
    el.innerHTML = misAlertasBodyHtml();
    wireMisAlertas();
  }

  function refreshReglas() {
    return apiFetch("/me/alertas/reglas").then(function (d) {
      state.reglas = normalizeList(d && d.reglas).map(normalizeRegla);
    });
  }

  function createGeoRegla(scope, value, opts) {
    opts = opts || {};
    setStatus("Guardando\u2026", true);
    return apiFetch("/me/alertas/reglas", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ scope_type: scope, scope_value: value, activa: true })
    }).then(refreshReglas).then(function () {
      setStatus("Alerta agregada.", true);
      if (opts.geoTreeOnly) refreshGeoTreeOnly();
      else refreshMisAlertas();
    }).catch(function (err) {
      setStatus(err.message, false);
      if (opts.geoTreeOnly) refreshGeoTreeOnly();
      else refreshMisAlertas();
    });
  }

  function deleteGeoRegla(id, opts) {
    opts = opts || {};
    setStatus("Quitando\u2026", true);
    return apiFetch("/me/alertas/reglas/" + id, { method: "DELETE" })
      .then(refreshReglas).then(function () {
        setStatus("Alerta quitada.", true);
        if (opts.geoTreeOnly) refreshGeoTreeOnly();
        else refreshMisAlertas();
      }).catch(function (err) {
        setStatus(err.message, false);
        if (opts.geoTreeOnly) refreshGeoTreeOnly();
        else refreshMisAlertas();
      });
  }

  var geoTreeOpts = { geoTreeOnly: true };

  function toggleRegionRegla(cod, checked) {
    if (checked) {
      createGeoRegla("region", { cod_reg: cod }, geoTreeOpts);
    } else {
      var id = selectedRegions()[cod];
      if (id) deleteGeoRegla(id, geoTreeOpts); else refreshGeoTreeOnly();
    }
  }

  function toggleComunaRegla(nom, checked) {
    if (checked) {
      createGeoRegla("comuna", { nom_com: nom }, geoTreeOpts);
    } else {
      var id = selectedComunas()[normalizeComuna(nom)];
      if (id) deleteGeoRegla(id, geoTreeOpts); else refreshGeoTreeOnly();
    }
  }

  function wireMisAlertas() {
    document.querySelectorAll("#al-misalertas-body .al-legacy-toggle").forEach(function (cb) {
      cb.addEventListener("change", function () {
        putLegacy(cb.getAttribute("data-scope"), cb.checked, cb);
      });
    });
    document.querySelectorAll("#al-misalertas-body .al-rule-toggle").forEach(function (cb) {
      cb.addEventListener("change", function () {
        var row = cb.closest(".al-rule");
        var id = row && row.getAttribute("data-regla-id");
        var scope = row && row.getAttribute("data-scope-type");
        if (!id) return;
        if (scope === "favorita" || scope === "grupo") {
          putLegacy(scope, cb.checked, cb);
          return;
        }
        putRegla(id, { activa: cb.checked }, cb);
      });
    });
    document.querySelectorAll("#al-misalertas-body .al-rule-del").forEach(function (btn) {
      btn.addEventListener("click", function () {
        var row = btn.closest(".al-rule");
        var id = row && row.getAttribute("data-regla-id");
        if (!id || !confirm("\u00bfEliminar esta alerta?")) return;
        deleteGeoRegla(id);
      });
    });

    var tree = $("al-geo-tree");
    if (tree) {
      tree.addEventListener("change", function (e) {
        var regCb = e.target.closest(".al-geo-reg-cb");
        if (regCb) {
          toggleRegionRegla(parseInt(regCb.getAttribute("data-cod"), 10), regCb.checked);
          return;
        }
        var comCb = e.target.closest(".al-geo-com-cb");
        if (comCb) {
          toggleComunaRegla(comCb.getAttribute("data-com"), comCb.checked);
        }
      });
      tree.addEventListener("click", function (e) {
        var mzTgl = e.target.closest(".al-geo-mz-toggle");
        if (mzTgl) {
          var mz = mzTgl.closest(".al-geo-mz");
          if (!mz) return;
          var mzId = mz.getAttribute("data-mz") || "";
          openMacrozones[mzId] = !openMacrozones[mzId];
          mz.classList.toggle("open", openMacrozones[mzId]);
          mzTgl.setAttribute("aria-expanded", openMacrozones[mzId] ? "true" : "false");
          return;
        }
        var tgl = e.target.closest(".al-geo-reg-toggle");
        if (!tgl) return;
        var reg = tgl.closest(".al-geo-reg");
        if (!reg) return;
        var cod = parseInt(reg.getAttribute("data-cod"), 10);
        openRegions[cod] = !openRegions[cod];
        reg.classList.toggle("open", openRegions[cod]);
        tgl.setAttribute("aria-expanded", openRegions[cod] ? "true" : "false");
      });
    }

    var search = $("al-geo-search");
    if (search) {
      search.addEventListener("input", function () {
        geoSearch = search.value.trim();
        refreshGeoTreeOnly();
        var s2 = $("al-geo-search");
        if (s2) { s2.focus(); s2.setSelectionRange(s2.value.length, s2.value.length); }
      });
    }

    var adv = document.querySelector("#al-misalertas-body .al-advanced-toggle");
    if (adv) {
      adv.addEventListener("change", function () {
        putLegacy("avanzada", adv.checked, adv);
      });
    }
  }

  function loadTabPrefs() {
    if (qs("desde") === "aviso") {
      activeTab = "resumen";
      return;
    }
    try {
      var t = sessionStorage.getItem(TAB_KEY);
      if (t === "resumen" || t === "config") activeTab = t;
    } catch (e) { /* ignore */ }
  }

  function saveTabPrefs() {
    try { sessionStorage.setItem(TAB_KEY, activeTab); } catch (e) { /* ignore */ }
  }

  function switchTab(tab) {
    if (tab !== "resumen" && tab !== "config") return;
    activeTab = tab;
    saveTabPrefs();
    var r = $("al-tab-resumen");
    var c = $("al-tab-config");
    if (r) r.hidden = tab !== "resumen";
    if (c) c.hidden = tab !== "config";
    document.querySelectorAll(".al-tab").forEach(function (btn) {
      var on = btn.getAttribute("data-tab") === tab;
      btn.classList.toggle("on", on);
      btn.setAttribute("aria-selected", on ? "true" : "false");
    });
  }

  function tabsHtml() {
    function tab(id, label) {
      var on = activeTab === id;
      return '<button type="button" class="al-tab' + (on ? " on" : "") +
        '" data-tab="' + id + '" role="tab" aria-selected="' + (on ? "true" : "false") +
        '">' + label + "</button>";
    }
    return '<nav class="al-tabs" role="tablist" aria-label="Secciones de alertas">' +
      tab("resumen", "Resumen") + tab("config", "Configurar alertas") + "</nav>";
  }

  function wireTabs() {
    document.querySelectorAll(".al-tab").forEach(function (btn) {
      btn.addEventListener("click", function () {
        switchTab(btn.getAttribute("data-tab"));
      });
    });
  }

  function glossaryHtml() {
    var items = [
      ["Puesto", "El n\u00famero junto a cada alerta (#1, #4\u2026) dice qu\u00e9 tan extremo fue ese d\u00eda en esa estaci\u00f3n. El #1 es el m\u00e1s extremo."],
      ["Historia de la estaci\u00f3n", "Se compara con todos los d\u00edas que esa estaci\u00f3n tiene registrados. Un valor raro en la costa puede ser normal en un valle."],
      ["Confirmada", "El d\u00eda ya ocurri\u00f3. El valor es real: por ejemplo, la lluvia o la temperatura de ayer."],
      ["Prevista", "El d\u00eda todav\u00eda no ocurre. Si el pron\u00f3stico se cumple, ese d\u00eda estar\u00eda entre los m\u00e1s extremos de la estaci\u00f3n."],
      ["Estaci\u00f3n", "Cada alerta mira la historia de una estaci\u00f3n, no un mismo l\u00edmite para todo el pa\u00eds."],
      ["Muy alto", "El lado alto: m\u00e1s lluvia, m\u00e1s calor, o una m\u00ednima menos fr\u00eda."],
      ["Muy bajo", "El lado bajo: el d\u00eda m\u00e1s fr\u00edo o la m\u00e1xima m\u00e1s baja."],
      ["Datos", "Lluvia, temperatura m\u00e1xima y temperatura m\u00ednima. Cada una se compara por separado."],
      ["Resumen del d\u00eda", "Un solo correo al d\u00eda con las alertas de los lugares que elegiste. Esta p\u00e1gina muestra lo mismo y permite filtrarlo."],
      ["Previstas y confirmadas", "Las previstas son el pron\u00f3stico de hoy y ma\u00f1ana. Las confirmadas son lo medido ayer. Puedes ver unas, las otras o ambas."],
      ["Zona", "Grupo de regiones (Norte, Centro Norte, Centro, Sur y Austral) para leer el mapa y la lista."]
    ];
    var html = '<section class="al-card al-glossary" id="al-glosario">' +
      "<h2>Palabras de esta p\u00e1gina</h2>" +
      '<p class="al-desc">Qu\u00e9 quiere decir cada cosa.</p>' +
      '<dl class="al-glossary-list">';
    items.forEach(function (pair) {
      html += "<div><dt>" + pair[0] + "</dt><dd>" + pair[1] + "</dd></div>";
    });
    return html + "</dl></section>";
  }

  function reboteBannerHtml() {
    var r = state.carta_devuelta || {};
    if (!r.bloqueado) return "";
    return '<div class="al-rebote-banner" role="status">' +
      "<p>Los correos a tu direcci\u00f3n est\u00e1n rebotando (p.ej. buz\u00f3n lleno). Pausamos los avisos por correo; cuando lo soluciones pulsa Reactivar.</p>" +
      '<button type="button" class="al-btn al-btn-primary" id="al-btn-rebote-reactivar">Reactivar</button>' +
      "</div>";
  }

  function reactivarCorreoRebote() {
    var btn = $("al-btn-rebote-reactivar");
    if (btn) btn.disabled = true;
    apiFetch("/me/alertas/correo-rebote/reactivar", { method: "POST" }).then(function (data) {
      state.carta_devuelta = (data && data.carta_devuelta) || { bloqueado: false };
      setStatus("Listo. Volveremos a enviarte los avisos por correo.", true);
      renderPanel();
    }).catch(function (err) {
      if (btn) btn.disabled = false;
      setStatus((err && err.message) || "No se pudo reactivar el correo.", false);
    });
  }

  function renderPanel() {
    loadTabPrefs();
    var panel = $("al-panel");
    if (!panel) return;
    var prefs = state.preferencias;
    var stats = state.stats || {};
    var ultimo = stats.ultimo_digest;
    var ultimoTxt = ultimo && ultimo.fecha_eval
      ? (Number(ultimo.n_eventos) === 1 ? "1 alerta el " : ultimo.n_eventos + " alertas el ") + ultimo.fecha_eval
      : "Sin env\u00edos a\u00fan";

    panel.innerHTML =
      tabsHtml() +
      reboteBannerHtml() +
      '<div id="al-tab-resumen" class="al-tab-panel"' + (activeTab === "resumen" ? "" : " hidden") + ">" +
        '<header class="al-pagehead">' +
          "<h1>Alertas de eventos extremos</h1>" +
          '<p class="al-sub">Estado de hoy e historial de las estaciones que monitoreas.</p>' +
        "</header>" +
        '<div id="al-filtro"></div>' +
        '<p id="al-live" class="al-sr" aria-live="polite"></p>' +
        '<p class="al-sr" id="al-digest-meta"></p>' +
        '<div id="al-digest-wrap"><p class="al-empty">Cargando\u2026</p></div>' +
        '<section class="al-card al-hist" aria-labelledby="al-hist-h">' +
          '<div class="al-hist-hd">' +
            "<div><h2 id=\"al-hist-h\">Historial</h2>" +
            '<p class="al-mut" id="al-hist-lead"></p></div>' +
            '<button type="button" class="al-btn" id="al-historial-csv">Descargar planilla</button>' +
          "</div>" +
          '<div id="al-hist-sum"></div>' +
          '<div id="al-historial-wrap"><p class="al-empty">Cargando\u2026</p></div>' +
        "</section>" +
        glossaryHtml() +
      "</div>" +
      '<div id="al-tab-config" class="al-tab-panel"' + (activeTab === "config" ? "" : " hidden") + ">" +
        '<header class="al-pagehead">' +
          "<h1>Configurar alertas</h1>" +
          '<p class="al-sub">Elige c\u00f3mo y cu\u00e1ndo quieres enterarte de un evento clim\u00e1tico extremo.</p>' +
        "</header>" +
        '<section class="al-card al-plan-card">' +
          '<span class="al-plan-chip">Plan ' + esc(tierLabel(state.tier)) + "</span> " +
          "Correo: <b>" + esc(prefs.email_efectivo || state.email) + "</b> \u00b7 M\u00ednimo un resumen diario consolidado" +
        "</section>" +
        (typeof rodajaPushHtml === "function" ? rodajaPushHtml(prefs) : "") +
        '<section class="al-card al-cfg">' +
          '<h2><svg class="i" viewBox="0 0 24 24"><rect x="3" y="5" width="18" height="14" rx="2"/><path d="M3 7l9 6 9-6"/></svg>Correo</h2>' +
          '<p class="al-desc">A qu\u00e9 direcci\u00f3n llega el resumen diario.</p>' +
          '<div class="al-radios">' +
            '<label class="al-rd"><input type="radio" name="al-email-mod" value="cuenta"' + (prefs.email_modo !== "alternativo" ? " checked" : "") + '><i></i>Mi correo de cuenta \u00b7 ' + esc(state.email || prefs.email_efectivo || "") + "</label>" +
            '<label class="al-rd"><input type="radio" name="al-email-mod" value="alternativo"' + (prefs.email_modo === "alternativo" ? " checked" : "") + "><i></i>Otro correo</label>" +
          "</div>" +
          '<input type="email" id="al-email-alt" class="al-input" placeholder="correo@ejemplo.cl" value="' + esc(prefs.email_alternativo || "") + '"' +
          (prefs.email_modo === "alternativo" ? "" : " hidden") + " />" +
        "</section>" +
        '<section class="al-card al-cfg">' +
          '<h2><svg class="i" viewBox="0 0 24 24"><path d="M6 8a6 6 0 1 1 12 0c0 7 3 8 3 8H3s3-1 3-8"/><path d="M10 20a2 2 0 0 0 4 0"/></svg>Tipos de alerta</h2>' +
          '<p class="al-desc">Puedes recibir una, otra, o ambas.</p>' +
          '<div class="al-opts">' +
            '<p class="al-desc" id="al-tardia-legado">Confirmadas (d\u00eda ya medido): las manda el producto confirmado (lluvia fuerte y rareza). El top-10 cl\u00e1sico est\u00e1 apagado.</p>' +
            '<input type="checkbox" id="al-toggle-tardia" hidden' + (state.alerta_tardia_activa !== false ? " checked" : "") + '>' +
            '<label class="al-opt"><input type="checkbox" id="al-toggle-temprana"' + (state.alerta_temprana_activa ? " checked" : "") + '>' +
              '<span class="al-opt-ic"><svg class="i" viewBox="0 0 24 24"><circle cx="12" cy="12" r="9"/><path d="M12 7v5l3 3"/></svg></span>' +
              "<span><b>Previstas</b><small>Pron\u00f3stico antes de que ocurra el evento.</small></span><span class=\"al-opt-chk\"></span></label>" +
          "</div>" +
        "</section>" +
        '<section class="al-card al-cfg">' +
          '<h2><svg class="i" viewBox="0 0 24 24"><path d="M4 6h10M4 12h6M4 18h13"/><circle cx="17" cy="6" r="2"/><circle cx="13" cy="18" r="2"/></svg>Variables y extremos</h2>' +
          (singleVariableOnly()
            ? '<p class="al-desc">Plan gratis: elige un dato. Pro permite varios o todos.</p>'
            : '<p class="al-desc">Aplica a todo el alcance geogr\u00e1fico que elijas abajo.</p>') +
          '<div class="al-chips" id="al-var-chips">' + variableChipsHtml() + "</div>" +
          '<div class="al-seg2">' +
            '<label><input type="checkbox" id="al-tipo-max"' + (prefs.alerta_max !== false ? " checked" : "") + "><span>M\u00e1ximos diarios</span></label>" +
            '<label><input type="checkbox" id="al-tipo-min"' + (prefs.alerta_min !== false ? " checked" : "") + "><span>M\u00ednimos diarios</span></label>" +
          "</div>" +
        "</section>" +
        '<section class="al-card al-cfg al-geo">' +
          '<h2><svg class="i" viewBox="0 0 24 24"><path d="M12 21s7-6.2 7-11a7 7 0 1 0-14 0c0 4.8 7 11 7 11z"/><circle cx="12" cy="10" r="2.5"/></svg>Alcance geogr\u00e1fico</h2>' +
          '<p class="al-desc">Para la mayor\u00eda de los casos basta con tu favorita o tu grupo de estaciones.</p>' +
          '<div id="al-misalertas-body">' + misAlertasBodyHtml() + "</div>" +
        "</section>" +
        '<div id="al-umbral-root"></div>' +
        '<section class="al-card al-cfg al-preview">' +
          '<h2><svg class="i" viewBox="0 0 24 24"><path d="M2 12s4-7 10-7 10 7 10 7-4 7-10 7-10-7-10-7z"/><circle cx="12" cy="12" r="3"/></svg>Vista previa</h2>' +
          '<p class="al-desc">As\u00ed se ver\u00eda el pr\u00f3ximo correo con tu configuraci\u00f3n actual.</p>' +
          '<div class="al-mail"><div class="al-mail-hd"><span class="al-mail-mk"></span>rodaja.cl \u00b7 resumen diario</div>' +
          '<div id="al-mail-preview"><div class="al-mail-b2">Cargando el resumen\u2026</div></div></div>' +
        "</section>" +
        '<section class="al-card al-stats-card">' +
          "<h2>Actividad</h2>" +
          '<p class="al-desc">Cu\u00e1ntas alertas se han enviado a tu correo.</p>' +
          '<div class="al-stats">' +
            '<div class="al-stat"><div class="al-stat-n">' + esc(stats.total || 0) + '</div><div class="al-stat-l">Total hist\u00f3rico</div></div>' +
            '<div class="al-stat"><div class="al-stat-n">' + esc(stats.este_mes || 0) + '</div><div class="al-stat-l">Este mes</div></div>' +
            '<div class="al-stat"><div class="al-stat-n al-stat-n--sm">' + esc(ultimoTxt) + '</div><div class="al-stat-l">\u00daltimo correo</div></div>' +
          "</div>" +
          '<div id="al-heatmap" class="al-heat-host"></div>' +
        "</section>" +
        '<div class="al-cfg-bar"><div class="al-cfg-bar-wrap">' +
          '<button type="button" class="al-btn al-btn-primary" id="al-save-prefs">Guardar cambios</button>' +
          '<button type="button" class="al-btn al-btn-ghost" id="al-btn-probar"' +
          (state.pruebaEnviadaHoy ? ' disabled title="Ya enviaste una prueba hoy"' : "") +
          ">Probar alerta</button>" +
          '<span class="al-dirty" id="al-dirty"></span>' +
          '<p id="al-status" class="al-status" role="status"></p>' +
        "</div></div>" +
      "</div>";

    hide($("al-gate"));
    show(panel);
    renderHeatmap();
    wireTabs();
    wirePanelEvents();
    mountUmbral();
    if (typeof rodajaResumenBoot === "function") {
      rodajaResumenBoot({ apiFetch: apiFetch, esc: esc, setStatus: setStatus });
    }
    loadDigest();
    applyHandoff();
  }

  function prefsDraft() {
    var modo = document.querySelector('input[name="al-email-mod"]:checked');
    var vids = state.preferencias.variable_ids;
    return JSON.stringify({
      email_modo: modo ? modo.value : "cuenta",
      email_alternativo: (($("al-email-alt") || {}).value || "").trim(),
      variable_ids: vids && vids.length ? vids.slice().sort() : null,
      alerta_max: !!(($("al-tipo-max") || {}).checked),
      alerta_min: !!(($("al-tipo-min") || {}).checked)
    });
  }

  var prefsSnapshot = "";

  function capturePrefsSnapshot() { prefsSnapshot = prefsDraft(); }

  function paintDirty() {
    var el = $("al-dirty");
    if (!el) return;
    var bits = [];
    if (state.pruebaEnviadaHoy) bits.push("Prueba diaria ya enviada hoy");
    var dirty = prefsDraft() !== prefsSnapshot;
    bits.push(dirty ? "<b>cambios sin guardar</b>" : "<b>sin cambios sin guardar</b>");
    el.innerHTML = bits.join(" \u00b7 ");
  }

  function mountUmbral() {
    if (typeof rodajaUmbralMount !== "function") return;
    if (!document.getElementById("al-umbral-root")) return;
    rodajaUmbralMount({
      state: state,
      apiFetch: apiFetch,
      setStatus: setStatus,
      esc: esc,
      isPro: isProTier
    });
  }

  function wirePanelEvents() {
    document.querySelectorAll('input[name="al-email-mod"]').forEach(function (rb) {
      rb.addEventListener("change", function () {
        var alt = $("al-email-alt");
        if (alt) alt.hidden = rb.value !== "alternativo" || !rb.checked;
        paintDirty();
      });
    });
    ["al-email-alt", "al-tipo-max", "al-tipo-min"].forEach(function (id) {
      var el = $(id);
      if (el) el.addEventListener("input", paintDirty);
      if (el) el.addEventListener("change", paintDirty);
    });
    capturePrefsSnapshot();
    paintDirty();

    var saveBtn = $("al-save-prefs");
    if (saveBtn) saveBtn.addEventListener("click", savePrefs);
    if (typeof rodajaPushWire === "function") {
      rodajaPushWire(apiFetch, setStatus, function (prefs) {
        if (prefs) state.preferencias = prefs;
        renderPanel();
      });
    }

    var tTard = $("al-toggle-tardia");
    if (tTard) tTard.addEventListener("change", function () {
      putAlertaTardia(tTard.checked).then(function () {
        setStatus(tTard.checked ? "Alertas confirmadas activadas." : "Alertas confirmadas desactivadas.", true);
      }).catch(function (err) { setStatus(err.message, false); tTard.checked = !tTard.checked; });
    });
    var tTemp = $("al-toggle-temprana");
    if (tTemp) tTemp.addEventListener("change", function () {
      putAlertaTemprana(tTemp.checked).then(function () {
        state.alerta_temprana_activa = tTemp.checked;
        setStatus(tTemp.checked ? "Alertas previstas activadas." : "Alertas previstas desactivadas.", true);
      }).catch(function (err) { setStatus(err.message, false); tTemp.checked = !tTemp.checked; });
    });
    var probarBtn = $("al-btn-probar");
    if (probarBtn) probarBtn.addEventListener("click", probarAlerta);
    var reactivarBtn = $("al-btn-rebote-reactivar");
    if (reactivarBtn) reactivarBtn.addEventListener("click", reactivarCorreoRebote);

    var chips = $("al-var-chips");
    if (chips) {
      chips.addEventListener("click", function (e) {
        var btn = e.target.closest(".al-chip");
        if (!btn) return;
        var vid = btn.getAttribute("data-vid");
        if (singleVariableOnly()) {
          if (!vid) return;
          state.preferencias.variable_ids = [parseInt(vid, 10)];
        } else if (vid === "") {
          state.preferencias.variable_ids = null;
        } else {
          var id = parseInt(vid, 10);
          var cur = state.preferencias.variable_ids || [];
          if (cur.indexOf(id) >= 0) cur = cur.filter(function (x) { return x !== id; });
          else cur = cur.concat([id]);
          state.preferencias.variable_ids = cur.length ? cur : null;
        }
        $("al-var-chips").innerHTML = variableChipsHtml();
        paintDirty();
      });
    }

    wireMisAlertas();
  }

  function probarAlerta() {
    if (state.pruebaEnviadaHoy) {
      setStatus("Ya enviaste una prueba hoy.", false);
      return;
    }
    setStatus("Enviando prueba\u2026", true);
    apiFetch("/me/alertas/probar", { method: "POST" }).then(function (data) {
      state.pruebaEnviadaHoy = true;
      setStatus("Correo de prueba enviado a " + (data.email || state.preferencias.email_efectivo || state.email) + ".", true);
      renderPanel();
    }).catch(function (err) { setStatus(err.message, false); });
  }

  function savePrefs() {
    var modo = document.querySelector('input[name="al-email-mod"]:checked');
    var body = {
      email_modo: modo ? modo.value : "cuenta",
      email_alternativo: ($("al-email-alt") || {}).value || null,
      variable_ids: state.preferencias.variable_ids,
      alerta_max: ($("al-tipo-max") || {}).checked,
      alerta_min: ($("al-tipo-min") || {}).checked
    };
    setStatus("Guardando\u2026", true);
    apiFetch("/me/alertas/preferencias", {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body)
    }).then(function (data) {
      state.preferencias = data.preferencias || state.preferencias;
      capturePrefsSnapshot();
      paintDirty();
      setStatus("Alertas guardadas.", true);
    }).catch(function (err) { setStatus(err.message, false); });
  }

  function putLegacy(scope, activa, cb) {
    setStatus("Guardando\u2026", true);
    if (scope === "avanzada") geoAdvancedOpen = !!activa;
    else if (activa) geoAdvancedOpen = false;
    apiFetch("/me/alertas", {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ scope_type: scope, activa: !!activa, exclusivo: true })
    }).then(function (data) {
      if (data && data.abono) state.abono = normalizeSuscripciones(data.abono);
      if (data && data.reglas) state.reglas = normalizeList(data.reglas).map(normalizeRegla);
      if (scope === "avanzada") {
        setStatus(activa ? "Selecci\u00f3n avanzada activada." : "Selecci\u00f3n avanzada desactivada.", true);
      } else {
        setStatus(activa ? "Alerta activada." : "Alerta desactivada.", true);
      }
      refreshMisAlertas();
    }).catch(function (err) {
      setStatus(err.message, false);
      if (scope === "avanzada") geoAdvancedOpen = !activa;
      else if (activa) geoAdvancedOpen = geoReglas().some(function (r) { return r.activa; });
      if (cb) cb.checked = !activa;
    });
  }

  function putRegla(id, patch, cb) {
    setStatus("Guardando\u2026", true);
    apiFetch("/me/alertas/reglas/" + id, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(patch)
    }).then(function (data) {
      if (data && data.regla) {
        var nr = normalizeRegla(data.regla);
        var rid = String(nr.id);
        var updated = false;
        state.reglas = (state.reglas || []).map(function (r) {
          if (String(apiScalar(r.id)) === rid) { updated = true; return nr; }
          return r;
        });
        if (!updated) state.reglas = (state.reglas || []).concat([nr]);
        syncSuscripcionFromRegla(nr);
      }
      setStatus("Regla actualizada.", true);
      refreshMisAlertas();
    }).catch(function (err) {
      setStatus(err.message, false);
      if (cb) cb.checked = !patch.activa;
    });
  }


  function putAlertaTardia(activa) {
    return apiFetch("/me/alertas/tardia", {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ activa: activa })
    }).then(function (data) {
      state.alerta_tardia_activa = apiBool(data.alerta_tardia);
    });
  }

  function putAlertaTemprana(activa) {
    return apiFetch("/me/alertas/pronostico", {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ activa: activa })
    }).then(function (data) {
      state.alerta_temprana_activa = apiBool(data.activa);
    });
  }
  function loadDigest() {
    if (typeof rodajaDigestLoad === "function") return rodajaDigestLoad(apiFetch);
    return Promise.resolve();
  }
  function loadHistorial() {
    if (typeof rodajaResumenLoad === "function") rodajaResumenLoad();
  }

  function applyHandoff() {
    var scope = qs("scope");
    var eid = qs("e");
    try {
      var snap = JSON.parse(sessionStorage.getItem(SNAPSHOT_KEY) || "null");
      if (snap && snap.scope) scope = snap.scope;
      if (snap && snap.eid) eid = String(snap.eid);
      sessionStorage.removeItem(SNAPSHOT_KEY);
    } catch (e) { /* ignore */ }
    if (scope === "grupo" && !isProTier()) return;
    if ((scope === "favorita" || scope === "grupo")) {
      switchTab("config");
      putLegacy(scope, true);
      return;
    }
    if (eid && isProTier()) {
      switchTab("config");
      var n = parseInt(eid, 10);
      if (isNaN(n)) return;
      var exists = (state.reglas || []).some(function (r) {
        return r.scope_type === "estacion" && parseInt(sv(r, "eid"), 10) === n;
      });
      if (!exists) createGeoRegla("estacion", { eid: n });
    }
  }

  var bootPromise = null;
  var authBooting = false;

  function authReady() {
    if (typeof rodajaInitAuth !== "function") return Promise.resolve(null);
    return new Promise(function (resolve, reject) {
      var settled = false;
      function finish(fn, val) {
        if (settled) return;
        settled = true;
        fn(val);
      }
      var timer = setTimeout(function () {
        finish(reject, new Error("Clerk timeout"));
      }, 10000);
      rodajaInitAuth().then(function (val) {
        clearTimeout(timer);
        finish(resolve, val);
      }).catch(function (err) {
        clearTimeout(timer);
        finish(reject, err);
      });
    });
  }

  function boot() {
    if (bootPromise) return bootPromise;
    authBooting = true;
    if (typeof rodajaInitAuth !== "function") {
      renderGate();
      return Promise.resolve();
    }
    bootPromise = authReady().then(function () {
      var signedIn = rodajaIsSignedIn && rodajaIsSignedIn();
      if (!signedIn) {
        renderGate();
        bootPromise = null;
        return;
      }
      renderLoading("Cargando panel de alertas\u2026");
      return loadCore().then(function () {
        renderPanel();
        return loadCatalog().catch(function () {});
      }).then(function () {
        refreshMisAlertas();
        mountUmbral();
      }).catch(function (err) {
        renderPanel();
        setStatus(err.message || "Error al cargar", false);
      });
    }).catch(function (err) {
      renderGate();
    }).then(function () {
      authBooting = false;
      bootPromise = null;
    });
    return bootPromise;
  }

  global.rodajaOpenAlertasPanel = function (opts) {
    opts = opts || {};
    try {
      sessionStorage.setItem(SNAPSHOT_KEY, JSON.stringify(opts));
    } catch (e) { /* ignore */ }
    var url = "/alertas/";
    if (opts.scope) url += "?scope=" + encodeURIComponent(opts.scope);
    else if (opts.eid) url += "?e=" + encodeURIComponent(opts.eid);
    global.open(url, "_blank");
  };


  if (typeof rodajaOnAuthChange === "function") {
    rodajaOnAuthChange(function (user, token) {
      if (typeof alRenderAuthBar === "function") alRenderAuthBar(user, token);
      if (authBooting) return;
      if (user && token) boot();
      else {
        bootPromise = null;
        renderGate();
      }
    });
  }
  global.rodajaAlertasBoot = boot;
})(window);
