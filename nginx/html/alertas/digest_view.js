(function (global) {
  "use strict";

  var panels = {};
  var VIEW_MODES = { macrozona: 1, variable: 1 };

  function esc(s) {
    var d = document.createElement("div");
    d.textContent = s == null ? "" : String(s);
    return d.innerHTML;
  }

  function $(id) { return document.getElementById(id); }

  function scalar(v, fallback) {
    if (v == null) return fallback == null ? "" : fallback;
    if (Array.isArray(v)) return v.length ? scalar(v[0], fallback) : (fallback == null ? "" : fallback);
    return v;
  }

  function asArray(val) {
    if (val == null) return [];
    if (Array.isArray(val)) return val.filter(function (x) { return x != null; });
    if (typeof val === "object") {
      var keys = Object.keys(val);
      if (!keys.length) return [];
      if (Array.isArray(val[keys[0]])) {
        var n = val[keys[0]].length;
        var rows = [];
        for (var i = 0; i < n; i++) {
          var row = {};
          keys.forEach(function (k) { row[k] = val[k][i]; });
          rows.push(row);
        }
        return rows.filter(function (x) { return x != null; });
      }
      if (keys.every(function (k) { return /^\d+$/.test(k); })) {
        keys.sort(function (a, b) { return Number(a) - Number(b); });
      }
      return keys.map(function (k) { return val[k]; }).filter(function (x) { return x != null; });
    }
    return [];
  }

  function defaultPrefs() {
    return {
      groupMode: "macrozona",
      sevFilter: "",
      tipoFilter: "",
      density: "detail",
      expanded: {}
    };
  }

  function normalizeGroupMode(mode) {
    return VIEW_MODES[mode] ? mode : "macrozona";
  }

  function loadPrefs(panel) {
    try {
      var raw = sessionStorage.getItem(panel.prefsKey);
      if (!raw) return;
      var p = JSON.parse(raw);
      panel.prefs.groupMode = normalizeGroupMode(p.groupMode);
      panel.prefs.sevFilter = "";
      // El filtro Todas/Previstas/Confirmadas vive en la URL, no en esta sesion.
      if (p.density === "detail" || p.density === "compact") panel.prefs.density = p.density;
      // ponytail: no persistir expanded — macrozona arranca colapsada en cada visita
    } catch (e) { /* ignore */ }
  }

  function savePrefs(panel) {
    try {
      var snap = {
        groupMode: panel.prefs.groupMode,
        sevFilter: panel.prefs.sevFilter,
        tipoFilter: panel.prefs.tipoFilter,
        density: panel.prefs.density
      };
      sessionStorage.setItem(panel.prefsKey, JSON.stringify(snap));
    } catch (e) { /* ignore */ }
  }

  function sevKey(ev) {
    return String(scalar(ev && ev.severidad, ""));
  }

  function zonaEtiqueta(ev) {
    var reg = String(scalar(ev.nom_reg, "") || scalar(ev.macrozona, "") || "Sin zona").toUpperCase();
    var com = String(scalar(ev.nom_com, "")).toUpperCase();
    return com ? reg + " \u00b7 " + com : reg;
  }

  function regionEtiqueta(ev) {
    return String(scalar(ev.nom_reg, "") || "Sin regi\u00f3n").toUpperCase();
  }

  function ubicacionEtiqueta(panel, ev) {
    if (panel.prefs.groupMode === "variable") return zonaEtiqueta(ev);
    return regionEtiqueta(ev);
  }

  function rowShowZona(panel) {
    return panel.prefs.groupMode === "variable" || panel.prefs.groupMode === "macrozona";
  }

  function tipoClass(ev) {
    return tipoKey(ev) === "temprana" ? "al-digest-tipo--temprana" : "al-digest-tipo--tardia";
  }

  function tipoKey(ev) {
    var t = String(scalar(ev && ev.tipo, ""));
    if (t === "temprana" || t === "tardia") return t;
    var lbl = String(scalar(ev && ev.tipo_etiqueta, "")).toLowerCase();
    if (lbl.indexOf("prevista") >= 0 || lbl.indexOf("temprana") >= 0 || lbl.indexOf("pron") >= 0) return "temprana";
    return "tardia";
  }

  function parseIsoDate(s) {
    var m = String(s || "").match(/^(\d{4})-(\d{2})-(\d{2})/);
    if (!m) return null;
    return new Date(Date.UTC(Number(m[1]), Number(m[2]) - 1, Number(m[3])));
  }

  function addUtcDays(d, n) {
    var x = new Date(d.getTime());
    x.setUTCDate(x.getUTCDate() + n);
    return x;
  }

  function isoFromDate(d) {
    return d.toISOString().slice(0, 10);
  }

  // Ventana de producto: tempranas lead 0 y 1 (hoy y mañana); tardías solo ayer.
  function digestFechaEnVentana(tipo, fechaIso, anchorIso) {
    var d = parseIsoDate(fechaIso);
    var anchor = parseIsoDate(anchorIso);
    if (!d || !anchor) return false;
    if (tipo === "temprana") {
      var endT = addUtcDays(anchor, 1);
      return d.getTime() >= anchor.getTime() && d.getTime() <= endT.getTime();
    }
    return d.getTime() === addUtcDays(anchor, -1).getTime();
  }

  function normalizeComuna(name) {
    var x = String(name || "").normalize("NFD").replace(/[\u0300-\u036f]/g, "");
    return x.toUpperCase().replace(/[^A-Z0-9 ]/g, "").replace(/\s+/g, " ").trim();
  }

  function comunaGid(ev) {
    var com = normalizeComuna(scalar(ev && ev.nom_com, "") || "Sin comuna");
    return "com:" + (com || "SIN COMUNA");
  }

  function walkDigestEvents(panel, fn) {
    var data = panel.data;
    if (!data) return;
    var groups = asArray(data.grupos);
    if (!groups.length && data.agrupaciones) {
      groups = asArray(data.agrupaciones.zona);
    }
    groups.forEach(function (g) {
      asArray(g && g.eventos).forEach(fn);
    });
  }

  function ensureDateContext(panel) {
    if (panel._dateCtx && panel._dateCtx.dataRef === panel.data) return panel._dateCtx;
    var fe = String(scalar(panel.data && panel.data.fecha_eval, ""));
    var anchor = parseIsoDate(fe);
    if (!anchor) {
      var now = new Date();
      anchor = new Date(Date.UTC(now.getFullYear(), now.getMonth(), now.getDate()));
    }
    panel._dateCtx = {
      dataRef: panel.data,
      anchor: anchor,
      anchorIso: isoFromDate(anchor)
    };
    return panel._dateCtx;
  }

  function eventPassesFilter(panel, ev) {
    if (panel.prefs.tipoFilter && tipoKey(ev) !== panel.prefs.tipoFilter) return false;
    var ctx = ensureDateContext(panel);
    return digestFechaEnVentana(tipoKey(ev), String(scalar(ev.fecha_iso, "")), ctx.anchorIso);
  }

  function filteredResumen(panel) {
    var tardia = 0;
    var temprana = 0;
    walkDigestEvents(panel, function (ev) {
      if (!eventPassesFilter(panel, ev)) return;
      if (tipoKey(ev) === "temprana") temprana++;
      else tardia++;
    });
    return {
      total: tardia + temprana,
      tardia: tardia,
      temprana: temprana
    };
  }

  function conteosComunaFiltrados(panel) {
    var by = {};
    walkDigestEvents(panel, function (ev) {
      if (!eventPassesFilter(panel, ev)) return;
      var gid = comunaGid(ev);
      if (!by[gid]) {
        by[gid] = {
          id: gid,
          etiqueta: String(scalar(ev.nom_com, "") || gid.replace(/^com:/, "")).toUpperCase(),
          tardia: 0,
          temprana: 0
        };
      }
      var row = by[gid];
      if (tipoKey(ev) === "temprana") row.temprana++;
      else row.tardia++;
    });
    return Object.keys(by).sort().map(function (k) { return by[k]; });
  }

  function splitEvents(panel, evs) {
    var criticas = [];
    var otras = [];
    asArray(evs).forEach(function (ev) {
      if (!eventPassesFilter(panel, ev)) return;
      if (sevKey(ev) === "critica") criticas.push(ev);
      else otras.push(ev);
    });
    return { criticas: criticas, otras: otras };
  }

  function countOtrasInTree(panel, g) {
    if (!g) return 0;
    var n = splitEvents(panel, g.eventos).otras.length;
    asArray(g.subgrupos).forEach(function (sg) {
      n += countOtrasInTree(panel, sg);
    });
    return n;
  }

  function collectVariablesFromGroups(groups, out) {
    out = out || {};
    asArray(groups).forEach(function (g) {
      if (!g) return;
      asArray(g.eventos).forEach(function (ev) {
        var v = String(scalar(ev.variable, "") || "Alerta");
        out[v] = true;
      });
      collectVariablesFromGroups(g.subgrupos, out);
    });
    return out;
  }

  function variableSummary(panel) {
    var groups = groupsForMode(panel);
    if (panel.prefs.groupMode === "variable") {
      var keys = [];
      groups.forEach(function (g) {
        if (g && g.etiqueta) keys.push(String(scalar(g.etiqueta, "")));
      });
      keys.sort();
      return { count: keys.length, list: keys.join(", ") || "\u2014" };
    }
    var seen = collectVariablesFromGroups(groups);
    var keys = Object.keys(seen).sort();
    return { count: keys.length, list: keys.join(", ") || "\u2014" };
  }

  function groupCounts(panel, g, parts) {
    var tardia = 0;
    var temprana = 0;
    parts.criticas.concat(parts.otras).forEach(function (ev) {
      if (tipoKey(ev) === "temprana") temprana++;
      else tardia++;
    });
    var bits = [];
    if (tardia) bits.push(tardia + " tard\u00eda" + (tardia === 1 ? "" : "s"));
    if (temprana) bits.push(temprana + " temprana" + (temprana === 1 ? "" : "s"));
    return bits.join(" \u00b7 ") || "0 alertas";
  }

  function confBadge(ev) {
    if (scalar(ev.confianza, "") === "baja") {
      return ' <span class="al-badge-conf">Confianza baja</span>';
    }
    return "";
  }

  function rowDetail(panel, ev, showZona) {
    var tipoCls = tipoClass(ev);
    var url = scalar(ev.reporte_url, "#");
    var zonaLine = showZona
      ? '<div class="al-digest-meta">' + esc(ubicacionEtiqueta(panel, ev)) + "</div>"
      : "";
    return '<article class="al-digest-row">' +
      '<div class="al-digest-row-stripe ' + tipoCls + '"></div>' +
      '<div class="al-digest-row-body">' +
        '<div class="al-digest-row-top">' +
          '<span class="al-digest-tipo-chip ' + tipoCls + '">' + esc(scalar(ev.tipo_etiqueta, "Ya pas\u00f3")) + "</span>" +
          confBadge(ev) +
        "</div>" +
        '<div class="al-digest-var">' + esc(scalar(ev.variable, "")) + "</div>" +
        '<div class="al-digest-valor">' + esc(scalar(ev.valor, "")) + "</div>" +
        '<div class="al-digest-meta">' + esc(scalar(ev.estacion, scalar(ev.estacion_nombre, ""))) +
          " \u00b7 " + esc(scalar(ev.fecha, "")) + "</div>" +
        zonaLine +
        '<div class="al-digest-rank">' + rankLineHtml(ev) + "</div>" +
      "</div>" +
      '<a class="al-btn al-btn-primary al-btn-sm al-digest-ver" href="' + esc(url) + '">Ver reporte</a>' +
      "</article>";
  }

  function rowCompact(panel, ev, showZona) {
    var tipoCls = tipoClass(ev);
    var url = scalar(ev.reporte_url, "#");
    var zona = showZona ? " \u00b7 " + ubicacionEtiqueta(panel, ev) : "";
    return '<article class="al-digest-row al-digest-row--compact">' +
      '<div class="al-digest-row-stripe ' + tipoCls + '"></div>' +
      '<span class="al-digest-tipo-chip ' + tipoCls + '">' + esc(scalar(ev.tipo_etiqueta, "Ya pas\u00f3")) + "</span>" +
      '<span class="al-digest-compact-main"><strong>' + esc(scalar(ev.variable, "")) + "</strong> " +
        esc(scalar(ev.valor, "")) + " \u2014 " + esc(scalar(ev.estacion, scalar(ev.estacion_nombre, ""))) + zona + "</span>" +
      '<a class="al-btn al-btn-primary al-btn-sm al-digest-ver" href="' + esc(url) + '">Ver reporte</a>' +
      "</article>";
  }

  function rowHtml(panel, ev, showZona) {
    return panel.prefs.density === "compact" ? rowCompact(panel, ev, showZona) : rowDetail(panel, ev, showZona);
  }

  function rankLineHtml(ev) {
    if (String(scalar(ev.tipo, "")) === "umbral") {
      return esc(scalar(ev.ranking_contexto, scalar(ev.umbral_frase, "")));
    }
    var rank = esc(scalar(ev.ranking, ""));
    var ctx = String(scalar(ev.ranking_contexto, ""));
    ctx = ctx.replace(/^(ranking|puesto)\s+\d+\s+/i, "")
      .replace(/^(Top|puesto)\s+\d+\s+(del hist\u00f3rico diario de la estaci\u00f3n|entre los d\u00edas m\u00e1s extremos de esta estaci\u00f3n)\s*/i, "")
      .trim();
    return '<span class="al-digest-rank-n">#' + rank + "</span> " + esc(ctx);
  }

  function windowResumen(panel) {
    var saved = panel.prefs.tipoFilter;
    panel.prefs.tipoFilter = "";
    var r = filteredResumen(panel);
    panel.prefs.tipoFilter = saved;
    return r;
  }

  function svgIcon(id) {
    var paths = {
      bell: '<path d="M6 8a6 6 0 1 1 12 0c0 7 3 8 3 8H3s3-1 3-8"/><path d="M10 20a2 2 0 0 0 4 0"/>',
      clock: '<circle cx="12" cy="12" r="9"/><path d="M12 7v5l3 3"/>',
      chk: '<path d="M20 6L9 17l-5-5"/>',
      info: '<circle cx="12" cy="12" r="9"/><path d="M12 11v6M12 7h.01"/>',
      rain: '<path d="M7 15a5 5 0 0 1 1-9.9A6 6 0 0 1 19 8a4 4 0 0 1-1 8H7z"/><path d="M8 19l-1 2M12 19l-1 2M16 19l-1 2"/>',
      cold: '<path d="M12 2v20M5 6l14 12M19 6L5 18M2 12h20"/>',
      heat: '<circle cx="12" cy="12" r="5"/><path d="M12 1v3M12 20v3M4.2 4.2l2.1 2.1M17.7 17.7l2.1 2.1M1 12h3M20 12h3M4.2 19.8l2.1-2.1M17.7 6.3l2.1-2.1"/>',
      wind: '<path d="M3 8h11a3 3 0 1 0-3-3M3 12h15a3 3 0 1 1-3 3M3 16h8"/>'
    };
    return '<svg class="i" viewBox="0 0 24 24" aria-hidden="true">' + (paths[id] || paths.bell) + "</svg>";
  }

  function kpiHtml(resumen) {
    var r = resumen || {};
    return '<div class="al-strip">' +
      '<div class="al-kpi al-kpi--tot"><div class="al-kpi-n"><span class="al-kpi-ic">' + svgIcon("bell") + "</span>" +
        esc(scalar(r.total, 0)) + '</div><small>Alertas activas hoy</small></div>' +
      '<div class="al-kpi al-kpi--prev"><div class="al-kpi-n"><span class="al-kpi-ic">' + svgIcon("clock") + "</span>" +
        esc(scalar(r.temprana, 0)) + '</div><small>Previstas \u00b7 pron\u00f3stico de hoy y ma\u00f1ana</small></div>' +
      '<div class="al-kpi al-kpi--conf"><div class="al-kpi-n"><span class="al-kpi-ic">' + svgIcon("chk") + "</span>" +
        esc(scalar(r.tardia, 0)) + '</div><small>Confirmadas \u00b7 observadas ayer</small></div>' +
      "</div>";
  }

  function partialBannerHtml(panel) {
    if (!(panel.data && panel.data.correo_parcial)) return "";
    return '<div class="al-digest-partial-banner">Parte de estas alertas a\u00fan no se han enviado por correo. El pr\u00f3ximo correo las incluir\u00e1 todas.</div>';
  }

  function mapShellHtml() {
    return (global.rodajaDigestMap && global.rodajaDigestMap.shellHtml)
      ? global.rodajaDigestMap.shellHtml()
      : "";
  }

  function bodyHtml(panel) {
    return '<div class="al-digest-body">' +
      mapShellHtml() +
      '<div class="al-digest-body-main">' +
        toolbarHtml(panel) +
        '<div class="al-digest-list">' + accordionHtml(panel) + "</div>" +
      "</div>" +
      "</div>";
  }

  function comunaDomId(gid) {
    return "al-digest-" + String(gid || "").replace(/[^A-Za-z0-9_-]/g, "-");
  }

  /** Ancestor gids (macrozona + region) for a comuna leaf under agrupaciones.macrozona. */
  function findComunaAncestors(panel, comGid) {
    var tree = asArray((panel.data && panel.data.agrupaciones && panel.data.agrupaciones.macrozona) || []);
    var found = null;
    tree.forEach(function (mz) {
      if (found || !mz) return;
      asArray(mz.subgrupos).forEach(function (reg) {
        if (found || !reg) return;
        asArray(reg.subgrupos).forEach(function (com) {
          if (found || !com) return;
          if (String(com.id) === String(comGid)) {
            found = { mz: String(mz.id || ""), reg: String(reg.id || ""), com: String(com.id || "") };
          }
        });
      });
    });
    return found;
  }

  function filterSig(panel) {
    return String(panel.prefs.sevFilter || "") + "|" +
      String(panel.prefs.tipoFilter || "");
  }

  function mountOrPaintMap(panel) {
    if (!global.rodajaDigestMap) return;
    var host = $("al-digest-map");
    if (!host) return;
    // Conteos ya filtrados (fecha + sev + tipo) — paint sin re-filtrar sev/tipo
    var conteos = conteosComunaFiltrados(panel);
    var cobertura = (panel.data && panel.data.comunas_cobertura) || [];
    global.rodajaDigestMap.paint(
      host,
      conteos,
      "",
      "",
      function (gid) { jumpToComuna(panel, gid); },
      cobertura
    );
    panel._mapFilterSig = filterSig(panel);
  }

  function jumpToComuna(panel, comGid) {
    // Ensure macrozona grouping so comuna sections exist
    if (panel.prefs.groupMode !== "macrozona") {
      panel.prefs.groupMode = "macrozona";
      panel.prefs.expanded = {};
    }
    var anc = findComunaAncestors(panel, comGid);
    if (anc) {
      if (anc.mz) panel.prefs.expanded[anc.mz] = true;
      if (anc.reg) panel.prefs.expanded[anc.reg] = true;
      if (anc.com) panel.prefs.expanded[anc.com] = true;
    } else {
      panel.prefs.expanded[comGid] = true;
    }
    savePrefs(panel);
    renderList(panel);
    var el = document.getElementById(comunaDomId(comGid)) ||
      document.querySelector('[data-gid="' + comGid.replace(/"/g, "") + '"]');
    if (el && el.scrollIntoView) {
      el.scrollIntoView({ behavior: "smooth", block: "start" });
      el.classList.add("al-digest-acc--flash");
      setTimeout(function () { el.classList.remove("al-digest-acc--flash"); }, 1200);
    }
  }

  function abrOf(ev) {
    return String(scalar(ev.nombre_abr, "")).toLowerCase();
  }

  function dirOf(ev) {
    return String(scalar(ev.direccion, "max")) === "min" ? "min" : "max";
  }

  function phenomenon(ev) {
    var abr = abrOf(ev);
    var dir = dirOf(ev);
    if (abr.indexOf("u") === 0) return "wind";
    if (abr === "pp") return "rain";
    if (abr === "t_min") return dir === "min" ? "cold" : "heat";
    if (abr === "t_max") return dir === "min" ? "cold" : "heat";
    return "rain";
  }

  function titlePhrase(ev) {
    var abr = abrOf(ev);
    var dir = dirOf(ev);
    if (abr === "t_max" && dir === "min") return "Fr\u00edo poco habitual";
    if (abr === "t_max") return "Calor poco habitual";
    if (abr === "t_min" && dir === "min") return "Fr\u00edo poco habitual";
    if (abr === "t_min") return "Calor poco habitual";
    if (abr === "pp" && dir === "min") return "Poca lluvia";
    if (abr === "pp") return "Lluvia poco habitual";
    var nom = String(scalar(ev.variable_nombre, "")).replace(/_/g, " ").trim();
    return nom || "Valor poco habitual";
  }

  function titleEs(s) {
    var small = { en: 1, de: 1, del: 1, la: 1, las: 1, los: 1, el: 1, y: 1, a: 1, e: 1 };
    var parts = String(s || "").split(/\s+/).filter(Boolean);
    return parts.map(function (w, i) {
      var low = w.toLocaleLowerCase("es");
      if (i > 0 && small[low]) return low;
      return low.charAt(0).toLocaleUpperCase("es") + low.slice(1);
    }).join(" ");
  }

  function stationLabel(ev) {
    var s = String(scalar(ev.estacion, scalar(ev.estacion_nombre, "")));
    s = s.replace(/_(DMC|DGA|INIA|CEAZA|SINCA|AGROMET)$/i, "").replace(/_/g, " ").trim();
    return titleEs(s) || "Estaci\u00f3n";
  }

  function niceWord(x) {
    return titleEs(x);
  }

  function placeLabel(ev) {
    var com = niceWord(scalar(ev.nom_com, ""));
    var reg = niceWord(scalar(ev.nom_reg, ""));
    if (com && reg) return com + ", " + reg;
    return com || reg;
  }

  function shortDate(ev) {
    var iso = String(scalar(ev.fecha_iso, ""));
    var m = iso.match(/^(\d{4})-(\d{2})-(\d{2})/);
    if (!m) return String(scalar(ev.fecha, ""));
    var months = ["ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"];
    return Number(m[3]) + " " + months[Number(m[2]) - 1] + " " + m[1];
  }

  function scaleLeft(ev) {
    var rank = Number(scalar(ev.ranking, 5));
    if (!isFinite(rank) || rank < 1) rank = 5;
    if (rank > 10) rank = 10;
    var t = (rank - 1) / 9;
    var span = 108;
    return Math.round(dirOf(ev) === "min" ? t * span : (1 - t) * span);
  }

  function rankPhrase(ev) {
    var rank = Number(scalar(ev.ranking, 0));
    var abr = abrOf(ev);
    var dir = dirOf(ev);
    if (!rank) return "";
    if (rank === 1 && abr === "pp" && dir === "max") return "la mayor lluvia registrada en esta estaci\u00f3n";
    if (rank === 1 && abr === "t_min" && dir === "min") return "el d\u00eda m\u00e1s fr\u00edo desde que hay registro";
    if (rank === 1 && abr === "t_max" && dir === "max") return "el d\u00eda m\u00e1s c\u00e1lido desde que hay registro";
    if (rank === 1) return "el m\u00e1s extremo desde que hay registro";
    if (abr === "pp" && dir === "max") return rank + "\u00b0 d\u00eda m\u00e1s lluvioso en esta estaci\u00f3n";
    if ((abr === "t_min" && dir === "min") || (abr === "t_max" && dir === "min")) {
      return "entre los " + rank + " d\u00edas m\u00e1s fr\u00edos desde que hay registro";
    }
    if (abr === "t_min" || abr === "t_max") return rank + "\u00b0 d\u00eda m\u00e1s c\u00e1lido en esta estaci\u00f3n";
    return "puesto " + rank + " entre los d\u00edas m\u00e1s extremos de esta estaci\u00f3n";
  }

  function flatEvents(panel) {
    var out = [];
    walkDigestEvents(panel, function (ev) {
      if (eventPassesFilter(panel, ev)) out.push(ev);
    });
    out.sort(function (a, b) {
      var ra = Number(scalar(a.ranking, 99));
      var rb = Number(scalar(b.ranking, 99));
      if (ra !== rb) return ra - rb;
      return stationLabel(a).localeCompare(stationLabel(b));
    });
    return out;
  }

  function cardHtml(ev) {
    var kind = phenomenon(ev);
    var prevista = tipoKey(ev) === "temprana";
    var badge = prevista ? "Prevista" : "Confirmada";
    var tip = prevista
      ? "Todav\u00eda no ocurre: es un pron\u00f3stico, no un valor observado."
      : "Ya ocurri\u00f3: es un valor observado, no un pron\u00f3stico.";
    var url = scalar(ev.reporte_url, "#");
    var place = placeLabel(ev);
    return '<article class="al-card-ev">' +
      '<span class="al-ic2 al-ic2--' + kind + '">' + svgIcon(kind) + "</span>" +
      "<div>" +
        '<div class="al-ttl"><b>' + esc(titlePhrase(ev) + " en " + stationLabel(ev)) + "</b>" +
          '<span class="al-badge al-badge--' + (prevista ? "prev" : "conf") + '" title="' + esc(tip) + '">' +
          svgIcon(prevista ? "clock" : "chk") + badge + "</span></div>" +
        (place ? '<div class="al-loc">' + esc(place) + "</div>" : "") +
        '<div class="al-rec"><span class="al-scale al-scale--' + kind + '"><i style="left:' + scaleLeft(ev) + 'px"></i></span>' +
          esc(String(scalar(ev.valor, "")) + " \u2014 " + rankPhrase(ev)) + "</div>" +
      "</div>" +
      '<div class="al-when">' + esc(shortDate(ev)) +
        '<a class="al-act" href="' + esc(url) + '">Ver estaci\u00f3n \u2192</a></div>' +
      "</article>";
  }

  function emptyPrevistasHtml() {
    return '<div class="al-emptycard">' + svgIcon("clock") +
      '<p class="al-emptycard-t">Sin alertas previstas para hoy y ma\u00f1ana</p>' +
      '<p class="al-mut">Te avisaremos por correo si el pron\u00f3stico anticipa un evento extremo.</p></div>';
  }

  function listHtml(panel) {
    var evs = flatEvents(panel);
    if (!evs.length && panel.prefs.tipoFilter === "temprana") return emptyPrevistasHtml();
    if (!evs.length) return '<p class="al-empty">Ninguna alerta coincide con este filtro.</p>';
    return '<div class="al-list">' + evs.map(cardHtml).join("") + "</div>";
  }

  function filterHtml(panel) {
    function seg(val, label, on) {
      return '<button type="button" class="al-seg-btn' + (on ? " on" : "") +
        '" data-digest-kind="tipo" data-digest-val="' + esc(val) + '">' + label + "</button>";
    }
    var tipo = panel.prefs.tipoFilter;
    return '<div class="al-bar">' +
      '<div class="al-seg" role="group" aria-label="Tipo de alerta">' +
        seg("", "Todas", !tipo) +
        seg("temprana", "Previstas", tipo === "temprana") +
        seg("tardia", "Confirmadas", tipo === "tardia") +
      "</div>" +
      '<span class="al-help">' + svgIcon("info") + "Previstas = pron\u00f3stico pr\u00f3ximo \u00b7 Confirmadas = ya ocurri\u00f3</span>" +
      "</div>";
  }

  function toolbarHtml(panel) {
    return filterHtml(panel);
  }

  function groupsForMode(panel) {
    var data = panel.data;
    if (!data) return [];
    if (data.agrupaciones && data.agrupaciones[panel.prefs.groupMode]) {
      return asArray(data.agrupaciones[panel.prefs.groupMode]);
    }
    return asArray(data.grupos);
  }

  function subGroupHasContent(panel, sg) {
    if (!sg) return false;
    var parts = splitEvents(panel, sg.eventos);
    if (parts.criticas.length || parts.otras.length) return true;
    return asArray(sg.subgrupos).some(function (child) {
      return subGroupHasContent(panel, child);
    });
  }

  function groupHasExpandable(panel, g, parts, hasSubs) {
    if (parts.criticas.length || parts.otras.length) return true;
    if (hasSubs) {
      return asArray(g.subgrupos).some(function (sg) { return subGroupHasContent(panel, sg); });
    }
    return false;
  }

  function accordionBodyRows(panel, parts, showZona) {
    var html = "";
    parts.otras.forEach(function (ev) { html += rowHtml(panel, ev, showZona); });
    return html;
  }

  function accordionCriticasHtml(panel, parts, showZona) {
    if (!parts.criticas.length) return "";
    var html = '<div class="al-digest-acc-criticas">';
    parts.criticas.forEach(function (ev) { html += rowHtml(panel, ev, showZona); });
    html += "</div>";
    return html;
  }

  function subAccordionBodyHtml(panel, g, showZona) {
    var nested = asArray(g.subgrupos);
    if (nested.length) {
      var html = '<div class="al-digest-acc-sub">';
      nested.forEach(function (sg) {
        html += subAccordionSectionHtml(panel, sg, showZona);
      });
      html += "</div>";
      return html;
    }
    var parts = splitEvents(panel, g.eventos);
    return accordionCriticasHtml(panel, parts, showZona) +
      accordionBodyRows(panel, parts, showZona);
  }

  function subAccordionSectionHtml(panel, sg, showZona) {
    if (!sg || !subGroupHasContent(panel, sg)) return "";

    var parts = splitEvents(panel, sg.eventos);
    var nested = asArray(sg.subgrupos);
    var sgid = String(sg.id || sg.etiqueta || "");
    var sopen = !!panel.prefs.expanded[sgid];
    var scounts = groupCounts(panel, sg, parts);
    var depthCls = nested.length > 0 ? " al-digest-acc--region" : " al-digest-acc--comuna";
    var canExpand = groupHasExpandable(panel, sg, parts, nested.length > 0);

    var html = '<section class="al-digest-acc al-digest-acc--sub' + depthCls +
      (sopen ? " al-digest-acc--open" : "") + '" data-gid="' + esc(sgid) + '"' +
      (nested.length === 0 ? ' id="' + esc(comunaDomId(sgid)) + '"' : "") + ">";
    html += '<button type="button" class="al-digest-acc-hd al-digest-acc-hd--sub" aria-expanded="' +
      (sopen ? "true" : "false") + '"' + (canExpand ? "" : ' disabled') + ">";
    html += '<span class="al-digest-acc-title">' + esc(scalar(sg.etiqueta, "")) + "</span>";
    html += '<span class="al-digest-acc-counts">' + esc(scounts) + "</span>";
    if (canExpand) html += '<span class="al-digest-acc-chevron" aria-hidden="true"></span>';
    html += "</button>";
    if (canExpand) {
      html += '<div class="al-digest-acc-more">' + subAccordionBodyHtml(panel, sg, showZona) + "</div>";
    }
    html += "</section>";
    return html;
  }

  function accordionGroupHtml(panel, g) {
    var parts = splitEvents(panel, g.eventos);
    if (!parts.criticas.length && !parts.otras.length && !asArray(g.subgrupos).length) return "";

    var subgrupos = asArray(g.subgrupos);
    var hasSubs = subgrupos.length > 0 && panel.prefs.groupMode === "macrozona";
    var showZona = rowShowZona(panel);
    var gid = String(g.id || g.etiqueta || "");
    var open = !!panel.prefs.expanded[gid];
    var canExpand = groupHasExpandable(panel, g, parts, hasSubs);
    var counts = groupCounts(panel, g, parts);
    var cls = "al-digest-acc" + (open ? " al-digest-acc--open" : "");

    var html = '<section class="' + cls + '" data-gid="' + esc(gid) + '">';
    html += '<button type="button" class="al-digest-acc-hd" aria-expanded="' + (open ? "true" : "false") + '"' +
      (canExpand ? "" : ' disabled') + ">";
    html += '<span class="al-digest-acc-title">' + esc(scalar(g.etiqueta, "")) + "</span>";
    html += '<span class="al-digest-acc-counts">' + esc(counts) + "</span>";
    if (canExpand) html += '<span class="al-digest-acc-chevron" aria-hidden="true"></span>';
    html += "</button>";

    if (canExpand) {
      html += '<div class="al-digest-acc-more">';
      if (hasSubs) {
        html += '<div class="al-digest-acc-sub">';
        subgrupos.forEach(function (sg) {
          html += subAccordionSectionHtml(panel, sg, showZona);
        });
        html += "</div>";
      } else {
        html += accordionCriticasHtml(panel, parts, showZona);
        html += accordionBodyRows(panel, parts, showZona);
      }
      html += "</div>";
    }

    html += "</section>";
    return html;
  }

  function accordionHtml(panel) {
    var groups = groupsForMode(panel);
    var html = "";
    var any = false;
    groups.forEach(function (g) {
      if (!g) return;
      var block = accordionGroupHtml(panel, g);
      if (!block) return;
      any = true;
      html += block;
    });
    if (!any) {
      html += '<p class="al-empty">Ninguna alerta coincide con los filtros seleccionados.</p>';
    }
    return html;
  }

  function isVacioFlag(v) {
    if (v === true || v === 1 || v === "true") return true;
    if (Array.isArray(v)) return v[0] === true || v[0] === 1 || v[0] === "true";
    return false;
  }

  function eventCount(data) {
    if (!data) return 0;
    var r = data.resumen || {};
    var rt = Number(scalar(r.total, 0));
    if (rt > 0) return rt;
    var t = Number(scalar(data.total, 0));
    if (t > 0) return t;
    var ne = Number(scalar(data.n_eventos, 0));
    if (ne > 0) return ne;
    return asArray(data.eventos).length;
  }

  function hasContent(data) {
    return !!data && !isVacioFlag(data.vacio) && !data.error && eventCount(data) > 0;
  }

  function paint(panel) {
    var wrap = $(panel.wrapId);
    if (!wrap) return;
    panel._dateCtx = null;
    wrap.innerHTML = kpiHtml(windowResumen(panel)) +
      '<section class="al-sec" aria-labelledby="al-activas-h">' +
      '<h2 id="al-activas-h">Alertas activas</h2>' +
      partialBannerHtml(panel) +
      listHtml(panel) +
      "</section>";
    wireControls(panel, wrap);
  }

  function render(panel) {
    var wrap = $(panel.wrapId);
    if (!wrap) return;
    if (!hasContent(panel.data)) {
      wrap.innerHTML = '<p class="al-empty">' + esc(panel.emptyMsg) + "</p>";
      return;
    }
    try {
      paint(panel);
    } catch (e) {
      console.error("digest_view render", panel.wrapId, e);
      wrap.innerHTML = '<p class="al-empty">' + esc(panel.errorMsg || "No se pudo mostrar las alertas.") + "</p>";
    }
  }

  function wireControls(panel, root) {
    root.querySelectorAll(".al-seg-btn").forEach(function (btn) {
      btn.addEventListener("click", function () {
        panel.prefs.tipoFilter = btn.getAttribute("data-digest-val") || "";
        savePrefs(panel);
        render(panel);
      });
    });
  }

  function wireAccordionOnly(panel, listRoot) {
    if (!listRoot) return;
    listRoot.querySelectorAll(".al-digest-acc-hd:not([disabled])").forEach(function (btn) {
      btn.addEventListener("click", function () {
        var sec = btn.closest(".al-digest-acc");
        if (!sec) return;
        var gid = sec.getAttribute("data-gid") || "";
        var open = !sec.classList.contains("al-digest-acc--open");
        if (open) panel.prefs.expanded[gid] = true;
        else delete panel.prefs.expanded[gid];
        savePrefs(panel);
        var wrap = $(panel.wrapId);
        var list = wrap && wrap.querySelector(".al-digest-list");
        if (list) {
          list.innerHTML = accordionHtml(panel);
          wireAccordionOnly(panel, list);
        } else {
          render(panel);
        }
      });
    });
  }

  // Kept for jumpToComuna path
  function renderList(panel) {
    render(panel);
  }

  function panelFor(opts) {
    var id = opts.wrapId;
    if (!panels[id]) {
      panels[id] = {
        wrapId: id,
        prefsKey: opts.prefsKey,
        emptyMsg: opts.emptyMsg,
        sectionId: opts.sectionId || null,
        metaId: opts.metaId || null,
        metaText: opts.metaText || null,
        hideWhenEmpty: !!opts.hideWhenEmpty,
        errorMsg: opts.errorMsg || "No se pudieron cargar las alertas.",
        data: null,
        prefs: defaultPrefs()
      };
    }
    return panels[id];
  }

  function paintMailPreview(panel) {
    var box = document.getElementById("al-mail-preview");
    if (!box || !panel || panel.wrapId !== "al-digest-wrap") return;
    var first = null;
    var nConf = 0;
    var nPrev = 0;
    walkDigestEvents(panel, function (ev) {
      if (!first) first = ev;
      if (tipoKey(ev) === "temprana") nPrev++;
      else nConf++;
    });
    if (!first) {
      box.innerHTML = '<div class="al-mail-b2">Todav\u00eda no hay una alerta para mostrar. El pr\u00f3ximo correo usar\u00e1 esta configuraci\u00f3n.</div>';
      return;
    }
    var bits = [];
    if (nConf) bits.push(nConf + " confirmada" + (nConf === 1 ? "" : "s"));
    if (nPrev) bits.push(nPrev + " prevista" + (nPrev === 1 ? "" : "s"));
    var where = " en tu alcance";
    box.innerHTML = '<div class="al-mail-t2">' + esc(bits.join(" y ") + where) + "</div>" +
      '<div class="al-mail-b2">' + esc(titlePhrase(first) + " en " + stationLabel(first) + " \u2014 " +
        String(scalar(first.valor, "")) + ", " + rankPhrase(first)) + ".</div>";
  }

  function loadPanel(apiFetch, opts) {
    var panel = panelFor(opts);
    panel.emptyMsg = opts.emptyMsg || panel.emptyMsg;
    if (opts.errorMsg) panel.errorMsg = opts.errorMsg;
    loadPrefs(panel);
    if (typeof global.rodajaResumenTipo === "function") {
      panel.prefs.tipoFilter = global.rodajaResumenTipo();
    }
    return apiFetch(opts.url).then(function (resp) {
      panel.data = resp || { vacio: true };
      panel.prefs.expanded = {};
      panel._dateCtx = null;
      if (panel.sectionId) {
        var sec = $(panel.sectionId);
        if (sec) {
          if (panel.hideWhenEmpty && !hasContent(panel.data)) sec.hidden = true;
          else sec.hidden = false;
        }
      }
      if (panel.metaId && panel.metaText) {
        try {
          var meta = $(panel.metaId);
          if (meta) meta.textContent = panel.metaText(panel.data);
        } catch (eMeta) {
          console.error("digest_view meta", panel.wrapId, eMeta);
        }
      }
      try {
        render(panel);
      } catch (e) {
        console.error("digest_view loadPanel", panel.wrapId, e);
        var wrap = $(panel.wrapId);
        if (wrap) wrap.innerHTML = '<p class="al-empty">' + esc(panel.errorMsg) + "</p>";
      }
      paintMailPreview(panel);
      if (typeof global.rodajaResumenAnunciar === "function") global.rodajaResumenAnunciar();
    }).catch(function (err) {
      console.error("digest_view fetch", panel.wrapId, err);
      var wrap = $(panel.wrapId);
      if (wrap) wrap.innerHTML = '<p class="al-empty">' + esc(panel.errorMsg) + "</p>";
      if (panel.sectionId) {
        var sec = $(panel.sectionId);
        if (sec) {
          if (panel.hideWhenEmpty) sec.hidden = true;
          else sec.hidden = false;
        }
      }
    });
  }

  global.rodajaDigestSetTipo = function (tipo) {
    var panel = panels["al-digest-wrap"];
    if (!panel) return 0;
    panel.prefs.tipoFilter = tipo || "";
    render(panel);
    return flatEvents(panel).length;
  };

  global.rodajaDigestCount = function () {
    var panel = panels["al-digest-wrap"];
    if (!panel || !panel.data) return 0;
    return flatEvents(panel).length;
  };

  global.rodajaDigestLoad = function (apiFetch) {
    return loadPanel(apiFetch, {
      url: "/me/alertas/digest",
      prefsKey: "rodaja_digest_prefs",
      wrapId: "al-digest-wrap",
      sectionId: null,
      metaId: "al-digest-meta",
      hideWhenEmpty: false,
      errorMsg: "No se pudo mostrar el resumen diario.",
      emptyMsg: "A\u00fan no hay alertas calculadas para hoy.",
      metaText: function (d) {
        if (d.vacio) return "";
        var total = scalar((d.resumen || {}).total, scalar(d.total, 0));
        if (d.es_hoy) {
          var parts = [];
          if (Number(scalar(d.n_enviados_hoy, 0)) > 0) {
            parts.push(scalar(d.n_enviados_hoy, 0) + " ya enviadas hoy");
          }
          if (Number(scalar(d.n_pendientes, 0)) > 0) {
            parts.push(scalar(d.n_pendientes, 0) + " pendientes de correo");
          }
          var extra = parts.length ? " (" + parts.join(" \u00b7 ") + ")" : "";
          return "Resumen de hoy \u2014 " + total + " alertas" + extra;
        }
        return "\u00daltimo correo \u2014 " + (d.fecha_eval || "") +
          (d.enviado_en ? " (" + d.enviado_en + ")" : "") + " \u2014 " + total + " alertas";
      }
    });
  };

  global.rodajaDigestFechaEnVentana = digestFechaEnVentana;
  if (typeof process !== "undefined" && process.env && process.env.RODAJA_DIGEST_SELFCHECK === "1") {
    var anchor = "2026-09-25";
    function assertVentana(cond, msg) { if (!cond) throw new Error(msg); }
    assertVentana(digestFechaEnVentana("temprana", "2026-09-25", anchor), "lead 0");
    assertVentana(digestFechaEnVentana("temprana", "2026-09-26", anchor), "lead 1");
    assertVentana(!digestFechaEnVentana("temprana", "2026-09-27", anchor), "lead 2");
    assertVentana(digestFechaEnVentana("tardia", "2026-09-24", anchor), "ayer");
    assertVentana(!digestFechaEnVentana("tardia", "2026-09-25", anchor), "hoy no es tardia");
    assertVentana(!digestFechaEnVentana("tardia", "2026-09-23", anchor), "anteayer");
    console.log("digest ventana ok");
  }
})(typeof window !== "undefined" ? window : global);
