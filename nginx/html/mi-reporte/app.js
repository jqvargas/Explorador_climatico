(function () {
  "use strict";

  var API_BASE = "/api";

  var EC_CALIDAD_NIVELES = {
    excelente: { etiqueta: "Excelente", color: "#1B7F4B" },
    buena: { etiqueta: "Buena", color: "#6E7C8C" },
    regular: { etiqueta: "Regular", color: "#EAB308" },
    limitada: { etiqueta: "Limitada", color: "#DC2626" }
  };
  var EC_CALIDAD_TIP = "Puntaje de calidad del dato de la estaci\u00f3n. Combina: datos al d\u00eda (30 %), d\u00edas con dato (25 %), a\u00f1os sin cortes (20 %), variables medidas (10 %), lecturas descartadas (10 %) y volumen de registros (5 %).";
  var EC_QC_TIP = "Lecturas que el control de calidad descart\u00f3 en todas las variables de la estaci\u00f3n (imposibles f\u00edsicamente o falsas seg\u00fan estaciones vecinas). No se usan en el reporte ni en las alertas. Bajo 1 % no afectan la calidad del dato.";

  function qMark(tip) {
    return '<span class="ec-rp-q" tabindex="0" title="' + esc(tip) + '" aria-label="' + esc(tip) + '">?</span>';
  }

  function qcRangeText(nImp, pImp) {
    var pctPart = (isNaN(pImp) || pImp < 0.1)
      ? "menos de 0,1 % del total"
      : String(pImp.toFixed(1)).replace(".", ",") + " % del total";
    var noun = nImp === 1 ? "valor fuera de rango" : "valores fuera de rango";
    return nImp + " " + noun + " \u00b7 " + pctPart;
  }

  function weatherFileForVtype(vtype) {
    if (vtype === "precip" || vtype === "flow" || vtype === "level") return "lluvia";
    if (vtype === "temp_min" || vtype === "humidity") return "nublado";
    if (vtype === "temp_max" || vtype === "temp_mean") return "soleado";
    return "parcial-soleado";
  }

  /* ponytail: umbrales fijos (0.1 / 2 / 20 mm), no la climatología de la estación. */
  function weatherFileForObs(vtype, valor) {
    if (vtype === "precip") {
      var v = Number(valor);
      if (!isFinite(v) || v < 0.1) return "parcial-soleado";
      if (v < 2) return "llovizna";
      if (v < 20) return "lluvia";
      return "lluvia-fuerte";
    }
    return weatherFileForVtype(vtype);
  }

  function weatherIconHtml(file) {
    return '<img class="ec-rp-wx-icon" src="/icons/weather/' + file + '.svg" alt="" aria-hidden="true" width="22" height="22">';
  }

  function varChipMeta(nombre) {
    var n = String(nombre || "").toLowerCase();
    if (/precip|lluvia|^pp$|^p$/.test(n)) return { icon: "fa-tint", vtype: "precip", weather: "lluvia" };
    if (/temp.*max|tmax|t_max/.test(n)) return { icon: "fa-sun-o", vtype: "temp_max", weather: "soleado" };
    if (/temp.*min|tmin|t_min/.test(n)) return { icon: "fa-moon-o", vtype: "temp_min", weather: "nublado" };
    if (/temp/.test(n)) return { icon: "fa-thermometer-half", vtype: "temp_mean", weather: "soleado" };
    if (/humedad|humidity|hr/.test(n)) return { icon: "fa-tint", vtype: "humidity", weather: "nublado" };
    if (/caudal|flujo|q_|streamflow/.test(n)) return { icon: "fa-tint", vtype: "flow", weather: "lluvia" };
    if (/nivel|level/.test(n)) return { icon: "fa-bar-chart", vtype: "level", weather: "lluvia" };
    return { icon: "fa-line-chart", vtype: "generic", weather: "parcial-soleado" };
  }

  function sparkAttr(v) {
    var s = v && v.spark;
    if (s == null) return "";
    if (!Array.isArray(s)) s = [s];
    var nums = s.map(Number).filter(function (n) { return isFinite(n); });
    if (nums.length < 2) return "";
    return ' data-spark="' + nums.join(",") + '"';
  }

  function weekTrendHtml(cur, prev, vtype) {
    if (prev == null || !isFinite(prev) || cur == null || !isFinite(cur)) return "";
    var d = cur - prev;
    var cls = "ec-rp-week-trend";
    var arrow = "\u2192";
    if (d > 1e-6) { cls += " is-up"; arrow = "\u2191"; }
    else if (d < -1e-6) { cls += " is-down"; arrow = "\u2193"; }
    else cls += " is-flat";
    var txt;
    if (vtype === "precip") {
      txt = (d > 0 ? "+" : "") + Number(d).toLocaleString("es-CL", { maximumFractionDigits: 1 }) + " mm";
    } else {
      if (Math.abs(prev) < 1e-9) return "";
      var pct = (d / Math.abs(prev)) * 100;
      txt = (pct > 0 ? "+" : "") + String(Math.round(pct)) + "%";
    }
    return '<span class="' + cls + '">' + arrow + " " + txt + "</span>";
  }

  function weekBarHtml(cur, weekMax, vtype) {
    if (vtype !== "precip") return "";
    if (cur == null || !isFinite(cur) || !isFinite(weekMax) || weekMax <= 0) return "";
    var pct = Math.max(0, Math.min(100, (cur / weekMax) * 100));
    var tip = Math.round(pct) + " % del d\u00eda m\u00e1s lluvioso de la semana";
    return '<div class="ec-rp-week-bar" title="' + esc(tip) + '" aria-hidden="true"><span style="width:' + pct.toFixed(0) + '%"></span></div>';
  }

  function cardWaveHtml() {
    /* decorative only — not data-bound */
    return '<span class="ec-rp-card-aura" aria-hidden="true"></span>' +
      '<svg class="ec-rp-card-wave" viewBox="0 0 200 40" preserveAspectRatio="none" aria-hidden="true">' +
      '<path fill="currentColor" d="M0 24 Q 40 8 80 22 T 160 18 T 200 24 V 40 H 0 Z"/></svg>';
  }

  function $(id) { return document.getElementById(id); }

  var MR_SEC_ORDER = [
    "mr-sec-resumen",
    "mr-sec-pronostico",
    "mr-sec-semana",
    "mr-sec-acumulados",
    "mr-sec-historicos",
    "mr-sec-tablas"
  ];
  var MR_SEC_LABELS = {
    "mr-sec-resumen": "Estaci\u00f3n",
    "mr-sec-semana": "Semana pasada",
    "mr-sec-pronostico": "Pron\u00f3stico",
    "mr-sec-acumulados": "Acumulados",
    "mr-sec-historicos": "Hist\u00f3ricos",
    "mr-sec-tablas": "Tablas"
  };
  var mrSpyObserver = null;
  var mrTopObserver = null;
  var mrThumbObs = null;
  var mrPillsScrollEl = null;
  var mrSpyLock = false;
  var mrSpyUnlockTimer = null;
  var mrSpyLive = {};
  var mrSpyIds = [];

  function mrPrefersReducedMotion() {
    return window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  }

  function mrIsSectionId(id) {
    if (id === "mr-sec-variables") return true;
    return MR_SEC_ORDER.indexOf(id) >= 0;
  }

  function mrCanonicalSectionId(id) {
    if (id === "mr-sec-variables") return "mr-sec-resumen";
    return id;
  }

  /* last intersecting in document order */
  function mrPickActive(order, liveMap) {
    var live = [];
    var i;
    for (i = 0; i < order.length; i++) {
      if (liveMap[order[i]]) live.push(order[i]);
    }
    if (!live.length) return null;
    return live[live.length - 1];
  }

  function mrTeardownSectionNav() {
    if (mrSpyObserver) { mrSpyObserver.disconnect(); mrSpyObserver = null; }
    if (mrTopObserver) { mrTopObserver.disconnect(); mrTopObserver = null; }
    if (mrThumbObs) { mrThumbObs.disconnect(); mrThumbObs = null; }
    if (mrPillsScrollEl) {
      mrPillsScrollEl.removeEventListener("scroll", mrMoveThumb);
      mrPillsScrollEl = null;
    }
    mrSpyLock = false;
    mrSpyLive = {};
    mrSpyIds = [];
    if (mrSpyUnlockTimer) { clearTimeout(mrSpyUnlockTimer); mrSpyUnlockTimer = null; }
    window.removeEventListener("scrollend", mrUnlockSpy);
  }

  function mrFlushSpy() {
    if (mrSpyLock) return;
    var picked = mrPickActive(mrSpyIds, mrSpyLive);
    if (picked) mrSetActivePill(picked);
  }

  function mrMoveThumb() {
    var nav = $("mr-sec-nav");
    if (!nav) return;
    var group = nav.querySelector(".mr-sec-pills");
    var thumb = nav.querySelector(".mr-sec-thumb");
    var active = nav.querySelector("[data-sec].active");
    if (!group || !thumb || !active) return;
    thumb.style.width = active.offsetWidth + "px";
    thumb.style.transform = "translateX(" + (active.offsetLeft - group.scrollLeft) + "px)";
    nav.classList.add("is-armed");
  }

  function mrSetActivePill(id) {
    var nav = $("mr-sec-nav");
    if (!nav) return;
    nav.querySelectorAll("[data-sec]").forEach(function (p) {
      var on = p.getAttribute("data-sec") === id;
      p.classList.toggle("active", on);
      if (on) p.setAttribute("aria-current", "true");
      else p.removeAttribute("aria-current");
    });
    var active = nav.querySelector("[data-sec].active");
    var group = nav.querySelector(".mr-sec-pills");
    if (active && group) {
      var left = active.offsetLeft;
      var right = left + active.offsetWidth;
      if (left < group.scrollLeft) group.scrollLeft = Math.max(0, left - 12);
      else if (right > group.scrollLeft + group.clientWidth) {
        group.scrollLeft = right - group.clientWidth + 12;
      }
    }
    mrMoveThumb();
  }

  function mrExpandTablas() {
    var sec = $("mr-sec-tablas");
    if (!sec) return;
    var was = sec.classList.contains("is-collapsed");
    sec.classList.remove("is-collapsed");
    var btn = sec.querySelector(".mr-tablas-toggle");
    if (btn) btn.setAttribute("aria-expanded", "true");
    if (was) {
      requestAnimationFrame(function () {
        var mount = $("mr_year_grid");
        if (!mount) return;
        var wrap = mount.querySelector(".ec-rp-year-grid-wrap");
        var th = mount.querySelector("thead th.ec-yg-month-current");
        if (wrap && th) {
          var left = th.offsetLeft - wrap.clientWidth / 2 + th.offsetWidth / 2;
          wrap.scrollLeft = Math.max(0, left);
        }
      });
    }
  }

  function mrExpandSemana() {
    var sec = $("mr-sec-semana");
    if (!sec) return;
    var det = sec.querySelector("details.ec-rp-ultimas-obs-details");
    if (det) det.open = true;
  }

  function mrCollapseTablas() {
    var sec = $("mr-sec-tablas");
    if (!sec) return;
    sec.classList.add("is-collapsed");
    var btn = sec.querySelector(".mr-tablas-toggle");
    if (btn) btn.setAttribute("aria-expanded", "false");
  }

  function mrSetHash(id) {
    if (!mrIsSectionId(id)) return;
    var next = location.pathname + location.search + "#" + id;
    if (location.pathname + location.search + location.hash !== next) {
      history.replaceState({}, "", next);
    }
  }

  function mrUnlockSpy() {
    mrSpyLock = false;
    if (mrSpyUnlockTimer) { clearTimeout(mrSpyUnlockTimer); mrSpyUnlockTimer = null; }
    window.removeEventListener("scrollend", mrUnlockSpy);
    mrFlushSpy();
  }

  function mrScrollToId(id) {
    id = mrCanonicalSectionId(id);
    var el = $(id);
    if (!el || MR_SEC_ORDER.indexOf(id) < 0) return;
    if (id === "mr-sec-tablas") mrExpandTablas();
    if (id === "mr-sec-semana") mrExpandSemana();
    mrSpyLock = true;
    mrSetActivePill(id);
    mrSetHash(id);
    el.scrollIntoView({
      behavior: mrPrefersReducedMotion() ? "auto" : "smooth",
      block: "start"
    });
    window.removeEventListener("scrollend", mrUnlockSpy);
    window.addEventListener("scrollend", mrUnlockSpy, { once: true });
    if (mrSpyUnlockTimer) clearTimeout(mrSpyUnlockTimer);
    mrSpyUnlockTimer = setTimeout(mrUnlockSpy, 800);
  }

  function mrNavHeight() {
    var nav = $("mr-sec-nav");
    if (!nav) return 52;
    return Math.round(nav.getBoundingClientRect().height) || 72;
  }

  function mrInitScrollSpy() {
    if (typeof IntersectionObserver !== "function") return;
    mrSpyIds = MR_SEC_ORDER.filter(function (id) { return $(id); });
    if (!mrSpyIds.length) return;
    var navH = mrNavHeight();
    mrSpyLive = {};
    mrSpyObserver = new IntersectionObserver(function (entries) {
      entries.forEach(function (e) {
        mrSpyLive[e.target.id] = e.isIntersecting;
      });
      mrFlushSpy();
    }, {
      root: null,
      rootMargin: "-" + navH + "px 0px -55% 0px",
      threshold: [0, 0.1, 0.25, 0.5, 1]
    });
    mrSpyIds.forEach(function (id) { mrSpyObserver.observe($(id)); });

    var hero = $("mr-sec-resumen");
    var topBtn = $("mr-back-top");
    if (hero && topBtn) {
      mrTopObserver = new IntersectionObserver(function (entries) {
        var vis = entries[0] && entries[0].isIntersecting;
        topBtn.hidden = !!vis;
      }, { threshold: 0, rootMargin: "-" + navH + "px 0px 0px 0px" });
      mrTopObserver.observe(hero);
    }
  }

  function mrApplyHashOnBoot() {
    var id = mrCanonicalSectionId((location.hash || "").replace(/^#/, ""));
    if (MR_SEC_ORDER.indexOf(id) < 0 || !$(id)) return;
    if (id === "mr-sec-tablas") mrExpandTablas();
    requestAnimationFrame(function () { mrScrollToId(id); });
  }

  function buildVarSwitch(variables) {
    if (!variables || variables.length < 2) return "";
    var active = null;
    var i;
    for (i = 0; i < variables.length; i++) {
      if (variables[i].active) { active = variables[i]; break; }
    }
    if (!active) active = variables[0];
    var activeMeta = varChipMeta(active.nombre);
    var opts = variables.map(function (v) {
      var meta = varChipMeta(v.nombre);
      var on = v.active ? " is-active" : "";
      var aria = v.active ? ' aria-selected="true"' : ' aria-selected="false"';
      return (
        '<button type="button" class="mr-var-opt' + on + '" role="option" data-vid="' + v.id +
        '" data-vtype="' + meta.vtype + '"' + sparkAttr(v) + aria + ">" +
        '<span class="mr-var-opt-icon">' + weatherIconHtml(meta.weather) + "</span>" +
        '<span class="mr-var-opt-label">' + esc(v.nombre) + "</span>" +
        '<span class="ec-rp-spark" aria-hidden="true"></span>' +
        "</button>"
      );
    }).join("");
    return (
      '<details class="mr-var-switch">' +
      '<summary class="mr-var-switch-btn" aria-label="Variable del reporte" title="Cambia la variable de Semana pasada, Acumulados, Hist\u00f3ricos y Tablas">' +
      '<span class="mr-var-switch-kicker">Variable</span>' +
      '<span class="mr-var-switch-icon">' + weatherIconHtml(activeMeta.weather) + "</span>" +
      '<span class="mr-var-switch-label">' + esc(active.nombre) + "</span>" +
      '<span class="mr-var-switch-caret" aria-hidden="true"></span>' +
      "</summary>" +
      '<div class="mr-var-switch-menu" role="listbox">' + opts + "</div>" +
      "</details>"
    );
  }

  function buildSectionNav(opts) {
    opts = opts || {};
    var items = [
      { id: "mr-sec-resumen", show: true },
      { id: "mr-sec-pronostico", show: !!opts.hasPronostico },
      { id: "mr-sec-semana", show: !!opts.hasSemana },
      { id: "mr-sec-acumulados", show: true },
      { id: "mr-sec-historicos", show: true },
      { id: "mr-sec-tablas", show: !!opts.hasTabla }
    ].filter(function (it) { return it.show; });
    var pills = items.map(function (it, i) {
      var cls = i === 0 ? "pill active" : "pill";
      var cur = i === 0 ? ' aria-current="true"' : "";
      var sep = it.id === "mr-sec-acumulados"
        ? '<span class="mr-sec-sep" aria-hidden="true"></span>'
        : "";
      return sep + (
        '<a class="' + cls + '" href="#' + it.id + '" data-sec="' + it.id + '"' + cur + ">" +
        MR_SEC_LABELS[it.id] + "</a>"
      );
    }).join("");
    return (
      '<nav id="mr-sec-nav" class="mr-sec-nav" aria-label="Secciones del reporte">' +
      '<div class="pill-group mr-sec-pills">' +
      '<span class="mr-sec-thumb" aria-hidden="true"></span>' +
      pills + "</div>" +
      buildVarSwitch(opts.variables) +
      "</nav>"
    );
  }

  function bindSectionNav(root) {
    if (!root) return;
    root.querySelectorAll("#mr-sec-nav [data-sec]").forEach(function (a) {
      a.addEventListener("click", function (ev) {
        ev.preventDefault();
        mrScrollToId(a.getAttribute("data-sec"));
      });
    });
    var tog = root.querySelector(".mr-tablas-toggle");
    if (tog) {
      tog.addEventListener("click", function () {
        var sec = $("mr-sec-tablas");
        if (!sec) return;
        if (sec.classList.contains("is-collapsed")) {
          mrScrollToId("mr-sec-tablas");
        } else {
          mrCollapseTablas();
        }
      });
    }
    var topBtn = $("mr-back-top");
    if (topBtn) {
      topBtn.addEventListener("click", function () {
        window.scrollTo({
          top: 0,
          behavior: mrPrefersReducedMotion() ? "auto" : "smooth"
        });
        mrSetActivePill("mr-sec-resumen");
        mrSetHash("mr-sec-resumen");
      });
    }
    mrInitScrollSpy();
    requestAnimationFrame(mrMoveThumb);
    var navEl = $("mr-sec-nav");
    var pillsEl = navEl && navEl.querySelector(".mr-sec-pills");
    if (pillsEl) {
      mrPillsScrollEl = pillsEl;
      pillsEl.addEventListener("scroll", mrMoveThumb, { passive: true });
    }
    if (navEl && typeof ResizeObserver === "function") {
      mrThumbObs = new ResizeObserver(function () { mrMoveThumb(); });
      mrThumbObs.observe(navEl);
    }
    if (document.fonts && document.fonts.ready) {
      document.fonts.ready.then(mrMoveThumb);
    }
  }

  /* ponytail: last-in-order spy; switcher no es destino */
  (function mrNavSelfCheck() {
    var lower = mrPickActive(
      ["mr-sec-semana", "mr-sec-pronostico"],
      { "mr-sec-semana": true, "mr-sec-pronostico": true }
    );
    if (lower !== "mr-sec-pronostico") throw new Error("mrPickActive last-in-order");
    if (mrCanonicalSectionId("mr-sec-variables") !== "mr-sec-resumen") {
      throw new Error("mrCanonicalSectionId variables");
    }
    var html = buildSectionNav({
      variables: [
        { id: 1, nombre: "Precipitacion", active: true },
        { id: 2, nombre: "Temperatura" }
      ],
      hasSemana: true,
      hasPronostico: true,
      hasTabla: true
    });
    if (html.indexOf('class="mr-sec-thumb"') < 0) throw new Error("buildSectionNav thumb");
    if (html.indexOf("mr-var-switch") < 0) throw new Error("buildSectionNav switch");
    if (html.indexOf(">Variable<") < 0 || html.indexOf("Variable del reporte") < 0) {
      throw new Error("buildSectionNav variable");
    }
    if (html.indexOf("mr-sec-sep") < 0) throw new Error("buildSectionNav sep");
    if (html.indexOf('data-sec="mr-sec-variables"') >= 0) throw new Error("buildSectionNav no tab variables");
    if ((html.match(/data-sec=/g) || []).length !== 6) throw new Error("buildSectionNav pills");
    if (html.indexOf('data-sec="mr-sec-pronostico"') > html.indexOf('data-sec="mr-sec-semana"')) {
      throw new Error("buildSectionNav pronostico before semana");
    }
    var semana = buildUltimasObservaciones({
      registros: [{ weekday: "Lun", fecha: "1 sep", valueFmt: "0", valor: 0, isLatest: true }],
      variable: "Pp",
      vtype: "precip"
    });
    if (semana.indexOf("<details") < 0 || /<details[^>]*\sopen[\s>]/.test(semana)) {
      throw new Error("semana pasada collapsed by default");
    }
    if (weatherFileForObs("precip", 0) !== "parcial-soleado") throw new Error("obs icon dry");
    if (weatherFileForObs("precip", 1) !== "llovizna") throw new Error("obs icon drizzle");
    if (weatherFileForObs("precip", 5) !== "lluvia") throw new Error("obs icon rain");
    if (weatherFileForObs("precip", 25) !== "lluvia-fuerte") throw new Error("obs icon heavy");
    if (semana.indexOf("parcial-soleado") < 0) throw new Error("semana dry icon");
    if (weekBarHtml(3, 10, "temp_max") !== "") throw new Error("week bar temp");
    if (weekBarHtml(5, 10, "precip").indexOf("width:50%") < 0) throw new Error("week bar precip");
  })();

  function apiFetch(path) {
    if (typeof rodajaApiFetch === "function") {
      return rodajaApiFetch(path).then(function (r) {
        if (!r.ok) throw new Error("HTTP " + r.status);
        return r.json();
      });
    }
    return fetch(API_BASE + path, { headers: { Accept: "application/json" } })
      .then(function (r) {
        if (!r.ok) throw new Error("HTTP " + r.status);
        return r.json();
      });
  }

  var APP_STATION_KEY = "rodaja_app_reporte_v1";

  function readAppStation() {
    try {
      var store = JSON.parse(localStorage.getItem("rodaja_favorites_v1") || "null");
      if (store && store.primary && store.primary.eid) {
        return {
          eid: store.primary.eid,
          variableId: store.primary.variableId || null
        };
      }
    } catch (e) { /* ignore */ }
    try {
      var o = JSON.parse(localStorage.getItem(APP_STATION_KEY) || "null");
      if (o && o.eid) return { eid: o.eid, variableId: o.variableId || null };
    } catch (e2) { /* ignore */ }
    return null;
  }

  function resolveTarget() {
    var p = rodajaParseMiReporteParams();
    if (p.eid) {
      return {
        eid: p.eid,
        variableId: p.variableId,
        fuenteId: p.fuenteId
      };
    }
    return readAppStation();
  }

  function esc(s) {
    var d = document.createElement("div");
    d.textContent = s == null ? "" : String(s);
    return d.innerHTML;
  }

  function asArray(val) {
    if (!val) return [];
    if (Array.isArray(val)) return val;
    if (typeof val === "object") return Object.keys(val).sort(function (a, b) { return Number(a) - Number(b); }).map(function (k) { return val[k]; });
    return [];
  }

  function sanitizeCiclo(ciclo) {
    if (!ciclo) return ciclo;
    var out = Object.assign({}, ciclo);
    out.meses = asArray(out.meses);
    ["parcialMensual", "acumuladoMensual", "anioPasado", "anioNormal", "mediaNormal", "mediaAnioPasado", "mediaAnioActual"].forEach(function (k) {
      if (out[k] && typeof ecCleanSeries === "function") {
        out[k] = ecCleanSeries(asArray(out[k]));
      } else if (out[k]) {
        out[k] = asArray(out[k]);
      }
    });
    if (out.mesActual && typeof out.mesActual === "object") {
      var ma = Object.assign({}, out.mesActual);
      ma.labels = asArray(ma.labels);
      ["acumuladoDiario", "anioPasado", "anioNormal", "mediaNormal", "mediaAnioPasado", "mediaAnioActual"].forEach(function (k) {
        if (ma[k] && typeof ecCleanSeries === "function") {
          ma[k] = ecCleanSeries(asArray(ma[k]));
        } else if (ma[k]) {
          ma[k] = asArray(ma[k]);
        }
      });
      out.mesActual = ma;
    }
    return out;
  }

  function metaBadge(iconClass, text) {
    if (!text) return "";
    var t = String(text).trim();
    if (!t) return "";
    return (
      '<span class="ec-rp-meta-badge">' +
      '<span class="ec-rp-meta-icon"><i class="fa ' + iconClass + '" aria-hidden="true"></i></span>' +
      '<span class="ec-rp-meta-text">' + esc(t) + "</span></span>"
    );
  }

  function buildHeader(st) {
    var badges = [
      metaBadge("fa-calendar", st.period),
      metaBadge("fa-arrows-v", st.altitude),
      metaBadge("fa-sitemap", st.cuenca),
      metaBadge("fa-map-marker", st.region)
    ].filter(Boolean);
    if (!badges.length && st.metaGeo) {
      badges.push('<span class="ec-rp-meta-badge">' + esc(st.metaGeo) + "</span>");
    }

    var calidadBadge = "";
    if (st.calidadNivel) {
      var niv = EC_CALIDAD_NIVELES[String(st.calidadNivel)] || { etiqueta: String(st.calidadNivel), color: "#8A95A0" };
      var scoreTxt = st.calidadScore != null && !isNaN(Number(st.calidadScore)) ? " " + st.calidadScore + "/100" : "";
      calidadBadge =
        '<span class="ec-rp-calidad-badge" style="--ec-cal-color:' + niv.color + ';">' +
        '<span class="ec-rp-calidad-dot"></span>' + esc(niv.etiqueta + scoreTxt) +
        qMark(EC_CALIDAD_TIP) + "</span>";
    }

    var qcAlert = "";
    var nImp = parseInt(st.nImposibles, 10);
    var pImp = parseFloat(st.pctImposibles);
    if (!isNaN(nImp) && nImp > 0) {
      // ponytail: naranja solo desde 1 %; bajo eso el descarte no mueve el puntaje
      var warn = !isNaN(pImp) && pImp >= 1 ? " is-warn" : "";
      qcAlert =
        '<span class="ec-rp-qc-alert' + warn + '" role="status">' +
        '<span class="ec-rp-qc-alert-body">' + esc(qcRangeText(nImp, pImp)) + "</span>" +
        qMark(EC_QC_TIP) + "</span>";
    }

    var metaHtml = "";
    var metaBits = badges.slice();
    if (qcAlert) metaBits.push(qcAlert);
    if (metaBits.length) {
      metaHtml = '<div class="ec-rp-station-meta">' +
        metaBits.join('<span class="ec-rp-meta-sep" aria-hidden="true">\u00b7</span>') +
        "</div>";
    }

    var aside = "";
    var eid = st.eid != null ? parseInt(st.eid, 10) : NaN;
    var vid = st.vid != null ? parseInt(st.vid, 10) : NaN;
    if (!isNaN(eid) && eid > 0) {
      var dlQs = !isNaN(vid) && vid > 0 ? "?e=" + eid + "&v=" + vid : "?e=" + eid;
      var btns =
        '<a class="ec-rp-download-btn" href="/descargar/index.html' + dlQs + '" target="_blank" rel="noopener">' +
        '<i class="fa fa-download" aria-hidden="true"></i> Descargar</a>' +
        '<button type="button" class="ec-rp-download-btn ec-rp-vista-pdf-btn" ' +
        'onclick="ecExportReportePdf({root: document.getElementById(\'mr-report\'), button: this}); return false;">' +
        '<i class="fa fa-file-pdf-o" aria-hidden="true"></i> Imprimir vista</button>';
      if (st.reportePdf && st.reportePdf.visible) {
        if (st.reportePdf.disponible && st.reportePdf.pdf_url) {
          btns +=
            '<button type="button" class="ec-rp-download-btn ec-rp-pdf-btn" data-pdf-path="' + esc(st.reportePdf.pdf_url) + '" ' +
            'onclick="ecDescargarInformePdf(this.getAttribute(\'data-pdf-path\'), this); return false;">' +
            '<i class="fa fa-file-pdf-o" aria-hidden="true"></i> Informe mensual</button>';
        } else if (st.reportePdf.estado === "pendiente") {
          btns +=
            '<span class="ec-rp-pdf-pending"><i class="fa fa-clock-o" aria-hidden="true"></i> ' +
            esc(st.reportePdf.mensaje || "Informe mensual pendiente") + "</span>";
        } else if (st.reportePdf.estado === "congelado") {
          btns += '<span class="ec-rp-pdf-frozen">' + esc(st.reportePdf.mensaje || "Informe congelado") + "</span>";
        }
      }
      aside = '<div class="ec-rp-header-aside">' + btns + "</div>";
    }

    return (
      '<div class="ec-rp-header" id="mr-sec-resumen">' +
      '<div class="ec-rp-header-deco" aria-hidden="true"></div>' +
      '<div class="ec-rp-header-grid">' +
      '<div class="ec-rp-header-main">' +
      '<div class="ec-rp-station-art">' +
      '<img class="ec-rp-station-art-img" src="img/station-dish.png" alt="" aria-hidden="true" loading="lazy" decoding="async">' +
      '</div>' +
      '<div class="ec-rp-header-text">' +
      '<div class="ec-rp-station-title-row">' +
      '<h2 class="ec-rp-station-name">' + esc(st.name) + "</h2>" +
      (st.fuente ? '<span class="ec-rp-src-tag">' + esc(st.fuente) + "</span>" : "") +
      calidadBadge +
      "</div>" +
      metaHtml +
      "</div>" +
      aside +
      "</div></div></div>"
    );
  }

  function ultimasObsRankHtml(reg) {
    var rank = parseInt(reg.rankHist, 10);
    if (!rank || isNaN(rank) || rank < 1) return "";
    if (rank > 100) return "";
    var cls = "ec-rp-ultima-obs-rank-num" + (rank <= 3 ? " is-top-" + rank : "");
    var inner = (rank <= 3 && typeof ecMedal === "function") ? ecMedal(rank) : ("#" + rank);
    var num = "<span class=\"" + cls + "\">" + inner + "</span>";
    var hint = "";
    if (reg.rankTotal != null && reg.rankTotal > 0) {
      hint = "<span class=\"ec-rp-ultima-obs-rank-hint\">de " + Number(reg.rankTotal).toLocaleString("es-CL") + " d\u00edas</span>";
    }
    var aria = "Puesto " + rank + " de " + (reg.rankTotal || "?") + " d\u00edas hist\u00f3ricos";
    return "<div class=\"ec-rp-ultima-obs-rank-row\" aria-label=\"" + esc(aria) + "\">" + num + hint + "</div>";
  }

  function primeraFechaPronostico(payload, todayIso) {
    if (!payload || typeof ecSanitizePronosticoPayload !== "function") return "";
    var lite = ecSanitizePronosticoPayload(payload.pronosticoLite);
    if (!lite) return "";
    var dias = typeof ecPronosticoDiasDesdeHoy === "function"
      ? ecPronosticoDiasDesdeHoy(lite.dias, todayIso)
      : lite.dias;
    var first = "";
    var i, f;
    for (i = 0; i < dias.length; i++) {
      f = dias[i] && dias[i].fecha ? String(dias[i].fecha).slice(0, 10) : "";
      if (f && (!first || f < first)) first = f;
    }
    return first;
  }

  function slidesSinDato(lastIso, firstIso, variable) {
    if (!lastIso || !firstIso || typeof ecPronosticoAddDaysIso !== "function") return "";
    var huecos = [];
    var d = ecPronosticoAddDaysIso(lastIso, 1);
    while (d && d < firstIso) {
      huecos.push(d);
      if (huecos.length > 3) return "";
      d = ecPronosticoAddDaysIso(d, 1);
    }
    if (!huecos.length) return "";
    return huecos.map(function (iso) {
      var hdr = typeof ecPronosticoFormatDayHeader === "function"
        ? ecPronosticoFormatDayHeader(iso)
        : { weekday: "", dateLine: iso };
      return (
        '<div class="swiper-slide ec-ultimas-obs-slide is-gap" data-fecha="' + esc(iso) + '">' +
        '<div class="ec-ultimas-obs-slide-inner">' +
        cardWaveHtml() +
        '<header class="ec-ultimas-obs-slide-title">' +
        '<div class="ec-ultimas-obs-slide-head">' +
        '<span class="ec-ultimas-obs-weekday">' + esc(hdr.weekday || "") + "</span>" +
        "</div>" +
        '<time class="ec-ultimas-obs-date" datetime="' + esc(iso) + '">' +
        esc(hdr.dateLine || iso) + "</time>" +
        "</header>" +
        '<span class="ec-rp-ultima-obs-variable">' + esc(variable || "") + "</span>" +
        '<div class="ec-rp-ultima-obs-value-row">' +
        '<span class="ec-rp-ultima-obs-value ec-rp-ultima-obs-gap">Sin dato</span>' +
        "</div></div></div>"
      );
    }).join("");
  }

  function buildUltimasObservaciones(block, payload) {
    if (!block) return "";
    var regs = block.registros || [];
    if (!regs.length && block.valueFmt) {
      regs = [Object.assign({}, block, { isLatest: true })];
      block = {
        variable: block.variable,
        unidad: block.unidad,
        vtype: block.vtype,
        accentColor: block.accentColor,
        accentIcon: block.accentIcon,
        registros: regs
      };
    }
    if (!regs.length) return "";
    var todayIso = typeof ecPronosticoTodayIso === "function" ? ecPronosticoTodayIso() : "";
    var vtype = block.vtype || "generic";
    var accent = block.accentColor || "#F4B860";
    var chrono = regs.slice().reverse();
    var weekVals = chrono.map(function (r) { return Number(r.valor); }).filter(function (n) { return isFinite(n); });
    var weekMax = weekVals.length ? Math.max.apply(null, weekVals) : 0;
    var slides = chrono.map(function (reg, i) {
      var cls = "swiper-slide ec-ultimas-obs-slide" + (reg.isLatest ? " is-latest is-hero" : "");
      var fresco = todayIso && reg.fechaIso && typeof ecPronosticoFrescura === "function"
        ? ecPronosticoFrescura(reg.fechaIso, todayIso)
        : null;
      if (fresco) {
        reg = Object.assign({}, reg, { freshLabel: fresco.label, fresh: fresco.fresh, diasAtras: fresco.dias });
      }
      var diasAtras = Number(reg.diasAtras);
      var dayOffset = isFinite(diasAtras) && diasAtras >= 0 ? Math.min(6, Math.floor(diasAtras)) : 0;
      var unit = block.unidad ? '<span class="ec-rp-ultima-obs-unit">' + esc(block.unidad) + "</span>" : "";
      var badgeCls = "ec-pronostico-today-chip ec-timeline-anchor-chip";
      if (reg.freshLabel === "Hoy" || reg.freshLabel === "Ayer") badgeCls += " ec-timeline-anchor-chip--accent";
      var badge = reg.isLatest && reg.freshLabel ? '<span class="' + badgeCls + '">' + esc(reg.freshLabel) + '</span>' : "";
      var prevVal = i > 0 ? Number(chrono[i - 1].valor) : NaN;
      var curVal = Number(reg.valor);
      var wx = weatherFileForObs(vtype, curVal);
      return (
        '<div class="' + cls + '" data-day-offset="' + dayOffset + '">' +
        '<div class="ec-ultimas-obs-slide-inner">' +
        cardWaveHtml() +
        '<header class="ec-ultimas-obs-slide-title">' +
        '<div class="ec-ultimas-obs-slide-head">' +
        '<span class="ec-ultimas-obs-weekday">' + esc(reg.weekday || "") + "</span>" +
        badge +
        "</div>" +
        '<time class="ec-ultimas-obs-date"' +
        (reg.fechaIso ? ' datetime="' + esc(reg.fechaIso) + '"' : "") +
        ">" + esc(reg.fecha || "") + "</time>" +
        "</header>" +
        '<div class="ec-rp-ultima-obs-icon">' + weatherIconHtml(wx) + "</div>" +
        '<span class="ec-rp-ultima-obs-variable">' + esc(block.variable || "") + "</span>" +
        '<div class="ec-rp-ultima-obs-value-row">' +
        '<span class="ec-rp-ultima-obs-value">' + esc(reg.valueFmt) + "</span>" +
        unit +
        weekTrendHtml(curVal, prevVal, vtype) +
        "</div>" +
        weekBarHtml(curVal, weekMax, vtype) +
        ultimasObsRankHtml(reg) +
        "</div></div>"
      );
    }).join("");
    var lastIso = "";
    regs.forEach(function (r) {
      var f = r && r.fechaIso ? String(r.fechaIso).slice(0, 10) : "";
      if (f && (!lastIso || f > lastIso)) lastIso = f;
    });
    slides += slidesSinDato(lastIso, primeraFechaPronostico(payload, todayIso), block.variable);
    return (
      '<div class="ec-rp-ultimas-obs" id="mr-sec-semana" data-vtype="' + esc(vtype) + '" style="--ec-obs-accent:' + accent + ';">' +
      '<div class="ec-rp-ultimas-obs-deco" aria-hidden="true"></div>' +
      '<details class="ec-rp-ultimas-obs-details ec-rp-ultimas-obs-inner">' +
      '<summary class="ec-rp-ultimas-obs-head">' +
      '<span class="widget-title ec-rp-ultimas-obs-kicker">Semana pasada</span>' +
      '<span class="ec-rp-ultimas-obs-toggle">' +
      '<span class="ec-rp-ultimas-obs-hint">Ver registros</span>' +
      '<span class="ec-rp-ultimas-obs-caret" aria-hidden="true"></span>' +
      "</span></summary>" +
      '<div class="ec-rp-ultimas-obs-mount">' +
      '<div class="ec-rp-pronostico-swiper-wrap ec-rp-ultimas-obs-swiper-wrap">' +
      '<div class="swiper ec-ultimas-obs-swiper"><div class="swiper-wrapper">' + slides + "</div></div>" +
      '<button type="button" class="ec-pronostico-nav ec-pronostico-nav-prev" aria-label="Registro anterior">' +
      '<i class="fa fa-chevron-left" aria-hidden="true"></i></button>' +
      '<button type="button" class="ec-pronostico-nav ec-pronostico-nav-next" aria-label="Registro siguiente">' +
      '<i class="fa fa-chevron-right" aria-hidden="true"></i></button>' +
      "</div></div></details></div>"
    );
  }

  function renderReport(payload) {
    var root = $("mr-report");
    var st = payload.station || {};
    var widgets = payload.widgets || [];
    var widgetShells = widgets.map(function (w) {
      return '<div class="widget ec-rw-widget" data-widget-id="' + esc(w.id) + '"></div>';
    }).join("\n");

    var chartId = "mr_ts_chart";
    var cicloId = "mr_ciclo_chart";
    var yearGridId = "mr_year_grid";
    var ciclo = sanitizeCiclo(payload.cicloAnual || {});
    var tabla = payload.tablaAnio || {};
    var hasTabla = !!tabla.hasData;
    var hasMesActual = !!ciclo.hasMesActual;
    var cicloPills =
      '<div class="pill-group ec-ciclo-pills" role="group" aria-label="Per\u00edodo del ciclo">' +
      '<button type="button" class="pill active" data-ciclo-mode="anual" aria-pressed="true" onclick="ecCicloSetMode(this,\'' + cicloId + '\',\'anual\')">Anual</button>' +
      (hasMesActual
        ? '<button type="button" class="pill" data-ciclo-mode="mesActual" aria-pressed="false" onclick="ecCicloSetMode(this,\'' + cicloId + '\',\'mesActual\')">Mes Actual</button>'
        : "") +
      "</div>";

    var pronSectionId = "mr-sec-pronostico";
    var pronMountId = "mr_pronostico_carousel";
    var pronDetailHref = st.eid
      ? "/mi-reporte/?view=pronostico&e=" + encodeURIComponent(String(st.eid)) +
        (st.vid ? "&v=" + encodeURIComponent(String(st.vid)) : "")
      : "";
    var pronosticoShell = typeof ecBuildPronosticoCarouselShell === "function"
      ? ecBuildPronosticoCarouselShell(pronSectionId, pronMountId, pronDetailHref)
      : "";

    var ultimasHtml = buildUltimasObservaciones(payload.ultimasObservaciones || payload.ultimaObservacion, payload);
    var sectionNav = buildSectionNav({
      variables: st.variables || [],
      hasSemana: !!ultimasHtml,
      hasPronostico: !!pronosticoShell,
      hasTabla: hasTabla
    });

    var yearGridSection = hasTabla
      ? '<div class="ec-rp-section ec-rp-year-grid-section" id="mr-sec-tablas">' +
        '<div class="widget ec-rp-year-grid-card">' +
        '<div class="ec-rp-year-grid-mount" id="' + yearGridId + '"></div>' +
        '</div></div>'
      : "";

    mrTeardownSectionNav();
    var histBtn = $("mr-btn-historico");
    if (histBtn) histBtn.hidden = true;
    root.innerHTML =
      '<script class="ec-report-data-json" type="application/json">' +
      JSON.stringify(payload) +
      "</script>" +
      sectionNav +
      buildHeader(st) +
      '<div class="ec-rp-timeline-pair">' +
      pronosticoShell +
      (ultimasHtml
        ? '<div class="ec-rp-timeline-connector" aria-hidden="true"><span class="ec-rp-timeline-connector-line"></span></div>' + ultimasHtml
        : "") +
      '</div>' +
      (payload.insightGlobal
        ? '<div class="ec-rp-insight-strip">' + esc(payload.insightGlobal) + "</div>"
        : "") +
      '<div class="ec-rp-body">' +
      '<div class="ec-rp-section" id="mr-sec-acumulados"><h3 class="ec-rp-section-title" style="text-transform:none;letter-spacing:normal">C\u00f3mo se compara este a\u00f1o con los anteriores?</h3>' +
      '<div class="widget ec-rp-ciclo-card"><div class="widget-header ec-rp-ciclo-header">' +
      '<div class="ec-rp-ciclo-titles">' +
      '<span id="' + cicloId + '_title" class="widget-title">' + esc(ciclo.title || "Ciclo Anual") + "</span>" +
      (ciclo.subtitle ? '<span id="' + cicloId + '_subtitle" class="ec-rp-ciclo-subtitle">' + esc(ciclo.subtitle) + "</span>" : '<span id="' + cicloId + '_subtitle" class="ec-rp-ciclo-subtitle"></span>') +
      "</div>" +
      '<div class="ec-rp-ciclo-controls">' +
      '<span id="' + cicloId + '_normal_period" class="ec-normal-period-mount ec-legend-item"></span>' +
      cicloPills +
      "</div>" +
      "</div>" +
      '<div id="' + cicloId + '_kpis" class="ec-rp-ciclo-kpis"></div>' +
      '<p id="' + cicloId + '_sr" class="ec-rp-ciclo-sr"></p>' +
      '<p id="' + cicloId + '_line_label" class="ec-rp-ciclo-panel-label" hidden>Acumulado</p>' +
      '<div class="ec-rp-ciclo-chart-wrap"><canvas id="' + cicloId + '" aria-label="C\u00f3mo se compara este a\u00f1o con los anteriores?" role="img"></canvas></div>' +
      '<div id="' + cicloId + '_bars_wrap" class="ec-rp-ciclo-bars-block" hidden>' +
      '<p class="ec-rp-ciclo-panel-label">Por mes</p>' +
      '<div class="ec-rp-ciclo-chart-wrap ec-rp-ciclo-bars"><canvas id="' + cicloId + '_bars" aria-label="Precipitaci\u00f3n por mes" role="img"></canvas></div>' +
      "</div>" +
      '<p id="' + cicloId + '_foot" class="ec-rp-ciclo-foot"></p>' +
      "</div></div>" +
      '<div id="mr-sec-historicos" class="ec-rp-historicos-wrap">' +
      '<div class="ec-rp-section"><h3 class="ec-rp-section-title" style="text-transform:none;letter-spacing:normal;font-weight:500;font-size:15px;color:#394553">Registros hist\u00f3ricos</h3>' +
      '<div class="ec-rw-yearbar" data-role="yearbar" hidden></div>' +
      '<div class="ec-rp-widgets-row">' + widgetShells + "</div></div>" +
      '<div class="ec-rp-section"><div class="widget ec-rp-ts-card">' +
      '<div class="widget-header ec-rp-ts-header"><div class="ec-rp-ts-titles">' +
      '<div class="ec-rp-ts-title">' + esc(st.variable ? ('Historial de ' + st.variable) : 'Historial') + '</div>' +
      '<p id="' + chartId + '_ts_subtitle" class="ec-rp-ts-subtitle">Totales anuales</p>' +
      '</div><div class="ec-rp-ts-controls">' +
      '<div class="pill-group ec-ts-pills" role="group" aria-label="Resoluci\u00f3n del historial">' +
      '<button type="button" class="pill active" data-ts-res="anual" onclick="ecTSSetRes(this,\'' + chartId + '\',\'anual\')">Anual</button>' +
      '<button type="button" class="pill" data-ts-res="mensual" onclick="ecTSSetRes(this,\'' + chartId + '\',\'mensual\')">Mensual</button>' +
      '<button type="button" class="pill" data-ts-res="completitud" onclick="ecTSSetRes(this,\'' + chartId + '\',\'completitud\')">% completitud datos</button>' +
      '</div>' +
      '<div id="' + chartId + '_span" class="pill-group ec-ts-span" role="group" aria-label="Per\u00edodo del \u00faltimo a\u00f1o" style="display:none;">' +
      '<button type="button" class="pill active" data-ts-span="completo" onclick="ecTSSetSpan(this,\'' + chartId + '\',\'completo\')">A\u00f1o completo</button>' +
      '<button type="button" class="pill" data-ts-span="hasta_hoy" onclick="ecTSSetSpan(this,\'' + chartId + '\',\'hasta_hoy\')">Hasta hoy</button>' +
      '</div></div></div>' +
      '<p id="' + chartId + '_sr" class="ec-rp-ts-sr"></p>' +
      '<div id="' + chartId + '_kpis" class="ec-rp-ts-kpis"></div>' +
      '<div id="' + chartId + '_ts_legend" class="ec-rp-ts-legend"></div>' +
      '<div class="ec-rp-ts-chart-wrap is-annual">' +
      '<svg id="' + chartId + '_svg" class="ec-rp-ts-svg" role="group" aria-label="' + esc('Historial anual de ' + (st.variable || 'la variable')) + '"></svg>' +
      '<div id="' + chartId + '_tt" class="ec-rp-ts-tip"></div>' +
      '<canvas id="' + chartId + '" aria-label="' + esc('Serie temporal de ' + (st.variable || 'la variable')) + '" role="img"></canvas>' +
      '</div>' +
      '<div class="ec-rp-ts-foot"><div class="ec-rp-ts-ref">' +
      '<span class="ec-rp-ts-ref-label">Per\u00edodo de referencia</span>' +
      '<span id="' + chartId + '_ts_normal_period" class="ec-normal-period-mount ec-rp-ts-period"></span>' +
      '</div><p id="' + chartId + '_ts_note" class="ec-rp-ts-note"></p></div>' +
      '<table id="' + chartId + '_ts_table" class="ec-rp-ts-backup"></table>' +
      '</div></div></div>' +
      yearGridSection +
      "</div>" +
      '<button type="button" id="mr-back-top" class="mr-btn mr-back-top" hidden aria-label="Volver arriba">' +
      '<i class="fa fa-chevron-up" aria-hidden="true"></i></button>';

    root.hidden = false;
    $("mr-loading").hidden = true;

    window.__ecReportChartData = payload.chart;
    window.__ecReportCicloData = ciclo;
    window.__ecReportTablaAnio = hasTabla ? tabla : null;
    window.__ecReportPronosticoLite = payload.pronosticoLite || null;
    window.__ecReportNormalClimatology = payload.normalClimatology || null;

    root.querySelectorAll(".mr-var-opt[data-vid]").forEach(function (btn) {
      btn.addEventListener("click", function () {
        var vid = parseInt(btn.getAttribute("data-vid"), 10);
        if (!vid) return;
        var eid = st.eid || (resolveTarget() && resolveTarget().eid);
        if (!eid) return;
        if (st.vid && parseInt(st.vid, 10) === vid) {
          var d = btn.closest("details");
          if (d) d.open = false;
          return;
        }
        var url = new URL(location.href);
        url.searchParams.set("e", eid);
        url.searchParams.set("v", vid);
        history.replaceState({}, "", url.pathname + url.search);
        loadReport({ eid: eid, variableId: vid });
      });
    });

    bindSectionNav(root);

    setTimeout(function () {
      if (typeof ecBootReportPostRender === "function") {
        ecBootReportPostRender({
          eid: st.eid,
          vid: st.vid,
          pronMountId: pronMountId,
          pronSectionId: pronSectionId,
          pronosticoLite: window.__ecReportPronosticoLite,
          chartId: chartId,
          cicloId: cicloId,
          yearGridId: yearGridId,
          hasChart: !!payload.chart,
          hasCiclo: !!ciclo,
          hasTabla: hasTabla
        });
      } else {
        if (typeof ecInitReport === "function") ecInitReport();
        if (typeof ecInitTSChart === "function" && payload.chart) ecInitTSChart(chartId, payload.chart);
        if (typeof ecInitCicloAnualChart === "function" && ciclo) ecInitCicloAnualChart(cicloId, ciclo);
        if (typeof ecInitYearGrid === "function" && hasTabla) ecInitYearGrid(yearGridId, tabla);
        if (typeof ecLoadPronosticoCarousel === "function" && st.eid) {
          ecLoadPronosticoCarousel(st.eid, pronMountId, pronSectionId);
        }
      }
      mrApplyHashOnBoot();
    }, 50);
  }

  function fmtValor(v) {
    if (v == null || isNaN(v)) return "\u2014";
    return Number(v).toLocaleString("es-CL", { maximumFractionDigits: 1 });
  }

  function renderPronostico(payload) {
    var root = $("mr-report");
    mrTeardownSectionNav();
    var st = payload.station || {};
    var meta = payload.meta || {};
    var dias = Array.isArray(payload.dias) ? payload.dias : [];
    var varLabels = { pp: "Precipitaci\u00f3n", t_max: "Temp. m\u00e1x", t_min: "Temp. m\u00edn", u_max_diario: "Viento m\u00e1x", u2_mean: "Viento medio", u_10_kt: "Viento predominante" };

    var rows = "";
    dias.forEach(function (d) {
      (d.variables || []).forEach(function (v) {
        var rankHtml = "";
        (v.rankings || []).forEach(function (r) {
          if (r.ranking_estimado && r.ranking_estimado <= 10) {
            rankHtml += '<span class="ec-pronostico-rank">Puesto ' + r.ranking_estimado + "</span>";
          }
        });
        rows += "<tr><td>" + esc(d.fecha) + "</td><td>" + esc(varLabels[v.nombre_abr] || v.nombre || v.nombre_abr) +
          "</td><td>" + fmtValor(v.valor) + (v.unidad ? " " + esc(v.unidad) : "") + rankHtml + "</td></tr>";
      });
    });

    var horizonte = meta.horizonte_dias || dias.length || 0;
    var pronSectionId = "mr-sec-pronostico-detalle";
    var pronMountId = "mr_pronostico_carousel_detalle";
    var pronosticoShell = typeof ecBuildPronosticoCarouselShell === "function"
      ? ecBuildPronosticoCarouselShell(pronSectionId, pronMountId, "")
      : "";
    root.innerHTML =
      '<div class="ec-rp-header">' +
      '<h2 class="ec-rp-title">' + esc(st.nombre || "Estaci\u00f3n") + '</h2>' +
      '<p class="ec-rp-subtitle">Pron\u00f3stico de la estaci\u00f3n \u00b7 ' + horizonte + ' d\u00edas</p>' +
      (st.nivel ? '<span class="ec-rp-meta-badge">Calidad: ' + esc(st.nivel) + "</span>" : "") +
      (meta.stale ? '<span class="ec-rp-meta-badge" style="color:#DC2626;">Datos desactualizados</span>' : "") +
      "</div>" +
      pronosticoShell +
      '<div class="ec-rp-body">' +
      '<table class="ec-pronostico-table"><thead><tr><th>Fecha</th><th>Variable</th><th>Valor pronosticado</th></tr></thead><tbody>' +
      rows +
      "</tbody></table>" +
      '<p class="ec-pronostico-attrib">' + esc(meta.atribucion || "Open-Meteo CC BY 4.0") +
      (meta.fetched_at && typeof ecPronosticoFmtActualizado === "function"
        ? " \u00b7 Actualizado " + esc(ecPronosticoFmtActualizado(meta.fetched_at))
        : "") + "</p>" +
      '<div class="ec-pronostico-toggle" id="mr-pronostico-alertas"></div>' +
      "</div>";

    root.hidden = false;
    $("mr-loading").hidden = true;
    if (typeof ecMountPronosticoCarousel === "function" && st.eid) {
      ecMountPronosticoCarousel(st.eid, pronMountId, pronSectionId, {
        pronosticoLite: payload
      });
    }
    initPronosticoAlertasToggle();
  }

  function initPronosticoAlertasToggle() {
    var slot = $("mr-pronostico-alertas");
    if (!slot) return;
    if (typeof rodajaGetToken !== "function" || !rodajaGetToken()) {
      slot.innerHTML = '<p class="ec-pronostico-attrib">Inicia sesi\u00f3n para recibir avisos del pron\u00f3stico.</p>';
      return;
    }
    var path = "/me/alertas/pronostico";
    var fetchFn = typeof rodajaApiFetch === "function"
      ? function () { return rodajaApiFetch(path).then(function (r) { if (!r.ok) throw new Error("HTTP " + r.status); return r.json(); }); }
      : function () { return fetch(API_BASE + path, { headers: { Accept: "application/json", Authorization: "Bearer " + rodajaGetToken() } }).then(function (r) { if (!r.ok) throw new Error("HTTP " + r.status); return r.json(); }); };
    fetchFn().then(function (st) {
      if (!st.disponible) return;
      var on = !!st.activa;
      slot.innerHTML =
        '<label><input type="checkbox" id="mr-pronostico-alertas-cb"' + (on ? " checked" : "") + "> " +
        "Avisar si el pron\u00f3stico est\u00e1 entre los d\u00edas m\u00e1s extremos</label>" +
        (!st.estacion_elegible ? '<p class="ec-pronostico-attrib">Tu estaci\u00f3n favorita necesita datos completos.</p>' : "");
      var cb = $("mr-pronostico-alertas-cb");
      if (cb) {
        cb.addEventListener("change", function () {
          var body = JSON.stringify({ activa: cb.checked });
          var putFn = typeof rodajaApiFetch === "function"
            ? rodajaApiFetch("/me/alertas/pronostico", { method: "PUT", headers: { "Content-Type": "application/json" }, body: body })
            : fetch(API_BASE + "/me/alertas/pronostico", { method: "PUT", headers: { "Content-Type": "application/json", Accept: "application/json", Authorization: "Bearer " + rodajaGetToken() }, body: body });
          Promise.resolve(putFn).then(function (r) {
            if (r.ok) rodajaToast(cb.checked ? "Alertas previstas activadas" : "Alertas previstas desactivadas", true);
            else rodajaToast("No se pudo actualizar la suscripci\u00f3n", false);
          }).catch(function () { rodajaToast("Error al guardar", false); });
        });
      }
    }).catch(function () { /* ignore */ });
  }

  var LAST_REPORT_KEY = "rodaja_reporte_last_v1";

  function saveLastReport(eid, vid, payload) {
    try {
      localStorage.setItem(LAST_REPORT_KEY, JSON.stringify({
        eid: eid,
        variableId: vid,
        payload: payload,
        ts: Date.now()
      }));
    } catch (e) { /* quota */ }
  }

  function readLastReport() {
    try { return JSON.parse(localStorage.getItem(LAST_REPORT_KEY) || "null"); }
    catch (e) { return null; }
  }

  function snapshotFits(target, snap) {
    if (!snap || !snap.payload || !target || !target.eid) return false;
    if (snap.eid !== target.eid) return false;
    if (target.variableId && target.variableId > 0 && snap.variableId && snap.variableId !== target.variableId) return false;
    return true;
  }

  function showOfflineNote(ts) {
    var root = $("mr-report");
    if (!root) return;
    var old = document.getElementById("mr-offline-note");
    if (old) old.remove();
    var el = document.createElement("div");
    el.id = "mr-offline-note";
    el.className = "mr-offline-note";
    var when = "";
    try { when = new Date(ts).toLocaleString("es-CL"); } catch (e2) { /* ignore */ }
    el.textContent = "Sin conexi\u00f3n \u00b7 datos de " + when;
    root.insertBefore(el, root.firstChild);
  }

  function renderOffline(target) {
    var snap = readLastReport();
    if (!snapshotFits(target, snap)) return false;
    renderReport(snap.payload);
    showOfflineNote(snap.ts);
    return true;
  }

  function paintStationBar(name) {
    var el = $("mr-station-bar-name");
    if (el && name) el.textContent = name;
  }

  function finishReport(payload, eid, vid) {
    renderReport(payload);
    var st = payload.station || {};
    paintStationBar(st.name);
    var useEid = eid || st.eid;
    var useVid = vid || st.vid;
    saveLastReport(useEid, useVid, payload);
    try {
      var prev = readAppStation();
      localStorage.setItem(APP_STATION_KEY, JSON.stringify({
        eid: useEid,
        variableId: useVid || null,
        nombre: st.name || (prev && prev.nombre) || ""
      }));
    } catch (e) { /* quota */ }
  }

  function loadPronostico(target) {
    if (!target || !target.eid) {
      showWizard();
      return;
    }
    $("mr-loading").hidden = false;
    $("mr-report").hidden = true;
    $("mr-wizard").hidden = true;
    var path = "/estaciones/" + target.eid + "/pronostico";
    if (typeof rodajaGetToken === "function" && rodajaGetToken()) {
      path = "/me/pronostico";
    }
    apiFetch(path)
      .then(function (payload) {
        if (payload.error) throw new Error(typeof payload.error === "string" ? payload.error : JSON.stringify(payload.error));
        renderPronostico(payload);
      })
      .catch(function (err) {
        $("mr-loading").hidden = true;
        showWizard("No se pudo cargar el pron\u00f3stico: " + err.message);
      });
  }

  function loadReport(target) {
    if (!target || !target.eid) {
      showWizard();
      return;
    }
    $("mr-loading").hidden = false;
    $("mr-report").hidden = true;
    $("mr-wizard").hidden = true;

    function fetchReport(vid) {
      if (!vid || vid < 1) {
        $("mr-loading").hidden = true;
        showWizard("No hay variable seleccionada para esta estaci\u00f3n.");
        return;
      }
      apiFetch("/estaciones/" + target.eid + "/reporte?id_variable=" + vid)
        .then(function (payload) {
          if (payload.error) throw new Error(typeof payload.error === "string" ? payload.error : JSON.stringify(payload.error));
          var st = payload.station || {};
          apiFetch("/estaciones/" + target.eid + "/reporte/status")
            .then(function (pdfSt) {
              var showPdf = !!pdfSt.disponible ||
                pdfSt.estado === "pendiente" ||
                pdfSt.estado === "congelado";
              if (showPdf) {
                st.reportePdf = {
                  visible: true,
                  disponible: !!pdfSt.disponible,
                  estado: pdfSt.estado || "none",
                  pdf_url: pdfSt.pdf_url || null,
                  mensaje: pdfSt.mensaje || ""
                };
              }
              payload.station = st;
              finishReport(payload, target.eid, vid);
            })
            .catch(function () {
              finishReport(payload, target.eid, vid);
            });
        })
        .catch(function (err) {
          if (renderOffline({ eid: target.eid, variableId: vid })) return;
          $("mr-loading").hidden = true;
          showWizard("No se pudo cargar el reporte: " + err.message);
        });
    }

    if (target.variableId && target.variableId > 0) {
      fetchReport(target.variableId);
      return;
    }
    apiFetch("/estaciones/" + target.eid + "/variables")
      .then(function (vars) {
        var list = Array.isArray(vars) ? vars : (vars && vars.data ? vars.data : []);
        var first = list.length ? parseInt(list[0].id, 10) : null;
        fetchReport(first);
      })
      .catch(function () {
        if (renderOffline(target)) return;
        $("mr-loading").hidden = true;
        showWizard("No se pudieron cargar las variables de la estaci\u00f3n.");
      });
  }

  function showWizard(msg) {
    mrTeardownSectionNav();
    $("mr-loading").hidden = true;
    $("mr-report").hidden = true;
    var w = $("mr-wizard");
    if (!w) {
      location.replace("/mi-reporte/?cambiar=1");
      return;
    }
    w.hidden = false;
    w.innerHTML =
      '<div class="mr-wizard-inner">' +
      (msg ? '<p class="mr-wiz-err">' + esc(msg) + "</p>" : "") +
      '<p class="mr-wiz-lead">Puede elegir otra estaci\u00f3n y volver a abrir el reporte.</p>' +
      '<a class="mr-btn mr-btn-primary" href="/mi-reporte/?cambiar=1">Cambiar estaci\u00f3n</a></div>';
  }

  $("mr-btn-historico").addEventListener("click", function () {
    var t = resolveTarget();
    if (!t) { showWizard(); return; }
    var url = new URL(location.href);
    url.searchParams.delete("view");
    history.replaceState({}, "", url.pathname + url.search);
    loadReport(t);
  });

  document.addEventListener("DOMContentLoaded", function () {
    if (document.documentElement.classList.contains("ra-need-station")) return;
    var target = resolveTarget();
    var view = rodajaParseMiReporteParams().view || new URLSearchParams(location.search).get("view");
    if (!target || !target.eid) {
      location.replace("/mi-reporte/?cambiar=1");
      return;
    }
    try {
      var store = JSON.parse(localStorage.getItem("rodaja_favorites_v1") || "null");
      var fav = (store && store.primary && store.primary.eid) ? store.primary : null;
      if (!fav) fav = JSON.parse(localStorage.getItem(APP_STATION_KEY) || "null");
      paintStationBar(fav && (fav.nombre || fav.alias));
    } catch (eName) { /* ignore */ }
    $("mr-btn-historico").hidden = view !== "pronostico";
    if (view === "pronostico") loadPronostico(target);
    else loadReport(target);
  });
})();
