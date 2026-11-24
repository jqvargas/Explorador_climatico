// reporte_pronostico.js ? Carrusel Swiper de pron?stico (mi-reporte + Shiny)
"use strict";

var _ecPronosticoSwiper = null;
var _ecPronosticoPrefetch = null;
var _ecPronosticoPrefetchEid = null;
var _ecPronosticoHorasState = null;
var EC_PRONOSTICO_SKELETON_DAYS = 16;

var EC_PRONOSTICO_VAR_META = {
  pp: { label: "Precipitaci\u00f3n", icon: "fa-tint", color: "#F4B860" },
  t_max: { label: "Temperatura m\u00e1xima", icon: "fa-sun-o", color: "#E2734B" },
  t_min: { label: "Temp. m\u00edn", icon: "fa-moon-o", color: "#C4821E" },
  u2_mean: { label: "Viento medio", icon: "fa-long-arrow-right", color: "#6E7C8C" },
  u_max_diario: { label: "Viento m\u00e1x", icon: "fa-flag", color: "#394553" },
  u_10_kt: { label: "Viento predominante", icon: "fa-flag", color: "#475569" }
};

/** Slots fijos del carrusel. t_max/t_min van juntos en el rango. u_max_diario no cae a u_10_kt. */
var EC_PRONOSTICO_DISPLAY_SLOTS = [
  { abr: "pp", featured: false },
  { abr: "u2_mean", featured: false },
  { abr: "u_max_diario", featured: false }
];

function ecDeferHeavy(fn) {
  if (typeof requestIdleCallback === "function") {
    requestIdleCallback(function () { fn(); });
  } else {
    setTimeout(fn, 0);
  }
}

function ecPronosticoSkeletonHtml() {
  var slides = "";
  for (var i = 0; i < EC_PRONOSTICO_SKELETON_DAYS; i++) {
    slides +=
      '<div class="swiper-slide ec-pronostico-slide ec-pronostico-slide--skeleton">' +
      '<div class="ec-pronostico-slide-inner ec-pronostico-skeleton-inner">' +
      '<div class="ec-pronostico-skeleton-block ec-pronostico-skeleton-icon"></div>' +
      '<div class="ec-pronostico-skeleton-block ec-pronostico-skeleton-line"></div>' +
      '<div class="ec-pronostico-skeleton-block ec-pronostico-skeleton-line ec-pronostico-skeleton-line--short"></div>' +
      '<div class="ec-pronostico-skeleton-block ec-pronostico-skeleton-metric"></div>' +
      "</div></div>";
  }
  return (
    '<div class="ec-rp-pronostico-swiper-wrap ec-rp-pronostico-swiper-wrap--loading">' +
    '<div class="swiper ec-pronostico-swiper ec-pronostico-swiper--skeleton">' +
    '<div class="swiper-wrapper">' + slides + "</div></div></div>"
  );
}

function ecStartPronosticoPrefetch(eid) {
  eid = parseInt(eid, 10);
  if (!eid || eid < 1) {
    _ecPronosticoPrefetch = null;
    _ecPronosticoPrefetchEid = null;
    return null;
  }
  if (_ecPronosticoPrefetchEid === eid && _ecPronosticoPrefetch) {
    return _ecPronosticoPrefetch;
  }
  _ecPronosticoPrefetchEid = eid;
  _ecPronosticoPrefetch = ecPronosticoApiFetch("/estaciones/" + eid + "/pronostico")
    .then(function (payload) {
      if (payload.error) throw new Error(payload.error);
      return payload;
    })
    .catch(function (err) {
      _ecPronosticoPrefetch = null;
      _ecPronosticoPrefetchEid = null;
      throw err;
    });
  return _ecPronosticoPrefetch;
}

function ecPatchPronosticoRankings(mountId, fullPayload) {
  var sanitized = ecSanitizePronosticoPayload(fullPayload);
  var mount = document.getElementById(mountId);
  if (!sanitized || !mount) return;
  (sanitized.dias || []).forEach(function (d, dayIdx) {
    var slide = mount.querySelector('.ec-pronostico-slide[data-day-offset="' + dayIdx + '"]');
    if (!slide) return;
    (d.variables || []).forEach(function (v) {
      var rankHtml = ecPronosticoRankHtml(v.rankings);
      if (!rankHtml) return;
      var abr = String(v.nombre_abr || "").toLowerCase();
      var metric = slide.querySelector(".ec-pronostico-metric--" + abr);
      if (!metric) return;
      var valEl = metric.querySelector(".ec-pronostico-metric-value");
      if (!valEl || valEl.querySelector(".ec-pronostico-rank")) return;
      valEl.insertAdjacentHTML("beforeend", rankHtml);
    });
  });
}

function ecMountPronosticoCarousel(eid, mountId, sectionId, opts) {
  opts = opts || {};
  eid = parseInt(eid, 10);
  var section = sectionId ? document.getElementById(sectionId) : null;
  var mount = document.getElementById(mountId);
  if (!eid || eid < 1 || !mount) {
    if (section) section.hidden = true;
    return Promise.resolve();
  }
  if (section) {
    section.hidden = false;
    section.classList.add("ec-rp-pronostico-loading");
  }

  var lite = ecSanitizePronosticoPayload(opts.pronosticoLite);
  if (lite) {
    if (section) section.classList.remove("ec-rp-pronostico-loading");
    return ecInitPronosticoCarousel(mountId, lite, sectionId).then(function () {
      var rankPref = (_ecPronosticoPrefetchEid === eid && _ecPronosticoPrefetch)
        ? _ecPronosticoPrefetch
        : ecStartPronosticoPrefetch(eid);
      return Promise.resolve(rankPref).then(function (full) {
        ecPatchPronosticoRankings(mountId, full);
      }).catch(function () { /* rankings opcionales */ }).finally(function () {
        _ecPronosticoPrefetch = null;
        _ecPronosticoPrefetchEid = null;
      });
    });
  }

  if (!mount.innerHTML) {
    mount.innerHTML = ecPronosticoSkeletonHtml();
  }
  var pref = (_ecPronosticoPrefetchEid === eid && _ecPronosticoPrefetch)
    ? _ecPronosticoPrefetch
    : ecStartPronosticoPrefetch(eid);
  return Promise.resolve(pref).then(function (payload) {
    _ecPronosticoPrefetch = null;
    _ecPronosticoPrefetchEid = null;
    if (section) section.classList.remove("ec-rp-pronostico-loading");
    return ecInitPronosticoCarousel(mountId, payload, sectionId);
  }).catch(function () {
    if (section) {
      section.classList.remove("ec-rp-pronostico-loading");
      section.hidden = true;
    }
  });
}

/** Post-render boot compartido (mi-reporte + Shiny). */
function ecBootReportPostRender(opts) {
  opts = opts || {};
  var mountPromise = null;
  if (opts.eid && typeof ecMountPronosticoCarousel === "function") {
    try {
      mountPromise = ecMountPronosticoCarousel(opts.eid, opts.pronMountId, opts.pronSectionId, {
        pronosticoLite: opts.pronosticoLite
      });
    } catch (e) { /* ponytail: no bloquear graficos si falla el carrusel */ }
  }
  try {
    if (typeof ecInitReport === "function") ecInitReport();
  } catch (e) { /* ignore */ }
  if (typeof ecHydrateSparks === "function") {
    try { ecHydrateSparks(); } catch (e2) { /* ignore */ }
  }
  if (typeof ecNormalInitReport === "function") {
    try {
      ecNormalInitReport({
        eid: opts.eid,
        vid: opts.vid,
        chartId: opts.chartId,
        cicloId: opts.cicloId
      });
    } catch (e) { /* ponytail: no bloquear graficos si falla el selector normal */ }
  }
  if (typeof ecInitTSChart === "function" && opts.hasChart) {
    try { ecInitTSChart(opts.chartId, window.__ecReportChartData); } catch (e) { /* ignore */ }
  }
  if (typeof ecInitCicloAnualChart === "function" && opts.hasCiclo) {
    try { ecInitCicloAnualChart(opts.cicloId, window.__ecReportCicloData); } catch (e) { /* ignore */ }
  }
  if (opts.hasTabla && typeof ecInitYearGrid === "function") {
    try { ecInitYearGrid(opts.yearGridId, window.__ecReportTablaAnio); } catch (e) { /* ignore */ }
  }
  if (typeof window.ecAppProfileMark === "function") {
    try {
      window.ecAppProfileMark("browser.reporte_usable", {
        eid: opts.eid || 0,
        vid: opts.vid || 0
      });
    } catch (e) { /* ignore */ }
  }
  return mountPromise;
}

var EC_CAROUSEL_NARROW_MQ = "(max-width: 600px)";
var _ecPronosticoAxisMq = null;

function ecCarouselIsNarrow() {
  return window.matchMedia(EC_CAROUSEL_NARROW_MQ).matches;
}

function ecDestroyPronosticoSwiperOnly() {
  if (_ecPronosticoSwiper) {
    try { _ecPronosticoSwiper.destroy(true, true); } catch (e) { /* ignore */ }
    _ecPronosticoSwiper = null;
  }
}

var _ecPronosticoSwiperEl = null;

function ecPronosticoSwiperParams(prevEl, nextEl) {
  return {
    direction: "horizontal",
    slidesPerView: 2.15,
    spaceBetween: 10,
    freeMode: { enabled: true, sticky: true },
    watchOverflow: true,
    grabCursor: true,
    navigation: prevEl && nextEl ? { prevEl: prevEl, nextEl: nextEl } : undefined,
    a11y: {
      enabled: true,
      prevSlideMessage: "D\u00eda anterior",
      nextSlideMessage: "D\u00eda siguiente"
    },
    breakpoints: {
      480: { slidesPerView: 3, spaceBetween: 12 },
      768: { slidesPerView: 4, spaceBetween: 12 },
      1024: { slidesPerView: 5, spaceBetween: 14, freeMode: false }
    },
    on: {
      afterInit: function () { ecPronosticoSelectDia(this.activeIndex || 0); },
      slideChange: function () { ecPronosticoSelectDia(this.activeIndex || 0); }
    }
  };
}

function ecPronosticoMarkStacked(swiperEl, stacked) {
  if (!swiperEl) return;
  swiperEl.classList.toggle("is-stacked", !!stacked);
  var mount = swiperEl.closest(".ec-rp-pronostico-mount");
  if (mount) mount.classList.toggle("is-stacked", !!stacked);
}

function ecPronosticoClearStackedHoras(swiperEl) {
  if (!swiperEl) return;
  var nodes = swiperEl.querySelectorAll(".ec-pronostico-hora-host--in-slide");
  var i;
  for (i = 0; i < nodes.length; i++) {
    if (nodes[i].parentNode) nodes[i].parentNode.removeChild(nodes[i]);
  }
}

function ecPronosticoSlideHorasHtml(dia) {
  return ecPronosticoHoraHostHtml({ inSlide: true, dia: dia });
}

var EC_PRONOSTICO_STACK_VISIBLE = 5;

function ecPronosticoClearStackFold(swiperEl) {
  if (!swiperEl) return;
  swiperEl.classList.remove("is-expanded");
  var btn = swiperEl.querySelector(".ec-pronostico-more");
  if (btn && btn.parentNode) btn.parentNode.removeChild(btn);
  var later = swiperEl.querySelectorAll(".ec-pronostico-slide.is-later");
  var i;
  for (i = 0; i < later.length; i++) later[i].classList.remove("is-later");
}

function ecPronosticoApplyStackFold(swiperEl) {
  ecPronosticoClearStackFold(swiperEl);
  var slides = swiperEl.querySelectorAll(".ec-pronostico-slide");
  if (slides.length <= EC_PRONOSTICO_STACK_VISIBLE) return;
  var extra = slides.length - EC_PRONOSTICO_STACK_VISIBLE;
  var i;
  for (i = EC_PRONOSTICO_STACK_VISIBLE; i < slides.length; i++) {
    slides[i].classList.add("is-later");
  }
  var btn = document.createElement("button");
  btn.type = "button";
  btn.className = "ec-pronostico-more";
  function syncLabel() {
    var open = swiperEl.classList.contains("is-expanded");
    btn.textContent = open ? "Mostrar solo 5 d\u00edas" : "Ver los " + extra + " d\u00edas siguientes";
    btn.setAttribute("aria-expanded", open ? "true" : "false");
  }
  syncLabel();
  btn.addEventListener("click", function () {
    swiperEl.classList.toggle("is-expanded");
    syncLabel();
  });
  var wrap = swiperEl.querySelector(".swiper-wrapper");
  if (wrap) wrap.appendChild(btn);
}

function ecPronosticoMountStackedHoras(swiperEl) {
  ecPronosticoClearStackedHoras(swiperEl);
  var st = _ecPronosticoHorasState;
  if (!st) return;
  var dias = (st.payload && st.payload.dias) || [];
  var slides = swiperEl.querySelectorAll(".ec-pronostico-slide");
  var i;
  for (i = 0; i < slides.length; i++) {
    var dia = dias[i];
    if (!dia || !ecPronosticoHasHoras(dia) || !ecPronosticoInHorasHorizon(dia.fecha, st.todayIso)) continue;
    var inner = slides[i].querySelector(".ec-pronostico-slide-inner");
    if (!inner) continue;
    inner.insertAdjacentHTML("beforeend", ecPronosticoSlideHorasHtml(dia));
  }
}

function ecMountPronosticoSwiper(swiperEl) {
  if (!swiperEl) return;
  _ecPronosticoSwiperEl = swiperEl;
  ecDestroyPronosticoSwiperOnly();
  if (ecCarouselIsNarrow()) {
    ecPronosticoMarkStacked(swiperEl, true);
    ecPronosticoMountStackedHoras(swiperEl);
    ecPronosticoApplyStackFold(swiperEl);
    return;
  }
  ecPronosticoMarkStacked(swiperEl, false);
  ecPronosticoClearStackFold(swiperEl);
  ecPronosticoClearStackedHoras(swiperEl);
  ecPronosticoBindSlideSelect(swiperEl);
  if (typeof Swiper === "undefined") {
    ecPronosticoSelectDia(0);
    return;
  }
  var wrap = swiperEl.closest(".ec-rp-pronostico-swiper-wrap");
  var prevEl = wrap ? wrap.querySelector(".ec-pronostico-nav-prev") : null;
  var nextEl = wrap ? wrap.querySelector(".ec-pronostico-nav-next") : null;
  _ecPronosticoSwiper = new Swiper(swiperEl, ecPronosticoSwiperParams(prevEl, nextEl));
  ecPronosticoSelectDia((_ecPronosticoSwiper && _ecPronosticoSwiper.activeIndex) || 0);
}

function ecPronosticoSelectDia(index) {
  if (ecCarouselIsNarrow()) return;
  var el = _ecPronosticoSwiperEl;
  if (el) {
    var slides = el.querySelectorAll(".ec-pronostico-slide");
    var i;
    for (i = 0; i < slides.length; i++) {
      slides[i].classList.toggle("is-selected", i === index);
    }
  }
  ecPronosticoSyncHoras(index);
}

function ecPronosticoBindSlideSelect(swiperEl) {
  if (!swiperEl || swiperEl.getAttribute("data-ec-select") === "1") return;
  swiperEl.setAttribute("data-ec-select", "1");
  swiperEl.addEventListener("click", function (ev) {
    if (ecCarouselIsNarrow()) return;
    if (ev.target.closest("button, a")) return;
    var slide = ev.target.closest(".ec-pronostico-slide");
    if (!slide || !swiperEl.contains(slide)) return;
    var slides = swiperEl.querySelectorAll(".ec-pronostico-slide");
    var idx = -1;
    var i;
    for (i = 0; i < slides.length; i++) {
      if (slides[i] === slide) { idx = i; break; }
    }
    if (idx < 0) return;
    ecPronosticoSelectDia(idx);
  });
}

function ecBindPronosticoAxis() {
  if (_ecPronosticoAxisMq) return;
  _ecPronosticoAxisMq = window.matchMedia(EC_CAROUSEL_NARROW_MQ);
  var onChange = function () {
    var el = _ecPronosticoSwiperEl;
    if (!el || !document.contains(el)) return;
    var idx = (_ecPronosticoSwiper && _ecPronosticoSwiper.activeIndex) || 0;
    ecMountPronosticoSwiper(el);
    if (_ecPronosticoSwiper && idx) _ecPronosticoSwiper.slideTo(idx, 0, false);
  };
  if (_ecPronosticoAxisMq.addEventListener) _ecPronosticoAxisMq.addEventListener("change", onChange);
  else if (_ecPronosticoAxisMq.addListener) _ecPronosticoAxisMq.addListener(onChange);
}

function ecDestroyPronosticoCarousel() {
  ecDestroyPronosticoSwiperOnly();
  _ecPronosticoSwiperEl = null;
  _ecPronosticoHorasState = null;
}

function ecTimelineAnchorChipClass(freshLabel) {
  var cls = "ec-pronostico-today-chip ec-timeline-anchor-chip";
  if (freshLabel === "Hoy" || freshLabel === "Ayer") cls += " ec-timeline-anchor-chip--accent";
  return cls;
}

function ecPronosticoKickerText(horizonteDias, historiaLimitada) {
  var base = "Pron\u00f3stico de la estaci\u00f3n";
  if (historiaLimitada) {
    return base + " \u00b7 historia < 20 a\u00f1os";
  }
  return base;
}

function ecPronosticoUpdateKicker(sectionId, horizonteDias, historiaLimitada) {
  if (!sectionId) return;
  var section = document.getElementById(sectionId);
  if (!section) return;
  var kicker = section.querySelector(".ec-rp-pronostico-kicker");
  if (kicker) {
    kicker.textContent = ecPronosticoKickerText(horizonteDias, historiaLimitada);
  }
  var link = section.querySelector(".ec-rp-pronostico-detail");
  var n = parseInt(horizonteDias, 10);
  if (link && n > 0) link.textContent = "Ver los " + n + " d\u00edas \u2192";
}

function ecSanitizePronosticoPayload(payload) {
  if (!payload || payload.error || !Array.isArray(payload.dias) || !payload.dias.length) return null;
  return payload;
}

function ecPronosticoEsc(s) {
  if (s == null) return "";
  return String(s)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

function ecPronosticoFmtValor(v) {
  if (v == null || isNaN(v)) return null;
  return Number(v).toLocaleString("es-CL", { maximumFractionDigits: 1 });
}

function ecPronosticoTodayIso() {
  var p = ecPronosticoSantiagoParts(new Date());
  if (p.year && p.month && p.day) return p.year + "-" + p.month + "-" + p.day;
  var d = new Date();
  var y = d.getFullYear();
  var m = String(d.getMonth() + 1).padStart(2, "0");
  var day = String(d.getDate()).padStart(2, "0");
  return y + "-" + m + "-" + day;
}

function ecPronosticoDiasDesdeHoy(dias, todayIso) {
  var out = [];
  var i, d, f;
  if (!Array.isArray(dias)) return out;
  for (i = 0; i < dias.length; i++) {
    d = dias[i];
    f = d && d.fecha ? String(d.fecha).slice(0, 10) : "";
    if (!f) continue;
    if (todayIso && f < todayIso) continue;
    out.push(d);
  }
  return out;
}

function ecPronosticoDaysBetween(fromIso, toIso) {
  var a = Date.parse(String(fromIso).slice(0, 10) + "T00:00:00Z");
  var b = Date.parse(String(toIso).slice(0, 10) + "T00:00:00Z");
  if (isNaN(a) || isNaN(b)) return null;
  return Math.round((b - a) / 86400000);
}

function ecPronosticoFrescura(fechaIso, todayIso) {
  var dias = ecPronosticoDaysBetween(fechaIso, todayIso);
  if (dias == null || dias < 0) return { dias: dias, label: "", fresh: false };
  if (dias === 0) return { dias: 0, label: "Hoy", fresh: true };
  if (dias === 1) return { dias: 1, label: "Ayer", fresh: true };
  if (dias <= 7) return { dias: dias, label: "Hace " + dias + " d\u00edas", fresh: true };
  if (dias <= 30) return { dias: dias, label: "Hace " + dias + " d\u00edas", fresh: false };
  return { dias: dias, label: "", fresh: false };
}

function ecPronosticoFormatDayHeader(fecha) {
  if (!fecha) return { weekday: "", dateLine: "" };
  var parts = String(fecha).split("-");
  if (parts.length < 3) return { weekday: fecha, dateLine: "" };
  var dt = new Date(parseInt(parts[0], 10), parseInt(parts[1], 10) - 1, parseInt(parts[2], 10));
  if (isNaN(dt.getTime())) return { weekday: fecha, dateLine: "" };
  var weekday = dt.toLocaleDateString("es-CL", { weekday: "long" });
  weekday = weekday.charAt(0).toUpperCase() + weekday.slice(1);
  var dateLine = dt.toLocaleDateString("es-CL", { day: "numeric", month: "short" });
  return { weekday: weekday, dateLine: dateLine };
}

function ecPronosticoVarsMap(variables) {
  var byAbr = {};
  (variables || []).forEach(function (v) {
    if (v && v.nombre_abr) byAbr[v.nombre_abr] = v;
  });
  return byAbr;
}

function ecPronosticoGetVar(byAbr, slot) {
  if (byAbr[slot.abr]) return byAbr[slot.abr];
  if (slot.fallback && byAbr[slot.fallback]) return byAbr[slot.fallback];
  return null;
}

function ecPronosticoNum(byAbr, abr) {
  var v = byAbr[abr];
  if (!v || v.valor == null || isNaN(v.valor)) return null;
  return Number(v.valor);
}

/** Paleta de condici\u00f3n (sol / nube / lluvia / nieve / tormenta). */
var EC_PRONOSTICO_COND_COLORS = {
  sun: "#DF9531",
  cloud: "#8A95A0",
  rain: "#F4B860",
  rainHeavy: "#C4821E",
  snow: "#6E7C8C",
  storm: "#394553"
};

/** Fallback neutro cuando weather_code a\u00fan no est\u00e1 en los datos. */
var EC_PRONOSTICO_COND_NEUTRAL = {
  id: "unknown",
  composite: "nublado",
  label: "Sin datos",
  desc: "Sin informaci\u00f3n de condici\u00f3n",
  color: EC_PRONOSTICO_COND_COLORS.cloud
};

function ecPronosticoResolveWeatherCode(dia, byAbr) {
  if (dia && dia.weather_code != null && dia.weather_code !== "") {
    var top = Number(dia.weather_code);
    if (isFinite(top)) return top;
  }
  return ecPronosticoNum(byAbr, "weather_code");
}

/** Entrada WMO: id, composite, label corto, descripci\u00f3n (tooltip), color. */
function ecPronosticoWmoEntry(id, composite, label, desc, color) {
  return { id: id, composite: composite, label: label, desc: desc, color: color };
}

/**
 * Mapeo WMO Open-Meteo (daily weather_code) \u2192 icono compuesto FA 4.7.
 * Cada c\u00f3digo con etiqueta e intensidad visual propia; ver https://open-meteo.com/en/docs
 */
function ecPronosticoWmoCondition(code) {
  code = Math.round(Number(code));
  if (!isFinite(code)) return null;
  var C = EC_PRONOSTICO_COND_COLORS;
  var map = {
    0: ecPronosticoWmoEntry("sunny", "soleado", "Soleado", "Cielo despejado", C.sun),
    1: ecPronosticoWmoEntry("mainly_clear", "parcial-soleado", "Mayormente soleado", "Casi despejado con pocas nubes", C.sun),
    2: ecPronosticoWmoEntry("partly", "parcial-nublado", "Parcialmente soleado", "Sol con nubosidad parcial", C.sun),
    3: ecPronosticoWmoEntry("cloudy", "nublado", "Nublado", "Cielo cubierto", C.cloud),
    45: ecPronosticoWmoEntry("fog", "soleado-bruma", "Niebla", "Niebla", C.cloud),
    48: ecPronosticoWmoEntry("fog_rime", "soleado-bruma", "Niebla con escarcha", "Niebla con dep\u00f3sito de escarcha", C.cloud),
    51: ecPronosticoWmoEntry("drizzle_light", "llovizna", "Llovizna ligera", "Llovizna de baja intensidad", C.rain),
    53: ecPronosticoWmoEntry("drizzle", "llovizna", "Llovizna", "Llovizna moderada", C.rain),
    55: ecPronosticoWmoEntry("drizzle_heavy", "llovizna", "Llovizna intensa", "Llovizna densa", C.rain),
    56: ecPronosticoWmoEntry("freezing_drizzle_light", "nieve-hielo", "Llovizna helada", "Llovizna helada ligera", C.rainHeavy),
    57: ecPronosticoWmoEntry("freezing_drizzle_heavy", "nieve-hielo", "Llovizna helada intensa", "Llovizna helada densa", C.rainHeavy),
    61: ecPronosticoWmoEntry("rain_light", "lluvia", "Lluvia ligera", "Lluvia de baja intensidad", C.rain),
    63: ecPronosticoWmoEntry("rain", "lluvia", "Lluvia moderada", "Lluvia moderada", C.rain),
    65: ecPronosticoWmoEntry("rain_heavy", "lluvia-fuerte", "Lluvia fuerte", "Lluvia de alta intensidad", C.rainHeavy),
    66: ecPronosticoWmoEntry("freezing_rain_light", "nieve-hielo", "Lluvia helada", "Lluvia helada ligera", C.rainHeavy),
    67: ecPronosticoWmoEntry("freezing_rain_heavy", "nieve-hielo", "Lluvia helada fuerte", "Lluvia helada intensa", C.rainHeavy),
    71: ecPronosticoWmoEntry("snow_light", "nieve", "Nieve ligera", "Nevada de baja intensidad", C.snow),
    73: ecPronosticoWmoEntry("snow", "nieve", "Nieve moderada", "Nevada moderada", C.snow),
    75: ecPronosticoWmoEntry("snow_heavy", "nieve", "Nieve fuerte", "Nevada intensa", C.snow),
    77: ecPronosticoWmoEntry("snow_grains", "nieve", "Granos de nieve", "Granos de nieve", C.snow),
    80: ecPronosticoWmoEntry("showers_light", "lluvia-y-sol", "Chubascos ligeros", "Chubascos d\u00e9biles con claros", C.rain),
    81: ecPronosticoWmoEntry("showers", "lluvia-fuerte", "Chubascos", "Chubascos moderados", C.rain),
    82: ecPronosticoWmoEntry("showers_heavy", "lluvia-fuerte", "Chubascos fuertes", "Chubascos violentos", C.rainHeavy),
    85: ecPronosticoWmoEntry("snow_showers_light", "nieve", "Nevada ligera", "Chubascos de nieve leves", C.snow),
    86: ecPronosticoWmoEntry("snow_showers_heavy", "lluvia-y-nieve", "Nevada fuerte", "Chubascos de nieve intensos", C.snow),
    95: ecPronosticoWmoEntry("thunderstorm", "nube-truenos", "Tormenta", "Tormenta el\u00e9ctrica", C.storm),
    96: ecPronosticoWmoEntry("thunderstorm_hail", "lluvia-con-granizo", "Tormenta con granizo", "Tormenta con granizo ligero", C.storm),
    99: ecPronosticoWmoEntry("thunderstorm_hail_heavy", "lluvia-con-granizo", "Tormenta con granizo fuerte", "Tormenta con granizo intenso", C.storm)
  };
  return map[code] || null;
}

function ecPronosticoWmoPrecipSeverity(code) {
  code = Math.round(Number(code));
  if (!isFinite(code)) return 0;
  if (code >= 95) return 5;
  if (code === 82 || code === 65 || code === 67 || code === 57 || code === 55 || code === 75 || code === 86) return 3;
  if (code === 81 || code === 63 || code === 53 || code === 73) return 2;
  if ((code >= 51 && code <= 67) || (code >= 71 && code <= 82) || (code >= 85 && code <= 86)) return 1;
  return 0;
}

/** Desempate dentro de la misma severidad: el chubasco le gana a la llovizna. */
function ecPronosticoPrecipKindRank(code) {
  code = Math.round(Number(code));
  if (code === 80 || code === 81 || code === 82) return 2;
  if (code >= 61 && code <= 67) return 1;
  return 0;
}

function ecPronosticoNumAt(v) {
  if (v == null || v === "") return null;
  var n = Number(v);
  return isFinite(n) ? n : null;
}

function ecPronosticoModeCode(codes) {
  var counts = {};
  var best = null;
  var bestN = 0;
  var i, c;
  for (i = 0; i < codes.length; i++) {
    c = codes[i];
    if (c == null || !isFinite(Number(c))) continue;
    c = Math.round(Number(c));
    counts[c] = (counts[c] || 0) + 1;
    if (
      counts[c] > bestN ||
      (counts[c] === bestN && best != null && ecPronosticoWmoPrecipSeverity(c) > ecPronosticoWmoPrecipSeverity(best))
    ) {
      best = c;
      bestN = counts[c];
    }
  }
  return best;
}

function ecPronosticoCopyCond(cond, label) {
  return {
    id: cond.id,
    composite: cond.composite,
    label: label,
    desc: label,
    color: cond.color
  };
}

function ecPronosticoNightComposite(code) {
  code = Math.round(Number(code));
  if (code === 0) return "luna";
  if (code === 1) return "luna-parcial";
  if (code === 2) return "luna-nubes";
  if (code === 45 || code === 48) return "bruma";
  if (code === 80) return "lluvia-y-luna";
  return null;
}

function ecPronosticoHourIsNight(ts, isDay) {
  var day = ecPronosticoNumAt(isDay);
  if (day != null) return day === 0;
  return ecPronosticoIsNightHour(ecPronosticoLocalHour(ts));
}

function ecPronosticoHourCondition(code, isNight) {
  var cond = ecPronosticoWmoCondition(code);
  if (!cond) return null;
  if (!isNight) return cond;
  var night = ecPronosticoNightComposite(code);
  if (!night) return cond;
  var out = ecPronosticoCopyCond(cond, cond.label);
  out.desc = cond.desc;
  out.composite = night;
  return out;
}

/** Una hora cuenta como lluvia probable desde este % (inclusive), en la fila y en el día. */
var EC_PRONOSTICO_POP_LLUVIA = 50;

function ecPronosticoIsSunComposite(composite) {
  return composite === "soleado" || composite === "parcial-soleado" || composite === "parcial-nublado";
}

function ecPronosticoIsPrecipComposite(composite) {
  return composite === "llovizna" || composite === "lluvia" || composite === "lluvia-fuerte" ||
    composite === "lluvia-y-sol" || composite === "lluvia-y-luna" ||
    composite === "nieve" || composite === "nieve-hielo" || composite === "lluvia-y-nieve" ||
    composite === "nube-truenos" || composite === "lluvia-con-granizo";
}

/** Fila horaria: el icono es el weather_code. El pop >= 50 suma lluvia a un cielo seco. Los mm no entran. */
function ecPronosticoHoraPpCondition(cond, pp, isNight, pop) {
  if (!cond) return null;
  void pp;
  var prob = ecPronosticoNumAt(pop);
  if (prob == null || prob < EC_PRONOSTICO_POP_LLUVIA) return cond;
  var c = cond.composite;
  if (ecPronosticoIsPrecipComposite(c)) return cond;
  var next = (
    c === "soleado" || c === "parcial-soleado" || c === "parcial-nublado" ||
    c === "luna" || c === "luna-parcial" || c === "luna-nubes"
  ) ? (isNight ? "lluvia-y-luna" : "lluvia-y-sol") : "lluvia";
  if (next === c) return cond;
  return { id: cond.id, composite: next, label: cond.label, desc: cond.desc, color: cond.color };
}

function ecPronosticoHoraMmText(pp) {
  var p = ecPronosticoNumAt(pp);
  if (p == null) return "";
  var txt = ecPronosticoFmtValor(p);
  return txt == null ? "" : txt + " mm";
}

function ecPronosticoHoraPopText(pop) {
  var prob = ecPronosticoNumAt(pop);
  if (prob == null) return "";
  return Math.round(prob) + " %";
}

/** Máximo de las horas del día: la probabilidad de lluvia de la tarjeta. */
function ecPronosticoDayPopMax(dia) {
  if (!dia || !dia.horas || dia.horas.pop == null) return null;
  var pop = ecPronosticoAsArr(dia.horas.pop);
  var max = null;
  var i, n;
  for (i = 0; i < pop.length; i++) {
    n = ecPronosticoNumAt(pop[i]);
    if (n == null) continue;
    if (max == null || n > max) max = n;
  }
  return max == null ? null : Math.round(max);
}

function ecPronosticoHourPeriodKnown(ts, isDay) {
  if (ecPronosticoNumAt(isDay) != null) return true;
  return ecPronosticoLocalHour(ts) != null;
}

/** Máximo de pop en las horas diurnas (wantNight false) o nocturnas. */
function ecPronosticoPopPeriodMax(dia, wantNight) {
  var none = { known: false, max: null };
  if (!dia || !dia.horas || dia.horas.pop == null) return none;
  var pop = ecPronosticoAsArr(dia.horas.pop);
  var day = dia.horas.day == null ? [] : ecPronosticoAsArr(dia.horas.day);
  var ts = dia.horas.ts == null ? [] : ecPronosticoAsArr(dia.horas.ts);
  var max = null;
  var known = false;
  var i, n, night;
  for (i = 0; i < pop.length; i++) {
    if (!ecPronosticoHourPeriodKnown(ts[i], day[i])) continue;
    night = ecPronosticoHourIsNight(ts[i], day[i]);
    if (night !== !!wantNight) continue;
    known = true;
    n = ecPronosticoNumAt(pop[i]);
    if (n == null) continue;
    if (max == null || n > max) max = n;
  }
  return { known: known, max: max == null ? null : Math.round(max) };
}

function ecPronosticoDayPopRow(label, prob) {
  var empty = prob == null;
  var val = empty ? "\u2014" : prob + " %";
  return (
    '<span class="ec-pronostico-day-pop-label">' + label + "</span>" +
    '<span class="ec-pronostico-day-pop-val' + (empty ? " is-empty" : "") + '">' + val + "</span>"
  );
}

function ecPronosticoDayPopHtml(dia) {
  var dayP = ecPronosticoPopPeriodMax(dia, false);
  var nightP = ecPronosticoPopPeriodMax(dia, true);
  if (dayP.known || nightP.known) {
    if (dayP.max == null && nightP.max == null) return "";
    var dayTxt = dayP.max == null ? "sin dato" : dayP.max + " %";
    var nightTxt = nightP.max == null ? "sin dato" : nightP.max + " %";
    var label = "Probabilidad de lluvia diurna " + dayTxt +
      ". Probabilidad de lluvia nocturna " + nightTxt + ".";
    return (
      '<span class="ec-pronostico-day-pop" title="' + ecPronosticoEsc(label) +
      '" aria-label="' + ecPronosticoEsc(label) + '">' +
      '<span class="ec-pronostico-day-pop-kicker">Prob. de lluvia</span>' +
      ecPronosticoDayPopRow("Diurna", dayP.max) +
      ecPronosticoDayPopRow("Nocturna", nightP.max) +
      "</span>"
    );
  }
  var prob = ecPronosticoDayPopMax(dia);
  if (prob == null) return "";
  var one = "Probabilidad de lluvia " + prob + " %";
  return (
    '<span class="ec-pronostico-day-pop" title="' + ecPronosticoEsc(one) +
    '" aria-label="' + ecPronosticoEsc(one) + '">' +
    '<span class="ec-pronostico-day-pop-kicker">Prob. de lluvia</span>' +
    '<span class="ec-pronostico-day-pop-val">' + prob + " %</span></span>"
  );
}

function ecPronosticoConditionFromDailyCode(dia, byAbr) {
  var pp = ecPronosticoNum(byAbr, "pp");
  var code = ecPronosticoResolveWeatherCode(dia, byAbr);
  if (code == null) return EC_PRONOSTICO_COND_NEUTRAL;
  if (pp != null && isFinite(pp) && pp <= 0 && ecPronosticoWmoPrecipSeverity(code) > 0) {
    return ecPronosticoWmoCondition(3);
  }
  return ecPronosticoWmoCondition(code) || EC_PRONOSTICO_COND_NEUTRAL;
}

function ecPronosticoDayHours(dia) {
  if (!dia || !dia.horas) return null;
  var ts = ecPronosticoAsArr(dia.horas.ts);
  if (ts.length < 12) return null;
  var n = ts.length;
  function col(name) {
    var a = ecPronosticoAsArr(dia.horas[name]);
    var i, out;
    if (a.length >= n) return a;
    out = a.slice();
    for (i = a.length; i < n; i++) out.push(null);
    return out;
  }
  return { ts: ts, pp: col("pp"), wc: col("wc"), day: col("day"), pop: col("pop"), n: n };
}

/** Más de 3 horas de día con pop >= 50 %: el icono sale de esos códigos. Cielo seco → lluvia genérica. */
function ecPronosticoPopRainLead(codes, sky) {
  var precip = [];
  var i, c, cond;
  for (i = 0; i < codes.length; i++) {
    c = codes[i];
    if (c != null && ecPronosticoWmoPrecipSeverity(c) >= 1) precip.push(c);
  }
  if (precip.length) {
    cond = ecPronosticoWmoCondition(ecPronosticoModeCode(precip));
    if (cond) return cond;
  }
  var C = EC_PRONOSTICO_COND_COLORS;
  var tip = "M\u00e1s de 3 horas de d\u00eda con 50 % o m\u00e1s de probabilidad de lluvia";
  var sunny = sky && (
    sky.composite === "soleado" || sky.composite === "parcial-soleado" || sky.composite === "parcial-nublado"
  );
  if (sunny) return ecPronosticoWmoEntry("pop_likely", "lluvia-y-sol", "Probable lluvia", tip, C.rain);
  return ecPronosticoWmoEntry("pop_likely", "lluvia", "Probable lluvia", tip, C.rain);
}

function ecPronosticoConditionFromHours(dia, byAbr, horas) {
  var ppChip = ecPronosticoNum(byAbr, "pp");
  var ppZero = ppChip != null && isFinite(ppChip) && ppChip <= 0;
  var dayIdx = [];
  var i;
  for (i = 0; i < horas.n; i++) {
    if (!ecPronosticoHourIsNight(horas.ts[i], horas.day[i])) dayIdx.push(i);
  }
  if (dayIdx.length < 4) return ecPronosticoConditionFromDailyCode(dia, byAbr);

  function popAt(idx) {
    if (!horas.pop) return null;
    return ecPronosticoNumAt(horas.pop[idx]);
  }
  function wcAt(idx) {
    var c = ecPronosticoNumAt(horas.wc[idx]);
    return c == null ? null : Math.round(c);
  }
  function wet(idx) {
    var c = wcAt(idx);
    return c != null && ecPronosticoWmoPrecipSeverity(c) >= 1;
  }
  function atSeverity(sev) {
    return function (idx) {
      var c = wcAt(idx);
      return c != null && ecPronosticoWmoPrecipSeverity(c) === sev;
    };
  }
  function thunder(idx) {
    var c = wcAt(idx);
    return c != null && c >= 95;
  }
  function collect(idxs, pred) {
    var out = [];
    var k, c;
    for (k = 0; k < idxs.length; k++) {
      if (!pred(idxs[k])) continue;
      c = wcAt(idxs[k]);
      if (c != null) out.push(c);
    }
    return out;
  }

  var dryCodes = [];
  var k, c;
  for (k = 0; k < dayIdx.length; k++) {
    c = wcAt(dayIdx[k]);
    if (c != null && ecPronosticoWmoPrecipSeverity(c) === 0) dryCodes.push(c);
  }
  var skyCode = ecPronosticoModeCode(dryCodes);
  var sky = (skyCode == null ? ecPronosticoWmoCondition(3) : ecPronosticoWmoCondition(skyCode)) || ecPronosticoWmoCondition(3);

  var popCodes = [];
  var popN = 0;
  for (k = 0; k < dayIdx.length; k++) {
    if (popAt(dayIdx[k]) == null || popAt(dayIdx[k]) < EC_PRONOSTICO_POP_LLUVIA) continue;
    popN++;
    c = wcAt(dayIdx[k]);
    if (c != null) popCodes.push(c);
  }

  var thunderCodes = collect(dayIdx, thunder);
  var band3 = collect(dayIdx, atSeverity(3));
  var band2 = collect(dayIdx, atSeverity(2));
  var wetCodes = collect(dayIdx, wet);
  var lead = sky;
  var leadCode = null;
  // Dentro de la banda, manda el código que concentra los mm. Si empatan, el chubasco le gana a la llovizna.
  function leadFromDay(pred) {
    var mmBy = {};
    var nBy = {};
    var order = [];
    var j, code, pMm, best, bestMm, bestN, sameMm, better;
    for (j = 0; j < dayIdx.length; j++) {
      if (!pred(dayIdx[j])) continue;
      code = wcAt(dayIdx[j]);
      if (code == null) continue;
      if (!nBy[code]) {
        nBy[code] = 0;
        mmBy[code] = 0;
        order.push(code);
      }
      nBy[code]++;
      pMm = ecPronosticoNumAt(horas.pp[dayIdx[j]]);
      if (pMm != null && pMm > 0) mmBy[code] += pMm;
    }
    best = null;
    bestMm = 0;
    bestN = 0;
    for (j = 0; j < order.length; j++) {
      code = order[j];
      if (best == null) {
        best = code;
        bestMm = mmBy[code];
        bestN = nBy[code];
        continue;
      }
      sameMm = Math.abs(mmBy[code] - bestMm) <= 1e-6;
      better = mmBy[code] > bestMm + 1e-6 ||
        (sameMm && nBy[code] > bestN) ||
        (sameMm && nBy[code] === bestN && ecPronosticoPrecipKindRank(code) > ecPronosticoPrecipKindRank(best));
      if (better) {
        best = code;
        bestMm = mmBy[code];
        bestN = nBy[code];
      }
    }
    if (best == null) return;
    leadCode = best;
    lead = ecPronosticoWmoCondition(best) || sky;
  }
  if (thunderCodes.length >= 1) {
    leadFromDay(thunder);
  } else if (band3.length >= 2) {
    leadFromDay(atSeverity(3));
  } else if (band2.length >= 2) {
    leadFromDay(atSeverity(2));
  } else if (popN > 3) {
    var anyPopPrecip = false;
    for (k = 0; k < dayIdx.length; k++) {
      if (popAt(dayIdx[k]) != null && popAt(dayIdx[k]) >= EC_PRONOSTICO_POP_LLUVIA && wet(dayIdx[k])) {
        anyPopPrecip = true;
        break;
      }
    }
    if (anyPopPrecip) {
      leadFromDay(function (idx) {
        return popAt(idx) != null && popAt(idx) >= EC_PRONOSTICO_POP_LLUVIA && wet(idx);
      });
    } else lead = ecPronosticoPopRainLead(popCodes, sky) || sky;
  } else if (wetCodes.length >= 4 || wetCodes.length / dayIdx.length >= 0.35) {
    leadFromDay(wet);
  }
  var dailyCode = ecPronosticoResolveWeatherCode(dia, byAbr);
  var dailyPrecip = dailyCode != null && ecPronosticoWmoPrecipSeverity(dailyCode) > 0;
  var anyPrecip = dailyPrecip;
  if (!anyPrecip) {
    for (k = 0; k < horas.n; k++) {
      c = wcAt(k);
      if (c != null && ecPronosticoWmoPrecipSeverity(c) > 0) { anyPrecip = true; break; }
    }
  }
  if (ppZero) return anyPrecip ? ecPronosticoWmoCondition(3) : sky;
  // La banda más severa que concentra más de la mitad de los mm diurnos pone el nombre.
  if (lead && ecPronosticoIsPrecipComposite(lead.composite)) {
    var sevList = [1, 2, 3, 5];
    var buckets = {};
    var bi;
    for (bi = 0; bi < sevList.length; bi++) buckets[sevList[bi]] = { mm: 0, codes: [] };
    var totalMm = 0;
    var s, p;
    for (k = 0; k < dayIdx.length; k++) {
      c = wcAt(dayIdx[k]);
      if (c == null) continue;
      s = ecPronosticoWmoPrecipSeverity(c);
      if (!buckets[s]) continue;
      p = ecPronosticoNumAt(horas.pp[dayIdx[k]]);
      if (p == null || p <= 0) continue;
      buckets[s].mm += p;
      buckets[s].codes.push(c);
      totalMm += p;
    }
    var leadSev = leadCode == null ? 0 : ecPronosticoWmoPrecipSeverity(leadCode);
    var bestSev = 0;
    var bestMm = 0;
    for (bi = 0; bi < sevList.length; bi++) {
      s = sevList[bi];
      if (s <= leadSev || buckets[s].mm <= bestMm) continue;
      bestMm = buckets[s].mm;
      bestSev = s;
    }
    if (totalMm > 0 && bestSev > leadSev && bestMm > totalMm / 2) {
      leadFromDay(atSeverity(bestSev));
    }
  }
  if (ppChip != null && ppChip >= 0.5 && dailyPrecip && lead) {
    var useDaily = ecPronosticoIsSunComposite(lead.composite);
    if (!useDaily && ecPronosticoIsPrecipComposite(lead.composite)) {
      var lSev = leadCode == null ? 0 : ecPronosticoWmoPrecipSeverity(leadCode);
      useDaily = ecPronosticoWmoPrecipSeverity(dailyCode) > lSev;
    }
    if (useDaily) {
      leadCode = dailyCode;
      lead = ecPronosticoWmoCondition(dailyCode) || lead;
    }
  }
  return lead;
}

function ecPronosticoConditionForDay(dia, byAbr) {
  var horas = ecPronosticoDayHours(dia);
  if (horas) return ecPronosticoConditionFromHours(dia, byAbr, horas);
  return ecPronosticoConditionFromDailyCode(dia, byAbr);
}

/** Renderiza icono como <img> a un SVG independiente (uno por condici?n). */
function ecPronosticoConditionIconHtml(cond) {
  var iconId = (cond && cond.composite) || "nublado";
  var src = "/icons/weather/" + iconId + ".svg?v=wmo5";
  return (
    '<img class="ec-wmo-icon ec-wmo-icon--' + ecPronosticoEsc(iconId) + '" src="' +
    ecPronosticoEsc(src) + '" alt="" aria-hidden="true" loading="lazy" decoding="async" />'
  );
}
function ecPronosticoBestRank(rankings) {
  var best = null;
  (rankings || []).forEach(function (r) {
    if (!r || !r.ranking_estimado || r.ranking_estimado > 10) return;
    if (!best || r.ranking_estimado < best.ranking_estimado) best = r;
  });
  return best;
}

function ecPronosticoRankTooltip(rank, direccion) {
  var dirTxt = direccion === "min" ? "bajo" : "alto";
  return "Este valor entrar\u00eda como el #" + rank + " m\u00e1s " + dirTxt + " registrado hist\u00f3ricamente en esta estaci\u00f3n.";
}

function ecPronosticoRankHtml(rankings) {
  var r = ecPronosticoBestRank(rankings);
  if (!r) return "";
  var rank = r.ranking_estimado;
  var tip = ecPronosticoRankTooltip(rank, r.direccion);
  var inner = (rank <= 3 && typeof ecMedal === "function")
    ? ecMedal(rank)
    : '<i class="fa fa-exclamation-circle" aria-hidden="true"></i>';
  return '<span class="ec-pronostico-rank ec-pronostico-rank--n' + rank + '" title="' + ecPronosticoEsc(tip) + '" aria-label="' + ecPronosticoEsc(tip) + '">' + inner + "</span>";
}

function ecPronosticoTempBit(byAbr, abr, icon) {
  var v = byAbr[abr];
  var txt = v ? ecPronosticoFmtValor(v.valor) : null;
  var rankHtml = v ? ecPronosticoRankHtml(v.rankings) : "";
  var valueHtml = txt == null
    ? '<span class="ec-pronostico-metric-empty">\u2014</span>'
    : ecPronosticoEsc(txt) + "\u00b0" + rankHtml;
  return (
    '<span class="ec-pronostico-metric ec-pronostico-metric--' + abr + '">' +
    '<i class="fa ' + icon + '" aria-hidden="true"></i>' +
    '<span class="ec-pronostico-metric-value">' + valueHtml + "</span>" +
    "</span>"
  );
}

function ecPronosticoTempRangeHtml(byAbr) {
  var mx = byAbr.t_max;
  var mn = byAbr.t_min;
  var mxTxt = mx ? ecPronosticoFmtValor(mx.valor) : null;
  var mnTxt = mn ? ecPronosticoFmtValor(mn.valor) : null;
  var label = "Temperatura m\u00e1xima " + (mxTxt == null ? "sin dato" : mxTxt + " grados") +
    ", m\u00ednima " + (mnTxt == null ? "sin dato" : mnTxt + " grados");
  return (
    '<div class="ec-pronostico-temp-range" aria-label="' + ecPronosticoEsc(label) + '">' +
    ecPronosticoTempBit(byAbr, "t_max", "fa-sun-o") +
    '<span class="ec-pronostico-temp-sep" aria-hidden="true">/</span>' +
    ecPronosticoTempBit(byAbr, "t_min", "fa-moon-o") +
    "</div>"
  );
}

function ecPronosticoMetricHtml(slot, byAbr) {
  var meta = EC_PRONOSTICO_VAR_META[slot.abr] || EC_PRONOSTICO_VAR_META[slot.fallback] || {
    label: slot.abr,
    icon: "fa-line-chart",
    color: "#F4B860"
  };
  var v = ecPronosticoGetVar(byAbr, slot);
  var cssAbr = slot.abr;
  var featured = !!slot.featured;
  var cls = "ec-pronostico-metric ec-pronostico-metric--" + ecPronosticoEsc(cssAbr);
  if (featured) cls += " ec-pronostico-metric--featured";
  if (!v) cls += " ec-pronostico-metric--missing";

  var rankHtml = v ? ecPronosticoRankHtml(v.rankings) : "";
  var valueHtml;
  if (!v || ecPronosticoFmtValor(v.valor) == null) {
    valueHtml = '<span class="ec-pronostico-metric-empty">Sin informaci\u00f3n</span>';
  } else {
    var unitHtml = v.unidad
      ? '<span class="ec-pronostico-metric-unit">' + ecPronosticoEsc(v.unidad) + "</span>"
      : "";
    valueHtml = ecPronosticoFmtValor(v.valor) + unitHtml + rankHtml;
  }

  return (
    '<div class="' + cls + '" style="--ec-pron-metric-color:' + meta.color + ';"' +
    ' aria-label="' + ecPronosticoEsc(meta.label) + '">' +
    '<span class="ec-pronostico-metric-icon"><i class="fa ' + meta.icon + '" aria-hidden="true"></i></span>' +
    '<div class="ec-pronostico-metric-body">' +
    '<span class="ec-pronostico-metric-label">' + ecPronosticoEsc(meta.label) + "</span>" +
    '<span class="ec-pronostico-metric-value">' + valueHtml + "</span>" +
    "</div></div>"
  );
}

function ecRenderPronosticoSlide(dia, todayIso, dayOffset) {
  var hdr = ecPronosticoFormatDayHeader(dia.fecha);
  var isToday = dia.fecha === todayIso;
  var isHero = dayOffset === 0;
  var byAbr = ecPronosticoVarsMap(dia.variables);
  var cond = ecPronosticoConditionForDay(dia, byAbr);
  var wmoCode = ecPronosticoResolveWeatherCode(dia, byAbr);
  var condTip = cond.desc || cond.label;

  var secondaryHtml = "";
  EC_PRONOSTICO_DISPLAY_SLOTS.forEach(function (slot) {
    secondaryHtml += ecPronosticoMetricHtml(slot, byAbr);
  });

  var slideCls = "swiper-slide ec-pronostico-slide";
  if (isToday) slideCls += " is-today";
  if (isHero) slideCls += " is-hero";
  slideCls += " ec-pronostico-slide--" + ecPronosticoEsc(cond.id);

  return (
    '<div class="' + slideCls + '" data-day-offset="' + dayOffset + '" data-fecha="' +
    ecPronosticoEsc(dia.fecha || "") + '">' +
    '<div class="ec-pronostico-slide-inner">' +
    '<header class="ec-pronostico-slide-title">' +
    '<div class="ec-pronostico-slide-head">' +
    '<span class="ec-pronostico-weekday">' + ecPronosticoEsc(hdr.weekday) + "</span>" +
    (isToday ? '<span class="' + ecTimelineAnchorChipClass("Hoy") + '">Hoy</span>' : (isHero ? '<span class="ec-pronostico-today-chip ec-pronostico-next-chip">Pr\u00f3ximo</span>' : "")) +
    "</div>" +
    '<time class="ec-pronostico-date" datetime="' + ecPronosticoEsc(dia.fecha) + '">' + ecPronosticoEsc(hdr.dateLine) + "</time>" +
    "</header>" +
    '<div class="ec-pronostico-condition-wrap" style="--ec-pron-cond-color:' + cond.color + ';">' +
    '<div class="ec-pronostico-condition"' +
    (wmoCode != null ? ' data-weather-code="' + ecPronosticoEsc(String(wmoCode)) + '"' : "") +
    ' title="' + ecPronosticoEsc(condTip) + '" aria-label="' + ecPronosticoEsc(condTip) + '">' +
    ecPronosticoConditionIconHtml(cond) +
    "</div>" +
    '<span class="ec-pronostico-condition-label" title="' + ecPronosticoEsc(condTip) + '">' +
    ecPronosticoEsc(cond.label) + "</span>" +
    ecPronosticoDayPopHtml(dia) +
    "</div>" +
    ecPronosticoTempRangeHtml(byAbr) +
    '<div class="ec-pronostico-metrics-secondary">' + secondaryHtml + "</div>" +
    "</div></div>"
  );
}

function ecPronosticoAsArr(x) {
  if (x == null) return [];
  if (Array.isArray(x)) return x;
  return [x];
}

function ecPronosticoHasHoras(dia) {
  var h = dia && dia.horas;
  if (!h) return false;
  var ts = ecPronosticoAsArr(h.ts);
  var t = ecPronosticoAsArr(h.t);
  return ts.length > 0 && t.length === ts.length;
}

function ecPronosticoAddDaysIso(iso, n) {
  var p = String(iso).split("-");
  var d = new Date(Date.UTC(parseInt(p[0], 10), parseInt(p[1], 10) - 1, parseInt(p[2], 10)));
  d.setUTCDate(d.getUTCDate() + n);
  return d.toISOString().slice(0, 10);
}

function ecPronosticoInHorasHorizon(fecha, todayIso) {
  return !!fecha && !!todayIso && fecha >= todayIso && fecha <= ecPronosticoAddDaysIso(todayIso, 2);
}

function ecPronosticoTsMs(ts) {
  var t = Date.parse(ts);
  return isNaN(t) ? null : t;
}

function ecPronosticoAlignT(arr, tMinChip, tMaxChip) {
  var vals = [];
  var i;
  for (i = 0; i < arr.length; i++) {
    if (arr[i] != null && isFinite(arr[i])) vals.push(arr[i]);
  }
  if (!vals.length || tMinChip == null || tMaxChip == null || !isFinite(tMinChip) || !isFinite(tMaxChip)) {
    return arr.slice();
  }
  var mn = Math.min.apply(null, vals);
  var mx = Math.max.apply(null, vals);
  var span = mx - mn;
  return arr.map(function (t) {
    if (t == null || !isFinite(t)) return t;
    if (Math.abs(span) < 1e-12) return t + (tMaxChip - mx);
    return tMinChip + (t - mn) / span * (tMaxChip - tMinChip);
  });
}

function ecPronosticoLocalHour(ts) {
  var d = new Date(ts);
  if (isNaN(d.getTime())) return null;
  return d.getHours();
}

/* Noche civil fija (antes de las 7 y desde las 20). Sin amanecer por estación. */
function ecPronosticoIsNightHour(h) {
  return h != null && (h < 7 || h >= 20);
}

function ecPronosticoSparkSvg(horas, opts) {
  opts = opts || {};
  var t = ecPronosticoAsArr(horas.t);
  var pp = ecPronosticoAsArr(horas.pp);
  var u = ecPronosticoAsArr(horas.u);
  var ts = ecPronosticoAsArr(horas.ts);
  var n = ts.length;
  if (!n) return "";
  var only = opts.only;
  if (only !== "t" && only !== "pp" && only !== "u") only = "";
  var showT = only === "" || only === "t";
  var showPp = only === "" || only === "pp";
  var showU = only === "u" || (only === "" && !!opts.wind);
  var w = 240;
  var h = (only === "" && opts.wind) ? 64 : 36;
  var padL = 2;
  var padR = 2;
  var padT = 3;
  var padB = 3;
  var innerW = w - padL - padR;
  var innerH = h - padT - padB;
  var tFinite = [];
  var i;
  for (i = 0; i < t.length; i++) {
    if (t[i] != null && isFinite(t[i])) tFinite.push(t[i]);
  }
  var tMin = tFinite.length ? Math.min.apply(null, tFinite) : 0;
  var tMax = tFinite.length ? Math.max.apply(null, tFinite) : 1;
  if (tMax === tMin) tMax = tMin + 1;
  var ppMax = 0;
  for (i = 0; i < pp.length; i++) {
    if (pp[i] != null && isFinite(pp[i]) && pp[i] > ppMax) ppMax = pp[i];
  }
  var uFinite = [];
  for (i = 0; i < u.length; i++) {
    if (u[i] != null && isFinite(u[i])) uFinite.push(u[i]);
  }
  var uMin = uFinite.length ? Math.min.apply(null, uFinite) : 0;
  var uMax = uFinite.length ? Math.max.apply(null, uFinite) : 1;
  if (uMax === uMin) uMax = uMin + 1;
  var now = Date.now();
  var barW = Math.max(1.2, innerW / n * 0.55);
  var bars = "";
  var tPts = [];
  var uPts = [];
  function sparkX(iHour) {
    return padL + (n === 1 ? innerW / 2 : (iHour / (n - 1)) * innerW);
  }
  function sparkEdge(iHour) {
    if (n === 1) return iHour <= 0 ? padL : padL + innerW;
    if (iHour <= 0) return padL;
    if (iHour >= n) return padL + innerW;
    return (sparkX(iHour - 1) + sparkX(iHour)) / 2;
  }
  var dn = "";
  var dnLabels = "";
  var run = 0;
  function flushDn(end) {
    var hour0 = ecPronosticoLocalHour(ts[run]);
    if (hour0 == null) return;
    var night = ecPronosticoIsNightHour(hour0);
    var x0 = sparkEdge(run);
    var x1 = sparkEdge(end);
    var bw = x1 - x0;
    if (bw <= 0) return;
    var cls = night ? "is-night" : "is-day";
    var word = night ? "noche" : "d\u00eda";
    dn += '<rect class="ec-pronostico-spark-dn-plot ' + cls + '" x="' + x0.toFixed(2) +
      '" y="0" width="' + bw.toFixed(2) + '" height="' + h + '"/>';
    dnLabels += '<span class="ec-pronostico-spark-dn ' + cls + '" style="flex:' + (end - run) +
      '"><b>' + word + "</b></span>";
  }
  for (i = 1; i <= n; i++) {
    var prevNight = ecPronosticoIsNightHour(ecPronosticoLocalHour(ts[run]));
    var curNight = i < n && ecPronosticoIsNightHour(ecPronosticoLocalHour(ts[i]));
    if (i === n || prevNight !== curNight) {
      flushDn(i);
      run = i;
    }
  }
  for (i = 0; i < n; i++) {
    var x = sparkX(i);
    var past = "";
    var ms = ecPronosticoTsMs(ts[i]);
    if (ms != null && ms < now) past = " is-past";
    var ppi = pp[i];
    if (showPp && ppi != null && isFinite(ppi) && ppi > 0 && ppMax > 0) {
      var bh = (ppi / ppMax) * (only === "pp" ? innerH : innerH * 0.42);
      bars += '<rect class="ec-pronostico-spark-pp' + past + '" x="' + (x - barW / 2).toFixed(2) +
        '" y="' + (h - padB - bh).toFixed(2) + '" width="' + barW.toFixed(2) +
        '" height="' + bh.toFixed(2) + '" rx="0.6"/>';
    }
    var ti = t[i];
    if (showT && ti != null && isFinite(ti)) {
      var y = padT + (1 - (ti - tMin) / (tMax - tMin)) * innerH;
      tPts.push(x.toFixed(2) + "," + y.toFixed(2));
    }
    if (showU) {
      var ui = u[i];
      if (ui != null && isFinite(ui)) {
        var yu = padT + (1 - (ui - uMin) / (uMax - uMin)) * innerH;
        uPts.push(x.toFixed(2) + "," + yu.toFixed(2));
      }
    }
  }
  var lineT = (showT && tPts.length)
    ? '<polyline class="ec-pronostico-spark-t" fill="none" points="' + tPts.join(" ") + '"/>'
    : "";
  var lineU = (showU && uPts.length)
    ? '<polyline class="ec-pronostico-spark-u" fill="none" points="' + uPts.join(" ") + '"/>'
    : "";
  return '<div class="ec-pronostico-spark-fig">' +
    '<svg class="ec-pronostico-spark" viewBox="0 0 ' + w + " " + h +
    '" width="100%" height="' + h + '" aria-hidden="true">' + dn + bars + lineU + lineT + "</svg>" +
    '<div class="ec-pronostico-spark-axis" aria-hidden="true">' + dnLabels + "</div></div>";
}

function ecPronosticoHoraLegendHtml(opts) {
  opts = opts || {};
  var viento = opts.wind === false
    ? ""
    : '<li><span class="ec-pronostico-hora-legend-swatch is-u" aria-hidden="true"></span>Viento</li>';
  return (
    '<ul class="ec-pronostico-hora-legend">' +
    '<li><span class="ec-pronostico-hora-legend-swatch is-t" aria-hidden="true"></span>Temperatura</li>' +
    viento +
    '<li><span class="ec-pronostico-hora-legend-swatch is-pp" aria-hidden="true"></span>Precipitaci\u00f3n</li>' +
    "</ul>"
  );
}

function ecPronosticoHoraLabel(ts) {
  var d = new Date(ts);
  if (isNaN(d.getTime())) return "";
  var hh = String(d.getHours()).padStart(2, "0");
  var mm = String(d.getMinutes()).padStart(2, "0");
  return hh + ":" + mm;
}

function ecPronosticoHoraListHtml(dia) {
  var horas = {
    ts: ecPronosticoAsArr(dia.horas.ts),
    t: ecPronosticoAsArr(dia.horas.t),
    pp: ecPronosticoAsArr(dia.horas.pp),
    u: ecPronosticoAsArr(dia.horas.u),
    wc: ecPronosticoAsArr(dia.horas.wc),
    day: ecPronosticoAsArr(dia.horas.day),
    pop: ecPronosticoAsArr(dia.horas.pop)
  };
  var chipPp = ecPronosticoNum(ecPronosticoVarsMap(dia.variables), "pp");
  var chipZero = chipPp != null && chipPp <= 0;
  dia = { fecha: dia.fecha, horas: horas, horas_caption: dia.horas_caption, variables: dia.variables };
  var items = "";
  var now = Date.now();
  var i;
  for (i = 0; i < horas.ts.length; i++) {
    var ms = ecPronosticoTsMs(horas.ts[i]);
    var pastCls = (ms != null && ms < now) ? " is-past" : "";
    var wc = horas.wc && horas.wc[i] != null ? horas.wc[i] : null;
    var isNight = ecPronosticoHourIsNight(horas.ts[i], horas.day ? horas.day[i] : null);
    var cond = wc != null ? ecPronosticoHourCondition(wc, isNight) : null;
    cond = ecPronosticoHoraPpCondition(cond, horas.pp[i], isNight, horas.pop ? horas.pop[i] : null);
    if (chipZero && cond && ecPronosticoIsPrecipComposite(cond.composite)) {
      cond = ecPronosticoWmoCondition(3);
    }
    var icon = cond ? ecPronosticoConditionIconHtml(cond) : "";
    var tTxt = ecPronosticoFmtValor(horas.t[i]);
    var ppTxt = ecPronosticoFmtValor(horas.pp[i]);
    var popN = horas.pop ? ecPronosticoNumAt(horas.pop[i]) : null;
    var mmCell = ecPronosticoHoraMmText(horas.pp[i]);
    var popCell = ecPronosticoHoraPopText(horas.pop ? horas.pop[i] : null);
    var uTxt = ecPronosticoFmtValor(horas.u[i]);
    var tip = ecPronosticoHoraLabel(horas.ts[i]);
    if (tTxt != null) tip += " \u00b7 " + tTxt + " \u00b0C";
    if (ppTxt != null) tip += " \u00b7 " + ppTxt + " mm";
    if (popN != null) tip += " \u00b7 " + Math.round(popN) + " %";
    items +=
      '<li class="ec-pronostico-hora-item' + pastCls + '" title="' + ecPronosticoEsc(tip) + '">' +
      '<span class="ec-pronostico-hora-time">' + ecPronosticoEsc(ecPronosticoHoraLabel(horas.ts[i])) + "</span>" +
      '<span class="ec-pronostico-hora-icon">' + icon + "</span>" +
      '<span class="ec-pronostico-hora-t">' + (tTxt == null ? "\u2014" : tTxt + "\u00b0") + "</span>" +
      '<span class="ec-pronostico-hora-pp">' + ecPronosticoEsc(mmCell) + "</span>" +
      '<span class="ec-pronostico-hora-pop">' + ecPronosticoEsc(popCell) + "</span>" +
      '<span class="ec-pronostico-hora-u">' + (uTxt == null ? "\u2014" : uTxt + " km/h") + "</span>" +
      "</li>";
  }
  return (
    '<div class="ec-pronostico-hora-scroll">' +
    '<div class="ec-pronostico-hora-head">' +
    "<span>Hora</span>" +
    "<span>Cielo</span>" +
    "<span>Temperatura</span>" +
    "<span>mm</span>" +
    "<span>%</span>" +
    "<span>Viento</span>" +
    "</div>" +
    '<ol class="ec-pronostico-hora-list">' + items + "</ol>" +
    "</div>"
  );
}

function ecPronosticoHoraOnly() {
  var st = _ecPronosticoHorasState;
  if (st && (st.only === "pp" || st.only === "u")) return st.only;
  return "t";
}

function ecPronosticoHoraTitle(dia) {
  var hdr = ecPronosticoFormatDayHeader(dia && dia.fecha);
  if (!hdr.weekday) return "Hora a hora";
  return hdr.weekday + " " + hdr.dateLine + ", hora a hora";
}

function ecPronosticoHoraTogglesHtml() {
  var only = ecPronosticoHoraOnly();
  function chip(id, label) {
    var on = only === id ? " is-on" : "";
    return '<button type="button" class="ec-pronostico-hora-chip' + on + '" data-only="' + id + '">' + label + "</button>";
  }
  return (
    '<div class="ec-pronostico-hora-toggles">' +
    chip("t", "Temperatura") +
    chip("pp", "Precipitaci\u00f3n") +
    chip("u", "Viento") +
    "</div>"
  );
}

var EC_PRONOSTICO_HORA_CAPTION =
  "El total del d\u00eda est\u00e1 corregido con la historia de esta estaci\u00f3n; las horas muestran el ritmo del modelo, alineado a ese total.";

function ecPronosticoHoraHostHtml(opts) {
  opts = opts || {};
  var dia = opts.dia || null;
  var horas = dia && dia.horas;
  var inSlide = !!opts.inSlide;
  var cls = "ec-pronostico-hora-host" + (inSlide ? " ec-pronostico-hora-host--in-slide" : "");
  var spark = horas ? ecPronosticoSparkSvg(horas, { only: ecPronosticoHoraOnly() }) : "";
  var chart = horas ? ecPronosticoSparkSvg(horas, { wind: true }) : "";
  var list = dia ? ecPronosticoHoraListHtml(dia) : "";
  var capHidden = dia && dia.horas_caption ? "" : " hidden";
  return (
    '<div class="' + cls + '"' + (inSlide ? "" : " hidden") + ">" +
    '<div class="ec-pronostico-hora-head-row">' +
    '<b class="ec-pronostico-hora-day-title">' + ecPronosticoEsc(ecPronosticoHoraTitle(dia)) + "</b>" +
    ecPronosticoHoraTogglesHtml() +
    "</div>" +
    '<div class="ec-pronostico-spark-wrap">' + spark + "</div>" +
    '<button type="button" class="ec-pronostico-hora-btn" aria-expanded="false">Ver detalle hora a hora \u2192</button>' +
    '<div class="ec-pronostico-hora-panel" hidden>' +
    '<div class="ec-pronostico-hora-panel-head">' +
    '<span class="ec-pronostico-hora-panel-title">C\u00f3mo evoluciona el d\u00eda</span>' +
    '<button type="button" class="ec-pronostico-hora-close" aria-label="Cerrar">' +
    '<i class="fa fa-times" aria-hidden="true"></i></button>' +
    "</div>" +
    '<p class="ec-pronostico-hora-caption"' + capHidden + ">" + EC_PRONOSTICO_HORA_CAPTION + "</p>" +
    '<div class="ec-pronostico-hora-chart">' + chart + "</div>" +
    ecPronosticoHoraLegendHtml() +
    '<div class="ec-pronostico-hora-list-mount">' + list + "</div>" +
    "</div></div>"
  );
}

function ecPronosticoPaintHoraSpark(host, dia) {
  if (!host || !dia || !dia.horas) return;
  var title = host.querySelector(".ec-pronostico-hora-day-title");
  if (title) title.textContent = ecPronosticoHoraTitle(dia);
  var sparkWrap = host.querySelector(".ec-pronostico-spark-wrap");
  if (sparkWrap) sparkWrap.innerHTML = ecPronosticoSparkSvg(dia.horas, { only: ecPronosticoHoraOnly() });
}

function ecPronosticoRefreshHoraSeries(mount) {
  var st = _ecPronosticoHorasState;
  if (!mount || !st) return;
  var only = ecPronosticoHoraOnly();
  var chips = mount.querySelectorAll(".ec-pronostico-hora-chip");
  var i;
  for (i = 0; i < chips.length; i++) {
    chips[i].classList.toggle("is-on", chips[i].getAttribute("data-only") === only);
  }
  var dias = (st.payload && st.payload.dias) || [];
  var hosts = mount.querySelectorAll(".ec-pronostico-hora-host");
  for (i = 0; i < hosts.length; i++) {
    var host = hosts[i];
    var dia = null;
    var off;
    if (host.classList.contains("ec-pronostico-hora-host--in-slide")) {
      var slide = host.closest(".ec-pronostico-slide");
      off = slide ? parseInt(slide.getAttribute("data-day-offset"), 10) : NaN;
    } else {
      var sel = mount.querySelector(".ec-pronostico-slide.is-selected");
      off = sel ? parseInt(sel.getAttribute("data-day-offset"), 10) : 0;
    }
    if (isFinite(off)) dia = dias[off];
    ecPronosticoPaintHoraSpark(host, dia);
  }
}

function ecPronosticoSyncHoras(activeIndex) {
  var st = _ecPronosticoHorasState;
  if (!st || !st.mount) return;
  var host = st.mount.querySelector(".ec-pronostico-hora-host:not(.ec-pronostico-hora-host--in-slide)");
  if (!host) return;
  var dias = (st.payload && st.payload.dias) || [];
  var dia = dias[activeIndex];
  var show = !!(dia && ecPronosticoHasHoras(dia) && ecPronosticoInHorasHorizon(dia.fecha, st.todayIso));
  host.hidden = !show;
  if (!show) {
    var panelOff = host.querySelector(".ec-pronostico-hora-panel");
    if (panelOff) panelOff.hidden = true;
    var btnOff = host.querySelector(".ec-pronostico-hora-btn");
    if (btnOff) btnOff.setAttribute("aria-expanded", "false");
    return;
  }
  ecPronosticoPaintHoraSpark(host, dia);
  var caption = host.querySelector(".ec-pronostico-hora-caption");
  if (caption) caption.hidden = !dia.horas_caption;
  var chart = host.querySelector(".ec-pronostico-hora-chart");
  if (chart) chart.innerHTML = ecPronosticoSparkSvg(dia.horas, { wind: true });
  var listMount = host.querySelector(".ec-pronostico-hora-list-mount");
  if (listMount) listMount.innerHTML = ecPronosticoHoraListHtml(dia);
}

function ecPronosticoBindHoraMount(mount) {
  if (!mount || mount.getAttribute("data-ec-hora") === "1") return;
  mount.setAttribute("data-ec-hora", "1");
  mount.addEventListener("click", function (ev) {
    if (!ev.target.closest) return;
    var chip = ev.target.closest(".ec-pronostico-hora-chip");
    if (chip && mount.contains(chip)) {
      var only = chip.getAttribute("data-only");
      if (_ecPronosticoHorasState && (only === "t" || only === "pp" || only === "u")) {
        _ecPronosticoHorasState.only = only;
      }
      ecPronosticoRefreshHoraSeries(mount);
      return;
    }
    var btn = ev.target.closest(".ec-pronostico-hora-btn");
    if (btn && mount.contains(btn)) {
      var host = btn.closest(".ec-pronostico-hora-host");
      var panel = host && host.querySelector(".ec-pronostico-hora-panel");
      if (panel) {
        panel.hidden = !panel.hidden;
        btn.setAttribute("aria-expanded", panel.hidden ? "false" : "true");
      }
      return;
    }
    var closeBtn = ev.target.closest(".ec-pronostico-hora-close");
    if (closeBtn && mount.contains(closeBtn)) {
      var host2 = closeBtn.closest(".ec-pronostico-hora-host");
      var panel2 = host2 && host2.querySelector(".ec-pronostico-hora-panel");
      if (panel2) panel2.hidden = true;
      var b2 = host2 && host2.querySelector(".ec-pronostico-hora-btn");
      if (b2) b2.setAttribute("aria-expanded", "false");
    }
  });
}

function ecPronosticoSantiagoParts(d) {
  var parts = new Intl.DateTimeFormat("en-CA", {
    timeZone: "America/Santiago",
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    hourCycle: "h23"
  }).formatToParts(d);
  var o = { year: "", month: "", day: "", hour: "", minute: "" };
  var i;
  for (i = 0; i < parts.length; i++) {
    if (parts[i].type !== "literal") o[parts[i].type] = parts[i].value;
  }
  if (o.hour.length < 2) o.hour = "0" + o.hour;
  if (o.minute.length < 2) o.minute = "0" + o.minute;
  return o;
}

function ecPronosticoParseTs(iso) {
  if (iso == null || iso === "") return null;
  var s = String(iso).trim();
  if (/^\d{4}-\d{2}-\d{2} \d{2}:\d{2}/.test(s)) {
    s = s.replace(" ", "T").replace(/\.\d+/, "");
    if (!/[zZ]|[+-]\d{2}/.test(s)) s += "Z";
  } else if (/^\d{4}-\d{2}-\d{2}T/.test(s) && !/[zZ]|[+-]\d{2}/.test(s)) {
    s = s.replace(/\.\d+/, "") + "Z";
  }
  var d = new Date(s);
  return isNaN(d.getTime()) ? null : d;
}

function ecPronosticoFmtActualizado(iso, now) {
  var d = ecPronosticoParseTs(iso);
  if (!d) return "";
  now = now || new Date();
  var p = ecPronosticoSantiagoParts(d);
  var n = ecPronosticoSantiagoParts(now);
  var hhmm = p.hour + ":" + p.minute;
  var day = p.year + "-" + p.month + "-" + p.day;
  var today = n.year + "-" + n.month + "-" + n.day;
  var when;
  if (day === today) when = "hoy, " + hhmm;
  else if (day === ecPronosticoAddDaysIso(today, -1)) when = "ayer, " + hhmm;
  else {
    var label = new Intl.DateTimeFormat("es-CL", {
      timeZone: "America/Santiago",
      weekday: "short",
      day: "numeric",
      month: "short"
    }).format(d).replace(/\./g, "");
    when = label + ", " + hhmm;
  }
  return when + " (hora de Chile)";
}

function ecBuildPronosticoCarouselHtml(mountId, payload) {
  var meta = payload.meta || {};
  var todayIso = ecPronosticoTodayIso();
  var dias = ecPronosticoDiasDesdeHoy(payload.dias || [], todayIso);
  var slides = dias.map(function (d, i) {
    return ecRenderPronosticoSlide(d, todayIso, i);
  }).join("");
  var staleBadge = meta.stale
    ? '<span class="ec-rp-pronostico-stale">Datos desactualizados</span>'
    : "";
  var fetched = meta.fetched_at
    ? " \u00b7 Actualizado " + ecPronosticoEsc(ecPronosticoFmtActualizado(meta.fetched_at))
    : "";

  return (
    '<div class="ec-rp-pronostico-swiper-wrap">' +
    '<div class="swiper ec-pronostico-swiper" id="' + ecPronosticoEsc(mountId) + '_swiper">' +
    '<div class="swiper-wrapper">' + slides + "</div>" +
    "</div>" +
    '<button type="button" class="ec-pronostico-nav ec-pronostico-nav-prev" aria-label="D\u00eda anterior">' +
    '<i class="fa fa-chevron-left" aria-hidden="true"></i></button>' +
    '<button type="button" class="ec-pronostico-nav ec-pronostico-nav-next" aria-label="D\u00eda siguiente">' +
    '<i class="fa fa-chevron-right" aria-hidden="true"></i></button>' +
    "</div>" +
    ecPronosticoHoraHostHtml() +
    '<p class="ec-pronostico-attrib ec-rp-pronostico-attrib">' +
    ecPronosticoEsc(meta.atribucion || "Open-Meteo CC BY 4.0") +
    fetched +
    staleBadge +
    "</p>"
  );
}

function ecInitPronosticoCarousel(mountId, payload, sectionId) {
  ecDestroyPronosticoCarousel();
  var sanitized = ecSanitizePronosticoPayload(payload);
  var section = sectionId ? document.getElementById(sectionId) : null;
  var mount = document.getElementById(mountId);
  if (sanitized) {
    sanitized = Object.assign({}, sanitized, {
      dias: ecPronosticoDiasDesdeHoy(sanitized.dias, ecPronosticoTodayIso())
    });
  }
  if (!sanitized || !sanitized.dias.length || !mount) {
    if (section) section.hidden = true;
    return Promise.resolve();
  }

  mount.innerHTML = ecBuildPronosticoCarouselHtml(mountId, sanitized);
  if (section) section.hidden = false;
  ecPronosticoUpdateKicker(
    sectionId,
    (sanitized.meta || {}).horizonte_dias,
    !!(sanitized.meta || {}).historia_limitada
  );
  _ecPronosticoHorasState = {
    payload: sanitized,
    mount: mount,
    todayIso: ecPronosticoTodayIso(),
    only: "t"
  };
  ecPronosticoBindHoraMount(mount);

  var swiperEl = document.getElementById(mountId + "_swiper");
  if (!swiperEl || typeof Swiper === "undefined") {
    ecPronosticoSelectDia(0);
    return Promise.resolve();
  }

  ecBindPronosticoAxis();
  ecMountPronosticoSwiper(swiperEl);
  return Promise.resolve();
}

function ecPronosticoApiFetch(path) {
  if (typeof rodajaApiFetch === "function") {
    return rodajaApiFetch(path).then(function (r) {
      if (!r.ok) throw new Error("HTTP " + r.status);
      return r.json();
    });
  }
  return fetch("/api" + path, { headers: { Accept: "application/json" } }).then(function (r) {
    if (!r.ok) throw new Error("HTTP " + r.status);
    return r.json();
  });
}

function ecLoadPronosticoCarousel(eid, mountId, sectionId) {
  return ecMountPronosticoCarousel(eid, mountId, sectionId);
}

function ecBuildPronosticoCarouselShell(sectionId, mountId, detailHref) {
  var linkHtml = detailHref
    ? '<a class="ec-rp-pronostico-detail" href="' + ecPronosticoEsc(detailHref) + '">Ver el pron\u00f3stico \u2192</a>'
    : "";
  return (
    '<div class="ec-rp-pronostico-section ec-rp-pronostico-loading" id="' + ecPronosticoEsc(sectionId) + '">' +
    '<div class="widget ec-rp-pronostico-card">' +
    '<div class="widget-header ec-rp-pronostico-header">' +
    '<div class="ec-rp-pronostico-titles">' +
    '<span class="widget-title ec-rp-pronostico-kicker">Pron\u00f3stico de la estaci\u00f3n</span>' +
    '<span class="ec-rp-pronostico-sub" title="Resoluci\u00f3n ~10 km">C\u00f3mo evoluciona el d\u00eda</span>' +
    "</div>" +
    linkHtml +
    "</div>" +
    '<div class="ec-rp-pronostico-mount" id="' + ecPronosticoEsc(mountId) + '">' +
    ecPronosticoSkeletonHtml() +
    "</div></div></div>"
  );
}

/* ponytail: franjas noche/día; falla si el eje deja de nombrarlas */
(function ecPronosticoNightDaySelfCheck() {
  if (!ecPronosticoIsNightHour(4) || ecPronosticoIsNightHour(12) || !ecPronosticoIsNightHour(21)) {
    throw new Error("ecPronosticoIsNightHour");
  }
  if (ecPronosticoIsNightHour(null)) throw new Error("ecPronosticoIsNightHour null");
  function localIso(hour) {
    var d = new Date(2026, 8, 24, hour, 0, 0);
    var off = -d.getTimezoneOffset();
    var sign = off >= 0 ? "+" : "-";
    var abs = Math.abs(off);
    var p = function (n) { return String(n).padStart(2, "0"); };
    return "2026-09-24T" + p(hour) + ":00:00" + sign + p(Math.floor(abs / 60)) + ":" + p(abs % 60);
  }
  var horas = { ts: [], t: [], pp: [], u: [] };
  var hour;
  for (hour = 0; hour < 24; hour++) {
    horas.ts.push(localIso(hour));
    horas.t.push(10);
    horas.pp.push(0);
    horas.u.push(5);
  }
  var svg = ecPronosticoSparkSvg(horas, { wind: false });
  if (svg.indexOf(">noche<") < 0 || svg.indexOf(">d\u00eda<") < 0 || svg.indexOf("ec-pronostico-spark-axis") < 0) {
    throw new Error("ecPronosticoSparkSvg sin franja noche/d\u00eda");
  }
})();

/* ponytail: self-check WMO ? composite; falla en consola si se rompe el mapeo */
(function ecPronosticoWmoSelfCheck() {
  var cases = [
    [0, "soleado", "Soleado"], [1, "parcial-soleado", "Mayormente soleado"], [3, "nublado", "Nublado"],
    [51, "llovizna", "Llovizna ligera"], [61, "lluvia", "Lluvia ligera"],
    [63, "lluvia", "Lluvia moderada"], [65, "lluvia-fuerte", "Lluvia fuerte"],
    [80, "lluvia-y-sol", "Chubascos ligeros"], [81, "lluvia-fuerte", "Chubascos"],
    [82, "lluvia-fuerte", "Chubascos fuertes"], [95, "nube-truenos", "Tormenta"]
  ];
  for (var i = 0; i < cases.length; i++) {
    var got = ecPronosticoWmoCondition(cases[i][0]);
    if (!got || got.composite !== cases[i][1] || got.label !== cases[i][2]) {
      throw new Error("ecPronosticoWmoCondition(" + cases[i][0] + ") mismatch");
    }
  }
  if (ecPronosticoConditionForDay({}, {}).id !== "unknown") {
    throw new Error("ecPronosticoConditionForDay neutral fallback");
  }
  var ppOverride = ecPronosticoConditionForDay({}, {
    weather_code: { valor: 63 },
    pp: { valor: 57.4 }
  });
  if (!ppOverride || ppOverride.label !== "Lluvia moderada" || ppOverride.composite !== "lluvia") {
    throw new Error("ecPronosticoConditionForDay mm no pisa el codigo");
  }
  var ppZero = ecPronosticoConditionForDay({}, {
    weather_code: { valor: 51 },
    pp: { valor: 0 }
  });
  if (!ppZero || ppZero.label !== "Nublado" || ppZero.composite !== "nublado") {
    throw new Error("ecPronosticoConditionForDay pp 0 keeps drizzle");
  }
  function ecWxLocalIso(hour) {
    var d = new Date(2026, 9, 7, hour, 0, 0);
    var off = -d.getTimezoneOffset();
    var sign = off >= 0 ? "+" : "-";
    var abs = Math.abs(off);
    var p = function (n) { return String(n).padStart(2, "0"); };
    return "2026-10-07T" + p(hour) + ":00:00" + sign + p(Math.floor(abs / 60)) + ":" + p(abs % 60);
  }
  var tepualRaw = [0.1, 2, 2.5, 0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0.1, 0, 0, 0];
  var tepualWc = [51, 80, 81, 51, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 51, 3, 3, 51, 51, 2, 2, 3];
  var tepualSum = 0;
  var hi;
  for (hi = 0; hi < tepualRaw.length; hi++) tepualSum += tepualRaw[hi];
  var tepualChip = 3.19452054794521;
  var tepualPp = [];
  var tepualTs = [];
  for (hi = 0; hi < 24; hi++) {
    tepualPp.push(tepualRaw[hi] * tepualChip / tepualSum);
    tepualTs.push(ecWxLocalIso(hi));
  }
  var tepual = ecPronosticoConditionForDay({
    horas: { ts: tepualTs, pp: tepualPp, wc: tepualWc }
  }, { pp: { valor: tepualChip }, weather_code: { valor: 81 } });
  if (!tepual || tepual.label !== "Nublado" || tepual.composite !== "nublado") {
    throw new Error("ecPronosticoConditionForDay El Tepual: " + (tepual && tepual.label));
  }
  var dzTs = [];
  var dzPp = [];
  var dzWc = [];
  for (hi = 0; hi < 24; hi++) {
    dzTs.push(ecWxLocalIso(hi));
    dzPp.push(hi === 2 ? 1.2 : 0);
    dzWc.push(hi === 2 ? 53 : 3);
  }
  var drizzle = ecPronosticoConditionForDay({
    horas: { ts: dzTs, pp: dzPp, wc: dzWc }
  }, { pp: { valor: 1.2 } });
  if (!drizzle || drizzle.label !== "Nublado" || drizzle.composite !== "nublado") {
    throw new Error("lluvia de noche no cambia el dia: " + (drizzle && (drizzle.label + " " + drizzle.composite)));
  }
  function ecWxPack(hours) {
    var ts = [], pp = [], wc = [], day = [], pop = [];
    var h, row;
    for (h = 0; h < 24; h++) {
      row = hours[h] || {};
      ts.push(ecWxLocalIso(h));
      pp.push(row.pp == null ? 0 : row.pp);
      wc.push(row.wc == null ? 0 : row.wc);
      day.push(row.day == null ? (h >= 8 && h <= 19 ? 1 : 0) : row.day);
      pop.push(row.pop == null ? 0 : row.pop);
    }
    return { horas: { ts: ts, pp: pp, wc: wc, day: day, pop: pop } };
  }
  var qn = ecPronosticoConditionForDay(ecWxPack({
    14: { wc: 51, pop: 71 },
    15: { wc: 80, pop: 96 },
    16: { wc: 80, pop: 100 },
    17: { wc: 81, pop: 100 },
    18: { wc: 80, pop: 100 },
    19: { wc: 81, pop: 100 },
    20: { wc: 81, pop: 100, day: 0 },
    21: { wc: 80, pop: 100, day: 0 }
  }), { pp: { valor: 6 } });
  if (!qn || qn.label !== "Chubascos" || qn.composite !== "lluvia-fuerte") {
    throw new Error("Quinta Normal dia: " + (qn && (qn.label + " " + qn.composite)));
  }
  var qnOct20 = {};
  var qnH20;
  for (qnH20 = 8; qnH20 <= 15; qnH20++) qnOct20[qnH20] = { wc: 51, pp: 0.2, pop: 80 };
  qnOct20[7] = { wc: 53, pp: 0.7, pop: 64, day: 1 };
  qnOct20[16] = { wc: 80, pp: 1.4, pop: 100 };
  qnOct20[17] = { wc: 81, pp: 3.1, pop: 100 };
  qnOct20[18] = { wc: 80, pp: 1.4, pop: 100 };
  qnOct20[19] = { wc: 81, pp: 2.6, pop: 100 };
  qnOct20[21] = { wc: 81, pp: 3.2, pop: 100, day: 0 };
  var qnOct20Dia = ecWxPack(qnOct20);
  qnOct20Dia.weather_code = 81;
  var qnOct20Day = ecPronosticoConditionForDay(qnOct20Dia, {
    pp: { valor: 20.4 },
    weather_code: { valor: 81 }
  });
  if (!qnOct20Day || qnOct20Day.label !== "Chubascos" || qnOct20Day.composite !== "lluvia-fuerte") {
    throw new Error("Quinta Normal 2026-10-20: " + (qnOct20Day && (qnOct20Day.label + " " + qnOct20Day.composite)));
  }
  var qnHoy = {
    7: { wc: 51, pp: 0.21, pop: 79, day: 1 },
    8: { wc: 51, pp: 0.21, pop: 74 },
    9: { wc: 51, pp: 0.1, pop: 71 },
    10: { wc: 51, pp: 0.31, pop: 76 },
    11: { wc: 51, pp: 0.31, pop: 85 },
    12: { wc: 51, pp: 0.42, pop: 92 },
    13: { wc: 51, pp: 0.42, pop: 95 },
    14: { wc: 53, pp: 0.52, pop: 97 },
    15: { wc: 53, pp: 0.52, pop: 98 },
    16: { wc: 80, pp: 1.36, pop: 99 },
    17: { wc: 81, pp: 3.86, pop: 100 },
    18: { wc: 81, pp: 4.7, pop: 100 },
    19: { wc: 80, pp: 2.19, pop: 100 },
    20: { wc: 81, pp: 6.05, pop: 100, day: 0 }
  };
  var qnHoyDia = ecWxPack(qnHoy);
  qnHoyDia.weather_code = 81;
  var qnHoyDay = ecPronosticoConditionForDay(qnHoyDia, {
    pp: { valor: 33.5 },
    weather_code: { valor: 81 }
  });
  if (!qnHoyDay || qnHoyDay.label !== "Chubascos" || qnHoyDay.composite !== "lluvia-fuerte") {
    throw new Error("Quinta Normal 33 mm empate 53/81: " + (qnHoyDay && (qnHoyDay.label + " " + qnHoyDay.composite)));
  }
  var solo51 = {};
  for (qnH20 = 8; qnH20 <= 15; qnH20++) solo51[qnH20] = { wc: 51, pp: 0.2, pop: 80 };
  var solo51Dia = ecWxPack(solo51);
  solo51Dia.weather_code = 81;
  var solo51Day = ecPronosticoConditionForDay(solo51Dia, {
    pp: { valor: 20 },
    weather_code: { valor: 81 }
  });
  if (!solo51Day || solo51Day.label !== "Chubascos") {
    throw new Error("codigo diario mas severo: " + (solo51Day && solo51Day.label));
  }
  var mmFuerte = {};
  for (qnH20 = 8; qnH20 <= 15; qnH20++) mmFuerte[qnH20] = { wc: 51, pp: 0.1, pop: 80 };
  mmFuerte[16] = { wc: 65, pp: 12, pop: 100 };
  var mmFuerteDay = ecPronosticoConditionForDay(ecWxPack(mmFuerte), { pp: { valor: 12.8 } });
  if (!mmFuerteDay || mmFuerteDay.label !== "Lluvia fuerte") {
    throw new Error("mm diurnos de la banda fuerte: " + (mmFuerteDay && mmFuerteDay.label));
  }
  var qnOct8Hours = {};
  var qnH;
  for (qnH = 8; qnH <= 12; qnH++) qnOct8Hours[qnH] = { wc: qnH < 11 ? 0 : 1, pp: 0, pop: 10 };
  for (qnH = 13; qnH <= 19; qnH++) qnOct8Hours[qnH] = { wc: qnH % 2 ? 80 : 81, pp: 0.2, pop: 40 };
  var qnOct8Dia = ecWxPack(qnOct8Hours);
  qnOct8Dia.weather_code = 81;
  var qnOct8 = ecPronosticoConditionForDay(qnOct8Dia, {
    pp: { valor: 19.2 },
    weather_code: { valor: 81 }
  });
  if (!qnOct8 || ecPronosticoIsSunComposite(qnOct8.composite)) {
    throw new Error("Quinta Normal 2026-10-08 sol: " + (qnOct8 && (qnOct8.label + " " + qnOct8.composite)));
  }
  var qnShort = ecWxPack({
    15: { wc: 80, pp: 0.2, pop: 20 },
    16: { wc: 81, pp: 0.2, pop: 20 }
  });
  qnShort.weather_code = 81;
  var qnShortDay = ecPronosticoConditionForDay(qnShort, {
    pp: { valor: 19.2 },
    weather_code: { valor: 81 }
  });
  if (!qnShortDay || ecPronosticoIsSunComposite(qnShortDay.composite) || qnShortDay.label !== "Chubascos") {
    throw new Error("codigo diario 81 con chip alto: " + (qnShortDay && (qnShortDay.label + " " + qnShortDay.composite)));
  }
  var tres = ecPronosticoConditionForDay(ecWxPack({
    15: { wc: 80, pop: 90 },
    16: { wc: 80, pop: 90 },
    17: { wc: 80, pop: 90 }
  }), { pp: { valor: 2 } });
  if (!tres || tres.composite !== "soleado") {
    throw new Error("3 horas no bastan: " + (tres && tres.composite));
  }
  var cuatro = ecPronosticoConditionForDay(ecWxPack({
    12: { wc: 0, pop: 50 },
    13: { wc: 0, pop: 50 },
    14: { wc: 0, pop: 50 },
    15: { wc: 0, pop: 50 }
  }), { pp: { valor: 1 } });
  if (!cuatro || cuatro.label !== "Probable lluvia" || cuatro.composite !== "lluvia-y-sol") {
    throw new Error("4 horas cielo claro: " + (cuatro && (cuatro.label + " " + cuatro.composite)));
  }
  var cub = {};
  for (hi = 8; hi <= 19; hi++) cub[hi] = { wc: 3, pop: 60 };
  var cubierto = ecPronosticoConditionForDay(ecWxPack(cub), { pp: { valor: 2 } });
  if (!cubierto || cubierto.label !== "Probable lluvia" || cubierto.composite !== "lluvia") {
    throw new Error("4 horas nublado: " + (cubierto && (cubierto.label + " " + cubierto.composite)));
  }
  var noche = ecPronosticoConditionForDay(ecWxPack({
    0: { wc: 80, pop: 100, day: 0 },
    1: { wc: 81, pop: 100, day: 0 },
    2: { wc: 65, pop: 100, day: 0, pp: 4 },
    3: { wc: 81, pop: 100, day: 0 }
  }), { pp: { valor: 8 } });
  if (!noche || noche.composite !== "soleado") {
    throw new Error("noche no pinta el dia: " + (noche && noche.composite));
  }
  var cero = ecPronosticoConditionForDay(ecWxPack({
    14: { wc: 80, pop: 100 },
    15: { wc: 80, pop: 100 },
    16: { wc: 80, pop: 100 },
    17: { wc: 81, pop: 100 }
  }), { pp: { valor: 0 } });
  if (!cero || cero.composite !== "nublado" || cero.label !== "Nublado") {
    throw new Error("chip 0 a lo sumo nublado: " + (cero && cero.composite));
  }
  var nieve = ecPronosticoConditionForDay(ecWxPack({
    12: { wc: 71, pop: 80 },
    13: { wc: 73, pop: 80 },
    14: { wc: 71, pop: 80 },
    15: { wc: 71, pop: 80 }
  }), { pp: { valor: 4 } });
  if (!nieve || nieve.label !== "Nieve ligera" || nieve.composite !== "nieve") {
    throw new Error("pop de nieve: " + (nieve && (nieve.label + " " + nieve.composite)));
  }
  var torm = ecPronosticoConditionForDay(ecWxPack({
    12: { wc: 95, pp: 2, pop: 90 },
    13: { wc: 3, pop: 90 },
    14: { wc: 3, pop: 90 },
    15: { wc: 3, pop: 90 },
    16: { wc: 3, pop: 90 }
  }), { pp: { valor: 5 } });
  if (!torm || torm.label !== "Tormenta" || torm.composite !== "nube-truenos") {
    throw new Error("tormenta gana a la probabilidad: " + (torm && torm.label));
  }
  var heavyTs = [];
  var heavyPp = [];
  var heavyWc = [];
  for (hi = 0; hi < 24; hi++) {
    heavyTs.push(ecWxLocalIso(hi));
    heavyPp.push(hi === 10 || hi === 11 ? 4 : 0);
    heavyWc.push(hi === 10 || hi === 11 ? 65 : 1);
  }
  var heavy = ecPronosticoConditionForDay({
    horas: { ts: heavyTs, pp: heavyPp, wc: heavyWc }
  }, { pp: { valor: 8 } });
  if (!heavy || heavy.label !== "Lluvia fuerte" || heavy.composite !== "lluvia-fuerte") {
    throw new Error("ecPronosticoConditionForDay heavy: " + (heavy && heavy.label));
  }
  var night1 = ecPronosticoHourCondition(1, true);
  if (!night1 || night1.composite === "parcial-soleado" || night1.composite !== "luna-parcial") {
    throw new Error("ecPronosticoHourCondition night sun");
  }
  if (ecPronosticoHourCondition(1, false).composite !== "parcial-soleado") {
    throw new Error("ecPronosticoHourCondition day sun");
  }
  if (ecPronosticoHoraMmText(0.1).indexOf("mm") < 0) throw new Error("hora mm bajo 1");
  if (ecPronosticoHoraPopText(35).indexOf("35") < 0 || ecPronosticoHoraPopText(35).indexOf("%") < 0) {
    throw new Error("hora pop 35");
  }
  if (ecPronosticoHoraPopText(24) !== "24 %") throw new Error("hora pop 24");
  if (ecPronosticoHoraPopText(null) !== "") throw new Error("hora pop null");
  if (ecPronosticoHoraMmText(1.5).indexOf("mm") < 0) throw new Error("hora mm 1.5");
  if (ecPronosticoHoraMmText(null) !== "") throw new Error("hora mm null");
  var horaHead = ecPronosticoHoraListHtml({
    fecha: "2026-10-08",
    horas: { ts: ["2026-10-08T05:00:00-03:00"], t: [8.5], pp: [0], u: [6], wc: [0], pop: [2] }
  });
  if (horaHead.indexOf("ec-pronostico-hora-head") < 0 ||
      horaHead.indexOf(">Hora<") < 0 || horaHead.indexOf(">Cielo<") < 0 ||
      horaHead.indexOf(">Temperatura<") < 0 || horaHead.indexOf(">mm<") < 0 ||
      horaHead.indexOf(">%</span>") < 0 || horaHead.indexOf(">Viento<") < 0) {
    throw new Error("ecPronosticoHoraListHtml sin encabezados");
  }
  var both = ecPronosticoHoraListHtml({
    fecha: "2026-10-08",
    horas: { ts: ["2026-10-08T15:00:00-03:00"], t: [13.4], pp: [0.4], u: [5], wc: [3], pop: [100] }
  });
  if (both.indexOf("ec-pronostico-hora-pp") < 0 || both.indexOf("ec-pronostico-hora-pop") < 0 ||
      both.indexOf("100 %") < 0 || both.indexOf(" mm") < 0) {
    throw new Error("hora mm y % en la misma fila");
  }
  function horaComposite(code, night, pp, pop) {
    var row = ecPronosticoHoraPpCondition(ecPronosticoHourCondition(code, night), pp, night, pop);
    return row && row.composite;
  }
  if (horaComposite(80, false, 0.6, 96) !== "lluvia-y-sol") throw new Error("hora pop Quinta Normal 15:00");
  if (horaComposite(51, false, 0.1, 71) !== "llovizna") throw new Error("hora pop llovizna");
  if (horaComposite(0, false, 0, 53) !== "lluvia-y-sol") throw new Error("hora pop sol");
  if (horaComposite(0, true, 0, 53) !== "lluvia-y-luna") throw new Error("hora pop luna");
  if (horaComposite(3, false, null, 80) !== "lluvia") throw new Error("hora pop sin pp");
  if (horaComposite(80, false, 0.4, 50) !== "lluvia-y-sol") throw new Error("hora pop 50");
  if (horaComposite(0, false, 0, 50) !== "lluvia-y-sol") throw new Error("hora pop sol 50");
  if (horaComposite(80, false, 0.4, 49) !== "lluvia-y-sol") throw new Error("hora pop 49");
  if (horaComposite(80, false, 0.4) !== "lluvia-y-sol") throw new Error("hora pp code 80");
  if (horaComposite(63, false, 0.4) !== "lluvia") throw new Error("hora pp code 63");
  if (horaComposite(65, false, 0.4) !== "lluvia-fuerte") throw new Error("hora pp code 65");
  if (horaComposite(95, false, 0) !== "nube-truenos") throw new Error("hora pp code 95");
  if (horaComposite(71, false, 0.2) !== "nieve") throw new Error("hora pp code 71");
  if (horaComposite(0, false, 1.2) !== "soleado") throw new Error("hora mm no pisa codigo 0");
  if (horaComposite(3, false, 1.2) !== "nublado") throw new Error("hora mm no pisa codigo 3");
  if (horaComposite(2, true, 1.2) !== "luna-nubes") throw new Error("hora mm no pisa codigo 2");
  var chip0hora = ecPronosticoHoraListHtml({
    fecha: "2026-10-09",
    variables: [{ nombre_abr: "pp", valor: 0 }],
    horas: { ts: ["2026-10-09T15:00:00-03:00", "2026-10-09T16:00:00-03:00"], t: [12, 13], pp: [0.2, 0], u: [1, 1], wc: [51, 0], pop: [10, 0] }
  });
  if (chip0hora.indexOf("/llovizna.svg") >= 0 || chip0hora.indexOf("/nublado.svg") < 0 ||
      chip0hora.indexOf("/soleado.svg") < 0) {
    throw new Error("chip 0: hora 51 nublada y hora 0 soleada");
  }
  if (horaComposite(0, false, 0) !== "soleado" || ecPronosticoHoraPopText(40) !== "40 %" ||
      ecPronosticoHoraMmText(0).indexOf("mm") < 0) {
    throw new Error("hora pp sol con pop");
  }
  if (ecPronosticoDayPopMax({ horas: { pop: [10, 40, 96] } }) !== 96) {
    throw new Error("pop diario es el maximo");
  }
  if (ecPronosticoDayPopMax({ horas: { pop: [null, null] } }) !== null) {
    throw new Error("pop diario sin datos");
  }
  if (ecPronosticoDayPopMax({}) !== null) throw new Error("pop diario sin horas");
  var popDia = ecPronosticoPopPeriodMax({
    horas: { pop: [10, 96, 4, 40], day: [1, 1, 0, 0] }
  }, false);
  if (!popDia.known || popDia.max !== 96) throw new Error("pop diurna");
  var popNoche = ecPronosticoPopPeriodMax({
    horas: { pop: [10, 96, 4, 40], day: [1, 1, 0, 0] }
  }, true);
  if (!popNoche.known || popNoche.max !== 40) throw new Error("pop nocturna");
  var popHora = ecPronosticoPopPeriodMax({
    horas: {
      pop: [70, 5],
      ts: ["2026-10-12T03:00:00-03:00", "2026-10-12T15:00:00-03:00"]
    }
  }, true);
  if (!popHora.known || popHora.max !== 70) throw new Error("pop nocturna por hora");
  if (ecPronosticoPopPeriodMax({ horas: { pop: [10] } }, false).known) {
    throw new Error("pop sin dia ni hora no se parte");
  }
  var slideSplit = ecRenderPronosticoSlide({
    fecha: "2026-10-12",
    variables: [],
    horas: { pop: [10, 96, 40], day: [1, 1, 0] }
  }, "2026-10-08", 4);
  if (slideSplit.indexOf("Diurna") < 0 || slideSplit.indexOf("Nocturna") < 0 ||
      slideSplit.indexOf("96 %") < 0 || slideSplit.indexOf("40 %") < 0 ||
      slideSplit.indexOf("Prob. de lluvia") < 0 ||
      slideSplit.indexOf("Probabilidad de lluvia diurna 96 %") < 0 ||
      slideSplit.indexOf("Probabilidad de lluvia nocturna 40 %") < 0) {
    throw new Error("tarjeta sin prob diurna y nocturna");
  }
  var slideRange = ecRenderPronosticoSlide({
    fecha: "2026-10-09",
    variables: [
      { nombre_abr: "t_max", valor: 19.5, unidad: "\u00b0C" },
      { nombre_abr: "t_min", valor: 7.1, unidad: "\u00b0C" },
      { nombre_abr: "pp", valor: 0.2, unidad: "mm" }
    ]
  }, "2026-10-09", 0);
  if (slideRange.indexOf("ec-pronostico-temp-range") < 0 ||
      slideRange.indexOf("ec-pronostico-metric--t_max") < 0 ||
      slideRange.indexOf("ec-pronostico-metric--t_min") < 0 ||
      slideRange.indexOf("19,5") < 0 || slideRange.indexOf("7,1") < 0) {
    throw new Error("rango max/min");
  }
  if (slideRange.indexOf("Temp. m\u00edn") >= 0) throw new Error("min sigue en filas");
  var fmtHoy = ecPronosticoFmtActualizado("2026-10-09T12:12:33Z", new Date("2026-10-09T18:30:00Z"));
  if (fmtHoy !== "hoy, 09:12 (hora de Chile)") throw new Error("fecha chile: " + fmtHoy);
  var fmtOld = ecPronosticoFmtActualizado("2026-10-09 12:12:33.04258", new Date("2026-10-09T18:30:00Z"));
  if (fmtOld !== "hoy, 09:12 (hora de Chile)") throw new Error("fecha sin zona: " + fmtOld);
  var onlyT = ecPronosticoSparkSvg(
    { ts: ["2026-10-09T12:00:00Z", "2026-10-09T13:00:00Z"], t: [10, 12], pp: [1, 0], u: [3, 4] },
    { only: "t" }
  );
  if (onlyT.indexOf("ec-pronostico-spark-t") < 0 || onlyT.indexOf("ec-pronostico-spark-pp") >= 0 ||
      onlyT.indexOf("ec-pronostico-spark-u") >= 0) {
    throw new Error("spark only t");
  }
  var onlyPp = ecPronosticoSparkSvg(
    { ts: ["2026-10-09T12:00:00Z", "2026-10-09T13:00:00Z"], t: [10, 12], pp: [1, 2], u: [3, 4] },
    { only: "pp" }
  );
  if (onlyPp.indexOf("ec-pronostico-spark-pp") < 0 || onlyPp.indexOf("ec-pronostico-spark-t") >= 0) {
    throw new Error("spark only pp");
  }
  var hostHtml = ecPronosticoHoraHostHtml({
    dia: { fecha: "2026-10-09", horas: { ts: ["2026-10-09T15:00:00-03:00"], t: [10], pp: [0], u: [1], wc: [0], pop: [2] } }
  });
  if (hostHtml.indexOf("Ver detalle hora a hora") < 0 || hostHtml.indexOf("data-only=\"t\"") < 0 ||
      hostHtml.indexOf("hora a hora") < 0) {
    throw new Error("host horario");
  }
  var slidePop = ecRenderPronosticoSlide({
    fecha: "2026-10-12",
    variables: [],
    horas: { pop: [10, 96, 40] }
  }, "2026-10-08", 4);
  if (slidePop.indexOf("ec-pronostico-day-pop") < 0 || slidePop.indexOf("96 %") < 0 ||
      slidePop.indexOf("Probabilidad de lluvia 96 %") < 0) {
    throw new Error("tarjeta diaria sin % de lluvia");
  }
  var slideNoPop = ecRenderPronosticoSlide({
    fecha: "2026-10-12",
    variables: []
  }, "2026-10-08", 4);
  if (slideNoPop.indexOf("ec-pronostico-day-pop") >= 0) {
    throw new Error("tarjeta sin pop no debe mostrar %");
  }
  var hoyPartes = ecPronosticoSantiagoParts(new Date());
  if (ecPronosticoTodayIso() !== hoyPartes.year + "-" + hoyPartes.month + "-" + hoyPartes.day) {
    throw new Error("hoy chile");
  }
  var keptDias = ecPronosticoDiasDesdeHoy(
    [{ fecha: "2026-10-25" }, { fecha: "2026-10-26" }],
    "2026-10-26"
  );
  if (keptDias.length !== 1 || keptDias[0].fecha !== "2026-10-26") {
    throw new Error("dia pasado en el carrusel");
  }
  var slideHoy = ecRenderPronosticoSlide(keptDias[0], "2026-10-26", 0);
  if (slideHoy.indexOf(">Hoy<") < 0 || slideHoy.indexOf("Pr\u00f3ximo") >= 0) {
    throw new Error("chip hoy");
  }
  var fres = ecPronosticoFrescura("2026-10-24", "2026-10-26");
  if (!fres || fres.dias !== 2 || fres.label !== "Hace 2 d\u00edas") {
    throw new Error("frescura " + (fres && fres.label));
  }
  var aligned = ecPronosticoAlignT([10, 20, 15], 8, 28);
  if (Math.abs(Math.max.apply(null, aligned) - 28) > 1e-9) {
    throw new Error("ecPronosticoAlignT max != chip t_max");
  }
  if (Math.abs(Math.min.apply(null, aligned) - 8) > 1e-9) {
    throw new Error("ecPronosticoAlignT min != chip t_min");
  }
})();

/* ponytail: lite path chains .then; ecInitPronosticoCarousel must return Promise */
(function ecPronosticoPromiseSelfCheck() {
  var pr = ecInitPronosticoCarousel("_ec_promise_chk_mount_", null, null);
  if (!pr || typeof pr.then !== "function") {
    throw new Error("ecInitPronosticoCarousel must return a Promise");
  }
})();
