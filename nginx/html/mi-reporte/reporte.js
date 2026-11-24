// reporte.js — Reporte Climatico Estacion
// Requiere Chart.js 4.x (cargado via CDN en el <head>)
// El reporte se muestra via Shiny showModal/removeModal.
// Este archivo solo maneja la interactividad interna del reporte.
// ============================================================

// ---- Chart.js time series ----
var _ecChartInstances = {};
var _ecChartResolution = {};
var _ecChartSpan = {};
var _ecCicloChartInstances = {};
var _ecCicloChartMode = {};
var _ecCicloResizeBound = false;

function ecEnsureChartLocale() {
  if (typeof Chart !== 'undefined' && Chart.defaults) Chart.defaults.locale = 'es-CL';
}

function ecFmtNum(v, dec) {
  if (v == null || isNaN(Number(v))) return '\u2014';
  var n = Number(v);
  var digits = dec == null ? 0 : dec;
  var opts = { maximumFractionDigits: digits, minimumFractionDigits: 0, useGrouping: 'always' };
  try {
    return n.toLocaleString('es-CL', opts);
  } catch (e) {
    opts.useGrouping = true;
    return n.toLocaleString('es-CL', opts);
  }
}

function ecFmtAxis(v) {
  if (v == null || isNaN(Number(v))) return '';
  var n = Number(v);
  return ecFmtNum(n, Math.abs(n - Math.round(n)) < 1e-6 ? 0 : 1);
}

/* 1811.3 -> 1900: dos cifras significativas, hacia arriba. */
function ecRoundAxisMax(axisMax) {
  if (axisMax == null || !isFinite(axisMax) || axisMax <= 0) return axisMax;
  var exp = Math.floor(Math.log10(axisMax)) - 1;
  var step = Math.pow(10, exp);
  return Math.ceil(axisMax / step) * step;
}

function ecIsMobileLayout() {
  return window.matchMedia('(max-width: 600px)').matches;
}

function ecBindCicloResizeListener() {
  if (_ecCicloResizeBound) return;
  _ecCicloResizeBound = true;
  var mql = window.matchMedia('(max-width: 600px)');
  var lastMobile = ecIsMobileLayout();
  function onLayoutChange() {
    var nowMobile = ecIsMobileLayout();
    if (nowMobile === lastMobile) return;
    lastMobile = nowMobile;
    Object.keys(_ecCicloChartInstances).forEach(function(canvasId) {
      var data = window['_ecCicloData_' + canvasId];
      if (data) ecInitCicloAnualChart(canvasId, data, _ecCicloChartMode[canvasId] || 'anual');
    });
  }
  if (mql.addEventListener) mql.addEventListener('change', onLayoutChange);
  else if (mql.addListener) mql.addListener(onLayoutChange);
}

function ecFmtWithUnit(v, unit) {
  if (v === null || v === undefined || isNaN(Number(v))) return '\u2014';
  var txt = ecFmtNum(v, 1);
  return unit ? txt + ' ' + unit : txt;
}

function ecParseYmd(s) {
  var m = /^(\d{4})-(\d{2})-(\d{2})/.exec(String(s || ''));
  if (!m) return null;
  return new Date(Number(m[1]), Number(m[2]) - 1, Number(m[3]));
}

function ecUpdateTSLegend(canvasId, resolution) {
  var annualView = resolution === 'anual';
  var legend = document.getElementById(canvasId + '_ts_legend');
  var kpis = document.getElementById(canvasId + '_kpis');
  var note = document.getElementById(canvasId + '_ts_note');
  var subtitle = document.getElementById(canvasId + '_ts_subtitle');
  var spanEl = document.getElementById(canvasId + '_span');
  var pack = window['_ecTSData_' + canvasId];
  var annual = pack && pack.annual;
  if (legend) legend.style.display = annualView ? '' : 'none';
  if (kpis) kpis.style.display = annualView ? '' : 'none';
  if (note) note.style.display = annualView ? '' : 'none';
  if (spanEl) {
    spanEl.style.display = (annualView && annual && annual.partial_year) ? '' : 'none';
  }
  if (subtitle && !annualView) {
    if (resolution === 'completitud') {
      subtitle.textContent = 'Cobertura diaria: d\u00edas con registro / d\u00edas del a\u00f1o';
      subtitle.style.display = '';
    } else {
      subtitle.style.display = 'none';
    }
  }
}

function ecInitTSChart(canvasId, data) {
  if (!data || data === 'null') return;
  window['_ecTSData_' + canvasId] = data;
  _ecChartResolution[canvasId] = 'anual';
  if (!_ecChartSpan[canvasId]) _ecChartSpan[canvasId] = 'completo';
  ecRenderTS(canvasId);
}

function ecDestroyAllCharts() {
  Object.keys(_ecChartInstances).forEach(function(id) {
    if (_ecChartInstances[id]) {
      _ecChartInstances[id].destroy();
      delete _ecChartInstances[id];
    }
  });
  Object.keys(_ecCicloChartInstances).forEach(function(id) {
    if (_ecCicloChartInstances[id]) {
      _ecCicloChartInstances[id].destroy();
      delete _ecCicloChartInstances[id];
    }
  });
  _ecChartResolution = {};
  _ecChartSpan = {};
  _ecCicloChartMode = {};
}

function ecTSSetRes(btn, canvasId, resolution) {
  var container = btn.closest('.ec-rp-ts-chips') || btn.closest('.ec-ts-pills') || btn.closest('.pill-group');
  if (container) {
    container.querySelectorAll('.ec-chip, .pill').forEach(function(c) {
      c.classList.remove('active');
    });
  }
  btn.classList.add('active');

  if (!document.getElementById(canvasId)) return;
  if (!window['_ecTSData_' + canvasId]) return;
  _ecChartResolution[canvasId] = resolution;
  ecRenderTS(canvasId);
}

function ecTSSetSpan(btn, canvasId, span) {
  var container = btn.closest('.ec-ts-span');
  if (container) {
    container.querySelectorAll('.pill').forEach(function(c) {
      c.classList.remove('active');
    });
  }
  btn.classList.add('active');
  _ecChartSpan[canvasId] = span;
  var card = document.getElementById(canvasId);
  card = card ? card.closest('.ec-rp-ts-card') : null;
  var resBtn = card ? card.querySelector('[data-ts-res="anual"]') : null;
  if (resBtn) ecTSSetRes(resBtn, canvasId, 'anual');
}

function ecTsNum(v) {
  if (v === null || v === undefined || v === '') return null;
  if (typeof v === 'string' && (v === 'NA' || v === 'NaN' || v.trim() === '')) return null;
  var n = Number(v);
  return isNaN(n) ? null : n;
}

function ecTsEsc(s) {
  return String(s == null ? '' : s)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}

function ecTsFmt(v, unit) {
  if (v === null || v === undefined || isNaN(Number(v))) return '\u2014';
  var fmt = (typeof ecFmtNum === 'function')
    ? ecFmtNum(Number(v), 1)
    : String(Math.round(Number(v) * 10) / 10);
  return unit ? fmt + ' ' + unit : fmt;
}

function ecTsFillYearGaps(labels, values, coverage) {
  var rows = [];
  var i;
  for (i = 0; i < (labels || []).length; i++) {
    var y = parseInt(labels[i], 10);
    if (isNaN(y)) continue;
    rows.push({
      year: y,
      value: ecTsNum(values ? values[i] : null),
      coverage: ecTsNum(coverage ? coverage[i] : null)
    });
  }
  rows.sort(function(a, b) { return a.year - b.year; });
  if (!rows.length) return [];
  var byYear = {};
  rows.forEach(function(r) { byYear[r.year] = r; });
  var out = [];
  for (var y = rows[0].year; y <= rows[rows.length - 1].year; y++) {
    if (byYear[y]) out.push(byYear[y]);
    else out.push({ year: y, value: null, coverage: null, gap: true });
  }
  return out;
}

function ecTsAxis(slots, mean, vtype) {
  var precip = vtype === 'precip';
  var yMin = 0;
  var yMax = 0;
  var saw = false;
  slots.forEach(function(s) {
    if (s.value === null) return;
    if (!saw || s.value < yMin) yMin = s.value;
    if (!saw || s.value > yMax) yMax = s.value;
    saw = true;
  });
  if (mean !== null) {
    if (!saw || mean > yMax) yMax = mean;
    if (!saw || mean < yMin) yMin = mean;
    saw = true;
  }
  if (!saw) return { yMin: 0, yMax: precip ? 100 : 1, step: precip ? 20 : 1 };
  if (precip || yMin >= 0) yMin = 0;
  var span = yMax - yMin;
  if (!(span > 0)) span = precip ? 100 : 1;
  if (!precip && yMin < 0) yMin = yMin - span * 0.08;
  span = yMax - yMin;
  var rough = span / 5;
  var pow = Math.pow(10, Math.floor(Math.log10(Math.max(rough, 1e-9))));
  if (!isFinite(pow) || pow <= 0) pow = 1;
  var err = rough / pow;
  var nice = err >= 7.5 ? 10 : err >= 3.5 ? 5 : err >= 1.5 ? 2 : 1;
  var step = nice * pow;
  if (precip && yMax >= 200 && step < 100) step = 100;
  var padded = yMax + (yMax - (precip || yMin === 0 ? 0 : yMin)) * 0.08;
  if (padded <= yMax) padded = yMax + step;
  var yTop = yMin + Math.ceil((padded - yMin) / step) * step;
  if (!(yTop > yMin)) yTop = yMin + step;
  if (!precip && yMin < 0) yMin = Math.floor(yMin / step) * step;
  return { yMin: yMin, yMax: yTop, step: step };
}

function ecTsAnnualModel(opts) {
  opts = opts || {};
  var slots = ecTsFillYearGaps(opts.labels || [], opts.values || [], opts.coverage || []);
  var min = opts.coverageMin == null ? 70 : Number(opts.coverageMin);
  var start = opts.start == null ? null : parseInt(opts.start, 10);
  var end = opts.end == null ? null : parseInt(opts.end, 10);
  if (isNaN(start)) start = null;
  if (isNaN(end)) end = null;
  var currentYear = parseInt(opts.currentYear, 10);
  var useYtd = !!opts.useYtd;
  var partial = !!opts.partialYear && !useYtd;
  var usable = [];
  slots.forEach(function(s) {
    s.gap = !!s.gap || s.value === null;
    s.open = partial && s.year === currentYear && s.value !== null;
    s.low = s.value !== null && (s.coverage === null || s.coverage < min);
    s.inPeriod = (start === null || s.year >= start) && (end === null || s.year <= end);
    s.forMean = s.inPeriod && !s.open && s.value !== null && !s.low;
    if (s.forMean) usable.push(s);
  });
  var mean = null;
  if (usable.length) {
    var sum = 0;
    usable.forEach(function(s) { sum += s.value; });
    mean = sum / usable.length;
  }
  var pool = slots.filter(function(s) { return s.value !== null && !s.open && !s.low; });
  var maxS = null;
  var minS = null;
  pool.forEach(function(s) {
    if (!maxS || s.value > maxS.value) maxS = s;
    if (!minS || s.value < minS.value) minS = s;
  });
  if (maxS && minS && maxS.year === minS.year) minS = null;
  var axis = ecTsAxis(slots, mean, opts.vtype);
  return {
    slots: slots,
    mean: mean,
    nUsable: usable.length,
    max: maxS,
    min: minS,
    yMin: axis.yMin,
    yMax: axis.yMax,
    step: axis.step,
    periodStart: start,
    periodEnd: end,
    vtype: opts.vtype || 'generic'
  };
}

function ecTsBarColor(slot, model) {
  if (slot.value === null) return '#394553';
  if (model.vtype !== 'precip' || model.mean === null) return '#394553';
  return slot.value >= model.mean ? '#2A78D6' : '#EB6834';
}

function ecTsKpi(label, value, extra) {
  return '<div class="ec-rp-ts-kpi"><small>' + ecTsEsc(label) + '</small><b>' +
    ecTsEsc(value) + '</b>' +
    (extra ? '<span>' + ecTsEsc(extra) + '</span>' : '') + '</div>';
}

function ecTsLegendHtml(model) {
  var precip = model.vtype === 'precip' && model.mean !== null;
  var bits = [];
  if (precip) {
    bits.push('<span><i class="ec-ts-swatch" style="background:#2A78D6"></i>Sobre la media</span>');
    bits.push('<span><i class="ec-ts-swatch" style="background:#EB6834"></i>Bajo la media</span>');
  } else {
    bits.push('<span><i class="ec-ts-swatch" style="background:#394553"></i>Valor anual</span>');
  }
  bits.push('<span class="ec-ts-legend-mean"><svg width="16" height="8" aria-hidden="true"><line x1="0" y1="4" x2="16" y2="4" stroke="#5f6b76" stroke-width="1.5" stroke-dasharray="3 3"></line></svg>Media del per\u00edodo</span>');
  if (model.slots.some(function(s) { return s.open; })) {
    var c = model.vtype === 'precip' ? '#2A78D6' : '#394553';
    bits.push('<span><i class="ec-ts-swatch ec-ts-swatch-open" style="color:' + c + '"></i>A\u00f1o en curso</span>');
  }
  if (model.slots.some(function(s) { return s.gap && s.value === null; })) {
    bits.push('<span><i class="ec-ts-swatch ec-ts-swatch-gap"></i>Sin datos</span>');
  }
  return bits.join('');
}

function ecTsTipHtml(slot, model) {
  var unit = model.unit || '';
  var head = String(slot.year) + (slot.open ? ' \u00b7 en curso' : '');
  var html = '<div class="ec-rp-ts-tip-year">' + ecTsEsc(head) + '</div>';
  if (slot.value === null) {
    html += '<div class="ec-rp-ts-tip-val">Sin datos</div>';
    html += '<div class="ec-rp-ts-tip-sub">La estaci\u00f3n no report\u00f3 este a\u00f1o.</div>';
    return html;
  }
  html += '<div class="ec-rp-ts-tip-val">' + ecTsEsc(ecTsFmt(slot.value, '')) +
    (unit ? ' <span>' + ecTsEsc(unit) + '</span>' : '') + '</div>';
  if (model.mean !== null) {
    var d = slot.value - model.mean;
    var pct = Math.abs(model.mean) > 1e-9 ? Math.round(d / Math.abs(model.mean) * 100) : null;
    var col = model.vtype === 'precip' ? (d >= 0 ? '#2A78D6' : '#EB6834') : '#394553';
    var sign = d >= 0 ? '+' : '\u2212';
    var line = sign + ecTsFmt(Math.abs(d), unit);
    if (pct !== null) line += ' (' + (pct >= 0 ? '+' : '\u2212') + Math.abs(pct) + '%)';
    line += ' vs media';
    html += '<div class="ec-rp-ts-tip-delta"><i style="background:' + col + '"></i><span>' + ecTsEsc(line) + '</span></div>';
    if (model.vtype === 'precip' && pct !== null) {
      var cat = pct > 15 ? 'A\u00f1o h\u00famedo' : (pct < -15 ? 'A\u00f1o seco' : 'A\u00f1o normal');
      html += '<div class="ec-rp-ts-tip-sub">' + cat + '</div>';
    }
  }
  if (slot.low && slot.coverage !== null) {
    html += '<div class="ec-rp-ts-tip-sub">Cobertura baja: ' + ecTsEsc(ecTsFmt(slot.coverage, '%')) + ' de los d\u00edas con registro.</div>';
  }
  if (slot.open && model.lastLabel) {
    html += '<div class="ec-rp-ts-tip-sub">Acumulado hasta el ' + ecTsEsc(model.lastLabel) + '.</div>';
  }
  return html;
}

function ecTsHideTip(canvasId) {
  var tip = document.getElementById(canvasId + '_tt');
  var svg = document.getElementById(canvasId + '_svg');
  if (tip) {
    tip.classList.remove('is-on');
    tip.style.display = 'none';
  }
  if (svg) svg.querySelectorAll('.ec-ts-band').forEach(function(b) { b.style.display = 'none'; });
}

function ecTsShowTip(canvasId, el) {
  var model = window['_ecTsModel_' + canvasId];
  var tip = document.getElementById(canvasId + '_tt');
  var svg = document.getElementById(canvasId + '_svg');
  if (!model || !tip || !svg || !el) return;
  var slot = model.slots[parseInt(el.getAttribute('data-i'), 10)];
  if (!slot) return;
  svg.querySelectorAll('.ec-ts-band').forEach(function(b) { b.style.display = 'none'; });
  var band = el.querySelector('.ec-ts-band');
  if (band) band.style.display = 'block';
  tip.innerHTML = ecTsTipHtml(slot, model);
  tip.classList.add('is-on');
  tip.style.display = 'block';
  var wrap = svg.parentNode;
  var vb = svg.viewBox.baseVal;
  var svgBox = svg.getBoundingClientRect();
  var wrapBox = wrap.getBoundingClientRect();
  var k = svgBox.width / (vb.width || 680);
  var cx = (svgBox.left - wrapBox.left) + parseFloat(el.getAttribute('data-cx')) * k;
  var top = (svgBox.top - wrapBox.top) + parseFloat(el.getAttribute('data-top')) * k;
  var w = tip.offsetWidth;
  var h = tip.offsetHeight;
  var L = Math.min(Math.max(cx - w / 2, 0), Math.max(wrap.clientWidth - w, 0));
  var T = top - h - 8;
  if (T < 0) T = top + 16;
  tip.style.left = L + 'px';
  tip.style.top = T + 'px';
}

function ecRenderAnnualSvg(canvasId, data) {
  var svg = document.getElementById(canvasId + '_svg');
  if (!svg || !data) return;
  var useYtd = _ecChartSpan[canvasId] === 'hasta_hoy';
  var values = useYtd ? (data.ytd || data.current || []) : (data.current || []);
  var coverage = useYtd ? (data.coverage_ytd || data.coverage || []) : (data.coverage || []);
  values = ecCleanSeries(values) || [];
  coverage = ecCleanSeries(coverage) || [];
  var period = data.normalPeriod || {};
  var clim = (typeof ecNormalGetClimatology === 'function') ? ecNormalGetClimatology() : null;
  var vtype = (clim && clim.vtype) || 'generic';
  var model = ecTsAnnualModel({
    labels: ecAsNumericSeries(data.labels) || data.labels || [],
    values: values,
    coverage: coverage,
    start: period.start,
    end: period.end,
    currentYear: data.current_year || new Date().getFullYear(),
    partialYear: !!data.partial_year,
    useYtd: useYtd,
    coverageMin: data.coverage_min || 70,
    vtype: vtype
  });
  model.unit = data.unit || '';
  model.lastLabel = data.last_label || '';
  window['_ecTsModel_' + canvasId] = model;

  var y0 = model.slots.length ? model.slots[0].year : '';
  var y1 = model.slots.length ? model.slots[model.slots.length - 1].year : '';
  var nData = model.slots.filter(function(s) { return s.value !== null; }).length;
  var kind = vtype === 'precip' ? 'Totales anuales' : 'Valores anuales';
  var subtitle = kind + ' ' + y0 + '\u2013' + y1 + ' \u00b7 ' + nData + ' de ' + model.slots.length + ' a\u00f1os con datos';
  if (useYtd && data.last_label) {
    subtitle = 'Cada a\u00f1o, solo hasta el ' + data.last_label + ' \u00b7 ' + nData + ' de ' + model.slots.length + ' a\u00f1os con datos';
  }
  var subEl = document.getElementById(canvasId + '_ts_subtitle');
  if (subEl) {
    subEl.textContent = subtitle;
    subEl.style.display = '';
  }

  var precip = vtype === 'precip';
  var maxName = precip ? 'A\u00f1o m\u00e1s lluvioso' : 'Mayor';
  var minName = precip ? 'A\u00f1o m\u00e1s seco' : 'Menor';
  var periodTxt = (model.periodStart != null && model.periodEnd != null)
    ? (model.periodStart + '\u2013' + model.periodEnd) : '';
  var kpis = document.getElementById(canvasId + '_kpis');
  if (kpis) {
    kpis.innerHTML =
      ecTsKpi('Media del per\u00edodo', model.mean === null ? '\u2014' : ecTsFmt(model.mean, model.unit), periodTxt) +
      ecTsKpi(maxName, model.max ? ecTsFmt(model.max.value, model.unit) : '\u2014', model.max ? String(model.max.year) : '') +
      ecTsKpi(minName, model.min ? ecTsFmt(model.min.value, model.unit) : '\u2014', model.min ? String(model.min.year) : '');
  }
  var legend = document.getElementById(canvasId + '_ts_legend');
  if (legend) legend.innerHTML = ecTsLegendHtml(model);

  var note = document.getElementById(canvasId + '_ts_note');
  if (note) {
    var bits = [];
    var spanYears = (model.periodStart != null && model.periodEnd != null)
      ? (model.periodEnd - model.periodStart + 1) : null;
    if (model.mean === null) {
      bits.push('Este per\u00edodo no tiene a\u00f1os con datos suficientes para una media.');
    } else if (spanYears !== null && spanYears < 30) {
      bits.push('Promedio de los a\u00f1os con datos en el per\u00edodo; no es una normal de 30 a\u00f1os.');
    }
    if (!useYtd && data.partial_year) bits.push('El a\u00f1o en curso incluye datos hasta la fecha.');
    note.textContent = bits.join(' ');
  }

  var srTxt = 'Barras anuales de ' + y0 + ' a ' + y1 + ', ' + nData + ' de ' + model.slots.length + ' a\u00f1os con datos.';
  if (model.mean !== null) srTxt += ' Media del per\u00edodo ' + ecTsFmt(model.mean, model.unit) + '.';
  if (model.max) srTxt += ' Mayor: ' + model.max.year + ', ' + ecTsFmt(model.max.value, model.unit) + '.';
  if (model.min) srTxt += ' Menor: ' + model.min.year + ', ' + ecTsFmt(model.min.value, model.unit) + '.';
  var sr = document.getElementById(canvasId + '_sr');
  if (sr) sr.textContent = srTxt;
  svg.setAttribute('aria-label', srTxt);

  var table = document.getElementById(canvasId + '_ts_table');
  if (table) {
    var rows = model.slots.map(function(s) {
      var val = s.value === null ? 'sin datos' : ecTsFmt(s.value, model.unit);
      var cov = s.coverage === null ? '\u2014' : ecTsFmt(s.coverage, '%');
      return '<tr><th scope="row">' + s.year + '</th><td>' + ecTsEsc(val) + '</td><td>' + ecTsEsc(cov) + '</td></tr>';
    }).join('');
    table.innerHTML = '<caption>Historial anual</caption><thead><tr><th>A\u00f1o</th><th>Valor</th><th>Cobertura</th></tr></thead><tbody>' + rows + '</tbody>';
  }

  var W = 780;
  var H = 246;
  var x0 = 56;
  var yb = 270;
  var pw = 600;
  var n = model.slots.length || 1;
  var st = pw / n;
  var bw = Math.min(18, Math.max(4, st * 0.62));
  var ySpan = model.yMax - model.yMin || 1;
  function Y(v) { return yb - ((v - model.yMin) / ySpan) * H; }
  var pid = 'ecTsHatch' + String(canvasId).replace(/[^A-Za-z0-9_-]/g, '');
  var parts = [];
  parts.push('<defs>');
  model.slots.forEach(function(s, i) {
    if (!s.open) return;
    var c = ecTsBarColor(s, model);
    parts.push('<pattern id="' + pid + i + '" width="5" height="5" patternUnits="userSpaceOnUse" patternTransform="rotate(45)">');
    parts.push('<rect width="5" height="5" fill="' + c + '" fill-opacity="0.12"/>');
    parts.push('<line x1="0" y1="0" x2="0" y2="5" stroke="' + c + '" stroke-width="2.4"/></pattern>');
  });
  parts.push('</defs>');
  var axisTitle = vtype === 'precip' ? ((data.unit || 'mm') + ' por a\u00f1o') : (data.unit || '');
  if (axisTitle) parts.push('<text x="0" y="14" font-size="11" fill="#8A95A0">' + ecTsEsc(axisTitle) + '</text>');
  var nLines = Math.round((model.yMax - model.yMin) / model.step);
  var gi;
  for (gi = 0; gi <= nLines; gi++) {
    var g = model.yMin + gi * model.step;
    var gy = Y(g);
    var base = Math.abs(g) < 1e-6;
    parts.push('<line x1="' + x0 + '" x2="' + (x0 + pw) + '" y1="' + gy + '" y2="' + gy + '" stroke="' + (base ? '#c5c9ce' : '#e7e5e0') + '" stroke-width="1"/>');
    parts.push('<text x="' + (x0 - 8) + '" y="' + (gy + 4) + '" text-anchor="end" font-size="11" fill="#8A95A0">' + ecTsEsc(ecTsFmt(g, '')) + '</text>');
  }
  var labelEvery = st < 28 ? 2 : 1;
  model.slots.forEach(function(s, i) {
    var cx = x0 + st * (i + 0.5);
    var x = cx - bw / 2;
    var top = s.value === null ? (yb - 40) : Y(s.value);
    var aria = s.value === null
      ? (s.year + ': sin datos')
      : (s.year + ': ' + ecTsFmt(s.value, model.unit) + (s.open ? ', a\u00f1o en curso' : '') + (s.low ? ', cobertura baja' : ''));
    parts.push('<g class="ec-ts-slot" data-i="' + i + '" data-cx="' + cx + '" data-top="' + top + '" tabindex="0" role="button" aria-label="' + ecTsEsc(aria) + '">');
    parts.push('<rect class="ec-ts-band" x="' + (x0 + st * i) + '" y="20" width="' + st + '" height="' + (H + 8) + '" fill="rgba(128,128,128,0.10)" style="display:none"/>');
    if (s.value === null) {
      parts.push('<rect x="' + x + '" y="' + (yb - 26) + '" width="' + bw + '" height="26" rx="3" fill="none" stroke="#8A95A0" stroke-dasharray="3 3"/>');
      parts.push('<text x="' + cx + '" y="' + (yb - 32) + '" text-anchor="middle" font-size="11" fill="#8A95A0">s/d</text>');
    } else {
      var h = Math.max(yb - Y(s.value), 2);
      var y = yb - h;
      var r = Math.min(4, h / 2);
      var c = ecTsBarColor(s, model);
      var dpath = 'M' + x + ' ' + yb + 'V' + (y + r) + 'Q' + x + ' ' + y + ' ' + (x + r) + ' ' + y +
        'H' + (x + bw - r) + 'Q' + (x + bw) + ' ' + y + ' ' + (x + bw) + ' ' + (y + r) + 'V' + yb + 'Z';
      if (s.open) parts.push('<path d="' + dpath + '" fill="url(#' + pid + i + ')" stroke="' + c + '" stroke-width="1.5"/>');
      else parts.push('<path d="' + dpath + '" fill="' + c + '"/>');
      if ((model.max && s.year === model.max.year) || (model.min && s.year === model.min.year)) {
        parts.push('<text x="' + cx + '" y="' + (y - 6) + '" text-anchor="middle" font-size="11" font-weight="500" fill="#394553">' + ecTsEsc(ecTsFmt(s.value, '')) + '</text>');
      }
    }
    parts.push('<rect x="' + (x0 + st * i) + '" y="16" width="' + st + '" height="' + (yb - 16 + 28) + '" fill="transparent"/>');
    var showLbl = (i % labelEvery === 0) || s.value === null || s.open || i === 0 || i === n - 1;
    if (showLbl) {
      parts.push('<text x="' + cx + '" y="292" text-anchor="middle" font-size="11" fill="#8A95A0">\u2019' + String(s.year).slice(-2) + '</text>');
    }
    parts.push('</g>');
  });
  if (model.mean !== null) {
    var my = Y(model.mean);
    parts.push('<line x1="' + x0 + '" x2="' + (x0 + pw) + '" y1="' + my + '" y2="' + my + '" stroke="#5f6b76" stroke-width="1.25" stroke-dasharray="4 4"/>');
    parts.push('<text x="' + (x0 + pw + 8) + '" y="' + (my - 2) + '" font-size="11" fill="#5f6b76">Media</text>');
    parts.push('<text x="' + (x0 + pw + 8) + '" y="' + (my + 12) + '" font-size="11" font-weight="500" fill="#394553">' + ecTsEsc(ecTsFmt(model.mean, model.unit)) + '</text>');
  }
  svg.setAttribute('viewBox', '0 0 ' + W + ' 310');
  svg.innerHTML = parts.join('');
  svg.querySelectorAll('.ec-ts-slot').forEach(function(el) {
    el.addEventListener('mouseenter', function() { ecTsShowTip(canvasId, el); });
    el.addEventListener('mouseleave', function() { ecTsHideTip(canvasId); });
    el.addEventListener('focus', function() { ecTsShowTip(canvasId, el); });
    el.addEventListener('blur', function() { ecTsHideTip(canvasId); });
  });
}

function ecRenderTS(canvasId) {
  var resolution = _ecChartResolution[canvasId] || 'anual';
  var pack = window['_ecTSData_' + canvasId];
  var canvas = document.getElementById(canvasId);
  if (!pack || !canvas) return;
  var wrap = canvas.closest('.ec-rp-ts-chart-wrap');
  var svg = document.getElementById(canvasId + '_svg');
  if (resolution === 'anual' && svg) {
    if (wrap) {
      wrap.classList.add('is-annual');
      wrap.classList.remove('is-other');
    }
    if (_ecChartInstances[canvasId]) {
      _ecChartInstances[canvasId].destroy();
      delete _ecChartInstances[canvasId];
    }
    ecTsHideTip(canvasId);
    ecRenderAnnualSvg(canvasId, pack.annual || pack);
    ecUpdateTSLegend(canvasId, resolution);
    return;
  }
  if (wrap) {
    wrap.classList.add('is-other');
    wrap.classList.remove('is-annual');
  }
  ecTsHideTip(canvasId);
  if (typeof Chart === 'undefined') {
    var tries = ecRenderTS._tries || (ecRenderTS._tries = {});
    tries[canvasId] = (tries[canvasId] || 0) + 1;
    if (tries[canvasId] < 30) setTimeout(function() { ecRenderTS(canvasId); }, 100);
    return;
  }
  if (ecRenderTS._tries) ecRenderTS._tries[canvasId] = 0;
  var resData;
  if (resolution === 'mensual') {
    resData = pack.monthly || pack;
    resData.flagged_annual = pack.flagged_monthly;
  } else if (resolution === 'completitud') {
    resData = pack.completeness || { labels: [], values: [] };
  } else {
    resData = pack.annual || pack;
    resData.flagged_annual = pack.flagged_annual;
  }
  if (_ecChartInstances[canvasId]) {
    _ecChartInstances[canvasId].destroy();
    delete _ecChartInstances[canvasId];
  }
  _ecChartInstances[canvasId] = ecBuildChart(canvas, resData, resolution);
  ecUpdateTSLegend(canvasId, resolution);
}

function ecAsNumericSeries(arr) {
  if (arr == null) return arr;
  if (Array.isArray(arr)) return arr;
  if (typeof arr === 'object') {
    var keys = Object.keys(arr).filter(function(k) { return /^\d+$/.test(k); });
    if (keys.length) {
      keys.sort(function(a, b) { return Number(a) - Number(b); });
      return keys.map(function(k) { return arr[k]; });
    }
  }
  return arr;
}

function ecCleanSeries(arr) {
  arr = ecAsNumericSeries(arr);
  if (!arr || !arr.length) return arr;
  return arr.map(function(v) {
    if (v === null || v === undefined) return null;
    if (typeof v === 'object' && v !== null && Object.keys(v).length === 0) return null;
    if (typeof v === 'string' && (v === 'NA' || v === 'NaN' || v.trim() === '')) return null;
    if (typeof v === 'number' && isNaN(v)) return null;
    return v;
  });
}

function ecBuildChart(canvas, data, resolution) {
  if (!data || !data.labels) return null;
  if (resolution === 'anual') return null;
  ecEnsureChartLocale();

  if (data.current) data.current = ecCleanSeries(data.current);
  if (data.ytd) data.ytd = ecCleanSeries(data.ytd);
  if (data.previous) data.previous = ecCleanSeries(data.previous);
  if (data.hist_mean) data.hist_mean = ecCleanSeries(data.hist_mean);
  if (data.hist_mean_ytd) data.hist_mean_ytd = ecCleanSeries(data.hist_mean_ytd);
  if (data.normal_mean) data.normal_mean = ecCleanSeries(data.normal_mean);
  if (data.normal_mean_ytd) data.normal_mean_ytd = ecCleanSeries(data.normal_mean_ytd);
  if (data.coverage) data.coverage = ecCleanSeries(data.coverage);
  if (data.coverage_ytd) data.coverage_ytd = ecCleanSeries(data.coverage_ytd);
  if (data.values) data.values = ecCleanSeries(data.values);
  if (data.flagged_monthly) data.flagged_monthly = ecCleanSeries(data.flagged_monthly);
  if (data.flagged_annual) data.flagged_annual = ecCleanSeries(data.flagged_annual);

  var ctx = canvas.getContext('2d');
  var currentYear = data.current_year || new Date().getFullYear();
  var fontCfg = { size: 16, family: "'Inter', sans-serif" };
  var gridColor = 'rgba(0,0,0,0.06)';

  if (resolution === 'completitud') {
    var vals = data.values || [];
    var labels = data.labels || [];
    var barColors = labels.map(function(y) {
      return parseInt(y, 10) === currentYear ? '#DF9531' : 'rgba(57,69,83,0.45)';
    });

    return new Chart(ctx, {
      type: 'bar',
      data: {
        labels: labels,
        datasets: [{
          label: '% completitud',
          data: vals,
          backgroundColor: barColors,
          borderColor: barColors,
          borderWidth: 1,
          borderRadius: 4,
          maxBarThickness: 36
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        interaction: { mode: 'index', intersect: false },
        plugins: {
          legend: { display: false },
          tooltip: {
            callbacks: {
              label: function(ctx) {
                var v = ctx.parsed.y;
                if (v === null || isNaN(v)) return '% completitud: \u2014';
                return '% completitud: ' + ecFmtNum(v, 1) + ' %';
              }
            }
          }
        },
        scales: {
          x: {
            grid: { display: false },
            ticks: {
              font: fontCfg,
              maxTicksLimit: 12,
              maxRotation: 0
            }
          },
          y: {
            min: 0,
            max: 100,
            grid: { color: gridColor },
            ticks: {
              font: fontCfg,
              callback: function(v) { return ecFmtAxis(v) + ' %'; }
            }
          }
        }
      }
    });
  }

  var datasets = [];
  var isHistSeries = (resolution === 'anual' || resolution === 'mensual');
  var unit = data.unit || '';
  var span = (resolution === 'anual' && _ecChartSpan[canvas.id] === 'hasta_hoy') ? 'hasta_hoy' : 'completo';
  var useYtd = span === 'hasta_hoy';
  var seriesLabel = resolution === 'anual' ? 'Valores anuales' : 'Valores mensuales';
  var coverageMin = data.coverage_min || 70;
  var coverage = useYtd ? (data.coverage_ytd || data.coverage) : data.coverage;
  var labelsSeries = data.labels || [];
  var years = data.year || null;
  var months = data.month || null;
  var partialYearDraw = resolution === 'anual' && !useYtd && !!data.partial_year;
  var partialMonthDraw = resolution === 'mensual' && !!data.partial_month;
  var lastMonth = parseInt(data.last_month, 10);
  var GOLD = '#DF9531';
  var INK = 'rgba(57,69,83,0.9)';

  function ecTSMeta(i) {
    var yr = years ? parseInt(years[i], 10) : parseInt(labelsSeries[i], 10);
    var mo = months ? parseInt(months[i], 10) : NaN;
    var cov = coverage ? coverage[i] : null;
    var low = cov !== null && cov !== undefined && !isNaN(Number(cov)) && Number(cov) < coverageMin;
    var openYear = partialYearDraw && yr === currentYear;
    var openMonth = partialMonthDraw && yr === currentYear && mo === lastMonth;
    return { yr: yr, low: low, open: openYear || openMonth, hollow: low || openYear || openMonth, cov: cov };
  }

  if (isHistSeries) {
    var valsSeries = useYtd ? (data.ytd || data.current || []) : (data.current || []);
    var histSeries = useYtd ? (data.hist_mean_ytd || data.hist_mean) : data.hist_mean;
    var normSeries = useYtd ? (data.normal_mean_ytd || data.normal_mean) : data.normal_mean;

    var pointBg = valsSeries.map(function(v, i) {
      if (v === null || v === undefined || isNaN(v)) return 'transparent';
      var meta = ecTSMeta(i);
      if (meta.hollow) return 'transparent';
      return meta.yr === currentYear ? GOLD : INK;
    });
    var pointBorder = valsSeries.map(function(v, i) {
      if (v === null || v === undefined || isNaN(v)) return 'transparent';
      var meta = ecTSMeta(i);
      return meta.yr === currentYear ? GOLD : INK;
    });
    var pointRadius = valsSeries.map(function(v, i) {
      if (v === null || v === undefined || isNaN(v)) return 0;
      var meta = ecTSMeta(i);
      if (meta.yr === currentYear) return 5;
      return meta.hollow ? 4 : 2;
    });
    var pointBorderW = valsSeries.map(function(v, i) {
      if (v === null || v === undefined || isNaN(v)) return 0;
      return ecTSMeta(i).hollow ? 2 : 0;
    });

    datasets.push({
      label: seriesLabel,
      data: valsSeries,
      borderColor: INK,
      backgroundColor: INK,
      borderWidth: 1.5,
      pointStyle: 'circle',
      pointBackgroundColor: pointBg,
      pointBorderColor: pointBorder,
      pointBorderWidth: pointBorderW,
      pointRadius: pointRadius,
      tension: 0.2,
      spanGaps: false,
      fill: false,
      order: 1
    });

    if (histSeries) {
      datasets.push({
        label: 'Media hist\u00f3rica',
        data: histSeries,
        borderColor: '#999',
        backgroundColor: '#999',
        borderWidth: 1,
        borderDash: [5, 3],
        pointStyle: 'line',
        pointRadius: 0,
        tension: 0,
        fill: false,
        order: 2
      });
    }
    if (normSeries) {
      datasets.push({
        label: data._normalLabel || 'Normal',
        data: normSeries,
        borderColor: '#394553',
        backgroundColor: '#394553',
        borderWidth: 1.5,
        pointStyle: 'line',
        pointRadius: 0,
        tension: 0,
        fill: false,
        order: 3
      });
    }
    var flagged = useYtd ? null : data.flagged_annual;
    if (flagged) {
      var hasFlag = flagged.some(function(v) { return v !== null && v !== undefined && !isNaN(v); });
      if (hasFlag) {
        datasets.push({
          label: 'Valor imposible',
          data: flagged,
          type: 'scatter',
          pointStyle: 'crossRot',
          pointRadius: 8,
          pointBackgroundColor: '#DC2626',
          borderColor: '#DC2626',
          backgroundColor: '#DC2626',
          showLine: false,
          order: 0
        });
      }
    }
  }

  return new Chart(ctx, {
    type: 'line',
    data: { labels: data.labels, datasets: datasets },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      interaction: { mode: 'index', intersect: false },
      plugins: {
        legend: {
          position: 'bottom',
          labels: {
            usePointStyle: true,
            boxWidth: 28,
            boxHeight: 4,
            padding: 12,
            font: fontCfg,
            filter: function(item) {
              return String(item.text || '').indexOf('Normal') !== 0;
            }
          }
        },
        tooltip: {
          callbacks: {
            label: function(ctx) {
              var v = ctx.parsed.y;
              if (ctx.dataset.label === 'Valor imposible') {
                return 'Valor imposible: ' + ecFmtWithUnit(v, unit) + ' (excluido de medias)';
              }
              var line = ctx.dataset.label + ': ' + ecFmtWithUnit(v, unit);
              if (ctx.dataset.label !== seriesLabel || !isHistSeries) return line;
              var meta = ecTSMeta(ctx.dataIndex);
              if (meta.low && meta.cov !== null && !isNaN(Number(meta.cov))) {
                line += ' \u00b7 ' + ecFmtNum(Number(meta.cov), 1) + ' % de d\u00edas con registro';
              }
              if (meta.open && data.last_label) {
                line += ' \u00b7 acumulado hasta el ' + data.last_label;
              }
              return line;
            }
          }
        }
      },
      scales: {
        x: {
          grid: { color: gridColor },
          ticks: {
            font: fontCfg,
            maxTicksLimit: resolution === 'mensual' ? 24 : 10,
            maxRotation: resolution === 'mensual' ? 45 : 0
          }
        },
        y: {
          grid: { color: gridColor },
          ticks: {
            font: fontCfg,
            callback: function(v) { return ecFmtAxis(v); }
          },
          title: {
            display: !!unit,
            text: unit,
            font: { size: 13, family: "'Inter', sans-serif" }
          }
        }
      }
    }
  });
}

// ---- Ciclo Anual chart ----
function ecCicloRender(canvasId, data, mode) {
  var canvas = document.getElementById(canvasId);
  if (!canvas || typeof Chart === 'undefined') return false;
  if (_ecCicloChartInstances[canvasId]) {
    _ecCicloChartInstances[canvasId].destroy();
    delete _ecCicloChartInstances[canvasId];
  }
  var ppAnual = mode !== 'mesActual' && data && data.variant === 'pp' &&
    !!document.getElementById(canvasId + '_bars');
  ecCicloSetPrecipChrome(canvasId, data, ppAnual);
  if (mode === 'mesActual') {
    _ecCicloChartInstances[canvasId] = ecBuildCicloMesActualChart(canvas, data.mesActual);
  } else {
    _ecCicloChartInstances[canvasId] = ecBuildCicloAnualChart(canvas, data);
  }
  if (!ppAnual) ecUpdateCicloHeader(canvasId, mode);
  return true;
}

function ecInitCicloAnualChart(canvasId, data, mode) {
  if (!data || data === 'null') return;
  window['_ecCicloData_' + canvasId] = data;
  if (!mode) mode = _ecCicloChartMode[canvasId] || 'anual';
  if (mode === 'mesActual' && !data.mesActual) mode = 'anual';
  _ecCicloChartMode[canvasId] = mode;

  var attempts = 0;
  function tryInit() {
    if (!ecCicloRender(canvasId, data, mode)) {
      if (++attempts < 30) setTimeout(tryInit, 100);
      return;
    }
    ecBindCicloResizeListener();
  }
  tryInit();
}

function ecCicloSetMode(btn, canvasId, mode) {
  var container = btn.closest('.ec-ciclo-pills') || btn.closest('.pill-group');
  if (container) {
    container.querySelectorAll('.pill').forEach(function(c) {
      var pressed = c === btn;
      c.classList.toggle('active', pressed);
      c.setAttribute('aria-pressed', pressed ? 'true' : 'false');
    });
  } else {
    btn.classList.add('active');
    btn.setAttribute('aria-pressed', 'true');
  }

  var data = window['_ecCicloData_' + canvasId];
  if (!data) return;
  if (mode === 'mesActual' && !data.mesActual) return;

  _ecCicloChartMode[canvasId] = mode;
  ecCicloRender(canvasId, data, mode);
}

function ecUpdateCicloHeader(canvasId, mode) {
  var data = window['_ecCicloData_' + canvasId];
  if (!data) return;
  var src = mode === 'mesActual' && data.mesActual ? data.mesActual : data;
  var titleEl = document.getElementById(canvasId + '_title');
  var subtitleEl = document.getElementById(canvasId + '_subtitle');
  if (titleEl && src.title) titleEl.textContent = src.title;
  if (subtitleEl) subtitleEl.textContent = src.subtitle || '';
}

function ecCicloHasData(arr) {
  if (!arr || !arr.length) return false;
  return arr.some(function(v) { return v !== null && v !== undefined && !isNaN(v); });
}

function ecCicloPointRadii(dataArr, currentMonth, baseRadius) {
  baseRadius = baseRadius || 3;
  var hi = baseRadius + 3;
  return (dataArr || []).map(function(v, i) {
    if (v === null || v === undefined || isNaN(v)) return 0;
    return (i + 1) === currentMonth ? hi : baseRadius;
  });
}

function ecCicloPointBorderWidths(dataArr, currentMonth) {
  return (dataArr || []).map(function(v, i) {
    if (v === null || v === undefined || isNaN(v)) return 0;
    return (i + 1) === currentMonth ? 2.5 : 1;
  });
}

function ecCicloDayPointRadii(dataArr, labels, currentDay, baseRadius) {
  baseRadius = baseRadius || 3;
  var hi = baseRadius + 3;
  return (dataArr || []).map(function(v, i) {
    if (v === null || v === undefined || isNaN(v)) return 0;
    if (currentDay == null || isNaN(currentDay)) return baseRadius;
    var dayNum = parseInt(labels[i], 10);
    return dayNum === currentDay ? hi : baseRadius;
  });
}

function ecCicloDayPointBorderWidths(dataArr, labels, currentDay) {
  return (dataArr || []).map(function(v, i) {
    if (v === null || v === undefined || isNaN(v)) return 0;
    if (currentDay == null || isNaN(currentDay)) return 1;
    var dayNum = parseInt(labels[i], 10);
    return dayNum === currentDay ? 2.5 : 1;
  });
}

function ecCicloApplyYScale(data, yMax, yScale) {
  if (yScale.ticks && !yScale.ticks.color) yScale.ticks.color = '#394553';
  var srvMax = data.ySuggestedMax;
  var srvMin = data.ySuggestedMin;
  var visMax = yMax > 0 ? yMax * 1.12 : null;
  var axisMax = null;
  if (srvMax != null && !isNaN(srvMax) && srvMax > 0) {
    axisMax = srvMax;
    if (visMax != null && axisMax > visMax * 1.2) axisMax = visMax;
  } else if (visMax != null) {
    axisMax = visMax;
  }
  if (axisMax != null && axisMax > 0) {
    yScale.max = ecRoundAxisMax(axisMax);
  }
  if (srvMin != null && !isNaN(srvMin)) {
    yScale.min = srvMin;
    yScale.beginAtZero = false;
  }
  return yScale;
}

function ecBuildCicloMesActualChart(canvas, data) {
  if (!data || !data.labels || !data.labels.length) return null;
  ecEnsureChartLocale();

  var ctx = canvas.getContext('2d');
  var fontCfg = { size: 14, family: "'Inter', sans-serif" };
  var gridColor = 'rgba(0,0,0,0.06)';
  var unit = data.unidad || '';
  var currentDay = (data.currentDay != null && !isNaN(Number(data.currentDay)))
    ? Number(data.currentDay) : null;
  var isMobile = ecIsMobileLayout();
  var labels = data.labels;

  var datasets = [];
  var yMax = 0;

  function trackMax(arr) {
    if (!arr) return;
    arr.forEach(function(v) {
      if (v !== null && v !== undefined && !isNaN(v) && v > yMax) yMax = v;
    });
  }

  if (data.variant === 'pp') {
    var acum = ecCleanSeries(data.acumuladoDiario || []);
    var pasado = ecCleanSeries(data.anioPasado || []);
    var normal = ecCleanSeries(data.anioNormal || []);

    trackMax(acum); trackMax(pasado); trackMax(normal);

    if (ecCicloHasData(normal)) {
      datasets.push({
        type: 'line',
        label: 'A\u00f1o Normal',
        data: normal,
        borderColor: '#394553',
        backgroundColor: '#394553',
        borderWidth: 2,
        pointStyle: 'rect',
        pointRadius: ecCicloDayPointRadii(normal, labels, currentDay, 4),
        pointBorderWidth: ecCicloDayPointBorderWidths(normal, labels, currentDay),
        tension: 0.2,
        spanGaps: false,
        order: 3
      });
    }
    if (ecCicloHasData(pasado)) {
      datasets.push({
        type: 'line',
        label: 'A\u00f1o Pasado',
        data: pasado,
        borderColor: '#6E7C8C',
        backgroundColor: '#6E7C8C',
        borderWidth: 2,
        pointRadius: ecCicloDayPointRadii(pasado, labels, currentDay, 3),
        pointBorderWidth: ecCicloDayPointBorderWidths(pasado, labels, currentDay),
        tension: 0.2,
        spanGaps: false,
        order: 2
      });
    }
    if (ecCicloHasData(acum)) {
      datasets.push({
        type: 'line',
        label: 'Acumulado Anual',
        data: acum,
        borderColor: '#DF9531',
        backgroundColor: '#DF9531',
        borderWidth: 2.5,
        pointRadius: ecCicloDayPointRadii(acum, labels, currentDay, 4),
        pointBorderWidth: ecCicloDayPointBorderWidths(acum, labels, currentDay),
        tension: 0.2,
        spanGaps: false,
        order: 1
      });
    }
  } else {
    var mediaNorm = ecCleanSeries(data.mediaNormal || []);
    var mediaPas = ecCleanSeries(data.mediaAnioPasado || []);
    var mediaAct = ecCleanSeries(data.mediaAnioActual || []);

    trackMax(mediaNorm); trackMax(mediaPas); trackMax(mediaAct);

    if (ecCicloHasData(mediaNorm)) {
      datasets.push({
        type: 'line',
        label: 'A\u00f1o Normal',
        data: mediaNorm,
        borderColor: '#394553',
        backgroundColor: '#394553',
        borderWidth: 2,
        pointStyle: 'rect',
        pointRadius: ecCicloDayPointRadii(mediaNorm, labels, currentDay, 4),
        pointBorderWidth: ecCicloDayPointBorderWidths(mediaNorm, labels, currentDay),
        tension: 0.2,
        spanGaps: false,
        order: 3
      });
    }
    if (ecCicloHasData(mediaPas)) {
      datasets.push({
        type: 'line',
        label: 'A\u00f1o Pasado',
        data: mediaPas,
        borderColor: '#6E7C8C',
        backgroundColor: '#6E7C8C',
        borderWidth: 2,
        pointRadius: ecCicloDayPointRadii(mediaPas, labels, currentDay, 3),
        pointBorderWidth: ecCicloDayPointBorderWidths(mediaPas, labels, currentDay),
        tension: 0.2,
        spanGaps: false,
        order: 2
      });
    }
    if (ecCicloHasData(mediaAct)) {
      datasets.push({
        type: 'line',
        label: 'A\u00f1o Actual',
        data: mediaAct,
        borderColor: '#DF9531',
        backgroundColor: '#DF9531',
        borderWidth: 2.5,
        pointRadius: ecCicloDayPointRadii(mediaAct, labels, currentDay, 4),
        pointBorderWidth: ecCicloDayPointBorderWidths(mediaAct, labels, currentDay),
        tension: 0.2,
        spanGaps: false,
        order: 1
      });
    }
  }

  if (datasets.length === 0) return null;

  var yScale = ecCicloApplyYScale(data, yMax, {
    beginAtZero: true,
    grid: { color: gridColor },
    ticks: {
      font: fontCfg,
      callback: function(v) { return ecFmtAxis(v); }
    },
    title: {
      display: !!unit,
      text: unit,
      font: { size: 13, family: "'Inter', sans-serif" }
    }
  });

  return new Chart(ctx, {
    type: 'line',
    data: { labels: labels, datasets: datasets },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      interaction: { mode: 'index', intersect: false },
      plugins: {
        legend: {
          position: isMobile ? 'bottom' : 'top',
          align: isMobile ? 'center' : 'end',
          labels: {
            boxWidth: 14,
            padding: 10,
            font: fontCfg,
            usePointStyle: true
          }
        },
        tooltip: {
          callbacks: {
            label: function(ctx) {
              var v = ctx.parsed.y;
              var lbl = ctx.dataset.label || '';
              if (v === null || isNaN(v)) return lbl + ': \u2014';
              var fmt = ecFmtNum(v, Math.abs(v) >= 100 ? 0 : 1);
              return lbl + ': ' + fmt + (unit ? ' ' + unit : '');
            }
          }
        }
      },
      scales: {
        x: {
          grid: { display: false },
          ticks: {
            font: fontCfg,
            maxRotation: isMobile ? 45 : 0,
            minRotation: isMobile ? 45 : 0,
            color: '#394553'
          }
        },
        y: yScale
      }
    }
  });
}

function ecCicloNum(v) {
  if (v === null || v === undefined || v === '' || isNaN(Number(v))) return null;
  return Number(v);
}

function ecCicloFmtSigned(v) {
  if (v === null || v === undefined || isNaN(Number(v))) return '\u2014';
  var n = Math.round(Number(v));
  var body = ecFmtNum(Math.abs(n), 0);
  if (n > 0) return '+' + body;
  if (n < 0) return '\u2212' + body;
  return body;
}

function ecCicloPrecipKpis(acum, normal, pasado, monthIndex) {
  function at(arr) {
    if (!arr || monthIndex < 0 || monthIndex >= arr.length) return null;
    return ecCicloNum(arr[monthIndex]);
  }
  var a = at(acum);
  var n = at(normal);
  var p = at(pasado);
  var deltaNormal = (a !== null && n !== null) ? a - n : null;
  var pct = (deltaNormal !== null && n !== 0) ? (deltaNormal / n) * 100 : null;
  return {
    acum: a,
    normal: n,
    pasado: p,
    deltaNormal: deltaNormal,
    pct: pct,
    deltaPasado: (a !== null && p !== null) ? a - p : null
  };
}

function ecCicloPrecipHeadline(kpis, anio) {
  var fallback = 'Precipitaci\u00f3n a\u00f1o ' + anio;
  if (!kpis || kpis.acum === null || kpis.pct === null || isNaN(kpis.pct)) return fallback;
  if (Math.abs(kpis.pct) < 1) return 'Este a\u00f1o va en la normal';
  var rounded = Math.round(Math.abs(kpis.pct));
  if (kpis.pct < 0) return 'Este a\u00f1o va ' + rounded + ' % bajo lo normal';
  return 'Este a\u00f1o va ' + rounded + ' % sobre lo normal';
}

function ecCicloLastIdx(arr, currentMonth) {
  var i = Math.min(11, (currentMonth || 1) - 1);
  while (i >= 0 && ecCicloNum(arr && arr[i]) === null) i--;
  return i;
}

function ecCicloGapSpan(acum, normal, lastIdx) {
  if (lastIdx < 0 || !acum || !normal) return null;
  var a = ecCicloNum(acum[lastIdx]);
  var n = ecCicloNum(normal[lastIdx]);
  if (a === null || n === null || a === n) return null;
  var below = a < n;
  var from = lastIdx;
  for (var i = lastIdx; i >= 0; i--) {
    var ai = ecCicloNum(acum[i]);
    var ni = ecCicloNum(normal[i]);
    if (ai === null || ni === null) break;
    if ((ai < ni) !== below) break;
    from = i;
  }
  return { from: from, to: lastIdx, below: below };
}

function ecCicloSetPrecipChrome(canvasId, data, on) {
  var canvas = document.getElementById(canvasId);
  var card = canvas ? canvas.closest('.ec-rp-ciclo-card') : null;
  var titleEl = document.getElementById(canvasId + '_title');
  var kpisEl = document.getElementById(canvasId + '_kpis');
  var srEl = document.getElementById(canvasId + '_sr');
  var footEl = document.getElementById(canvasId + '_foot');
  var barsWrap = document.getElementById(canvasId + '_bars_wrap');
  var lineLabel = document.getElementById(canvasId + '_line_label');
  var barsCanvas = document.getElementById(canvasId + '_bars');
  if (card) card.classList.toggle('is-pp-anual', !!on);
  if (barsWrap) barsWrap.hidden = !on;
  if (lineLabel) lineLabel.hidden = !on;
  if (titleEl) titleEl.classList.toggle('ec-rp-ciclo-finding', !!on);
  if (!on) {
    if (kpisEl) kpisEl.textContent = '';
    if (srEl) srEl.textContent = '';
    if (footEl) footEl.textContent = '';
    return;
  }
  var unit = data.unidad || 'mm';
  var anio = data.anio || new Date().getFullYear();
  var lastIdx = ecCicloLastIdx(data.acumuladoMensual || [], data.currentMonth);
  var kpis = ecCicloPrecipKpis(data.acumuladoMensual, data.anioNormal, data.anioPasado, lastIdx);
  var headline = ecCicloPrecipHeadline(kpis, anio);
  if (titleEl) titleEl.textContent = headline;
  if (lineLabel) lineLabel.textContent = 'Acumulado, ' + unit;
  if (barsWrap) {
    var monthLabel = barsWrap.querySelector('.ec-rp-ciclo-panel-label');
    if (monthLabel) monthLabel.textContent = 'Por mes, ' + unit;
  }
  if (kpisEl) {
    var cells = [
      ['Acumulado', kpis.acum === null ? '\u2014' : ecFmtNum(kpis.acum, 0) + ' ' + unit]
    ];
    if (kpis.pct !== null) {
      cells.push(['vs normal', ecCicloFmtSigned(kpis.deltaNormal) + ' ' + unit + ' (' + ecCicloFmtSigned(kpis.pct) + ' %)']);
    }
    cells.push(['vs ' + (anio - 1), kpis.deltaPasado === null ? '\u2014' : ecCicloFmtSigned(kpis.deltaPasado) + ' ' + unit]);
    kpisEl.innerHTML = cells.map(function(c) {
      return '<div class="ec-rp-ciclo-kpi"><span class="ec-rp-ciclo-kpi-label">' + c[0] + '</span><b>' + c[1] + '</b></div>';
    }).join('');
  }
  var summary = headline;
  if (kpis.acum !== null) summary += '. Acumulado ' + ecFmtNum(kpis.acum, 0) + ' ' + unit;
  if (kpis.deltaNormal !== null) summary += '. Versus la normal ' + ecCicloFmtSigned(kpis.deltaNormal) + ' ' + unit;
  if (kpis.deltaPasado !== null) summary += '. Versus ' + (anio - 1) + ' ' + ecCicloFmtSigned(kpis.deltaPasado) + ' ' + unit;
  if (data.corte) summary += '. Corte ' + data.corte;
  summary += '.';
  if (srEl) srEl.textContent = summary;
  if (canvas) canvas.setAttribute('aria-label', summary);
  if (barsCanvas) barsCanvas.setAttribute('aria-label', 'Precipitaci\u00f3n por mes. ' + summary);
  var foot = [];
  if (data.corte) foot.push('Corte ' + data.corte);
  var per = data.normalPeriod;
  if (per && per.start && per.end && kpis.normal !== null) {
    foot.push('Normal ' + per.start + '\u2013' + per.end);
  }
  if (data.monthPartial) foot.push('* mes en curso');
  if (footEl) footEl.textContent = foot.join(' \u00b7 ');
}

var _ecCicloPluginsOn = false;

var ecCicloDeficitPlugin = {
  id: 'ecCicloDeficit',
  beforeDatasetsDraw: function(chart) {
    var cfg = chart.options.plugins && chart.options.plugins.ecCicloDeficit;
    if (!cfg || cfg.from == null || cfg.to == null || cfg.actualIndex == null || cfg.normalIndex == null) return;
    var metaA = chart.getDatasetMeta(cfg.actualIndex);
    var metaN = chart.getDatasetMeta(cfg.normalIndex);
    if (!metaA || !metaN) return;
    var ctx = chart.ctx;
    ctx.save();
    ctx.beginPath();
    var started = false;
    var i;
    for (i = cfg.from; i <= cfg.to; i++) {
      var a = metaA.data[i];
      if (!a || a.skip) continue;
      if (!started) { ctx.moveTo(a.x, a.y); started = true; }
      else ctx.lineTo(a.x, a.y);
    }
    for (i = cfg.to; i >= cfg.from; i--) {
      var n = metaN.data[i];
      if (!n || n.skip) continue;
      ctx.lineTo(n.x, n.y);
    }
    if (started) {
      ctx.closePath();
      ctx.fillStyle = cfg.below ? 'rgba(223, 149, 49, 0.22)' : 'rgba(43, 175, 110, 0.20)';
      ctx.fill();
    }
    var a1 = metaA.data[cfg.to];
    var n1 = metaN.data[cfg.to];
    if (cfg.label && a1 && n1 && !a1.skip && !n1.skip) {
      var lx = a1.x - 8;
      var ly = (a1.y + n1.y) / 2;
      var area = chart.chartArea;
      if (!area || lx - 52 >= area.left) {
        ctx.fillStyle = cfg.below ? '#DF9531' : '#2BAF6E';
        ctx.font = '600 13px Inter, sans-serif';
        ctx.textAlign = 'right';
        ctx.textBaseline = 'middle';
        ctx.fillText(cfg.label, lx, ly);
      }
    }
    ctx.restore();
  }
};

var ecCicloEndLabelsPlugin = {
  id: 'ecCicloEndLabels',
  afterDraw: function(chart) {
    var cfg = chart.options.plugins && chart.options.plugins.ecCicloEndLabels;
    if (!cfg || !cfg.labels) return;
    var area = chart.chartArea;
    var inline = !!cfg.inline;
    var placed = [];
    cfg.labels.forEach(function(spec) {
      var meta = chart.getDatasetMeta(spec.datasetIndex);
      if (!meta || meta.hidden) return;
      var pt = null;
      for (var i = meta.data.length - 1; i >= 0; i--) {
        var el = meta.data[i];
        if (!el || el.skip) continue;
        if (ecCicloNum(chart.data.datasets[spec.datasetIndex].data[i]) === null) continue;
        pt = el;
        break;
      }
      if (!pt) return;
      placed.push({ x: pt.x, y: pt.y, text: spec.text, color: spec.color });
    });
    placed.sort(function(a, b) { return a.y - b.y; });
    var minY = (area ? area.top : 0) + 8;
    placed.forEach(function(p) {
      if (p.y < minY) p.y = minY;
      minY = p.y + 15;
    });
    var ctx = chart.ctx;
    ctx.save();
    ctx.font = '600 13px Inter, sans-serif';
    ctx.textBaseline = 'middle';
    placed.forEach(function(p) {
      ctx.fillStyle = p.color;
      if (inline) {
        ctx.textAlign = 'right';
        ctx.fillText(p.text, p.x - 6, p.y);
      } else {
        ctx.textAlign = 'left';
        ctx.fillText(p.text, p.x + 8, p.y);
      }
    });
    ctx.restore();
  }
};

var ecCicloBarsPlugin = {
  id: 'ecCicloBars',
  beforeDatasetsDraw: function(chart) {
    var cfg = chart.options.plugins && chart.options.plugins.ecCicloBars;
    if (!cfg || cfg.futureFrom == null) return;
    var xScale = chart.scales.x;
    var area = chart.chartArea;
    if (!xScale || !area) return;
    var step = 0;
    if (chart.data.labels && chart.data.labels.length > 1) {
      step = Math.abs(xScale.getPixelForValue(1) - xScale.getPixelForValue(0));
    }
    var x0 = xScale.getPixelForValue(cfg.futureFrom) - step / 2;
    if (x0 < area.left) x0 = area.left;
    if (x0 >= area.right - 4) return;
    var ctx = chart.ctx;
    ctx.save();
    ctx.fillStyle = 'rgba(57, 69, 83, 0.07)';
    ctx.fillRect(x0, area.top, area.right - x0, area.bottom - area.top);
    if (cfg.showLabel !== false && area.right - x0 > 72) {
      ctx.fillStyle = '#394553';
      ctx.font = '600 13px Inter, sans-serif';
      ctx.textAlign = 'center';
      ctx.textBaseline = 'middle';
      ctx.fillText('Por registrar', (x0 + area.right) / 2, (area.top + area.bottom) / 2);
    }
    ctx.restore();
  },
  afterDatasetsDraw: function(chart) {
    var cfg = chart.options.plugins && chart.options.plugins.ecCicloBars;
    if (!cfg) return;
    var ctx = chart.ctx;
    ctx.save();
    ctx.font = '600 13px Inter, sans-serif';
    ctx.fillStyle = '#394553';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'bottom';
    chart.data.datasets.forEach(function(ds, di) {
      var meta = chart.getDatasetMeta(di);
      if (!meta || meta.type !== 'bar') return;
        meta.data.forEach(function(el, i) {
        var v = ecCicloNum(ds.data[i]);
        if (!el || el.skip || v === null) return;
        var star = cfg.partialIndex === i ? '*' : '';
        if ((el.width || 0) >= 18 || star) ctx.fillText(ecFmtNum(v, 0) + star, el.x, el.y - 4);
        if (cfg.partialIndex !== i) return;
        var w = el.width || 16;
        var top = Math.min(el.y, el.base);
        var h = Math.abs((el.base || el.y) - el.y);
        ctx.save();
        ctx.strokeStyle = '#2BAF6E';
        ctx.lineWidth = 1.5;
        ctx.setLineDash([3, 3]);
        ctx.strokeRect(el.x - w / 2, top, w, Math.max(h, 2));
        ctx.restore();
      });
    });
    ctx.restore();
  }
};

function ecEnsureCicloPlugins() {
  if (_ecCicloPluginsOn || typeof Chart === 'undefined') return;
  Chart.register(ecCicloDeficitPlugin, ecCicloEndLabelsPlugin, ecCicloBarsPlugin);
  _ecCicloPluginsOn = true;
}

function ecCicloSplitLayout(canvas) {
  var w = (canvas && canvas.clientWidth) || 640;
  var narrow = w < 520;
  return {
    narrow: narrow,
    // ponytail: mismo ancho de eje en los dos paneles; en móvil no reserva
    // margen para etiquetas al costado porque no cabe el área de dibujo
    yWidth: narrow ? 48 : 68,
    right: narrow ? 8 : 78
  };
}

function ecCicloSplitY(axisMax, yWidth) {
  var scale = {
    beginAtZero: true,
    grid: { color: 'rgba(57, 69, 83, 0.16)' },
    border: { display: false },
    ticks: {
      color: '#394553',
      font: { size: 14, family: "'Inter', sans-serif" },
      maxTicksLimit: 4,
      callback: function(v) { return ecFmtAxis(v); }
    },
    afterFit: function(s) { s.width = yWidth; }
  };
  if (axisMax) scale.max = axisMax;
  return scale;
}

function ecCicloSplitX(showLabels, narrow) {
  return {
    offset: true,
    grid: { display: false },
    ticks: {
      display: !!showLabels,
      color: '#394553',
      font: { size: 14, family: "'Inter', sans-serif" },
      maxRotation: 0,
      autoSkip: !!narrow,
      maxTicksLimit: narrow ? 6 : 12
    }
  };
}

function ecCicloSeriesMax(arr) {
  var m = 0;
  (arr || []).forEach(function(v) {
    var n = ecCicloNum(v);
    if (n !== null && n > m) m = n;
  });
  return m;
}

function ecBuildCicloPrecipAnual(canvas, data) {
  ecEnsureChartLocale();
  ecEnsureCicloPlugins();
  var barsCanvas = document.getElementById(canvas.id + '_bars');
  if (!barsCanvas) return null;

  var labels = data.meses || [];
  var unit = data.unidad || 'mm';
  var anio = data.anio || new Date().getFullYear();
  var currentMonth = data.currentMonth || new Date().getMonth() + 1;
  var parcial = ecCleanSeries(data.parcialMensual || []);
  var acum = ecCleanSeries(data.acumuladoMensual || []);
  var pasado = ecCleanSeries(data.anioPasado || []);
  var normal = ecCleanSeries(data.anioNormal || []);
  var lastIdx = ecCicloLastIdx(acum, currentMonth);
  var kpis = ecCicloPrecipKpis(acum, normal, pasado, lastIdx);
  var gap = ecCicloGapSpan(acum, normal, lastIdx);
  var partialIndex = (data.monthPartial && currentMonth >= 1 && currentMonth <= 12)
    ? currentMonth - 1 : null;
  var futureFrom = currentMonth < labels.length ? currentMonth : null;

  var datasets = [];
  var endLabels = [];
  var normalIndex = null;
  var actualIndex = null;

  function radii(arr, markLast) {
    return (arr || []).map(function(v, i) {
      if (ecCicloNum(v) === null) return 0;
      return markLast && i === lastIdx ? 5 : 0;
    });
  }

  if (ecCicloHasData(pasado)) {
    endLabels.push({ datasetIndex: datasets.length, text: String(anio - 1), color: '#6E7C8C' });
    datasets.push({
      type: 'line',
      label: String(anio - 1),
      data: pasado,
      borderColor: '#6E7C8C',
      backgroundColor: '#6E7C8C',
      borderWidth: 1.25,
      pointRadius: radii(pasado, false),
      pointHitRadius: 8,
      tension: 0,
      spanGaps: false,
      order: 3
    });
  }
  if (ecCicloHasData(normal)) {
    normalIndex = datasets.length;
    endLabels.push({ datasetIndex: datasets.length, text: 'Normal', color: '#394553' });
    datasets.push({
      type: 'line',
      label: 'Normal',
      data: normal,
      borderColor: '#394553',
      backgroundColor: '#394553',
      borderWidth: 1.5,
      borderDash: [2, 3],
      pointRadius: radii(normal, false),
      pointHitRadius: 8,
      tension: 0,
      spanGaps: false,
      order: 2
    });
  }
  if (ecCicloHasData(acum)) {
    actualIndex = datasets.length;
    endLabels.push({ datasetIndex: datasets.length, text: String(anio), color: '#DF9531' });
    datasets.push({
      type: 'line',
      label: String(anio),
      data: acum,
      borderColor: '#DF9531',
      backgroundColor: '#DF9531',
      borderWidth: 3,
      pointRadius: radii(acum, true),
      pointBackgroundColor: '#DF9531',
      pointBorderColor: '#ffffff',
      pointBorderWidth: radii(acum, true).map(function(r) { return r ? 2 : 0; }),
      pointHitRadius: 8,
      tension: 0,
      spanGaps: false,
      order: 1
    });
  }
  if (!datasets.length && !ecCicloHasData(parcial)) return null;

  var lineMax = Math.max(ecCicloSeriesMax(acum), ecCicloSeriesMax(pasado), ecCicloSeriesMax(normal));
  var barMax = ecCicloSeriesMax(parcial);
  var lay = ecCicloSplitLayout(canvas);
  var padX = { left: 4, right: lay.right };
  var tip = {
    callbacks: {
      label: function(ctx) {
        var v = ctx.parsed.y;
        var lbl = ctx.dataset.label || '';
        if (v === null || isNaN(v)) return lbl + ': \u2014';
        var star = partialIndex === ctx.dataIndex && ctx.dataset.type === 'bar' ? '*' : '';
        return lbl + ': ' + ecFmtNum(v, Math.abs(v) >= 100 ? 0 : 1) + star + (unit ? ' ' + unit : '');
      }
    }
  };

  var line = new Chart(canvas.getContext('2d'), {
    type: 'line',
    data: { labels: labels, datasets: datasets },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      animation: false,
      interaction: { mode: 'index', intersect: false },
      layout: { padding: { left: padX.left, right: padX.right, top: 8, bottom: 2 } },
      plugins: {
        legend: { display: false },
        tooltip: tip,
        ecCicloEndLabels: { labels: endLabels, inline: lay.narrow },
        ecCicloDeficit: (gap && actualIndex !== null && normalIndex !== null) ? {
          actualIndex: actualIndex,
          normalIndex: normalIndex,
          from: gap.from,
          to: gap.to,
          below: gap.below,
          label: lay.narrow ? null : ecCicloFmtSigned(kpis.deltaNormal) + ' ' + unit
        } : false,
        ecCicloBars: { futureFrom: futureFrom, showLabel: false }
      },
      scales: {
        x: ecCicloSplitX(false, lay.narrow),
        y: ecCicloSplitY(lineMax > 0 ? ecRoundAxisMax(lineMax * 1.12) : null, lay.yWidth)
      }
    }
  });

  var barColors = parcial.map(function(v, i) {
    if (ecCicloNum(v) === null) return 'transparent';
    return partialIndex === i ? 'rgba(43, 175, 110, 0.45)' : '#2BAF6E';
  });
  var bars = new Chart(barsCanvas.getContext('2d'), {
    type: 'bar',
    data: {
      labels: labels,
      datasets: [{
        type: 'bar',
        label: 'Mes',
        data: parcial,
        backgroundColor: barColors,
        borderWidth: 0,
        borderRadius: 3,
        maxBarThickness: 36,
        categoryPercentage: 0.72,
        barPercentage: 0.9
      }]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      animation: false,
      interaction: { mode: 'index', intersect: false },
      layout: { padding: { left: padX.left, right: padX.right, top: 18, bottom: 2 } },
      plugins: {
        legend: { display: false },
        tooltip: tip,
        ecCicloBars: { futureFrom: futureFrom, partialIndex: partialIndex }
      },
      scales: {
        x: ecCicloSplitX(true, lay.narrow),
        y: ecCicloSplitY(barMax > 0 ? ecRoundAxisMax(barMax * 1.35) : null, lay.yWidth)
      }
    }
  });

  return {
    destroy: function() {
      line.destroy();
      bars.destroy();
    }
  };
}

function ecBuildCicloAnualChart(canvas, data) {
  if (!data || !data.meses) return null;
  if (data.variant === 'pp' && document.getElementById(canvas.id + '_bars')) {
    return ecBuildCicloPrecipAnual(canvas, data);
  }
  ecEnsureChartLocale();

  var ctx = canvas.getContext('2d');
  var fontCfg = { size: 14, family: "'Inter', sans-serif" };
  var gridColor = 'rgba(0,0,0,0.06)';
  var unit = data.unidad || '';
  var currentMonth = data.currentMonth || new Date().getMonth() + 1;
  var isMobile = ecIsMobileLayout();
  var labels = data.meses;

  var datasets = [];
  var yMax = 0;

  function trackMax(arr) {
    if (!arr) return;
    arr.forEach(function(v) {
      if (v !== null && v !== undefined && !isNaN(v) && v > yMax) yMax = v;
    });
  }

  if (data.variant === 'pp') {
    var parcial = ecCleanSeries(data.parcialMensual || []);
    var acum = ecCleanSeries(data.acumuladoMensual || []);
    var pasado = ecCleanSeries(data.anioPasado || []);
    var normal = ecCleanSeries(data.anioNormal || []);

    trackMax(parcial); trackMax(acum); trackMax(pasado); trackMax(normal);

    if (ecCicloHasData(normal)) {
      datasets.push({
        type: 'line',
        label: 'A\u00f1o Normal',
        data: normal,
        borderColor: '#394553',
        backgroundColor: '#394553',
        borderWidth: 2,
        pointStyle: 'rect',
        pointRadius: ecCicloPointRadii(normal, currentMonth, 4),
        pointBorderWidth: ecCicloPointBorderWidths(normal, currentMonth),
        tension: 0.2,
        spanGaps: false,
        order: 4
      });
    }
    if (ecCicloHasData(pasado)) {
      datasets.push({
        type: 'line',
        label: 'A\u00f1o Pasado',
        data: pasado,
        borderColor: '#6E7C8C',
        backgroundColor: '#6E7C8C',
        borderWidth: 2,
        pointRadius: ecCicloPointRadii(pasado, currentMonth, 3),
        pointBorderWidth: ecCicloPointBorderWidths(pasado, currentMonth),
        tension: 0.2,
        spanGaps: false,
        order: 3
      });
    }
    if (ecCicloHasData(acum)) {
      datasets.push({
        type: 'line',
        label: 'Acumulado Mensual',
        data: acum,
        borderColor: '#DF9531',
        backgroundColor: '#DF9531',
        borderWidth: 2.5,
        pointRadius: ecCicloPointRadii(acum, currentMonth, 3),
        pointBorderWidth: ecCicloPointBorderWidths(acum, currentMonth),
        tension: 0.2,
        spanGaps: false,
        order: 2
      });
    }
    if (ecCicloHasData(parcial)) {
      var barColors = parcial.map(function(v, i) {
        if (v === null || v === undefined || isNaN(v)) return 'transparent';
        return (i + 1) === currentMonth ? 'rgba(43,175,110,0.75)' : 'rgba(43,175,110,0.55)';
      });
      datasets.push({
        type: 'bar',
        label: 'Parcial Mensual',
        data: parcial,
        backgroundColor: barColors,
        borderColor: barColors,
        borderWidth: 1,
        borderRadius: 3,
        maxBarThickness: 28,
        order: 1
      });
    }
  } else {
    var mediaNorm = ecCleanSeries(data.mediaNormal || []);
    var mediaPas = ecCleanSeries(data.mediaAnioPasado || []);
    var mediaAct = ecCleanSeries(data.mediaAnioActual || []);

    trackMax(mediaNorm); trackMax(mediaPas); trackMax(mediaAct);

    if (ecCicloHasData(mediaNorm)) {
      datasets.push({
        type: 'line',
        label: 'A\u00f1o Normal',
        data: mediaNorm,
        borderColor: '#394553',
        backgroundColor: '#394553',
        borderWidth: 2,
        pointStyle: 'rect',
        pointRadius: ecCicloPointRadii(mediaNorm, currentMonth, 4),
        pointBorderWidth: ecCicloPointBorderWidths(mediaNorm, currentMonth),
        tension: 0.2,
        spanGaps: false,
        order: 3
      });
    }
    if (ecCicloHasData(mediaPas)) {
      datasets.push({
        type: 'line',
        label: 'A\u00f1o Pasado',
        data: mediaPas,
        borderColor: '#6E7C8C',
        backgroundColor: '#6E7C8C',
        borderWidth: 2,
        pointRadius: ecCicloPointRadii(mediaPas, currentMonth, 3),
        pointBorderWidth: ecCicloPointBorderWidths(mediaPas, currentMonth),
        tension: 0.2,
        spanGaps: false,
        order: 2
      });
    }
    if (ecCicloHasData(mediaAct)) {
      datasets.push({
        type: 'line',
        label: 'A\u00f1o Actual',
        data: mediaAct,
        borderColor: '#DF9531',
        backgroundColor: '#DF9531',
        borderWidth: 2.5,
        pointRadius: ecCicloPointRadii(mediaAct, currentMonth, 4),
        pointBorderWidth: ecCicloPointBorderWidths(mediaAct, currentMonth),
        tension: 0.2,
        spanGaps: false,
        order: 1
      });
    }
  }

  if (datasets.length === 0) return null;

  var yScale = ecCicloApplyYScale(data, yMax, {
    beginAtZero: true,
    grid: { color: gridColor },
    ticks: {
      font: fontCfg,
      callback: function(v) { return ecFmtAxis(v); }
    },
    title: {
      display: !!unit,
      text: unit,
      font: { size: 13, family: "'Inter', sans-serif" }
    }
  });

  return new Chart(ctx, {
    type: 'bar',
    data: { labels: labels, datasets: datasets },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      interaction: { mode: 'index', intersect: false },
      plugins: {
        legend: {
          position: isMobile ? 'bottom' : 'top',
          align: isMobile ? 'center' : 'end',
          labels: {
            boxWidth: 14,
            padding: 10,
            font: fontCfg,
            usePointStyle: true
          }
        },
        tooltip: {
          callbacks: {
            label: function(ctx) {
              var v = ctx.parsed.y;
              var lbl = ctx.dataset.label || '';
              if (v === null || isNaN(v)) return lbl + ': \u2014';
              var fmt = ecFmtNum(v, Math.abs(v) >= 100 ? 0 : 1);
              return lbl + ': ' + fmt + (unit ? ' ' + unit : '');
            }
          }
        }
      },
      scales: {
        x: {
          grid: { display: false },
          ticks: {
            font: fontCfg,
            maxRotation: isMobile ? 45 : 0,
            minRotation: isMobile ? 45 : 0,
            color: '#394553'
          }
        },
        y: yScale
      }
    }
  });
}

// ---- Período de referencia normal (client-side, sin recargar reporte) ----
var EC_NORMAL_LS_PREFIX = 'ecNormalPeriod:';

function ecNormalDaysInMonth(year, month) {
  return new Date(year, month, 0).getDate();
}

function ecNormalFormatLabel(start, end) {
  return String(start) + '\u2013' + String(end);
}

function ecNormalSubtitleWithSuffix(base, suffix) {
  var b = String(base || '').replace(/\s*\u00b7\s*Normal\s+[\d\u2013\-]+/g, '').trim();
  return suffix ? b + suffix : b;
}

function ecNormalComputeNormM(src, start, end) {
  if (!src || !src.byYear) return null;
  var minYears = src.minYears || 5;
  var out = [];
  for (var m = 1; m <= 12; m++) {
    var vals = [];
    Object.keys(src.byYear).forEach(function(ys) {
      var y = parseInt(ys, 10);
      if (isNaN(y) || y < start || y > end) return;
      var row = src.byYear[ys];
      var v = Array.isArray(row) ? row[m - 1] : null;
      if (v !== null && v !== undefined && !isNaN(v)) vals.push(v);
    });
    out.push(vals.length >= minYears ? vals.reduce(function(a, b) { return a + b; }, 0) / vals.length : null);
  }
  return out;
}

function ecNormalCumsum(vec) {
  var running = 0;
  return (vec || []).map(function(v) {
    if (v === null || v === undefined || isNaN(v)) return null;
    running += v;
    return running;
  });
}

function ecNormalYtdPrecip(normM, year, targetDate) {
  if (!normM || normM.length !== 12 || !targetDate) return null;
  var d = targetDate instanceof Date ? targetDate : new Date(targetDate);
  if (isNaN(d.getTime())) return null;
  var mes = d.getMonth() + 1;
  var dia = d.getDate();
  var cum = 0;
  var has = false;
  if (mes > 1) {
    for (var i = 0; i < mes - 1; i++) {
      if (normM[i] !== null && normM[i] !== undefined && !isNaN(normM[i])) {
        cum += normM[i];
        has = true;
      }
    }
  }
  var mVal = normM[mes - 1];
  if (mVal !== null && mVal !== undefined && !isNaN(mVal)) {
    var diasMes = ecNormalDaysInMonth(year, mes);
    if (diasMes > 0) {
      cum += mVal * (dia / diasMes);
      has = true;
    }
  }
  return has ? cum : null;
}

function ecNormalYtdMean(normM, year, targetDate) {
  if (!normM || normM.length !== 12 || !targetDate) return null;
  var d = targetDate instanceof Date ? targetDate : new Date(targetDate);
  if (isNaN(d.getTime())) return null;
  var mes = d.getMonth() + 1;
  var dia = d.getDate();
  var weighted = 0;
  var total = 0;
  for (var m = 1; m < mes; m++) {
    if (normM[m - 1] !== null && normM[m - 1] !== undefined && !isNaN(normM[m - 1])) {
      weighted += normM[m - 1] * ecNormalDaysInMonth(year, m);
      total += ecNormalDaysInMonth(year, m);
    }
  }
  if (normM[mes - 1] !== null && normM[mes - 1] !== undefined && !isNaN(normM[mes - 1])) {
    weighted += normM[mes - 1] * dia;
    total += dia;
  }
  return total > 0 ? weighted / total : null;
}

function ecNormalAnnualScalar(src, start, end) {
  if (!src || !src.byYear) return null;
  var minYears = src.minYears || 5;
  var isPrecip = src.vtype === 'precip';
  var annual = [];
  Object.keys(src.byYear).forEach(function(ys) {
    var y = parseInt(ys, 10);
    if (isNaN(y) || y < start || y > end) return;
    var row = src.byYear[ys];
    if (!Array.isArray(row)) return;
    var vals = row.filter(function(v) { return v !== null && v !== undefined && !isNaN(v); });
    if (vals.length < 12) return;
    annual.push(isPrecip
      ? vals.reduce(function(a, b) { return a + b; }, 0)
      : vals.reduce(function(a, b) { return a + b; }, 0) / vals.length);
  });
  if (annual.length < minYears) return null;
  return annual.reduce(function(a, b) { return a + b; }, 0) / annual.length;
}

function ecNormalYearOptions(years) {
  return years.map(function(y) {
    return '<option value="' + y + '">' + y + '</option>';
  }).join('');
}

function ecNormalMountSelectors(mount, years, start, end, onChange) {
  if (!mount || !years.length) return;
  var bare = mount.classList && mount.classList.contains('ec-rp-ts-period');
  var startLabel = bare ? 'A\u00f1o inicio del per\u00edodo de referencia' : 'A\u00f1o inicio periodo normal';
  var endLabel = bare ? 'A\u00f1o fin del per\u00edodo de referencia' : 'A\u00f1o fin periodo normal';
  mount.innerHTML =
    (bare ? '' :
      '<span class="ec-legend-line ec-legend-normal" aria-hidden="true"></span>' +
      '<span class="ec-normal-period-label">Normal clim\u00e1tica</span>') +
    '<select class="ec-normal-start" aria-label="' + startLabel + '">' +
    ecNormalYearOptions(years) + '</select>' +
    '<span class="ec-normal-period-sep" aria-hidden="true">\u2013</span>' +
    '<select class="ec-normal-end" aria-label="' + endLabel + '">' +
    ecNormalYearOptions(years) + '</select>';
  var startEl = mount.querySelector('.ec-normal-start');
  var endEl = mount.querySelector('.ec-normal-end');
  startEl.value = String(start);
  endEl.value = String(end);
  function syncEnd() {
    var s = parseInt(startEl.value, 10);
    var e = parseInt(endEl.value, 10);
    if (e < s) endEl.value = String(s);
    onChange(s, parseInt(endEl.value, 10));
  }
  startEl.addEventListener('change', function() {
    var s = parseInt(startEl.value, 10);
    var e = parseInt(endEl.value, 10);
    if (e < s) endEl.value = String(s);
    syncEnd();
  });
  endEl.addEventListener('change', syncEnd);
}

function ecNormalApplyPeriod(start, end, opts) {
  opts = opts || {};
  var clim = ecNormalGetClimatology();
  if (!clim || !clim.byYear) return;
  start = parseInt(start, 10);
  end = parseInt(end, 10);
  if (isNaN(start) || isNaN(end) || end < start) return;

  var normM = ecNormalComputeNormM(clim, start, end);
  var hasNormal = normM && normM.some(function(v) { return v !== null && !isNaN(v); });
  var suffix = hasNormal ? ' \u00b7 Normal ' + ecNormalFormatLabel(start, end) : '';
  var normalLabel = hasNormal ? 'Normal ' + ecNormalFormatLabel(start, end) : 'Normal';
  var period = { start: start, end: end };

  if (window.__ecReportChartData) {
    var cd = window.__ecReportChartData;
    var annualScalar = hasNormal ? ecNormalAnnualScalar(clim, start, end) : null;
    if (cd.annual) {
      cd.annual.normalPeriod = period;
      cd.annual._normalLabel = normalLabel;
      if (annualScalar !== null && cd.annual.labels && cd.annual.labels.length) {
        cd.annual.normal_mean = cd.annual.labels.map(function() { return annualScalar; });
      } else {
        cd.annual.normal_mean = null;
      }
      var ytdN = null;
      if (hasNormal && cd.annual.last_date) {
        var ytdDate = ecParseYmd(cd.annual.last_date);
        var ytdYear = ytdDate ? ytdDate.getFullYear() : new Date().getFullYear();
        ytdN = clim.vtype === 'precip'
          ? ecNormalYtdPrecip(normM, ytdYear, ytdDate)
          : ecNormalYtdMean(normM, ytdYear, ytdDate);
      }
      cd.annual.normal_mean_ytd = (ytdN !== null && cd.annual.labels && cd.annual.labels.length)
        ? cd.annual.labels.map(function() { return ytdN; })
        : null;
    }
    if (cd.monthly) {
      cd.monthly.normalPeriod = period;
      cd.monthly._normalLabel = normalLabel;
      if (hasNormal && cd.monthly.month && cd.monthly.month.length) {
        cd.monthly.normal_mean = cd.monthly.month.map(function(m) {
          var i = parseInt(m, 10) - 1;
          return (i >= 0 && i < normM.length) ? normM[i] : null;
        });
      } else {
        cd.monthly.normal_mean = hasNormal ? normM : null;
      }
    }
    cd.normalPeriod = period;
    if (opts.chartId) {
      var card = document.getElementById(opts.chartId);
      card = card ? card.closest('.ec-rp-ts-card') : null;
      var res = _ecChartResolution[opts.chartId] || 'anual';
      var btn = card ? card.querySelector('[data-ts-res="' + res + '"]') : null;
      if (!btn && card) btn = card.querySelector('.pill.active');
      if (btn) ecTSSetRes(btn, opts.chartId, res);
    }
  }

  if (window.__ecReportCicloData) {
    var ciclo = window.__ecReportCicloData;
    ciclo.normalPeriod = period;
    delete ciclo.ySuggestedMax;
    delete ciclo.ySuggestedMin;
    if (hasNormal) {
      if (ciclo.variant === 'pp') ciclo.anioNormal = ecNormalCumsum(normM);
      else ciclo.mediaNormal = normM;
      ciclo.subtitle = ecNormalSubtitleWithSuffix(ciclo.subtitle, suffix);
    } else {
      ciclo.anioNormal = null;
      ciclo.mediaNormal = null;
      ciclo.subtitle = ecNormalSubtitleWithSuffix(ciclo.subtitle, '');
    }
    if (ciclo.mesActual) {
      var ma = ciclo.mesActual;
      var anio = ciclo.anio || new Date().getFullYear();
      var month = ciclo.currentMonth || new Date().getMonth() + 1;
      delete ma.ySuggestedMax;
      delete ma.ySuggestedMin;
      if (hasNormal && ma.labels && ma.labels.length) {
        var rebuilt = ma.labels.map(function(lbl) {
          var day = parseInt(lbl, 10);
          var target = new Date(anio, month - 1, day);
          return ma.variant === 'pp'
            ? ecNormalYtdPrecip(normM, anio, target)
            : ecNormalYtdMean(normM, anio, target);
        });
        if (ma.variant === 'pp') ma.anioNormal = rebuilt;
        else ma.mediaNormal = rebuilt;
      } else {
        ma.anioNormal = null;
        ma.mediaNormal = null;
      }
      ma.subtitle = ecNormalSubtitleWithSuffix(ma.subtitle, suffix);
    }
    if (opts.cicloId) {
      ecInitCicloAnualChart(opts.cicloId, ciclo, _ecCicloChartMode[opts.cicloId] || 'anual');
    }
  }

  if (!opts.skipStorage && opts.eid && opts.vid) {
    try {
      localStorage.setItem(EC_NORMAL_LS_PREFIX + opts.eid + ':' + opts.vid, JSON.stringify(period));
    } catch (e) { /* ignore */ }
  }
}

function ecNormalGetClimatology() {
  if (window.__ecReportNormalClimatology && window.__ecReportNormalClimatology.byYear) {
    return window.__ecReportNormalClimatology;
  }
  var root = document.querySelector('.ec-reporte-contenido');
  var el = root ? root.querySelector('script.ec-report-data-json[type="application/json"]') : null;
  if (!el || !el.textContent) return null;
  try {
    var payload = JSON.parse(el.textContent);
    if (payload && payload.normalClimatology) {
      window.__ecReportNormalClimatology = payload.normalClimatology;
      return payload.normalClimatology;
    }
  } catch (e) { /* ignore */ }
  return null;
}

function ecNormalGetMountIds(opts) {
  var ids = [];
  if (opts && opts.cicloId) ids.push(opts.cicloId + '_normal_period');
  if (opts && opts.chartId) ids.push(opts.chartId + '_ts_normal_period');
  return ids;
}

function ecNormalSyncMounts(mountIds, start, end) {
  mountIds.forEach(function(mid) {
    var mount = document.getElementById(mid);
    if (!mount) return;
    var startEl = mount.querySelector('.ec-normal-start');
    var endEl = mount.querySelector('.ec-normal-end');
    if (startEl) startEl.value = String(start);
    if (endEl) endEl.value = String(end);
  });
}

function ecNormalInitReport(opts) {
  opts = opts || {};
  var clim = ecNormalGetClimatology();
  if (!clim || !clim.byYear || !Object.keys(clim.byYear).length) return;
  var years = (clim.years || Object.keys(clim.byYear)).map(function(y) { return parseInt(y, 10); })
    .filter(function(y) { return !isNaN(y); })
    .sort(function(a, b) { return a - b; });
  if (!years.length) return;
  var minY = years[0];
  var maxY = years[years.length - 1];
  var defStart = clim.defaultStart || minY;
  var defEnd = clim.defaultEnd || maxY;
  var saved = null;
  if (opts.eid && opts.vid) {
    try {
      saved = JSON.parse(localStorage.getItem(EC_NORMAL_LS_PREFIX + opts.eid + ':' + opts.vid) || 'null');
    } catch (e) { /* ignore */ }
  }
  var start = saved && saved.start ? parseInt(saved.start, 10) : defStart;
  var end = saved && saved.end ? parseInt(saved.end, 10) : defEnd;
  start = Math.max(minY, Math.min(maxY, start));
  end = Math.max(start, Math.min(maxY, end));

  var mountIds = ecNormalGetMountIds(opts);
  mountIds.forEach(function(mid) {
    var mount = document.getElementById(mid);
    if (!mount) return;
    ecNormalMountSelectors(mount, years, start, end, function(s, e) {
      ecNormalSyncMounts(mountIds, s, e);
      ecNormalApplyPeriod(s, e, {
        chartId: opts.chartId,
        cicloId: opts.cicloId,
        eid: opts.eid,
        vid: opts.vid
      });
    });
  });

  if (saved || start !== defStart || end !== defEnd) {
    ecNormalApplyPeriod(start, end, {
      chartId: null,
      cicloId: null,
      eid: opts.eid,
      vid: opts.vid,
      skipStorage: !saved && start === defStart && end === defEnd
    });
  }
}

if (typeof window === 'undefined') {
  var _ts = ecTsAnnualModel({
    labels: ['2009', '2010', '2012', '2013'],
    values: [10, 100, 300, 50],
    coverage: [100, 100, 90, 40],
    start: 2010,
    end: 2013,
    currentYear: 2026,
    partialYear: false,
    useYtd: false,
    coverageMin: 70,
    vtype: 'precip'
  });
  if (_ts.slots.length !== 5 || _ts.slots[2].year !== 2011 || _ts.slots[2].value !== null) {
    throw new Error('ecTsAnnualModel: no insertó el hueco de 2011');
  }
  if (Math.abs(_ts.mean - 200) > 1e-6) throw new Error('ecTsAnnualModel: media ' + _ts.mean);
  if (_ts.yMin !== 0) throw new Error('ecTsAnnualModel: eje ' + _ts.yMin);
  if (!_ts.max || _ts.max.year !== 2012) throw new Error('ecTsAnnualModel: máximo');
  if (!_ts.min || _ts.min.year === 2013 || _ts.min.year !== 2009) {
    throw new Error('ecTsAnnualModel: cobertura baja en los extremos');
  }
}

(function () {
  if (ecFmtNum(2500, 0) !== '2.500' && typeof console !== 'undefined') {
    console.error('ecFmtNum self-check failed: ' + ecFmtNum(2500, 0));
  }
  if (ecRoundAxisMax(1811.3) !== 1900 && typeof console !== 'undefined') {
    console.error('ecRoundAxisMax self-check failed: ' + ecRoundAxisMax(1811.3));
  }
})();
