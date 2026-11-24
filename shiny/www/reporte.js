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

function ecUpdateTSLegend(canvasId, resolution) {
  var legend = document.getElementById(canvasId + '_ts_legend');
  var subtitle = document.getElementById(canvasId + '_ts_subtitle');
  var spanEl = document.getElementById(canvasId + '_span');
  var pack = window['_ecTSData_' + canvasId];
  var annual = pack && pack.annual;
  if (legend) {
    legend.style.display = resolution === 'completitud' ? 'none' : '';
  }
  if (spanEl) {
    spanEl.style.display = (resolution === 'anual' && annual && annual.partial_year) ? '' : 'none';
  }
  if (subtitle) {
    var span = _ecChartSpan[canvasId] || 'completo';
    if (resolution === 'completitud') {
      subtitle.textContent = 'Cobertura diaria: d\u00edas con registro / d\u00edas del a\u00f1o';
      subtitle.style.display = '';
    } else if (resolution === 'anual' && span === 'hasta_hoy' && annual && annual.last_label) {
      subtitle.textContent = 'Cada a\u00f1o, solo hasta el ' + annual.last_label + '.';
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

  var attempts = 0;
  function tryInit() {
    var canvas = document.getElementById(canvasId);
    if (!canvas || typeof Chart === 'undefined') {
      if (++attempts < 30) setTimeout(tryInit, 100);
      return;
    }
    if (_ecChartInstances[canvasId]) {
      _ecChartInstances[canvasId].destroy();
      delete _ecChartInstances[canvasId];
    }
    var annualData = data.annual || data;
    _ecChartInstances[canvasId] = ecBuildChart(canvas, annualData, 'anual');
    ecUpdateTSLegend(canvasId, 'anual');
  }
  tryInit();
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

  var canvas = document.getElementById(canvasId);
  if (!canvas) return;

  var data = window['_ecTSData_' + canvasId];
  if (!data) return;

  var resData;
  if (resolution === 'mensual') {
    resData = data.monthly || data;
    resData.flagged_annual = data.flagged_monthly;
  } else if (resolution === 'completitud') {
    resData = data.completeness || { labels: [], values: [] };
  } else {
    resData = data.annual || data;
    resData.flagged_annual = data.flagged_annual;
  }

  if (_ecChartInstances[canvasId]) {
    _ecChartInstances[canvasId].destroy();
    delete _ecChartInstances[canvasId];
  }
  _ecChartResolution[canvasId] = resolution;
  _ecChartInstances[canvasId] = ecBuildChart(canvas, resData, resolution);
  ecUpdateTSLegend(canvasId, resolution);
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

function ecFormatChartValue(v, unit) {
  if (v === null || v === undefined || isNaN(v)) return '\u2014';
  var fmt = (Math.round(v * 10) / 10).toFixed(1).replace('.', ',');
  return unit ? fmt + ' ' + unit : fmt;
}

function ecParseYmd(s) {
  var m = /^(\d{4})-(\d{2})-(\d{2})/.exec(String(s || ''));
  if (!m) return null;
  return new Date(Number(m[1]), Number(m[2]) - 1, Number(m[3]));
}

function ecBuildChart(canvas, data, resolution) {
  if (!data || !data.labels) return null;

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
                if (v === null || isNaN(v)) return '% completitud: —';
                return '% completitud: ' + v.toFixed(1).replace('.', ',') + ' %';
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
              callback: function(v) { return v + ' %'; }
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
                return 'Valor imposible: ' + ecFormatChartValue(v, unit) + ' (excluido de medias)';
              }
              var line = ctx.dataset.label + ': ' + ecFormatChartValue(v, unit);
              if (ctx.dataset.label !== seriesLabel) return line;
              var meta = ecTSMeta(ctx.dataIndex);
              if (meta.low && meta.cov !== null && !isNaN(Number(meta.cov))) {
                line += ' \u00b7 ' + ecFormatChartValue(Number(meta.cov), '%') + ' de d\u00edas con registro';
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
          ticks: { font: fontCfg },
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
function ecInitCicloAnualChart(canvasId, data, mode) {
  if (!data || data === 'null') return;
  window['_ecCicloData_' + canvasId] = data;
  if (!mode) mode = _ecCicloChartMode[canvasId] || 'anual';
  if (mode === 'mesActual' && !data.mesActual) mode = 'anual';
  _ecCicloChartMode[canvasId] = mode;

  var attempts = 0;
  function tryInit() {
    var canvas = document.getElementById(canvasId);
    if (!canvas || typeof Chart === 'undefined') {
      if (++attempts < 30) setTimeout(tryInit, 100);
      return;
    }
    if (_ecCicloChartInstances[canvasId]) {
      _ecCicloChartInstances[canvasId].destroy();
      delete _ecCicloChartInstances[canvasId];
    }
    if (mode === 'mesActual') {
      _ecCicloChartInstances[canvasId] = ecBuildCicloMesActualChart(canvas, data.mesActual);
    } else {
      _ecCicloChartInstances[canvasId] = ecBuildCicloAnualChart(canvas, data);
    }
    ecUpdateCicloHeader(canvasId, mode);
    ecBindCicloResizeListener();
  }
  tryInit();
}

function ecCicloSetMode(btn, canvasId, mode) {
  var container = btn.closest('.ec-ciclo-pills') || btn.closest('.pill-group');
  if (container) {
    container.querySelectorAll('.pill').forEach(function(c) {
      c.classList.remove('active');
    });
  }
  btn.classList.add('active');

  var data = window['_ecCicloData_' + canvasId];
  if (!data) return;
  if (mode === 'mesActual' && !data.mesActual) return;

  var canvas = document.getElementById(canvasId);
  if (!canvas) return;

  if (_ecCicloChartInstances[canvasId]) {
    _ecCicloChartInstances[canvasId].destroy();
    delete _ecCicloChartInstances[canvasId];
  }

  _ecCicloChartMode[canvasId] = mode;
  if (mode === 'mesActual') {
    _ecCicloChartInstances[canvasId] = ecBuildCicloMesActualChart(canvas, data.mesActual);
  } else {
    _ecCicloChartInstances[canvasId] = ecBuildCicloAnualChart(canvas, data);
  }
  ecUpdateCicloHeader(canvasId, mode);
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
    yScale.max = axisMax;
  }
  if (srvMin != null && !isNaN(srvMin)) {
    yScale.min = srvMin;
    yScale.beginAtZero = false;
  }
  return yScale;
}

function ecBuildCicloMesActualChart(canvas, data) {
  if (!data || !data.labels || !data.labels.length) return null;

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
    ticks: { font: fontCfg },
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
              var fmt = Math.abs(v) >= 100 ? v.toFixed(0) : v.toFixed(1);
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
            minRotation: isMobile ? 45 : 0
          }
        },
        y: yScale
      }
    }
  });
}

function ecBuildCicloAnualChart(canvas, data) {
  if (!data || !data.meses) return null;

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

  var yScale = {
    beginAtZero: true,
    grid: { color: gridColor },
    ticks: { font: fontCfg },
    title: {
      display: !!unit,
      text: unit,
      font: { size: 13, family: "'Inter', sans-serif" }
    }
  };

  // Tope del eje Y: ajustado a las series dibujadas (tope duro, no sugerido).
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
    yScale.max = axisMax;
  }
  if (srvMin != null && !isNaN(srvMin)) {
    yScale.min = srvMin;
    yScale.beginAtZero = false;
  }

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
              var fmt = Math.abs(v) >= 100 ? v.toFixed(0) : v.toFixed(1);
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
            minRotation: isMobile ? 45 : 0
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
  mount.innerHTML =
    '<span class="ec-legend-line ec-legend-normal" aria-hidden="true"></span>' +
    '<span class="ec-normal-period-label">Normal</span>' +
    '<select class="ec-normal-start" aria-label="A\u00f1o inicio periodo normal">' +
    ecNormalYearOptions(years) + '</select>' +
    '<span class="ec-normal-period-sep" aria-hidden="true">\u2013</span>' +
    '<select class="ec-normal-end" aria-label="A\u00f1o fin periodo normal">' +
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
