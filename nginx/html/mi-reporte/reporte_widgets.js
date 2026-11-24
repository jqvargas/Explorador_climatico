// reporte_widgets.js — Ranking widgets (hydration from #ec-report-data)
// ============================================================


var _ecUltimasObsSwiper = null;
var _ecUltimasObsAxisMq = null;
var EC_CAROUSEL_NARROW_MQ = "(max-width: 600px)";

function ecDestroyUltimasObsSwiper() {
  if (_ecUltimasObsSwiper) {
    _ecUltimasObsSwiper.destroy(true, true);
    _ecUltimasObsSwiper = null;
  }
}

function ecSparkTone(vtype) {
  if (vtype === "precip" || vtype === "flow" || vtype === "level") return "#F4B860";
  if (vtype === "temp_max" || vtype === "temp_mean" || vtype === "temp_min") return "#E2734B";
  return "#6E7C8C";
}

function ecSparklineSvg(values, vtype) {
  var w = 80;
  var h = 22;
  var pad = 1;
  var nums = (values || []).map(Number).filter(function (n) { return isFinite(n); });
  if (nums.length < 2) return "";
  var min = Math.min.apply(null, nums);
  var max = Math.max.apply(null, nums);
  var span = max - min;
  if (span < 1e-9) span = 1;
  var pts = nums.map(function (v, i) {
    var x = pad + (i / (nums.length - 1)) * (w - pad * 2);
    var y = h - pad - ((v - min) / span) * (h - pad * 2);
    return x.toFixed(1) + "," + y.toFixed(1);
  }).join(" ");
  var color = ecSparkTone(vtype);
  return '<svg viewBox="0 0 ' + w + " " + h + '" width="' + w + '" height="' + h + '" aria-hidden="true">' +
    '<polyline fill="none" stroke="' + color + '" stroke-width="1.75" stroke-linecap="round" stroke-linejoin="round" points="' + pts + '"/></svg>';
}

function ecHydrateSparks(root) {
  root = root || document;
  var chips = root.querySelectorAll(".ec-rp-var-chip[data-spark], .mr-var-opt[data-spark]");
  chips.forEach(function (chip) {
    var mount = chip.querySelector(".ec-rp-spark");
    if (!mount) return;
    var raw = chip.getAttribute("data-spark") || "";
    var vals = raw.split(",").map(Number).filter(function (n) { return isFinite(n); });
    if (vals.length < 2) {
      mount.innerHTML = "";
      return;
    }
    mount.innerHTML = ecSparklineSvg(vals, chip.getAttribute("data-vtype") || "generic");
  });
}

(function () {
  var s = ecSparklineSvg([1, 2, 3, 2], "precip");
  if (s.indexOf("polyline") < 0 && typeof console !== "undefined") {
    console.error("ecSparklineSvg self-check failed");
  }
})();

function ecUltimasObsSlidesPerView(swiper) {
  var spv = swiper.params.slidesPerView;
  if (typeof spv === 'number' && !isNaN(spv)) return spv;
  var bp = swiper.currentBreakpoint && swiper.params.breakpoints
    ? swiper.params.breakpoints[swiper.currentBreakpoint]
    : null;
  if (bp && typeof bp.slidesPerView === 'number') return bp.slidesPerView;
  return 2.15;
}

function ecUltimasObsScrollToRecent(swiper) {
  var n = swiper.slides ? swiper.slides.length : 0;
  if (n <= 1) return;
  var spv = ecUltimasObsSlidesPerView(swiper);
  var target = Math.max(0, n - Math.ceil(spv));
  if (swiper.activeIndex !== target) swiper.slideTo(target, 0, false);
}

function ecBindUltimasObsDisclosure() {
  var det = document.querySelector('.ec-rp-ultimas-obs-details');
  if (!det) {
    ecInitUltimasObsSwiper();
    return;
  }
  function reveal() {
    if (_ecUltimasObsSwiper) {
      _ecUltimasObsSwiper.update();
      ecUltimasObsScrollToRecent(_ecUltimasObsSwiper);
    } else {
      ecInitUltimasObsSwiper();
    }
  }
  if (det.open) reveal();
  if (det.getAttribute('data-ec-bound') === '1') return;
  det.setAttribute('data-ec-bound', '1');
  det.addEventListener('toggle', function () {
    if (det.open) reveal();
  });
}

function ecUltimasObsSwiperParams(prevEl, nextEl) {
  return {
    direction: 'horizontal',
    slidesPerView: 2.15,
    spaceBetween: 10,
    freeMode: { enabled: true, sticky: true },
    watchOverflow: true,
    grabCursor: true,
    navigation: prevEl && nextEl ? { prevEl: prevEl, nextEl: nextEl } : undefined,
    a11y: {
      enabled: true,
      prevSlideMessage: 'D\u00eda m\u00e1s antiguo',
      nextSlideMessage: 'D\u00eda m\u00e1s reciente'
    },
    breakpoints: {
      480: { slidesPerView: 3, spaceBetween: 12 },
      768: { slidesPerView: 4, spaceBetween: 12 },
      1024: { slidesPerView: 5, spaceBetween: 14, freeMode: false }
    },
    on: {
      init: function(swiper) { ecUltimasObsScrollToRecent(swiper); },
      breakpoint: function(swiper) { ecUltimasObsScrollToRecent(swiper); }
    }
  };
}

function ecBindUltimasObsAxis() {
  if (_ecUltimasObsAxisMq) return;
  _ecUltimasObsAxisMq = window.matchMedia(EC_CAROUSEL_NARROW_MQ);
  var onChange = function () {
    var el = document.querySelector('.ec-ultimas-obs-swiper');
    if (!el) return;
    ecInitUltimasObsSwiper();
  };
  if (_ecUltimasObsAxisMq.addEventListener) _ecUltimasObsAxisMq.addEventListener('change', onChange);
  else if (_ecUltimasObsAxisMq.addListener) _ecUltimasObsAxisMq.addListener(onChange);
}

function ecInitUltimasObsSwiper() {
  ecDestroyUltimasObsSwiper();
  var el = document.querySelector('.ec-ultimas-obs-swiper');
  if (!el) return;
  ecBindUltimasObsAxis();
  if (window.matchMedia(EC_CAROUSEL_NARROW_MQ).matches) {
    el.classList.add('is-stacked');
    return;
  }
  el.classList.remove('is-stacked');
  if (typeof Swiper === 'undefined') return;
  var wrap = el.closest('.ec-rp-ultimas-obs-swiper-wrap');
  var prevEl = wrap ? wrap.querySelector('.ec-pronostico-nav-prev') : null;
  var nextEl = wrap ? wrap.querySelector('.ec-pronostico-nav-next') : null;
  _ecUltimasObsSwiper = new Swiper(el, ecUltimasObsSwiperParams(prevEl, nextEl));
}
var _ecReportState = {
  data: null,
  widgets: {},
  listeners: []
};

function ecInitReport() {
  var root = document.querySelector('.ec-reporte-contenido');
  var el = root ? root.querySelector('script.ec-report-data-json[type="application/json"]') : null;
  if (!el || !el.textContent) return;
  try {
    _ecReportState.data = ecSanitizeWidgetPayload(JSON.parse(el.textContent));
  } catch (e) {
    console.error('ecInitReport: invalid JSON', e);
    return;
  }
  var nodes = document.querySelectorAll('.ec-rw-widget[data-widget-id]');
  nodes.forEach(function(node) {
    var id = node.getAttribute('data-widget-id');
    var spec = (_ecReportState.data.widgets || []).find(function(w) { return w.id === id; });
    if (!spec) return;
    _ecReportState.widgets[id] = {
      res: 'annual',
      monthMode: 'all',
      month: spec.defaultMonth || 1,
      compareAnnual: 'hasta_hoy',
      compareMonthly: 'completo'
    };
  });
  nodes.forEach(function(node) {
    var id = node.getAttribute('data-widget-id');
    var spec = (_ecReportState.data.widgets || []).find(function(w) { return w.id === id; });
    if (spec && _ecReportState.widgets[id]) ecRenderWidget(node, spec);
  });
  ecRenderYearBar();
  var nEx = (_ecReportState.data && _ecReportState.data.nFlaggedExcluded) || 0;
  if (nEx > 0) {
    var row = document.querySelector('.ec-rp-widgets-row');
    if (row && !row.parentNode.querySelector('.ec-rw-qc-footnote')) {
      var note = document.createElement('p');
      note.className = 'ec-rw-qc-footnote';
      note.textContent = nEx + ' valores imposibles que no entran en la lista';
      row.parentNode.insertBefore(note, row.nextSibling);
    }
  }
  ecBindUltimasObsDisclosure();
}

function ecDestroyReport() {
  _ecReportState.data = null;
  _ecReportState.widgets = {};
  _ecReportState.listeners.forEach(function(fn) {
    try { fn(); } catch (e) { /* ignore */ }
  });
  _ecReportState.listeners = [];
  if (typeof ecDestroyUltimasObsSwiper === 'function') ecDestroyUltimasObsSwiper();
  if (typeof ecDestroyPronosticoCarousel === 'function') ecDestroyPronosticoCarousel();
  if (typeof ecDestroyAllCharts === 'function') ecDestroyAllCharts();
}

function ecEscText(s) {
  return String(s == null ? '' : s)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}

function ecMedal(rank) {
  var tier = (rank === 1 || rank === 2 || rank === 3) ? String(rank) : 'n';
  return '<span class="ec-medal ec-medal--' + tier + '">' + rank + '</span>';
}

function ecFmtTop5Value(row) {
  var v = row.value;
  if (typeof v === 'number' && !isNaN(v)) {
    var n = Math.round(v);
    if (Math.abs(n) >= 1000) {
      return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, '.');
    }
    return String(n);
  }
  return row.valueFmt;
}

function ecSafeLabel(val, fallback) {
  fallback = fallback || '';
  if (val == null) return fallback;
  if (typeof val === 'string') return val;
  if (typeof val === 'number' || typeof val === 'boolean') return String(val);

  if (Array.isArray(val)) {
    for (var i = 0; i < val.length; i++) {
      var s = ecSafeLabel(val[i], '');
      if (s) return s;
    }
    return fallback;
  }

  if (typeof val === 'object') {
    if (val.main != null) {
      var m = ecSafeLabel(val.main, '');
      if (m) return m;
    }
    if (val.label != null) {
      var l = ecSafeLabel(val.label, '');
      if (l) return l;
    }
    if (val.year != null) {
      var y = ecSafeLabel(val.year, '');
      if (y) return y;
    }
    var keys = Object.keys(val);
    for (var k = 0; k < keys.length; k++) {
      var inner = ecSafeLabel(val[keys[k]], '');
      if (inner) return inner;
    }
    return fallback;
  }

  return String(val);
}

function ecFmtRank(rank) {
  if (rank == null || rank === '') return '\u2014';
  if (typeof rank === 'number' && !isNaN(rank)) return '#' + Math.round(rank);
  var s = ecSafeLabel(rank, '');
  if (!s) return '\u2014';
  var n = parseInt(s, 10);
  if (!isNaN(n)) return '#' + n;
  return '#' + s;
}

function ecFmtDisplayValue(val) {
  if (val == null) return '\u2014';
  if (typeof val === 'object') return ecSafeLabel(val, '\u2014');
  return String(val);
}

function ecSanitizeScalar(val, fallback) {
  fallback = fallback || '';
  if (val == null) return fallback;
  if (typeof val === 'string') return val;
  if (typeof val === 'number' && !isNaN(val)) return String(val);
  if (typeof val === 'boolean') return String(val);
  var s = ecSafeLabel(val, '');
  return s || fallback;
}

function ecSanitizeRank(val) {
  if (val == null || val === '') return null;
  if (typeof val === 'number' && !isNaN(val)) return Math.round(val);
  var s = ecSafeLabel(val, '');
  if (!s) return null;
  var n = parseInt(s, 10);
  return isNaN(n) ? null : n;
}

function ecSanitizeRows(rows) {
  if (!Array.isArray(rows)) return;
  rows.forEach(function(row) {
    if (!row) return;
    ['label', 'labelSub', 'labelMain', 'labelYear'].forEach(function(f) {
      if (row[f] != null) row[f] = ecSanitizeScalar(row[f], '');
    });
    if (row.rank != null) row.rank = ecSanitizeRank(row.rank);
  });
}

function ecSanitizeCurrent(cur) {
  if (!cur || typeof cur !== 'object') return;
  ['label', 'labelSub', 'labelMain', 'labelYear', 'sectionLabel', 'futureMsg', 'incompleteMsg', 'lastRecord', 'valueVerb'].forEach(function(f) {
    if (cur[f] != null) cur[f] = ecSanitizeScalar(cur[f], '');
  });
  if (cur.rankCompleto != null) cur.rankCompleto = ecSanitizeRank(cur.rankCompleto);
  if (cur.rankHastaHoy != null) cur.rankHastaHoy = ecSanitizeRank(cur.rankHastaHoy);
  if (cur.rank != null) cur.rank = ecSanitizeRank(cur.rank);
}

function ecSanitizeCompareBlock(block) {
  if (!block) return;
  if (block.top5) ecSanitizeRows(block.top5);
  if (block.current) ecSanitizeCurrent(block.current);
}

function ecSanitizeWidgetPayload(data) {
  if (!data || !Array.isArray(data.widgets)) return data;
  data.widgets.forEach(function(w) {
    ['annual', 'monthlyAll'].forEach(function(key) {
      var block = w[key];
      if (!block) return;
      ecSanitizeCurrent(block.current);
      if (block.top5) ecSanitizeRows(block.top5);
      if (block.compare) {
        ecSanitizeCompareBlock(block.compare.completo);
        ecSanitizeCompareBlock(block.compare.hasta_hoy);
      }
    });
    if (w.daily) {
      if (w.daily.top5) ecSanitizeRows(w.daily.top5);
      if (Array.isArray(w.daily.recentRecords)) {
        w.daily.recentRecords.forEach(function(r) {
          if (!r) return;
          ['date', 'labelMain', 'labelYear', 'valueFmt'].forEach(function(f) {
            if (r[f] != null) r[f] = ecSanitizeScalar(r[f], '');
          });
        });
      }
    }
    if (w.monthlyByMonth) {
      Object.keys(w.monthlyByMonth).forEach(function(m) {
        var bm = w.monthlyByMonth[m];
        ecSanitizeCurrent(bm.current);
        if (bm.top5) ecSanitizeRows(bm.top5);
        if (bm.compare) {
          ecSanitizeCompareBlock(bm.compare.completo);
          ecSanitizeCompareBlock(bm.compare.hasta_hoy);
        }
      });
    }
  });
  return data;
}

function ecCutoffShort(lastRecord) {
  return ecSafeLabel(lastRecord, '').replace(/\s+\d{4}$/, '');
}

function ecAnnualCompleteCount(current, compare) {
  if (!current) return 0;
  var n = parseInt(compare === 'hasta_hoy' ? current.nAniosHastaHoy : current.nAniosCompleto, 10);
  return isNaN(n) ? 0 : n;
}

function ecRankScale(res) {
  var max = 0;
  var consider = function(v) {
    if (typeof v === 'number' && isFinite(v) && v > max) max = v;
  };
  var widgets = (_ecReportState.data && _ecReportState.data.widgets) || [];
  widgets.forEach(function(spec) {
    var state = _ecReportState.widgets[spec.id];
    if (!state || state.res !== res) return;
    var block = ecGetBlock(spec, state);
    (block.top5 || []).forEach(function(r) { if (r) consider(r.value); });
    if (res === 'annual' && block.current) consider(block.current.value);
  });
  return max;
}

function ecBarWidth(value, scale) {
  if (!scale || typeof value !== 'number' || !isFinite(value) || value <= 0) return 0;
  return Math.max(0, Math.min(100, value / scale * 100));
}

function ecRankRowHtml(row, opts) {
  opts = opts || {};
  var rankNum = ecSanitizeRank(row.rank);
  var rankCol = opts.pin
    ? '<span class="ec-medal ec-medal--pin">' + (rankNum != null ? rankNum : '\u2014') + '</span>'
    : (rankNum != null ? ecMedal(rankNum) : '<span class="rank-num">\u2014</span>');

  var labelHtml;
  var labelSub = ecSafeLabel(row.labelSub);
  var stacked = false;
  if (opts.daily) {
    stacked = true;
    labelHtml = '<div class="label-stack"><span class="date-main">' + ecSafeLabel(row.labelMain) + '</span>' +
      '<span class="date-year">' + ecSafeLabel(row.labelYear) + '</span></div>';
  } else if (labelSub) {
    stacked = true;
    labelHtml = '<div class="label-stack"><span class="ec-rw-year-text">' + ecSafeLabel(row.label) + '</span>' +
      '<span class="label-sub">' + labelSub + '</span></div>';
  } else {
    var badge = row.recent ? ' <span class="badge green">Reciente</span>' : '';
    labelHtml = '<span class="ec-rw-year-text">' + ecSafeLabel(row.label) + '</span>' + badge;
  }

  var pill = '';
  if (opts.enCurso) {
    pill = '<span class="ec-rw-pill">En curso</span>';
  } else if (row.incompleteHist && row.coveragePct != null) {
    var pct = ecSanitizeRank(row.coveragePct);
    var noun = opts.monthly ? 'per\u00edodo' : 'a\u00f1o';
    var extra = opts.monthly
      ? ' Entra al listado, pero no est\u00e1 completo.'
      : ' Entra al listado, pero no cuenta como a\u00f1o completo.';
    var tip = 'Este ' + noun + ' tiene ' + pct + '% de d\u00edas con registro.' + extra;
    pill = '<span class="ec-rw-pill" title="' + ecEscText(tip) + '">Incompleto</span>';
  }
  if (pill) labelHtml += pill;

  var pct = ecBarWidth(row.value, opts.scale);
  var cls = 'rank-row' + (stacked ? ' is-stack' : ' is-line') + (opts.pin ? ' rank-row-pin' : '');
  return '<div class="' + cls + '" role="row">' +
    '<div class="rank-col">' + rankCol + '</div>' +
    '<div class="label-col">' + labelHtml + '</div>' +
    '<div class="ec-rw-bar" aria-hidden="true"><i style="width:' + pct.toFixed(1) + '%"></i></div>' +
    '<div class="value-col"><span class="value-num">' + ecFmtTop5Value(row) + '</span></div>' +
    '</div>';
}

function ecRenderTop5(top5, opts) {
  if (!top5 || !top5.length) {
    return '<div class="rank-list"><p class="ec-rw-empty-msg">Sin datos</p></div>';
  }
  return '<div class="rank-list">' + top5.map(function(r) { return ecRankRowHtml(r, opts); }).join('') + '</div>';
}

function ecRenderRecentRecords(recentRecords, lastRecord, unit) {
  var rows = Array.isArray(recentRecords) && recentRecords.length ? recentRecords : null;
  if (!rows && lastRecord) {
    rows = [{
      date: lastRecord.date,
      labelMain: lastRecord.date,
      labelYear: '',
      valueFmt: lastRecord.valueFmt,
      isLatest: true
    }];
  }
  if (!rows || !rows.length) return '';

  var listHtml = rows.map(function(r, i) {
    var isLatest = r.isLatest === true || i === 0;
    var cls = 'last-record-row' + (isLatest ? ' latest' : '');
    var dateMain = ecSafeLabel(r.labelMain || r.date);
    var dateYear = ecSafeLabel(r.labelYear);
    var badge = isLatest ? '<span class="last-record-badge">M\u00e1s reciente</span>' : '';
    return '<div class="' + cls + '">' +
      '<div class="last-record-date">' +
        '<span class="date-main">' + dateMain + '</span>' +
        (dateYear ? '<span class="date-year">' + dateYear + '</span>' : '') +
      '</div>' +
      '<div class="last-record-value">' +
        '<span class="value-num">' + ecFmtDisplayValue(r.valueFmt) + '</span>' +
        (unit ? '<span class="value-unit">' + ecSafeLabel(unit) + '</span>' : '') +
      '</div>' +
      badge +
    '</div>';
  }).join('');

  return '<div class="last-record-bar">' +
    '<div class="last-record-header">' +
      '<div class="last-record-dot"></div>' +
      '<p class="last-record-title">\u00daltimos registros</p>' +
    '</div>' +
    '<div class="last-record-list">' + listHtml + '</div>' +
  '</div>';
}

function ecRenderSharedLegend() {
  var row = document.querySelector('.ec-reporte-contenido .ec-rp-widgets-row');
  if (!row || !row.parentNode) return;
  var any = false;
  var annual = false;
  var widgets = (_ecReportState.data && _ecReportState.data.widgets) || [];
  widgets.forEach(function(spec) {
    var state = _ecReportState.widgets[spec.id];
    if (!state) return;
    var block = ecGetBlock(spec, state);
    (block.top5 || []).forEach(function(r) {
      if (r && r.incompleteHist) {
        any = true;
        if (state.res === 'annual') annual = true;
      }
    });
  });
  var el = row.parentNode.querySelector(':scope > .ec-rw-cov-legend');
  if (!any) {
    if (el) el.remove();
    return;
  }
  if (!el) {
    el = document.createElement('p');
    el.className = 'ec-rw-cov-legend';
    var qc = row.parentNode.querySelector(':scope > .ec-rw-qc-footnote');
    row.parentNode.insertBefore(el, qc || row.nextSibling);
  }
  el.textContent = annual
    ? 'Incompleto: menos del 95% de los d\u00edas con registro. No cuenta entre los a\u00f1os completos.'
    : 'Incompleto: menos del 95% de los d\u00edas con registro.';
}

function ecRenderAnnualPin(current, compare, opts) {
  if (!current) return '';
  var hasta = compare === 'hasta_hoy';
  var rank = hasta ? current.rankHastaHoy : current.rankCompleto;
  var n = ecAnnualCompleteCount(current, compare);
  var year = ecSafeLabel(current.label);
  var open = !!current.incomplete;
  var rowHtml = ecRankRowHtml({
    rank: rank,
    label: year,
    value: current.value,
    valueFmt: current.valueFmt
  }, Object.assign({}, opts, { pin: true, enCurso: open }));
  var text;
  if (!n) {
    text = 'No hay a\u00f1os con datos completos para comparar.';
  } else if (rank == null || rank === '') {
    text = 'Sin puesto entre ' + n + ' a\u00f1os con datos completos.';
  } else if (open) {
    text = 'Puesto provisional: ' + rank + ' de ' + (n + 1) + ' (' + n + ' completos + ' + year + ')';
  } else {
    text = 'Puesto: ' + rank + ' de ' + (n + 1) + ' (' + n + ' completos + ' + year + ')';
  }
  return rowHtml + '<p class="ec-rw-pin-cap">' + ecEscText(text) + '</p>';
}

function ecYearCompareMode() {
  var ids = Object.keys(_ecReportState.widgets);
  if (!ids.length) return 'hasta_hoy';
  return _ecReportState.widgets[ids[0]].compareAnnual || 'hasta_hoy';
}

function ecSetCompareAnnual(mode) {
  if (mode !== 'hasta_hoy' && mode !== 'completo') mode = 'hasta_hoy';
  Object.keys(_ecReportState.widgets).forEach(function(id) {
    _ecReportState.widgets[id].compareAnnual = mode;
  });
  var widgets = (_ecReportState.data && _ecReportState.data.widgets) || [];
  document.querySelectorAll('.ec-rw-widget[data-widget-id]').forEach(function(node) {
    var id = node.getAttribute('data-widget-id');
    var spec = widgets.find(function(w) { return w.id === id; });
    if (spec) ecRenderWidget(node, spec);
  });
  ecRenderYearBar();
}

function ecRenderYearBar() {
  var bar = document.querySelector('.ec-reporte-contenido .ec-rw-yearbar');
  if (!bar) return;
  var widgets = (_ecReportState.data && _ecReportState.data.widgets) || [];
  var spec = widgets[0];
  var cur = spec && spec.annual && spec.annual.current;
  if (!cur || !cur.incomplete) {
    bar.hidden = true;
    bar.innerHTML = '';
    return;
  }
  var mode = ecYearCompareMode();
  var unit = spec.unit ? ecEscText(spec.unit) : '';
  var station = _ecReportState.data && _ecReportState.data.station;
  var stationName = station && station.name ? ecSafeLabel(station.name) : '';
  var when = ecCutoffShort(cur.lastRecord);
  var year = ecSafeLabel(cur.label);
  var help = 'Hasta hoy corta cada a\u00f1o en la misma fecha y la tabla muestra ese acumulado. ' +
    'A\u00f1os cerrados muestra el total de enero a diciembre. ' +
    'El a\u00f1o en curso no se proyecta.';
  var note = mode === 'hasta_hoy'
    ? 'Cada fila es lo acumulado hasta el \u00faltimo registro, con el mismo corte en cada a\u00f1o. No es una proyecci\u00f3n.'
    : 'Cada fila es el total de enero a diciembre. ' + year + ' no se proyecta: su puesto usa el acumulado al \u00faltimo registro.';
  var who = stationName ? stationName + ' \u00b7 ' : '';
  bar.hidden = false;
  bar.innerHTML =
    '<div class="ec-rw-year-top">' +
      '<div>' +
        '<div class="ec-rw-year-meta">' + ecEscText(who + year) + ' \u00b7 acumulado al ' + ecEscText(when) +
          '<button type="button" class="ec-rw-year-q" title="' + ecEscText(help) + '" aria-label="M\u00e1s informaci\u00f3n">?</button></div>' +
        '<div class="ec-rw-year-total">' + ecEscText(ecFmtTop5Value(cur)) +
          (unit ? '<small>' + unit + '</small>' : '') + '</div>' +
      '</div>' +
      '<div class="ec-rw-year-seg" role="group" aria-label="Base de comparaci\u00f3n">' +
        '<button type="button" data-compare-annual="hasta_hoy"' + (mode === 'hasta_hoy' ? ' class="on"' : '') +
          '>Hasta hoy</button>' +
        '<button type="button" data-compare-annual="completo"' + (mode === 'completo' ? ' class="on"' : '') +
          '>A\u00f1os cerrados</button>' +
      '</div>' +
    '</div>' +
    '<p class="ec-rw-year-note">' + ecEscText(note) + '</p>';
  bar.querySelectorAll('[data-compare-annual]').forEach(function(btn) {
    btn.addEventListener('click', function() {
      ecSetCompareAnnual(btn.getAttribute('data-compare-annual'));
    });
  });
}

function ecRenderCurrentAnnual(current, compare, unit, rankOnly) {
  if (!current) return '';
  var safeLabel = ecSafeLabel(current.label);
  var rank = compare === 'hasta_hoy' ? current.rankHastaHoy : current.rankCompleto;
  var rankStr = ecFmtRank(rank);
  var valueHtml = rankOnly ? '' :
    '<div class="value-col"><span class="value-num">' + ecFmtDisplayValue(current.valueFmt) + '</span>' +
    '<span class="value-unit">' + ecSafeLabel(unit) + '</span></div>';
  var html = '<div class="current-section">';
  html += '<p class="current-section-label">' + ecSafeLabel(current.sectionLabel, 'A\u00f1o en curso') + '</p>';
  html += '<div class="rank-row current' + (rankOnly ? ' is-rank-only' : '') + '">' +
    '<div class="rank-col"><span class="rank-num">' + rankStr + '</span></div>' +
    '<div class="label-col"><span class="ec-rw-year-text">' + safeLabel + '</span></div>' +
    valueHtml + '</div>';

  if (current.incomplete) {
    html += '<div class="incomplete-bar"><span class="incomplete-icon">\u26a0\ufe0f</span><div class="incomplete-body">' +
      '<p class="incomplete-text">' + (current.incompleteMsg || 'Per\u00edodo incompleto') + '</p>' +
      '<span class="incomplete-date">\u00daltimo registro: ' + current.lastRecord + '</span>' +
      '<div class="compare-toggle" data-role="compare-toggle">' +
      '<button type="button" class="compare-pill active" data-compare="completo">Completo</button>' +
      '<button type="button" class="compare-pill" data-compare="hasta_hoy">Hasta hoy</button>' +
      '</div></div></div>';
  }
  html += '</div>';
  return html;
}

function ecRenderCurrentMonthly(current, compare, unit, rankOnly) {
  if (!current) return '';
  if (current.status === 'future') {
    return '<div class="current-section">' +
      '<p class="current-section-label">' + current.sectionLabel + '</p>' +
      '<div class="rank-row current" style="opacity:0.45;">' +
      '<div class="rank-col"><span class="rank-num">\u2014</span></div>' +
      '<div class="label-col label-col-muted">Sin datos a\u00fan</div>' +
      '<div class="value-col"><span class="value-num">\u2014</span></div></div>' +
      '<div class="incomplete-bar" style="margin-bottom:13px;">' +
      '<span class="incomplete-icon">\uD83D\uDCC5</span><div class="incomplete-body">' +
      '<p class="incomplete-text">' + current.futureMsg + '</p>' +
      '<span class="incomplete-date">\u00daltimo registro: ' + current.lastRecord + '</span>' +
      '</div></div></div>';
  }

  var rank = compare === 'hasta_hoy' ? current.rankHastaHoy : current.rankCompleto;
  var rankStr = ecFmtRank(rank);
  var safeLabel = ecSafeLabel(current.label);
  var safeLabelSub = ecSafeLabel(current.labelSub);
  var labelHtml = safeLabelSub
    ? '<div class="label-stack"><span class="ec-rw-year-text">' + safeLabel + '</span><span class="label-sub label-sub-current">' + safeLabelSub + '</span></div>'
    : '<span class="ec-rw-year-text">' + safeLabel + '</span>';
  var valueHtml = rankOnly ? '' :
    '<div class="value-col"><span class="value-num">' + ecFmtDisplayValue(current.valueFmt) + '</span>' +
    '<span class="value-unit">' + ecSafeLabel(unit) + '</span></div>';

  var html = '<div class="current-section">';
  html += '<p class="current-section-label">' + ecSafeLabel(current.sectionLabel, 'Mes en curso') + '</p>';
  html += '<div class="rank-row current' + (rankOnly ? ' is-rank-only' : '') + '">' +
    '<div class="rank-col"><span class="rank-num">' + rankStr + '</span></div>' +
    '<div class="label-col">' + labelHtml + '</div>' +
    valueHtml + '</div>';

  if (current.incomplete) {
    html += '<div class="incomplete-bar"><span class="incomplete-icon">\u26a0\ufe0f</span><div class="incomplete-body">' +
      '<p class="incomplete-text">' + (current.incompleteMsg || '') + '</p>' +
      '<span class="incomplete-date">\u00daltimo registro: ' + current.lastRecord + '</span>' +
      '<div class="compare-toggle" data-role="compare-toggle">' +
      '<button type="button" class="compare-pill' + (compare === 'hasta_hoy' ? '' : ' active') + '" data-compare="completo">Completo</button>' +
      '<button type="button" class="compare-pill' + (compare === 'hasta_hoy' ? ' active' : '') + '" data-compare="hasta_hoy">Hasta hoy</button>' +
      '</div></div></div>';
  }
  html += '</div>';
  return html;
}

function ecGetBlock(spec, state) {
  var unit = spec.unit || '';
  if (state.res === 'annual') {
    var block = spec.annual;
    var cmp = state.compareAnnual || 'hasta_hoy';
    var data = block.compare && block.compare[cmp] ? block.compare[cmp] : block;
    return {
      title: spec.titleAnnual,
      colMid: block.colLabelMid,
      colRight: block.colLabelRight,
      top5: data.top5,
      current: block.current,
      insight: block.insights && block.insights[cmp] ? block.insights[cmp] : '',
      opts: { unit: unit },
      showMonthUI: false
    };
  }
  if (state.res === 'daily') {
    var d = spec.daily;
    return {
      title: spec.titleDaily,
      colMid: d.colLabelMid,
      colRight: d.colLabelRight,
      top5: d.top5,
      current: null,
      lastRecord: d.lastRecord,
      recentRecords: d.recentRecords,
      insight: d.insight,
      opts: { unit: unit, daily: true },
      showMonthUI: false
    };
  }
  /* monthly */
  if (state.monthMode === 'bymonth') {
    var bm = spec.monthlyByMonth[String(state.month)] || spec.monthlyByMonth[state.month];
    if (!bm) bm = { top5: [], current: null, colLabelMid: 'Mes \u00b7 A\u00f1o' };
    return {
      title: spec.titleMonthly,
      colMid: bm.colLabelMid,
      colRight: 'Total',
      top5: bm.top5,
      current: bm.current,
      insight: bm.insights && bm.insights.completo ? bm.insights.completo : '',
      opts: { unit: unit, monthly: true },
      showMonthUI: true,
      monthScroller: true
    };
  }
  var ma = spec.monthlyAll;
  var cmpM = state.compareMonthly || 'completo';
  var dataM = ma.compare && ma.compare[cmpM] ? ma.compare[cmpM] : ma;
  return {
    title: spec.titleMonthly,
    colMid: ma.colLabelMid,
    colRight: ma.colLabelRight,
    top5: dataM.top5,
    current: ma.current,
    insight: ma.insights && ma.insights[cmpM] ? ma.insights[cmpM] : '',
    opts: { unit: unit, monthly: true },
    showMonthUI: true,
    monthScroller: false
  };
}

function ecRenderWidgetBody(node, spec, state) {
  var block = ecGetBlock(spec, state);
  var unit = spec.unit || '';
  var html = '';
  block.opts.scale = ecRankScale(state.res);

  html += '<div class="col-labels" role="row">' +
    '<span class="col-label">#</span>' +
    '<span class="col-label">' + block.colMid + '</span>' +
    '<span class="col-label"></span>' +
    '<span class="col-label right">' + ecEscText(unit || block.colRight) + '</span></div>';

  html += ecRenderTop5(block.top5, block.opts);

  if (state.res === 'annual') {
    return html + ecRenderAnnualPin(block.current, state.compareAnnual || 'hasta_hoy', block.opts);
  }
  var rankOnly = spec.id === 'minimos';
  if (state.res === 'mensual' && block.current) {
    html += ecRenderCurrentMonthly(block.current, state.compareMonthly || 'completo', unit, rankOnly);
  } else if (state.res === 'daily' && (block.recentRecords || block.lastRecord)) {
    html += ecRenderRecentRecords(block.recentRecords, block.lastRecord, unit);
  }

  var footer = block.insight
    ? '<div class="widget-footer"><div class="insight-dot"></div><p class="insight-text">' + block.insight + '</p></div>'
    : '';

  return html + footer;
}

function ecRenderWidget(node, spec) {
  var state = _ecReportState.widgets[spec.id];
  if (!state) return;

  // If current state is 'daily' but widget skipDaily, reset to 'annual'
  if (state.res === 'daily' && spec.skipDaily) state.res = 'annual';

  var resPills = '<div class="pill-group ec-rw-seg" data-role="res-pills" role="group" aria-label="Periodo">' +
    '<button type="button" class="pill' + (state.res === 'annual' ? ' active' : '') + '" data-res="annual">Anual</button>' +
    '<button type="button" class="pill' + (state.res === 'mensual' ? ' active' : '') + '" data-res="mensual">Mensual</button>' +
    '<button type="button" class="pill' + (state.res === 'daily' ? ' active' : '') +
      (spec.skipDaily ? ' is-disabled' : '') + '" data-res="daily"' +
      (spec.skipDaily ? ' disabled title="El ranking diario no aplica a los a\u00f1os m\u00e1s secos."' : '') +
      '>Diario</button>' +
    '</div>';

  var monthModePills = '';
  if (state.res === 'mensual') {
    monthModePills = '<div class="pill-group ec-rw-seg" data-role="month-mode-pills">' +
      '<button type="button" class="pill' + (state.monthMode === 'all' ? ' active' : '') + '" data-month-mode="all">Todos</button>' +
      '<button type="button" class="pill' + (state.monthMode === 'bymonth' ? ' active' : '') + '" data-month-mode="bymonth">Por mes</button>' +
      '</div>';
  }

  var title = spec.titleAnnual;
  if (state.res === 'mensual') title = spec.titleMonthly;
  if (state.res === 'daily') title = spec.titleDaily;

  var sub = '';
  if (state.res === 'annual' && spec.annual && spec.annual.current) {
    var cmpSub = state.compareAnnual || 'hasta_hoy';
    var nSub = ecAnnualCompleteCount(spec.annual.current, cmpSub);
    var subText = 'Top 5 \u00b7 ' + nSub + (nSub === 1 ? ' a\u00f1o completo' : ' a\u00f1os completos');
    if (cmpSub === 'hasta_hoy') {
      var short = ecCutoffShort(spec.annual.current.lastRecord);
      if (short) subText += ' \u00b7 al ' + short;
    }
    sub = '<p class="ec-rw-card-sub">' + ecEscText(subText) + '</p>';
  }
  node.classList.toggle('is-max', spec.decreasing !== false);
  node.classList.toggle('is-min', spec.decreasing === false);

  var monthScroller = '';
  if (state.res === 'mensual' && state.monthMode === 'bymonth') {
    var chips = ['Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dic'];
    monthScroller = '<div class="month-scroller" data-role="month-scroller">' +
      chips.map(function(c, i) {
        var m = i + 1;
        return '<button type="button" class="month-chip' + (state.month === m ? ' active' : '') +
          '" data-month="' + m + '">' + c + '</button>';
      }).join('') + '</div>';
  }

  node.innerHTML =
    '<div class="widget-header">' +
    '<div class="ec-rw-card-titles"><span class="widget-title">' + title + '</span>' + sub + '</div>' +
    '<div class="ec-rw-seg-wrap">' +
    resPills + monthModePills + '</div></div>' +
    monthScroller +
    '<div data-role="widget-body">' + ecRenderWidgetBody(node, spec, state) + '</div>';

  ecBindWidgetEvents(node, spec);
  ecRenderSharedLegend();
}

function ecBindWidgetEvents(node, spec) {
  var state = _ecReportState.widgets[spec.id];

  node.querySelectorAll('[data-res]').forEach(function(btn) {
    btn.addEventListener('click', function() {
      if (btn.disabled) return;
      state.res = btn.getAttribute('data-res');
      if (state.res === 'mensual' && !state.monthMode) state.monthMode = 'all';
      ecRenderWidget(node, spec);
    });
  });

  node.querySelectorAll('[data-month-mode]').forEach(function(btn) {
    btn.addEventListener('click', function() {
      state.monthMode = btn.getAttribute('data-month-mode');
      ecRenderWidget(node, spec);
    });
  });

  node.querySelectorAll('[data-month]').forEach(function(btn) {
    btn.addEventListener('click', function() {
      state.month = parseInt(btn.getAttribute('data-month'), 10);
      ecRenderWidget(node, spec);
    });
  });

  node.querySelectorAll('[data-compare]').forEach(function(btn) {
    btn.addEventListener('click', function() {
      state.compareMonthly = btn.getAttribute('data-compare');
      var body = node.querySelector('[data-role="widget-body"]');
      if (body) body.innerHTML = ecRenderWidgetBody(node, spec, state);
      node.querySelectorAll('[data-compare]').forEach(function(b) {
        b.classList.toggle('active', b.getAttribute('data-compare') === state.compareMonthly);
      });
      ecRenderSharedLegend();
      ecBindCompareOnly(node, spec);
    });
  });
}

function ecBindCompareOnly(node, spec) {
  var state = _ecReportState.widgets[spec.id];
  node.querySelectorAll('[data-compare]').forEach(function(btn) {
    btn.addEventListener('click', function() {
      state.compareMonthly = btn.getAttribute('data-compare');
      var body = node.querySelector('[data-role="widget-body"]');
      if (body) body.innerHTML = ecRenderWidgetBody(node, spec, state);
      node.querySelectorAll('[data-compare]').forEach(function(b) {
        b.classList.toggle('active', b.getAttribute('data-compare') === state.compareMonthly);
      });
      ecRenderSharedLegend();
      ecBindCompareOnly(node, spec);
    });
  });
}

function ecGetTablaAnioData() {
  if (window.__ecReportTablaAnio) return window.__ecReportTablaAnio;
  var root = document.querySelector('.ec-reporte-contenido');
  var el = root ? root.querySelector('script.ec-report-data-json[type="application/json"]') : null;
  if (!el || !el.textContent) return null;
  try {
    var payload = JSON.parse(el.textContent);
    return payload.tablaAnio || null;
  } catch (e) {
    return null;
  }
}

function ecYearGridCellAt(row, monthIdx) {
  if (row == null) return null;
  if (Array.isArray(row)) return monthIdx < row.length ? row[monthIdx] : null;
  if (typeof row === 'object') {
    var m1 = monthIdx + 1;
    if (row[monthIdx] != null) return row[monthIdx];
    if (row[m1] != null) return row[m1];
    if (row[String(monthIdx)] != null) return row[String(monthIdx)];
    if (row[String(m1)] != null) return row[String(m1)];
  }
  return null;
}

function ecAsYearGridRows(cells) {
  if (!cells) return [];
  if (Array.isArray(cells)) return cells;
  if (typeof cells === 'object') {
    var out = [];
    for (var i = 1; i <= 31; i++) {
      out.push(cells[i] != null ? cells[i] : cells[String(i)]);
    }
    return out;
  }
  return [];
}

function ecYearGridKind(cell) {
  if (!cell || typeof cell !== 'object' || Array.isArray(cell)) return 'future';
  if (cell.kind === 'invalid' || cell.kind === 'missing' || cell.kind === 'future' ||
      cell.kind === 'obs' || cell.kind === 'fc') return cell.kind;
  if (cell.invalid === true) return 'invalid';
  if (cell.forecast === true) return 'fc';
  if (cell.v != null || cell.fmt) return 'obs';
  return 'future';
}

function ecYgLevel(v) {
  var n = Math.abs(Number(v));
  if (n < 1) return 1;
  if (n < 5) return 2;
  if (n < 20) return 3;
  if (n < 50) return 4;
  return 5;
}

function ecYgFmt(v) {
  var n = Number(v);
  if (!isFinite(n)) return '\u2013';
  if (Math.abs(n) >= 1000) {
    var sign = n < 0 ? '-' : '';
    return sign + String(Math.round(Math.abs(n))).replace(/\B(?=(\d{3})+(?!\d))/g, '.');
  }
  var neg = n < 0;
  var s = (Math.round(Math.abs(n) * 10) / 10).toFixed(1).replace('.', ',');
  return neg ? '-' + s : s;
}

function ecYgValueClass(kind, v, precip) {
  var n = Number(v);
  var zero = isFinite(n) && n === 0;
  if (kind === 'fc') {
    if (!precip) return 'ec-yg-fcplain';
    return zero ? 'ec-yg-q0' : 'ec-yg-q' + ecYgLevel(n);
  }
  if (!precip) return 'ec-yg-obs';
  return zero ? 'ec-yg-z' : 'ec-yg-l' + ecYgLevel(n);
}

function ecYgMonthStats(cellRows) {
  var out = [];
  for (var m = 0; m < 12; m++) {
    var sum = 0, n = 0, mx = null, days = 0, miss = 0, fcSum = 0, fcN = 0, fcLast = 0;
    for (var d = 0; d < cellRows.length; d++) {
      var cell = ecYearGridCellAt(cellRows[d], m);
      var kind = ecYearGridKind(cell);
      if (kind === 'obs') {
        var v = Number(cell.v);
        if (!isFinite(v)) continue;
        sum += v;
        n++;
        if (v >= 1) days++;
        if (mx == null || v > mx) mx = v;
      } else if (kind === 'missing') {
        miss++;
      } else if (kind === 'fc') {
        var fv = Number(cell.v);
        if (!isFinite(fv)) continue;
        fcSum += fv;
        fcN++;
        if (d + 1 > fcLast) fcLast = d + 1;
      }
    }
    out.push({
      sum: n ? sum : null,
      n: n,
      mx: mx,
      days: n ? days : null,
      miss: miss,
      fcSum: fcN ? fcSum : null,
      fcLast: fcLast
    });
  }
  return out;
}

function ecYgNote(data, stats) {
  var parts = [];
  var anyMiss = false;
  var fcTotal = 0;
  var hasFc = false;
  var lastM = -1;
  var lastD = 0;
  for (var m = 0; m < stats.length; m++) {
    if (stats[m].miss) anyMiss = true;
    if (stats[m].fcSum != null) {
      hasFc = true;
      fcTotal += stats[m].fcSum;
      if (m > lastM) {
        lastM = m;
        lastD = stats[m].fcLast;
      }
    }
  }
  if (anyMiss) parts.push('* Mes con d\u00edas sin dato: el total es parcial.');
  if (data.vtype === 'precip') parts.push('D\u00eda con lluvia: registro de 1,0 mm o m\u00e1s.');
  if (data.vtype === 'precip' && hasFc && data.todayDay && data.todayMonth) {
    var meses = data.meses || [];
    var obsMes = String(meses[data.todayMonth - 1] || '').toLowerCase();
    var fcMes = String(meses[lastM] || '').toLowerCase();
    var unidad = data.unidad || 'mm';
    parts.push(
      'En ' + obsMes + ' el total es lo observado hasta el ' + data.todayDay +
      '; el pron\u00f3stico suma ' + ecYgFmt(fcTotal) + ' ' + unidad +
      ' hasta el ' + lastD + ' ' + fcMes + '.'
    );
  }
  return parts.join(' ');
}

function ecYgDownloadCsv(data, cellRows, meses, btn) {
  var lines = ['dia;' + meses.join(';')];
  for (var d = 0; d < cellRows.length; d++) {
    var cols = [String(d + 1)];
    for (var m = 0; m < 12; m++) {
      var cell = ecYearGridCellAt(cellRows[d], m);
      var kind = ecYearGridKind(cell);
      if ((kind === 'obs' || kind === 'fc') && cell && cell.fmt != null) {
        var num = String(cell.fmt);
        cols.push(kind === 'fc' ? num + ' (pronostico)' : num);
      } else {
        cols.push('');
      }
    }
    lines.push(cols.join(';'));
  }
  var csv = '\ufeff' + lines.join('\r\n');
  var anio = data.anio || null;
  if (btn) btn.disabled = true;

  function done() {
    if (btn) btn.disabled = false;
  }
  function needAuth() {
    if (typeof rodajaOpenSignIn === 'function') rodajaOpenSignIn();
    else if (typeof rodajaSignInUrl === 'function') window.location.href = rodajaSignInUrl(window.location.href);
    else window.alert('Inicia sesi\u00f3n para exportar el CSV.');
  }

  var authReady = Promise.resolve(false);
  if (typeof rodajaGetAuthToken === 'function') {
    authReady = rodajaGetAuthToken().then(function (token) { return !!token; });
  }
  authReady.then(function (ok) {
    if (!ok) {
      needAuth();
      done();
      return;
    }
    if (typeof rodajaApiFetch !== 'function') {
      window.alert('No se pudo registrar la descarga.');
      done();
      return;
    }
    return rodajaApiFetch('/me/reporte-tabla/consumir', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ csv: csv, anio: anio })
    }).then(function (r) {
      if (r.status === 401) {
        needAuth();
        return;
      }
      if (!r.ok) {
        return r.text().then(function (text) {
          var msg = 'No te alcanza el l\u00edmite de descarga para exportar el CSV.';
          try {
            var j = JSON.parse(text);
            if (j && (j.error || j.mensaje)) msg = j.mensaje || j.error;
          } catch (e) { /* keep msg */ }
          window.alert(msg);
        });
      }
      var disp = r.headers.get('Content-Disposition') || '';
      var matched = /filename\*=UTF-8''([^;\s]+)|filename="?([^";]+)"?/i.exec(disp);
      var filename = 'registros-diarios-' + (anio || 'anio') + '.csv';
      if (matched) {
        try {
          filename = decodeURIComponent(matched[1] || matched[2] || filename);
        } catch (e2) {
          filename = matched[2] || filename;
        }
      }
      return r.blob().then(function (blob) {
        var a = document.createElement('a');
        a.href = URL.createObjectURL(blob);
        a.download = filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(a.href);
      });
    }).catch(function () {
      window.alert('No se pudo exportar el CSV.');
    }).then(done);
  }).catch(function () {
    window.alert('No se pudo exportar el CSV.');
    done();
  });
}

function ecYgFootRow(label, stats, pick, opts) {
  opts = opts || {};
  var meses = opts.meses || [];
  var html = '<tr class="' + opts.rowClass + '"><th scope="row" class="ec-yg-day">' + ecEscText(label) + '</th>';
  for (var m = 0; m < 12; m++) {
    var st = stats[m];
    var v = pick(st);
    var cls = 'ec-yg-ft';
    var inner = '\u2013';
    if (v != null && isFinite(Number(v))) {
      inner = opts.asInt ? String(Math.round(Number(v))) : ecYgFmt(v);
      if (Number(v) === 0) cls += ' ec-yg-ft-z';
    }
    if (opts.star && st.miss > 0 && v != null) {
      inner += '<span class="ec-yg-star" title="Incluye d\u00edas sin dato">*</span>';
    }
    if (opts.fc && st.fcSum != null) {
      var until = st.fcLast + ' ' + String(meses[m] || '').toLowerCase();
      inner += '<span class="ec-yg-fcsum" title="Pron\u00f3stico hasta el ' + ecEscText(until) + '">+' + ecYgFmt(st.fcSum) + '</span>';
    }
    html += '<td class="' + cls + '">' + inner + '</td>';
  }
  return html + '</tr>';
}

function ecYearGridIso(anio, monthIndex, dayNum) {
  var y = parseInt(anio, 10);
  var m = monthIndex + 1;
  var d = dayNum;
  if (!isFinite(y) || y < 1 || m < 1 || m > 12 || d < 1 || d > 31) return "";
  var mm = m < 10 ? "0" + m : String(m);
  var dd = d < 10 ? "0" + d : String(d);
  return y + "-" + mm + "-" + dd;
}

function ecYearGridAjustarHoy(data, todayIso) {
  if (!data || !data.cells || !todayIso) return data;
  var rows = ecAsYearGridRows(data.cells);
  var anio = data.anio;
  var lastObs = "";
  var anyFc = false;
  var di, mj, cell, kind, iso;
  for (di = 0; di < rows.length; di++) {
    if (!rows[di]) continue;
    for (mj = 0; mj < 12; mj++) {
      cell = ecYearGridCellAt(rows[di], mj);
      if (!cell || typeof cell !== "object") continue;
      kind = ecYearGridKind(cell);
      iso = ecYearGridIso(anio, mj, di + 1);
      if (!iso) continue;
      if (kind === "fc" && iso < todayIso) {
        cell.kind = "missing";
        cell.empty = true;
        cell.forecast = false;
        cell.v = null;
        cell.fmt = null;
        kind = "missing";
      }
      if (kind === "obs" && (!lastObs || iso > lastObs)) lastObs = iso;
      if (kind === "fc") anyFc = true;
    }
  }
  data.hasForecast = anyFc;
  if (lastObs) {
    var p = lastObs.split("-");
    data.todayMonth = Number(p[1]);
    data.todayDay = Number(p[2]);
    data.markerLabel = lastObs === todayIso ? "Hoy" : "\u00daltimo dato";
  }
  return data;
}

(function () {
  var cells = [];
  var di, row, m;
  for (di = 0; di < 31; di++) {
    row = [];
    for (m = 0; m < 12; m++) row.push({ kind: "future", empty: true });
    cells.push(row);
  }
  cells[22][9] = { kind: "obs", v: 1, fmt: "1" };
  cells[24][9] = { kind: "fc", v: 0, fmt: "0", forecast: true };
  cells[25][9] = { kind: "fc", v: 14.9, fmt: "14,9", forecast: true };
  var data = {
    anio: 2026,
    cells: cells,
    hasForecast: true,
    todayDay: 25,
    todayMonth: 10,
    markerLabel: "Hoy"
  };
  ecYearGridAjustarHoy(data, "2026-10-26");
  if (ecYearGridKind(cells[24][9]) !== "missing" || ecYearGridKind(cells[25][9]) !== "fc") {
    throw new Error("grilla: pronostico de un dia pasado");
  }
  if (data.markerLabel !== "\u00daltimo dato" || data.todayDay !== 23 || data.todayMonth !== 10) {
    throw new Error("grilla: marcador");
  }
})();

function ecInitYearGrid(mountId, data) {
  if (!data) data = ecGetTablaAnioData();
  if (data && typeof ecPronosticoTodayIso === "function") {
    ecYearGridAjustarHoy(data, ecPronosticoTodayIso());
  }
  var mount = mountId ? document.getElementById(mountId) : null;
  if (!mount) mount = document.querySelector('.ec-rp-year-grid-mount');
  if (!mount || !data || !data.hasData || !data.cells) return;
  var cellRows = ecAsYearGridRows(data.cells);
  var meses = data.meses || ['Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic'];
  var todayDay = Number(data.todayDay);
  var todayMonth = Number(data.todayMonth);
  var hasMarker = isFinite(todayDay) && isFinite(todayMonth) && todayMonth >= 1 && todayMonth <= 12;
  var precip = data.vtype === 'precip';
  var unidad = data.unidad ? String(data.unidad) : '';
  var anio = data.anio || '';
  var marker = data.markerLabel ? String(data.markerLabel) : '\u00daltimo dato';
  var stats = ecYgMonthStats(cellRows);
  var sub = precip
    ? 'Precipitaci\u00f3n diaria' + (unidad ? ' \u00b7 ' + unidad : '')
    : (data.variable ? String(data.variable) : 'Valores diarios') + (unidad ? ' \u00b7 ' + unidad : '');

  var html = '<div class="ec-yg">';
  html += '<div class="ec-yg-head">';
  html += '<div><h3 class="ec-yg-title">Registros diarios ' + ecEscText(anio) + '</h3>';
  html += '<p class="ec-yg-sub">' + ecEscText(sub) + '</p></div>';
  html += '<button type="button" class="ec-yg-export"><i class="fa fa-download" aria-hidden="true"></i> Exportar CSV</button>';
  html += '</div>';

  html += '<div class="ec-yg-legend">';
  if (precip) {
    html += '<span class="ec-yg-legend-item">Observado';
    html += '<span class="ec-yg-lgd ec-yg-l1">&lt;1</span>';
    html += '<span class="ec-yg-lgd ec-yg-l2">1\u20135</span>';
    html += '<span class="ec-yg-lgd ec-yg-l3">5\u201320</span>';
    html += '<span class="ec-yg-lgd ec-yg-l4">20\u201350</span>';
    html += '<span class="ec-yg-lgd ec-yg-l5">&gt;50</span>';
    html += '</span>';
  } else {
    html += '<span class="ec-yg-legend-item">Observado</span>';
  }
  if (data.hasForecast) {
    if (precip) {
      html += '<span class="ec-yg-legend-item">Pron\u00f3stico \u00b7 Open-Meteo';
      html += '<span class="ec-yg-swatch ec-yg-q1"></span>';
      html += '<span class="ec-yg-swatch ec-yg-q3"></span>';
      html += '<span class="ec-yg-swatch ec-yg-q4"></span>';
      html += '</span>';
    } else {
      html += '<span class="ec-yg-legend-item"><span class="ec-yg-swatch ec-yg-fcplain"></span>Pron\u00f3stico \u00b7 Open-Meteo</span>';
    }
  }
  html += '<span class="ec-yg-legend-item"><span class="ec-yg-swatch ec-yg-miss"></span>Sin dato</span>';
  if (hasMarker) {
    html += '<span class="ec-yg-legend-item"><span class="ec-yg-swatch ec-yg-swatch-now"></span>' + ecEscText(marker) + '</span>';
  }
  html += '</div>';

  var caption = precip
    ? 'Precipitaci\u00f3n diaria' + (unidad ? ' en ' + unidad : '') + ', ' + anio
    : (data.variable ? String(data.variable) : 'Valores diarios') + (unidad ? ' en ' + unidad : '') + ', ' + anio;
  html += '<div class="ec-rp-year-grid-wrap" tabindex="0">';
  html += '<table class="ec-year-grid' + (precip ? ' is-precip' : '') + '">';
  html += '<caption class="ec-yg-sr">' + ecEscText(caption) + '</caption>';
  html += '<colgroup><col class="ec-yg-col-day"></colgroup>';
  html += '<thead><tr><th class="ec-yg-day ec-yg-day-hdr" scope="col">D\u00eda</th>';
  for (var mi = 0; mi < 12; mi++) {
    var thCls = ' class="ec-yg-month';
    if (hasMarker && todayMonth === mi + 1) thCls += ' ec-yg-month-current';
    thCls += '"';
    html += '<th scope="col" data-m="' + mi + '"' + thCls + '>' + ecEscText(meses[mi]) + '</th>';
  }
  html += '</tr></thead><tbody>';

  for (var di = 0; di < cellRows.length; di++) {
    var dayNum = di + 1;
    var dayOn = hasMarker && todayDay === dayNum;
    html += '<tr><th scope="row" class="ec-yg-day" data-d="' + dayNum + '">';
    html += '<span class="ec-yg-dn' + (dayOn ? ' ec-yg-dn-on' : '') + '">' + dayNum + '</span></th>';
    for (var mj = 0; mj < 12; mj++) {
      var cell = ecYearGridCellAt(cellRows[di], mj);
      var kind = ecYearGridKind(cell);
      var isNow = hasMarker && todayDay === dayNum && todayMonth === mj + 1;
      if (kind === 'invalid' || kind === 'future') {
        html += '<td></td>';
        continue;
      }
      var cls = [];
      var content = '';
      var origin = '';
      var valueLine = '';
      if (kind === 'missing') {
        cls.push('ec-yg-miss');
        origin = 'Sin dato';
        valueLine = 'El sensor no report\u00f3';
      } else {
        cls.push(ecYgValueClass(kind, cell && cell.v, precip));
        content = ecEscText(cell && cell.fmt != null ? cell.fmt : '');
        var unitBit = unidad ? ' ' + unidad : '';
        valueLine = (cell && cell.fmt != null ? String(cell.fmt) : '') + unitBit;
        if (kind === 'fc') origin = 'Pron\u00f3stico \u00b7 Open-Meteo';
        else origin = isNow ? marker + ' \u00b7 observado' : 'Observado';
      }
      if (isNow) cls.push('ec-yg-now');
      var when = dayNum + ' ' + String(meses[mj] || '').toLowerCase();
      html += '<td class="' + cls.join(' ') + '" data-m="' + mj + '" data-d="' + dayNum + '"';
      html += ' data-a="' + ecEscText(when) + '" data-b="' + ecEscText(origin) + '" data-c="' + ecEscText(valueLine) + '">';
      html += content + '</td>';
    }
    html += '</tr>';
  }
  html += '</tbody><tfoot>';
  html += '<tr class="ec-yg-ft-gap"><td colspan="13"></td></tr>';
  if (precip) {
    html += ecYgFootRow('Total mm', stats, function (st) { return st.sum; }, {
      rowClass: 'ec-yg-ft-total', star: true, fc: true, meses: meses
    });
    html += ecYgFootRow('D\u00edas \u2265 1 mm', stats, function (st) { return st.days; }, {
      rowClass: 'ec-yg-ft-mid', asInt: true
    });
    html += ecYgFootRow('M\u00e1x. diario', stats, function (st) { return st.mx; }, {
      rowClass: 'ec-yg-ft-max'
    });
  } else {
    var mediaLabel = 'Media' + (unidad ? ' ' + unidad : '');
    html += ecYgFootRow(mediaLabel, stats, function (st) { return st.n ? st.sum / st.n : null; }, {
      rowClass: 'ec-yg-ft-total', star: true
    });
    html += ecYgFootRow('M\u00e1x.', stats, function (st) { return st.mx; }, {
      rowClass: 'ec-yg-ft-max'
    });
  }
  html += '</tfoot></table></div>';
  var note = ecYgNote(data, stats);
  if (note) html += '<p class="ec-yg-note">' + ecEscText(note) + '</p>';
  html += '<div class="ec-yg-tip" role="tooltip"></div></div>';
  mount.innerHTML = html;

  var root = mount.querySelector('.ec-yg');
  var tip = root.querySelector('.ec-yg-tip');
  var tb = root.querySelector('tbody');
  function clearHl() {
    root.querySelectorAll('.ec-yg-hl').forEach(function (el) { el.classList.remove('ec-yg-hl'); });
    tip.style.opacity = '0';
  }
  tb.addEventListener('mouseover', function (e) {
    var el = e.target && e.target.nodeType === 1 ? e.target : (e.target && e.target.parentElement);
    var c = el && el.closest ? el.closest('td[data-m]') : null;
    clearHl();
    if (!c) return;
    var th = root.querySelector('thead th[data-m="' + c.getAttribute('data-m') + '"]');
    var dh = root.querySelector('tbody th[data-d="' + c.getAttribute('data-d') + '"]');
    if (th) th.classList.add('ec-yg-hl');
    if (dh) dh.classList.add('ec-yg-hl');
    tip.textContent = '';
    var k = document.createElement('span');
    k.className = 'ec-yg-tip-k';
    k.textContent = c.getAttribute('data-a') + ' \u00b7 ' + c.getAttribute('data-b');
    var br = document.createElement('br');
    var val = document.createElement('span');
    val.className = 'ec-yg-tip-v';
    val.textContent = c.getAttribute('data-c');
    tip.appendChild(k);
    tip.appendChild(br);
    tip.appendChild(val);
    var r = c.getBoundingClientRect();
    var q = root.getBoundingClientRect();
    var left = r.left - q.left + r.width / 2 - tip.offsetWidth / 2;
    left = Math.min(Math.max(8, left), q.width - tip.offsetWidth - 8);
    var top = r.top - q.top - tip.offsetHeight - 6;
    if (top < 4) top = r.bottom - q.top + 6;
    tip.style.left = left + 'px';
    tip.style.top = top + 'px';
    tip.style.opacity = '1';
  });
  tb.addEventListener('mouseleave', clearHl);

  var btn = root.querySelector('.ec-yg-export');
  if (btn) {
    btn.addEventListener('click', function () {
      ecYgDownloadCsv(data, cellRows, meses, btn);
    });
  }

  if (hasMarker) {
    var wrap = root.querySelector('.ec-rp-year-grid-wrap');
    var monthTh = root.querySelectorAll('thead th')[todayMonth];
    if (wrap && monthTh) {
      var left = monthTh.offsetLeft - wrap.clientWidth / 2 + monthTh.offsetWidth / 2;
      wrap.scrollLeft = Math.max(0, left);
    }
  }
}
if (typeof Shiny !== 'undefined') {
  Shiny.addCustomMessageHandler('ec_report_destroy', function(_msg) {
    ecDestroyReport();
    document.querySelectorAll('.modal-backdrop').forEach(function (el) {
      if (el.parentNode) el.parentNode.removeChild(el);
    });
  });
  // Aridad 1: hidrata chart/widgets/ciclo sin 2o renderUI del modal.
  Shiny.addCustomMessageHandler('ec_report_hydrate_full', function (msg) {
    msg = msg || {};
    var payload = msg.payload;
    if (!payload) return;
    var root = document.querySelector('.ec-reporte-contenido');
    if (!root) return;
    var script = root.querySelector('script.ec-report-data-json[type="application/json"]');
    if (script) {
      try { script.textContent = JSON.stringify(payload); } catch (e) { /* ignore */ }
    }
    window.__ecReportChartData = payload.chart || null;
    window.__ecReportCicloData = payload.cicloAnual || null;
    window.__ecReportTablaAnio = payload.tablaAnio || null;
    window.__ecReportNormalClimatology = payload.normalClimatology || null;
    if (payload.pronosticoLite) window.__ecReportPronosticoLite = payload.pronosticoLite;
    var banner = root.querySelector('.ec-rp-heavy-banner');
    if (banner && banner.parentNode) banner.parentNode.removeChild(banner);
    var body = root.querySelector('.ec-rp-body');
    if (body) body.classList.remove('is-shell-pending');

    // Shell/skel no trae mounts de widgets ni year-grid; crearlos antes del boot.
    var row = root.querySelector('.ec-rp-widgets-row');
    if (row) {
      row.querySelectorAll('.ec-rp-skel-widget').forEach(function (n) {
        if (n.parentNode) n.parentNode.removeChild(n);
      });
      (payload.widgets || []).forEach(function (w) {
        if (!w || w.id == null) return;
        var id = String(w.id);
        var exists = false;
        row.querySelectorAll('.ec-rw-widget[data-widget-id]').forEach(function (n) {
          if (n.getAttribute('data-widget-id') === id) exists = true;
        });
        if (exists) return;
        var div = document.createElement('div');
        div.className = 'widget ec-rw-widget';
        div.setAttribute('data-widget-id', id);
        row.appendChild(div);
      });
    }
    if (payload.tablaAnio && payload.tablaAnio.hasData && msg.yearGridId) {
      var yg = document.getElementById(msg.yearGridId);
      if (!yg && body) {
        var sec = document.createElement('div');
        sec.className = 'ec-rp-section ec-rp-year-grid-section';
        var card = document.createElement('div');
        card.className = 'widget ec-rp-year-grid-card';
        var mount = document.createElement('div');
        mount.className = 'ec-rp-year-grid-mount';
        mount.id = String(msg.yearGridId);
        card.appendChild(mount);
        sec.appendChild(card);
        body.appendChild(sec);
      }
    }

    if (typeof ecBootReportPostRender === 'function') {
      try {
        ecBootReportPostRender({
          eid: msg.eid || 0,
          vid: msg.vid || 0,
          pronMountId: msg.pronMountId,
          pronSectionId: msg.pronSectionId,
          pronosticoLite: window.__ecReportPronosticoLite,
          chartId: msg.chartId,
          cicloId: msg.cicloId,
          yearGridId: msg.yearGridId,
          hasChart: !!window.__ecReportChartData,
          hasCiclo: !!window.__ecReportCicloData,
          hasTabla: !!window.__ecReportTablaAnio
        });
      } catch (e2) { /* ignore */ }
    }
  });
}
