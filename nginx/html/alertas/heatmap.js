(function (global) {
  "use strict";

  var MONTH_ABBR = ["E", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"];
  var DOW_LABEL = ["L", "", "X", "", "V", "", "D"];
  var MONTHS_ES = ["ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"];
  var MONTHS_FULL = ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"];

  function isoDate(d) {
    var y = d.getFullYear();
    var m = String(d.getMonth() + 1).padStart(2, "0");
    var day = String(d.getDate()).padStart(2, "0");
    return y + "-" + m + "-" + day;
  }

  function parseIso(s) {
    var p = String(s).slice(0, 10).split("-");
    return new Date(parseInt(p[0], 10), parseInt(p[1], 10) - 1, parseInt(p[2], 10));
  }

  function esc(s) {
    return String(s == null ? "" : s)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function dayMap(porDia) {
    var map = {};
    var maxN = 0;
    (porDia || []).forEach(function (x) {
      var k = String(x.dia || "").slice(0, 10);
      var n = parseInt(x.n, 10) || 0;
      if (!k) return;
      map[k] = n;
      if (n > maxN) maxN = n;
    });
    return { map: map, maxN: maxN };
  }

  function buildWeeks(map) {
    var today = new Date();
    today.setHours(0, 0, 0, 0);
    var end = new Date(today);
    var start = new Date(today);
    start.setDate(start.getDate() - 364);
    while (start.getDay() !== 1) start.setDate(start.getDate() - 1);

    var weeks = [];
    var cur = new Date(start);
    while (cur <= end) {
      var week = [];
      for (var i = 0; i < 7; i++) {
        var key = isoDate(cur);
        week.push({ dia: key, n: map[key] || 0, out: cur > end });
        cur.setDate(cur.getDate() + 1);
      }
      weeks.push(week);
    }
    return weeks;
  }

  function heatLevel(n, maxN) {
    if (!n) return 0;
    if (maxN <= 1) return 1;
    var t = n / maxN;
    if (t <= 0.25) return 1;
    if (t <= 0.5) return 2;
    if (t <= 0.75) return 3;
    return 4;
  }

  function fmtDayEs(iso) {
    if (!iso) return "\u2014";
    var p = iso.split("-");
    return parseInt(p[2], 10) + " " + MONTHS_ES[parseInt(p[1], 10) - 1] + " " + p[0];
  }

  function fmtMonthEs(ym) {
    if (!ym) return "\u2014";
    var p = ym.split("-");
    return MONTHS_FULL[parseInt(p[1], 10) - 1] + " " + p[0];
  }

  function insights(map) {
    var keys = Object.keys(map).filter(function (k) { return map[k] > 0; }).sort();
    var bestDay = null;
    var bestN = 0;
    keys.forEach(function (k) {
      if (map[k] > bestN) { bestN = map[k]; bestDay = k; }
    });

    var byMonth = {};
    keys.forEach(function (k) {
      var ym = k.slice(0, 7);
      byMonth[ym] = (byMonth[ym] || 0) + map[k];
    });
    var bestMonth = null;
    var bestMonthN = 0;
    Object.keys(byMonth).forEach(function (ym) {
      if (byMonth[ym] > bestMonthN) { bestMonthN = byMonth[ym]; bestMonth = ym; }
    });

    var curStreak = 0;
    var d = new Date();
    d.setHours(0, 0, 0, 0);
    while (true) {
      var k = isoDate(d);
      if ((map[k] || 0) > 0) { curStreak++; d.setDate(d.getDate() - 1); }
      else break;
    }

    var longest = 0;
    var run = 0;
    if (keys.length) {
      var c = parseIso(keys[0]);
      var endD = parseIso(keys[keys.length - 1]);
      while (c <= endD) {
        var kk = isoDate(c);
        if ((map[kk] || 0) > 0) { run++; if (run > longest) longest = run; }
        else run = 0;
        c.setDate(c.getDate() + 1);
      }
    }

    return { bestDay: bestDay, bestN: bestN, bestMonth: bestMonth, curStreak: curStreak, longest: longest };
  }

  function heatmapHtml(porDia) {
    var dm = dayMap(porDia);
    var map = dm.map;
    var maxN = dm.maxN;
    var weeks = buildWeeks(map);
    var ins = insights(map);
    var totalYear = 0;
    Object.keys(map).forEach(function (k) { totalYear += map[k]; });

    var monthHtml = "";
    var prevMo = -1;
    weeks.forEach(function (week) {
      var mon = parseInt(week[0].dia.slice(5, 7), 10) - 1;
      var label = mon !== prevMo ? MONTH_ABBR[mon] : "";
      prevMo = mon;
      monthHtml += '<span class="al-heat-mo">' + esc(label) + "</span>";
    });

    var dowHtml = DOW_LABEL.map(function (l) {
      return '<span class="al-heat-dow-l">' + esc(l) + "</span>";
    }).join("");

    var cellsHtml = "";
    weeks.forEach(function (week) {
      cellsHtml += '<div class="al-heat-week">';
      week.forEach(function (cell) {
        var lvl = heatLevel(cell.n, maxN);
        var cls = "al-heat-cell al-heat-l-" + lvl + (cell.out ? " al-heat-out" : "");
        var tip = cell.n
          ? cell.n + " alerta" + (cell.n === 1 ? "" : "s") + " \u00b7 " + fmtDayEs(cell.dia)
          : fmtDayEs(cell.dia) + " \u00b7 sin alertas";
        cellsHtml += '<span class="' + cls + '" title="' + esc(tip) + '"></span>';
      });
      cellsHtml += "</div>";
    });

    var legendCells = [0, 1, 2, 3, 4].map(function (l) {
      return '<span class="al-heat-cell al-heat-l-' + l + '"></span>';
    }).join("");

    return (
      '<div class="al-heat-wrap">' +
        '<div class="al-heat-count">' + esc(totalYear) + ' alertas en el \u00faltimo a\u00f1o</div>' +
        '<div class="al-heat-inner">' +
          '<div class="al-heat-dow-col">' + dowHtml + "</div>" +
          '<div class="al-heat-board">' +
            '<div class="al-heat-months">' + monthHtml + "</div>" +
            '<div class="al-heat-cells">' + cellsHtml + "</div>" +
          "</div>" +
        "</div>" +
        '<div class="al-heat-meta">' +
          '<div><div class="al-heat-meta-l">Mes m\u00e1s activo</div><div class="al-heat-meta-n">' + esc(fmtMonthEs(ins.bestMonth)) + "</div></div>" +
          '<div><div class="al-heat-meta-l">D\u00eda m\u00e1s activo</div><div class="al-heat-meta-n">' + esc(fmtDayEs(ins.bestDay)) + "</div></div>" +
          '<div><div class="al-heat-meta-l">Racha m\u00e1s larga</div><div class="al-heat-meta-n">' + esc(ins.longest ? ins.longest + "d" : "\u2014") + "</div></div>" +
          '<div><div class="al-heat-meta-l">Racha actual</div><div class="al-heat-meta-n">' + esc(ins.curStreak ? ins.curStreak + "d" : "\u2014") + "</div></div>" +
        "</div>" +
        '<div class="al-heat-legend">' +
          '<span>Menos</span>' + legendCells + "<span>M\u00e1s</span>" +
        "</div>" +
      "</div>"
    );
  }

  global.rodajaAlertasHeatmapHtml = heatmapHtml;
})(typeof window !== "undefined" ? window : global);