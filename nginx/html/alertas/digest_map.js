(function (global) {
  "use strict";

  var SVG_URL = "/alertas/geo/comunas.svg";
  // Secuencial unipolar anclada en marca #DF9531 (ColorBrewer Oranges adaptado)
  var ZERO = "#F4F6F8";
  var NO_COVER = "#E0E4E8";
  var ORANGES = [ZERO, "#FCE4C4", "#F5C078", "#DF9531", "#A65F12"];
  var svgCache = null;
  var svgPromise = null;

  function esc(s) {
    var d = document.createElement("div");
    d.textContent = s == null ? "" : String(s);
    return d.innerHTML;
  }

  function asArray(val) {
    if (val == null) return [];
    if (Array.isArray(val)) return val;
    if (typeof val === "object") {
      var keys = Object.keys(val);
      if (keys.every(function (k) { return /^\d+$/.test(k); })) {
        keys.sort(function (a, b) { return Number(a) - Number(b); });
        return keys.map(function (k) { return val[k]; });
      }
    }
    return [];
  }

  function loadSvg() {
    if (svgCache) return Promise.resolve(svgCache);
    if (svgPromise) return svgPromise;
    svgPromise = fetch(SVG_URL, { credentials: "same-origin" })
      .then(function (r) {
        if (!r.ok) throw new Error("SVG " + r.status);
        return r.text();
      })
      .then(function (txt) {
        svgCache = txt;
        return txt;
      })
      .catch(function (err) {
        svgPromise = null;
        throw err;
      });
    return svgPromise;
  }

  function filterCounts(row, sevFilter, tipoFilter) {
    if (row && ("tardia" in row || "temprana" in row) && !("critica" in row)) {
      var tardiaN = (!tipoFilter || tipoFilter === "tardia") ? Number(row.tardia || 0) : 0;
      var tempranaN = (!tipoFilter || tipoFilter === "temprana") ? Number(row.temprana || 0) : 0;
      return { tardia: tardiaN, temprana: tempranaN, total: tardiaN + tempranaN };
    }
    var c = 0;
    var d = 0;
    if (!tipoFilter || tipoFilter === "tardia") {
      if (!sevFilter || sevFilter === "critica") c += Number(row.critica_tardia || 0);
      if (!sevFilter || sevFilter === "aviso_de_madrugada") d += Number(row.destacada_tardia || 0);
    }
    if (!tipoFilter || tipoFilter === "temprana") {
      if (!sevFilter || sevFilter === "critica") c += Number(row.critica_temprana || 0);
      if (!sevFilter || sevFilter === "aviso_de_madrugada") d += Number(row.destacada_temprana || 0);
    }
    if (!tipoFilter && !("critica_tardia" in row) && !("critica_temprana" in row)) {
      if (!sevFilter || sevFilter === "critica") c = Number(row.critica || 0);
      if (!sevFilter || sevFilter === "aviso_de_madrugada") d = Number(row.destacada || 0);
    }
    return { critica: c, destacada: d, total: c + d };
  }

  function indexByGid(conteos, sevFilter, tipoFilter) {
    var map = {};
    asArray(conteos).forEach(function (row) {
      if (!row || !row.id) return;
      map[String(row.id)] = {
        etiqueta: row.etiqueta || String(row.id).replace(/^com:/, ""),
        counts: filterCounts(row, sevFilter, tipoFilter)
      };
    });
    return map;
  }

  function coberturaSet(cobertura) {
    var set = {};
    asArray(cobertura).forEach(function (id) {
      if (id) set[String(id)] = 1;
    });
    return set;
  }

  function uniqueAsc(nums) {
    var out = [];
    nums.slice().sort(function (a, b) { return a - b; }).forEach(function (n) {
      if (!out.length || out[out.length - 1] !== n) out.push(n);
    });
    return out;
  }

  /** Cuantiles sobre valores > 0 → hasta 4 tramos positivos (+ cero = 5). */
  function buildScale(positiveTotals) {
    var pos = uniqueAsc(positiveTotals.filter(function (n) { return n > 0; }));
    var labels = ["0"];
    var uppers = [0];

    if (!pos.length) {
      return {
        labels: labels.concat(["\u2014", "\u2014", "\u2014", "\u2014"]),
        colors: ORANGES,
        index: function () { return 0; }
      };
    }

    // 3 cortes (p25/p50/p75) sobre la serie positiva; dedupe si hay pocos valores
    function qAt(p) {
      var i = Math.min(pos.length - 1, Math.max(0, Math.ceil(p * pos.length) - 1));
      return pos[i];
    }
    var cuts = uniqueAsc([qAt(0.25), qAt(0.5), qAt(0.75)]);
    // Asegurar que el máximo quede en el último tramo
    var max = pos[pos.length - 1];
    var edges = cuts.filter(function (c) { return c < max; });
    edges.push(max);

    // edges[i] = upper bound inclusive del tramo positivo i+1
    var prev = 0;
    edges.forEach(function (hi) {
      var lo = prev + 1;
      if (lo > hi) return;
      labels.push(lo === hi ? String(lo) : lo + "\u2013" + hi);
      uppers.push(hi);
      prev = hi;
    });
    // Rellenar hasta 5 entradas de leyenda (0 + 4)
    while (labels.length < 5) {
      labels.push("\u2014");
      uppers.push(uppers[uppers.length - 1]);
    }
    labels = labels.slice(0, 5);
    uppers = uppers.slice(0, 5);

    return {
      labels: labels,
      colors: ORANGES,
      index: function (n) {
        var v = Number(n) || 0;
        if (v <= 0) return 0;
        for (var i = 1; i < uppers.length; i++) {
          if (v <= uppers[i]) return i;
        }
        return uppers.length - 1;
      }
    };
  }

  function ensureHatch(svg) {
    if (!svg || svg.querySelector("#al-digest-hatch")) return;
    var ns = "http://www.w3.org/2000/svg";
    var defs = svg.querySelector("defs");
    if (!defs) {
      defs = document.createElementNS(ns, "defs");
      svg.insertBefore(defs, svg.firstChild);
    }
    var pat = document.createElementNS(ns, "pattern");
    pat.setAttribute("id", "al-digest-hatch");
    pat.setAttribute("patternUnits", "userSpaceOnUse");
    pat.setAttribute("width", "6");
    pat.setAttribute("height", "6");
    pat.setAttribute("patternTransform", "rotate(45)");
    var bg = document.createElementNS(ns, "rect");
    bg.setAttribute("width", "6");
    bg.setAttribute("height", "6");
    bg.setAttribute("fill", NO_COVER);
    var line = document.createElementNS(ns, "line");
    line.setAttribute("x1", "0");
    line.setAttribute("y1", "0");
    line.setAttribute("x2", "0");
    line.setAttribute("y2", "6");
    line.setAttribute("stroke", "#B8C0C8");
    line.setAttribute("stroke-width", "2");
    pat.appendChild(bg);
    pat.appendChild(line);
    defs.appendChild(pat);
  }

  function legendHtml(scale, hasNoCover) {
    var html = '<div class="al-digest-map-legend" aria-hidden="true">';
    scale.labels.forEach(function (lab, i) {
      if (!lab || lab === "\u2014") return;
      html += '<span class="al-digest-map-leg-item">' +
        '<span class="al-digest-map-swatch" style="background:' + scale.colors[Math.min(i, scale.colors.length - 1)] + '"></span>' +
        esc(lab) + "</span>";
    });
    if (hasNoCover) {
      html += '<span class="al-digest-map-leg-item">' +
        '<span class="al-digest-map-swatch al-digest-map-swatch--hatch" title="Sin estaciones"></span>' +
        "Sin cobertura</span>";
    }
    html += "</div>";
    return html;
  }

  function shellHtml() {
    return '<aside class="al-digest-map" id="al-digest-map" aria-label="Mapa de alertas por comuna">' +
      '<div class="al-digest-map-title">Por comuna</div>' +
      '<div class="al-digest-map-canvas" id="al-digest-map-canvas"></div>' +
      '<div id="al-digest-map-legend-host"></div>' +
      '<div class="al-digest-map-tip" id="al-digest-map-tip" hidden></div>' +
      "</aside>";
  }

  function paint(host, conteos, sevFilter, tipoFilter, onComunaClick, cobertura) {
    if (!host) return Promise.resolve();
    var canvas = host.querySelector("#al-digest-map-canvas") || host;
    var legendHost = host.querySelector("#al-digest-map-legend-host");
    var tip = host.querySelector("#al-digest-map-tip");
    var byGid = indexByGid(conteos, sevFilter, tipoFilter);
    var cover = coberturaSet(cobertura);
    var hasCoverList = Object.keys(cover).length > 0;
    var positives = [];
    Object.keys(byGid).forEach(function (gid) {
      var t = byGid[gid].counts.total;
      if (t > 0) positives.push(t);
    });
    var scale = buildScale(positives);
    var filterSig = String(sevFilter || "") + "|" + String(tipoFilter || "");

    if (legendHost) legendHost.innerHTML = legendHtml(scale, hasCoverList);

    function applyFills(root) {
      var svg = root.querySelector("svg");
      ensureHatch(svg);
      root.querySelectorAll("path[data-gid]").forEach(function (path) {
        var gid = path.getAttribute("data-gid") || "";
        var row = byGid[gid];
        var total = row ? row.counts.total : 0;
        var covered = !hasCoverList || !!cover[gid];
        var label = path.getAttribute("data-label") || gid.replace(/^com:/, "");
        var title;

        path.removeAttribute("stroke-dasharray");
        if (!covered) {
          path.setAttribute("fill", "url(#al-digest-hatch)");
          path.setAttribute("data-total", "0");
          path.setAttribute("data-cover", "0");
          path.style.cursor = "default";
          title = label + ": sin cobertura (sin estaciones)";
        } else {
          var bi = scale.index(total);
          path.setAttribute("fill", scale.colors[bi]);
          path.setAttribute("data-total", String(total));
          path.setAttribute("data-cover", "1");
          path.style.cursor = total > 0 ? "pointer" : "default";
          if (total > 0 && row) {
            var bits = [];
            var nt = Number(row.counts.tardia || 0);
            var np = Number(row.counts.temprana || 0);
            if (nt) bits.push(nt + " tard\u00eda" + (nt === 1 ? "" : "s"));
            if (np) bits.push(np + " temprana" + (np === 1 ? "" : "s"));
            title = label + ": " + (bits.join(" \u00b7 ") || (total + " alerta" + (total === 1 ? "" : "s")));
          } else {
            title = label + ": 0 alertas";
          }
        }
        path.setAttribute("data-tip", title);
      });
    }

    function wire(root) {
      if (root.getAttribute("data-map-wired") === "1") return;
      root.setAttribute("data-map-wired", "1");
      root.addEventListener("mouseover", function (ev) {
        var path = ev.target.closest && ev.target.closest("path[data-gid]");
        if (!path || !tip) return;
        tip.hidden = false;
        tip.textContent = path.getAttribute("data-tip") || "";
      });
      root.addEventListener("mouseout", function (ev) {
        var path = ev.target.closest && ev.target.closest("path[data-gid]");
        if (!path || !tip) return;
        tip.hidden = true;
      });
      root.addEventListener("mousemove", function (ev) {
        if (!tip || tip.hidden) return;
        var rect = host.getBoundingClientRect();
        tip.style.left = Math.min(rect.width - 8, Math.max(8, ev.clientX - rect.left + 12)) + "px";
        tip.style.top = Math.min(rect.height - 8, Math.max(8, ev.clientY - rect.top + 12)) + "px";
      });
      root.addEventListener("click", function (ev) {
        var path = ev.target.closest && ev.target.closest("path[data-gid]");
        if (!path) return;
        if (Number(path.getAttribute("data-total") || 0) <= 0) return;
        var gid = path.getAttribute("data-gid") || "";
        var cb = host._onComunaClick;
        if (typeof cb === "function") cb(gid);
      });
    }

    host._onComunaClick = onComunaClick;
    host._cobertura = cobertura;

    if (canvas.querySelector("svg")) {
      canvas.setAttribute("data-filter-sig", filterSig);
      applyFills(canvas);
      wire(canvas);
      return Promise.resolve();
    }

    return loadSvg().then(function (txt) {
      canvas.innerHTML = txt;
      canvas.setAttribute("data-filter-sig", filterSig);
      var svg = canvas.querySelector("svg");
      if (svg) {
        svg.setAttribute("preserveAspectRatio", "xMidYMin meet");
        svg.removeAttribute("width");
        svg.removeAttribute("height");
        svg.style.cssText = "position:absolute;inset:0;width:100%;height:100%;display:block;";
      }
      applyFills(canvas);
      wire(canvas);
    }).catch(function (err) {
      console.error("digest_map", err);
      canvas.innerHTML = '<p class="al-digest-map-err">No se pudo cargar el mapa.</p>';
    });
  }

  function updateFills(host, conteos, sevFilter, tipoFilter) {
    paint(host, conteos, sevFilter, tipoFilter, host && host._onComunaClick, host && host._cobertura);
  }

  global.rodajaDigestMap = {
    shellHtml: shellHtml,
    paint: paint,
    updateFills: updateFills,
    filterSig: function (sev, tipo) { return String(sev || "") + "|" + String(tipo || ""); }
  };
})(window);
