(function (global) {
  "use strict";

  var page = 0;
  var pageSize = 5;
  var reqId = 0;
  var SIZES = [5, 10, 25];
  var apiFetch = null;
  var esc = function (s) { return String(s == null ? "" : s); };
  var onStatus = function () {};

  function $(id) { return document.getElementById(id); }

  function estado() {
    var e = "";
    try { e = new URLSearchParams(global.location.search).get("estado") || ""; } catch (err) { e = ""; }
    if (e === "previstas" || e === "confirmadas") return e;
    return "todas";
  }

  function tipoDe(est) {
    if (est === "previstas") return "temprana";
    if (est === "confirmadas") return "tardia";
    return "";
  }

  function writeEstado(est) {
    var u = new URL(global.location.href);
    if (est === "todas") u.searchParams.delete("estado");
    else u.searchParams.set("estado", est);
    global.history.replaceState(null, "", u.pathname + u.search + u.hash);
  }

  function scalar(v) {
    if (v == null) return null;
    return Array.isArray(v) ? (v.length ? v[0] : null) : v;
  }

  function num(v) {
    v = scalar(v);
    if (v == null || v === "") return null;
    var n = Number(v);
    return isFinite(n) ? n : null;
  }

  function digits(n) {
    return Math.abs(n * 10 - Math.round(n * 10)) < 1e-6 ? 1 : 2;
  }

  function fmtNum(v, unidad) {
    var n = num(v);
    if (n == null) return "\u2014";
    var s = new Intl.NumberFormat("es-CL", {
      minimumFractionDigits: digits(n),
      maximumFractionDigits: digits(n)
    }).format(n);
    return unidad ? s + unidad : s;
  }

  function fmtDiff(v, unidad) {
    var n = num(v);
    if (n == null) return "\u2014";
    var s = new Intl.NumberFormat("es-CL", {
      minimumFractionDigits: digits(n),
      maximumFractionDigits: digits(n),
      signDisplay: "exceptZero"
    }).format(n).replace(/^-/, "\u2212");
    return unidad ? s + unidad : s;
  }

  function fmtFecha(iso, fallback) {
    var m = String(iso || "").match(/^(\d{4})-(\d{2})-(\d{2})/);
    if (!m) return fallback || "";
    var d = new Date(Date.UTC(Number(m[1]), Number(m[2]) - 1, Number(m[3])));
    var s = new Intl.DateTimeFormat("es-CL", {
      day: "numeric", month: "short", year: "numeric", timeZone: "UTC"
    }).format(d);
    return s.replace(/\./g, "").replace(/\s+de\s+/g, " ").toLowerCase();
  }

  function icon(kind) {
    var paths = {
      chk: '<path d="M5 12l5 5L20 7"/>',
      no: '<path d="M6 6l12 12M18 6L6 18"/>',
      clock: '<circle cx="12" cy="12" r="9"/><path d="M12 7v5l3 2"/>',
      info: '<circle cx="12" cy="12" r="9"/><path d="M12 11v5M12 8h.01"/>'
    };
    return '<svg class="i" viewBox="0 0 24 24" aria-hidden="true">' + (paths[kind] || paths.info) + "</svg>";
  }

  function resultadoHtml(codigo, txt) {
    var map = { cumplio: "ok", no_cumplio: "no", pendiente: "pend", sin_dato: "pend" };
    var ic = { cumplio: "chk", no_cumplio: "no", pendiente: "clock", sin_dato: "info" };
    var cls = map[codigo] || "pend";
    return '<span class="al-res al-res--' + cls + '">' + icon(ic[codigo] || "info") + esc(txt || "") + "</span>";
  }

  function chipHtml(tipo) {
    var prev = tipo === "temprana";
    return '<span class="al-chip2 al-chip2--' + (prev ? "p" : "c") + '">' +
      (prev ? "Prevista" : "Confirmada") + "</span>";
  }

  function lead(est) {
    if (est === "previstas") return "Cada previsi\u00f3n se compara con lo que realmente se observ\u00f3.";
    if (est === "confirmadas") return "Eventos que ya ocurrieron, con su posici\u00f3n entre todos los d\u00edas registrados de la estaci\u00f3n.";
    return "Previstas y confirmadas, de la m\u00e1s reciente a la m\u00e1s antigua.";
  }

  function legend(est) {
    if (est === "previstas") return "Diferencia = observado \u2212 pronosticado. Falsa alarma = se previ\u00f3 un extremo que no ocurri\u00f3.";
    if (est === "confirmadas") return "Ranking: posici\u00f3n del d\u00eda entre todos los registros de la estaci\u00f3n (1.\u00ba = el m\u00e1s extremo). Un valor raro en la costa puede ser normal en un valle.";
    return "Valor = pronosticado en las previstas, observado en las confirmadas. \u00abSe cumpli\u00f3\u00bb = lo observado tambi\u00e9n super\u00f3 el umbral de alerta.";
  }

  function th(label, num) {
    return '<th scope="col"' + (num ? ' class="n"' : "") + ">" + label + "</th>";
  }

  function filaTodas(ev) {
    var u = ev.unidad || "";
    var prev = ev.tipo === "temprana";
    var valor = prev ? ev.valor_pronostico : ev.valor;
    var detalle = prev
      ? resultadoHtml(ev.resultado, ev.resultado_txt) +
        (num(ev.valor_observado) != null ? ' <span class="al-mut">\u00b7 obs. ' + esc(fmtNum(ev.valor_observado, u)) + "</span>" : "")
      : esc(ev.ranking_txt || "");
    return "<tr><td>" + chipHtml(ev.tipo) + "</td><td>" + esc(ev.estacion) + "</td><td>" +
      esc(ev.alerta) + "<small>" + esc(ev.variable) + "</small></td><td>" + esc(fmtFecha(ev.fecha, ev.fecha_txt)) +
      '</td><td class="n">' + esc(fmtNum(valor, u)) + ' <span class="al-mut">' + (prev ? "pron." : "obs.") +
      "</span></td><td>" + detalle + "</td></tr>";
  }

  function filaPrev(ev) {
    var u = ev.unidad || "";
    var diff = fmtDiff(ev.diferencia, u);
    var malo = ev.resultado === "no_cumplio";
    return "<tr><td>" + esc(ev.estacion) + "</td><td>" + esc(ev.alerta) + "</td><td>" +
      esc(fmtFecha(ev.fecha, ev.fecha_txt)) + '</td><td class="n">' + esc(fmtNum(ev.valor_pronostico, u)) +
      '</td><td class="n">' + esc(fmtNum(ev.valor_observado, u)) + '</td><td class="n">' +
      (malo ? "<b class=\"al-res al-res--no\">" + esc(diff) + "</b>" : esc(diff)) +
      "</td><td>" + resultadoHtml(ev.resultado, ev.resultado_txt) + "</td></tr>";
  }

  function filaConf(ev) {
    var u = ev.unidad || "";
    var si = ev.fue_prevista === true || ev.fue_prevista === "true";
    return "<tr><td>" + esc(ev.estacion) + "</td><td>" + esc(ev.alerta) +
      "<small>" + esc(ev.variable) + "</small></td><td>" + esc(fmtFecha(ev.fecha, ev.fecha_txt)) +
      '</td><td class="n">' + esc(fmtNum(ev.valor, u)) + "</td><td>" + esc(ev.ranking_txt || "") +
      "</td><td>" + resultadoHtml(si ? "cumplio" : "no_cumplio", si ? "S\u00ed" : "No") + "</td></tr>";
  }

  function tabla(est, items) {
    var head, rows;
    if (est === "previstas") {
      head = th("Estaci\u00f3n") + th("Alerta") + th("Fecha") + th("Pronosticado", true) +
        th("Observado", true) + th("Diferencia", true) + th("Resultado");
      rows = items.map(filaPrev).join("");
    } else if (est === "confirmadas") {
      head = th("Estaci\u00f3n") + th("Alerta") + th("Fecha") + th("Valor", true) +
        th("Ranking en la estaci\u00f3n") + th("\u00bfFue prevista?");
      rows = items.map(filaConf).join("");
    } else {
      head = th("Estado") + th("Estaci\u00f3n") + th("Alerta") + th("Fecha") + th("Valor", true) + th("Detalle");
      rows = items.map(filaTodas).join("");
    }
    return '<div class="al-scroll"><table class="al-tb"><thead><tr>' + head + "</tr></thead><tbody>" +
      rows + "</tbody></table></div>";
  }

  function resumenHtml(r) {
    if (!r) return "";
    var et = esc(r.etiqueta || "\u00faltimos 30 d\u00edas");
    if (!r.resueltas) {
      return '<p class="al-sum">Ninguna prevista resuelta en los ' + et + ".</p>";
    }
    var mae = r.mae_mixta ? "varias unidades" : (r.mae == null ? "\u2014" : fmtNum(r.mae, r.unidad || ""));
    return '<div class="al-sum"><span><b>' + esc(r.aciertos) + " de " + esc(r.resueltas) +
      "</b> previstas se cumplieron</span><span><b>" + esc(mae) +
      "</b> error medio absoluto</span><span><b>" + esc(r.falsas_alarmas) +
      "</b> " + (Number(r.falsas_alarmas) === 1 ? "falsa alarma" : "falsas alarmas") +
      '</span><span class="al-mut">' + et + "</span></div>";
  }

  function pager(total, est) {
    var pages = Math.max(1, Math.ceil(total / pageSize));
    if (page >= pages) page = pages - 1;
    if (page < 0) page = 0;
    var start = total ? page * pageSize + 1 : 0;
    var end = Math.min(total, (page + 1) * pageSize);
    var sizes = SIZES.map(function (n) {
      return '<option value="' + n + '"' + (n === pageSize ? " selected" : "") + ">" + n + "</option>";
    }).join("");
    return '<div class="al-ft"><span class="al-mut">' + start + "\u2013" + end + " de " + total + "</span>" +
      '<label class="al-mut">Por p\u00e1gina <select id="al-hist-size">' + sizes + "</select></label>" +
      '<span class="al-ft-nav">' +
      '<button type="button" class="al-btn" id="al-hist-prev"' + (page <= 0 ? " disabled" : "") + ">Anterior</button>" +
      '<button type="button" class="al-btn" id="al-hist-next"' + (page >= pages - 1 ? " disabled" : "") + ">Siguiente</button>" +
      "</span></div>" +
      '<p class="al-leg">' + legend(est) + "</p>";
  }

  function wirePager(pages) {
    var prev = $("al-hist-prev");
    var next = $("al-hist-next");
    var size = $("al-hist-size");
    if (prev) prev.addEventListener("click", function () {
      if (page <= 0) return;
      page -= 1;
      load();
    });
    if (next) next.addEventListener("click", function () {
      if (page >= pages - 1) return;
      page += 1;
      load();
    });
    if (size) size.addEventListener("change", function () {
      var n = parseInt(size.value, 10);
      if (SIZES.indexOf(n) < 0) return;
      pageSize = n;
      page = 0;
      load();
    });
  }

  function anunciar(est, nActivas) {
    var live = $("al-live");
    if (!live) return;
    var n = nActivas == null ? 0 : nActivas;
    var txt = est === "todas"
      ? "Mostrando todas las alertas"
      : "Mostrando " + n + " alertas " + est;
    if (live.textContent === txt) live.textContent = "";
    live.textContent = txt;
  }

  function activas() {
    if (typeof global.rodajaDigestCount === "function") return global.rodajaDigestCount();
    return 0;
  }

  function paintFiltro() {
    var host = $("al-filtro");
    if (!host) return;
    var est = estado();
    function radio(val, label) {
      var id = "al-f-" + val;
      return '<input type="radio" name="al-estado" id="' + id + '" value="' + val + '"' +
        (est === val ? " checked" : "") + "><label for=\"" + id + "\">" + label + "</label>";
    }
    host.innerHTML =
      '<div class="al-bar">' +
      '<fieldset class="al-seg2f"><legend class="al-sr">Filtrar alertas</legend>' +
      radio("todas", "Todas") + radio("previstas", "Previstas") + radio("confirmadas", "Confirmadas") +
      "</fieldset>" +
      '<p class="al-mut al-help">Previstas = pron\u00f3stico pr\u00f3ximo \u00b7 Confirmadas = ya ocurri\u00f3. El filtro aplica a alertas activas e historial.</p>' +
      "</div>";
    host.querySelectorAll('input[name="al-estado"]').forEach(function (inp) {
      inp.addEventListener("change", function () {
        if (!inp.checked) return;
        writeEstado(inp.value);
        page = 0;
        if (typeof global.rodajaDigestSetTipo === "function") global.rodajaDigestSetTipo(tipoDe(inp.value));
        var leadEl = $("al-hist-lead");
        if (leadEl) leadEl.textContent = lead(inp.value);
        load();
        anunciar(inp.value, activas());
      });
    });
  }

  function load() {
    var wrap = $("al-historial-wrap");
    if (!wrap || typeof apiFetch !== "function") return;
    var est = estado();
    var id = ++reqId;
    var leadEl = $("al-hist-lead");
    if (leadEl) leadEl.textContent = lead(est);
    apiFetch("/me/alertas/eventos?limit=" + pageSize + "&offset=" + (page * pageSize) +
      "&estado=" + encodeURIComponent(est)).then(function (data) {
      if (id !== reqId || !$("al-historial-wrap")) return;
      wrap = $("al-historial-wrap");
      var items = (data && data.eventos) || [];
      if (!Array.isArray(items)) items = [];
      var total = parseInt(scalar(data && data.total), 10);
      if (isNaN(total) || total < items.length) total = items.length;
      var sum = $("al-hist-sum");
      if (sum) sum.innerHTML = est === "previstas" ? resumenHtml(data && data.resumen_previstas) : "";
      if (!total) {
        wrap.innerHTML = '<p class="al-empty">A\u00fan no hay alertas en esta vista.</p>';
        return;
      }
      var pages = Math.max(1, Math.ceil(total / pageSize));
      if (page >= pages) { page = pages - 1; load(); return; }
      wrap.innerHTML = tabla(est, items) + pager(total, est);
      wirePager(pages);
    }).catch(function () {
      if (id !== reqId) return;
      wrap = $("al-historial-wrap");
      if (wrap) wrap.innerHTML = '<p class="al-empty">No se pudo cargar el historial.</p>';
    });
  }

  function wireCsv() {
    var btn = $("al-historial-csv");
    if (!btn || btn.getAttribute("data-wired") === "1") return;
    btn.setAttribute("data-wired", "1");
    btn.addEventListener("click", function () {
      if (typeof global.rodajaDownloadFile !== "function") {
        onStatus("No se pudo iniciar la descarga.", false);
        return;
      }
      var est = estado();
      btn.disabled = true;
      onStatus("Preparando la descarga\u2026", true);
      global.rodajaDownloadFile(
        "/me/alertas/eventos.csv?estado=" + encodeURIComponent(est),
        "rodaja_alertas_historial_" + est + ".csv"
      ).then(function () { onStatus("Historial descargado.", true); })
        .catch(function (err) { onStatus((err && err.message) || "No se pudo descargar el historial.", false); })
        .then(function () { btn.disabled = false; });
    });
  }

  function sidebarVisible() {
    var side = document.getElementById("ra-sidebar");
    if (!side) return;
    if (global.matchMedia && global.matchMedia("(max-width: 900px)").matches) {
      side.removeAttribute("aria-hidden");
    }
  }

  global.rodajaResumenEstado = estado;
  global.rodajaResumenTipo = function () { return tipoDe(estado()); };
  global.rodajaResumenAnunciar = function () { anunciar(estado(), activas()); };
  global.rodajaResumenLoad = load;

  global.rodajaResumenBoot = function (opts) {
    opts = opts || {};
    apiFetch = opts.apiFetch || apiFetch;
    if (opts.esc) esc = opts.esc;
    if (opts.setStatus) onStatus = opts.setStatus;
    paintFiltro();
    wireCsv();
    load();
    sidebarVisible();
    if (!sidebarVisible.bound) {
      sidebarVisible.bound = true;
      global.addEventListener("resize", sidebarVisible);
    }
  };
})(typeof window !== "undefined" ? window : global);
