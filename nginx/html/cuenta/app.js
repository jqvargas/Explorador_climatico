(function () {
  "use strict";

  var state = {
    me: null,
    cuota: null,
    billing: null,
    onDemand: null,
    payment: null,
    descargas: null,
    alertasStats: null,
    alertasMain: null,
    eventos: []
  };
  var chartDescargas = null;
  var chartAlertas = null;
  var authReady = null;
  var dashboardRoot = null;
  var profileMounted = false;

  function $(id, root) {
    var r = root || dashboardRoot || document;
    if (r === document) return document.getElementById(id);
    return r.querySelector("#" + id);
  }

  function esc(s) {
    return String(s == null ? "" : s)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function scalar(v) {
    return Array.isArray(v) ? v[0] : v;
  }

  function showBanner(text, kind) {
    var el = document.getElementById("cu-status-banner");
    if (!el) return;
    if (!text) {
      el.hidden = true;
      el.textContent = "";
      return;
    }
    el.textContent = text;
    el.className = "cu-banner cu-banner-" + (kind || "info");
    el.hidden = false;
  }

  function isExternalFlowUrl(url) {
    try {
      var host = new URL(url, location.href).hostname.toLowerCase();
      return host === "www.flow.cl" || host === "sandbox.flow.cl" ||
        (host.length > 8 && host.slice(-8) === ".flow.cl");
    } catch (e) {
      return false;
    }
  }

  function showCheckoutBlocked(url) {
    var el = document.getElementById("cu-status-banner");
    if (!el) return;
    el.className = "cu-banner cu-banner-warn";
    el.hidden = false;
    el.textContent = "";
    el.appendChild(document.createTextNode("El navegador bloqueó la ventana de pago. "));
    var a = document.createElement("a");
    a.href = url;
    a.target = "_blank";
    a.rel = "noopener noreferrer";
    a.textContent = "Abrir Flow en una pestaña nueva";
    el.appendChild(a);
  }

  // ponytail: TWA _blank → CCT/Chrome con barra si flow.cl NO es extra origin (techo: popup blocker; upgrade = tap del link, nunca extra origin Flow).
  function openCheckoutUrl(url, btn) {
    if (!url) return false;
    if (!isExternalFlowUrl(url)) {
      window.location.assign(url);
      return true;
    }
    var w = window.open(url, "_blank", "noopener,noreferrer");
    if (btn) {
      btn.disabled = false;
      btn.textContent = btn.getAttribute("data-label") || btn.textContent;
    }
    if (w) return true;
    showCheckoutBlocked(url);
    return false;
  }

  function ensureAuthReady() {
    if (authReady) return authReady;
    if (typeof rodajaInitAuth !== "function") {
      authReady = Promise.resolve();
      return authReady;
    }
    authReady = rodajaInitAuth().catch(function () {
      authReady = null;
    });
    return authReady;
  }

  function isSignedIn() {
    return typeof rodajaIsSignedIn === "function" && rodajaIsSignedIn();
  }

  function apiFetch(path) {
    if (typeof rodajaApiFetch !== "function") return Promise.reject(new Error("Auth no disponible"));
    return rodajaApiFetch(path).then(function (r) {
      return r.text().then(function (text) {
        var body = null;
        if (text) {
          try { body = JSON.parse(text); } catch (e) {
            throw new Error("HTTP " + r.status);
          }
        }
        if (!r.ok) throw new Error((body && body.error) || ("HTTP " + r.status));
        return body;
      });
    });
  }

  function apiFetchSafe(path) {
    return apiFetch(path).catch(function (err) {
      return { _error: err.message || "Error" };
    });
  }

  function formatBytes(n) {
    n = Number(n);
    if (!n || isNaN(n) || n < 0) return "0 B";
    var units = ["B", "KB", "MB", "GB"];
    var i = 0;
    var v = n;
    while (v >= 1024 && i < units.length - 1) {
      v /= 1024;
      i++;
    }
    var decimals = i > 0 && v < 10 ? 1 : 0;
    return v.toFixed(decimals) + " " + units[i];
  }

  function formatResetDate(iso) {
    if (!iso) return "";
    try {
      return new Date(iso + "T12:00:00").toLocaleDateString("es-CL", { day: "numeric", month: "short" });
    } catch (e) {
      return iso;
    }
  }

  function billingFieldString(v) {
    v = scalar(v);
    if (v == null || v === "") return "";
    if (typeof v === "object") {
      if (v.status != null) return String(v.status);
      if (v.label != null) return String(v.label);
      return "";
    }
    return String(v);
  }

  function formatDateTime(iso) {
    if (!iso) return "\u2014";
    var raw = billingFieldString(iso);
    if (!raw) return "\u2014";
    var normalized = raw;
    if (/^\d{4}-\d{2}-\d{2}$/.test(raw)) {
      normalized = raw + "T12:00:00Z";
    } else if (raw.indexOf(" ") >= 0 && raw.indexOf("T") < 0) {
      normalized = raw.replace(" ", "T");
    }
    try {
      var d = new Date(normalized);
      if (isNaN(d.getTime())) return "\u2014";
      return d.toLocaleString("es-CL", {
        day: "numeric", month: "short", year: "numeric",
        hour: "2-digit", minute: "2-digit"
      });
    } catch (e) {
      return "\u2014";
    }
  }

  function tierLabel(tier) {
    var t = String(scalar(tier) || "gratuito").toLowerCase();
    if (t === "enterprise") return "Institucional";
    if (t === "pro") return "Pro";
    return "Gratuito";
  }

  function subscriptionStatusLabel(status) {
    var s = billingFieldString(status).toLowerCase();
    if (!s) return "";
    if (s === "active" || s === "activa" || s === "1") return "Activa";
    if (s === "canceled" || s === "cancelada") return "Cancelada";
    if (s === "past_due") return "Pago pendiente";
    return billingFieldString(status);
  }

  function isPaidTier(tier) {
    var t = String(scalar(tier) || "gratuito").toLowerCase();
    return t === "pro" || t === "enterprise";
  }

  function dashBadgeClass(estado) {
    var e = String(estado || "").toLowerCase();
    if (e === "listo") return "cu-dash-badge-listo";
    if (e === "error" || e === "fallido") return "cu-dash-badge-error";
    if (e === "procesando" || e === "pendiente") return "cu-dash-badge-procesando";
    return "";
  }

  function formatPct(pct) {
    pct = Number(pct) || 0;
    if (pct >= 100) return "100%";
    if (pct >= 10) return pct.toFixed(0) + "%";
    if (pct >= 1) return pct.toFixed(1) + "%";
    if (pct > 0) return pct.toFixed(2) + "%";
    return "0%";
  }

  function formatClp(n) {
    n = Number(n);
    if (isNaN(n)) n = 0;
    return new Intl.NumberFormat("es-CL", {
      style: "currency",
      currency: "CLP",
      maximumFractionDigits: 0
    }).format(n);
  }

  function onDemandDisponible(od) {
    if (!od || od._error || !od.eligible) return false;
    if (od.on_demand_disponible != null) return !!od.on_demand_disponible;
    if (!od.enabled) return !!od.can_enable;
    if (od.limit_clp == null) return true;
    return (Number(od.clp_usado) || 0) < Number(od.limit_clp);
  }

  function usageTotalCardHtml(tier, cuota) {
    if (!cuota) return "";
    var usado = Number(cuota.bytes_usados) || 0;
    var limite = Number(cuota.limite_bytes) || 0;
    var pct = limite > 0 ? Math.min(100, (usado / limite) * 100) : 0;
    var fillClass = "cu-dash-total-fill";
    if (pct >= 100) fillClass += " cu-dash-total-fill--over";
    else if (pct >= 80) fillClass += " cu-dash-total-fill--warn";

    var detail = esc(formatBytes(usado)) + " de " + esc(formatBytes(limite)) + " usados";
    if (cuota.reset_en) {
      detail += " \u00b7 se renueva el " + esc(formatResetDate(cuota.reset_en));
    }
    var saldo = Number(cuota.bytes_extra_saldo) || 0;
    if (saldo > 0) {
      detail += " \u00b7 saldo de compra \u00fanica " + esc(formatBytes(saldo));
    }

    return '<div class="cu-dash-included">' +
        '<p class="cu-dash-included-label">Incluido en ' + esc(tierLabel(tier)) + "</p>" +
        '<div class="cu-dash-total-card">' +
          '<div class="cu-dash-total-row">' +
            '<span class="cu-dash-total-label">Total</span>' +
            '<span class="cu-dash-total-pct">' + esc(formatPct(pct)) + "</span>" +
          "</div>" +
          '<div class="cu-dash-total-bar"><span class="' + fillClass + '" style="width:' + pct.toFixed(1) + '%"></span></div>' +
          '<p class="cu-dash-total-detail">' + detail +
            '<svg class="cu-dash-chevron" width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true"><path d="M6 9l6 6 6-6"/></svg>' +
          "</p>" +
        "</div>" +
      "</div>";
  }

  function chartBaseOptions(yTitle) {
    var opts = {
      responsive: true,
      maintainAspectRatio: false,
      plugins: { legend: { display: false } },
      scales: {
        x: {
          grid: { display: false },
          ticks: { color: "#737373", font: { size: 11 } }
        },
        y: {
          beginAtZero: true,
          grid: { color: "#f0f0f0" },
          border: { display: false },
          ticks: { color: "#737373", font: { size: 11 }, padding: 8 }
        }
      }
    };
    if (yTitle) {
      opts.scales.y.title = { display: true, text: yTitle, color: "#a3a3a3", font: { size: 11 } };
    } else {
      opts.scales.y.ticks.stepSize = 1;
    }
    return opts;
  }

  function destroyCharts() {
    if (chartDescargas) {
      chartDescargas.destroy();
      chartDescargas = null;
    }
    if (chartAlertas) {
      chartAlertas.destroy();
      chartAlertas = null;
    }
  }

  function loadChartLib() {
    if (typeof Chart !== "undefined") return Promise.resolve();
    return new Promise(function (resolve, reject) {
      var s = document.createElement("script");
      s.src = "https://cdn.jsdelivr.net/npm/chart.js@4.4.1/dist/chart.umd.min.js";
      s.onload = function () { resolve(); };
      s.onerror = function () { reject(new Error("Chart.js")); };
      document.head.appendChild(s);
    });
  }

  function renderPlan(root) {
    var el = $("cu-plan", root);
    if (!el) return;
    var b = state.billing;
    var me = state.me;
    if (b && b._error) {
      el.innerHTML = '<section class="cu-dash-panel"><p class="cu-dash-error">' + esc(b._error) + "</p></section>";
      return;
    }
    if (!b || !me) {
      el.innerHTML = '<section class="cu-dash-panel"><p class="cu-dash-empty">Cargando\u2026</p></section>';
      return;
    }

    var tier = String(scalar(b.tier) || scalar(me.tier) || "gratuito").toLowerCase();
    var paid = isPaidTier(tier);
    var periodEnd = b.subscription_period_end;
    var periodFmt = periodEnd ? formatDateTime(periodEnd) : "";
    var statusTxt = subscriptionStatusLabel(b.subscription_status);
    if (paid && !statusTxt) statusTxt = "Activa";

    var pill = paid || b.has_active_subscription === true
      ? '<span class="cu-dash-pill cu-dash-pill--ok">' + esc(statusTxt || "Activa") + "</span>"
      : '<span class="cu-dash-pill cu-dash-pill--muted">Sin suscripci\u00f3n</span>';

    var caption = "";
    if (paid && periodFmt && periodFmt.indexOf("\u2014") < 0) {
      caption = "Pr\u00f3ximo cobro \u00b7 " + esc(periodFmt);
    } else if (!paid) {
      caption = "Plan gratuito con l\u00edmite mensual";
    }

    var actions = "";
    if (!paid && b.can_upgrade === true && b.checkout_enabled === true) {
      actions += '<button type="button" class="cu-dash-btn cu-dash-btn--primary cu-btn-upgrade">Suscribirse a Pro</button>';
    } else if (!paid && b.can_upgrade === true && b.checkout_enabled !== true) {
      actions += '<span class="cu-dash-caption">El pago en l\u00ednea no est\u00e1 disponible.</span>' +
        '<a class="cu-dash-btn cu-dash-btn--ghost" href="/precios/">Ver planes</a>';
    }
    if (b.can_cancel === true) {
      actions += '<button type="button" class="cu-dash-btn cu-dash-btn--danger cu-btn-cancel">Cancelar</button>';
    }
    if (tier === "enterprise") {
      actions += '<a class="cu-dash-btn cu-dash-btn--ghost" href="/formulario/?asunto=Plan%20Institucional">Contactar ventas</a>';
    }
    if (!actions && !paid) {
      actions = '<a class="cu-dash-btn cu-dash-btn--ghost" href="/precios/">Ver planes</a>';
    }

    var cuota = state.cuota || (state.descargas && state.descargas.cuota);
    var usageCard = usageTotalCardHtml(tier, cuota);
    var usageNote = "";
    if (cuota) {
      var usado = Number(cuota.bytes_usados) || 0;
      var limite = Number(cuota.limite_bytes) || 0;
      var pct = limite > 0 ? Math.min(100, (usado / limite) * 100) : 0;
      if (pct >= 100) {
        var od = state.onDemand;
        if (od && !od._error && od.eligible && (od.on_demand_disponible || od.can_enable)) {
          if (od.enabled) {
            usageNote = '<p class="cu-dash-note cu-dash-note-err">Se acab\u00f3 tu l\u00edmite de descarga. El pago por uso est\u00e1 activo; lo extra se cobra seg\u00fan tu l\u00edmite mensual. <a href="#cu-on-demand">Ver ajustes</a></p>';
          } else {
            usageNote = '<p class="cu-dash-note cu-dash-note-err">Se acab\u00f3 tu l\u00edmite de descarga. <a href="#cu-on-demand">Activa el pago por uso</a> para seguir, <a href="/precios/">mejora tu plan</a> o espera a que se renueve.</p>';
          }
        } else {
          usageNote = '<p class="cu-dash-note cu-dash-note-err">Se acab\u00f3 tu l\u00edmite de descarga. <a href="/precios/">Mejora tu plan</a> o espera a que se renueve.</p>';
        }
      } else if (pct >= 80) {
        usageNote = '<p class="cu-dash-note cu-dash-note-warn">Quedan ' + esc(formatBytes(cuota.bytes_disponibles)) +
          " hasta que se renueve (" + esc(formatResetDate(cuota.reset_en)) + ").</p>";
      }
    }

    el.innerHTML =
      '<section class="cu-dash-panel">' +
        '<div class="cu-dash-row cu-dash-row--between">' +
          '<div>' +
            '<p class="cu-dash-kicker">Plan</p>' +
            '<div class="cu-dash-plan-name">' +
              '<span class="cu-dash-plan-title">' + esc(tierLabel(tier)) + "</span>" +
              pill +
            "</div>" +
            (caption ? '<p class="cu-dash-caption">' + caption + "</p>" : "") +
          "</div>" +
          (actions ? '<div class="cu-dash-actions">' + actions + "</div>" : "") +
        "</div>" +
        usageCard +
        usageNote +
      "</section>";
  }

  function renderOnDemand(root) {
    var el = $("cu-on-demand", root);
    if (!el) return;
    var od = state.onDemand;
    if (!od || od._error) {
      el.innerHTML = "";
      return;
    }
    if (!od.eligible) {
      el.innerHTML = "";
      return;
    }

    var enabled = !!od.enabled;
    var limitVal = od.limit_clp == null ? "unlimited" : String(od.limit_clp);
    var limitDisabled = !enabled ? " disabled" : "";
    var limitOpts = '<option value="off"' + (enabled ? "" : " selected") + '>Desactivado</option>';
    if (enabled) {
      limitOpts = "";
      [2000, 5000, 10000, 20000].forEach(function (v) {
        var sel = limitVal === String(v) ? " selected" : "";
        limitOpts += '<option value="' + v + '"' + sel + ">" + esc(formatClp(v)) + "</option>";
      });
      limitOpts += '<option value="unlimited"' + (limitVal === "unlimited" ? " selected" : "") + ">Sin l\u00edmite</option>";
    }

    var bytesExtra = Number(od.bytes_overage_mes) || 0;
    var clpUsado = Number(od.clp_usado) || 0;
    var usageLine = "Este mes: " + esc(formatBytes(bytesExtra)) + " extra \u00b7 " + esc(formatClp(clpUsado));
    if (od.limit_clp == null) {
      usageLine += " sin l\u00edmite";
    } else {
      usageLine += " de " + esc(formatClp(od.limit_clp));
    }

    var cardHtml = "";
    if (!od.has_card && od.can_enable) {
      cardHtml =
        '<p class="cu-dash-caption cu-dash-od-card-msg">Necesitas registrar una tarjeta para activar pago por uso.</p>' +
        '<div class="cu-dash-od-actions">' +
          '<button type="button" class="cu-dash-btn cu-dash-btn--primary cu-od-register-card">Registrar tarjeta</button>' +
        "</div>";
    }

    el.innerHTML =
      '<section class="cu-dash-panel" id="cu-on-demand-panel">' +
        '<p class="cu-dash-kicker">Pago por uso</p>' +
        '<h2 class="cu-dash-h2">Descargas extra</h2>' +
        '<p class="cu-dash-caption">M\u00e1s all\u00e1 del l\u00edmite de tu plan \u00b7 ' +
          esc(formatClp(od.clp_per_gb || 500)) + "/GB</p>" +
        '<div class="cu-dash-od-row">' +
          '<label class="cu-dash-od-label" for="cu-od-enabled">Pago por uso extra</label>' +
          '<select id="cu-od-enabled" class="cu-dash-od-select cu-od-enabled">' +
            '<option value="0"' + (enabled ? "" : " selected") + ">Desactivado</option>" +
            '<option value="1"' + (enabled ? " selected" : "") + ">Activado</option>" +
          "</select>" +
        "</div>" +
        '<div class="cu-dash-od-row">' +
          '<label class="cu-dash-od-label" for="cu-od-limit">L\u00edmite mensual</label>' +
          '<select id="cu-od-limit" class="cu-dash-od-select cu-od-limit"' + limitDisabled + ">" +
            limitOpts +
          "</select>" +
        "</div>" +
        '<div class="cu-dash-od-actions">' +
          '<button type="button" class="cu-dash-btn cu-dash-btn--primary cu-od-save">Guardar</button>' +
        "</div>" +
        '<p class="cu-dash-caption cu-dash-od-usage">' + usageLine + "</p>" +
        cardHtml +
      "</section>";
  }

  function cardBrandLabel(brand) {
    var b = String(brand || "").toLowerCase();
    if (b.indexOf("visa") >= 0) return "Visa";
    if (b.indexOf("master") >= 0) return "Mastercard";
    if (b.indexOf("amex") >= 0 || b.indexOf("american") >= 0) return "American Express";
    if (b.indexOf("diners") >= 0) return "Diners Club";
    if (b.indexOf("magna") >= 0 || b.indexOf("redcompra") >= 0) return "Redcompra";
    return brand ? String(brand) : "Tarjeta";
  }

  function renderPayment(root) {
    var el = $("cu-payment", root);
    if (!el) return;
    var p = state.payment;
    if (!p || p._error || !p.configured) {
      el.innerHTML = "";
      return;
    }

    var body;
    if (p.has_card) {
      var brand = cardBrandLabel(p.brand);
      var last4 = p.last4 ? String(p.last4) : "\u2022\u2022\u2022\u2022";
      body =
        '<div class="cu-dash-card-row">' +
          '<div class="cu-dash-card-visual">' +
            '<svg width="26" height="26" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" aria-hidden="true"><rect x="2" y="5" width="20" height="14" rx="2"/><path d="M2 10h20"/></svg>' +
          "</div>" +
          '<div class="cu-dash-card-info">' +
            '<div class="cu-dash-card-brand">' + esc(brand) + "</div>" +
            '<div class="cu-dash-card-num">\u2022\u2022\u2022\u2022 ' + esc(last4) + "</div>" +
          "</div>" +
        "</div>" +
        '<div class="cu-dash-od-actions">' +
          '<button type="button" class="cu-dash-btn cu-dash-btn--ghost cu-pm-register">Cambiar tarjeta</button>' +
          (p.can_delete
            ? '<button type="button" class="cu-dash-btn cu-dash-btn--danger cu-pm-remove">Eliminar</button>'
            : "") +
        "</div>";
      if (!p.can_delete) {
        var reason = p.subscription_active
          ? "Tienes una suscripci\u00f3n Pro activa. Canc\u00e9lala para poder eliminar la tarjeta."
          : (p.on_demand_enabled
            ? "Tienes pago por uso activo. Desact\u00edvalo para poder eliminar la tarjeta."
            : "");
        if (reason) body += '<p class="cu-dash-caption cu-dash-pm-note">' + esc(reason) + "</p>";
      }
    } else {
      body =
        '<p class="cu-dash-caption cu-dash-pm-empty">No tienes ninguna tarjeta guardada.</p>' +
        '<div class="cu-dash-od-actions">' +
          '<button type="button" class="cu-dash-btn cu-dash-btn--primary cu-pm-register">Agregar tarjeta</button>' +
        "</div>";
    }

    el.innerHTML =
      '<section class="cu-dash-panel" id="cu-payment-panel">' +
        '<p class="cu-dash-kicker">Medios de pago</p>' +
        '<h2 class="cu-dash-h2">Tu tarjeta</h2>' +
        '<p class="cu-dash-caption">Tarjeta usada para tu suscripci\u00f3n y el pago por uso. Al cambiarla se reemplaza la anterior.</p>' +
        body +
      "</section>";
  }

  function renderMbPacks(root) {
    var el = $("cu-mb-packs", root);
    if (!el) return;
    var packs = state.mbPacks;
    if (!packs || packs._error) {
      el.innerHTML = "";
      return;
    }
    var saldo = Number(packs.saldo_bytes) || 0;
    var rawPacks = packs.packs || [];
    var list = Array.isArray(rawPacks) ? rawPacks : Object.keys(rawPacks).map(function (k) { return rawPacks[k]; });
    var buttons = "";
    if (packs.checkout_enabled === true && list.length) {
      buttons = '<div class="cu-mb-packs">' + list.map(function (p) {
        return '<button type="button" class="cu-dash-btn cu-dash-btn--primary cu-mb-buy" data-pack="' + esc(p.id) + '" data-clp="' + esc(p.clp) + '">' +
          esc(p.label) + " \u00b7 " + esc(formatClp(p.clp)) +
          "</button>";
      }).join("") + "</div>";
    } else {
      buttons = '<p class="cu-dash-caption">El pago no est\u00e1 disponible en este momento.</p>';
    }
    el.innerHTML =
      '<section class="cu-dash-panel" id="cu-mb-packs-panel">' +
        '<p class="cu-dash-kicker">Compra \u00fanica</p>' +
        '<h2 class="cu-dash-h2">M\u00e1s MB, sin suscripci\u00f3n</h2>' +
        '<p class="cu-dash-caption">Se suman una sola vez y no se renuevan. Se usan cuando se acaba el l\u00edmite de tu plan y quedan hasta que los gastes. Por cada MB salen el doble que el plan Pro ($9.990 al mes, 1 GB al mes).</p>' +
        '<p class="cu-dash-caption cu-mb-saldo">Saldo disponible: <strong>' + esc(formatBytes(saldo)) + "</strong></p>" +
        buttons +
      "</section>";
  }

  function renderDescargas(root) {
    var el = $("cu-descargas", root);
    if (!el) return;
    var d = state.descargas;
    var cuota = (d && d.cuota) || state.cuota;
    if (d && d._error) {
      el.innerHTML = '<section class="cu-dash-panel"><p class="cu-dash-error">' + esc(d._error) + "</p></section>";
      return;
    }
    if (!cuota) {
      el.innerHTML = '<section class="cu-dash-panel"><p class="cu-dash-empty">Cargando\u2026</p></section>';
      return;
    }

    var recientes = (d && d.recientes) || [];
    var tableHtml = "";
    if (recientes.length === 0) {
      tableHtml = '<p class="cu-dash-empty">Sin descargas a\u00fan. <a href="/descargar/">Descargar datos</a></p>';
    } else {
      var rows = recientes.map(function (r) {
        var badgeCls = dashBadgeClass(r.estado);
        var nombre = r.archivo_nombre || r.id || "\u2014";
        var sub = r.formato ? esc(r.formato) : "";
        if (r.expirado) sub = (sub ? sub + " \u00b7 " : "") + "expirado";
        return "<tr>" +
          "<td>" + esc(formatDateTime(r.creado_en)) + "</td>" +
          "<td><div class=\"cu-dash-td-main\">" + esc(nombre) + "</div>" +
            (sub ? '<div class="cu-dash-td-sub">' + sub + "</div>" : "") + "</td>" +
          '<td><span class="cu-dash-badge ' + badgeCls + '">' + esc(r.estado_label || r.estado) + "</span></td>" +
          "<td>" + esc(r.bytes_label || formatBytes(r.bytes)) + "</td>" +
          "</tr>";
      }).join("");
      tableHtml =
        '<div class="cu-dash-table-wrap"><table class="cu-dash-table">' +
          "<thead><tr><th>Fecha</th><th>Archivo</th><th>Estado</th><th>Tama\u00f1o</th></tr></thead>" +
          "<tbody>" + rows + "</tbody></table></div>";
    }

    el.innerHTML =
      '<section class="cu-dash-panel">' +
        '<h2 class="cu-dash-h2">Descargas</h2>' +
        '<p class="cu-dash-caption">Historial diario y actividad reciente</p>' +
        '<div class="cu-dash-chart"><canvas id="cu-chart-descargas"></canvas></div>' +
        '<div class="cu-dash-divider"></div>' +
        '<h3 class="cu-dash-h3">Actividad reciente</h3>' +
        tableHtml +
      "</section>";

    renderDescargasChart(root);
  }

  function renderDescargasChart(root) {
    var canvas = $("cu-chart-descargas", root);
    if (!canvas) return;
    var serie = (state.descargas && state.descargas.serie_diaria) || [];
    loadChartLib().then(function () {
      if (typeof Chart === "undefined") return;
      var labels = serie.map(function (x) {
        try {
          return new Date(x.dia + "T12:00:00").toLocaleDateString("es-CL", { day: "numeric", month: "short" });
        } catch (e) {
          return x.dia;
        }
      });
      var values = serie.map(function (x) { return Math.round(Number(x.bytes) / 1024 / 1024 * 10) / 10; });
      if (chartDescargas) chartDescargas.destroy();
      chartDescargas = new Chart(canvas, {
        type: "bar",
        data: {
          labels: labels,
          datasets: [{
            label: "MB descargados",
            data: values,
            backgroundColor: "rgba(23, 23, 23, 0.85)",
            borderRadius: 3,
            maxBarThickness: 28
          }]
        },
        options: chartBaseOptions("MB")
      });
    }).catch(function () {});
  }

  function renderAlertas(root) {
    var el = $("cu-alertas", root);
    if (!el) return;
    var stats = state.alertasStats;
    var main = state.alertasMain;
    if (stats && stats._error) {
      el.innerHTML = '<section class="cu-dash-panel"><p class="cu-dash-error">' + esc(stats._error) + "</p></section>";
      return;
    }
    if (!stats) {
      el.innerHTML = '<section class="cu-dash-panel"><p class="cu-dash-empty">Cargando\u2026</p></section>';
      return;
    }

    var ultimo = stats.ultimo_digest;
    var ultimoTxt = ultimo && ultimo.fecha_eval
      ? formatDateTime(ultimo.enviado_en || ultimo.fecha_eval)
      : "\u2014";

    var reglas = (main && main.reglas) || [];
    var reglasActivas = reglas.filter(function (r) { return r.activa !== false; }).length;
    var eventos = state.eventos || [];
    var eventosHtml = "";
    if (eventos.length === 0) {
      eventosHtml = '<p class="cu-dash-empty">Sin alertas enviadas recientemente.</p>';
    } else {
      var rows = eventos.map(function (ev) {
        var dir = ev.direccion === "min" ? "muy bajo" : "muy alto";
        var mainTxt = esc(ev.estacion_nombre || "Estaci\u00f3n");
        var subTxt = esc(ev.variable_nombre || "") +
          (ev.variable_nombre ? " \u00b7 " : "") + dir + " \u00b7 #" + esc(ev.ranking);
        return "<tr>" +
          "<td>" + esc(formatDateTime(ev.enviado_en)) + "</td>" +
          "<td><div class=\"cu-dash-td-main\">" + mainTxt + "</div>" +
            '<div class="cu-dash-td-sub">' + subTxt + "</div></td>" +
          "</tr>";
      }).join("");
      eventosHtml =
        '<div class="cu-dash-table-wrap"><table class="cu-dash-table cu-dash-table--2col">' +
          "<thead><tr><th>Fecha</th><th>Alerta</th></tr></thead>" +
          "<tbody>" + rows + "</tbody></table></div>";
    }

    var rebote = (main && main.carta_devuelta) || {};
    var reboteHtml = "";
    if (rebote.bloqueado) {
      reboteHtml =
        '<div class="cu-dash-note cu-dash-note-warn" id="cu-rebote-banner" role="status">' +
          "<p>Los correos a tu direcci\u00f3n est\u00e1n rebotando (p.ej. buz\u00f3n lleno). Pausamos los avisos por correo; cuando lo soluciones pulsa Reactivar.</p>" +
          '<p style="margin-top:10px"><button type="button" class="cu-dash-btn cu-dash-btn--primary" id="cu-btn-rebote-reactivar">Reactivar</button></p>' +
        "</div>";
    }

    el.innerHTML =
      '<section class="cu-dash-panel">' +
        '<div class="cu-dash-usage-head">' +
          "<div>" +
            '<h2 class="cu-dash-h2">Alertas</h2>' +
            '<p class="cu-dash-caption">Los d\u00edas m\u00e1s extremos, por correo \u00b7 ' + esc(reglasActivas) + " alertas activas</p>" +
          "</div>" +
        "</div>" +
        reboteHtml +
        '<div class="cu-dash-metrics">' +
          '<div class="cu-dash-metric"><div class="cu-dash-metric-v">' + esc(stats.total || 0) + '</div><div class="cu-dash-metric-l">Total hist\u00f3rico</div></div>' +
          '<div class="cu-dash-metric"><div class="cu-dash-metric-v">' + esc(stats.este_mes || 0) + '</div><div class="cu-dash-metric-l">Este mes</div></div>' +
          '<div class="cu-dash-metric"><div class="cu-dash-metric-v cu-dash-metric-v--sm">' + esc(ultimoTxt) + '</div><div class="cu-dash-metric-l">\u00daltimo correo</div></div>' +
        "</div>" +
        '<div class="cu-dash-chart"><canvas id="cu-chart-alertas"></canvas></div>' +
        '<div class="cu-dash-divider"></div>' +
        '<h3 class="cu-dash-h3">Actividad reciente</h3>' +
        eventosHtml +
        '<div class="cu-dash-actions" style="margin-top:14px">' +
          '<a class="cu-dash-btn cu-dash-btn--primary" href="/alertas/">Abrir panel de alertas</a>' +
        "</div>" +
      "</section>";

    renderAlertasChart(root);
  }

  function renderAlertasChart(root) {
    var canvas = $("cu-chart-alertas", root);
    if (!canvas) return;
    var serie = (state.alertasStats && state.alertasStats.por_mes) || [];
    loadChartLib().then(function () {
      if (typeof Chart === "undefined") return;
      var labels = serie.map(function (x) { return x.mes; });
      var values = serie.map(function (x) { return x.n; });
      if (chartAlertas) chartAlertas.destroy();
      chartAlertas = new Chart(canvas, {
        type: "bar",
        data: {
          labels: labels,
          datasets: [{
            label: "Alertas",
            data: values,
            backgroundColor: "rgba(23, 23, 23, 0.75)",
            borderRadius: 3,
            maxBarThickness: 28
          }]
        },
        options: chartBaseOptions("")
      });
    }).catch(function () {});
  }

  function renderLinks(root) {
    var el = $("cu-links", root);
    if (!el) return;
    el.innerHTML =
      '<nav class="cu-dash-footer">' +
        '<a href="/precios/">Precios</a><span class="cu-dash-dot">\u00b7</span>' +
        '<a href="/explorador/">Explorador</a>' +
      "</nav>";
  }

  function renderAll(root) {
    renderPlan(root);
    renderOnDemand(root);
    renderPayment(root);
    renderDescargas(root);
    renderAlertas(root);
    renderLinks(root);
  }

  function loadBillingData() {
    return Promise.all([
      apiFetchSafe("/me"),
      apiFetchSafe("/me/cuota"),
      apiFetchSafe("/me/billing"),
      apiFetchSafe("/me/on-demand"),
      apiFetchSafe("/me/payment-method")
    ]).then(function (results) {
      state.me = results[0];
      state.cuota = results[1] && !results[1]._error ? results[1] : null;
      state.billing = results[2];
      state.onDemand = results[3] && !results[3]._error ? results[3] : null;
      if (state.onDemand) {
        state.onDemand.on_demand_disponible = onDemandDisponible(state.onDemand);
      }
      state.payment = results[4] && !results[4]._error ? results[4] : null;
    });
  }

  function startMbPackCheckout(btn) {
    if (!btn || btn.disabled) return;
    var packId = btn.getAttribute("data-pack");
    var clp = Number(btn.getAttribute("data-clp")) || 0;
    var original = btn.textContent;
    btn.disabled = true;
    btn.textContent = "Redirigiendo al pago\u2026";
    if (typeof rodajaTrack === "function") {
      rodajaTrack("begin_checkout", { plan: "mb_pack", pack: packId, currency: "CLP", value: clp });
    }
    rodajaApiFetch("/billing/mb-pack", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ pack_id: packId })
    }).then(function (r) {
      return r.text().then(function (text) {
        var body = null;
        if (text) {
          try { body = JSON.parse(text); } catch (e) { /* ignore */ }
        }
        if (!r.ok) throw new Error((body && body.error) || ("Error " + r.status));
        return body;
      });
    }).then(function (data) {
      var url = scalar(data && data.url);
      if (url) {
        openCheckoutUrl(url, btn);
        return;
      }
      throw new Error("No se recibi\u00f3 URL de pago");
    }).catch(function (err) {
      btn.disabled = false;
      btn.textContent = original;
      showBanner(err.message || "No se pudo iniciar la compra.", "warn");
    });
  }

  function wireMbPackEvents(root) {
    if (!root) return;
    var buttons = root.querySelectorAll(".cu-mb-buy");
    for (var i = 0; i < buttons.length; i++) {
      buttons[i].addEventListener("click", function () { startMbPackCheckout(this); });
    }
  }

  function loadUsageData() {
    return Promise.all([
      apiFetchSafe("/me/descargas?limit=15&dias=30"),
      apiFetchSafe("/me/mb-packs"),
      apiFetchSafe("/me/alertas/estadisticas"),
      apiFetchSafe("/me/alertas"),
      apiFetchSafe("/me/alertas/eventos?limit=5")
    ]).then(function (results) {
      state.descargas = results[0];
      state.mbPacks = results[1];
      if (state.descargas && !state.descargas._error && state.descargas.cuota) {
        state.cuota = state.descargas.cuota;
      }
      state.alertasStats = results[2];
      state.alertasMain = results[3] && !results[3]._error ? results[3] : null;
      var evPayload = results[4];
      state.eventos = (evPayload && evPayload.eventos) || [];
    });
  }

  function startCheckout(btn) {
    if (!btn || btn.disabled) return;
    var original = btn.textContent;
    btn.setAttribute("data-label", original);
    btn.disabled = true;
    btn.textContent = "Redirigiendo al pago\u2026";
    if (typeof rodajaTrack === "function") {
      rodajaTrack("begin_checkout", { plan: "pro", currency: "CLP", value: 9990 });
    }
    rodajaApiFetch("/billing/checkout", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ plan: "pro" })
    }).then(function (r) {
      return r.text().then(function (text) {
        var body = null;
        if (text) {
          try { body = JSON.parse(text); } catch (e) { /* ignore */ }
        }
        if (!r.ok) {
          throw new Error((body && body.error) || ("Error " + r.status));
        }
        return body;
      });
    }).then(function (data) {
      var url = scalar(data && data.url);
      if (url) {
        openCheckoutUrl(url, btn);
        return;
      }
      throw new Error("No se recibi\u00f3 URL de pago");
    }).catch(function (err) {
      btn.disabled = false;
      btn.textContent = original;
      showBanner(err.message || "No se pudo iniciar el pago.", "warn");
    });
  }

  function cancelSubscription(btn) {
    if (!confirm("\u00bfCancelar tu suscripci\u00f3n Pro? Seguir\u00e1s con plan Gratuito al terminar el periodo actual.")) {
      return;
    }
    if (btn) {
      btn.disabled = true;
      btn.textContent = "Cancelando\u2026";
    }
    rodajaApiFetch("/billing/cancel", { method: "POST" })
      .then(function (r) {
        return r.text().then(function (text) {
          var body = null;
          if (text) {
            try { body = JSON.parse(text); } catch (e) { /* ignore */ }
          }
          if (!r.ok) throw new Error((body && body.error) || ("Error " + r.status));
          return body;
        });
      })
      .then(function () {
        showBanner("Suscripci\u00f3n cancelada. Tu plan pas\u00f3 a Gratuito.", "ok");
        return loadBillingData().then(function () {
          if (dashboardRoot) { renderAll(dashboardRoot); wireDashboardEvents(dashboardRoot); }
        });
      })
      .catch(function (err) {
        showBanner(err.message || "No se pudo cancelar.", "err");
        if (btn) {
          btn.disabled = false;
          btn.textContent = "Cancelar suscripci\u00f3n";
        }
      });
  }

  function syncOnDemandLimitSelect(root, enabled) {
    var limitSel = root.querySelector(".cu-od-limit");
    if (!limitSel) return;
    var od = state.onDemand || {};
    var limitVal = od.limit_clp == null ? "unlimited" : String(od.limit_clp);
    if (!enabled) {
      limitSel.disabled = true;
      limitSel.innerHTML = '<option value="off" selected>Desactivado</option>';
      return;
    }
    limitSel.disabled = false;
    var html = "";
    [2000, 5000, 10000, 20000].forEach(function (v) {
      var sel = limitVal === String(v) ? " selected" : "";
      html += '<option value="' + v + '"' + sel + ">" + formatClp(v) + "</option>";
    });
    html += '<option value="unlimited"' + (limitVal === "unlimited" ? " selected" : "") + ">Sin l\u00edmite</option>";
    limitSel.innerHTML = html;
  }

  function saveOnDemandSettings(root, btn) {
    var enabledSel = root.querySelector(".cu-od-enabled");
    var limitSel = root.querySelector(".cu-od-limit");
    if (!enabledSel) return;
    var enabled = enabledSel.value === "1";
    var limitClp = null;
    if (enabled && limitSel) {
      var lv = limitSel.value;
      if (lv !== "unlimited" && lv !== "off") limitClp = parseInt(lv, 10);
    }
    var original = btn ? btn.textContent : "";
    if (btn) {
      btn.disabled = true;
      btn.textContent = "Guardando\u2026";
    }
    rodajaApiFetch("/me/on-demand", {
      method: "PATCH",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ enabled: enabled, limit_clp: limitClp })
    }).then(function (r) {
      return r.text().then(function (text) {
        var body = null;
        if (text) {
          try { body = JSON.parse(text); } catch (e) { /* ignore */ }
        }
        if (!r.ok) {
          if (r.status === 409 && body && body.register_url) {
            openCheckoutUrl(body.register_url, btn);
            return null;
          }
          throw new Error((body && body.error) || ("Error " + r.status));
        }
        return body;
      });
    }).then(function (data) {
      if (!data) return;
      state.onDemand = data;
      state.onDemand.on_demand_disponible = onDemandDisponible(state.onDemand);
      showBanner("Configuraci\u00f3n de pago por uso guardada.", "ok");
      renderAll(root);
      wireDashboardEvents(root);
    }).catch(function (err) {
      showBanner(err.message || "No se pudo guardar.", "err");
    }).finally(function () {
      if (btn) {
        btn.disabled = false;
        btn.textContent = original || "Guardar";
      }
    });
  }

  function registerPaymentCard(btn) {
    if (!btn || btn.disabled) return;
    var original = btn.textContent;
    btn.setAttribute("data-label", original);
    btn.disabled = true;
    btn.textContent = "Redirigiendo\u2026";
    rodajaApiFetch("/me/payment-method/register", { method: "POST" })
      .then(function (r) {
        return r.text().then(function (text) {
          var body = null;
          if (text) { try { body = JSON.parse(text); } catch (e) { /* ignore */ } }
          if (!r.ok) throw new Error((body && body.error) || ("Error " + r.status));
          return body;
        });
      })
      .then(function (data) {
        var url = scalar(data && data.url);
        if (url) { openCheckoutUrl(url, btn); return; }
        throw new Error("No se recibi\u00f3 URL de registro");
      })
      .catch(function (err) {
        btn.disabled = false;
        btn.textContent = original;
        showBanner(err.message || "No se pudo iniciar el registro de tarjeta.", "warn");
      });
  }

  function removePaymentCard(root, btn) {
    if (!confirm("\u00bfEliminar tu tarjeta guardada? No podr\u00e1s hacer pagos hasta registrar otra.")) {
      return;
    }
    var original = btn ? btn.textContent : "";
    if (btn) { btn.disabled = true; btn.textContent = "Eliminando\u2026"; }
    rodajaApiFetch("/me/payment-method/remove", { method: "POST" })
      .then(function (r) {
        return r.text().then(function (text) {
          var body = null;
          if (text) { try { body = JSON.parse(text); } catch (e) { /* ignore */ } }
          if (!r.ok) throw new Error((body && body.error) || ("Error " + r.status));
          return body;
        });
      })
      .then(function () {
        showBanner("Tarjeta eliminada.", "ok");
        return loadBillingData().then(function () {
          if (dashboardRoot) { renderAll(dashboardRoot); wireDashboardEvents(dashboardRoot); }
        });
      })
      .catch(function (err) {
        showBanner(err.message || "No se pudo eliminar la tarjeta.", "err");
        if (btn) { btn.disabled = false; btn.textContent = original || "Eliminar"; }
      });
  }

  function reactivarCorreoRebote(root, btn) {
    if (btn) { btn.disabled = true; btn.textContent = "Reactivando\u2026"; }
    return rodajaApiFetch("/me/alertas/correo-rebote/reactivar", { method: "POST" })
      .then(function (r) {
        return r.text().then(function (text) {
          var body = null;
          if (text) {
            try { body = JSON.parse(text); } catch (e) { /* ignore */ }
          }
          if (!r.ok) throw new Error((body && body.error) || ("Error " + r.status));
          return body;
        });
      })
      .then(function (data) {
        if (state.alertasMain) {
          state.alertasMain.carta_devuelta = (data && data.carta_devuelta) || { bloqueado: false };
        }
        showBanner("Listo. Volveremos a enviarte los avisos por correo.", "ok");
        renderAlertas(root);
        wireReboteEvents(root);
      })
      .catch(function (err) {
        if (btn) { btn.disabled = false; btn.textContent = "Reactivar"; }
        showBanner((err && err.message) || "No se pudo reactivar el correo.", "err");
      });
  }

  function wireReboteEvents(root) {
    if (!root) return;
    var btn = root.querySelector("#cu-btn-rebote-reactivar");
    if (btn) {
      btn.addEventListener("click", function () { reactivarCorreoRebote(root, btn); });
    }
  }

  function wireDashboardEvents(root) {
    if (!root) return;
    var upgrade = root.querySelector(".cu-btn-upgrade");
    if (upgrade) {
      upgrade.addEventListener("click", function () { startCheckout(upgrade); });
    }
    var cancel = root.querySelector(".cu-btn-cancel");
    if (cancel) {
      cancel.addEventListener("click", function () { cancelSubscription(cancel); });
    }
    var odEnabled = root.querySelector(".cu-od-enabled");
    if (odEnabled) {
      odEnabled.addEventListener("change", function () {
        syncOnDemandLimitSelect(root, odEnabled.value === "1");
      });
    }
    var odSave = root.querySelector(".cu-od-save");
    if (odSave) {
      odSave.addEventListener("click", function () { saveOnDemandSettings(root, odSave); });
    }
    var odRegister = root.querySelector(".cu-od-register-card");
    if (odRegister) {
      odRegister.addEventListener("click", function () {
        var enabledSel = root.querySelector(".cu-od-enabled");
        if (enabledSel) enabledSel.value = "1";
        syncOnDemandLimitSelect(root, true);
        saveOnDemandSettings(root, odRegister);
      });
    }
    var pmRegister = root.querySelector(".cu-pm-register");
    if (pmRegister) {
      pmRegister.addEventListener("click", function () { registerPaymentCard(pmRegister); });
    }
    var pmRemove = root.querySelector(".cu-pm-remove");
    if (pmRemove) {
      pmRemove.addEventListener("click", function () { removePaymentCard(root, pmRemove); });
    }
  }

  function revealDashContent() {
    if (!dashboardRoot) return;
    var loading = dashboardRoot.querySelector(".cu-dash-loading");
    var content = dashboardRoot.querySelector(".cu-dash-content");
    if (loading) loading.hidden = true;
    if (content) content.hidden = false;
  }

  function mountBillingPage(el) {
    el.innerHTML =
      '<div class="cu-dashboard-root">' +
        '<header class="cu-dash-header">' +
          '<h1 class="cu-dash-title">Facturaci\u00f3n</h1>' +
          '<p class="cu-dash-lead">Plan, pago por uso y medios de pago.</p>' +
        "</header>" +
        '<p class="cu-dash-loading">Cargando\u2026</p>' +
        '<div class="cu-dash-content cu-dash-stack" hidden>' +
          '<div id="cu-plan"></div>' +
          '<div id="cu-on-demand"></div>' +
          '<div id="cu-payment"></div>' +
        "</div>" +
      "</div>";
    dashboardRoot = el.querySelector(".cu-dashboard-root");
    loadBillingData().then(function () {
      revealDashContent();
      renderPlan(dashboardRoot);
      renderOnDemand(dashboardRoot);
      renderPayment(dashboardRoot);
      wireDashboardEvents(dashboardRoot);
    }).catch(function () {
      if (dashboardRoot) {
        dashboardRoot.innerHTML = '<p class="cu-card-error">No se pudo cargar facturaci\u00f3n.</p>';
      }
    });
  }

  function mountUsagePage(el) {
    el.innerHTML =
      '<div class="cu-dashboard-root">' +
        '<header class="cu-dash-header">' +
          '<h1 class="cu-dash-title">Uso</h1>' +
          '<p class="cu-dash-lead">Descargas y alertas de tu cuenta.</p>' +
        "</header>" +
        '<p class="cu-dash-loading">Cargando uso\u2026</p>' +
        '<div class="cu-dash-content cu-dash-stack" hidden>' +
          '<div id="cu-mb-packs"></div>' +
          '<div id="cu-descargas"></div>' +
          '<div id="cu-alertas"></div>' +
          '<div id="cu-links"></div>' +
        "</div>" +
      "</div>";
    dashboardRoot = el.querySelector(".cu-dashboard-root");
    loadUsageData().then(function () {
      revealDashContent();
      renderMbPacks(dashboardRoot);
      wireMbPackEvents(dashboardRoot);
      renderDescargas(dashboardRoot);
      renderAlertas(dashboardRoot);
      wireReboteEvents(dashboardRoot);
      renderLinks(dashboardRoot);
    }).catch(function () {
      if (dashboardRoot) {
        dashboardRoot.innerHTML = '<p class="cu-card-error">No se pudo cargar el panel de uso.</p>';
      }
    });
  }

  function unmountDashboardPage(el) {
    destroyCharts();
    dashboardRoot = null;
    el.innerHTML = "";
  }

  function iconChart(el) {
    el.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true"><path d="M3 3v18h18"/><path d="M7 16l4-8 4 5 4-9"/></svg>';
  }

  function iconCard(el) {
    el.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true"><rect x="2" y="5" width="20" height="14" rx="2"/><path d="M2 10h20"/></svg>';
  }

  function iconClear(el) {
    el.innerHTML = "";
  }

  function mountClerkProfile() {
    var host = document.getElementById("cu-clerk-profile");
    if (!host || profileMounted) return;
    var Clerk = window.Clerk;
    if (!Clerk || typeof Clerk.mountUserProfile !== "function") {
      host.hidden = false;
      host.innerHTML = '<p class="cu-card-error">No se pudo cargar el perfil Clerk.</p>';
      return;
    }
    if (!Clerk.user) {
      host.hidden = false;
      host.innerHTML = '<p class="cu-card-error">Sesi\u00f3n no disponible en Clerk. <a href="#" data-rodaja-signin>Inicia sesi\u00f3n</a></p>';
      return;
    }
    profileMounted = true;
    host.hidden = false;
    try {
      Clerk.mountUserProfile(host, {
        customPages: [
          {
            url: "facturacion",
            label: "Facturaci\u00f3n",
            mountIcon: iconCard,
            unmountIcon: iconClear,
            mount: mountBillingPage,
            unmount: unmountDashboardPage
          },
          {
            url: "uso",
            label: "Uso",
            mountIcon: iconChart,
            unmountIcon: iconClear,
            mount: mountUsagePage,
            unmount: unmountDashboardPage
          },
          { label: "account" },
          { label: "security" }
        ]
      });
    } catch (err) {
      host.innerHTML = '<p class="cu-card-error">Error al montar perfil: ' + esc(err.message || err) + "</p>";
    }
  }

  function hideLoading() {
    var loading = document.getElementById("cu-loading");
    if (loading) loading.hidden = true;
  }

  function showGuest() {
    hideLoading();
    var guest = document.getElementById("cu-guest");
    var host = document.getElementById("cu-clerk-profile");
    if (guest) guest.hidden = false;
    if (host) host.hidden = true;
    var link = document.getElementById("cu-guest-signin");
    if (link) {
      link.href = "#";
      link.setAttribute("data-rodaja-signin", "");
    }
  }

  function showProfile() {
    hideLoading();
    var guest = document.getElementById("cu-guest");
    if (guest) guest.hidden = true;
    mountClerkProfile();
  }

  function handleQueryBanner() {
    var params = new URLSearchParams(location.search);
    if (params.get("checkout") === "cancel") {
      showBanner("Pago cancelado. Puedes intentar de nuevo cuando quieras.", "warn");
    }
    var mb = params.get("mb");
    if (mb === "ok") {
      showBanner("Compra lista. Los MB quedaron en tu saldo y no se renuevan.", "ok");
    } else if (mb === "error") {
      showBanner("No se complet\u00f3 la compra de MB. Puedes intentarlo de nuevo.", "warn");
    }
    var card = params.get("card");
    if (card === "updated") {
      showBanner("Tarjeta registrada correctamente.", "ok");
    } else if (card === "error") {
      showBanner("No se pudo registrar la tarjeta. Int\u00e9ntalo de nuevo.", "warn");
    }
  }

  function boot() {
    handleQueryBanner();
    ensureAuthReady().then(function () {
      if (isSignedIn()) showProfile();
      else showGuest();
    }).catch(function () {
      showGuest();
    });
    if (typeof rodajaOnAuthChange === "function") {
      rodajaOnAuthChange(function () {
        if (isSignedIn()) showProfile();
        else {
          profileMounted = false;
          showGuest();
        }
      });
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    boot();
  }
})();
