/* Alertas personalizadas: umbral absoluto, aparte del top 10. */
(function (global) {
  "use strict";

  var ctx = null;
  var open = false;
  var editId = null;
  var draft = null;
  var ref = null;
  var refToken = 0;
  var saving = false;

  function esc(s) {
    return ctx.esc(s);
  }

  function $(id) {
    return document.getElementById(id);
  }

  function freshDraft() {
    return {
      nombre: "",
      familia: "pp",
      serie: "t_min",
      operador: "gt",
      valor: "30",
      valorHasta: "40",
      ventana: 24,
      scope: "favorita",
      codReg: "",
      nomCom: "",
      canal: "correo_push",
      frecuencia: "cada_vez"
    };
  }

  function asRules(val) {
    if (!val) return [];
    if (Array.isArray(val)) return val.filter(function (r) { return r && typeof r === "object" && (r.variable_abr || r.id); });
    if (val.variable_abr || val.id) return [val];
    var keys = Object.keys(val);
    if (!keys.length) return [];
    if (Array.isArray(val[keys[0]])) {
      var n = val[keys[0]].length;
      var out = [];
      for (var i = 0; i < n; i++) {
        var row = {};
        keys.forEach(function (k) { row[k] = val[k][i]; });
        out.push(row);
      }
      return out;
    }
    if (val[keys[0]] && typeof val[keys[0]] === "object" && val[keys[0]].variable_abr) {
      return keys.map(function (k) { return val[k]; });
    }
    return [];
  }

  function scopeValue(r) {
    var sv = r && r.scope_value;
    if (!sv) return {};
    if (typeof sv === "string") {
      try { return JSON.parse(sv) || {}; } catch (e) { return {}; }
    }
    return sv;
  }

  function kindOf(abr) {
    if (abr === "t_max") return "heat";
    if (abr === "t_min") return "cold";
    if (abr === "u_rafaga") return "wind";
    return "rain";
  }

  function icon(kind) {
    if (kind === "heat") {
      return '<svg class="i" viewBox="0 0 24 24"><circle cx="12" cy="12" r="4"/><path d="M12 2v2M12 20v2M4.9 4.9l1.4 1.4M17.7 17.7l1.4 1.4M2 12h2M20 12h2M4.9 19.1l1.4-1.4M17.7 6.3l1.4-1.4"/></svg>';
    }
    if (kind === "cold") {
      return '<svg class="i" viewBox="0 0 24 24"><path d="M12 2v20M5 6l14 12M19 6L5 18M2 12h20"/></svg>';
    }
    if (kind === "wind") {
      return '<svg class="i" viewBox="0 0 24 24"><path d="M3 8h11a3 3 0 1 0-3-3M3 16h15a3 3 0 1 1-3 3M3 12h8"/></svg>';
    }
    return '<svg class="i" viewBox="0 0 24 24"><path d="M7 15a5 5 0 0 1 1-9.9A6 6 0 0 1 19 8a4 4 0 0 1-1 8H7z"/><path d="M8 19l-1 2M12 19l-1 2M16 19l-1 2"/></svg>';
  }

  function contexto() {
    return (ctx.state && ctx.state.umbralContexto) || {};
  }

  function regionName(cod) {
    var want = parseInt(cod, 10);
    var nom = "";
    ((ctx.state && ctx.state.regions) || []).some(function (rg) {
      var c = parseInt(rg.cod_reg != null ? rg.cod_reg : rg.id, 10);
      if (c === want) {
        nom = rg.nom_reg || rg.nombre || "";
        return true;
      }
      return false;
    });
    return nom;
  }

  function scopeLabel(r) {
    var n = parseInt(r.n_estaciones, 10) || 0;
    var sv = scopeValue(r);
    if (r.scope_type === "favorita") return "Estaci\u00f3n favorita";
    if (r.scope_type === "grupo") {
      return "Mi grupo \u00b7 " + n + " estaci\u00f3n" + (n === 1 ? "" : "es");
    }
    if (r.scope_type === "comuna") return "Comuna \u00b7 " + (sv.nom_com || "");
    if (r.scope_type === "region") {
      var nom = regionName(sv.cod_reg);
      return nom ? ("Regi\u00f3n \u00b7 " + nom) : "Regi\u00f3n";
    }
    return r.scope_type || "";
  }

  function rulesHtml() {
    var rows = asRules(ctx.state.umbrales);
    if (!rows.length) {
      return '<p class="alu-empty">Todav\u00eda no tienes reglas. La primera puede ser una helada, una lluvia o una r\u00e1faga.</p>';
    }
    return rows.map(function (r) {
      var on = r.activa === true || r.activa === 1 || r.activa === "true" || r.activa === "TRUE";
      var kind = kindOf(String(r.variable_abr || ""));
      return '<div class="alu-rule' + (on ? "" : " off") + '" data-id="' + esc(r.id) + '">' +
        '<span class="alu-ic ' + kind + '">' + icon(kind) + "</span>" +
        '<div class="alu-info">' +
          "<b>" + esc(r.nombre || "Regla") + "</b>" +
          '<div class="alu-cond">' + esc(r.frase || "") + "</div>" +
          '<span class="alu-scope">' + esc(scopeLabel(r)) + "</span>" +
        "</div>" +
        '<label class="al-switch">' +
          '<input type="checkbox" class="alu-toggle"' + (on ? " checked" : "") + " />" +
          '<span class="al-slider"></span>' +
        "</label>" +
        '<button type="button" class="alu-edit" aria-label="Editar regla">' +
          '<svg class="i" viewBox="0 0 24 24"><path d="M12 20h9"/><path d="M16.5 3.5a2.1 2.1 0 0 1 3 3L7 19l-4 1 1-4z"/></svg>' +
        "</button>" +
      "</div>";
    }).join("");
  }

  function abrOf(d) {
    if (d.familia === "temp") return d.serie === "t_max" ? "t_max" : "t_min";
    if (d.familia === "wind") return "u_rafaga";
    return "pp";
  }

  function unidadOf(d) {
    if (d.familia === "wind") return "km/h";
    if (d.familia === "temp") return "\u00b0C";
    return "mm";
  }

  function fmtNum(v) {
    if (!isFinite(v)) return "";
    var r = Math.round(v * 10) / 10;
    if (Math.abs(r - Math.round(r)) < 0.05) return String(Math.round(r));
    return String(r).replace(".", ",");
  }

  function scaleOf() {
    var nums = [0];
    var a = parseFloat(draft.valor);
    var b = parseFloat(draft.valorHasta);
    if (isFinite(a)) nums.push(a);
    if (draft.operador === "between" && isFinite(b)) nums.push(b);
    if (ref && isFinite(Number(ref.maximo))) nums.push(Number(ref.maximo));
    if (ref && isFinite(Number(ref.minimo))) nums.push(Number(ref.minimo));
    var lo = draft.familia === "temp" ? Math.min.apply(null, nums) : 0;
    var hi = Math.max.apply(null, nums.concat([1]));
    if (hi <= lo) hi = lo + 1;
    hi = hi + (hi - lo) * 0.08;
    return { lo: lo, hi: hi };
  }

  function pct(v, sc) {
    return Math.max(0, Math.min(100, ((v - sc.lo) / (sc.hi - sc.lo)) * 100));
  }

  function caption() {
    var fav = (contexto().favorita) || {};
    if (!ref) return "Buscando el hist\u00f3rico de tu estaci\u00f3n favorita\u2026";
    if (ref.sin_favorita || !fav.configurada) {
      return "Fija una estaci\u00f3n favorita para ver el hist\u00f3rico de esa estaci\u00f3n.";
    }
    var nombre = ref.estacion || fav.nombre || "tu estaci\u00f3n favorita";
    var extremo = draft.operador === "lt" ? ref.minimo : ref.maximo;
    var palabra = draft.operador === "lt" ? "m\u00ednimo hist\u00f3rico" : "m\u00e1ximo hist\u00f3rico";
    if (extremo == null || !isFinite(Number(extremo))) {
      return "Tu estaci\u00f3n favorita (" + nombre + ") todav\u00eda no tiene suficiente historia para esta variable.";
    }
    var extra = "";
    if (draft.familia === "pp") {
      var w = String(draft.ventana) === "168" ? "7 d\u00edas" : (draft.ventana + " h");
      extra = " en " + w;
    }
    return "Como referencia: el " + palabra + " en " + nombre + " es " + fmtNum(Number(extremo)) + " " + unidadOf(draft) + extra + ".";
  }

  function barHtml() {
    var sc = scaleOf();
    var a = parseFloat(draft.valor);
    var b = parseFloat(draft.valorHasta);
    var mark = isFinite(a) ? pct(a, sc) : 0;
    var band = "left:0;width:0";
    if (draft.operador === "lt") {
      band = "left:0;width:" + mark + "%";
    } else if (draft.operador === "between" && isFinite(b)) {
      var p1 = pct(Math.min(a, b), sc);
      var p2 = pct(Math.max(a, b), sc);
      band = "left:" + p1 + "%;width:" + Math.max(0, p2 - p1) + "%";
      mark = p2;
    } else {
      band = "left:" + mark + "%;width:" + (100 - mark) + "%";
    }
    var ticks = "";
    for (var i = 0; i < 5; i++) {
      ticks += "<span>" + esc(fmtNum(sc.lo + ((sc.hi - sc.lo) * i / 4))) + "</span>";
    }
    return '<div class="alu-scale"><div class="alu-band" style="' + band + '"></div>' +
      '<div class="alu-mark" style="left:' + mark + '%"></div></div>' +
      '<div class="alu-ticks">' + ticks + "</div>" +
      '<p class="alu-cap">' + esc(caption()) + "</p>";
  }

  function comunasDe(cod) {
    var want = parseInt(cod, 10);
    var set = {};
    ((ctx.state && ctx.state.stationsCatalog) || []).forEach(function (s) {
      if (parseInt(s.cod_reg, 10) !== want) return;
      var nom = s.nom_com ? String(s.nom_com).trim() : "";
      if (nom) set[nom] = true;
    });
    return Object.keys(set).sort(function (a, b) { return a.localeCompare(b, "es"); });
  }

  function builderHtml() {
    if (!draft) draft = freshDraft();
    var pro = ctx.isPro();
    var fav = (contexto().favorita) || {};
    var grp = (contexto().grupo) || {};
    var favHint = fav.configurada ? "1 estaci\u00f3n" : "Sin estaci\u00f3n favorita";
    var grpN = parseInt(grp.n, 10) || 0;
    var grpHint = grp.configurado ? (grpN + " estaci\u00f3n" + (grpN === 1 ? "" : "es")) : "Sin estaciones en el grupo";
    var regs = ((ctx.state && ctx.state.regions) || []).slice().sort(function (a, b) {
      return String(a.nom_reg || a.nombre || "").localeCompare(String(b.nom_reg || b.nombre || ""), "es");
    });
    var regOpts = '<option value="">Elige una regi\u00f3n</option>' + regs.map(function (rg) {
      var cod = rg.cod_reg != null ? rg.cod_reg : rg.id;
      var sel = String(cod) === String(draft.codReg) ? " selected" : "";
      return '<option value="' + esc(cod) + '"' + sel + ">" + esc(rg.nom_reg || rg.nombre || cod) + "</option>";
    }).join("");
    var comOpts = '<option value="">Toda la regi\u00f3n</option>';
    if (draft.codReg) {
      comunasDe(draft.codReg).forEach(function (nom) {
        var sel = nom === draft.nomCom ? " selected" : "";
        comOpts += '<option value="' + esc(nom) + '"' + sel + ">" + esc(nom) + "</option>";
      });
    }
    var u = unidadOf(draft);
    var vp = function (fam, kind, label) {
      return '<button type="button" class="alu-vp ' + kind + (draft.familia === fam ? " on" : "") + '" data-fam="' + fam + '">' +
        '<span class="alu-vp-ic">' + icon(kind) + "</span>" + label + "</button>";
    };
    var seg = function (op, label) {
      return '<button type="button" class="alu-seg' + (draft.operador === op ? " on" : "") + '" data-op="' + op + '">' + label + "</button>";
    };
    var scopeBtn = function (id, title, hint) {
      return '<button type="button" class="alu-scope-row' + (draft.scope === id ? " on" : "") + '" data-scope="' + id + '">' +
        "<i></i><span><b>" + title + "</b><small>" + esc(hint) + "</small></span></button>";
    };
    var vals = '<div class="alu-vals">' +
      '<div class="alu-num"><input id="alu-valor" inputmode="decimal" value="' + esc(draft.valor) + '"><span>' + esc(u) + "</span></div>";
    if (draft.operador === "between") {
      vals += '<span class="alu-and">y</span>' +
        '<div class="alu-num"><input id="alu-valor2" inputmode="decimal" value="' + esc(draft.valorHasta) + '"><span>' + esc(u) + "</span></div>";
    }
    if (draft.familia === "pp") {
      vals += '<span class="alu-and">acumulados en</span>' +
        '<select id="alu-ventana">' +
          '<option value="24"' + (String(draft.ventana) === "24" ? " selected" : "") + ">24 h</option>" +
          '<option value="48"' + (String(draft.ventana) === "48" ? " selected" : "") + ">48 h</option>" +
          '<option value="168"' + (String(draft.ventana) === "168" ? " selected" : "") + ">7 d\u00edas</option>" +
        "</select>";
    }
    vals += "</div>";
    var serie = "";
    if (draft.familia === "temp") {
      serie = '<div class="alu-segs alu-serie">' +
        '<button type="button" class="alu-seg' + (draft.serie !== "t_max" ? " on" : "") + '" data-serie="t_min">M\u00ednima</button>' +
        '<button type="button" class="alu-seg' + (draft.serie === "t_max" ? " on" : "") + '" data-serie="t_max">M\u00e1xima</button>' +
      "</div>";
    }
    var avanzada = "";
    if (pro) {
      avanzada = scopeBtn("avanzada", "Selecci\u00f3n avanzada", "Por regi\u00f3n y comuna");
      if (draft.scope === "avanzada") {
        avanzada += '<div class="alu-adv">' +
          "<select id=\"alu-reg\">" + regOpts + "</select>" +
          "<select id=\"alu-com\">" + comOpts + "</select>" +
        "</div>";
      }
    }
    return '<div class="alu-build">' +
      '<div class="alu-fld"><label for="alu-nombre">Nombre de la regla</label>' +
        '<input id="alu-nombre" type="text" maxlength="80" placeholder="Ej: Helada tard\u00eda, Viento para fumigar" value="' + esc(draft.nombre) + '">' +
      "</div>" +
      '<div class="alu-fld"><label>Variable</label><div class="alu-vps">' +
        vp("pp", "rain", "Precipitaci\u00f3n") +
        vp("temp", "cold", "Temperatura") +
        vp("wind", "wind", "R\u00e1faga") +
      "</div>" + serie + "</div>" +
      '<div class="alu-fld"><label>Condici\u00f3n</label><div class="alu-segs">' +
        seg("gt", "Mayor que") + seg("lt", "Menor que") + seg("between", "Entre") +
      "</div>" + vals +
      '<div class="alu-range" id="alu-range">' + barHtml() + "</div></div>" +
      '<div class="alu-fld"><label>Alcance geogr\u00e1fico</label>' +
        scopeBtn("favorita", "Estaci\u00f3n favorita", favHint) +
        (pro ? scopeBtn("grupo", "Mi grupo", grpHint) : "") +
        avanzada +
        (pro ? "" : '<p class="alu-note">Plan gratis: la regla usa tu estaci\u00f3n favorita. Pro permite el grupo y una regi\u00f3n o comuna.</p>') +
      "</div>" +
      '<div class="alu-fld"><label>Avisar por</label><div class="alu-pair">' +
        '<select id="alu-canal">' +
          '<option value="correo_push"' + (draft.canal === "correo_push" ? " selected" : "") + ">Correo y push</option>" +
          '<option value="correo"' + (draft.canal === "correo" ? " selected" : "") + ">Solo correo</option>" +
          '<option value="push"' + (draft.canal === "push" ? " selected" : "") + ">Solo push</option>" +
        "</select>" +
        '<select id="alu-frec">' +
          '<option value="cada_vez"' + (draft.frecuencia === "cada_vez" ? " selected" : "") + ">Cada estaci\u00f3n que cumpla</option>" +
          '<option value="diaria"' + (draft.frecuencia === "diaria" ? " selected" : "") + ">M\u00e1ximo 1 vez al d\u00eda</option>" +
        "</select>" +
      "</div></div>" +
      '<div class="alu-actions">' +
        '<button type="button" class="alu-pri" id="alu-save">' + (editId ? "Guardar cambios" : "Guardar regla") + "</button>" +
        '<button type="button" class="alu-sec" id="alu-cancel">Cancelar</button>' +
        (editId ? '<button type="button" class="alu-sec alu-danger" id="alu-del">Eliminar</button>' : "") +
      "</div></div>";
  }

  function sectionHtml() {
    return '<section class="al-card al-cfg alu">' +
      "<h2>" + tuneIcon() + "Alertas personalizadas</h2>" +
      '<p class="al-desc">Adem\u00e1s de las alertas autom\u00e1ticas del hist\u00f3rico, define un umbral que te importe a ti. Se eval\u00faan en paralelo.</p>' +
      '<div id="alu-list">' + rulesHtml() + "</div>" +
      '<button type="button" class="alu-add" id="alu-add">' +
        '<svg class="i" viewBox="0 0 24 24"><path d="M12 5v14M5 12h14"/></svg>Nueva regla' +
      "</button>" +
      (open ? builderHtml() : "") +
    "</section>";
  }

  function paintBar() {
    var box = $("alu-range");
    if (box) box.innerHTML = barHtml();
  }

  function loadRef() {
    if (!open || !draft) return;
    var abr = abrOf(draft);
    var key = abr + "|" + (abr === "pp" ? draft.ventana : "");
    if (ref && ref._key === key) {
      paintBar();
      return;
    }
    var token = ++refToken;
    var q = "/me/alertas/referencia-umbral?abr=" + encodeURIComponent(abr);
    if (abr === "pp") q += "&ventana=" + encodeURIComponent(String(draft.ventana));
    ctx.apiFetch(q).then(function (data) {
      if (token !== refToken) return;
      ref = data || {};
      ref._key = key;
      paintBar();
    }).catch(function () {
      if (token !== refToken) return;
      ref = { sin_datos: true, _key: key };
      paintBar();
    });
  }

  function paint() {
    var root = $("al-umbral-root");
    if (!root || !ctx) return;
    root.innerHTML = sectionHtml();
    wire();
    if (open) loadRef();
  }

  function applyResp(data, msg) {
    if (data && data.reglas) ctx.state.umbrales = data.reglas;
    if (data && data.contexto) ctx.state.umbralContexto = data.contexto;
    open = false;
    editId = null;
    saving = false;
    paint();
    ctx.setStatus(msg, true);
  }

  function fail(err) {
    saving = false;
    ctx.setStatus((err && err.message) || "No se pudo guardar la regla.", false);
  }

  function payload() {
    var abr = abrOf(draft);
    var scope = draft.scope;
    var sv = {};
    if (!ctx.isPro() || scope === "favorita") scope = "favorita";
    if (scope === "avanzada") {
      if (draft.nomCom) {
        scope = "comuna";
        sv = { nom_com: draft.nomCom };
      } else if (draft.codReg) {
        scope = "region";
        sv = { cod_reg: parseInt(draft.codReg, 10) };
      } else {
        return { error: "Elige una regi\u00f3n o una comuna." };
      }
    }
    var valor = parseFloat(String(draft.valor).replace(",", "."));
    var hasta = parseFloat(String(draft.valorHasta).replace(",", "."));
    if (!String(draft.nombre || "").trim()) return { error: "Ponle un nombre a la regla." };
    if (!isFinite(valor)) return { error: "Falta el valor del umbral." };
    if (draft.operador === "between" && !isFinite(hasta)) return { error: "Falta el segundo valor del rango." };
    return {
      body: {
        nombre: String(draft.nombre).trim(),
        variable_abr: abr,
        operador: draft.operador,
        valor: valor,
        valor_hasta: draft.operador === "between" ? hasta : null,
        ventana_horas: abr === "pp" ? parseInt(draft.ventana, 10) : null,
        scope_type: scope,
        scope_value: sv,
        canal: draft.canal,
        frecuencia: draft.frecuencia,
        activa: true
      }
    };
  }

  function save() {
    if (saving) return;
    var pack = payload();
    if (pack.error) {
      ctx.setStatus(pack.error, false);
      return;
    }
    saving = true;
    var req = editId
      ? ctx.apiFetch("/me/alertas/umbrales/" + encodeURIComponent(editId), {
          method: "PUT",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(pack.body)
        })
      : ctx.apiFetch("/me/alertas/umbrales", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(pack.body)
        });
    req.then(function (data) {
      applyResp(data, editId ? "Regla actualizada." : "Regla guardada.");
    }).catch(fail);
  }

  function fillFrom(r) {
    var abr = String(r.variable_abr || "pp");
    var sv = scopeValue(r);
    draft = freshDraft();
    draft.nombre = r.nombre || "";
    draft.operador = r.operador || "gt";
    draft.valor = r.valor == null ? "" : String(r.valor);
    draft.valorHasta = r.valor_hasta == null ? "" : String(r.valor_hasta);
    draft.ventana = r.ventana_horas || 24;
    draft.canal = r.canal || "correo_push";
    draft.frecuencia = r.frecuencia || "cada_vez";
    if (abr === "t_max" || abr === "t_min") {
      draft.familia = "temp";
      draft.serie = abr;
    } else if (abr === "u_rafaga") {
      draft.familia = "wind";
    } else {
      draft.familia = "pp";
    }
    if (r.scope_type === "grupo") draft.scope = "grupo";
    else if (r.scope_type === "region" || r.scope_type === "comuna") {
      draft.scope = "avanzada";
      draft.codReg = sv.cod_reg || "";
      draft.nomCom = sv.nom_com || "";
      if (r.scope_type === "comuna" && !draft.codReg) {
        ((ctx.state && ctx.state.stationsCatalog) || []).some(function (s) {
          if (String(s.nom_com || "").trim() === draft.nomCom) {
            draft.codReg = s.cod_reg;
            return true;
          }
          return false;
        });
      }
    } else {
      draft.scope = "favorita";
    }
  }

  function wire() {
    var add = $("alu-add");
    if (add) {
      add.addEventListener("click", function () {
        if (open && !editId) {
          open = false;
        } else {
          draft = freshDraft();
          editId = null;
          open = true;
          ref = null;
        }
        paint();
      });
    }
    document.querySelectorAll("#alu-list .alu-toggle").forEach(function (cb) {
      cb.addEventListener("change", function () {
        var row = cb.closest(".alu-rule");
        var id = row && row.getAttribute("data-id");
        if (!id) return;
        var on = cb.checked;
        ctx.apiFetch("/me/alertas/umbrales/" + encodeURIComponent(id), {
          method: "PUT",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ activa: on })
        }).then(function (data) {
          if (data && data.reglas) ctx.state.umbrales = data.reglas;
          if (data && data.contexto) ctx.state.umbralContexto = data.contexto;
          paint();
          ctx.setStatus(on ? "Regla activada." : "Regla desactivada.", true);
        }).catch(function (err) {
          cb.checked = !on;
          fail(err);
        });
      });
    });
    document.querySelectorAll("#alu-list .alu-edit").forEach(function (btn) {
      btn.addEventListener("click", function () {
        var row = btn.closest(".alu-rule");
        var id = row && row.getAttribute("data-id");
        var hit = null;
        asRules(ctx.state.umbrales).some(function (r) {
          if (String(r.id) === String(id)) { hit = r; return true; }
          return false;
        });
        if (!hit) return;
        fillFrom(hit);
        editId = id;
        open = true;
        ref = null;
        paint();
      });
    });
    if (!open) return;

    var nombre = $("alu-nombre");
    if (nombre) nombre.addEventListener("input", function () { draft.nombre = nombre.value; });
    var valor = $("alu-valor");
    if (valor) valor.addEventListener("input", function () { draft.valor = valor.value; paintBar(); });
    var valor2 = $("alu-valor2");
    if (valor2) valor2.addEventListener("input", function () { draft.valorHasta = valor2.value; paintBar(); });
    var ventana = $("alu-ventana");
    if (ventana) ventana.addEventListener("change", function () {
      draft.ventana = parseInt(ventana.value, 10);
      ref = null;
      loadRef();
    });
    var canal = $("alu-canal");
    if (canal) canal.addEventListener("change", function () { draft.canal = canal.value; });
    var frec = $("alu-frec");
    if (frec) frec.addEventListener("change", function () { draft.frecuencia = frec.value; });
    var reg = $("alu-reg");
    if (reg) reg.addEventListener("change", function () {
      draft.codReg = reg.value;
      draft.nomCom = "";
      paint();
    });
    var com = $("alu-com");
    if (com) com.addEventListener("change", function () { draft.nomCom = com.value; });

    document.querySelectorAll("#al-umbral-root .alu-vp").forEach(function (btn) {
      btn.addEventListener("click", function () {
        var fam = btn.getAttribute("data-fam");
        draft.familia = fam;
        if (fam === "temp") {
          draft.operador = "lt";
          draft.serie = "t_min";
          draft.valor = "2";
        } else if (fam === "wind") {
          draft.operador = "gt";
          draft.valor = "25";
        } else {
          draft.operador = "gt";
          draft.valor = "30";
          draft.ventana = 24;
        }
        ref = null;
        paint();
      });
    });
    document.querySelectorAll("#al-umbral-root .alu-segs .alu-seg").forEach(function (btn) {
      btn.addEventListener("click", function () {
        var serie = btn.getAttribute("data-serie");
        if (serie) {
          draft.serie = serie;
          ref = null;
          paint();
          return;
        }
        draft.operador = btn.getAttribute("data-op") || "gt";
        if (draft.familia === "temp") {
          if (draft.operador === "gt") draft.serie = "t_max";
          if (draft.operador === "lt") draft.serie = "t_min";
        }
        ref = null;
        paint();
      });
    });
    document.querySelectorAll("#al-umbral-root .alu-scope-row").forEach(function (btn) {
      btn.addEventListener("click", function () {
        draft.scope = btn.getAttribute("data-scope") || "favorita";
        paint();
      });
    });
    var saveBtn = $("alu-save");
    if (saveBtn) saveBtn.addEventListener("click", save);
    var cancel = $("alu-cancel");
    if (cancel) cancel.addEventListener("click", function () {
      open = false;
      editId = null;
      paint();
    });
    var del = $("alu-del");
    if (del) del.addEventListener("click", function () {
      if (!editId || !global.confirm("\u00bfEliminar esta regla?")) return;
      ctx.apiFetch("/me/alertas/umbrales/" + encodeURIComponent(editId), { method: "DELETE" })
        .then(function (data) { applyResp(data, "Regla eliminada."); })
        .catch(fail);
    });
  }

  function tuneIcon() {
    return '<svg class="i" viewBox="0 0 24 24"><path d="M4 6h10M4 12h6M4 18h13"/><circle cx="17" cy="6" r="2"/><circle cx="13" cy="18" r="2"/></svg>';
  }

  global.rodajaUmbralMount = function (c) {
    ctx = c;
    if (!draft) draft = freshDraft();
    paint();
  };
})(window);
