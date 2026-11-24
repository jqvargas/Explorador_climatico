/* rodaja.cl — favoritos (PWA mi-reporte) */
(function (global) {
  "use strict";
  var STORAGE_KEY = "rodaja_favorites_v1";

  function parseJson(s, fallback) {
    try { return JSON.parse(s); } catch (e) { return fallback; }
  }
  function emptyStore() { return { primary: null, recents: [] }; }
  function normalizeFav(f) {
    if (!f || typeof f !== "object") return null;
    var eid = parseInt(f.eid, 10);
    if (!eid || eid < 1) return null;
    return {
      eid: eid,
      variableId: f.variableId != null && f.variableId !== "" ? parseInt(f.variableId, 10) : null,
      fuenteId: f.fuenteId != null && f.fuenteId !== "" ? parseInt(f.fuenteId, 10) : null,
      alias: typeof f.alias === "string" ? f.alias : "",
      lat: typeof f.lat === "number" ? f.lat : null,
      lon: typeof f.lon === "number" ? f.lon : null
    };
  }
  function rodajaGetFavorites() {
    var raw = localStorage.getItem(STORAGE_KEY);
    if (!raw) return emptyStore();
    var o = parseJson(raw, emptyStore());
    o.primary = normalizeFav(o.primary);
    o.recents = Array.isArray(o.recents) ? o.recents.map(normalizeFav).filter(Boolean) : [];
    return o;
  }
  function rodajaSaveFavorites(store) {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(store));
    try {
      var p = store && store.primary;
      if (p && p.eid) {
        localStorage.setItem("rodaja_app_reporte_v1", JSON.stringify({
          eid: p.eid,
          variableId: p.variableId || null,
          nombre: p.alias || ""
        }));
      } else {
        localStorage.removeItem("rodaja_app_reporte_v1");
      }
    } catch (eMir) { /* ignore */ }
    try {
      global.dispatchEvent(new CustomEvent("rodaja:favorites-changed", { detail: store }));
    } catch (e) { /* ignore */ }
    if (typeof global.rodajaSyncFavoritesToCloud === "function" && typeof global.rodajaIsSignedIn === "function" && global.rodajaIsSignedIn()) {
      global.rodajaSyncFavoritesToCloud(store);
    }
  }
  function rodajaSetPrimary(fav) {
    var f = normalizeFav(fav);
    if (!f) return false;
    var store = rodajaGetFavorites();
    store.primary = f;
    var rec = store.recents.filter(function (r) { return r.eid !== f.eid; });
    rec.unshift(f);
    store.recents = rec.slice(0, 5);
    rodajaSaveFavorites(store);
    return true;
  }
  function qsInt(sp, keys) {
    var i, n;
    for (i = 0; i < keys.length; i++) {
      if (!sp.has(keys[i])) continue;
      n = parseInt(sp.get(keys[i]), 10);
      if (n && n > 0) return n;
    }
    return null;
  }
  function parseParams() {
    var sp = new URLSearchParams(global.location.search);
    return {
      eid: qsInt(sp, ["e", "eid"]),
      variableId: qsInt(sp, ["v", "variableId"]),
      fuenteId: qsInt(sp, ["f", "fuenteId"])
    };
  }
  function toast(msg) {
    var el = document.getElementById("ec-fav-toast");
    if (!el) {
      el = document.createElement("div");
      el.id = "ec-fav-toast";
      el.className = "ec-fav-toast";
      document.body.appendChild(el);
    }
    el.textContent = msg;
    el.style.display = "block";
    clearTimeout(el._t);
    el._t = setTimeout(function () { el.style.display = "none"; }, 2800);
  }
  global.rodajaGetFavorites = rodajaGetFavorites;
  global.rodajaSetPrimary = rodajaSetPrimary;
  global.rodajaParseMiReporteParams = parseParams;
  global.rodajaToast = toast;
})(window);
