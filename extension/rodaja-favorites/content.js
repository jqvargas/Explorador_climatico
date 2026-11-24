(function () {
  "use strict";
  var KEY = "rodaja_favorites_v1";

  function parseExplorador(url) {
    var u = new URL(url);
    if (u.pathname.indexOf("/explorador") < 0) return null;
    var sp = u.searchParams;
    var eid = parseInt(sp.get("estacion") || "", 10);
    if (!eid) return null;
    return {
      eid: eid,
      variableId: parseInt(sp.get("variable") || "", 10) || null,
      fuenteId: parseInt(sp.get("operador") || "", 10) || null,
      alias: ""
    };
  }

  function parseMiReporte(url) {
    var u = new URL(url);
    if (u.pathname.indexOf("/mi-reporte") < 0) return null;
    var eid = parseInt(u.searchParams.get("e") || "", 10);
    if (!eid) return null;
    return {
      eid: eid,
      variableId: parseInt(u.searchParams.get("v") || "", 10) || null,
      fuenteId: parseInt(u.searchParams.get("f") || "", 10) || null,
      alias: ""
    };
  }

  function savePrimary(fav) {
    if (!fav || !fav.eid) return false;
    var store = { primary: fav, recents: [fav] };
    try {
      var raw = localStorage.getItem(KEY);
      if (raw) {
        var o = JSON.parse(raw);
        if (o && o.recents) store.recents = [fav].concat(o.recents.filter(function (r) { return r.eid !== fav.eid; })).slice(0, 5);
      }
    } catch (e) { /* ignore */ }
    localStorage.setItem(KEY, JSON.stringify(store));
    chrome.storage.local.set({ rodaja_favorites_v1: JSON.stringify(store) });
    return true;
  }

  chrome.runtime.onMessage.addListener(function (msg, sender, sendResponse) {
    if (msg.action !== "fixFromUrl") return;
    var fav = parseExplorador(window.location.href) || parseMiReporte(window.location.href);
    if (!fav) {
      sendResponse({ ok: false });
      return;
    }
    if (window.rodajaGetFavorites) {
      var store = window.rodajaGetFavorites();
      if (store.primary && store.primary.alias) fav.alias = store.primary.alias;
    }
    sendResponse({ ok: savePrimary(fav) });
  });
})();
