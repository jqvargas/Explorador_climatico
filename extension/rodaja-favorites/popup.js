(function () {
  "use strict";
  var KEY = "rodaja_favorites_v1";

  function getStore(cb) {
    chrome.storage.local.get(KEY, function (data) {
      var store = { primary: null, recents: [] };
      try {
        if (data[KEY]) store = JSON.parse(data[KEY]);
      } catch (e) { /* ignore */ }
      cb(store);
    });
  }

  function render(store) {
    var status = document.getElementById("status");
    var primary = document.getElementById("primary");
    var openBtn = document.getElementById("open");
    if (!store.primary || !store.primary.eid) {
      status.textContent = "Sin estaci\u00f3n favorita. Usa el explorador o fija la pesta\u00f1a actual.";
      openBtn.hidden = true;
      primary.hidden = true;
      return;
    }
    status.hidden = true;
    primary.hidden = false;
    openBtn.hidden = false;
    primary.textContent = store.primary.alias || ("Estaci\u00f3n " + store.primary.eid);
    openBtn.onclick = function () {
      var p = store.primary;
      var q = "?e=" + p.eid;
      if (p.variableId) q += "&v=" + p.variableId;
      if (p.fuenteId) q += "&f=" + p.fuenteId;
      var base = "http://localhost:3000/mi-reporte/";
      chrome.storage.local.get("rodaja_base_url", function (cfg) {
        if (cfg.rodaja_base_url) base = cfg.rodaja_base_url;
        chrome.tabs.create({ url: base + q });
      });
    };
  }

  document.getElementById("fix-tab").addEventListener("click", function () {
    chrome.tabs.query({ active: true, currentWindow: true }, function (tabs) {
      var tab = tabs[0];
      if (!tab || !tab.url) return;
      chrome.tabs.sendMessage(tab.id, { action: "fixFromUrl" }, function (resp) {
        if (chrome.runtime.lastError || !resp || !resp.ok) {
          document.getElementById("status").textContent = "Abre /explorador o /mi-reporte en esa pesta\u00f1a.";
          return;
        }
        getStore(render);
      });
    });
  });

  getStore(render);
})();
