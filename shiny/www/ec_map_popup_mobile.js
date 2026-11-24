/* Mobile: detalle estacion pantalla completa (mensaje Shiny ec_station_sheet). Desktop sin cambios. */
(function () {
  "use strict";

  var wired = false;

  function isMobile() {
    if (window.matchMedia("(max-width: 767px)").matches) return true;
    if (window.matchMedia("(pointer: coarse)").matches && window.innerWidth <= 1024) return true;
    return /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini|Mobile/i.test(
      navigator.userAgent || ""
    );
  }

  function sheetEl() {
    return document.getElementById("ec_station_sheet");
  }

  function sheetContentEl() {
    return document.getElementById("ec_station_sheet_content");
  }

  function moveToBody(el) {
    if (el && el.parentElement !== document.body) {
      document.body.appendChild(el);
    }
  }

  function enableMap() {
    var mount = document.getElementById("mapa_mount");
    if (!mount) return;
    var root = mount.querySelector("[id$='-mapa']") || mount.querySelector(".leaflet-container");
    if (!root) return;
    var map = null;
    if (window.jQuery) map = window.jQuery(root).data("leaflet-map");
    if (!map && window.HTMLWidgets && root.id) {
      try {
        var w = window.HTMLWidgets.find("#" + root.id);
        if (w && typeof w.getMap === "function") map = w.getMap();
      } catch (eHw) { /* ignore */ }
    }
    if (!map) map = root._leaflet_map;
    if (!map) return;
    try {
      if (map.closePopup) map.closePopup();
      if (map.layerManager && map.layerManager.clearLayers) {
        map.layerManager.clearLayers("popup");
      }
      if (map.dragging && map.dragging.enable) map.dragging.enable();
      if (map.touchZoom && map.touchZoom.enable) map.touchZoom.enable();
    } catch (e) { /* ignore */ }
  }

  function openSheet(html) {
    if (!isMobile() || !html) return;
    document.querySelectorAll(".modal-backdrop").forEach(function (el) {
      el.parentNode && el.parentNode.removeChild(el);
    });
    var sheet = sheetEl();
    var content = sheetContentEl();
    if (!sheet || !content) return;
    moveToBody(sheet);
    content.innerHTML = html;
    sheet.classList.add("ec-station-sheet-open");
    sheet.style.display = "flex";
    document.documentElement.classList.add("ec-station-sheet-active");
    document.body.classList.add("ec-station-sheet-active");
    if (typeof window.ecRefreshPopupAuthUi === "function") {
      window.ecRefreshPopupAuthUi(content);
    }
    enableMap();
  }

  function closeSheet() {
    var sheet = sheetEl();
    if (sheet) {
      sheet.classList.remove("ec-station-sheet-open");
      sheet.style.display = "none";
    }
    var content = sheetContentEl();
    if (content) content.innerHTML = "";
    document.documentElement.classList.remove("ec-station-sheet-active");
    document.body.classList.remove("ec-station-sheet-active");
    enableMap();
  }

  function onSheetClick(e) {
    if (!isMobile()) return;
    var sheet = sheetEl();
    if (!sheet || !sheet.classList.contains("ec-station-sheet-open")) return;

    if (e.target.closest(".ec-map-popup-mobile-close")) {
      e.preventDefault();
      closeSheet();
      return;
    }

    var verBtn = e.target.closest(".ec-map-popup-ver-datos");
    if (verBtn) {
      e.preventDefault();
      var inputId = verBtn.getAttribute("data-ec-shiny-input");
      var eid = parseInt(verBtn.getAttribute("data-ec-eid"), 10);
      if (inputId && !isNaN(eid) && window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(inputId, 0);
        Shiny.setInputValue(inputId, eid, { priority: "event" });
      }
      closeSheet();
      return;
    }

    var favBtn = e.target.closest(".ec-map-popup-fijar-fav");
    if (favBtn) {
      e.preventDefault();
      if (favBtn.getAttribute("data-ec-requires-auth") === "1" &&
          typeof window.ecPopupRequireAuthOrRun === "function" &&
          window.ecPopupRequireAuthOrRun(function () { return true; }) === false) {
        return;
      }
      var favEid = parseInt(favBtn.getAttribute("data-ec-eid"), 10);
      var alias = favBtn.getAttribute("data-ec-alias") || "";
      if (!isNaN(favEid)) {
        if (window.ecFijarDesdeMapa) window.ecFijarDesdeMapa(favEid, alias);
        else if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue("fijar_favorito_mapa", favEid, { priority: "event" });
        }
      }
      closeSheet();
      return;
    }

    var dlBtn = e.target.closest(".ec-map-popup-descargar");
    if (dlBtn) {
      e.preventDefault();
      var dlEid = dlBtn.getAttribute("data-ec-descargar-eid");
      if (dlEid) window.open("/descargar/index.html?e=" + dlEid, "_blank");
      closeSheet();
    }
  }

  function wire() {
    if (wired) return;
    if (!window.Shiny || !Shiny.addCustomMessageHandler) return;
    wired = true;
    Shiny.addCustomMessageHandler("ec_station_sheet", function (msg) {
      if (!msg || !msg.html) return;
      openSheet(msg.html);
    });
    moveToBody(sheetEl());
  }

  document.addEventListener("click", onSheetClick, true);
  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape") closeSheet();
  });
  document.addEventListener("shiny:connected", wire);
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", wire);
  } else {
    wire();
  }
  window.ecCloseStationSheet = closeSheet;
})();
