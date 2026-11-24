/* Popup estacion en mapa: ajuste desktop tras apertura (zoom UI + autoPan). */
(function () {
  "use strict";

  function isMobile() {
    if (window.matchMedia("(max-width: 767px)").matches) return true;
    if (window.matchMedia("(pointer: coarse)").matches && window.innerWidth <= 1024) return true;
    return /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini|Mobile/i.test(
      navigator.userAgent || ""
    );
  }

  function mapHost() {
    var mount = document.getElementById("mapa_mount");
    if (!mount) return null;
    return mount.querySelector("[id$='-mapa']") || mount.querySelector(".leaflet-container");
  }

  function getLeafletMap() {
    var root = mapHost();
    if (!root) return null;
    if (window.jQuery) {
      var viaJq = window.jQuery(root).data("leaflet-map");
      if (viaJq) return viaJq;
    }
    if (window.HTMLWidgets && typeof window.HTMLWidgets.find === "function" && root.id) {
      try {
        var w = window.HTMLWidgets.find("#" + root.id);
        if (w && typeof w.getMap === "function") {
          var viaHw = w.getMap();
          if (viaHw) return viaHw;
        }
      } catch (eHw) { /* ignore */ }
    }
    if (root._leaflet_map) return root._leaflet_map;
    return null;
  }

  function closeDesktopPopup() {
    var map = getLeafletMap();
    if (map) {
      try {
        if (typeof map.closePopup === "function") map.closePopup();
      } catch (eClose) { /* ignore */ }
      try {
        if (map.layerManager && typeof map.layerManager.clearLayers === "function") {
          map.layerManager.clearLayers("popup");
        }
      } catch (eLm) { /* ignore */ }
    }
  }

  var _ecIgnoreMoveEnd = false;

  // Centra el mapa en el widget (card), no en el marcador: el popup vive arriba
  // de la estación y setView(lat/lon) lo dejaba cortado arriba.
  function centerPopupInMap(map, popup) {
    if (!map || !popup) return;
    var el = popup.getElement && popup.getElement();
    if (!el) return;
    var card = el.querySelector(".leaflet-popup-content-wrapper") || el;
    var cardR = card.getBoundingClientRect();
    var mapR = map.getContainer().getBoundingClientRect();
    if (cardR.width < 8 || cardR.height < 8) return;

    var popCx = (cardR.left + cardR.right) / 2;
    var popCy = (cardR.top + cardR.bottom) / 2;
    var mapCx = (mapR.left + mapR.right) / 2;
    var mapCy = (mapR.top + mapR.bottom) / 2;
    var dx = popCx - mapCx;
    var dy = popCy - mapCy;
    if (Math.abs(dx) < 8 && Math.abs(dy) < 8) return;

    var size = map.getSize();
    var scaleX = size && size.x ? mapR.width / size.x : 1;
    var scaleY = size && size.y ? mapR.height / size.y : 1;
    if (!isFinite(scaleX) || scaleX <= 0) scaleX = 1;
    if (!isFinite(scaleY) || scaleY <= 0) scaleY = 1;

    try {
      _ecIgnoreMoveEnd = true;
      map.panBy([dx / scaleX, dy / scaleY], { animate: false });
      requestAnimationFrame(function () {
        requestAnimationFrame(function () { _ecIgnoreMoveEnd = false; });
      });
    } catch (ePan) {
      _ecIgnoreMoveEnd = false;
    }
  }

  // Nested CSS zoom (html 0.67 × #mapa_mount ~1.49) makes Leaflet anchor the
  // popup TOP at latlng while the tip stays at the bottom. Measure tip vs
  // latlng and shift with margin so the flecha points at the estación.
  function alignPopupTipToMarker(map, popup) {
    if (!map || !popup) return;
    var el = popup.getElement && popup.getElement();
    if (!el) return;
    var tip = el.querySelector(".leaflet-popup-tip-container") || el.querySelector(".leaflet-popup-tip");
    if (!tip) return;
    var ll = popup.getLatLng && popup.getLatLng();
    if (!ll) return;

    el.style.marginTop = "";
    el.style.marginLeft = "";

    var mapR = map.getContainer().getBoundingClientRect();
    var tipR = tip.getBoundingClientRect();
    if (tipR.width < 2 || tipR.height < 2) return;
    var pt = map.latLngToContainerPoint(ll);
    var size = map.getSize();
    var scaleX = size && size.x ? mapR.width / size.x : 1;
    var scaleY = size && size.y ? mapR.height / size.y : 1;
    if (!isFinite(scaleX) || scaleX <= 0) scaleX = 1;
    if (!isFinite(scaleY) || scaleY <= 0) scaleY = 1;

    var markerVx = pt.x * scaleX;
    var markerVy = pt.y * scaleY;
    var tipVx = tipR.left + tipR.width / 2 - mapR.left;
    var tipVy = tipR.bottom - mapR.top;
    var dx = tipVx - markerVx;
    var dy = tipVy - markerVy;

    if (Math.abs(dx) < 6 && Math.abs(dy) < 6) return;
    el.style.marginLeft = (-dx / scaleX) + "px";
    el.style.marginTop = (-dy / scaleY) + "px";
  }

  function refreshPopupLayout(map, popup) {
    if (!map || !popup) return;
    try {
      map.invalidateSize({ pan: false });
      if (popup.update) popup.update();
      alignPopupTipToMarker(map, popup);
      centerPopupInMap(map, popup);
    } catch (e) { /* ignore */ }
  }

  function refreshPopupTipOnly(map, popup) {
    if (!map || !popup) return;
    try {
      alignPopupTipToMarker(map, popup);
    } catch (e) { /* ignore */ }
  }

  // Keep only the real double-border fix: drop stacked Leaflet popup DOM nodes.
  function dedupeLeafletPopups(root) {
    if (!root) return;
    var pane = root.querySelector(".leaflet-popup-pane");
    if (!pane) return;
    var pops = pane.querySelectorAll(".leaflet-popup");
    if (pops.length < 2) return;
    for (var i = 0; i < pops.length - 1; i++) {
      if (pops[i].parentNode) pops[i].parentNode.removeChild(pops[i]);
    }
  }

  function applyControlTip(el, tip) {
    if (!el || el.getAttribute("data-tip")) return;
    el.classList.add("ec-map-legend-tip", "ec-map-control-tip");
    el.setAttribute("data-tip", tip);
    el.setAttribute("aria-label", tip);
    if (!el.hasAttribute("tabindex")) el.setAttribute("tabindex", "0");
  }

  function wireMapControlTips() {
    var root = mapHost();
    if (!root) return;
    applyControlTip(root.querySelector(".leaflet-control-zoom-in"), "Acercar mapa");
    applyControlTip(root.querySelector(".leaflet-control-zoom-out"), "Alejar mapa");
  }

  function wireMapPopup() {
    if (isMobile()) return;
    var root = mapHost();
    var map = getLeafletMap();
    if (!root || !map || root._ecPopupDesktopWired) return;
    root._ecPopupDesktopWired = true;
    var layoutTimer = null;
    var lastPopup = null;

    function schedulePopupLayout(popup, delay) {
      if (popup) lastPopup = popup;
      if (layoutTimer) clearTimeout(layoutTimer);
      layoutTimer = setTimeout(function () {
        layoutTimer = null;
        refreshPopupLayout(map, lastPopup || popup);
      }, delay || 80);
    }

    map.on("popupopen", function (ev) {
      dedupeLeafletPopups(root);
      schedulePopupLayout(ev && ev.popup, 80);
      // Content swap loading→full cambia la altura: realinear tip + re-centrar.
      schedulePopupLayout(ev && ev.popup, 280);
    });

    map.on("moveend zoomend", function () {
      if (_ecIgnoreMoveEnd) return;
      var popup = lastPopup || map._popup;
      if (!popup) return;
      if (popup.isOpen && !popup.isOpen()) return;
      // No re-centrar: el usuario debe poder panear. Solo pegar tip al marcador.
      refreshPopupTipOnly(map, popup);
    });
  }

  function isSignedIn() {
    return typeof window.rodajaIsSignedIn === "function" && !!window.rodajaIsSignedIn();
  }

  function promptLogin() {
    var url = typeof window.rodajaSignInUrl === "function"
      ? window.rodajaSignInUrl(window.location.href)
      : "/cuenta/";
    window.location.href = url;
  }

  function refreshPopupAuthUi(root) {
    var signed = isSignedIn();
    var scope = root || document;
    scope.querySelectorAll("[data-ec-requires-auth]").forEach(function (btn) {
      btn.classList.toggle("ec-map-popup-auth-locked", !signed);
      btn.setAttribute("aria-disabled", signed ? "false" : "true");
      if (!signed) {
        btn.setAttribute("title", "Inicia sesi\u00f3n para usar esta acci\u00f3n");
      } else {
        btn.removeAttribute("title");
      }
    });
  }

  window.ecPopupRequireAuthOrRun = function (fn) {
    if (!isSignedIn()) {
      promptLogin();
      return false;
    }
    if (typeof fn === "function") return fn();
    return true;
  };

  window.ecRefreshPopupAuthUi = refreshPopupAuthUi;

  function tryWire() {
    wireMapControlTips();
    wireMapPopup();
    refreshPopupAuthUi();
    if (!getLeafletMap()) {
      setTimeout(tryWire, 400);
    }
  }

  document.addEventListener("click", function (e) {
    var authBtn = e.target.closest("[data-ec-requires-auth]");
    if (authBtn && !isSignedIn()) {
      e.preventDefault();
      e.stopPropagation();
      promptLogin();
      return;
    }
    if (isMobile()) return;
    if (!e.target.closest(".ec-map-popup-mobile-close")) return;
    e.preventDefault();
    e.stopPropagation();
    closeDesktopPopup();
  }, true);

  document.addEventListener("rodaja:auth-changed", function () {
    refreshPopupAuthUi();
  });

  function wireSetView() {
    if (typeof Shiny === "undefined" || !Shiny.addCustomMessageHandler) return;
    Shiny.addCustomMessageHandler("ec_map_set_view", function (msg) {
      if (!msg) return;
      var lat = Number(msg.lat);
      var lon = Number(msg.lon);
      var zoom = msg.zoom != null ? Number(msg.zoom) : 10;
      if (!isFinite(lat) || !isFinite(lon)) return;
      var tries = 0;
      function go() {
        var map = getLeafletMap();
        if (map && typeof map.setView === "function") {
          try { map.setView([lat, lon], isFinite(zoom) ? zoom : 10); } catch (e) { /* ignore */ }
          return;
        }
        if (tries++ < 40) setTimeout(go, 150);
      }
      go();
    });
  }

  document.addEventListener("shiny:connected", tryWire);
  document.addEventListener("shiny:connected", wireSetView);
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () {
      tryWire();
      wireSetView();
    });
  } else {
    tryWire();
    wireSetView();
  }
})();
