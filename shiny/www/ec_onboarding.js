/* rodaja.cl — tour guiado (cicerone / driver.js); solo botón Guía de Uso */
(function (global) {
  "use strict";

  function highlightedEl() {
    return document.querySelector(".driver-highlighted-element");
  }

  function popoverEl() {
    return document.getElementById("driver-popover-item")
      || document.getElementById("ec_onboarding")
      || document.querySelector(".ec-cicerone-pop");
  }

  global.ecOnboardingRepositionPopover = function ecOnboardingRepositionPopover(step) {
    var pop = popoverEl();
    if (!pop) return false;

    var before = pop.getBoundingClientRect();
    if (before.width < 20 || before.height < 20) return false;

    var target = highlightedEl();
    if (!target) return false;
    var tr = target.getBoundingClientRect();
    if (tr.width < 1 && tr.height < 1) return false;

    var pad = 16;
    var minTop = 72;
    var pw = before.width;
    var ph = before.height;
    var vw = window.innerWidth;
    var vh = window.innerHeight;
    var left;
    var top;

    pop.style.setProperty("position", "fixed", "important");
    pop.style.setProperty("transform", "none", "important");
    pop.style.setProperty("bottom", "auto", "important");
    pop.style.setProperty("right", "auto", "important");
    pop.classList.add("ec-popover-fixed");

    var stepKey = global.__ecOnboardingStepKey;
    if (stepKey === "welcome") {
      left = Math.max(pad, (vw - pw) / 2);
      top = Math.max(minTop, (vh - ph) / 2);
      pop.style.setProperty("left", Math.round(left) + "px", "important");
      pop.style.setProperty("top", Math.round(top) + "px", "important");
      return true;
    }

    if (tr.right + pad + pw <= vw - pad) {
      left = tr.right + pad;
      top = tr.top + (tr.height - ph) / 2;
    } else if (tr.left - pad - pw >= pad) {
      left = tr.left - pad - pw;
      top = tr.top + (tr.height - ph) / 2;
    } else {
      left = Math.max(pad, Math.min(tr.left, vw - pw - pad));
      top = tr.bottom + pad;
    }

    top = Math.max(minTop, Math.min(top, vh - ph - pad));
    if (top + ph > vh - pad) {
      top = Math.max(minTop, tr.top - ph - pad);
    }

    pop.style.setProperty("left", Math.round(left) + "px", "important");
    pop.style.setProperty("top", Math.round(top) + "px", "important");
    return true;
  };

  global.ecOnboardingFitPopoverWidth = function ecOnboardingFitPopoverWidth() {
    var pop = popoverEl();
    if (!pop) return false;
    var cap = Math.min(380, Math.max(260, Math.round(window.innerWidth * 0.94)));
    pop.style.setProperty("min-width", "260px", "important");
    pop.style.setProperty("max-width", cap + "px", "important");
    pop.style.setProperty("width", cap + "px", "important");
    pop.style.setProperty("box-sizing", "border-box", "important");
    var footer = pop.querySelector(".driver-popover-footer");
    if (footer) {
      footer.style.setProperty("width", "100%", "important");
      footer.style.setProperty("max-width", "100%", "important");
      footer.style.setProperty("box-sizing", "border-box", "important");
    }
    return true;
  };

  global.ecOnboardingScheduleFixPopover = function ecOnboardingScheduleFixPopover(step) {
    var tries = 0;
    function attempt() {
      tries += 1;
      if (global.ecOnboardingRepositionPopover(step)) {
        global.ecOnboardingFitPopoverWidth();
        return;
      }
      if (tries < 90) requestAnimationFrame(attempt);
    }
    attempt();
  };

  function leafletMap() {
    var mount = document.getElementById("mapa_mount");
    if (!mount) return null;
    var root = mount.querySelector(".leaflet-container") || mount.querySelector("[id$='-mapa']");
    if (!root || !root._leaflet_map) return null;
    return root._leaflet_map;
  }

  function invalidateLeafletSize() {
    var map = leafletMap();
    if (!map || typeof map.invalidateSize !== "function") return;
    try {
      map.invalidateSize({ animate: false, pan: false });
    } catch (e) { /* ignore */ }
  }

  function patchDriverCapture() {
    if (global.__ecDriverPatched || typeof global.Driver !== "function") return;
    global.__ecDriverPatched = true;
    var Orig = global.Driver;
    function WrappedDriver(opts) {
      var inst = new Orig(opts);
      var id = opts && (opts.id || (opts.globals && opts.globals.id));
      if (id === "ec_onboarding" || global.__ecOnboardingActive) {
        global.__ecCiceroneDriver = inst;
        var origRefresh = inst.refresh.bind(inst);
        inst.refresh = function () {
          origRefresh();
          var step = global.__ecOnboardingStepKey || global.__ecOnboardingCurrentStep;
          if (step) global.ecOnboardingScheduleFixPopover(step);
        };
      }
      return inst;
    }
    WrappedDriver.prototype = Orig.prototype;
    global.Driver = WrappedDriver;
  }

  function schedulePostStartRefresh() {
    requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        invalidateLeafletSize();
        global.ecOnboardingRefreshDriver();
        neutralizeOverlayDismiss();
      });
    });
  }

  // Overlay de driver.js cierra el tour si allowClose; el mapa debe seguir clicable.
  function neutralizeOverlayDismiss() {
    var overlay = document.getElementById("driver-page-overlay");
    if (!overlay || overlay.getAttribute("data-ec-overlay-neutral") === "1") return;
    overlay.setAttribute("data-ec-overlay-neutral", "1");
    overlay.addEventListener("click", function (ev) {
      ev.stopImmediatePropagation();
      ev.stopPropagation();
      ev.preventDefault();
    }, true);
  }

  function resetOnboardingUiState() {
    global.__ecOnboardingActive = false;
    global.__ecOnboardingStepKey = null;
    global.__ecOnboardingCurrentStep = null;
    document.documentElement.classList.remove("ec-onboarding-active");
    document.documentElement.classList.remove("ec-onboarding-force-drawer");
  }

  function destroyOnboardingDriver() {
    var d = global.__ecCiceroneDriver;
    global.__ecCiceroneDriver = null;
    if (!d) return;
    if (typeof d.reset === "function") {
      try { d.reset(); return; } catch (e) { /* fall through */ }
    }
    if (typeof d.destroy === "function") {
      try { d.destroy(); } catch (e2) { /* ignore */ }
    }
  }

  global.ecOnboardingEnter = function ecOnboardingEnter() {
    document.documentElement.classList.add("ec-onboarding-active");
    requestAnimationFrame(function () {
      requestAnimationFrame(invalidateLeafletSize);
    });
  };

  global.ecOnboardingLeave = function ecOnboardingLeave() {
    document.documentElement.classList.remove("ec-onboarding-active");
    document.documentElement.classList.remove("ec-onboarding-force-drawer");
    global.__ecCiceroneDriver = null;
    setTimeout(invalidateLeafletSize, 80);
  };

  global.ecOnboardingRefreshDriver = function ecOnboardingRefreshDriver() {
    var d = global.__ecCiceroneDriver;
    if (d && typeof d.refresh === "function") {
      try { d.refresh(); return true; } catch (e) { /* fall through */ }
    }
    var overlay = document.getElementById("driver-page-overlay");
    if (overlay && typeof overlay.refresh === "function") {
      try { overlay.refresh(); return true; } catch (e2) { /* ignore */ }
    }
    return false;
  };

  global.ecOnboardingScrollHighlight = function ecOnboardingScrollHighlight() {
    var target = highlightedEl();
    if (!target) return false;
    try {
      target.scrollIntoView({ block: "center", inline: "nearest", behavior: "instant" });
    } catch (e1) {
      try { target.scrollIntoView(true); } catch (e2) { /* ignore */ }
    }
    return true;
  };

  function elUsable(el) {
    if (!el) return false;
    if (el.offsetParent === null && el !== document.body) return false;
    var r = el.getBoundingClientRect();
    return r.width > 4 && r.height > 4;
  }

  function buildSteps() {
    var steps = ["welcome"];
    if (document.getElementById("ec-choropleth-bc")) steps.push("mapa");
    if (elUsable(document.getElementById("ec-icon-vars"))) steps.push("pronostico");
    else if (elUsable(document.getElementById("ec-map-capa-icon"))) steps.push("pronostico_dock");
    if (elUsable(document.getElementById("ec_map_legend"))) steps.push("leyenda");
    if (elUsable(document.getElementById("map_usar_geo"))) steps.push("geo");
    return steps;
  }

  function requiredPresent(steps) {
    if (steps.indexOf("welcome") >= 0 && !document.getElementById("ec_explorador_viewport")) return false;
    if (steps.indexOf("mapa") >= 0 && !document.getElementById("ec-choropleth-bc")) return false;
    if (steps.indexOf("pronostico") >= 0 && !document.getElementById("ec-icon-vars")) return false;
    if (steps.indexOf("pronostico_dock") >= 0 && !document.getElementById("ec-map-capa-icon")) return false;
    if (steps.indexOf("leyenda") >= 0 && !document.getElementById("ec_map_legend")) return false;
    if (steps.indexOf("geo") >= 0 && !document.getElementById("map_usar_geo")) return false;
    return true;
  }

  function sendTourRequest(steps) {
    var payload = JSON.stringify({ force: true, steps: steps, ts: Date.now() });
    Shiny.setInputValue("ec_onboarding_run", payload, { priority: "event" });
  }

  function requestTour() {
    if (typeof Shiny === "undefined" || !Shiny.setInputValue) return false;
    if (global.__ecCiceroneDriver) destroyOnboardingDriver();

    var steps = buildSteps();
    if (!requiredPresent(steps)) return false;

    global.__ecOnboardingActive = true;
    global.ecOnboardingEnter();
    requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        invalidateLeafletSize();
        sendTourRequest(steps);
      });
    });
    return true;
  }

  function startManualTour() {
    destroyOnboardingDriver();
    resetOnboardingUiState();
    var attempts = 0;
    (function tryStart() {
      if (requestTour()) return;
      if (attempts >= 8) return;
      attempts += 1;
      setTimeout(tryStart, 400);
    })();
  }

  global.ecOnboardingSetProgress = function (cur, total, stepKey) {
    global.__ecOnboardingCurrentStep = cur;
    global.__ecOnboardingStepKey = stepKey || null;
    var pop = popoverEl();
    if (pop) {
      var prog = pop.querySelector(".ec-onboarding-progress");
      if (!prog) {
        prog = document.createElement("div");
        prog.className = "ec-onboarding-progress";
        prog.setAttribute("aria-live", "polite");
        var title = pop.querySelector(".driver-popover-title");
        if (title) pop.insertBefore(prog, title);
        else pop.insertBefore(prog, pop.firstChild);
      }
      prog.textContent = "Paso " + cur + " de " + total;
    }

    global.ecOnboardingFitPopoverWidth();

    requestAnimationFrame(function () {
      global.ecOnboardingRefreshDriver();
      global.ecOnboardingScheduleFixPopover(cur);
      neutralizeOverlayDismiss();
    });
  };

  global.ecOnboardingMarkDone = function () {
    global.__ecOnboardingActive = false;
    global.ecOnboardingLeave();
  };

  global.ecOnboardingFinish = function () {
    if (!global.__ecOnboardingActive) return;
    destroyOnboardingDriver();
    global.ecOnboardingMarkDone();
  };

  function watchDriverEnd() {
    if (global.__ecOnboardingWatchInstalled) return;
    global.__ecOnboardingWatchInstalled = true;

    document.body.addEventListener("click", function (ev) {
      if (!global.__ecOnboardingActive) return;
      var btn = ev.target.closest
        ? ev.target.closest(".driver-popover-footer button, .driver-close-btn")
        : null;
      if (!btn) return;
      var txt = (btn.textContent || "").trim();
      if (txt === "Salir" || txt === "Finalizar" || btn.classList.contains("driver-close-btn")) {
        setTimeout(function () { global.ecOnboardingFinish(); }, 0);
      }
    }, true);

    document.addEventListener("keydown", function (ev) {
      if (!global.__ecOnboardingActive) return;
      if (ev.key === "Escape" || ev.keyCode === 27) {
        setTimeout(function () { global.ecOnboardingFinish(); }, 0);
      }
    });
  }

  global.ecStartManualTour = startManualTour;

  function registerHandlers() {
    if (global.__ecOnboardingHandlersRegistered) return;
    if (typeof Shiny === "undefined" || !Shiny.addCustomMessageHandler) return;
    global.__ecOnboardingHandlersRegistered = true;

    Shiny.addCustomMessageHandler("ec_onboarding_active", function (on) {
      if (on) {
        global.__ecOnboardingActive = true;
        schedulePostStartRefresh();
      }
    });
  }

  function boot() {
    patchDriverCapture();
    watchDriverEnd();
    registerHandlers();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    boot();
  }
  document.addEventListener("shiny:connected", function () {
    boot();
  });
})(window);
