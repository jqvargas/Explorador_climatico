window.RODAJA_CONFIG = {
  clerkPublishableKey: "pk_live_Y2xlcmsucm9kYWphLmNsJA",
  accountUrl: "https://accounts.rodaja.cl",
  gtmId: "GTM-P8DBND7F",
  ga4Id: "G-GSYEXFLDGB",
  adsensePublisherId: "",
  siteCanonicalBase: "https://app.rodaja.cl",
  cartoBasemapKey: "cb1_26va_1_24849550c5a331dae16cbb9f"
};

/* rodaja.cl — atribucion first-touch (referrer + UTM + landing). 90 dias. Sin datos personales. */
(function (w) {
  try {
    var KEY = "rodaja_ft", TTL = 90 * 864e5, now = Date.now(), ls = w.localStorage;
    var cur = null;
    try { cur = JSON.parse(ls.getItem(KEY) || "null"); } catch (e) { cur = null; }
    if (cur && cur.t && (now - cur.t) < TTL) return;
    var q = new URLSearchParams(w.location.search || "");
    var cut = function (v) { return v ? String(v).slice(0, 255) : ""; };
    var us = cut(q.get("utm_source")), um = cut(q.get("utm_medium")), uc = cut(q.get("utm_campaign"));
    var ref = "", rh = "";
    try {
      if (document.referrer) {
        var r = new URL(document.referrer);
        rh = r.hostname.toLowerCase();
        ref = (r.protocol + "//" + rh + r.pathname).slice(0, 255);
      }
    } catch (e) { ref = ""; rh = ""; }
    var interno = rh && (rh === w.location.hostname.toLowerCase() || /(^|\.)rodaja\.cl$/.test(rh) || /clerk\./.test(rh));
    if (interno && !us) return; /* navegacion interna o vuelta del login: no es primera visita */
    ls.setItem(KEY, JSON.stringify({
      t: now, referrer: interno ? "" : ref, utm_source: us, utm_medium: um, utm_campaign: uc,
      landing_path: cut(w.location.pathname || "/")
    }));
  } catch (e) { /* localStorage bloqueado: sin atribucion */ }
})(window);
