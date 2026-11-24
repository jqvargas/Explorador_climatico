package cl.rodaja.app.widget;

/** Decisiones de arranque y de pin, sin Android. */
public final class WidgetGates {
    public static final String PREFS = "rodaja_twa";
    public static final String PREF_OFFER_SEEN = "widget_offer_seen";
    public static final String EXTRA_OFFER_SEEN = "rodaja_offer_seen";
    public static final String EXTRA_LAUNCH_DATA = "rodaja_launch_data";
    public static final String EXTRA_ONLY_LOCATION = "rodaja_only_location";
    public static final String EXTRA_STATION = "e";

    private WidgetGates() {}

    /** SDK 26, launcher que ancla, y ninguna instancia. El rechazo no se reintenta solo. */
    public static boolean shouldOfferPin(int sdkInt, boolean launcherSupportsPin, int widgetCount) {
        return sdkInt >= 26 && launcherSupportsPin && widgetCount <= 0;
    }

    /** Extra S.e del esquema rodaja. Solo un entero positivo, sin punto ni signo. */
    public static Long stationExtra(String raw) {
        if (raw == null || raw.length() == 0) {
            return null;
        }
        for (int i = 0; i < raw.length(); i++) {
            char c = raw.charAt(i);
            if (c < '0' || c > '9') {
                return null;
            }
        }
        try {
            long id = Long.parseLong(raw);
            return id > 0L ? Long.valueOf(id) : null;
        } catch (NumberFormatException e) {
            return null;
        }
    }
}
