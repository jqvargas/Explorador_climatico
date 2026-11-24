package cl.rodaja.app.widget;

import java.util.List;

/**
 * Aplica {@link StationSelector} y elige el aviso. Si no hay permiso o no hay
 * Play Services, las coordenadas nuevas se tiran antes de elegir.
 */
public final class WidgetPlacement {
    public static final class Decision {
        public final long stationId;
        public final String reason;
        public final String notice;
        public final boolean replaceFix;

        public Decision(long stationId, String reason, String notice, boolean replaceFix) {
            this.stationId = stationId;
            this.reason = reason;
            this.notice = notice;
            this.replaceFix = replaceFix;
        }
    }

    private WidgetPlacement() {}

    public static Decision decide(
            String mode,
            Long heldStationId,
            Double prevLat,
            Double prevLon,
            boolean playServices,
            boolean permissionGranted,
            Double newLat,
            Double newLon,
            List<StationSelector.Point> catalog,
            long defaultStationId) {
        boolean canRead = playServices && permissionGranted;
        Double lat = canRead ? newLat : null;
        Double lon = canRead ? newLon : null;
        StationSelector.Result result = StationSelector.choose(
                mode,
                heldStationId,
                prevLat,
                prevLon,
                lat,
                lon,
                catalog,
                defaultStationId);
        String notice = null;
        if (!StationSelector.MODE_FIXED.equals(mode)) {
            if (!playServices) {
                notice = WidgetFrame.BANNER_NO_PLAY;
            } else if (!permissionGranted) {
                notice = WidgetFrame.BANNER_NO_LOCATION;
            }
        }
        boolean replace = StationSelector.MODE_NEAREST.equals(mode)
                && canRead
                && finite(newLat)
                && finite(newLon)
                && (StationSelector.REASON_CERCANA.equals(result.reason)
                || StationSelector.REASON_LEJOS.equals(result.reason)
                || StationSelector.REASON_VACIO.equals(result.reason));
        return new Decision(result.stationId, result.reason, notice, replace);
    }

    private static boolean finite(Double v) {
        return v != null && !v.isNaN() && !v.isInfinite();
    }
}
