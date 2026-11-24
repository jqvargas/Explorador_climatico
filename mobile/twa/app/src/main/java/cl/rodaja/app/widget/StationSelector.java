package cl.rodaja.app.widget;

import java.util.List;

/**
 * Elige el id de estación en el teléfono. No arma URLs.
 * Los {@code reason} no son texto de UI.
 */
public final class StationSelector {
    public static final double FAR_KM = 200.0;
    public static final double MOVE_KM = 25.0;

    public static final String MODE_FIXED = "fixed";
    public static final String MODE_NEAREST = "nearest";

    public static final String REASON_FIXED = "fixed";
    public static final String REASON_SIN_FIX = "sin_fix";
    public static final String REASON_SIN_MOVIMIENTO = "sin_movimiento";
    public static final String REASON_VACIO = "vacio";
    public static final String REASON_LEJOS = "lejos";
    public static final String REASON_CERCANA = "cercana";

    private StationSelector() {}

    public static final class Point {
        public final long id;
        public final double lat;
        public final double lon;

        public Point(long id, double lat, double lon) {
            this.id = id;
            this.lat = lat;
            this.lon = lon;
        }
    }

    public static final class Result {
        public final long stationId;
        public final String reason;

        public Result(long stationId, String reason) {
            this.stationId = stationId;
            this.reason = reason;
        }
    }

    /**
     * @param heldStationId en {@code fixed}, el id fijado; en {@code nearest}, el último elegido
     * @param prevLat       fix guardado; null si no hay
     * @param newLat        fix nuevo; null si no hay permiso o no hay posición
     */
    public static Result choose(
            String mode,
            Long heldStationId,
            Double prevLat,
            Double prevLon,
            Double newLat,
            Double newLon,
            List<Point> catalog,
            long defaultStationId) {
        if (MODE_FIXED.equals(mode)) {
            long id = heldStationId != null ? heldStationId.longValue() : defaultStationId;
            return new Result(id, REASON_FIXED);
        }
        if (!MODE_NEAREST.equals(mode)) {
            throw new IllegalArgumentException("modo desconocido");
        }
        if (!finite(newLat) || !finite(newLon)) {
            return new Result(defaultStationId, REASON_SIN_FIX);
        }
        if (finite(prevLat) && finite(prevLon) && heldStationId != null) {
            double moved = Haversine.km(prevLat, prevLon, newLat, newLon);
            if (moved < MOVE_KM) {
                return new Result(heldStationId.longValue(), REASON_SIN_MOVIMIENTO);
            }
        }
        Point best = nearest(newLat, newLon, catalog);
        if (best == null) {
            return new Result(defaultStationId, REASON_VACIO);
        }
        double km = Haversine.km(newLat, newLon, best.lat, best.lon);
        if (km > FAR_KM) {
            return new Result(defaultStationId, REASON_LEJOS);
        }
        return new Result(best.id, REASON_CERCANA);
    }

    private static Point nearest(double lat, double lon, List<Point> catalog) {
        if (catalog == null || catalog.isEmpty()) {
            return null;
        }
        Point best = null;
        double bestKm = Double.POSITIVE_INFINITY;
        for (int i = 0; i < catalog.size(); i++) {
            Point s = catalog.get(i);
            if (s == null || !Double.isFinite(s.lat) || !Double.isFinite(s.lon)) {
                continue;
            }
            double d = Haversine.km(lat, lon, s.lat, s.lon);
            // ponytail: empate exacto de double; a igualdad gana el menor id
            if (best == null || d < bestKm || (d == bestKm && s.id < best.id)) {
                best = s;
                bestKm = d;
            }
        }
        return best;
    }

    private static boolean finite(Double v) {
        return v != null && !v.isNaN() && !v.isInfinite();
    }
}
