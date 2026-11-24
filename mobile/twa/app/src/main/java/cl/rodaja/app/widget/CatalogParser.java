package cl.rodaja.app.widget;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.text.Normalizer;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/** Catálogo del asset. No abre red. */
public final class CatalogParser {
    public static final class Station {
        public final long id;
        public final String name;
        public final String comuna;
        public final double lat;
        public final double lon;

        public Station(long id, String name, String comuna, double lat, double lon) {
            this.id = id;
            this.name = name;
            this.comuna = comuna;
            this.lat = lat;
            this.lon = lon;
        }
    }

    public static final class Catalog {
        public final long defaultStationId;
        public final List<Station> stations;

        public Catalog(long defaultStationId, List<Station> stations) {
            this.defaultStationId = defaultStationId;
            this.stations = stations;
        }
    }

    private CatalogParser() {}

    public static Catalog parse(String json) {
        try {
            return parseChecked(json);
        } catch (JSONException e) {
            throw new IllegalArgumentException("catalogo", e);
        }
    }

    private static Catalog parseChecked(String json) throws JSONException {
        if (json == null) {
            throw new IllegalArgumentException("default_station_id ausente");
        }
        JSONObject root = new JSONObject(json);
        if (!root.has("default_station_id") || root.isNull("default_station_id")) {
            throw new IllegalArgumentException("default_station_id ausente");
        }
        long def = root.getLong("default_station_id");
        if (def < 1) {
            throw new IllegalArgumentException("default_station_id ausente");
        }
        JSONArray arr = root.optJSONArray("stations");
        List<Station> stations = new ArrayList<Station>();
        if (arr != null) {
            for (int i = 0; i < arr.length(); i++) {
                JSONObject o = arr.optJSONObject(i);
                if (o == null || !o.has("id") || o.isNull("id")) {
                    continue;
                }
                if (!o.has("lat") || o.isNull("lat") || !o.has("lon") || o.isNull("lon")) {
                    continue;
                }
                double lat = o.getDouble("lat");
                double lon = o.getDouble("lon");
                if (!finite(lat) || !finite(lon)) {
                    continue;
                }
                long id = o.getLong("id");
                if (id < 1) {
                    continue;
                }
                stations.add(new Station(id, text(o, "name"), text(o, "comuna"), lat, lon));
            }
        }
        return new Catalog(def, stations);
    }

    public static List<StationSelector.Point> points(List<Station> stations) {
        List<StationSelector.Point> out = new ArrayList<StationSelector.Point>();
        if (stations == null) {
            return out;
        }
        for (int i = 0; i < stations.size(); i++) {
            Station s = stations.get(i);
            if (s != null) {
                out.add(new StationSelector.Point(s.id, s.lat, s.lon));
            }
        }
        return out;
    }

    /** ponytail: recorre la lista entera en cada tecla. Con ~2000 filas sobra. */
    public static List<Station> filter(List<Station> stations, String query) {
        List<Station> out = new ArrayList<Station>();
        if (stations == null) {
            return out;
        }
        String q = fold(query);
        for (int i = 0; i < stations.size(); i++) {
            Station s = stations.get(i);
            if (s == null) {
                continue;
            }
            if (q.isEmpty() || fold(s.name).contains(q) || fold(s.comuna).contains(q)) {
                out.add(s);
            }
        }
        return out;
    }

    static String fold(String raw) {
        if (raw == null || raw.isEmpty()) {
            return "";
        }
        String n = Normalizer.normalize(raw, Normalizer.Form.NFD);
        StringBuilder b = new StringBuilder(n.length());
        for (int i = 0; i < n.length(); i++) {
            char c = n.charAt(i);
            if (Character.getType(c) != Character.NON_SPACING_MARK) {
                b.append(c);
            }
        }
        return b.toString().toLowerCase(Locale.ROOT);
    }

    private static String text(JSONObject o, String key) {
        if (!o.has(key) || o.isNull(key)) {
            return "";
        }
        return o.optString(key, "");
    }

    private static boolean finite(double v) {
        return !Double.isNaN(v) && !Double.isInfinite(v);
    }
}
