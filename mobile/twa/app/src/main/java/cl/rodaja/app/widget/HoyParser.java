package cl.rodaja.app.widget;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/** Parsea el objeto de {@code GET /widget/estaciones/<id>/hoy}. Ignora rankings, lat y lon. */
public final class HoyParser {
    private HoyParser() {}

    public static Hoy parse(String json) {
        if (json == null) {
            throw new IllegalArgumentException("JSON de hoy inválido");
        }
        try {
            JSONObject o = new JSONObject(json);
            if (!o.has("station_id") || o.isNull("station_id")) {
                throw new IllegalArgumentException("station_id ausente");
            }
            Object id = o.get("station_id");
            if (!(id instanceof Number)) {
                throw new IllegalArgumentException("station_id no es entero");
            }
            Hours hours = hours(o);
            Alert alert = alert(o);
            return new Hoy(
                    ((Number) id).longValue(),
                    text(o, "name"),
                    text(o, "comuna"),
                    number(o, "current_temp"),
                    number(o, "tmin"),
                    number(o, "tmax"),
                    integer(o, "precip_prob"),
                    number(o, "precip_mm"),
                    integer(o, "condition_code"),
                    integer(o, "is_day"),
                    integer(o, "hour_pop"),
                    hours.h,
                    hours.mm,
                    alert.variable,
                    alert.fecha,
                    alert.valor,
                    alert.unidad,
                    text(o, "updated_at"));
        } catch (JSONException e) {
            throw new IllegalArgumentException("JSON de hoy inválido", e);
        }
    }

    private static Hours hours(JSONObject o) throws JSONException {
        if (!o.has("hours") || o.isNull("hours") || !(o.get("hours") instanceof JSONArray)) {
            return new Hours(new int[0], new double[0]);
        }
        JSONArray arr = o.getJSONArray("hours");
        int n = 0;
        int[] h = new int[arr.length()];
        double[] mm = new double[arr.length()];
        for (int i = 0; i < arr.length(); i++) {
            if (!(arr.get(i) instanceof JSONObject)) {
                continue;
            }
            JSONObject row = arr.getJSONObject(i);
            Integer hour = integer(row, "h");
            Double rain = number(row, "mm");
            if (hour == null || rain == null) {
                continue;
            }
            h[n] = hour.intValue();
            mm[n] = rain.doubleValue();
            n++;
        }
        return new Hours(java.util.Arrays.copyOf(h, n), java.util.Arrays.copyOf(mm, n));
    }

    private static Alert alert(JSONObject o) throws JSONException {
        if (!o.has("alert") || o.isNull("alert") || !(o.get("alert") instanceof JSONObject)) {
            return new Alert(null, null, null, null);
        }
        JSONObject a = o.getJSONObject("alert");
        return new Alert(text(a, "variable"), text(a, "fecha"), number(a, "valor"), text(a, "unidad"));
    }

    private static final class Hours {
        final int[] h;
        final double[] mm;

        Hours(int[] h, double[] mm) {
            this.h = h;
            this.mm = mm;
        }
    }

    private static final class Alert {
        final String variable;
        final String fecha;
        final Double valor;
        final String unidad;

        Alert(String variable, String fecha, Double valor, String unidad) {
            this.variable = variable;
            this.fecha = fecha;
            this.valor = valor;
            this.unidad = unidad;
        }
    }

    private static String text(JSONObject o, String key) throws JSONException {
        if (!o.has(key) || o.isNull(key)) {
            return null;
        }
        Object v = o.get(key);
        if (!(v instanceof String)) {
            throw new IllegalArgumentException(key + " no es texto");
        }
        return (String) v;
    }

    private static Double number(JSONObject o, String key) throws JSONException {
        if (!o.has(key) || o.isNull(key)) {
            return null;
        }
        Object v = o.get(key);
        if (!(v instanceof Number)) {
            throw new IllegalArgumentException(key + " no es número");
        }
        return ((Number) v).doubleValue();
    }

    private static Integer integer(JSONObject o, String key) throws JSONException {
        if (!o.has(key) || o.isNull(key)) {
            return null;
        }
        Object v = o.get(key);
        if (!(v instanceof Number)) {
            throw new IllegalArgumentException(key + " no es entero");
        }
        return ((Number) v).intValue();
    }
}
