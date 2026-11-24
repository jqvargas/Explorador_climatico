package cl.rodaja.app.widget;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/** Qué pinta la tarjeta. No abre red ni guarda coordenadas. */
public final class WidgetFrame {
    public static final String LOADING = "loading";
    public static final String CONTENT = "content";
    public static final String OFFLINE = "offline";
    public static final String ERROR = "error";
    public static final String NO_LOCATION = "no_location";

    public static final String BANNER_LOADING = "Cargando pron\u00f3stico";
    public static final String BANNER_OFFLINE = "Sin conexi\u00f3n";
    public static final String BANNER_NO_LOCATION = "Toca para activar ubicaci\u00f3n";
    public static final String BANNER_NO_PLAY = "Sin servicios de Google. Estaci\u00f3n por defecto.";
    public static final String BANNER_ERROR = "No se pudo cargar el pron\u00f3stico";

    public final boolean numbers;
    public final String banner;
    public final String name;
    public final String comuna;
    public final String temp;
    public final String range;
    public final String precip;
    public final String hora;
    public final String iconStem;
    public final String iconLabel;
    public final long stationId;
    public final String updatedAt;
    public final Integer conditionCode;
    public final Integer isDay;
    public final Double currentTemp;
    public final Double tmin;
    public final Double tmax;
    public final Double precipMm;
    public final Integer precipProb;
    public final Integer hourPop;
    public final int[] hourH;
    public final double[] hourMm;
    public final String alertVariable;
    public final Double alertValor;

    private WidgetFrame(
            boolean numbers,
            String banner,
            String name,
            String comuna,
            String temp,
            String range,
            String precip,
            String hora,
            String iconStem,
            String iconLabel,
            long stationId,
            String updatedAt,
            Integer conditionCode,
            Integer isDay,
            Double currentTemp,
            Double tmin,
            Double tmax,
            Double precipMm,
            Integer precipProb,
            Integer hourPop,
            int[] hourH,
            double[] hourMm,
            String alertVariable,
            Double alertValor) {
        this.numbers = numbers;
        this.banner = banner;
        this.name = name;
        this.comuna = comuna;
        this.temp = temp;
        this.range = range;
        this.precip = precip;
        this.hora = hora;
        this.iconStem = iconStem;
        this.iconLabel = iconLabel;
        this.stationId = stationId;
        this.updatedAt = updatedAt;
        this.conditionCode = conditionCode;
        this.isDay = isDay;
        this.currentTemp = currentTemp;
        this.tmin = tmin;
        this.tmax = tmax;
        this.precipMm = precipMm;
        this.precipProb = precipProb;
        this.hourPop = hourPop;
        this.hourH = hourH == null ? new int[0] : hourH;
        this.hourMm = hourMm == null ? new double[0] : hourMm;
        this.alertVariable = alertVariable;
        this.alertValor = alertValor;
    }

    public static WidgetFrame frame(String state, String json) {
        if (NO_LOCATION.equals(state)) {
            return bannerOnly(BANNER_NO_LOCATION);
        }
        Hoy hoy = parseOrNull(json);
        if (hoy == null) {
            if (ERROR.equals(state)) {
                return bannerOnly(BANNER_ERROR);
            }
            if (OFFLINE.equals(state)) {
                return bannerOnly(BANNER_OFFLINE);
            }
            return bannerOnly(BANNER_LOADING);
        }
        String banner = null;
        if (OFFLINE.equals(state)) {
            banner = BANNER_OFFLINE;
        } else if (ERROR.equals(state)) {
            banner = BANNER_ERROR;
        }
        return new WidgetFrame(
                true,
                banner,
                hoy.name == null ? "" : hoy.name,
                hoy.comuna == null ? "" : hoy.comuna,
                WidgetText.temp(hoy.currentTemp),
                WidgetText.range(hoy.tmin, hoy.tmax),
                WidgetText.precip(hoy.precipProb, hoy.precipMm),
                WidgetText.hora(hoy.updatedAt),
                WmoIcon.stem(hoy.conditionCode, hoy.isDay, hoy.hourPop),
                WmoIcon.label(hoy.conditionCode),
                hoy.stationId,
                hoy.updatedAt,
                hoy.conditionCode,
                hoy.isDay,
                hoy.currentTemp,
                hoy.tmin,
                hoy.tmax,
                hoy.precipMm,
                hoy.precipProb,
                hoy.hourPop,
                hoy.hourH,
                hoy.hourMm,
                hoy.alertVariable,
                hoy.alertValor);
    }

    /** Online con caché sigue mostrando números. Sin red, el cartel pasa a sin conexión. */
    public static String stateAfterSchedule(boolean online, boolean hasCache) {
        if (!online) {
            return OFFLINE;
        }
        return hasCache ? CONTENT : LOADING;
    }

    /** Transporte roto conserva el modo sin conexión. Un HTTP que no es 200 parseado es error. */
    public static String stateAfterFetch(boolean transportFailed, int http, boolean parsed) {
        if (transportFailed) {
            return OFFLINE;
        }
        if (http == 200 && parsed) {
            return CONTENT;
        }
        return ERROR;
    }

    public static String hoyUrl(long stationId) {
        if (stationId < 1) {
            throw new IllegalArgumentException("id");
        }
        return "https://app.rodaja.cl/api/widget/estaciones/" + stationId + "/hoy";
    }

    /** Deep link del tap. Mismo host que los atajos. Sin lat/lon ni rankings. */
    public static String reportUrl(long stationId) {
        if (stationId < 1) {
            throw new IllegalArgumentException("id");
        }
        return "https://app.rodaja.cl/mi-reporte/?view=pronostico&e=" + stationId;
    }

    public static long defaultStationId(String head) {
        if (head == null) {
            throw new IllegalArgumentException("default_station_id ausente");
        }
        java.util.regex.Matcher m = java.util.regex.Pattern
                .compile("\"default_station_id\"\\s*:\\s*(\\d+)")
                .matcher(head);
        if (!m.find()) {
            throw new IllegalArgumentException("default_station_id ausente");
        }
        return Long.parseLong(m.group(1));
    }

    /** JSON canónico del último dato bueno. Descarta rankings, lat, lon y cualquier otra clave. */
    public static String cacheJson(String raw) {
        Hoy h = HoyParser.parse(raw);
        JSONObject o = new JSONObject();
        try {
            o.put("station_id", h.stationId);
            put(o, "name", h.name);
            put(o, "comuna", h.comuna);
            putNum(o, "current_temp", h.currentTemp);
            putNum(o, "tmin", h.tmin);
            putNum(o, "tmax", h.tmax);
            if (h.precipProb == null) {
                o.put("precip_prob", JSONObject.NULL);
            } else {
                o.put("precip_prob", h.precipProb.intValue());
            }
            putNum(o, "precip_mm", h.precipMm);
            if (h.conditionCode == null) {
                o.put("condition_code", JSONObject.NULL);
            } else {
                o.put("condition_code", h.conditionCode.intValue());
            }
            if (h.isDay == null) {
                o.put("is_day", JSONObject.NULL);
            } else {
                o.put("is_day", h.isDay.intValue());
            }
            if (h.hourPop == null) {
                o.put("hour_pop", JSONObject.NULL);
            } else {
                o.put("hour_pop", h.hourPop.intValue());
            }
            JSONArray hours = new JSONArray();
            int n = Math.min(h.hourH.length, h.hourMm.length);
            for (int i = 0; i < n; i++) {
                JSONObject row = new JSONObject();
                row.put("h", h.hourH[i]);
                row.put("mm", h.hourMm[i]);
                hours.put(row);
            }
            o.put("hours", hours);
            if (h.alertVariable == null) {
                o.put("alert", JSONObject.NULL);
            } else {
                JSONObject alert = new JSONObject();
                alert.put("variable", h.alertVariable);
                put(alert, "fecha", h.alertFecha);
                putNum(alert, "valor", h.alertValor);
                put(alert, "unidad", h.alertUnidad);
                o.put("alert", alert);
            }
            put(o, "updated_at", h.updatedAt);
            return o.toString();
        } catch (JSONException e) {
            throw new IllegalArgumentException("JSON de hoy inválido", e);
        }
    }

    private static WidgetFrame bannerOnly(String banner) {
        return new WidgetFrame(false, banner, "", "", "", "", "", "", null, "Pron\u00f3stico", 0L,
                null, null, null, null, null, null, null, null, null, new int[0], new double[0], null, null);
    }

    private static Hoy parseOrNull(String json) {
        if (json == null || json.length() == 0) {
            return null;
        }
        try {
            return HoyParser.parse(json);
        } catch (RuntimeException ignored) {
            return null;
        }
    }

    private static void put(JSONObject o, String key, String value) throws JSONException {
        if (value == null) {
            o.put(key, JSONObject.NULL);
        } else {
            o.put(key, value);
        }
    }

    private static void putNum(JSONObject o, String key, Double value) throws JSONException {
        if (value == null) {
            o.put(key, JSONObject.NULL);
        } else {
            o.put(key, value.doubleValue());
        }
    }
}
