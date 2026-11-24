package cl.rodaja.app.widget;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class WidgetLogicTest {
    private static final String HOY = "{"
            + "\"station_id\":3939,"
            + "\"name\":\"Quinta Normal Santiago\","
            + "\"comuna\":\"Estaci\u00f3n Central\","
            + "\"current_temp\":14.6,"
            + "\"tmin\":11.2,"
            + "\"tmax\":24.1,"
            + "\"precip_prob\":57,"
            + "\"precip_mm\":1.2,"
            + "\"condition_code\":45,"
            + "\"is_day\":1,"
            + "\"updated_at\":\"2026-11-15T07:40:30-03:00\","
            + "\"rankings\":[{\"rank\":1}],"
            + "\"lat\":-33.4,"
            + "\"lon\":-70.6"
            + "}";

    @Test
    public void tarjetaDeHoy() {
        WidgetFrame f = WidgetFrame.frame(WidgetFrame.CONTENT, HOY);
        assertTrue(f.numbers);
        assertNull(f.banner);
        assertEquals(3939L, f.stationId);
        assertEquals("Quinta Normal Santiago", f.name);
        assertEquals("Estaci\u00f3n Central", f.comuna);
        assertEquals("14,6 \u00B0C", f.temp);
        assertEquals("11,2\u00B0 / 24,1\u00B0", f.range);
        assertEquals("57 % \u00B7 1,2 mm", f.precip);
        assertEquals("07:40", f.hora);
        assertEquals("soleado-bruma", f.iconStem);
        assertEquals("Niebla", f.iconLabel);
    }

    @Test
    public void temperaturaNulaEsRaya() {
        String json = HOY.replace("\"current_temp\":14.6", "\"current_temp\":null");
        WidgetFrame f = WidgetFrame.frame(WidgetFrame.CONTENT, json);
        assertEquals("\u2014", f.temp);
        assertEquals("11,2\u00B0 / 24,1\u00B0", f.range);
    }

    @Test
    public void estados() {
        WidgetFrame loading = WidgetFrame.frame(WidgetFrame.LOADING, null);
        assertFalse(loading.numbers);
        assertEquals(WidgetFrame.BANNER_LOADING, loading.banner);

        WidgetFrame offlineEmpty = WidgetFrame.frame(WidgetFrame.OFFLINE, null);
        assertFalse(offlineEmpty.numbers);
        assertEquals(WidgetFrame.BANNER_OFFLINE, offlineEmpty.banner);

        WidgetFrame offline = WidgetFrame.frame(WidgetFrame.OFFLINE, HOY);
        assertTrue(offline.numbers);
        assertEquals(WidgetFrame.BANNER_OFFLINE, offline.banner);
        assertEquals("07:40", offline.hora);
        assertEquals("14,6 \u00B0C", offline.temp);

        WidgetFrame error = WidgetFrame.frame(WidgetFrame.ERROR, null);
        assertFalse(error.numbers);
        assertEquals(WidgetFrame.BANNER_ERROR, error.banner);

        WidgetFrame errorConCache = WidgetFrame.frame(WidgetFrame.ERROR, HOY);
        assertTrue(errorConCache.numbers);
        assertEquals(WidgetFrame.BANNER_ERROR, errorConCache.banner);

        WidgetFrame ubicacion = WidgetFrame.frame(WidgetFrame.NO_LOCATION, HOY);
        assertFalse(ubicacion.numbers);
        assertEquals(WidgetFrame.BANNER_NO_LOCATION, ubicacion.banner);
    }

    @Test
    public void transiciones() {
        assertEquals(WidgetFrame.OFFLINE, WidgetFrame.stateAfterSchedule(false, true));
        assertEquals(WidgetFrame.OFFLINE, WidgetFrame.stateAfterSchedule(false, false));
        assertEquals(WidgetFrame.CONTENT, WidgetFrame.stateAfterSchedule(true, true));
        assertEquals(WidgetFrame.LOADING, WidgetFrame.stateAfterSchedule(true, false));

        assertEquals(WidgetFrame.OFFLINE, WidgetFrame.stateAfterFetch(true, 0, false));
        assertEquals(WidgetFrame.CONTENT, WidgetFrame.stateAfterFetch(false, 200, true));
        assertEquals(WidgetFrame.ERROR, WidgetFrame.stateAfterFetch(false, 200, false));
        assertEquals(WidgetFrame.ERROR, WidgetFrame.stateAfterFetch(false, 404, false));
        assertEquals(WidgetFrame.ERROR, WidgetFrame.stateAfterFetch(false, 500, false));
    }

    @Test
    public void reporteNoLlevaCoordenadas() {
        assertEquals(
                "https://app.rodaja.cl/mi-reporte/?view=pronostico&e=3939",
                WidgetFrame.reportUrl(3939));
        String url = WidgetFrame.reportUrl(1781);
        assertTrue(url.contains("view=pronostico"));
        assertTrue(url.contains("e=1781"));
        assertFalse(url.contains("lat"));
        assertFalse(url.contains("lon"));
        assertFalse(url.contains("rank"));
        try {
            WidgetFrame.reportUrl(0);
            fail();
        } catch (IllegalArgumentException expected) {
            // id no positivo
        }
    }

    @Test
    public void urlSoloLlevaElId() {
        assertEquals(
                "https://app.rodaja.cl/api/widget/estaciones/3939/hoy",
                WidgetFrame.hoyUrl(3939));
        String url = WidgetFrame.hoyUrl(1);
        assertFalse(url.contains("lat"));
        assertFalse(url.contains("lon"));
        assertFalse(url.contains("?"));
        try {
            WidgetFrame.hoyUrl(0);
            fail();
        } catch (IllegalArgumentException expected) {
            // id no positivo
        }
    }

    @Test
    public void estacionPorDefectoSaleDelEncabezado() {
        assertEquals(3939L, WidgetFrame.defaultStationId("{\n  \"default_station_id\": 3939,\n"));
        try {
            WidgetFrame.defaultStationId("{\"stations\":[]}");
            fail();
        } catch (IllegalArgumentException expected) {
            // sin la clave
        }
    }

    @Test
    public void cacheDescartaRankingsYCoordenadas() {
        String cached = WidgetFrame.cacheJson(HOY);
        assertFalse(cached.contains("rankings"));
        assertFalse(cached.contains("\"lat\""));
        assertFalse(cached.contains("\"lon\""));
        WidgetFrame f = WidgetFrame.frame(WidgetFrame.CONTENT, cached);
        assertEquals(3939L, HoyParser.parse(cached).stationId);
        assertEquals("14,6 \u00B0C", f.temp);
        String night = "{"
                + "\"station_id\":3939,\"name\":\"Q\",\"comuna\":\"\","
                + "\"current_temp\":1,\"tmin\":0,\"tmax\":2,"
                + "\"precip_prob\":null,\"precip_mm\":null,"
                + "\"condition_code\":45,\"is_day\":0,"
                + "\"updated_at\":\"2026-11-15T22:10:00-03:00\"}";
        WidgetFrame noche = WidgetFrame.frame(WidgetFrame.CONTENT, night);
        assertEquals("bruma", noche.iconStem);
        assertEquals("22:10", noche.hora);
    }

    @Test
    public void mapaWmoDiaYNoche() {
        assertEquals("soleado", WmoIcon.stem(0, 1));
        assertEquals("luna", WmoIcon.stem(0, 0));
        assertEquals("parcial-soleado", WmoIcon.stem(1, null));
        assertEquals("luna-parcial", WmoIcon.stem(1, 0));
        assertEquals("parcial-nublado", WmoIcon.stem(2, 1));
        assertEquals("luna-nubes", WmoIcon.stem(2, 0));
        assertEquals("nublado", WmoIcon.stem(3, 0));
        assertEquals("soleado-bruma", WmoIcon.stem(45, 1));
        assertEquals("soleado-bruma", WmoIcon.stem(48, null));
        assertEquals("bruma", WmoIcon.stem(45, 0));
        assertEquals("bruma", WmoIcon.stem(48, 0));
        assertEquals("llovizna", WmoIcon.stem(51, 0));
        assertEquals("llovizna", WmoIcon.stem(53, 1));
        assertEquals("llovizna", WmoIcon.stem(55, 0));
        assertEquals("nieve-hielo", WmoIcon.stem(56, 0));
        assertEquals("nieve-hielo", WmoIcon.stem(67, 1));
        assertEquals("lluvia", WmoIcon.stem(61, 0));
        assertEquals("lluvia", WmoIcon.stem(63, 1));
        assertEquals("lluvia-fuerte", WmoIcon.stem(65, 0));
        assertEquals("lluvia-fuerte", WmoIcon.stem(81, 1));
        assertEquals("lluvia-fuerte", WmoIcon.stem(82, 0));
        assertEquals("nieve", WmoIcon.stem(71, 0));
        assertEquals("nieve", WmoIcon.stem(85, 1));
        assertEquals("lluvia-y-sol", WmoIcon.stem(80, 1));
        assertEquals("lluvia-y-luna", WmoIcon.stem(80, 0));
        assertEquals("lluvia-y-nieve", WmoIcon.stem(86, 0));
        assertEquals("nube-truenos", WmoIcon.stem(95, 0));
        assertEquals("lluvia-con-granizo", WmoIcon.stem(96, 1));
        assertEquals("lluvia-con-granizo", WmoIcon.stem(99, 0));
        assertNull(WmoIcon.stem(null, 0));
        assertNull(WmoIcon.stem(4, 1));
        assertEquals("lluvia-y-sol", WmoIcon.stem(0, 1, 50));
        assertEquals("lluvia-y-luna", WmoIcon.stem(0, 0, 80));
        assertEquals("soleado", WmoIcon.stem(0, 1, 49));
        assertEquals("lluvia", WmoIcon.stem(61, 1, 90));
        assertEquals("lluvia", WmoIcon.stem(3, 1, 50));
        assertEquals("Nublado", WmoIcon.label(3));
        assertEquals("67 %", WidgetText.prob(67));
        assertEquals("", WidgetText.prob(null));
        assertEquals("Prevista \u00b7 24 mm", WidgetText.prevista(24.0));
        assertEquals("", WidgetText.precip(null, null));
        assertEquals("40 %", WidgetText.precip(40, null));
        assertEquals("0,0 mm", WidgetText.precip(null, 0.0));
        assertEquals("", WidgetText.hora(null));
        assertEquals("", WidgetText.hora("sin-hora"));
        assertEquals("18\u00b0", WidgetText.tempEntera(18.2));
        assertEquals("", WidgetText.tempEntera(null));
        assertEquals("\u219310\u00b0 \u219122\u00b0", WidgetText.extremos(10.2, 21.6));
        assertEquals("0,3 mm hoy", WidgetText.lluviaHoy(0.3));
        assertEquals("", WidgetText.lluviaHoy(null));
        assertEquals("Despejado, sin lluvia prevista", WidgetText.titulo(0, 0.0));
        assertEquals("Lluvia d\u00e9bil", WidgetText.titulo(61, 0.3));
        assertEquals("Lluvia intensa", WidgetText.titulo(65, 12.0));
        assertEquals("", WidgetText.titulo(null, null));
        long noon = java.time.OffsetDateTime.parse("2026-11-16T12:00:00-03:00").toInstant().toEpochMilli();
        Integer zero = WidgetText.ageHours("2026-11-16T12:00:00-03:00", noon);
        Integer five = WidgetText.ageHours("2026-11-16T07:00:00-03:00", noon);
        assertEquals(Integer.valueOf(0), zero);
        assertEquals(Integer.valueOf(5), five);
        assertTrue(WidgetText.stale(five));
        assertFalse(WidgetText.stale(zero));
        assertEquals("\u27f3 Hace 5 h", WidgetText.hace(5));
        assertEquals(1.0, WidgetText.techo(new double[] {0, 0.1, 0}), 0.001);
        assertEquals(3.0, WidgetText.techo(new double[] {0, 3, 1}), 0.001);
        assertEquals(4, WidgetText.picoIndex(new double[] {0, 0, 0, 0, 0.1, 0}));
        assertEquals(-1, WidgetText.picoIndex(new double[] {0, 0}));
        assertEquals("0,1", WidgetText.pico(0.1));
        int[] horas = new int[] {15, 16, 17, 18, 19, 20};
        assertEquals(
                "Lluvia d\u00e9bil a las 19:00",
                WidgetText.proximaLluvia(horas, new double[] {0, 0, 0, 0, 0.1, 0}, "Parcialmente soleado"));
        assertEquals(
                "Parcialmente soleado",
                WidgetText.proximaLluvia(horas, new double[] {0, 0, 0, 0, 0, 0}, "Parcialmente soleado"));
        assertEquals(
                "Lluvia intensa a las 16:00",
                WidgetText.proximaLluvia(horas, new double[] {0, 5, 0, 0, 0, 0}, "Nublado"));
        long tarde = java.time.OffsetDateTime.parse("2026-11-16T15:19:00-03:00").toInstant().toEpochMilli();
        long sync = java.time.OffsetDateTime.parse("2026-11-16T15:00:00-03:00").toInstant().toEpochMilli();
        WidgetText.Reloj origen = WidgetText.reloj("2026-11-16T07:32:20-03:00", sync, tarde);
        assertEquals("Estaci\u00f3n sin reporte desde 07:32", origen.text);
        assertEquals("Desde 07:32", origen.compact);
        assertTrue(origen.source);
        long viejo = java.time.OffsetDateTime.parse("2026-11-16T08:00:00-03:00").toInstant().toEpochMilli();
        WidgetText.Reloj widget = WidgetText.reloj("2026-11-16T07:32:20-03:00", viejo, tarde);
        assertEquals("\u27f3 Hace 7 h", widget.compact);
        assertFalse(widget.source);
    }
}
