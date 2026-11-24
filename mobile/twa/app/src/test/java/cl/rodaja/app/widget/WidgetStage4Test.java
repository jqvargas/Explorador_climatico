package cl.rodaja.app.widget;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class WidgetStage4Test {
    private static final long DEF = 3939L;

    @Test
    public void catalogoIgnoraRankingsYFilasSinCoordenada() {
        String json = "{"
                + "\"default_station_id\":3939,"
                + "\"rankings\":[{\"rank\":1}],"
                + "\"lat\":1,\"lon\":2,"
                + "\"stations\":["
                + "{\"id\":1,\"name\":\"Estaci\\u00f3n Central\",\"comuna\":\"Santiago\",\"lat\":-33.4,\"lon\":-70.6},"
                + "{\"id\":2,\"name\":\"Sin coordenada\",\"comuna\":\"X\"},"
                + "{\"name\":\"Sin id\",\"lat\":1,\"lon\":2}"
                + "]}";
        CatalogParser.Catalog cat = CatalogParser.parse(json);
        assertEquals(DEF, cat.defaultStationId);
        assertEquals(1, cat.stations.size());
        assertEquals("Estaci\u00f3n Central", cat.stations.get(0).name);
        List<CatalogParser.Station> all = CatalogParser.filter(cat.stations, "");
        assertEquals(1, all.size());
        assertEquals(1, CatalogParser.filter(cat.stations, "estacion").size());
        assertEquals(1, CatalogParser.filter(cat.stations, "SANTIAGO").size());
        assertEquals(0, CatalogParser.filter(cat.stations, "zzz").size());
    }

    @Test
    public void catalogoSinDefaultTira() {
        try {
            CatalogParser.parse("{\"stations\":[]}");
            fail();
        } catch (IllegalArgumentException expected) {
            // sin la clave
        }
    }

    @Test
    public void sinPermisoNoUsaElFix() {
        WidgetPlacement.Decision d = WidgetPlacement.decide(
                StationSelector.MODE_NEAREST,
                null,
                null,
                null,
                true,
                false,
                -33.4,
                -70.6,
                near(),
                DEF);
        assertEquals(DEF, d.stationId);
        assertEquals(StationSelector.REASON_SIN_FIX, d.reason);
        assertEquals(WidgetFrame.BANNER_NO_LOCATION, d.notice);
        assertFalse(d.replaceFix);
    }

    @Test
    public void sinPlayServicesUsaLaPorDefectoYLoDice() {
        WidgetPlacement.Decision d = WidgetPlacement.decide(
                StationSelector.MODE_NEAREST,
                null,
                null,
                null,
                false,
                false,
                0.0,
                0.0,
                near(),
                DEF);
        assertEquals(DEF, d.stationId);
        assertEquals(WidgetFrame.BANNER_NO_PLAY, d.notice);
        assertFalse(d.notice.contains("0.0"));
    }

    @Test
    public void cercanaYMovimiento() {
        WidgetPlacement.Decision first = WidgetPlacement.decide(
                StationSelector.MODE_NEAREST, null, null, null,
                true, true, 0.0, 0.0, near(), DEF);
        assertEquals(1L, first.stationId);
        assertEquals(StationSelector.REASON_CERCANA, first.reason);
        assertNull(first.notice);
        assertTrue(first.replaceFix);

        WidgetPlacement.Decision quiet = WidgetPlacement.decide(
                StationSelector.MODE_NEAREST, 1L, 0.0, 0.0,
                true, true, 0.09, 0.0, near(), DEF);
        assertEquals(1L, quiet.stationId);
        assertEquals(StationSelector.REASON_SIN_MOVIMIENTO, quiet.reason);
        assertFalse(quiet.replaceFix);

        WidgetPlacement.Decision again = WidgetPlacement.decide(
                StationSelector.MODE_NEAREST, 1L, null, null,
                true, true, 0.09, 0.0, near(), DEF);
        assertEquals(1L, again.stationId);
        assertEquals(StationSelector.REASON_CERCANA, again.reason);
        assertTrue(again.replaceFix);
    }

    @Test
    public void lejosCaeEnLaPorDefecto() {
        WidgetPlacement.Decision d = WidgetPlacement.decide(
                StationSelector.MODE_NEAREST, 1L, null, null,
                true, true, 50.0, 0.0, near(), DEF);
        assertEquals(DEF, d.stationId);
        assertEquals(StationSelector.REASON_LEJOS, d.reason);
        assertNull(d.notice);
        assertTrue(d.replaceFix);
    }

    @Test
    public void fijaNoSePisaNiPideUbicacion() {
        WidgetPlacement.Decision d = WidgetPlacement.decide(
                StationSelector.MODE_FIXED, 50L, 0.0, 0.0,
                false, false, 0.0, 0.0, near(), DEF);
        assertEquals(50L, d.stationId);
        assertEquals(StationSelector.REASON_FIXED, d.reason);
        assertNull(d.notice);
        assertFalse(d.replaceFix);
    }

    @Test
    public void pinYDesvio() {
        assertFalse(WidgetGates.shouldOfferPin(25, true, 0));
        assertTrue(WidgetGates.shouldOfferPin(26, true, 0));
        assertFalse(WidgetGates.shouldOfferPin(26, false, 0));
        assertFalse(WidgetGates.shouldOfferPin(26, true, 1));
        assertEquals(Long.valueOf(1781L), WidgetGates.stationExtra("1781"));
        assertNull(WidgetGates.stationExtra("12.5"));
        assertNull(WidgetGates.stationExtra("abc"));
        assertNull(WidgetGates.stationExtra(null));
        assertNull(WidgetGates.stationExtra("0"));
        assertNull(WidgetGates.stationExtra("-3"));
    }

    private static List<StationSelector.Point> near() {
        List<StationSelector.Point> c = new ArrayList<StationSelector.Point>();
        c.add(new StationSelector.Point(1L, 0.0, 0.0));
        return c;
    }
}
