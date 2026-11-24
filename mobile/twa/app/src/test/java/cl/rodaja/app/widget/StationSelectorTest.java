package cl.rodaja.app.widget;

import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class StationSelectorTest {
    private static final long DEFAULT_ID = 3939L;

    private static double northOf(double lat, double km) {
        return lat + Math.toDegrees(km / Haversine.EARTH_RADIUS_KM);
    }

    private static List<StationSelector.Point> points(StationSelector.Point... pts) {
        return Arrays.asList(pts);
    }

    @Test
    public void eligeLaMasCercana() {
        List<StationSelector.Point> catalog = points(
                new StationSelector.Point(1L, northOf(0.0, 50), 0.0),
                new StationSelector.Point(2L, northOf(0.0, 10), 0.0));
        StationSelector.Result r = StationSelector.choose(
                StationSelector.MODE_NEAREST, null, null, null, 0.0, 0.0, catalog, DEFAULT_ID);
        assertEquals(2L, r.stationId);
        assertEquals(StationSelector.REASON_CERCANA, r.reason);
    }

    @Test
    public void masDe200KmUsaLaDefault() {
        List<StationSelector.Point> catalog = points(
                new StationSelector.Point(2L, northOf(0.0, 201), 0.0));
        StationSelector.Result r = StationSelector.choose(
                StationSelector.MODE_NEAREST, null, null, null, 0.0, 0.0, catalog, DEFAULT_ID);
        assertEquals(DEFAULT_ID, r.stationId);
        assertEquals(StationSelector.REASON_LEJOS, r.reason);
    }

    @Test
    public void a199KmSigueSiendoLaCercana() {
        List<StationSelector.Point> catalog = points(
                new StationSelector.Point(7L, northOf(0.0, 199), 0.0));
        StationSelector.Result r = StationSelector.choose(
                StationSelector.MODE_NEAREST, null, null, null, 0.0, 0.0, catalog, DEFAULT_ID);
        assertEquals(7L, r.stationId);
        assertEquals(StationSelector.REASON_CERCANA, r.reason);
    }

    @Test
    public void catalogoVacioUsaLaDefault() {
        StationSelector.Result r = StationSelector.choose(
                StationSelector.MODE_NEAREST, null, null, null, 0.0, 0.0,
                Collections.<StationSelector.Point>emptyList(), DEFAULT_ID);
        assertEquals(DEFAULT_ID, r.stationId);
        assertEquals(StationSelector.REASON_VACIO, r.reason);
    }

    @Test
    public void catalogoNullUsaLaDefault() {
        StationSelector.Result r = StationSelector.choose(
                StationSelector.MODE_NEAREST, 1L, null, null, 0.0, 0.0, null, DEFAULT_ID);
        assertEquals(DEFAULT_ID, r.stationId);
        assertEquals(StationSelector.REASON_VACIO, r.reason);
    }

    @Test
    public void fixedIgnoraElFixNuevo() {
        List<StationSelector.Point> catalog = points(
                new StationSelector.Point(20L, 0.0, 0.0));
        StationSelector.Result r = StationSelector.choose(
                StationSelector.MODE_FIXED, 1781L, 0.0, 0.0, 0.0, 0.0, catalog, DEFAULT_ID);
        assertEquals(1781L, r.stationId);
        assertEquals(StationSelector.REASON_FIXED, r.reason);
    }

    @Test
    public void nearestNoRecalculaBajo25Km() {
        double lat = northOf(0.0, 10);
        List<StationSelector.Point> catalog = points(
                new StationSelector.Point(10L, 0.0, 5.0),
                new StationSelector.Point(20L, lat, 0.0));
        StationSelector.Result r = StationSelector.choose(
                StationSelector.MODE_NEAREST, 10L, 0.0, 0.0, lat, 0.0, catalog, DEFAULT_ID);
        assertEquals(10L, r.stationId);
        assertEquals(StationSelector.REASON_SIN_MOVIMIENTO, r.reason);
    }

    @Test
    public void nearestRecalculaDesde25Km() {
        double lat = northOf(0.0, 30);
        List<StationSelector.Point> catalog = points(
                new StationSelector.Point(10L, 0.0, 5.0),
                new StationSelector.Point(20L, lat, 0.0));
        StationSelector.Result r = StationSelector.choose(
                StationSelector.MODE_NEAREST, 10L, 0.0, 0.0, lat, 0.0, catalog, DEFAULT_ID);
        assertEquals(20L, r.stationId);
        assertEquals(StationSelector.REASON_CERCANA, r.reason);
    }

    @Test
    public void sinFixPrevioRecalculaAunqueHayaId() {
        List<StationSelector.Point> catalog = points(
                new StationSelector.Point(20L, northOf(0.0, 5), 0.0));
        StationSelector.Result r = StationSelector.choose(
                StationSelector.MODE_NEAREST, 10L, null, null, 0.0, 0.0, catalog, DEFAULT_ID);
        assertEquals(20L, r.stationId);
        assertEquals(StationSelector.REASON_CERCANA, r.reason);
    }

    @Test
    public void sinFixNuevoUsaLaDefault() {
        List<StationSelector.Point> catalog = points(
                new StationSelector.Point(20L, 0.0, 0.0));
        StationSelector.Result r = StationSelector.choose(
                StationSelector.MODE_NEAREST, 20L, 0.0, 0.0, null, null, catalog, DEFAULT_ID);
        assertEquals(DEFAULT_ID, r.stationId);
        assertEquals(StationSelector.REASON_SIN_FIX, r.reason);
    }

    @Test
    public void empateSeQuedaConElMenorId() {
        List<StationSelector.Point> catalog = points(
                new StationSelector.Point(8L, 0.0, 1.0),
                new StationSelector.Point(3L, 0.0, -1.0));
        StationSelector.Result r = StationSelector.choose(
                StationSelector.MODE_NEAREST, null, null, null, 0.0, 0.0, catalog, DEFAULT_ID);
        assertEquals(3L, r.stationId);
        assertEquals(StationSelector.REASON_CERCANA, r.reason);
    }

    @Test
    public void modoDesconocidoFalla() {
        try {
            StationSelector.choose("gps", null, null, null, 0.0, 0.0, null, DEFAULT_ID);
            fail();
        } catch (IllegalArgumentException expected) {
            // ok
        }
    }
}
