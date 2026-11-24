package cl.rodaja.app.widget;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class HaversineTest {
    @Test
    public void mismoPuntoEsCero() {
        assertEquals(0.0, Haversine.km(-33.45, -70.66, -33.45, -70.66), 0.0);
    }

    @Test
    public void unGradoDeLatitud() {
        double expected = Haversine.EARTH_RADIUS_KM * Math.PI / 180.0;
        assertEquals(expected, Haversine.km(0.0, 0.0, 1.0, 0.0), 1e-6);
        assertEquals(expected, Haversine.km(-33.0, -70.0, -32.0, -70.0), 1e-6);
    }

    @Test
    public void esSimetrico() {
        assertEquals(
                Haversine.km(-33.45, -70.66, -33.05, -71.62),
                Haversine.km(-33.05, -71.62, -33.45, -70.66),
                1e-9);
    }
}
