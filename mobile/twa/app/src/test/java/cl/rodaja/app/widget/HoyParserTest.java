package cl.rodaja.app.widget;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

public class HoyParserTest {
    @Test
    public void nullsYRankingsNoFallan() {
        String json = "{"
                + "\"station_id\":3939,"
                + "\"name\":null,"
                + "\"comuna\":null,"
                + "\"current_temp\":null,"
                + "\"tmin\":null,"
                + "\"tmax\":null,"
                + "\"precip_prob\":null,"
                + "\"precip_mm\":null,"
                + "\"condition_code\":null,"
                + "\"is_day\":null,"
                + "\"updated_at\":null,"
                + "\"rankings\":[{\"rank\":1}],"
                + "\"lat\":-33.4,"
                + "\"lon\":-70.6"
                + "}";
        Hoy hoy = HoyParser.parse(json);
        assertEquals(3939L, hoy.stationId);
        assertNull(hoy.name);
        assertNull(hoy.comuna);
        assertNull(hoy.currentTemp);
        assertNull(hoy.tmin);
        assertNull(hoy.tmax);
        assertNull(hoy.precipProb);
        assertNull(hoy.precipMm);
        assertNull(hoy.conditionCode);
        assertNull(hoy.isDay);
        assertNull(hoy.updatedAt);
    }

    @Test
    public void conditionCodeEsEntero() {
        String json = "{"
                + "\"station_id\":3939,"
                + "\"name\":\"Quinta Normal Santiago\","
                + "\"comuna\":\"Estación Central\","
                + "\"current_temp\":14.6,"
                + "\"tmin\":11.2,"
                + "\"tmax\":24.1,"
                + "\"precip_prob\":57,"
                + "\"precip_mm\":1.2,"
                + "\"condition_code\":45,"
                + "\"is_day\":0,"
                + "\"updated_at\":\"2026-11-15T07:40:30-03:00\","
                + "\"rankings\":{\"alerta\":1}"
                + "}";
        Hoy hoy = HoyParser.parse(json);
        assertEquals(3939L, hoy.stationId);
        assertEquals("Quinta Normal Santiago", hoy.name);
        assertEquals("Estación Central", hoy.comuna);
        assertEquals(14.6, hoy.currentTemp, 0.0);
        assertEquals(11.2, hoy.tmin, 0.0);
        assertEquals(24.1, hoy.tmax, 0.0);
        assertEquals(Integer.valueOf(57), hoy.precipProb);
        assertEquals(1.2, hoy.precipMm, 0.0);
        assertEquals(Integer.valueOf(45), hoy.conditionCode);
        assertEquals(Integer.valueOf(0), hoy.isDay);
        assertEquals("2026-11-15T07:40:30-03:00", hoy.updatedAt);
    }

    @Test
    public void clavesAusentesSonNull() {
        Hoy hoy = HoyParser.parse("{\"station_id\":1}");
        assertEquals(1L, hoy.stationId);
        assertNull(hoy.currentTemp);
        assertNull(hoy.conditionCode);
        assertNull(hoy.precipProb);
        assertNull(hoy.isDay);
        assertNull(hoy.tmin);
        assertNull(hoy.tmax);
        assertNull(hoy.precipMm);
        assertNull(hoy.name);
        assertNull(hoy.updatedAt);
        assertNull(hoy.hourPop);
        assertNull(hoy.alertVariable);
        assertEquals(0, hoy.hourH.length);
    }

    @Test
    public void horasYAlerta() {
        String json = "{"
                + "\"station_id\":3939,"
                + "\"hour_pop\":60,"
                + "\"hours\":[{\"h\":14,\"mm\":0.2},{\"h\":15,\"mm\":1}],"
                + "\"alert\":{\"variable\":\"pp\",\"fecha\":\"2026-11-16\",\"valor\":24,\"unidad\":\"mm\"}"
                + "}";
        Hoy hoy = HoyParser.parse(json);
        assertEquals(Integer.valueOf(60), hoy.hourPop);
        assertEquals(2, hoy.hourH.length);
        assertEquals(14, hoy.hourH[0]);
        assertEquals(0.2, hoy.hourMm[0], 0.0);
        assertEquals(15, hoy.hourH[1]);
        assertEquals("pp", hoy.alertVariable);
        assertEquals("2026-11-16", hoy.alertFecha);
        assertEquals(24.0, hoy.alertValor, 0.0);
        assertEquals("mm", hoy.alertUnidad);
    }

    @Test
    public void alertaAusente() {
        Hoy hoy = HoyParser.parse("{\"station_id\":1,\"alert\":null,\"hours\":[]}");
        assertNull(hoy.alertVariable);
        assertEquals(0, hoy.hourH.length);
    }

    @Test
    public void errorDeNegocioNoEsUnPronostico() {
        try {
            HoyParser.parse("{\"error\":\"Sin pronóstico para esta estación\",\"rankings\":[]}");
            fail();
        } catch (IllegalArgumentException expected) {
            // ok
        }
    }

    @Test
    public void jsonRotoFalla() {
        try {
            HoyParser.parse("{");
            fail();
        } catch (IllegalArgumentException expected) {
            // ok
        }
    }
}
