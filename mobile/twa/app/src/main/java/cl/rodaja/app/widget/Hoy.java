package cl.rodaja.app.widget;

/** Pronóstico de hoy ya parseado. Los null son los del JSON, no un cero inventado. */
public final class Hoy {
    public final long stationId;
    public final String name;
    public final String comuna;
    public final Double currentTemp;
    public final Double tmin;
    public final Double tmax;
    public final Integer precipProb;
    public final Double precipMm;
    public final Integer conditionCode;
    public final Integer isDay;
    public final Integer hourPop;
    public final int[] hourH;
    public final double[] hourMm;
    public final String alertVariable;
    public final String alertFecha;
    public final Double alertValor;
    public final String alertUnidad;
    public final String updatedAt;

    public Hoy(
            long stationId,
            String name,
            String comuna,
            Double currentTemp,
            Double tmin,
            Double tmax,
            Integer precipProb,
            Double precipMm,
            Integer conditionCode,
            Integer isDay,
            Integer hourPop,
            int[] hourH,
            double[] hourMm,
            String alertVariable,
            String alertFecha,
            Double alertValor,
            String alertUnidad,
            String updatedAt) {
        this.stationId = stationId;
        this.name = name;
        this.comuna = comuna;
        this.currentTemp = currentTemp;
        this.tmin = tmin;
        this.tmax = tmax;
        this.precipProb = precipProb;
        this.precipMm = precipMm;
        this.conditionCode = conditionCode;
        this.isDay = isDay;
        this.hourPop = hourPop;
        this.hourH = hourH == null ? new int[0] : hourH;
        this.hourMm = hourMm == null ? new double[0] : hourMm;
        this.alertVariable = alertVariable;
        this.alertFecha = alertFecha;
        this.alertValor = alertValor;
        this.alertUnidad = alertUnidad;
        this.updatedAt = updatedAt;
    }
}
