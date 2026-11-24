package cl.rodaja.app.widget;

/** Textos de la tarjeta. Coma decimal. La hora sale del offset que ya trae el JSON. */
public final class WidgetText {
    private WidgetText() {}

    public static String temp(Double v) {
        if (v == null) {
            return "\u2014";
        }
        return decimal(v) + " \u00B0C";
    }

    public static String range(Double min, Double max) {
        return side(min) + " / " + side(max);
    }

    public static String precip(Integer prob, Double mm) {
        String p = prob == null ? null : (prob + " %");
        String m = mm == null ? null : (decimal(mm) + " mm");
        if (p == null && m == null) {
            return "";
        }
        if (p == null) {
            return m;
        }
        if (m == null) {
            return p;
        }
        return p + " \u00B7 " + m;
    }

    /** Entero, sin unidad larga. Vacío si no hay dato. */
    public static String tempEntera(Double v) {
        if (v == null) {
            return "";
        }
        return Math.round(v) + "\u00B0";
    }

    /** "↓10° ↑22°". Omite el lado que falta. */
    public static String extremos(Double min, Double max) {
        String lo = tempEntera(min);
        String hi = tempEntera(max);
        if (lo.isEmpty() && hi.isEmpty()) {
            return "";
        }
        if (lo.isEmpty()) {
            return "\u2191" + hi;
        }
        if (hi.isEmpty()) {
            return "\u2193" + lo;
        }
        return "\u2193" + lo + " \u2191" + hi;
    }

    /** "67 %". Vacío si no hay probabilidad. */
    public static String prob(Integer p) {
        if (p == null) {
            return "";
        }
        return p.intValue() + " %";
    }

    /** "Prevista · 24 mm". Vacío si no hay milímetros. */
    public static String prevista(Double mm) {
        if (mm == null) {
            return "";
        }
        return "Prevista \u00b7 " + mmCorto(mm) + " mm";
    }

    /** "0,3 mm hoy". Vacío si no hay milímetros. */
    public static String lluviaHoy(Double mm) {
        if (mm == null) {
            return "";
        }
        return decimal(mm) + " mm hoy";
    }

    /**
     * Horas enteras desde {@code updatedAt} hasta {@code nowMs}. Null si no se puede leer.
     * El reloj lo pasa la UI; aquí no se consulta la hora del teléfono.
     */
    public static Integer ageHours(String updatedAt, long nowMs) {
        Long then = epochMs(updatedAt);
        if (then == null) {
            return null;
        }
        long diff = nowMs - then.longValue();
        if (diff < 0) {
            return Integer.valueOf(0);
        }
        return Integer.valueOf((int) (diff / 3600000L));
    }

    public static boolean stale(Integer ageHours) {
        return ageHours != null && ageHours.intValue() >= 3;
    }

    /** "⟳ Hace 5 h". */
    public static String hace(int ageHours) {
        if (ageHours < 1) {
            return "\u27F3 Ahora";
        }
        return "\u27F3 Hace " + ageHours + " h";
    }

    /**
     * Titular a partir de la condición y los mm del día.
     * Sin horas del pronóstico no se dice "durante la tarde".
     */
    public static String titulo(Integer code, Double mm) {
        if (code == null) {
            return "";
        }
        switch (code.intValue()) {
            case 0:
            case 1:
                if (mm == null || mm.doubleValue() < 0.05) {
                    return "Despejado, sin lluvia prevista";
                }
                return "Mayormente despejado";
            case 2:
                return "Parcialmente nublado";
            case 3:
                return "Nublado";
            case 45:
            case 48:
                return "Niebla";
            case 51:
            case 61:
            case 80:
                return "Lluvia d\u00e9bil";
            case 53:
            case 63:
            case 81:
                return "Lluvia";
            case 55:
            case 65:
            case 82:
                return "Lluvia intensa";
            case 56:
            case 57:
            case 66:
            case 67:
                return "Lluvia helada";
            case 71:
            case 73:
            case 77:
            case 85:
                return "Nieve";
            case 75:
            case 86:
                return "Nieve intensa";
            case 95:
                return "Tormenta";
            case 96:
            case 99:
                return "Tormenta con granizo";
            default:
                return "";
        }
    }

    private static Long epochMs(String updatedAt) {
        if (updatedAt == null || updatedAt.length() < 19) {
            return null;
        }
        try {
            java.text.SimpleDateFormat fmt = new java.text.SimpleDateFormat(
                    "yyyy-MM-dd'T'HH:mm:ssXXX", java.util.Locale.US);
            fmt.setLenient(false);
            java.util.Date d = fmt.parse(updatedAt);
            return d == null ? null : Long.valueOf(d.getTime());
        } catch (java.text.ParseException e) {
            return null;
        }
    }

    /** HH:mm de un ISO con offset. Vacío si no viene. */
    public static String hora(String updatedAt) {
        if (updatedAt == null) {
            return "";
        }
        int t = updatedAt.indexOf('T');
        if (t < 0 || updatedAt.length() < t + 6) {
            return "";
        }
        String hhmm = updatedAt.substring(t + 1, t + 6);
        if (hhmm.charAt(2) != ':') {
            return "";
        }
        return hhmm;
    }

    private static String side(Double v) {
        if (v == null) {
            return "\u2014";
        }
        return decimal(v) + "\u00B0";
    }

    /** "24" si es entero; "0,3" si hay décimas. */
    private static String mmCorto(double v) {
        String d = decimal(v);
        if (d.endsWith(",0")) {
            return d.substring(0, d.length() - 2);
        }
        return d;
    }

    static String decimal(double v) {
        long tenths = Math.round(v * 10.0);
        boolean neg = tenths < 0;
        long abs = Math.abs(tenths);
        return (neg ? "-" : "") + (abs / 10) + "," + (abs % 10);
    }

    /**
     * Techo del gráfico: max(pico, 1 mm/h). Un 0,1 no puede llenar el eje.
     */
    public static double techo(double[] mm) {
        double peak = 0;
        if (mm != null) {
            for (int i = 0; i < mm.length; i++) {
                if (mm[i] > peak) {
                    peak = mm[i];
                }
            }
        }
        return Math.max(peak, 1.0);
    }

    /** Índice de la hora más lluviosa. -1 si no llega a 0,05 mm/h. */
    public static int picoIndex(double[] mm) {
        if (mm == null || mm.length == 0) {
            return -1;
        }
        int at = -1;
        double peak = 0;
        for (int i = 0; i < mm.length; i++) {
            if (mm[i] > peak) {
                peak = mm[i];
                at = i;
            }
        }
        if (at < 0 || peak < 0.05) {
            return -1;
        }
        return at;
    }

    /** "0,1" sobre la barra pico. Vacío bajo 0,05. */
    public static String pico(double mm) {
        if (mm < 0.05) {
            return "";
        }
        return mmCorto(mm);
    }

    /**
     * Si las próximas horas traen lluvia, el titular es esa hora.
     * Bajo 1 mm/h es débil; desde 4 mm/h es intensa.
     */
    public static String proximaLluvia(int[] hours, double[] mm, String condicion) {
        int at = picoIndex(mm);
        if (at < 0 || hours == null || at >= hours.length) {
            return condicion == null ? "" : condicion;
        }
        double peak = mm[at];
        String kind = peak < 1.0 ? "Lluvia d\u00e9bil" : (peak < 4.0 ? "Lluvia" : "Lluvia intensa");
        return kind + " a las " + hours[at] + ":00";
    }

    /**
     * Pie de hora. El pronóstico se publica una vez al día: si el widget
     * sí sincronizó y el origen sigue viejo, no se dice "Hace N h".
     */
    public static Reloj reloj(String sourceAt, long syncedAtMs, long nowMs) {
        Integer sourceAge = ageHours(sourceAt, nowMs);
        boolean sourceOld = stale(sourceAge);
        String hhmm = hora(sourceAt);
        if (syncedAtMs > 0L) {
            int syncAge = ageHoursMs(syncedAtMs, nowMs);
            if (sourceOld && !stale(Integer.valueOf(syncAge))) {
                String full = hhmm.isEmpty()
                        ? "Estaci\u00f3n sin reporte"
                        : "Estaci\u00f3n sin reporte desde " + hhmm;
                String compact = hhmm.isEmpty() ? "Sin reporte" : "Desde " + hhmm;
                return new Reloj(full, compact, true, true);
            }
            if (stale(Integer.valueOf(syncAge))) {
                String hace = hace(syncAge);
                return new Reloj(hace, hace, true, false);
            }
        } else if (sourceOld && sourceAge != null) {
            String hace = hace(sourceAge.intValue());
            return new Reloj(hace, hace, true, false);
        }
        return new Reloj(hhmm, hhmm, false, false);
    }

    private static int ageHoursMs(long then, long now) {
        long diff = now - then;
        if (diff < 0L) {
            return 0;
        }
        return (int) (diff / 3600000L);
    }

    public static final class Reloj {
        public final String text;
        public final String compact;
        public final boolean warn;
        public final boolean source;

        public Reloj(String text, String compact, boolean warn, boolean source) {
            this.text = text;
            this.compact = compact;
            this.warn = warn;
            this.source = source;
        }
    }
}
