package cl.rodaja.app.widget;

import cl.rodaja.app.R;

/**
 * Mapa de {@code ecPronosticoWmoCondition}, {@code ecPronosticoNightComposite}
 * y {@code ecPronosticoHoraPpCondition}. {@code is_day} null o distinto de 0
 * usa el ícono de día. {@code pop} &gt;= 50 cambia el ícono, no la etiqueta.
 */
public final class WmoIcon {
    private WmoIcon() {}

    public static String stem(Integer code, Integer isDay) {
        if (code == null) {
            return null;
        }
        String day = dayStem(code);
        if (day == null) {
            return null;
        }
        if (isDay != null && isDay.intValue() == 0) {
            String night = nightOverride(code);
            if (night != null) {
                return night;
            }
        }
        return day;
    }

    /** Ícono de la hora. El texto sigue siendo {@link #label}. */
    public static String stem(Integer code, Integer isDay, Integer pop) {
        String base = stem(code, isDay);
        if (base == null || pop == null || pop.intValue() < 50) {
            return base;
        }
        if (isPrecip(base)) {
            return base;
        }
        boolean night = isDay != null && isDay.intValue() == 0;
        if (isOpenSky(base)) {
            return night ? "lluvia-y-luna" : "lluvia-y-sol";
        }
        return "lluvia";
    }

    public static String label(Integer code) {
        if (code == null) {
            return "Pron\u00f3stico";
        }
        switch (code.intValue()) {
            case 0: return "Soleado";
            case 1: return "Mayormente soleado";
            case 2: return "Parcialmente soleado";
            case 3: return "Nublado";
            case 45: return "Niebla";
            case 48: return "Niebla con escarcha";
            case 51: return "Llovizna ligera";
            case 53: return "Llovizna";
            case 55: return "Llovizna intensa";
            case 56: return "Llovizna helada";
            case 57: return "Llovizna helada intensa";
            case 61: return "Lluvia ligera";
            case 63: return "Lluvia moderada";
            case 65: return "Lluvia fuerte";
            case 66: return "Lluvia helada";
            case 67: return "Lluvia helada fuerte";
            case 71: return "Nieve ligera";
            case 73: return "Nieve moderada";
            case 75: return "Nieve fuerte";
            case 77: return "Granos de nieve";
            case 80: return "Chubascos ligeros";
            case 81: return "Chubascos";
            case 82: return "Chubascos fuertes";
            case 85: return "Nevada ligera";
            case 86: return "Nevada fuerte";
            case 95: return "Tormenta";
            case 96: return "Tormenta con granizo";
            case 99: return "Tormenta con granizo fuerte";
            default: return "Pron\u00f3stico";
        }
    }

    /** 0 si el código no tiene drawable. Lo llama la UI, no las pruebas JVM. */
    public static int drawableRes(String stem) {
        if (stem == null) {
            return 0;
        }
        switch (stem) {
            case "soleado": return R.drawable.w_soleado;
            case "parcial-soleado": return R.drawable.w_parcial_soleado;
            case "parcial-nublado": return R.drawable.w_parcial_nublado;
            case "nublado": return R.drawable.w_nublado;
            case "soleado-bruma": return R.drawable.w_soleado_bruma;
            case "bruma": return R.drawable.w_bruma;
            case "llovizna": return R.drawable.w_llovizna;
            case "nieve-hielo": return R.drawable.w_nieve_hielo;
            case "lluvia": return R.drawable.w_lluvia;
            case "lluvia-fuerte": return R.drawable.w_lluvia_fuerte;
            case "nieve": return R.drawable.w_nieve;
            case "lluvia-y-sol": return R.drawable.w_lluvia_y_sol;
            case "lluvia-y-luna": return R.drawable.w_lluvia_y_luna;
            case "lluvia-y-nieve": return R.drawable.w_lluvia_y_nieve;
            case "nube-truenos": return R.drawable.w_nube_truenos;
            case "lluvia-con-granizo": return R.drawable.w_lluvia_con_granizo;
            case "luna": return R.drawable.w_luna;
            case "luna-parcial": return R.drawable.w_luna_parcial;
            case "luna-nubes": return R.drawable.w_luna_nubes;
            default: return 0;
        }
    }

    private static boolean isPrecip(String stem) {
        return "llovizna".equals(stem) || "lluvia".equals(stem) || "lluvia-fuerte".equals(stem)
                || "lluvia-y-sol".equals(stem) || "lluvia-y-luna".equals(stem)
                || "nieve".equals(stem) || "nieve-hielo".equals(stem) || "lluvia-y-nieve".equals(stem)
                || "nube-truenos".equals(stem) || "lluvia-con-granizo".equals(stem);
    }

    private static boolean isOpenSky(String stem) {
        return "soleado".equals(stem) || "parcial-soleado".equals(stem) || "parcial-nublado".equals(stem)
                || "luna".equals(stem) || "luna-parcial".equals(stem) || "luna-nubes".equals(stem);
    }

    private static String dayStem(int code) {
        switch (code) {
            case 0: return "soleado";
            case 1: return "parcial-soleado";
            case 2: return "parcial-nublado";
            case 3: return "nublado";
            case 45:
            case 48: return "soleado-bruma";
            case 51:
            case 53:
            case 55: return "llovizna";
            case 56:
            case 57:
            case 66:
            case 67: return "nieve-hielo";
            case 61:
            case 63: return "lluvia";
            case 65:
            case 81:
            case 82: return "lluvia-fuerte";
            case 71:
            case 73:
            case 75:
            case 77:
            case 85: return "nieve";
            case 80: return "lluvia-y-sol";
            case 86: return "lluvia-y-nieve";
            case 95: return "nube-truenos";
            case 96:
            case 99: return "lluvia-con-granizo";
            default: return null;
        }
    }

    private static String nightOverride(int code) {
        switch (code) {
            case 0: return "luna";
            case 1: return "luna-parcial";
            case 2: return "luna-nubes";
            case 45:
            case 48: return "bruma";
            case 80: return "lluvia-y-luna";
            default: return null;
        }
    }
}
