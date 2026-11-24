# Contrato — API del widget de pronóstico

Medido en `https://app.rodaja.cl` el **2026-11-15** (hora civil Chile, UTC−3). Sin login. Nginx pone la API key; el teléfono no manda coordenadas ni token.

Los números cambian con el pronóstico del día. La forma no.

## `GET /api/widget/estaciones`

Array JSON. Solo estaciones en el bbox de Chile (`lat` −56…−17, `lon` −76…−66), calidad `excelente` o `buena`, con `t_max` de hoy. 1975 filas en esa fecha.

`Cache-Control: public, max-age=3600`. Redis 3600 s (`X-Cache: MISS` la primera, `HIT` después).

```json
[
  {
    "id": 1,
    "name": "Visviri",
    "comuna": "General Lagos",
    "lat": -17.595,
    "lon": -69.47666667
  }
]
```

| Campo | Tipo | Notas |
|-------|------|--------|
| `id` | entero | El único dato que el teléfono le devuelve al servidor. |
| `name` | string | `estacion_nombre_view(...)$name`. Sin la comuna entre paréntesis. |
| `comuna` | string | `nom_com`. Vacío si no hay. |
| `lat`, `lon` | número | Para el haversine en el teléfono. No se reenvían. |

No trae `rankings`, viento ni `station_id`.

La estación por defecto **no** viene en este array. Está en el asset `mobile/twa/app/src/main/assets/estaciones_pronostico.json`:

```json
{
  "default_station_id": 3939,
  "stations": [ ]
}
```

3939 es la DMC `Quinta_normal_santiago_DMC`: nombre «Quinta Normal Santiago», comuna «Estación Central», calidad excelente, con pronóstico de hoy. Es la única DMC cuyo nombre contiene «Quinta Normal». El 1781 del plan maestro es otra estación.

## `GET /api/widget/estaciones/<id>/hoy`

Objeto de hoy. `<id>` es un entero ≥ 1, solo dígitos. Si no: **400** `{"error":"id inválido"}` y `Cache-Control: no-store`. No entra a Redis.

Si la estación no es visible o no tiene `t_max` hoy: **404** `{"error":"Sin pronóstico para esta estación"}`, `no-store`, y Redis no lo guarda (dos llamadas seguidas siguen en `X-Cache: MISS`).

**200** con `Cache-Control: public, max-age=600` (Redis 600 s). Ejemplo real de 3939, ~08:42 hora de Chile:

```json
{
  "station_id": 3939,
  "name": "Quinta Normal Santiago",
  "comuna": "Estación Central",
  "current_temp": 14.6,
  "tmin": 11.2,
  "tmax": 24.1,
  "precip_prob": 57,
  "precip_mm": 1.2,
  "condition_code": 45,
  "is_day": 1,
  "updated_at": "2026-11-15T07:40:30-03:00"
}
```

| Campo | Tipo | Origen |
|-------|------|--------|
| `station_id` | entero | El id pedido. |
| `name`, `comuna` | string | Igual que el catálogo. |
| `current_temp` | número o null | `t_2m` de la hora cuyo intervalo `[ts, ts+1h)` contiene el instante actual. No es la temperatura observada. No se estira al mín/máx del día (eso solo lo hace el gráfico del pronóstico gordo). |
| `tmin`, `tmax`, `precip_mm` | número o null | `t_min`, `t_max`, `pp` del hoy civil, vía `display_valor`. En estas variables es identidad. |
| `precip_prob` | entero 0–100 o null | Máximo `pop` de las horas de hoy. En el ejemplo la hora actual tenía pop 20 y el máximo del día era 57. |
| `condition_code` | entero o null | `weather_code` de esa misma hora. |
| `is_day` | 0, 1 o null | De esa hora. |
| `updated_at` | string o null | `fetched_at` del día en `America/Santiago`, con offset `-03:00` o `-04:00`. |

Sin `lat`, `lon`, `rankings` ni viento. Si no hay fila horaria para «ahora», `current_temp`, `condition_code` e `is_day` van `null` y se siguen mostrando mín/máx.

## `condition_code` → archivo SVG

Mapa de `ecPronosticoWmoCondition` (composite) y `ecPronosticoNightComposite`. Archivos en `nginx/html/icons/weather/`. Con `is_day = 0` se usa la columna de noche cuando cambia; si no, el de día. `is_day` null usa el de día. Un código que no está en la tabla no tiene ícono. No se aplica el refuerzo de `pop ≥ 50` del gráfico horario.

| Código | Día | Noche (`is_day = 0`) |
|--------|-----|----------------------|
| 0 | `soleado.svg` | `luna.svg` |
| 1 | `parcial-soleado.svg` | `luna-parcial.svg` |
| 2 | `parcial-nublado.svg` | `luna-nubes.svg` |
| 3 | `nublado.svg` | `nublado.svg` |
| 45, 48 | `soleado-bruma.svg` | `bruma.svg` |
| 51, 53, 55 | `llovizna.svg` | `llovizna.svg` |
| 56, 57, 66, 67 | `nieve-hielo.svg` | `nieve-hielo.svg` |
| 61, 63 | `lluvia.svg` | `lluvia.svg` |
| 65, 81, 82 | `lluvia-fuerte.svg` | `lluvia-fuerte.svg` |
| 71, 73, 75, 77, 85 | `nieve.svg` | `nieve.svg` |
| 80 | `lluvia-y-sol.svg` | `lluvia-y-luna.svg` |
| 86 | `lluvia-y-nieve.svg` | `lluvia-y-nieve.svg` |
| 95 | `nube-truenos.svg` | `nube-truenos.svg` |
| 96, 99 | `lluvia-con-granizo.svg` | `lluvia-con-granizo.svg` |

El 45 del ejemplo (niebla, de día) es `soleado-bruma.svg`.
