# Plan de implementación — etapa 1 `api_hoy`

**No ejecutar código de producto hasta que este archivo exista y pase el self-check de abajo.**
Contrato: `docs/producto/widget_pronostico_PROMPT_FIJO.md`. Master: `docs/producto/PLAN_WIDGET_PRONOSTICO.md`.

## 1. Alcance de esta etapa (una frase)

Dos GET públicos y livianos (`/widget/estaciones` y `/widget/estaciones/<id>/hoy`) más el JSON de respaldo con `default_station_id`, sin login, sin rankings y sin coordenadas en el pronóstico de hoy.

## 2. Auditoría: qué se leyó

Solo la lectura mínima del master. No reauditar el repo.

| Ruta | Qué se confirmó |
|------|-----------------|
| `PLAN_WIDGET_PRONOSTICO.md` §2–4 | Catálogo = bbox Chile + calidad excelente/buena + fila de hoy de una sola variable. Hoy sin rankings ni lat/lon. Default = DMC cuyo nombre público contiene «Quinta Normal», con pronóstico. Si no existe, bloquear. |
| `api/plumber.R` | `endpoint_files` concatena los endpoints. El header ya carga `cache.R`, `unidades_display.R`, `estacion_label.R` y `pronostico_payload.R`. Auth anónimo sigue; no hace falta `require_auth`. |
| `api/endpoints/pronostico.R` | `GET /estaciones/<id>/pronostico` no llama `require_auth`. Cache con `cache_or_compute`. 400 si el id no es entero. El gordo no se reutiliza: trae varios días, rankings y lat/lon. |
| `api/utils/pronostico_payload.R` | `pronostico_estacion_visible` = nivel excelente/buena y `anios_span` ≥ `pronostico_min_anios_display()` (hoy 0). `SQL_HOY_CL` es el hoy civil de Chile. `display_valor` ya se usa para los chips. `pronostico_ts_iso_santiago` formatea con offset. `pronostico_json_encode` es el serializer. |
| `api/utils/cache.R` | `cache_or_compute` no guarda listas con `$error`. `encode` cachea el JSON ya serializado. El helper pone `X-Cache`, no `Cache-Control`. |
| `api/scripts/smoke_pronostico_payload.R` | Smoke con `stopifnot`, sources bajo `/app` o `API_ROOT`, se corre con `Rscript` en el contenedor. |
| `api/utils/unidades_display.R` | `display_valor(valor, nombre_abr)` convierte solo viento. Para `t_min`, `t_max`, `pp` y `t_2m` es identidad. |
| `nginx/html/mi-reporte/reporte_pronostico.js` (solo el mapa que pide el DoD) | `ecPronosticoWmoCondition` → composite; `ecPronosticoNightComposite` cambia el archivo cuando `is_day = 0`. Los SVG de esos composites están en `nginx/html/icons/weather/`. |

## 3. Estado actual (evidencia)

No existe `/widget/*`. El hoy de Chile en la base, medido el 2026-11-15 ~08:30 hora local, es `2026-11-15` (UTC−3).

Candidatos a default (el 1781 del §4 es el ejemplo de mentira: es otra estación, calidad buena, no es la DMC Quinta Normal):

| id | fuente | nombre | nombre_publico | comuna | calidad | t_max hoy |
|----|--------|--------|----------------|--------|---------|-----------|
| 3939 | 2 (DMC) | `Quinta_normal_santiago_DMC` | Quinta normal santiago DMC | Estación Central | excelente, 112 años | sí (24.1 °C) |
| 5638 | 3 (INIA) | Quinta Normal | Quinta Normal | Estación Central | sin fila de calidad | no |

La default es **3939**. Es la única DMC cuyo nombre contiene «Quinta Normal» y tiene pronóstico de hoy.

Hora que contiene las 08:30 CLT: `ts = 2026-11-15 11:00:00+00`, `t_2m = 14.6`, `weather_code = 45`, `is_day = 1`, `pop = 20`. Chips del día: `t_min = 11.2`, `t_max = 24.1`, `pp = 1.2`, `fetched_at = 2026-11-15 10:40:30+00`. `pop` ya está en 0–100. `t_max` es `magnitud.id = 3`, una sola fila.

El JOIN que filtra `nombre_abr = 't_max'` después del índice lee las 7 variables y tarda **1.55 s**. La misma query con `id_variable = (SELECT id FROM magnitud WHERE nombre_abr = 't_max' LIMIT 1)` entra al pkey y tarda **35 ms**, 1975 estaciones. El bbox ya usado por `/estaciones` es `lat BETWEEN -56 AND -17 AND lon BETWEEN -76 AND -66`.

Nginx inyecta `X-API-Key` y hace round-robin `api` / `api2`. El teléfono no manda login. Hay que reiniciar los dos procesos de la API para que el endpoint exista.

## 4. Cambios mínimos

| Cambio | Archivo | Validación | Caso borde |
|--------|---------|------------|------------|
| Builders: catálogo, default, hoy, parseo de id, hora que contiene «ahora» | `api/utils/widget_pronostico.R` | self-check al sourcear + smoke | Sin hora → `current_temp`/`condition_code`/`is_day` null y se quedan mín/máx. Sin t_max de hoy o no visible → `list(error=...)`. Default ausente → el smoke aborta (no se elige otra ciudad). |
| `GET /widget/estaciones` y `GET /widget/estaciones/<id>/hoy` | `api/endpoints/widget.R` | curl live | 400 si el path no es un entero ≥ 1, antes del cache. 404 con `$error` para que Redis no lo guarde. `Cache-Control` solo en 200 del hoy. |
| Una entrada `"widget.R"` | `api/plumber.R` `endpoint_files` | la API arranca | Sin otra línea en el header: el endpoint hace `source` del util, igual que el gordo cuando le falta el helper. |
| Smoke | `api/scripts/smoke_widget_pronostico.R` | `Rscript` en el contenedor `api` | Error de negocio se computa dos veces (no queda en Redis). Un 200 sí queda. |
| Export del asset | `api/scripts/export_widget_estaciones.R` | el archivo existe y el smoke ya validó el default | Escribe UTF-8. Si no hay default, sale con status ≠ 0 y no pisa el asset. |
| `{default_station_id, stations[]}` | `mobile/twa/app/src/main/assets/estaciones_pronostico.json` | `default_station_id` es 3939 y está en `stations` | El HTTP del catálogo es solo el array. El default vive en el asset, como dice el master. |
| Contrato con el ejemplo real y la tabla WMO → svg | `docs/producto/widget_pronostico/contrato_api.md` | se escribe después del curl | Números reales de 3939, no los del §4. |

Nombre: `estacion_nombre_view(...)$name` (ya cargado). La comuna va en su campo, no entre paréntesis. Para 3939 el nombre queda «Quinta Normal Santiago» y la comuna «Estación Central».

`current_temp` es el `t_2m` guardado de esa hora, pasado por `display_valor` (identidad). No se estira al mín/máx del día: ese ajuste es del gráfico horario del payload gordo y el master no lo pide aquí.

Desempate del default, si hubiera más de una DMC: nombre público más corto y, a igualdad, menor `id`. Hoy hay una sola.

## 5. Tareas HUMANAS

ninguna

## 6. Dependencias

- Handoff anterior: no hay (etapa 1).
- Restart api (`scripts/dc.ps1 restart api`) solo si esta etapa toca la API: sí. También `api2`, porque nginx reparte 50/50 y si no el curl puede pegarle al proceso viejo. No se reinicia shiny.
- assembleDebug: no.

## 7. Orden de implementación

1. `widget_pronostico.R` con el self-check puro (parseo de id y hora).
2. Smoke y export.
3. `widget.R` + la línea en `endpoint_files`.
4. Correr el smoke dentro del contenedor, antes del restart.
5. Exportar el asset.
6. `dc.ps1 restart api api2` y curl de ambos GET, más 400 y 404.
7. `contrato_api.md` con el cuerpo real.
8. Criterios C01–C05, handoff, cola `hecho` o `hecho_con_gaps`.
9. Commit en `widget-pronostico`, sin push.

## 8. Verificación

- `docker compose … exec -T api Rscript /app/scripts/smoke_widget_pronostico.R` → sale 0 y imprime el id 3939, el conteo y un tiempo de catálogo bajo 2 s.
- `curl.exe -sD - https://app.rodaja.cl/api/widget/estaciones` → 200, `Cache-Control: public, max-age=3600`, array con `id/name/comuna/lat/lon`, sin `rankings`.
- `curl.exe -sD - https://app.rodaja.cl/api/widget/estaciones/3939/hoy` → 200, `max-age=600`, las claves del §4.2, sin `lat`, `lon` ni `rankings`.
- `…/estaciones/abc/hoy` → 400. `…/estaciones/999999999/hoy` dos veces → 404 las dos, sin quedar cacheado como 200 (`X-Cache` no pasa a HIT).
- Asset con `default_station_id` 3939.

## 9. Fuera de alcance

Haversine, Glance, worker, permiso de ubicación, pin, manifest, `LauncherActivity`, privacidad live, AAB, minSdk, viento, rankings, `/estaciones/cercanas`.

## 10. Riesgos

- El catálogo son ~1975 estaciones. El asset pesa, y la etapa 2 lo va a parsear en el teléfono. Aceptado: el universo del widget es ese catálogo, no un recorte nuevo.
- `bubblewrap` no entra aquí. El JSON en `app/src/main/assets/` sí es del proyecto que se commitea; `mobile/twa/android/` sigue ignorado.
- Reiniciar la API deja el endpoint público. No pide login, igual que el pronóstico gordo, pero el cuerpo es chico.
- Si 3939 pierde el pronóstico de hoy entre el plan y el smoke, el script aborta y la etapa pasa a `bloqueado`. No se sustituye por la INIA ni por 1781.

## 11. Self-check

- [x] Filas `etapa_owner = 1` de `criterios.md` cubiertas (C01–C05)
- [x] Prohibidos globales del prompt fijo respetados
- [x] Humanos no fingidos (Play, keystore, teléfono, AAB)
- [x] No se sube minSdk, no se toca assetlinks ni el splash
- [x] Lat/lon no se envían al servidor (el catálogo los entrega para el haversine local; `/hoy` no los trae)
