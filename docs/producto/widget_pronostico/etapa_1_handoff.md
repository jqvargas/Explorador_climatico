# Handoff etapa 1 — `api_hoy`

**Fecha:** 2026-11-15
**Status en cola:** `hecho`
**Plan:** `docs/producto/widget_pronostico/etapa_1_plan.md`

## Qué se hizo

- `GET /widget/estaciones` y `GET /widget/estaciones/<id>/hoy`, públicos, sin `require_auth`.
- Catálogo: bbox Chile, calidad excelente o buena, una fila de `t_max` de hoy. El id de `t_max` entra al pkey (la variante que filtra `nombre_abr` después del join tardaba 1.55 s; esta, 35 ms en `EXPLAIN ANALYZE`).
- Hoy: chips `t_min` / `t_max` / `pp` con `display_valor`, `current_temp` = `t_2m` de la hora `[ts, ts+1h)`, `precip_prob` = máximo `pop` del día, `updated_at` en `America/Santiago`. Sin lat, lon, rankings ni viento.
- Default real: estación **3939**, DMC «Quinta Normal Santiago», comuna Estación Central. El 1781 del master es otra estación. El asset trae `default_station_id` y las 1975 `stations`.
- Contrato en `docs/producto/widget_pronostico/contrato_api.md`, con el JSON medido y la tabla WMO → svg.

## Qué se verificó

- [x] `docker compose … exec -T api Rscript /app/scripts/smoke_widget_pronostico.R` → exit 0. Default 3939, hoy `tmax=24.1 current=14.6 code=45 pop=57 updated=2026-11-15T07:40:30-03:00`. SQL del catálogo en caliente 0.08–0.21 s. Un error de negocio se calcula dos veces (no queda en Redis); un 200 queda a la primera.
- [x] `curl.exe` a `https://app.rodaja.cl/api/widget/estaciones` → 200, `Cache-Control: public, max-age=3600`, 1975 objetos `id/name/comuna/lat/lon`, sin `rankings`. MISS y después HIT.
- [x] `curl.exe` a `https://app.rodaja.cl/api/widget/estaciones/3939/hoy` → 200, `max-age=600`, el objeto del contrato, sin `lat` / `lon` / `rankings`. MISS 0.80 s y después HIT.
- [x] `…/abc/hoy` → 400 `{"error":"id inválido"}`, `Cache-Control: no-store`.
- [x] `…/999999999/hoy` dos veces → 404 `{"error":"Sin pronóstico para esta estación"}`, `no-store`, `X-Cache: MISS` las dos.
- [x] Miss en caliente, medido en nginx (`http://127.0.0.1/api/widget/estaciones`): **6.3 s**, 188103 bytes, `X-Cache: MISS`.
- [x] Reinicio `api` y `api2` (`scripts/dc.ps1 restart api api2`). No se reinició shiny. Tras el segundo arranque, `/ping` 200 y el hoy sigue en 200.
- [x] Filas C01–C05 de `criterios.md` quedaron en `ok`.

## Gaps / caveats / qué puede fallar después (obligatorio)

El SQL ya no es el cuello. El GET del catálogo en frío tarda ~6 s porque arma el nombre de 1975 estaciones en R y recién ahí serializa. Redis lo guarda 1 hora (`widget:estaciones:v1`); el teléfono tiene el mismo JSON en assets para no depender de esa espera.

| Riesgo | Señal | Impacto | Mitigación o etapa dueña |
|--------|-------|---------|--------------------------|
| Catálogo frío ~6 s | `X-Cache: MISS` y el curl pasa de 2 s | La primera lectura de la hora, en un solo proceso, espera. El HIT siguiente baja de 1 s | Etapa 3: el worker no debe bloquear la tarjeta. Usar el asset si no hay red. No hace falta otro índice |
| `bubblewrap update` | Borra bloques `RODAJA_WIDGET_*` (todavía no existen) | El asset bajo `app/src/main/assets/` no es de esos bloques, pero un update sigue prohibido | No correr bubblewrap en esta cola |
| Aparece una segunda DMC «Quinta Normal» | El smoke sigue eligiendo una | Hoy hay una. El desempate es nombre más corto y, a igualdad, menor id | Si el smoke deja de ver a la 3939, aborta. No elige otra ciudad |
| `current_temp` no coincide con la curva del pronóstico gordo | El gráfico estira las horas al mín/máx del día; este endpoint no | El widget muestra el `t_2m` guardado | Etapa 3 no debe «corregirlo» |

## TAREAS HUMANAS

- [x] ninguna

## Pendiente / no es de esta etapa

- Haversine, Glance, worker, permiso, pin, manifest, `LauncherActivity`, privacidad, AAB.
- `minSdk` sigue 21. No se tocó.

## Para la etapa siguiente

- No reabrir: default 3939; hoy sin coordenadas ni rankings; `current_temp` es el `t_2m` de la hora que contiene «ahora», no la observación ni la serie alineada; íconos según `contrato_api.md` (noche solo con `is_day = 0` y el composite nocturno; no aplicar el refuerzo `pop ≥ 50` del gráfico).
- Archivos tocados: `api/utils/widget_pronostico.R`, `api/endpoints/widget.R`, `api/plumber.R` (solo `widget.R` en `endpoint_files`), `api/scripts/smoke_widget_pronostico.R`, `api/scripts/export_widget_estaciones.R`, `mobile/twa/app/src/main/assets/estaciones_pronostico.json`, `docs/producto/widget_pronostico/contrato_api.md`, plan, handoff, criterios, cola.
- Git: la rama `widget-pronostico` sale de `cursor/alertas-resumen` porque el working tree de producción tiene mucho trabajo ajeno sin commit. No se hizo un checkout que lo tire. En el commit, `api/plumber.R` es el archivo que ya estaba en git más `widget.R` en `endpoint_files`. El resto del diff de `plumber.R` (perfil, billing, alertas) sigue sin stage. `mobile/twa/.gitignore` ignora `app/`; el asset entró con `git add -f` y el resto de `app/` sigue ignorado. La etapa 2 tiene que volver a usar `-f` para los `.java` de `app/src`, o el commit no los ve.
- Comandos útiles:
  - `docker compose -f docker-compose.yml -f docker-compose.prod.yml exec -T api Rscript /app/scripts/smoke_widget_pronostico.R`
  - `curl.exe -sD - https://app.rodaja.cl/api/widget/estaciones/3939/hoy`
  - El catálogo HTTP es el array. `default_station_id` está solo en el asset.

## Decisiones de producto tomadas aquí

Ninguna. El master ya había cerrado el contrato. 3939 es el id que la base devolvió para la DMC Quinta Normal, no una estación sustituta. El desempate de nombres (más corto, luego menor id) no se usó: hay una sola fila.
