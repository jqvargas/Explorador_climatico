# Modulo de ingesta (MeteoVision)

Actualiza `bitacora_oficial` desde DGA/DMC (scripts MeteoVision).

Hay dos formas de ejecutar la ingesta:

- **Docker (recomendado):** contenedor `ingest` con todas las dependencias
  pre-instaladas. No requiere R en el host. Ver [seccion Docker](#docker).
- **Host Windows:** `Rscript` + `install_deps.R`. Ver el resto del README.

```text
ingest/bat/*.bat -> ingest/run/*.R
  -> ingest/lib/orchestrate_meteovision.R (scraping)
  -> ingest/lib/persist.R (UPSERT + QC)
  -> post_ingesta_observaciones() (QC + upsert jornada_cerrada + estacion_calidad + Redis)
```

## Requisitos

- R 4.x en PATH (`Rscript`)
- Docker: timescaledb, redis, api
- Dependencias R:

```bat
Rscript ingest\install_deps.R
```

## Configuracion

1. `ingest\config\ingest.env` (desde `ingest.env.example`)
2. Indice UPSERT (una vez):

```bat
docker exec -i clima_db psql -U postgres -d clima -f /dev/stdin < api\scripts\add_unique_observacion_final.sql
```

Variables opcionales:

| Variable | Default | Uso |
|----------|---------|-----|
| `INGEST_FECHA_INI` | `YYYY-01-01` | Inicio rango |
| `INGEST_FECHA_FIN` | hoy | Fin rango |
| `INGEST_MODO` | `completo` | `completo` o `test` (1 estacion) |
| `INGEST_DRY_RUN` | â€” | Pipeline sin escribir BD |
| `INGEST_WRITE_TEST` | â€” | 1 estacion, 7 dias, con persist |
| `INGEST_SMOKE_CODIGO` | auto | Codigo nacional para smoke |
| `INGEST_SKIP_POST_INGESTA` | â€” | Suprime post_ingesta (uso interno orquestador) |
| `INGEST_BACKFILL` | â€” | Permite rango multi-ano en backfill manual |

## Actualizacion diaria holistica

**Canonical:** una sola tarea programada que ejecuta todo el pipeline en orden.

```bat
ingest\bat\actualizar_diario.bat
```

Orden de pasos (sin solapamiento):

```text
preflight BD
  -> ingesta DGA + DMC (un proceso R, un post_ingesta / REFRESH estacion_calidad)
  -> verify
  -> alertas top 10 (observaciones)
  -> refresh pronostico Open-Meteo
  -> alertas pronostico (encola tempranas + envia digest unificado por usuario)
```

Log unico: `ingest\logs\actualizar_diario_YYYYMMDD.log`  
Lockfile: `ingest\logs\actualizar_diario.lock` (JSON con PID; 8 h; evita instancias paralelas).

Si la ingesta falla (exit != 0), **no** se ejecutan pronostico ni alertas.

## Reportes PDF (mensual)

Los reportes PDF pre-generados **no** forman parte del pipeline diario. Se actualizan una vez al mes (estaciones calidad excelente).

```bat
ingest\bat\actualizar_reportes_mensual.bat
```

Pasos internos (`worker/run/actualizar_reportes_mensual.R`):

```text
REFRESH estacion_calidad
  -> sync ficha_del_mirador (elegibilidad, sin PDF)
  -> Fase 1 (pendientes / sin state.json)
  -> Heavy (activas con state)
```

Log: `ingest\logs\actualizar_reportes_mensual_YYYYMMDD.log`  
Lock: `ingest\logs\actualizar_reportes_mensual.lock` (24 h)

Smoke acotado:

```bat
docker compose -f docker-compose.yml -f docker-compose.prod.yml exec -T worker Rscript run/actualizar_reportes_mensual.R --limit=2 --eid=330021
```

Backfill manual (admin, una estacion):

```bat
ingest\bat\backfill_reporte_estacion.bat --eid=330021
```

`actualizar_reporte_pesado.bat` es alias de `actualizar_reportes_mensual.bat`.

Tarea programada: `Rodaja_Actualizar_Reportes_Mensual` (dia 1, 02:00; limite 24 h). Se registra con `register_actualizar_diario.ps1`.


## Tabla `jornada_cerrada` (ranking rapido)

Agregacion diaria precomputada desde `bitacora_oficial` (`SUM` para `pp`, `AVG` para el resto; respeta `qc_flag = 0`).

Tras cada ingesta, `post_ingesta_observaciones()` hace **upsert incremental** solo del rango de fechas insertadas (`observacion_diaria_upsert_rango` en `worker/utils/observacion_diaria.R`).

El paso `alertas_pronostico` del pipeline usa sync SQL batch sobre `jornada_cerrada` (segundos en lugar de ~60–90 min con el loop legacy). Si la tabla no existe o esta vacia, cae al sync legacy.

**Verificacion** (no relanzar el evaluador a mano; usar el pipeline):

```bat
ingest\bat\actualizar_diario.bat
```

En `ingest\logs\actualizar_diario_YYYYMMDD.log` buscar `[alertas:temprana] sync SQL upserts=`. Smoke SQL:

```bat
docker exec -i clima_db psql -U postgres -d clima -c "SELECT COUNT(*) FROM jornada_cerrada;"
docker exec -i clima_db psql -U postgres -d clima -c "SELECT COUNT(*) FROM aviso_de_madrugada WHERE fecha_objetivo >= CURRENT_DATE;"
```
### Robustez y resume

Orquestador: [`ingest/scripts/actualizar_diario.ps1`](scripts/actualizar_diario.ps1) (el `.bat` es wrapper).

| Archivo | Proposito |
|---------|-----------|
| `ingest/logs/actualizar_diario_YYYYMMDD.log` | Log del dia |
| `ingest/logs/actualizar_diario.checkpoint` | Progreso (estacion/variable + pasos pipeline) |
| `ingest/logs/actualizar_diario.status.json` | Ultimo run (exit, paso, timestamps) |
| `ingest/logs/actualizar_diario.lock` | Lock con PID (heartbeat) |

**Resume por estacion:** si falla a mitad de `dmc/hr_mean` estacion 42, al relanzar salta 1â€“41 y continua desde 42. UPSERT hace el re-scrape idempotente.

```powershell
# Ver estado
.\ingest\scripts\resume_actualizar.ps1 -Status

# Reanudar desde checkpoint
.\ingest\scripts\resume_actualizar.ps1 -Resume

# Corrida limpia (borra checkpoint)
.\ingest\scripts\resume_actualizar.ps1 -Reset

# Lock huerfano
.\ingest\scripts\resume_actualizar.ps1 -ClearLock
```

Variables: `INGEST_RESET_CHECKPOINT=1` fuerza inicio limpio en R.

### Task Scheduler

Instalar (PowerShell; pide usuario y contraseÃ±a **en consola**, compatible con SSH):

```powershell
Set-Location C:\Users\rodaja\Explorador_climatico
.\ingest\scripts\register_actualizar_diario.ps1
```

Sin prompts (solo si hace falta automatizar):

```powershell
.\ingest\scripts\register_actualizar_diario.ps1 -UserName "Javier_Zenbook\rodaja" -Password "tu_clave"
```

Crea tres tareas:

| Tarea | Hora | Accion |
|-------|------|--------|
| `Rodaja_Docker_Warmup` | 00:00 | `ensure_docker.ps1` |
| `Rodaja_Actualizar_Diario` | 00:10 | `actualizar_diario.ps1 -Resume` |
| `Rodaja_Actualizar_Reportes_Mensual` | dia 1, 02:00 | `actualizar_reportes_mensual.ps1` |

Settings: `StopOnIdleEnd=false`, `StartWhenAvailable`, reintento 3x cada 30 min, limite 8 h.

**Manual (GUI Docker Desktop, una vez si hace falta):** Settings â†’ General â†’ **Start Docker Desktop when you sign in**.

Probar:

```bat
schtasks /Run /TN "Rodaja_Actualizar_Diario"
```

Desactivar tareas antiguas separadas si existen (`actualizar_todo`, `evaluar_alertas`,
`refresh_pronostico`, `evaluar_alertas_pronostico`).

### Prueba antes de programar

```bat
REM Fase 1: preflight + fechas, sin scrape
docker compose -f docker-compose.yml -f docker-compose.prod.yml --profile ingest run --rm ^
  -e INGEST_MODO=test -e INGEST_DRY_RUN=1 ^
  ingest Rscript ingest/run/actualizar_diario.R

REM Fase 2: corrida acotada manual
set INGEST_FECHA_INI=2026-06-24
set INGEST_FECHA_FIN=2026-07-01
ingest\bat\actualizar_diario.bat
```

Backfill manual (no scheduler): `set INGEST_FECHA_INI=2024-01-01` y `set INGEST_BACKFILL=1`.

`actualizar_todo.bat` queda para uso manual; el job diario es `actualizar_diario.bat`.

## Pruebas (orden recomendado)

```bat
ingest\bat\smoke_test.bat
ingest\bat\smoke_scrape.bat
set INGEST_DRY_RUN=1
ingest\bat\smoke_pipeline.bat
set INGEST_WRITE_TEST=1
Rscript ingest\run\smoke_pipeline.R --fuente=dga --variable=pp
ingest\bat\actualizar_dga.bat
set INGEST_VERIFY_YEAR=2026
ingest\bat\verify_ingesta.bat
```

## Produccion

```bat
set INGEST_FECHA_INI=2026-01-01
set INGEST_FECHA_FIN=2026-12-31
ingest\bat\actualizar_dga.bat
ingest\bat\actualizar_dmc.bat
```

O `ingest\bat\actualizar_todo.bat` para ambas fuentes (manual; ver [Actualizacion diaria holistica](#actualizacion-diaria-holistica)).

## Docker

Imagen `clima_ingest` (definida en `ingest/Dockerfile`) con librerias de sistema
(`libxml2`, `libcurl`, `libssl`, `libpq`, `libhiredis`) y paquetes R
(`httr`, `rvest`, `xml2`, `RPostgres`, `redux`, `dplyr`, `lubridate`, ...) ya
instalados. El servicio usa el `profile: ingest`, asi que **no** arranca con
`docker compose up`; se ejecuta bajo demanda.

```bat
REM 1. Construir la imagen (una vez, o tras cambiar dependencias)
ingest\docker_build.bat

REM 2. Ejecutar scripts (el repo se monta en /app; la salida queda en host)
ingest\docker_run.bat ingest/run/smoke_test.R
ingest\docker_run.bat ingest/run/smoke_scrape.R --fuente=dga --variable=pp

set INGEST_DRY_RUN=1
ingest\docker_run.bat ingest/run/smoke_pipeline.R --fuente=dga --variable=pp

set INGEST_FECHA_INI=2026-01-01
set INGEST_FECHA_FIN=2026-12-31
ingest\docker_run.bat ingest/run/actualizar_dga.R
ingest\docker_run.bat ingest/run/actualizar_dmc.R

REM Produccion (compose prod):
ingest\docker_run_prod.bat ingest/run/actualizar_diario.R
```

`docker_run.bat` reenvia automaticamente las variables `INGEST_*` del entorno al
contenedor. `docker_run_prod.bat` anade `-f docker-compose.prod.yml`.
La conexion a BD/Redis usa los nombres de servicio internos
(`timescaledb`, `redis`), inyectados por `docker-compose.yml`.

Equivalente sin wrapper:

```bat
docker compose --profile ingest run --rm ingest Rscript ingest/run/smoke_scrape.R --fuente=dga
```

## Estructura

```text
ingest/
  vendor/meteovision/scrape/   descargar_DGA.R, descargar_DMC.R
  vendor/meteovision/lib/      queries_catalog.R, actualizar_bbdd.R (legacy)
  lib/                         orchestrate_meteovision.R, persist.R, ...
  run/                         actualizar_*.R, smoke_*.R
  bat/                         wrappers Windows
  sources/MANIFEST.md          inventario
```

## Reglas

- Unico escritor de `bitacora_oficial`: `ingest/lib/persist.R`
- Tras ingesta con filas nuevas: `post_ingesta_observaciones()`
- `ingest/vendor/meteovision/lib/actualizar_bbdd.R` es legacy (no usar en produccion)

## Alertas top 10 (post-ingesta)

Incluido en `actualizar_diario.bat` (paso 6). Para ejecucion manual:

```bat
ingest\bat\evaluar_alertas.bat
```

Requiere variables SMTP en `.env` del proyecto.

### Probar aspecto y envio de alertas (test)

```bat
ingest\bat\test_alertas_email.bat list
ingest\bat\test_alertas_email.bat preview
ingest\bat\test_alertas_email.bat send pp_rank7
ingest\bat\test_alertas_email.bat send-all
ingest\bat\test_alertas_email.bat eval
```

- **preview** â€” genera HTML/texto en `worker\logs\alertas_preview\` sin enviar correo.
- **send** â€” envia un escenario de muestra a `admin@rodaja.cl`.
- **send-all** â€” envia los 5 escenarios (precip, temp, caudal, rank 1 y 10).
- **eval** â€” simula la evaluacion real con abono activas (sin enviar).

Desde Docker directo:

```bat
docker compose -f docker-compose.yml -f docker-compose.prod.yml exec -T worker Rscript run/test_alertas_email.R --all --send --to=admin@rodaja.cl
```

## Pronostico Open-Meteo (batch diario)

Schema SQL (una vez):

```bat
Get-Content api\scripts\create_pronostico_estacion.sql | docker compose -f docker-compose.yml -f docker-compose.prod.yml exec -T timescaledb psql -U postgres -d clima
Get-Content api\scripts\create_alertas_pronostico.sql | docker compose -f docker-compose.yml -f docker-compose.prod.yml exec -T timescaledb psql -U postgres -d clima
```

Smoke (validar API Open-Meteo):

```bat
ingest\bat\smoke_openmeteo.bat --stations=330021,340031,380013
```

Refresh diario (~1970 estaciones excelente/buena, 16 dias, batch):

```bat
ingest\bat\refresh_pronostico.bat
```

Incluido en `actualizar_diario.bat` (paso 7).

Alertas tempranas por pronostico (usuarios registrados + favorita, tras refresh):

```bat
ingest\bat\evaluar_alertas_pronostico.bat
```

Incluido en `actualizar_diario.bat` (paso 8).

Desde Docker:

```bat
docker compose -f docker-compose.yml -f docker-compose.prod.yml exec -T worker Rscript run/smoke_openmeteo.R --stations=330021,340031,380013
docker compose -f docker-compose.yml -f docker-compose.prod.yml exec -T worker Rscript run/refresh_pronostico.R
docker compose -f docker-compose.yml -f docker-compose.prod.yml exec -T worker Rscript run/evaluar_alertas_pronostico.R --dry-run
```
