# Manifiesto MeteoVision (ingest/vendor/meteovision)

| Archivo | Rol | Escribe BD |
|---------|-----|------------|
| `scrape/descargar_DGA.R` | pp, caudal, temp, viento DGA | No |
| `scrape/descargar_DMC.R` | pp, temp, humedad, radiacion DMC | No |
| `scrape/descargar_INIA.R` | INIA agromet (fase 2) | No |
| `lib/queries_catalog.R` | Lectura catalogo estaciones/variables | No |
| `lib/actualizar_bbdd.R` | Legacy con `dbWriteTable` | **Deprecado** |
| `legacy/MAIN.R` | Entry legacy MeteoVision | Deprecado |

## Orquestacion Explorador

| Archivo | Rol |
|---------|-----|
| `ingest/lib/orchestrate_meteovision.R` | Bucle scrape + `persist.R` + `post_ingesta` |
| `ingest/lib/persist.R` | Unico UPSERT a `bitacora_oficial` |
| `ingest/run/actualizar_dga.R` | Entry DGA |
| `ingest/run/actualizar_dmc.R` | Entry DMC |
| `ingest/run/smoke_scrape.R` | Test descarga sin BD |
| `ingest/run/smoke_pipeline.R` | Test pipeline DRY/WRITE |

## Variables

**DGA:** pp, q, t_min (bundle t_mean+t_min+t_max), u2_* (bundle)  
**DMC:** pp, t_max, t_min, t_mean, hr_mean, rs_mean
