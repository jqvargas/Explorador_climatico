# Plan maestro: Reporte personalizado (reemplaza al Reporte PDF por estación)

**Estado:** plan escrito el 2026-11-23. **No hay nada implementado.** No se ejecuta hasta que el operador cierre la etapa 0 (decisiones).
Prompt a pegar: [reporte_personalizado_PROMPT_FIJO.md](reporte_personalizado_PROMPT_FIJO.md) · Cola: [reporte_personalizado_cola.json](reporte_personalizado_cola.json) · Criterios: [reporte_personalizado/criterios.md](reporte_personalizado/criterios.md)
Insumos: [reporte_pdf/PREREQUISITOS_SEIA.md](reporte_pdf/PREREQUISITOS_SEIA.md) (inventario del producto actual y datos que faltan), [SEIA_GAP_ANALYSIS.md](SEIA_GAP_ANALYSIS.md) y [REPORTES_ESTACION_ROADMAP.md](REPORTES_ESTACION_ROADMAP.md).

---

## 1. Objetivo final

Hoy el **Reporte PDF** es un archivo que se regenera cada mes, **uno por estación**, y solo para estaciones de calidad «excelente». Pasa a ser un producto nuevo, el **Reporte personalizado**:

1. Una **sección nueva en la navegación principal** de la app (el mismo chrome de `rodaja_pwa.js` que tiene Reporte, Explorador, Alertas, Descargar y Cuenta).
2. El usuario entra, elige un **punto** (clic en el mapa, búsqueda o coordenadas) o un **área** (dibujar un polígono o subir un KML, GeoJSON o SHP comprimido) y, si quiere, un radio de búsqueda.
3. El sistema le muestra las **estaciones candidatas** con su distancia, la diferencia de altura y la completitud. El usuario confirma o descarta cada una, con una justificación.
4. El usuario solicita el reporte, que se genera como un **trabajo asíncrono**. El PDF resultante es **inmutable**: tiene un ID, el hash de los datos y la versión de la metodología, y se puede verificar por QR.
5. Hay dos plantillas sobre el mismo pipeline. **General** sirve a cualquier usuario. **SEIA** sirve a empresas privadas y consultoras, y depende de los prerrequisitos P0.

**El cambio de fondo:** la unidad del producto deja de ser la `estación` y pasa a ser una **solicitud** (`geometría + estaciones elegidas + parámetros + foto de los datos`). Una estación pasa a ser solo un caso particular: un punto en sus coordenadas.

## 2. Arquitectura destino

```
[ /reporte-personalizado/ ]  (página estática nginx, mapa Leaflet)
   │ 1. punto / polígono / archivo
   ▼
POST /api/reporte-personalizado/previsualizar  ──► estaciones candidatas, subcuenca, comuna, región, macrozona
   │ 2. usuario confirma estaciones + plantilla + período
   ▼
POST /api/reporte-personalizado               ──► fila reporte_solicitud (estado = pendiente), devuelve id
   │
   ▼  worker (mismo patrón que encargo_de_archivo)
reporte_personalizado_pipeline(id)
   ├─ foto de datos (max fecha y n por serie, hash)
   ├─ métricas por estación  ← generalizar worker/utils/reporte_metricas.R
   ├─ síntesis multiestación ← nuevo
   ├─ gráficos y mapa del sitio
   └─ typst → /data/reportes_personalizados/<uuid>/reporte.pdf (+ sha256)
   │
   ▼
GET /api/reporte-personalizado/<id>/estado · GET …/<id>/reporte.pdf
GET /api/me/reportes-personalizados (historial)
GET /verificar/<codigo>  (público, solo metadatos, sin datos del cliente)
```

### 2.1 Modelo de datos (nuevo)

| Tabla | Columnas clave | Nota |
|---|---|---|
| `reporte_solicitud` | `id uuid PK`, `codigo_verificacion text UNIQUE`, `user_id`, `estado` (`borrador`/`pendiente`/`procesando`/`listo`/`error`/`expirado`), `tipo_geom` (`punto`/`area`), `geom_geojson jsonb`, `centroide_lat`, `centroide_lon`, `bbox` (4 numéricos), `buffer_m`, `plantilla` (`general`/`seia`), `periodo_desde`, `periodo_hasta`, `params jsonb`, `metodologia_version`, `parametros_sea_version`, `datos_snapshot jsonb`, `datos_hash`, `pdf_path`, `pdf_sha256`, `bytes`, `error_msg`, `creado_en`, `iniciado_en`, `completado_en` | Una fila por informe emitido. Se escribe una vez y no se reescribe al cerrar en `listo`. |
| `reporte_solicitud_estacion` | `solicitud_id FK`, `id_estacion FK`, `distancia_m`, `dif_altura_m`, `pct_completitud`, `rol` (`principal`/`apoyo`/`descartada`), `justificacion text`, `orden` | La justificación de representatividad (prerrequisito 9.2). |
| `geo_capa` + `geo_poligono` | `capa` (`subcuenca`, `comuna`, `region`, `macrozona`, `sector_cc`), `codigo`, `nombre`, `geom_geojson`, `bbox`, `version`, `fuente`, `licencia` | Capas de contexto. Las carga la etapa 1. Depende de la decisión D1. |

Los scripts nuevos van en `api/scripts/create_reporte_solicitud.sql` y `api/scripts/create_geo_capas.sql`, en el mismo estilo que `create_estacion_reporte.sql`.

### 2.2 Geometría: D1 decidida el 2026-11-24, PostGIS en la BD y `sf` en R

**Decisión del operador: las dos.**
- **BD:** la imagen nueva [db/Dockerfile](../../db/Dockerfile) es la **misma** timescaledb que corría (fijada por digest: Alpine 3.22, PG 15.18, timescaledb 2.25.2) con PostGIS 3.6.1 compilado encima. Monta el mismo `./pgdata`, así que no hay que mover datos. Las geometrías y las consultas espaciales (ST_DWithin, ST_Intersects, % de área por subcuenca o sector) se hacen en SQL.
- **R:** `sf` está instalado en las imágenes `api` y `worker`. Sirve para leer los archivos que sube el cliente (KML, SHP comprimido, GeoJSON) y para los mapas del PDF.
- **Sigue faltando:** `qrcode`. Lo instala la etapa 5.
- **Verificado el 2026-11-24:** `CREATE EXTENSION postgis` corrido en `clima` (3.6.1, GEOS 3.13.1, PROJ 9.6.0); timescaledb sigue en 2.25.2. `sf` en api (GEOS 3.14.1, GDAL 3.12.2, PROJ 9.7.1) y worker (GEOS 3.10.2, GDAL 3.4.1, PROJ 8.2.1). Reproyectar (-70.65, -33.45) a 32719 da lo mismo en PostGIS y en `sf`. Los dos Dockerfile de R necesitan `cmake` porque `s2` compila su propio Abseil.
- **Para volver atrás:** poner de nuevo `image: timescale/timescaledb:latest-pg15` en `docker-compose.yml`. Esa imagen sigue en el cache local. La extensión `postgis` habría que quitarla antes (`DROP EXTENSION postgis`), porque la imagen vieja no trae sus librerías.

La etapa 1 ya no instala nada. Parte creando la extensión (si no está), la columna `geom` y las capas.

### 2.3 Reuso del pipeline actual

| Hoy (por estación) | En el Reporte personalizado |
|---|---|
| `reporte_build_variable_state(df, var_meta, oni)` en [reporte_pipeline.R](../../worker/utils/reporte_pipeline.R) | Se reusa tal cual, por cada estación elegida. |
| [reporte_metricas.R](../../worker/utils/reporte_metricas.R) (Mann-Kendall, SPI, ENSO) | Se reusa. Recibe `periodo_desde` y `periodo_hasta` en vez del registro completo. |
| [reporte_charts.R](../../worker/utils/reporte_charts.R) y [reporte_sections.R](../../worker/utils/reporte_sections.R) | Se dividen en secciones por estación (reuso) y secciones del sitio (nuevas: mapa, tabla de representatividad, síntesis). |
| [reporte_storage.R](../../worker/utils/reporte_storage.R) con rutas `/data/reportes/<eid>` | Rutas nuevas en `/data/reportes_personalizados/<uuid>`, que se escriben una sola vez. |
| [md2pdf.R](../../worker/utils/md2pdf.R) y [reporte_estacion.typ](../../worker/templates/reporte_estacion.typ) | Se reusa el motor. Plantillas nuevas: `reporte_personalizado_general.typ` y `reporte_personalizado_seia.typ`. |
| `ficha_del_mirador` y el job mensual | **En pausa desde el 2026-11-24** (ver §2.4). Se retiran en la etapa 7. **No se borran.** |
| Cuota: `descarga_excede_cuota` / `reservar_bytes_descarga` en [reporte_pdf.R](../../api/endpoints/reporte_pdf.R) | Pendiente de la decisión D3 (cobro). Mientras tanto se usa el mismo cobro por bytes. |

**Antes de reusar:** hay que aclarar los `error_msg` que hoy tienen 672 de 683 filas de `ficha_del_mirador` (ver la §A1 de PREREQUISITOS_SEIA.md). Esto se hace en la etapa 3.

### 2.4 Pausa del PDF por estación (2026-11-24)

El reporte por estación no funcionaba bien (672 de 683 filas con `error_msg`), así que el operador decidió **pausarlo y no mostrarlo** hasta que el Reporte personalizado esté listo. Hay un solo interruptor: `REPORTE_PDF_HABILITADO` en `.env`. Si falta o vale `0`, el reporte está en pausa; con `1` vuelve a funcionar.

| Pieza | Qué hace con la pausa |
|---|---|
| [api/utils/reporte_pdf_offer.R](../../api/utils/reporte_pdf_offer.R) (`reporte_pdf_habilitado()`) | `/reporte/status` responde `estado = "pausado"` y `disponible = false`. `/reporte.pdf` responde 503. |
| `nginx/html/mi-reporte/app.js` | No se tocó: como el estado no es `disponible`, `pendiente` ni `congelado`, el botón «Informe mensual» no aparece. |
| [shiny/modules/mapa.R](../../shiny/modules/mapa.R) | No muestra el botón «Informe PDF» en el popup de la estación. |
| `worker/run/actualizar_reportes_mensual.R`, `actualizar_reporte_pesado.R`, `actualizar_reporte_estacion.R`, `backfill_reporte_estacion.R`, `reevaluar_reporte_elegibilidad.R` | Terminan al inicio con exit 0 y no generan nada. |
| Tarea `Rodaja_Actualizar_Reportes_Mensual` | Quedó `Disabled` en el Programador de tareas. Se reactiva con `schtasks /Change /TN "Rodaja_Actualizar_Reportes_Mensual" /ENABLE`. |

`estacion_calidad` no depende de esa tarea: la refresca el pipeline diario (`ingest/run/actualizar_diario.R`). Los PDF viejos siguen en `/data/reportes` y `ficha_del_mirador` no se tocó.

## 3. Decisiones de producto (etapa 0, las toma el operador)

| id | Pregunta | Recomendación |
|---|---|---|
| D1 | ¿Geometría con `sf` en R o con PostGIS? (§2.2) | **Decidida (2026-11-24): las dos.** PostGIS en la BD y `sf` en api y worker. |
| D2 | ¿La sección es una página estática nueva (`/reporte-personalizado/`) o una pestaña dentro del Explorador Shiny? | Página estática: el resto del chrome nuevo ya es estático (mi-reporte, alertas, descargar) y así no compite con la carga de Shiny. |
| D3 | ¿Cómo se cobra? ¿Por informe, por plan (N informes al mes) o por bytes como hoy? ¿La plantilla SEIA es solo de pago? | Plantilla general dentro del plan; SEIA como compra por informe. Hay que definir los precios en [MONETIZACION_ROADMAP.md](MONETIZACION_ROADMAP.md). |
| D4 | ¿Qué límites tiene la geometría? | Radio máximo de 50 km, polígono de hasta 2.000 km² y 5.000 vértices, archivo de hasta 5 MB y máximo 10 estaciones elegidas. |
| D5 | ¿Cuánto tiempo se conservan los PDF y las geometrías del cliente? | 24 meses; luego `expirado`, se borra el PDF y se mantienen los metadatos de verificación. Hay que avisarlo en [legal](../../nginx/html/legal/). |
| D6 | ¿Qué pasa con «Reporte» (`/mi-reporte/`) en la navegación? | Se mantiene `/mi-reporte/` como reporte en pantalla. El botón «PDF» de la estación abre `/reporte-personalizado/?eid=<id>` con el punto ya cargado. |
| D7 | ¿Nombre visible y texto de deslinde? | «Reporte personalizado». El deslinde contractual de [SEIA_GAP_ANALYSIS.md](SEIA_GAP_ANALYSIS.md) va en toda plantilla. |

## 4. Prohibido (todas las etapas)

- Codear una etapa sin su `etapa_<n>_plan.md`, o saltar etapas.
- Borrar `ficha_del_mirador`, `/data/reportes` o los endpoints `/estaciones/<id>/reporte*` antes de la etapa 7 y sin el visto bueno del operador.
- Cambiar la versión de PostgreSQL o de timescaledb, o el volumen `./pgdata`. La imagen de la BD es [db/Dockerfile](../../db/Dockerfile), fijada por digest; para subir de versión hace falta un plan aparte.
- Reactivar el PDF por estación (`REPORTE_PDF_HABILITADO=1` o la tarea mensual) sin que lo pida el operador.
- Reescribir un `reporte_solicitud` que ya está `listo` (el PDF y su hash son inmutables). Para corregir, se emite uno nuevo.
- Mostrar en `/verificar/<codigo>` la geometría, el nombre del cliente o datos del proyecto. Solo el código, la fecha, el hash, la versión y el estado.
- Enviar la geometría del cliente a servicios externos (geocodificadores, mapas base con payload). Los tiles del mapa base sí se permiten.
- Afirmar en el PDF que el reporte «cumple» con el SEA o que es «representativo». El texto es un insumo; la validación la hace el titular.
- Hacer commits salvo que se pidan. Usar `--build` salvo que cambie un Dockerfile (la etapa 5 lo cambia para `qrcode`). Este workspace **es producción**: ver [.cursor/rules/servidor-prod.mdc](../../.cursor/rules/servidor-prod.mdc).
- En el JS estático, seguir [.cursor/rules/js-html-quotes.mdc](../../.cursor/rules/js-html-quotes.mdc) y subir el `?v=` de cada script que se toque.

## 5. Cuándo una etapa está lista

1. Existen `reporte_personalizado/etapa_<n>_plan.md` y `etapa_<n>_handoff.md`.
2. Las filas `etapa_owner = n` de los criterios están en `ok`, `gap_documentado` o `no_aplica`.
3. Hay verificación con un comando o un gesto y su resultado (curl, psql o navegador), no una afirmación.
4. Si se tocó api, worker o nginx, el reinicio está hecho según servidor-prod.mdc.

## 6. Etapas

### Etapa 0: Decisiones (`decisiones`). La hace el operador.
Cerrar D1 a D7 en la §3, escribir la respuesta en la columna «Recomendación» (o reemplazarla) y marcar la etapa `hecho` en la cola. **Ningún agente reclama la etapa 1 hasta que esto esté hecho.**

### Etapa 1: Base geoespacial (`base_geo`)
- PostGIS y `sf` ya están instalados (§2.2). Verificar `CREATE EXTENSION IF NOT EXISTS postgis` y `requireNamespace("sf")` antes de empezar.
- Agregar `mirador.geom geometry(Point, 4326)` calculada a partir de lat y lon, con un índice GiST.
- Crear `geo_capa` y `geo_poligono` (`geom geometry(MultiPolygon, 4326)` con GiST). Cargar subcuencas DGA, comunas, regiones y macrozonas con su fuente, licencia y versión. El script va en `api/scripts/` y es reproducible.
- Crear `api/utils/geo.R` con: `geo_validar_geojson()`, `geo_leer_archivo()` (KML, GeoJSON y SHP comprimido con `sf`, dentro de los límites de D4), `geo_contexto(geom)` (subcuenca, comuna, región y macrozona con % de área, en SQL) y `geo_estaciones_cercanas(geom, radio_m)` (`ST_DWithin` sobre `geography`).
- **Fuera de alcance:** sectores A-N y DEM.

### Etapa 2: Solicitud y API (`api_solicitud`)
- Crear `reporte_solicitud` y `reporte_solicitud_estacion`.
- Crear `api/endpoints/reporte_personalizado.R` y registrarlo en `endpoint_files` de [api/plumber.R](../../api/plumber.R):
  - `POST /reporte-personalizado/previsualizar` → contexto geo + candidatas (distancia, dif. altura, completitud de `estacion_calidad` y variables). No escribe en la BD. Se puede usar sin cuenta.
  - `POST /reporte-personalizado` → exige cuenta; valida la geometría, las estaciones (máximo de D4), la plantilla y la cuota (D3); crea la fila `pendiente` y devuelve `{id, codigo_verificacion}`.
  - `GET /reporte-personalizado/<id>/estado`, `GET /reporte-personalizado/<id>/reporte.pdf` (solo el dueño, con `Cache-Control: private, no-store`) y `GET /me/reportes-personalizados`.
- En nginx: `location` para `/api/reporte-personalizado` con un límite de body acorde a D4. **No heredar** el timeout de 8 s de los listados.
- En esta etapa el worker todavía no genera nada: verificar que la fila queda `pendiente`.

### Etapa 3: Pipeline del worker (`pipeline`)
- Primero, diagnosticar los `error_msg` de `ficha_del_mirador` y documentar si la fase por estación falla sin avisar. **No arreglar el producto viejo** salvo que bloquee el reuso.
- `worker/utils/reporte_personalizado_pipeline.R`: toma solicitudes `pendiente` (igual que `encargo_de_archivo` en [worker/worker.R](../../worker/worker.R)), pasa a `procesando` y, por estación, reusa `reporte_build_variable_state` con el período pedido.
- **Foto de los datos:** por cada `(estación, variable)`, guardar `n`, `fecha_min`, `fecha_max` y `sum(valor)` en `datos_snapshot`; `datos_hash = sha256` del JSON canónico.
- Secciones nuevas: portada con el código, el mapa del sitio con las estaciones (imagen estática generada en el worker, sin servicios externos), la tabla de representatividad (de `reporte_solicitud_estacion`), la síntesis multiestación, la metodología (versiones) y la sección de fuentes y licencias.
- Plantilla `worker/templates/reporte_personalizado_general.typ` con el QR que apunta a `/verificar/<codigo>`.
- Guardar en `/data/reportes_personalizados/<uuid>/`, calcular `pdf_sha256` y pasar a `listo`. En caso de error, `error` con `error_msg` (**y limpiarlo cuando funciona**, al revés de lo que pasa hoy).
- `worker/scripts/smoke_reporte_personalizado.R`: un punto con 3 estaciones, de punta a punta.

### Etapa 4: Sección en la app (`ui_seccion`)
- Agregar una entrada en `NAV` de [nginx/html/rodaja_pwa.js](../../nginx/html/rodaja_pwa.js) (`key: "personalizado"`, `href: "/reporte-personalizado/"`, `lock: true` si D3 exige cuenta) y su detección de ruta (el mismo bloque que mapea `/mi-reporte` a `"reporte"`).
- Crear `nginx/html/reporte-personalizado/` (`index.html`, `app.js`, `styles.css`) y agregar `location` en [nginx.conf.template](../../nginx/nginx.conf.template), copiando el patrón de `/descargar/`.
- Flujo en 4 pasos:
  1. **Sitio:** punto, polígono o archivo, más el radio.
  2. **Estaciones:** lista de candidatas, marcar el rol y escribir la justificación.
  3. **Opciones:** plantilla, período y variables, con el costo según D3.
  4. **Solicitar:** ver el estado por polling (igual que en `/descargar/`) y descargar.
- Historial («Mis reportes»), con el código y un enlace a la verificación.
- Soportar `?eid=<id>` para precargar el punto de una estación (D6).
- Usar Leaflet por CDN con versión fija. Debe funcionar en el TWA (ancho de teléfono) y verse bien en el modo de la app.

### Etapa 5: Verificación pública (`verificacion`)
- `GET /api/verificar/<codigo>` y la página `nginx/html/verificar/`: muestra el código, la fecha, la plantilla, las versiones, `datos_hash` y `pdf_sha256`, más una opción para subir el PDF y comparar su sha256 en el navegador (el archivo no se sube al servidor).
- Limitar la tasa de consultas por IP. No mostrar nada de lo que prohíbe la §4.

### Etapa 6: Plantilla SEIA (`plantilla_seia`). Puede dividirse en sub-etapas.
- Depende de los prerrequisitos P0 de [PREREQUISITOS_SEIA.md](reporte_pdf/PREREQUISITOS_SEIA.md): normales en 3 períodos (8.1), sectores A-N con su tabla 3 (4.1 a 4.3), licencias (9.3) y evapotranspiración (1.2).
- **Si un prerrequisito P0 falta, la etapa queda `bloqueado` con su lista. No se inventan valores.**
- Agregar `reporte_personalizado_seia.typ` y las secciones A0 a A13 de SEIA_GAP_ANALYSIS.md que se puedan sostener con datos.
- Fijar `parametros_sea_version` en cada informe.

### Etapa 7: Retiro del reporte por estación (`retiro`)
- El botón «PDF» de [shiny/modules/mapa.R](../../shiny/modules/mapa.R) y de `nginx/html/mi-reporte/` pasa a abrir `/reporte-personalizado/?eid=`.
- `GET /estaciones/<id>/reporte.pdf` sigue sirviendo el último archivo congelado por un período de gracia definido por el operador, con un header `Deprecation`.
- Dejar de programar `ingest/bat/actualizar_reportes_mensual.bat` y `actualizar_reporte_pesado.bat` (lo hace un humano en el Programador de tareas).
- **No borrar** `ficha_del_mirador` ni `/data/reportes`. Eso es una decisión aparte del operador.

### Etapa 8: Cierre (`cierre`)
- Corrida completa en el navegador (escritorio y teléfono): punto, polígono y KML; general y, si está desbloqueada, SEIA; verificar el QR; historial.
- Escribir `reporte_personalizado/CIERRE.md` con los tiempos de generación (p50 y peor caso) y los gaps.

## 7. Tareas 100 % humanas

- Cerrar las decisiones D1 a D7 (etapa 0).
- Conseguir la capa de sectores A-N del SEA y confirmar las licencias de DMC, DGA y ERA5-Land (prerrequisitos de la etapa 6).
- Decidir cuándo se reactiva o se retira el PDF por estación (hoy en pausa, §2.4).
- Ajustar el texto legal (retención D5 y tratamiento de la geometría del cliente).
- Desprogramar las tareas mensuales en Windows (etapa 7).

## 8. Índice de lectura para un agente

1. Este archivo, §1 a §5 y la etapa que le toca.
2. [reporte_pdf/PREREQUISITOS_SEIA.md](reporte_pdf/PREREQUISITOS_SEIA.md), parte A (dónde está cada pieza hoy).
3. Los archivos de la columna «Hoy» de la §2.3 que su etapa reusa.
4. La fila de su etapa en la cola y en los criterios.
