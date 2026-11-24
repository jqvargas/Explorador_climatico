# Reporte PDF para SEIA: inventario actual y prerrequisitos

**Fecha:** 2026-11-23
**Para qué sirve:** antes de rediseñar el Reporte PDF como entregable para empresas privadas que presentan al SEIA, dejar escrito (1) dónde vive hoy el producto y de qué depende, y (2) qué datos y recursos hay que conseguir antes de construir la versión nueva.

> **2026-11-23:** el Reporte PDF por estación será reemplazado por el **Reporte personalizado** (punto o área elegidos por el usuario). El plan está en [PLAN_REPORTE_PERSONALIZADO.md](../PLAN_REPORTE_PERSONALIZADO.md). La parte A de este documento describe el producto actual que ese plan reusa y luego retira.

Documentos relacionados: [SEIA_GAP_ANALYSIS.md](../SEIA_GAP_ANALYSIS.md) (brecha capítulo a capítulo) y [REPORTES_ESTACION_ROADMAP.md](../REPORTES_ESTACION_ROADMAP.md) (roadmap y modelo comercial).

---

## Parte A. El producto hoy

### A1. Base de datos (`clima`, contenedor `clima_db`, TimescaleDB pg15)

| Objeto | Tipo | Rol en el reporte |
|---|---|---|
| `ficha_del_mirador` | tabla de control | Una fila por estación: `estado` (pendiente/activo/congelado/error), `elegible`, `congelado_desde`, `ultima_actualizacion`, `ultima_actualizacion_pesada`, `version_schema`, `error_msg`. DDL en [create_estacion_reporte.sql](../../../api/scripts/create_estacion_reporte.sql). No tiene FK ni vistas o funciones que dependan de ella. |
| `estacion_calidad` | vista materializada | Decide la elegibilidad (nivel `excelente`, vía `alerta_temprana_estacion_elegible`) y alimenta la sección de calidad. |
| `mirador` + `cantera` | tablas | Metadatos: nombre, código nacional, lat/lon, altura, región, comuna, cuenca y operador. |
| `magnitud` | tabla | Catálogo de variables (pp, t_max, t_min, t_mean, q, hr_mean, pa, rs_mean, viento…). |
| `jornada_cerrada` | hypertable (~11 GB) | Series diarias por estación y variable: fuente principal de todas las métricas. |
| `carta_del_mirador` | tabla | Sección de pronóstico (desde hoy en adelante). |
| `oficio_del_mirador` | tabla | Solo se usa para contar elegibles en la reevaluación. |

Estado al 2026-11-23: 683 filas, **664 activas y elegibles** y 19 congeladas. Última corrida mensual: 2026-11-01. Hay 666 `reporte.pdf` en disco.

> ⚠️ **Hay que revisarlo antes de construir encima.** 672 de las 683 filas tienen `error_msg` no nulo: `could not find function "reporte_render_charts"` (564), `non-numeric argument to binary operator` (78), `reporte_doc_version_string` no encontrada (28) y `'length = 4' in coercion to 'logical(1)'` (2). Aun así, las filas siguen en estado `activo` y los PDF se ven recientes. Puede que el mensaje haya quedado de una corrida anterior y nadie lo limpió, o que algunas fases fallen en silencio. Hoy ambas funciones existen en `/app/utils` del worker.

### A2. Código

**Generación (worker, R + pandoc 3.6.4 + typst 0.12.0; ver [worker/Dockerfile](../../../worker/Dockerfile))**
- Orquestación: [reporte_pipeline.R](../../../worker/utils/reporte_pipeline.R) (fase 1 completa, fase pesada y render)
- Consultas y estado: [reporte_db.R](../../../worker/utils/reporte_db.R)
- Cálculos (Mann-Kendall, SPI, ENSO, etc.): [reporte_metricas.R](../../../worker/utils/reporte_metricas.R), [senales_curvas.R](../../../worker/utils/senales_curvas.R)
- Gráficos: [reporte_charts.R](../../../worker/utils/reporte_charts.R)
- Secciones y textos: [reporte_sections.R](../../../worker/utils/reporte_sections.R), [reporte_interpretacion.R](../../../worker/utils/reporte_interpretacion.R)
- Archivos y manifiesto: [reporte_storage.R](../../../worker/utils/reporte_storage.R)
- Constantes, paleta y versión del documento (`REPORTE_DOC_VERSION = "1.0"`): [reporte_constants.R](../../../worker/utils/reporte_constants.R)
- Markdown → PDF: [md2pdf.R](../../../worker/utils/md2pdf.R) con la plantilla [reporte_estacion.typ](../../../worker/templates/reporte_estacion.typ)
- Datos estáticos: [worker/data/oni.csv](../../../worker/data/oni.csv) (índice ENSO)
- Utilidades de la API que el worker carga: `db.R`, `calidad.R`, `alerta_temprana.R`, `alertas_catalog.R`, `alertas_skill.R`, `unidades_display.R`, `reporte_pdf_offer.R`

**Jobs**
- [actualizar_reportes_mensual.R](../../../worker/run/actualizar_reportes_mensual.R): corrida mensual
- [actualizar_reporte_pesado.R](../../../worker/run/actualizar_reporte_pesado.R), [actualizar_reporte_estacion.R](../../../worker/run/actualizar_reporte_estacion.R), [backfill_reporte_estacion.R](../../../worker/run/backfill_reporte_estacion.R), [reevaluar_reporte_elegibilidad.R](../../../worker/run/reevaluar_reporte_elegibilidad.R)
- Programación en Windows: `ingest/bat/actualizar_reportes_mensual.bat`, `actualizar_reporte_pesado.bat`, `backfill_reporte_estacion.bat`
- Smoke: [smoke_reporte_pdf.R](../../../worker/scripts/smoke_reporte_pdf.R)

**Entrega (API plumber)**
- [api/endpoints/reporte_pdf.R](../../../api/endpoints/reporte_pdf.R): `GET /estaciones/<id>/reporte/status` y `GET /estaciones/<id>/reporte.pdf`. El PDF exige cuenta y descuenta su tamaño de la cuota de descarga.
- [api/utils/reporte_pdf_offer.R](../../../api/utils/reporte_pdf_offer.R): reglas de oferta compartidas (elegible, congelado, pendiente).
- Registro en [api/plumber.R](../../../api/plumber.R). El header `X-Reporte-Estado` se expone por CORS.
- Aparte está el reporte JSON en pantalla: [api/endpoints/reporte.R](../../../api/endpoints/reporte.R) y [api/utils/reporte_payload.R](../../../api/utils/reporte_payload.R).

**Quién lo consume:** [shiny/modules/mapa.R](../../../shiny/modules/mapa.R), `nginx/html/mi-reporte/app.js`, `nginx/html/admin/app.js`.

### A3. Almacenamiento

`REPORTES_DIR=/data/reportes` (definido en api, api2 y worker en [docker-compose.yml](../../../docker-compose.yml)). Por estación: `/data/reportes/<id>/` con `reporte.pdf`, `reporte_completo.md`, `state.json`, `manifest.json`, `sections/` y `sections_pdf/`.

### A4. Límites del diseño actual frente al SEIA

- Está centrado en la **estación**, no en el **sitio del proyecto**: no recibe un área de influencia ni justifica la representatividad.
- Hay un solo PDF por estación, que se regenera todos los meses. No guarda una foto de los datos por informe, no tiene ID verificable y los números pueden cambiar entre una descarga y otra.
- No incluye los parámetros del Criterio de Cambio Climático del SEA (sectores A-N, +1,4 °C, p25/p75) ni normales en los tres períodos de referencia.

---

## Parte B. Prerrequisitos antes de construir el reporte SEIA

**Prioridad:** P0 es lo mínimo para un informe defendible, P1 diferencia el producto y P2 es opcional. Lo marcado *(práctica)* es una recomendación nuestra, no un requisito del SEA.
**Estado:** ✅ ya existe en Rodaja · 🟡 parcial · ❌ falta. El estado viene de revisar la BD el 2026-11-23 y hay que confirmarlo al empezar cada bloque.

### 1. Observaciones in situ

| # | Recurso | Prio | Estado | Nota |
|---|---|---|---|---|
| 1.1 | Series diarias de pp, Tmax, Tmin y Tmedia (DGA, DMC, INIA) | P0 | 🟡 | `jornada_cerrada` ya las tiene. Falta guardar junto a cada dato el **crudo, la bandera de calidad y la fecha de descarga** de forma trazable. |
| 1.2 | Evapotranspiración (el SEA pide temperatura, precipitación y ET desde estaciones cercanas o del sitio) | P0 | ❌ | No hay una variable de ET en el catálogo. Hay que calcularla (por ejemplo, Hargreaves o FAO-56 donde haya HR, viento y Rs) o conseguirla. |
| 1.3 | Humedad, viento (velocidad y dirección), radiación y presión, que son las variables de ARClim | P0/P1 | 🟡 | `hr_mean`, `u2_*`, `u_10_*`, `pa` y `rs_mean` existen, pero con poca cobertura (Rs en casi ninguna estación). |
| 1.4 | Resolución horaria de los últimos 3 años disponibles (lo recomienda la Guía para el componente aire) | P0/P1 | 🟡 | Existe la tabla `reloj_de_horas`. Falta revisar su cobertura por variable y estación. |
| 1.5 | Caudales DGA (para el ICE y el componente hídrico) | P1 | 🟡 | La variable `q` existe (~930 estaciones). |

### 2. Metadatos de estación (P0: sostienen la justificación de representatividad)

| # | Recurso | Estado | Nota |
|---|---|---|---|
| 2.1 | Coordenadas, altitud, institución, código, tipo y variables medidas | ✅ | `mirador` + `cantera` + `magnitud`. |
| 2.2 | Fecha de inicio y fin, estado (vigente o suspendida) | 🟡 | Hay `fecha_ini`, pero falta en 4.887 de 5.721 estaciones. No hay columnas de fecha de fin ni de estado explícito. |
| 2.3 | Historial de traslados y cambios de sensor | ❌ | |
| 2.4 | % de completitud por año y variable, y vacíos largos | 🟡 | La vista materializada `estacion_variable_anio` y `estacion_calidad.pct_completitud` existen. Falta un catálogo de vacíos largos. |
| 2.5 | Catastro histórico que incluya estaciones cerradas (el período 1980-2010 las necesita) | 🟡 | Hay que verificar que la ingesta no descarte las suspendidas. |

### 3. Productos grillados históricos

| # | Recurso | Prio | Estado | Nota |
|---|---|---|---|---|
| 3.1 | Raster diario propio de 5 km, con métricas de validación contra estaciones por macrozona | P0 | 🟡 | Ver [PLAN_EXPLORADOR_RASTER.md](../PLAN_EXPLORADOR_RASTER.md). Faltan las métricas de validación publicadas. |
| 3.2 | CR2MET v2.5 (CC BY 4.0, 1960-2021) como referencia y para contrastar | P0 | ❌ | Descargar desde Zenodo y citar. |
| 3.3 | ERA5-Land crudo como respaldo y para variables que no hay en estación | P1 | ❌ | Hay que verificar su licencia. |

### 4. Cambio climático según el SEA (P0)

| # | Recurso | Prio | Estado | Nota |
|---|---|---|---|---|
| 4.1 | Tabla 3 del Criterio: cambio porcentual de precipitación (p25 y p75) y elevación media por sector A-N | P0 | ❌ | Hay que digitalizarla y versionarla como parámetro. |
| 4.2 | **Capa de sectores A-N por subcuenca** | P0 | ❌ | **Es el principal riesgo técnico.** El Criterio solo trae la Figura 12 como mapa, sin shapefile ni tabla de subcuencas por sector. Hay que digitalizarla o pedírsela al SEA. Si un proyecto cae en dos sectores, el valor es un promedio ponderado por área, así que se necesita el % de área por sector. |
| 4.3 | Incremento fijo de 1,4 °C y la regla para elegir p25 o p75 según el factor de impacto | P0 | ❌ | Va como parámetro versionado de la metodología. |
| 4.4 | ARClim: más de 60 índices actuales y proyectados a 2035-2065, a 5×5 km, y riesgo comunal para 79 cadenas de impacto | P0 | ❌ | |
| 4.5 | Balance Hídrico Nacional DGA 2022 (4 GCM), que el Criterio usa como referencia comparativa | P1 | ❌ | |
| 4.6 | Proyecciones propias (el Anexo B las admite con CMIP6, SSP2-4.5 y SSP5-8.5, y corrección de sesgo) | P2 | ❌ | |

### 5. Sequía y escasez hídrica (P1)

La Guía pide considerar los decretos de escasez hídrica, los índices estandarizados de sequía de la DGA y la Resolución 1331/2022.

| # | Recurso | Estado | Nota |
|---|---|---|---|
| 5.1 | Estaciones de referencia que la DGA define por zona, y los umbrales y plazos de IPE (precipitación) e ICE (caudal) por región | ❌ | Fuente: MOP/DGA. |
| 5.2 | IPE e ICE calculados por estación, con varios períodos de acumulación | 🟡 | El reporte ya calcula el SPI (`reporte_metricas.R`). Faltan el ICE y la equivalencia con el IPE de la DGA. |
| 5.3 | Catálogo de decretos de escasez hídrica (zona, fechas, vigencia) | ❌ | |

### 6. Capas geoespaciales de contexto

| # | Recurso | Prio | Estado | Nota |
|---|---|---|---|---|
| 6.1 | Subcuencas DGA (la unidad de análisis del SEA), además de comunas, regiones y macrozonas | P0 | 🟡 | `mirador` ya trae `cod_subc`/`nom_subc`, comuna, región y `macrozona` como atributos. Faltan los **polígonos** (no hay PostGIS instalado: solo timescaledb, pgcrypto y plpgsql). |
| 6.2 | Carga del área de influencia por el cliente (KML, SHP o GeoJSON) | P0 | ❌ | Es el cambio de fondo: el informe pasa a ser por sitio. |
| 6.3 | DEM para gradientes altitudinales y la diferencia de altura entre sitio y estación | P0 *(práctica)* | ❌ | |
| 6.4 | Pisos vegetacionales de Luebert y Pliscoff (la Guía los recomienda como escala de ecosistema) | P2 | ❌ | |
| 6.5 | Köppen-Geiger y glaciares DGA | P2 *(práctica)* | ❌ | |

### 7. Variabilidad y eventos extremos (P1, práctica)

| # | Recurso | Estado | Nota |
|---|---|---|---|
| 7.1 | Índices ENSO y PDO | 🟡 | ONI ya está en `worker/data/oni.csv`. Falta el PDO. |
| 7.2 | Catálogo de eventos (aluviones, temporales, olas de calor, incendios) para sustentar la condición de riesgo climático que pide el Reglamento | 🟡 | Las alertas confirmadas (`campanada`, `acta_de_chequeo`) pueden alimentarlo. Faltan fuentes externas. |

### 8. Tablas derivadas precalculadas (P0)

Precalcularlas permite generar el informe en segundos y que los números no cambien entre una descarga y otra.

| # | Recurso | Estado | Nota |
|---|---|---|---|
| 8.1 | Normales mensuales y anuales por estación y por celda en tres períodos: 1980-2010 (SEA), 1961-1990 (base de la DMC, recomendada por la OMM) y los últimos 30 años | ❌ | Hoy el reporte calcula sobre todo el registro y en el momento. `clima_de_referencia` (mediana, MAD, p01 y p99 mensuales) sirve para control de calidad, no como normal. |
| 8.2 | Extremos anuales, ajuste GEV o Gumbel con períodos de retorno, días con helada, con lluvia y de calor extremo | 🟡 | Revisar qué calcula hoy `reporte_metricas.R`. |
| 8.3 | Tendencias (Sen y Mann-Kendall), SPI, completitud y homogeneidad | 🟡 | Mann-Kendall y SPI existen (en el momento). Faltan la homogeneidad y la persistencia en tablas. |
| 8.4 | Curvas IDF | ❌ | P2 *(práctica)*. |

### 9. Trazabilidad y gestión de informes (P0)

| # | Recurso | Estado | Nota |
|---|---|---|---|
| 9.1 | Foto de los datos usados en cada informe (fecha, versión, hash), versión de la metodología y de los parámetros del SEA | 🟡 | Ya existen `manifest.json`, `version_schema` y `REPORTE_DOC_VERSION`, pero se sobrescriben cada mes. Falta guardar una foto inmutable por informe. |
| 9.2 | Estaciones elegidas y la justificación de cada una | ❌ | Depende de 6.2 y 6.3. |
| 9.3 | Licencia y cita por fuente | ❌ | **Falta verificar los términos de DMC, DGA y ERA5-Land.** CR2MET es CC BY 4.0. |
| 9.4 | Registro de informes con ID y verificación por QR, más plantillas DOCX y PDF | ❌ | Hoy no hay tabla de registro: `ficha_del_mirador` es una fila por estación, no por informe. Plantilla actual: typst (solo PDF). |

---

## Orden sugerido para conseguirlos

1. Aclarar los `error_msg` de `ficha_del_mirador` (A1), para no construir sobre un pipeline que quizás está fallando sin avisar.
2. Pedir o digitalizar la **capa de sectores A-N** (4.2), porque es lo que más tarda y nadie más lo controla.
3. Verificar las licencias de DMC, DGA y ERA5-Land (9.3), que también depende de terceros.
4. Instalar PostGIS y cargar subcuencas, comunas, regiones y DEM (6.1, 6.3), que habilitan el área de influencia (6.2).
5. Agregar las normales en 3 períodos, los extremos y las tablas inmutables con el registro de informes (8.x, 9.1, 9.4).
