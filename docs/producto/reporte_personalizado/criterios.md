# Criterios: Reporte personalizado

La etapa N solo actualiza las filas con `etapa_owner = N`. Status posibles: `pendiente` | `en_curso` | `ok` | `gap_documentado` | `no_aplica`.

| id | criterio | etapa_owner | status | evidencia | gap |
|----|----------|-------------|--------|-----------|-----|
| R0 | D1 a D7 tienen una respuesta escrita en la §3 del plan maestro | 0 | pendiente | | |
| R1 | `postgis` aparece en `pg_extension` de `clima`, `mirador.geom` tiene índice GiST, y `requireNamespace("sf")` da TRUE en `api` y `worker` | 1 | pendiente | (2026-11-24, ya instalado: ver §2.2 del maestro) | |
| R2 | `geo_poligono` tiene subcuencas, comunas, regiones y macrozonas, con fuente, licencia y versión. El script de carga es reproducible | 1 | pendiente | | |
| R3 | `geo_contexto()` de un punto conocido (una estación con `cod_subc`) devuelve la misma subcuenca que `mirador.cod_subc` | 1 | pendiente | | |
| R4 | `geo_leer_archivo()` acepta KML, GeoJSON y SHP comprimido, y rechaza lo que se pasa de los límites de D4 con un mensaje claro | 1 | pendiente | | |
| R5 | `POST /reporte-personalizado/previsualizar` con un punto responde con las candidatas ordenadas por distancia, sin escribir en la BD | 2 | pendiente | | |
| R6 | `POST /reporte-personalizado` sin cuenta da 401. Con cuenta crea una fila `pendiente` y sus `reporte_solicitud_estacion` | 2 | pendiente | | |
| R7 | `GET …/<id>/reporte.pdf` de otro usuario da 403 o 404. El timeout de nginx no es el de 8 s de los listados | 2 | pendiente | | |
| R8 | El diagnóstico de los `error_msg` de `ficha_del_mirador` queda escrito en el handoff de la etapa 3 | 3 | pendiente | | |
| R9 | El smoke de punta a punta deja la solicitud en `listo` con `datos_hash` y `pdf_sha256`. Recalcular el sha256 del archivo da el mismo valor | 3 | pendiente | | |
| R10 | Correr dos veces la misma geometría y parámetros sin datos nuevos da el mismo `datos_hash` | 3 | pendiente | | |
| R11 | Una solicitud que se recupera de un error deja `error_msg` en NULL | 3 | pendiente | | |
| R12 | La entrada de NAV aparece en escritorio y en el TWA, y la ruta queda activa en `/reporte-personalizado/` | 4 | pendiente | | |
| R13 | Los flujos de punto, polígono y KML llegan a descargar el PDF en el navegador. `?eid=` precarga la estación | 4 | pendiente | | |
| R14 | `/verificar/<codigo>` muestra solo los campos permitidos por la §4. Comparar el sha256 en el navegador funciona y el PDF no se sube | 5 | pendiente | | |
| R15 | La plantilla SEIA se emite solo con los prerrequisitos P0 disponibles; si no, la etapa queda `bloqueado` con la lista | 6 | pendiente | | |
| R16 | El botón PDF de la estación abre la sección nueva. El endpoint viejo responde con `Deprecation` y sigue entregando el archivo congelado | 7 | pendiente | | |
| R17 | `CIERRE.md` tiene los tiempos (p50 y peor caso) y la corrida en escritorio y teléfono | 8 | pendiente | | |
