# Auditoria de requests - Explorador Climatico

## 1. Carga inicial de la aplicacion

| # | Endpoint | Reactive | Cuando | Cache API |
|---|----------|----------|--------|-----------|
| 1 | `GET /metadata?chile_only=1` | `metadata()` | Carga inicial | Redis 24h |
| 2 | `GET /estaciones?chile_only=1&minimal=1` | `estaciones()` | Carga inicial | Redis 15min |

**Total: 2 requests** (metadata -> estaciones). Ambos sirven desde Redis tras la primera carga.

## 2. Requests bajo demanda

| Endpoint | Trigger | Proyeccion / Cache |
|----------|---------|--------------------|
| `GET /comunas?region=X` | Selecciona region | Redis 24h |
| `GET /estaciones` (con filtros) | Cambia operador/variable/region/comuna | Redis 15min por combinacion |
| `GET /datos` | Clic "Ver datos" (modo serie) | Solo `fecha,valor` + `meta` aparte; paginacion por cursor; Redis 10min |
| `GET /analisis-tabla?ids=...&variable_id=` | Modo tabla (1 sola llamada para N estaciones) | Redis 30min |
| `GET /estaciones/<id>/variables` | Popup de mapa | Redis 30min |
| `GET /estaciones/cercanas?lat=&lon=&id_variable=` | Wizard mi estacion / PWA | Redis 15min |
| `GET /estaciones/<id>/reporte?id_variable=` | PWA `/mi-reporte/` | Redis 10min |

## 3. Optimizaciones implementadas

1. **Proyeccion estricta en `/datos`**: ya no hace JOIN a estacion/variable ni repite
   nombres en cada fila. Devuelve `{ meta, data:[{fecha,valor}], next_cursor }`.
   Los metadatos de la serie se consultan una vez y se cachean.
2. **Paginacion por cursor** (`limit` + `cursor`) y tope de filas (50.000) en `/datos`.
3. **Endpoint batch `/analisis-tabla`**: reemplaza el patron N+1 de la tabla
   (antes ~2N llamadas; ahora 1 request -> 1-2 queries agregadas).
4. **Cache-aside con Redis** (`api/utils/cache.R`) en todas las lecturas idempotentes,
   con degradacion elegante si Redis no esta disponible. Header `X-Cache: HIT|MISS`.
5. **Pool de conexiones** (`api/utils/db.R`, paquete `pool`) reutilizado por request;
   helpers `db_query`/`db_execute` centralizan el acceso a BD.
6. **Ventana por defecto** del grafico reducida (365 dias) en lugar de 730.
7. **Indices** consolidados en `api/scripts/add_indexes_estaciones.sql`.

## 4. Invalidacion de cache

Tras ingestar datos nuevos, llamar `cache_invalidate("datos:*")`,
`cache_invalidate("resumen:*")`, `cache_invalidate("estaciones:*")` segun corresponda.
Los TTL acotan la obsolescencia incluso sin invalidacion explicita.
