# Auditoria de requests - Explorador Climatico

## 1. Carga inicial de la aplicacion

### Estado actual (optimizado)

| # | Endpoint | Reactive | Consumidores | Cuando |
|---|----------|----------|--------------|--------|
| 1 | `GET /metadata?chile_only=1` | `metadata()` | variables, fuentes, regiones | Carga inicial |
| 2 | `GET /estaciones?chile_only=1&minimal=1` | `estaciones()` | mapa, contador, select estacion, tabla | Carga inicial |

**Total: 2 requests secuenciales** (metadata -> estaciones)

### Optimizaciones aplicadas

1. **Endpoint /metadata** (api/endpoints/fuentes.R): Une variables, fuentes y regiones en una sola llamada. Antes 3 requests separados.

2. **comunas() lazy**: Solo llama a `/comunas` cuando hay region seleccionada. Sin region devuelve vacio sin llamar API. Evita 1 request en carga inicial.

3. **tabla_estaciones**: Guard `panel_modo() != "tabla"` evita ejecutar tabla_data en carga (cuando panel esta cerrado o en modo serie). No hace llamadas extra al iniciar.

### Orden de ejecucion (Shiny)

1. metadata() corre -> 1 HTTP a /metadata
2. variables(), fuentes(), regiones() leen de metadata (sin HTTP)
3. estaciones() corre -> 1 HTTP a /estaciones
4. comunas() retorna vacio (region_aplicada = "") sin HTTP

Tiempo estimado: T_metadata + T_estaciones (secuencial)

---

## 2. Requests bajo demanda

| Endpoint | Trigger |
|----------|---------|
| `GET /comunas?region=X` | Usuario selecciona region |
| `GET /estaciones` (con filtros) | Usuario cambia operador, variable, region o comuna |
| `GET /datos` | Usuario clic "Ver datos" (modo serie) |
| `GET /estaciones/<id>/variables` | Modo tabla, por cada estacion (max 100) |
| `GET /datos/resumen` | Modo tabla, por cada estacion con variable seleccionada |

---

## 3. Mejora pendiente: paralelizar carga inicial

Actualmente metadata y estaciones corren **secuencialmente**. (No usar library(future) sin instalarlo en el Dockerfile y reconstruir la imagen)

Cambio propuesto: reactive `carga_paralela()` que lanza ambos requests en paralelo con `future::future()` y `future::value()`, reduciendo el tiempo a ~max(T_metadata, T_estaciones).

---

## 4. Resumen

- Carga inicial: 2 requests (antes 5)
- Comunas: lazy, solo con region
- Tabla: no corre hasta que usuario abre panel en modo tabla
- Posible mejora: paralelizar metadata + estaciones con future
