# Plantilla para agregar una nueva fuente de datos

1. Cree `ingest/sources/<fuente>/` y copie aqui sus scripts de scraping.
2. Implemente `adapter.R` con funciones `fetch_*` que devuelvan un data.frame con:
   - `codigo_nacional`, `year`, `month`, `day` y columna de valor, **o**
   - `id_estacion`, `id_variable`, `fecha`, `valor` (ya resuelto).
3. Cree `ingest/run/actualizar_<fuente>.R` que llame `ingest_run_all_variables()`.
4. Agregue `ingest/bat/actualizar_<fuente>.bat` para Task Scheduler.

El adapter **no** debe escribir en la BD; solo descarga y normaliza.
