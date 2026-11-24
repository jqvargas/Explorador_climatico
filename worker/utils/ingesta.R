# =============================================================================
# Ingesta de observaciones — MOVIDO A ingest/ (host Windows)
# =============================================================================
# La ingesta externa (DGA, DMC, Task Scheduler) vive en:
#   ingest/run/actualizar_*.R
#   ingest/sources/<fuente>/adapter.R
#   ingest/lib/pipeline.R  -> persist + post_ingesta_observaciones()
#
# Este archivo se mantiene como referencia historica del worker Docker.
# El worker solo procesa exportaciones CSV (cola_descargas).

ingestar_observaciones <- function(conn, params) {
  stop(
    "Ingesta movida a ingest/ en el host. ",
    "Ejecute ingest/bat/actualizar_<fuente>.bat o Rscript ingest/run/actualizar_dga.R"
  )
}

run_ingesta_job <- function(job_id) {
  stop(
    "Ingesta movida a ingest/ (Task Scheduler + .bat). ",
    "No usar cola_ingesta en el worker Docker."
  )
}
