#!/usr/bin/env Rscript
# PAUSA: el PDF por estacion esta detenido mientras se construye el Reporte personalizado
# (docs/producto/PLAN_REPORTE_PERSONALIZADO.md). Se reactiva con REPORTE_PDF_HABILITADO=1.
if (!identical(trimws(Sys.getenv("REPORTE_PDF_HABILITADO", "0")), "1")) {
  message("[reporte-pdf] en pausa (REPORTE_PDF_HABILITADO != 1); no se genera nada.")
  quit(save = "no", status = 0)
}
# DEPRECATED: Fase 2 diaria eliminada; usar run/actualizar_reportes_mensual.R (Heavy mensual).
# Fase 2: actualizacion diaria incremental de reportes PDF.

locate_worker_root <- function() {
  fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  run_dir <- normalizePath(dirname(sub("^--file=", "", fa[1])), winslash = "/", mustWork = FALSE)
  cand_app <- normalizePath(file.path(run_dir, ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(cand_app, "utils", "worker_bootstrap.R"))) return(cand_app)
  cand_repo <- normalizePath(file.path(run_dir, "..", ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(cand_repo, "worker", "utils", "worker_bootstrap.R"))) {
    return(file.path(cand_repo, "worker"))
  }
  stop("No se encuentra worker/utils/worker_bootstrap.R")
}

worker_root <- locate_worker_root()
repo_root <- if (dir.exists(file.path(dirname(worker_root), "api"))) dirname(worker_root) else worker_root

source(file.path(worker_root, "utils", "worker_bootstrap.R"))
source(file.path(worker_root, "utils", "db.R"))
source(file.path(worker_root, "utils", "reporte_pipeline.R"))

worker_bootstrap_env(repo_root)
roots <- worker_resolve_data_roots(repo_root)
reporte_source_all(roots$api)

args <- commandArgs(trailingOnly = TRUE)
fecha_desde <- NULL
eids_arg <- NULL
for (a in args) {
  if (grepl("^--desde=", a)) fecha_desde <- sub("^--desde=", "", a)
  if (grepl("^--eid=", a)) eids_arg <- c(eids_arg, as.integer(sub("^--eid=", "", a)))
}

tryCatch({
  conn <- get_db_connection()
  on.exit(tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL), add = TRUE)
  if (!reporte_tabla_existe(conn)) {
    message("[reporte-diario] tabla ficha_del_mirador no existe, skip")
    quit(status = 0)
  }

  eids <- if (!is.null(eids_arg) && length(eids_arg) > 0) eids_arg else reporte_db_list_activos(conn)
  message("[reporte-diario] estaciones activas=", length(eids))
  for (eid in eids) {
    res <- tryCatch(
      reporte_pipeline_phase2(conn, eid, fecha_desde = fecha_desde),
      error = function(e) {
        reporte_db_upsert(conn, eid, list(error_msg = conditionMessage(e)))
        list(ok = FALSE, error = conditionMessage(e))
      }
    )
    if (isTRUE(res$ok)) reporte_log_timing("diario", eid, res$seconds)
  }
  message("[reporte-diario] OK")
}, error = function(e) {
  message("[reporte-diario] ERROR: ", conditionMessage(e))
  quit(status = 1)
})
