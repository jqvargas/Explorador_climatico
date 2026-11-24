#!/usr/bin/env Rscript
# PAUSA: el PDF por estacion esta detenido mientras se construye el Reporte personalizado
# (docs/producto/PLAN_REPORTE_PERSONALIZADO.md). Se reactiva con REPORTE_PDF_HABILITADO=1.
if (!identical(trimws(Sys.getenv("REPORTE_PDF_HABILITADO", "0")), "1")) {
  message("[reporte-pdf] en pausa (REPORTE_PDF_HABILITADO != 1); no se genera nada.")
  quit(save = "no", status = 0)
}
# Sync elegibilidad excelente → pendiente / congelado (sin generar PDF).
# Invocado desde actualizar_reportes_mensual.R; ya no forma parte del pipeline diario.

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
source(file.path(worker_root, "utils", "reporte_storage.R"))
source(file.path(worker_root, "utils", "reporte_db.R"))

worker_bootstrap_env(repo_root)
roots <- worker_resolve_data_roots(repo_root)
reporte_bootstrap_api_utils(roots$api)

tryCatch({
  conn <- get_db_connection()
  on.exit(tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL), add = TRUE)
  if (!reporte_tabla_existe(conn)) {
    message("[reporte-elegibilidad] tabla no existe, skip")
    quit(status = 0)
  }

  stats <- reporte_sync_elegibilidad(conn, log_fn = message)
  message("[reporte-elegibilidad] OK excelente=", stats$excelente,
          " nuevas=", stats$nuevas, " congeladas=", stats$congeladas,
          " reactivadas=", stats$reactivadas)
}, error = function(e) {
  message("[reporte-elegibilidad] ERROR: ", conditionMessage(e))
  quit(status = 1)
})
