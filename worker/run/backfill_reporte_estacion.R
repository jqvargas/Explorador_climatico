#!/usr/bin/env Rscript
# PAUSA: el PDF por estacion esta detenido mientras se construye el Reporte personalizado
# (docs/producto/PLAN_REPORTE_PERSONALIZADO.md). Se reactiva con REPORTE_PDF_HABILITADO=1.
if (!identical(trimws(Sys.getenv("REPORTE_PDF_HABILITADO", "0")), "1")) {
  message("[reporte-pdf] en pausa (REPORTE_PDF_HABILITADO != 1); no se genera nada.")
  quit(save = "no", status = 0)
}
# Fase 1: backfill reporte PDF por estacion (solo excelente).

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
eids <- NULL
limit <- NULL
for (a in args) {
  if (grepl("^--eid=", a)) eids <- c(eids, as.integer(sub("^--eid=", "", a)))
  if (grepl("^--eids=", a)) {
    eids <- as.integer(strsplit(sub("^--eids=", "", a), ",", fixed = TRUE)[[1]])
  }
  if (grepl("^--limit=", a)) limit <- as.integer(sub("^--limit=", "", a))
}

tryCatch({
  conn <- get_db_connection()
  on.exit(tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL), add = TRUE)

  if (!reporte_tabla_existe(conn)) stop("Falta la tabla ficha_del_mirador")

  if (is.null(eids) || length(eids) == 0) {
    eids <- reporte_eids_excelente(conn)
    pend <- reporte_db_list_pendientes(conn)
    eids <- unique(c(pend, eids))
  }
  eids <- eids[!is.na(eids) & eids > 0]
  if (!is.null(limit) && length(eids) > limit) eids <- eids[seq_len(limit)]

  message("[reporte-backfill] estaciones=", length(eids))
  for (eid in eids) {
    res <- tryCatch(reporte_pipeline_phase1(conn, eid), error = function(e) {
      reporte_db_upsert(conn, eid, list(estado = "error", error_msg = conditionMessage(e)))
      list(ok = FALSE, error = conditionMessage(e))
    })
    if (isTRUE(res$ok)) reporte_log_timing("backfill", eid, res$seconds)
  }
  message("[reporte-backfill] OK")
}, error = function(e) {
  message("[reporte-backfill] ERROR: ", conditionMessage(e))
  quit(status = 1)
})
