#!/usr/bin/env Rscript
# Refresh diario pronostico Open-Meteo (estaciones excelente/buena).

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
source(file.path(worker_root, "utils", "openmeteo_bias.R"))
source(file.path(worker_root, "utils", "openmeteo.R"))
source(file.path(worker_root, "utils", "profile_spans.R"))

worker_bootstrap_env(repo_root)
profile_worker_attach("refresh_pronostico")

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
limit <- NULL
eids <- NULL
for (a in args) {
  if (grepl("^--limit=", a)) limit <- as.integer(sub("^--limit=", "", a))
  if (grepl("^--eids=", a)) {
    eids <- as.integer(strsplit(sub("^--eids=", "", a), ",", fixed = TRUE)[[1]])
    eids <- eids[!is.na(eids) & eids > 0L]
  }
}

tryCatch({
  conn <- get_db_connection()
  on.exit(tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL), add = TRUE)

  chk <- DBI::dbGetQuery(
    conn,
    "SELECT 1 AS ok FROM information_schema.tables WHERE table_name = 'carta_del_mirador' LIMIT 1"
  )
  if (nrow(chk) == 0) stop("Falta la tabla carta_del_mirador")
  chk_h <- DBI::dbGetQuery(
    conn,
    "SELECT 1 AS ok FROM information_schema.tables WHERE table_name = 'carta_por_hora' LIMIT 1"
  )
  if (nrow(chk_h) == 0) stop("Falta la tabla carta_por_hora")

  res <- openmeteo_refresh_all(conn, eids = eids, limit = limit, dry_run = dry_run)
  message(
    "[pronostico] OK estaciones=", res$n_estaciones,
    " upserts=", res$n_upserts %||% 0,
    " upserts_hora=", res$n_upserts_hora %||% 0
  )
}, error = function(e) {
  message("[pronostico] ERROR: ", conditionMessage(e))
  quit(status = 1)
})
