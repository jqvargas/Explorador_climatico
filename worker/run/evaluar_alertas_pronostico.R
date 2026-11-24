#!/usr/bin/env Rscript
# Evaluacion alertas tempranas por pronostico (tras refresh_pronostico)

locate_worker_root <- function() {
  fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  run_dir <- normalizePath(dirname(sub("^--file=", "", fa[1])), winslash = "/", mustWork = FALSE)
  cand_app <- normalizePath(file.path(run_dir, ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(cand_app, "utils", "worker_bootstrap.R"))) return(cand_app)
  cand_repo <- normalizePath(file.path(run_dir, "..", ".."), winslash = "/", mustWork = FALSE)
  file.path(cand_repo, "worker")
}

worker_root <- locate_worker_root()
repo_root <- if (dir.exists(file.path(dirname(worker_root), "api"))) {
  dirname(worker_root)
} else if (dir.exists(file.path(dirname(worker_root), "shiny"))) {
  dirname(worker_root)
} else {
  worker_root
}

source(file.path(worker_root, "utils", "worker_bootstrap.R"))
source(file.path(worker_root, "utils", "db.R"))
source(file.path(worker_root, "utils", "alertas_mensajes.R"))
source(file.path(worker_root, "utils", "email.R"))
source(file.path(worker_root, "utils", "alertas_top10.R"))
source(file.path(worker_root, "utils", "alertas_pronostico.R"))
source(file.path(worker_root, "utils", "alertas_temprana_sql.R"))
api_skill <- file.path(Sys.getenv("API_ROOT", "/api"), "utils", "alertas_skill.R")
if (!file.exists(api_skill)) api_skill <- file.path(repo_root, "api", "utils", "alertas_skill.R")
if (file.exists(api_skill)) source(api_skill)
source(file.path(worker_root, "utils", "profile_spans.R"))

worker_bootstrap_env(repo_root)
profile_worker_attach("alertas_pronostico")

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
limit <- NULL
idx <- which(args == "--limit")
if (length(idx) > 0L && idx[1L] < length(args)) {
  limit <- suppressWarnings(as.integer(args[idx[1L] + 1L]))
  if (is.na(limit) || limit < 1L) limit <- NULL
}

tryCatch({
  evaluar_alertas_pronostico(repo_root, dry_run = dry_run, limit = limit)
}, error = function(e) {
  message("[alertas:pronostico] ERROR: ", conditionMessage(e))
  quit(status = 1)
})
