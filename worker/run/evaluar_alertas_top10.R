#!/usr/bin/env Rscript
# Evaluacion diaria de alertas top 10 (ejecutar tras ingesta)

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
# Docker: /app es worker; host: repo/worker -> repo_root = repo
repo_root <- if (dir.exists(file.path(worker_root, "utils")) &&
                  !dir.exists(file.path(worker_root, "..", "worker"))) {
  worker_root
} else if (dir.exists(file.path(dirname(worker_root), "api"))) {
  dirname(worker_root)
} else {
  normalizePath(file.path(worker_root, ".."), winslash = "/", mustWork = FALSE)
}

source(file.path(worker_root, "utils", "worker_bootstrap.R"))
source(file.path(worker_root, "utils", "db.R"))
source(file.path(worker_root, "utils", "alertas_mensajes.R"))
source(file.path(worker_root, "utils", "email.R"))
source(file.path(worker_root, "utils", "alertas_tardia_sql.R"))
source(file.path(worker_root, "utils", "alertas_top10.R"))
source(file.path(worker_root, "utils", "profile_spans.R"))

worker_bootstrap_env(repo_root)
profile_worker_attach("alertas_top10")

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args

tryCatch({
  evaluar_alertas_top10(repo_root, dry_run = dry_run)
}, error = function(e) {
  message("[alertas] ERROR: ", conditionMessage(e))
  quit(status = 1)
})
