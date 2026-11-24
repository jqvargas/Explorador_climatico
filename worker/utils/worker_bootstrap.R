# Bootstrap env para scripts worker/run (ingest.env + .env, preserva DB_*).

worker_bootstrap_env <- function(project_root) {
  if (!requireNamespace("dotenv", quietly = TRUE)) return(invisible(project_root))
  preserve <- c("DB_HOST", "DB_PORT", "DB_NAME", "DB_USER", "DB_PASS", "DB_PASSWORD")
  saved <- Sys.getenv(preserve, unset = NA)
  is_docker_worker <- dir.exists(file.path(project_root, "utils")) && !dir.exists(file.path(project_root, "worker"))
  ingest_env <- if (is_docker_worker) {
    file.path(project_root, "..", "ingest", "config", "ingest.env")
  } else {
    file.path(project_root, "ingest", "config", "ingest.env")
  }
  dotenv_path <- if (file.exists(ingest_env)) {
    ingest_env
  } else if (file.exists(file.path(project_root, ".env"))) {
    file.path(project_root, ".env")
  } else if (file.exists(file.path(project_root, "..", ".env"))) {
    file.path(project_root, "..", ".env")
  } else {
    NULL
  }
  if (!is.null(dotenv_path)) dotenv::load_dot_env(dotenv_path)
  for (k in preserve) {
    v <- saved[[k]]
    if (!is.na(v) && nzchar(v)) do.call(Sys.setenv, setNames(list(v), k))
  }
  invisible(project_root)
}

# Rutas shiny/api: env Docker (/shiny, /api) o layout repo en host.
worker_resolve_data_roots <- function(project_root = NULL) {
  shiny <- Sys.getenv("SHINY_ROOT", "")
  api <- Sys.getenv("API_ROOT", "")
  if (nzchar(shiny) && dir.exists(shiny)) {
    # ok
  } else {
    shiny <- ""
    for (c in c("/shiny", file.path(project_root, "shiny"), file.path(project_root, "..", "shiny"))) {
      if (nzchar(c) && dir.exists(c)) { shiny <- c; break }
    }
  }
  if (nzchar(api) && dir.exists(api)) {
    # ok
  } else {
    api <- ""
    for (c in c("/api", file.path(project_root, "api"), file.path(project_root, "..", "api"))) {
      if (nzchar(c) && dir.exists(c)) { api <- c; break }
    }
  }
  list(shiny = shiny, api = api)
}

worker_script_project_root <- function() {
  fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  run_dir <- normalizePath(dirname(sub("^--file=", "", fa[1])), winslash = "/", mustWork = FALSE)
  cand_app <- normalizePath(file.path(run_dir, ".."), winslash = "/", mustWork = FALSE)
  cand_repo <- normalizePath(file.path(run_dir, "..", ".."), winslash = "/", mustWork = FALSE)
  if (dir.exists(file.path(cand_app, "utils")) && dir.exists(file.path(cand_app, "run"))) {
    return(cand_app)
  }
  if (dir.exists(file.path(cand_repo, "worker")) && dir.exists(file.path(cand_repo, "api"))) {
    return(cand_repo)
  }
  cand_repo
}
