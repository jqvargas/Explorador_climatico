# Bootstrap del modulo ingest: rutas, env y carga de librerias compartidas.
# Llamar ingest_setup_from_script() al inicio de cada script en ingest/run/.

ingest_init <- function() {
  # Dia de ejecucion = calendario Chile (no UTC del contenedor).
  if (!nzchar(Sys.getenv("TZ", ""))) {
    Sys.setenv(TZ = "America/Santiago")
  }

  if (exists("INGEST_DIR", envir = .GlobalEnv, inherits = FALSE)) {
    ingest_dir <- get("INGEST_DIR", envir = .GlobalEnv)
  } else {
    ingest_dir <- normalizePath(
      file.path(getwd(), "ingest"),
      winslash = "/",
      mustWork = FALSE
    )
    if (!dir.exists(ingest_dir)) {
      stop("No se encontro ingest/. Ejecute desde la raiz del proyecto o defina INGEST_DIR.")
    }
  }

  project_root <- normalizePath(file.path(ingest_dir, ".."), winslash = "/")
  assign("INGEST_DIR", ingest_dir, envir = .GlobalEnv)
  assign("PROJECT_ROOT", project_root, envir = .GlobalEnv)

  ingest_env <- Sys.getenv("INGEST_ENV", file.path(ingest_dir, "config", "ingest.env"))
  preserve <- c("DB_HOST", "DB_PORT", "DB_NAME", "DB_USER", "DB_PASS", "DB_PASSWORD", "REDIS_HOST", "REDIS_PORT")
  saved <- Sys.getenv(preserve, unset = NA)
  if (file.exists(ingest_env) && requireNamespace("dotenv", quietly = TRUE)) {
    dotenv::load_dot_env(ingest_env)
    message("[ingest] Env cargado: ", ingest_env)
  } else if (file.exists(file.path(project_root, ".env")) && requireNamespace("dotenv", quietly = TRUE)) {
    dotenv::load_dot_env(file.path(project_root, ".env"))
    message("[ingest] Env cargado: ", file.path(project_root, ".env"))
  }
  for (k in preserve) {
    v <- saved[[k]]
    if (!is.na(v) && nzchar(v)) {
      do.call(Sys.setenv, setNames(list(v), k))
    }
  }

  lib_dir <- file.path(ingest_dir, "lib")
  worker_utils <- file.path(project_root, "worker", "utils")

  source(file.path(lib_dir, "db.R"), local = FALSE)
  source(file.path(lib_dir, "normalize.R"), local = FALSE)
  source(file.path(lib_dir, "catalog.R"), local = FALSE)
  source(file.path(worker_utils, "qc.R"), local = FALSE)
  source(file.path(worker_utils, "qc_multinivel.R"), local = FALSE)
  source(file.path(lib_dir, "persist.R"), local = FALSE)
  source(file.path(worker_utils, "observacion_diaria.R"), local = FALSE)
  source(file.path(worker_utils, "cobertura.R"), local = FALSE)
  source(file.path(lib_dir, "pipeline.R"), local = FALSE)
  source(file.path(lib_dir, "meteovision_init.R"), local = FALSE)
  source(file.path(lib_dir, "checkpoint.R"), local = FALSE)
  source(file.path(lib_dir, "scrape_rate_limit.R"), local = FALSE)
  source(file.path(lib_dir, "scrape_dead_stations.R"), local = FALSE)
  source(file.path(lib_dir, "dga_deferred_pool.R"), local = FALSE)
  source(file.path(lib_dir, "ingest_exit_decision.R"), local = FALSE)
  source(file.path(lib_dir, "profile_spans.R"), local = FALSE)
  source(file.path(lib_dir, "orchestrate_meteovision.R"), local = FALSE)

  invisible(list(ingest_dir = ingest_dir, project_root = project_root))
}

ingest_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0L) {
    return(normalizePath(getwd(), winslash = "/"))
  }
  normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/")
}

ingest_setup_from_script <- function() {
  run_dir <- ingest_script_dir()
  ingest_dir <- normalizePath(file.path(run_dir, ".."), winslash = "/")
  assign("INGEST_DIR", ingest_dir, envir = .GlobalEnv)
  ingest_init()
}
