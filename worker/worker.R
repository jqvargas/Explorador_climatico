#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(dotenv)
  library(redux)
  library(DBI)
  library(RPostgres)
  library(jsonlite)
  library(glue)
})
if (file.exists(".env")) load_dot_env()
base <- getwd()
source(file.path(base, "utils", "db.R"))
source(file.path(base, "utils", "exportar.R"))
source(file.path(base, "utils", "email.R"))

redis_host <- Sys.getenv("REDIS_HOST", "redis")
redis_port <- as.integer(Sys.getenv("REDIS_PORT", "6379"))

`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

run_job <- function(job_id) {
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn))

  # Get job row
  row <- DBI::dbGetQuery(conn, "SELECT id, estado, email, params FROM trabajos_descarga WHERE id = $1", params = list(job_id))
  if (nrow(row) == 0) {
    message("Job ", job_id, " no encontrado")
    return(invisible(NULL))
  }
  params <- jsonlite::fromJSON(row$params[1])
  email_dest <- row$email[1]
  if (is.na(email_dest)) email_dest <- ""

  DBI::dbExecute(conn, "UPDATE trabajos_descarga SET estado = $1 WHERE id = $2", params = list("procesando", job_id))

  archivo_tmp <- file.path(tempdir(), paste0("descarga_", job_id, ".csv"))
  tryCatch({
    generar_archivo(params, archivo_tmp, params$formato %||% "csv")
    link_descarga <- NA_character_

    r2_bucket <- Sys.getenv("R2_BUCKET")
    r2_key <- Sys.getenv("R2_KEY_PREFIX", "")
    if (!is.na(r2_bucket) && r2_bucket != "" && file.exists(archivo_tmp)) {
      if (requireNamespace("paws", quietly = TRUE)) {
        # TODO: upload to R2 via paws S3-compatible
        # link_descarga <- public URL or presigned
        # For now skip if not configured
      }
    }
    if (is.na(link_descarga) || link_descarga == "") {
      link_descarga <- paste0("/descarga/archivo/", job_id)
    }
    DBI::dbExecute(conn, "UPDATE trabajos_descarga SET estado = $1, link_descarga = $2, completado_en = NOW() WHERE id = $3",
      params = list("listo", link_descarga, job_id))
    if (email_dest != "") {
      enviar_link_descarga(email_dest, link_descarga, job_id)
    }
  }, error = function(e) {
    DBI::dbExecute(conn, "UPDATE trabajos_descarga SET estado = $1 WHERE id = $2", params = list("error", job_id))
    message("Error en job ", job_id, ": ", e$message)
  })
  descargas_dir <- Sys.getenv("DESCARGAS_DIR", "/tmp/descargas")
  if (dir.exists(descargas_dir) && file.exists(archivo_tmp)) {
    dest <- file.path(descargas_dir, paste0(job_id, ".csv"))
    file.copy(archivo_tmp, dest, overwrite = TRUE)
  }
  if (file.exists(archivo_tmp)) try(file.remove(archivo_tmp), silent = TRUE)
}

r <- redux::hiredis(host = redis_host, port = redis_port)
message("Worker conectado a Redis. Esperando trabajos en cola_descargas...")
while (TRUE) {
  res <- r$BLPOP("cola_descargas", 10)
  if (is.null(res)) next
  job_id <- res[[2]]
  message("Procesando job: ", job_id)
  run_job(job_id)
}

