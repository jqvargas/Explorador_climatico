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
source(file.path(base, "utils", "cobertura.R"))
source(file.path(base, "utils", "ingesta.R"))
source(file.path(base, "utils", "cuota.R"))
source(file.path(base, "utils", "flow_charge.R"))
source(file.path(base, "utils", "on_demand.R"))
source(file.path(base, "utils", "lote.R"))
source(file.path(base, "utils", "alertas_top10.R"))
source(file.path(base, "utils", "alertas_push.R"))

redis_host <- Sys.getenv("REDIS_HOST", "redis")
redis_port <- as.integer(Sys.getenv("REDIS_PORT", "6379"))

# is.na() de un character(0) no es TRUE/FALSE: el if reventaba el boletin
# despues de enviarlo, y el tick lo reenviaba cada minuto hasta las 22:00.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
stopifnot(identical(character(0) %||% "", ""))

worker_app_profile_beacon <- function(trace_id, spans) {
  if (!nzchar(as.character(trace_id %||% ""))) return(invisible(NULL))
  api <- Sys.getenv("API_URL", "http://api:8000")
  body <- jsonlite::toJSON(list(trace_id = as.character(trace_id), spans = spans),
                           auto_unbox = TRUE, null = "null")
  tryCatch({
    if (requireNamespace("httr", quietly = TRUE)) {
      httr::POST(paste0(sub("/$", "", api), "/app-profile/beacon"),
                 httr::timeout(3), httr::content_type_json(), body = body)
    } else {
      # curl fallback
      tf <- tempfile()
      writeLines(body, tf)
      system2("curl", c("-sS", "-m", "3", "-H", "Content-Type: application/json",
                        "-d", paste0("@", tf), paste0(sub("/$", "", api), "/app-profile/beacon")),
               stdout = FALSE, stderr = FALSE)
      unlink(tf)
    }
  }, error = function(e) NULL)
  invisible(NULL)
}

run_job <- function(job_id) {
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn))

  row <- DBI::dbGetQuery(conn,
    "SELECT id, estado, email, params, user_id, bytes_estimados, bytes_overage, bytes_extra, registros_estimados, granularidad, formato_descarga
     FROM encargo_de_archivo WHERE id = $1",
    params = list(job_id)
  )
  if (nrow(row) == 0) {
    message("Job ", job_id, " no encontrado")
    return(invisible(NULL))
  }

  params <- jsonlite::fromJSON(row$params[1])
  params$job_id <- job_id
  params$registros_estimados <- as.integer(row$registros_estimados[1])
  email_dest <- row$email[1]
  if (is.na(email_dest)) email_dest <- ""
  user_id <- row$user_id[1]
  estimado <- row$bytes_estimados[1]
  overage_est <- suppressWarnings(as.integer(row$bytes_overage[1]))
  if (is.na(overage_est)) overage_est <- 0L
  extra_est <- suppressWarnings(as.integer(row$bytes_extra[1]))
  if (is.na(extra_est)) extra_est <- 0L
  if (is.na(estimado) || estimado < 1L) {
    estimado <- as.integer(row$registros_estimados[1]) * 28L
  }
  tier <- worker_resolve_tier(conn, user_id)

  trace_id <- as.character(params$app_trace_id %||% "")
  if (length(trace_id) != 1L || is.na(trace_id)) trace_id <- ""
  t_job <- Sys.time()
  if (nzchar(trace_id)) {
    worker_app_profile_beacon(trace_id, list(list(
      name = "worker.job.start", cat = "worker", ts = 0, dur_ms = 0,
      attrs = list(lane = "worker", job_id = job_id)
    )))
  }

  DBI::dbExecute(conn, "UPDATE encargo_de_archivo SET estado = $1 WHERE id = $2", params = list("procesando", job_id))

  descargas_dir <- Sys.getenv("DESCARGAS_DIR", "/tmp/descargas")
  if (!dir.exists(descargas_dir)) dir.create(descargas_dir, recursive = TRUE, showWarnings = FALSE)
  archivo_tmp <- file.path(descargas_dir, paste0(job_id, ".csv"))

  tryCatch({
    result <- generar_archivo(params, archivo_tmp, params$formato_descarga %||% "liviano")
    real_bytes <- if (is.list(result)) result$bytes_reales else 0L
    out_path <- if (is.list(result)) result$path else archivo_tmp
    archivo_nombre <- if (is.list(result)) result$archivo_nombre else basename(out_path)
    link_descarga <- paste0("/descarga/archivo/", job_id)

    params$archivo_nombre <- archivo_nombre
    params_json <- jsonlite::toJSON(params, auto_unbox = TRUE)

    tryCatch({
      DBI::dbExecute(conn,
        "UPDATE encargo_de_archivo SET estado = $1, link_descarga = $2, completado_en = NOW(), bytes_reales = $4, params = $5::jsonb, archivo_nombre = $6 WHERE id = $3",
        params = list("listo", link_descarga, job_id, real_bytes, params_json, archivo_nombre)
      )
    }, error = function(e) {
      DBI::dbExecute(conn,
        "UPDATE encargo_de_archivo SET estado = $1, link_descarga = $2, completado_en = NOW(), bytes_reales = $4, params = $5::jsonb WHERE id = $3",
        params = list("listo", link_descarga, job_id, real_bytes, params_json)
      )
    })

    if (nzchar(trace_id)) {
      worker_app_profile_beacon(trace_id, list(list(
        name = "worker.job.done", cat = "worker",
        dur_ms = as.numeric(difftime(Sys.time(), t_job, units = "secs")) * 1000,
        attrs = list(lane = "worker", job_id = job_id, bytes = real_bytes, status = "listo")
      )))
    }

    lote_id <- params$lote_id %||% NULL
    is_lote_child <- !is.null(lote_id) && nzchar(as.character(lote_id))

    if (!is_lote_child) {
      limite <- worker_get_limite_bytes(tier)
      periodo <- worker_periodo_inicio(conn, user_id, tier)
      df_u <- DBI::dbGetQuery(conn,
        "SELECT bytes_consumidos FROM cupo_del_periodo WHERE user_id = $1::uuid AND periodo_inicio = $2",
        params = list(user_id, periodo)
      )
      usado <- if (nrow(df_u) > 0) as.numeric(df_u$bytes_consumidos[1]) else 0
      u0 <- usado - real_bytes
      overage_real <- if (extra_est > 0L) NULL else as.integer(worker_on_demand_overage_increment(u0, limite, real_bytes))
      worker_ajustar_cuota(conn, user_id, tier, estimado, real_bytes, overage_est, overage_real, extra_est)
    }

    if (is_lote_child) {
      tryCatch(lote_maybe_finalize(conn, as.character(lote_id)), error = function(e) {
        message("Error finalizando lote ", lote_id, ": ", e$message)
      })
    }

    if (email_dest != "" && !is_lote_child) {
      enviar_link_descarga(email_dest, link_descarga, job_id)
    }

    if (file.exists(out_path) && !identical(normalizePath(out_path), normalizePath(file.path(descargas_dir, basename(out_path))))) {
      file.copy(out_path, file.path(descargas_dir, basename(out_path)), overwrite = TRUE)
    }
  }, error = function(e) {
    DBI::dbExecute(conn, "UPDATE encargo_de_archivo SET estado = $1, bytes_reales = 0 WHERE id = $2",
      params = list("error", job_id))
    lote_id_err <- params$lote_id %||% NULL
    if (!is.null(lote_id_err) && nzchar(as.character(lote_id_err))) {
      tryCatch(lote_maybe_finalize(conn, as.character(lote_id_err)), error = function(e2) invisible(NULL))
    } else if (!is.na(estimado) && estimado > 0L) {
      worker_liberar_cuota(conn, user_id, tier, estimado, overage_est, extra_est)
    }
    message("Error en job ", job_id, ": ", e$message)
  })
}

briefing_tick <- function() {
  conn <- get_db_connection()
  on.exit(tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL), add = TRUE)
  n <- alertas_enviar_briefings(conn)
  if (is.numeric(n) && n > 0L) message("[alertas:push] briefings=", n)
  invisible(n)
}

r <- redux::hiredis(host = redis_host, port = redis_port)
message("Worker conectado a Redis. Esperando trabajos en cola_descargas...")
last_briefing <- Sys.time() - 120
while (TRUE) {
  res <- r$BLPOP("cola_descargas", 10)
  # ponytail: un tick por minuto en el hueco del BLPOP. Una exportacion larga
  # atrasa el boletin hasta que el job termina.
  if (difftime(Sys.time(), last_briefing, units = "secs") >= 60) {
    last_briefing <- Sys.time()
    tryCatch(briefing_tick(), error = function(e) message("[alertas:push] tick ERROR: ", e$message))
  }
  if (is.null(res)) next
  job_id <- res[[2]]
  message("Procesando exportacion: ", job_id)
  run_job(job_id)
}
