#* @apiTitle Explorador Climatico API
library(plumber)

base <- "/app"
source(file.path(base, "utils", "db.R"))
source(file.path(base, "utils", "cache.R"))
source(file.path(base, "utils", "limits.R"))
source(file.path(base, "utils", "auth.R"))
source(file.path(base, "utils", "descarga_helpers.R"))
source(file.path(base, "utils", "calidad.R"))
source(file.path(base, "utils", "estaciones_filtro.R"))
source(file.path(base, "utils", "unidades_display.R"))
source(file.path(base, "utils", "estacion_label.R"))
source(file.path(base, "utils", "app_profile.R"))

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

log_filter <- function(req, res) {
  t0 <- Sys.time()
  path <- req$PATH_INFO %||% ""
  # Skip profiler self-traffic noise
  skip_prof <- grepl("^/app-profile/", path) || grepl("^/admin/app-profiler/", path) ||
    identical(path, "/ping")
  prof_on <- !skip_prof && isTRUE(tryCatch(app_profile_enabled(), error = function(e) FALSE))
  if (prof_on) {
    hdr <- req$HTTP_X_APP_TRACE_ID %||% req$headers[["x-app-trace-id"]] %||% ""
    t0_hdr <- req$HTTP_X_APP_T0_MS %||% req$headers[["x-app-t0-ms"]] %||% ""
    # Sin header = no correlacion. No crear UUID huerfano por cada GET.
    if (nzchar(as.character(hdr))) {
      trace_id <- as.character(hdr)
      t0_ms <- suppressWarnings(as.numeric(t0_hdr))
      if (length(t0_ms) != 1L || !is.finite(t0_ms) || t0_ms <= 0) t0_ms <- app_profile_now_ms()
      if (abs(app_profile_now_ms() - t0_ms) > 3600000) t0_ms <- app_profile_now_ms()
      app_profile_ctx_set(trace_id, t0_ms = t0_ms)
      .GlobalEnv$.ec_app_active_trace <- as.character(trace_id)
      .GlobalEnv$.ec_app_active_t0 <- t0_ms
      .GlobalEnv$.ec_app_req_twall <- Sys.time()
      .GlobalEnv$.ec_app_http_pending <- list(
        method = req$REQUEST_METHOD %||% "GET",
        path = path,
        query = req$QUERY_STRING %||% "",
        trace_id = as.character(trace_id)
      )
      if (!file.exists(app_profile_summary_path(trace_id))) {
        tryCatch(app_profile_summary_write(trace_id, partial = TRUE, extra = list(t0_ms = t0_ms)), error = function(e) NULL)
      }
      tryCatch(res$setHeader("X-App-Trace-Id", trace_id), error = function(e) NULL)
    }
  }
  plumber::forward()
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  message(sprintf("[%s] %s %s - %s s", format(Sys.time(), "%H:%M:%S"), req$REQUEST_METHOD, req$PATH_INFO, elapsed))
}

api_key_filter <- function(req, res) {
  if (req$REQUEST_METHOD == "OPTIONS") { plumber::forward(); return(invisible(NULL)) }
  path <- req$PATH_INFO %||% ""
  if (grepl("^/webhooks/", path)) { plumber::forward(); return(invisible(NULL)) }
  if (identical(path, "/billing/flow/register-return")) { plumber::forward(); return(invisible(NULL)) }
  if (identical(path, "/ping")) { plumber::forward(); return(invisible(NULL)) }
  # Public app-profiler status/beacon (no API key; write path is rate-light + enabled-gated)
  if (grepl("^/app-profile/(status|beacon)$", path)) { plumber::forward(); return(invisible(NULL)) }
  required_key <- Sys.getenv("API_KEY", "")
  if (nchar(required_key) == 0) { plumber::forward(); return(invisible(NULL)) }
  key <- req$HTTP_X_API_KEY %||% req$headers[["x-api-key"]] %||% ""
  if (key != required_key) {
    res$status <- 401
    return(list(error = "API key requerida. Env\u00eda X-API-Key en el header."))
  }
  plumber::forward()
}

cors_allowed_origins <- function() {
  raw <- Sys.getenv("ALLOWED_ORIGINS", "")
  if (!nzchar(trimws(raw))) return(character(0))
  parts <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
  parts[nzchar(parts)]
}

cors_filter <- function(req, res) {
  allowed <- cors_allowed_origins()
  origin <- req$HTTP_ORIGIN %||% req$headers[["origin"]] %||% ""

  if (length(allowed) == 0L) {
    res$setHeader("Access-Control-Allow-Origin", "*")
  } else if (nzchar(origin) && origin %in% allowed) {
    res$setHeader("Access-Control-Allow-Origin", origin)
    res$setHeader("Vary", "Origin")
  }

  if (req$REQUEST_METHOD == "OPTIONS") {
    if (length(allowed) > 0L && (!nzchar(origin) || !origin %in% allowed)) {
      res$status <- 403
      return(list(error = "Origin not allowed"))
    }
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, PATCH, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, X-API-Key, Authorization, X-User-Api-Key, X-Admin-Key, X-App-Trace-Id, X-App-T0-Ms")
    res$setHeader("Access-Control-Expose-Headers", "Content-Disposition, X-Reporte-Estado, Last-Modified, X-App-Trace-Id, X-Cache")
    if (length(allowed) > 0L && nzchar(origin)) {
      res$setHeader("Access-Control-Allow-Origin", origin)
    }
    res$status <- 200
    return(list())
  }
  res$setHeader("Access-Control-Expose-Headers", "Content-Disposition")
  plumber::forward()
}

header <- c(
  'base <- "/app"',
  'source(file.path(base, "utils", "db.R"))',
  'source(file.path(base, "utils", "cache.R"))',
  'source(file.path(base, "utils", "limits.R"))',
  'source(file.path(base, "utils", "auth.R"))',
  'source(file.path(base, "utils", "descarga_helpers.R"))',
  'source(file.path(base, "utils", "calidad.R"))',
  'source(file.path(base, "utils", "estaciones_filtro.R"))',
  'source(file.path(base, "utils", "unidades_display.R"))',
  'source(file.path(base, "utils", "estacion_label.R"))',
  'source(file.path(base, "utils", "alertas_catalog.R"))',
  'source(file.path(base, "utils", "alerta_temprana.R"))',
  'source(file.path(base, "utils", "pronostico_payload.R"))',
  'source(file.path(base, "utils", "soporte.R"))',
  'source(file.path(base, "utils", "soporte_llm.R"))',
  'source(file.path(base, "utils", "pronostico_rank_sql.R"))',
  'source(file.path(base, "utils", "alertas_mensajes.R"))',
  'source(file.path(base, "utils", "alertas.R"))',
  'source(file.path(base, "utils", "alertas_resumen.R"))',
  'source(file.path(base, "utils", "alertas_resumen_query.R"))',
  'source(file.path(base, "utils", "alertas_umbral.R"))',
  'source(file.path(base, "utils", "alertas_digest.R"))',
  'source(file.path(base, "utils", "alertas_digest_api.R"))',
  'source(file.path(base, "utils", "alertas_email.R"))',
  'source(file.path(base, "utils", "correo_rebote.R"))',
  'source(file.path(base, "utils", "alertas_push.R"))',
  'source(file.path(base, "utils", "contacto_email.R"))',
  'source(file.path(base, "utils", "bias_alert_impact.R"))',
  'source(file.path(base, "utils", "pronostico_precision_series.R"))',
  'source(file.path(base, "utils", "alertas_skill.R"))',
  'source(file.path(base, "utils", "alertas_concordancia_regla.R"))',
  'source(file.path(base, "utils", "alertas_tipo_control.R"))',
  'source(file.path(base, "utils", "stripe.R"))',
  'source(file.path(base, "utils", "flow.R"))',
  'source(file.path(base, "utils", "on_demand.R"))',
  'source(file.path(base, "utils", "mb_pack.R"))',
  'source(file.path(base, "utils", "app_profile.R"))',
  'source(file.path(base, "utils", "reporte_pdf_offer.R"))',
  'source(file.path(base, "utils", "monitoreo.R"))',
  '`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x',
  ""
)
endpoint_files <- c("ping.R", "fuentes.R", "estaciones.R", "geo_conteos.R", "reporte.R", "reporte_pdf.R", "pronostico.R", "widget.R", "variables.R", "datos.R", "calidad.R", "analisis.R", "descargas.R", "usuario.R", "on_demand.R", "billing.R", "alertas.R", "contacto.R", "soporte.R", "webhooks.R", "admin.R", "app_profile.R", "monitoreo.R")
combined <- c(header, unlist(lapply(endpoint_files, function(f) {
  readLines(file.path(base, "endpoints", f), warn = FALSE)
})))
tmp <- tempfile(fileext = ".R")
writeLines(combined, tmp)
sub_pr <- plumber::plumb(tmp)
file.remove(tmp)

# Always stamp HTTP span after mounted handler (cache path also flushes — 2nd is no-op).
sub_pr <- tryCatch({
  plumber::pr_hook(sub_pr, "postroute", function(data, req, res) {
    st <- tryCatch({
      s <- res$status
      if (is.null(s) || !is.finite(as.numeric(s))) 200L else as.integer(s)
    }, error = function(e) 200L)
    tryCatch(app_profile_flush_http(st), error = function(e) NULL)
    invisible(TRUE)
  })
}, error = function(e) sub_pr)

pr <- plumber::pr() %>%
  plumber::pr_filter("cors", cors_filter) %>%
  plumber::pr_filter("auth", auth_filter) %>%
  plumber::pr_filter("apikey", api_key_filter) %>%
  plumber::pr_filter("log", log_filter) %>%
  plumber::pr_mount("/", sub_pr) %>%
  plumber::pr_set_docs(FALSE)

pr %>% plumber::pr_run(host = "0.0.0.0", port = 8000)
