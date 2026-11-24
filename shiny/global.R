# global.R - se ejecuta PRIMERO
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) return(y)
  if (length(x) == 1L && is.na(x)) return(y)
  x
}

library(shinyWidgets)
library(shinythemes)
if (requireNamespace("cicerone", quietly = TRUE)) {
  library(cicerone)
}

source("config.R", local = FALSE)
source("variable_labels.R", local = FALSE)
source("utils/filtros_estaciones.R", local = FALSE)
# API helpers (inline) - timeouts evitan que queries en cola bloqueen la app
api_url <- function() sub("/$", "", Sys.getenv("API_URL", "http://api:8000"))
API_TIMEOUT_DEFAULT <- 10
API_TIMEOUT_ESTACIONES <- 20
API_TIMEOUT_DATOS <- 45

# Memo de proceso: N sesiones no repiten el mismo GET de boot (event loop compartido).
.shiny_api_memo <- new.env(parent = emptyenv())
.shiny_api_memo$store <- new.env(parent = emptyenv())
API_MEMO_PATHS <- c(
  "/alertas/mapa", "/metadata", "/geo/conteos",
  "/variables", "/fuentes", "/regiones"
)
API_MEMO_TTL <- c(
  "/alertas/mapa" = 180, "/metadata" = 600, "/geo/conteos" = 300,
  "/variables" = 600, "/fuentes" = 600, "/regiones" = 600
)

api_memo_key <- function(path, params = NULL) {
  if (is.null(params) || length(params) == 0) return(path)
  nms <- sort(names(params))
  paste0(path, "?", paste(nms, vapply(params[nms], as.character, character(1)),
                          sep = "=", collapse = "&"))
}

api_memo_get <- function(key) {
  hit <- .shiny_api_memo$store[[key]]
  if (is.null(hit)) return(NULL)
  age <- as.numeric(difftime(Sys.time(), hit$at, units = "secs"))
  if (!is.finite(age) || age > hit$ttl) {
    rm(list = key, envir = .shiny_api_memo$store, inherits = FALSE)
    return(NULL)
  }
  hit$val
}

api_memo_set <- function(key, val, path) {
  if (is.null(val)) return(invisible(NULL))
  ttl <- unname(API_MEMO_TTL[[path]])
  if (is.null(ttl) || !is.finite(ttl)) ttl <- 120
  .shiny_api_memo$store[[key]] <- list(val = val, at = Sys.time(), ttl = as.numeric(ttl))
  invisible(NULL)
}

api_headers <- function(auth_token = NULL) {
  key <- Sys.getenv("API_KEY", "")
  token <- trimws(auth_token %||% "")
  hdrs <- list()
  if (nchar(key) > 0) hdrs[["X-API-Key"]] <- key
  if (nchar(token) > 0) hdrs[["Authorization"]] <- paste("Bearer", token)
  if (length(hdrs) == 0) httr::add_headers() else do.call(httr::add_headers, hdrs)
}

api_get <- function(path, params = NULL, timeout = API_TIMEOUT_DEFAULT,
                    reintentos = 2, pausa = 1.5, use_memo = NULL) {
  url <- paste0(api_url(), path)
  # httr::query encodes spaces/UTF-8 — manual paste broke "Los Lagos" etc.
  q <- if (!is.null(params) && length(params) > 0) as.list(params) else NULL

  memo_ok <- if (is.null(use_memo)) path %in% API_MEMO_PATHS else isTRUE(use_memo)
  memo_key <- if (memo_ok) api_memo_key(path, q) else NULL
  if (memo_ok) {
    cached <- api_memo_get(memo_key)
    if (!is.null(cached)) {
      prof <- shiny_app_profile_active()
      if (prof) {
        tid <- shiny_app_profile_trace()
        t0_ms <- shiny_app_profile_t0()
        if (!is.null(tid) && nzchar(tid)) {
          ts_start <- if (!is.null(t0_ms) && is.finite(t0_ms)) {
            as.numeric(Sys.time()) * 1000 - t0_ms
          } else {
            0
          }
          shiny_app_profile_beacon(list(list(
            name = paste0("shiny.api_get", path),
            cat = "shiny",
            ts = round(ts_start, 3),
            dur_ms = 0,
            attrs = list(lane = "shiny", path = path, phase = "api_get", memo = "HIT")
          )), tid, t0_ms)
        }
      }
      return(cached)
    }
  }

  t_status0 <- Sys.time()
  prof <- shiny_app_profile_active()
  t_status_ms <- as.numeric(difftime(Sys.time(), t_status0, units = "secs")) * 1000

  trace_id <- if (prof) shiny_app_profile_trace() else NULL
  t0_ms <- if (prof) shiny_app_profile_t0() else NULL
  t_wall <- Sys.time()
  # Client-side start offset for accurate profiler bars (not recomputed at beacon ingest).
  ts_start <- if (!is.null(t0_ms) && is.finite(t0_ms)) {
    as.numeric(Sys.time()) * 1000 - t0_ms
  } else {
    0
  }
  last_status <- NA_integer_
  last_get_ms <- NA_real_
  last_parse_ms <- NA_real_
  last_bytes <- NA_integer_
  last_times <- list()

  for (intento in seq_len(reintentos + 1)) {
    hdrs <- api_headers()
    if (prof && !is.null(trace_id) && nzchar(trace_id)) {
      extra <- list("X-App-Trace-Id" = trace_id)
      if (!is.null(t0_ms) && is.finite(t0_ms)) extra[["X-App-T0-Ms"]] <- as.character(round(t0_ms, 3))
      key <- Sys.getenv("API_KEY", "")
      all_h <- extra
      if (nchar(key) > 0) all_h[["X-API-Key"]] <- key
      hdrs <- do.call(httr::add_headers, all_h)
    }
    t_get0 <- Sys.time()
    resp <- tryCatch(
      if (!is.null(q)) {
        httr::GET(url, hdrs, httr::timeout(timeout), query = q)
      } else {
        httr::GET(url, hdrs, httr::timeout(timeout))
      },
      error = function(e) {
        last_get_ms <<- as.numeric(difftime(Sys.time(), t_get0, units = "secs")) * 1000
        NULL
      }
    )
    if (!is.null(resp)) {
      last_get_ms <- as.numeric(difftime(Sys.time(), t_get0, units = "secs")) * 1000
      last_status <- httr::status_code(resp)
      tryCatch({
        tm <- resp$times
        if (!is.null(tm) && length(tm)) {
          last_times <- list(
            t_namelookup_ms = round(as.numeric(tm[["namelookup"]]) * 1000, 3),
            t_connect_ms = round(as.numeric(tm[["connect"]]) * 1000, 3),
            t_starttransfer_ms = round(as.numeric(tm[["starttransfer"]]) * 1000, 3),
            t_total_ms = round(as.numeric(tm[["total"]]) * 1000, 3)
          )
        }
      }, error = function(e) NULL)
    }
    if (!is.null(resp) && httr::status_code(resp) == 200) {
      t_parse0 <- Sys.time()
      cl <- httr::headers(resp)[["content-length"]]
      last_bytes <- suppressWarnings(as.integer(cl %||% NA_integer_))
      parsed <- tryCatch(
        httr::content(resp, as = "parsed", type = "application/json"),
        error = function(e) NULL
      )
      last_parse_ms <- as.numeric(difftime(Sys.time(), t_parse0, units = "secs")) * 1000
      if (prof && !is.null(trace_id)) {
        dur_gp <- as.numeric(difftime(Sys.time(), t_wall, units = "secs")) * 1000
        t_ttfb <- suppressWarnings(as.numeric(last_times$t_starttransfer_ms %||% NA_real_))
        t_conn <- suppressWarnings(as.numeric(last_times$t_connect_ms %||% NA_real_))
        t_tot <- suppressWarnings(as.numeric(last_times$t_total_ms %||% last_get_ms))
        # Cola estimada en API: TTFB cliente − handler HTTP (si ya flushó en attrs via correlación UI).
        queue_est <- if (is.finite(t_ttfb) && is.finite(t_conn)) max(0, t_ttfb - t_conn) else NA_real_
        attrs <- c(
          list(
            lane = "shiny", path = path,
            query = if (is.null(q)) NULL else paste(names(q), unlist(q), sep = "=", collapse = "&"),
            status = 200L, attempt = intento,
            t_status_ms = round(t_status_ms, 3),
            t_get_ms = round(last_get_ms, 3),
            t_parse_ms = round(last_parse_ms, 3),
            t_queue_est_ms = if (is.finite(queue_est)) round(queue_est, 3) else NULL,
            body_bytes = last_bytes,
            phase = "api_get"
          ),
          last_times
        )
        attrs <- attrs[!vapply(attrs, is.null, logical(1))]
        spans_out <- list(list(
          name = paste0("shiny.api_get", path),
          cat = "shiny",
          ts = round(ts_start, 3),
          dur_ms = round(dur_gp, 3),
          attrs = attrs
        ))
        # Sub-fases deep dive (mismos datos, carril shiny_detail).
        if (is.finite(t_status_ms) && t_status_ms >= 5) {
          spans_out[[length(spans_out) + 1L]] <- list(
            name = paste0("shiny.status_check", path),
            cat = "detail",
            ts = round(max(0, ts_start - t_status_ms), 3),
            dur_ms = round(t_status_ms, 3),
            attrs = list(
              lane = "shiny_detail", path = path, phase = "status_check",
              note = "GET /app-profile/status antes del api_get"
            )
          )
        }
        if (is.finite(t_conn) && t_conn >= 1) {
          spans_out[[length(spans_out) + 1L]] <- list(
            name = paste0("shiny.connect", path),
            cat = "detail",
            ts = round(ts_start, 3),
            dur_ms = round(t_conn, 3),
            attrs = list(lane = "shiny_detail", path = path, phase = "connect")
          )
        }
        if (is.finite(t_ttfb) && t_ttfb >= 2) {
          spans_out[[length(spans_out) + 1L]] <- list(
            name = paste0("shiny.wait_ttfb", path),
            cat = "detail",
            ts = round(ts_start + if (is.finite(t_conn)) t_conn else 0, 3),
            dur_ms = round(max(0, t_ttfb - if (is.finite(t_conn)) t_conn else 0), 3),
            attrs = list(
              lane = "shiny_detail", path = path, phase = "wait_ttfb",
              note = "Espera hasta 1er byte: cola Plumber + handler hasta TTFB"
            )
          )
        }
        if (is.finite(t_tot) && is.finite(t_ttfb) && (t_tot - t_ttfb) >= 2) {
          spans_out[[length(spans_out) + 1L]] <- list(
            name = paste0("shiny.download", path),
            cat = "detail",
            ts = round(ts_start + t_ttfb, 3),
            dur_ms = round(t_tot - t_ttfb, 3),
            attrs = list(lane = "shiny_detail", path = path, phase = "download")
          )
        }
        if (is.finite(last_parse_ms) && last_parse_ms >= 1) {
          spans_out[[length(spans_out) + 1L]] <- list(
            name = paste0("shiny.parse", path),
            cat = "detail",
            ts = round(ts_start + if (is.finite(t_tot)) t_tot else last_get_ms, 3),
            dur_ms = round(last_parse_ms, 3),
            attrs = list(lane = "shiny_detail", path = path, phase = "parse")
          )
        }
        shiny_app_profile_beacon(spans_out, trace_id, t0_ms)
      }
      if (memo_ok) api_memo_set(memo_key, parsed, path)
      return(parsed)
    }
    if (intento <= reintentos) Sys.sleep(pausa)
  }
  if (prof && !is.null(trace_id)) {
    shiny_app_profile_beacon(list(list(
      name = paste0("shiny.api_get.fail", path),
      cat = "shiny",
      ts = round(ts_start, 3),
      dur_ms = as.numeric(difftime(Sys.time(), t_wall, units = "secs")) * 1000,
      attrs = list(
        lane = "shiny", path = path, status = last_status, error = TRUE,
        attempt = reintentos + 1L,
        t_status_ms = round(t_status_ms, 3),
        t_get_ms = round(last_get_ms, 3),
        t_parse_ms = round(last_parse_ms, 3),
        body_bytes = last_bytes
      )
    )), trace_id, t0_ms)
  }
  return(NULL)
}

# --- App profiler helpers (OFF by default) ---
.shiny_app_prof <- new.env(parent = emptyenv())

shiny_app_profile_dir <- function() {
  override <- Sys.getenv("APP_PROFILE_DIR", "")
  if (nzchar(override)) {
    dir.create(override, recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(override)) return(override)
  }
  for (c in c("/var/app_profiles", "/app/logs/app_profiles", "api/logs/app_profiles")) {
    if (dir.exists(c)) return(c)
  }
  NULL
}

shiny_app_profile_read_enabled_disk <- function() {
  d <- shiny_app_profile_dir()
  if (is.null(d)) return(NULL)
  path <- file.path(d, "enabled.json")
  if (!file.exists(path)) return(FALSE)
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(raw)) return(NULL)
  isTRUE(raw$enabled)
}

shiny_app_profile_active <- function() {
  dom <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  if (!is.null(dom) && !is.null(dom$userData$app_trace_id)) return(TRUE)

  now <- as.numeric(Sys.time())
  if (!is.null(.shiny_app_prof$on) && (now - (.shiny_app_prof$at %||% 0)) < 30) {
    return(isTRUE(.shiny_app_prof$on))
  }

  # Shared enabled.json — no HTTP race that caches false-OFF for 30s.
  disk <- shiny_app_profile_read_enabled_disk()
  if (!is.null(disk)) {
    .shiny_app_prof$on <- isTRUE(disk)
    .shiny_app_prof$at <- now
    return(isTRUE(disk))
  }

  on <- NULL
  tryCatch({
    resp <- httr::GET(paste0(api_url(), "/app-profile/status"), httr::timeout(0.5))
    if (httr::status_code(resp) == 200) {
      body <- httr::content(resp, as = "parsed", type = "application/json")
      en <- body$enabled
      if (is.list(en) || (is.logical(en) && length(en) > 1L)) en <- en[[1]]
      if (is.logical(en)) on <- isTRUE(en[[1]])
      else on <- identical(as.character(en[[1]]), "TRUE") || identical(as.character(en), "true")
    }
  }, error = function(e) NULL)
  if (is.null(on)) {
    if (!is.null(.shiny_app_prof$on)) return(isTRUE(.shiny_app_prof$on))
    return(FALSE)
  }
  .shiny_app_prof$on <- isTRUE(on)
  .shiny_app_prof$at <- now
  isTRUE(on)
}

shiny_app_profile_trace <- function() {
  dom <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  if (!is.null(dom) && !is.null(dom$userData$app_trace_id)) {
    return(as.character(dom$userData$app_trace_id))
  }
  .shiny_app_prof$trace_id %||% NULL
}

shiny_app_profile_t0 <- function() {
  dom <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  if (!is.null(dom) && !is.null(dom$userData$app_t0_ms)) {
    return(as.numeric(dom$userData$app_t0_ms))
  }
  .shiny_app_prof$t0_ms %||% NULL
}

shiny_app_profile_write_local <- function(spans, trace_id, t0_ms = NULL) {
  d <- shiny_app_profile_dir()
  if (is.null(d) || is.null(spans) || !length(spans)) return(FALSE)
  tid <- as.character(trace_id)
  if (!nzchar(tid)) return(FALSE)
  sum_path <- file.path(d, paste0("app_", tid, ".summary.json"))
  jsonl_path <- file.path(d, paste0("app_", tid, ".jsonl"))
  if (!file.exists(sum_path)) {
    obj <- list(
      version = 1L,
      run_id = tid,
      task_name = "App_Session",
      status = "running",
      started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      wall_ms = 0,
      n_http = 0L, n_db = 0L, n_cache = 0L, n_error = 0L,
      t0_ms = if (!is.null(t0_ms) && is.finite(t0_ms)) t0_ms else NULL
    )
    tryCatch(
      writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", pretty = TRUE), sum_path, useBytes = TRUE),
      error = function(e) NULL
    )
  }
  lines <- character(0)
  for (i in seq_along(spans)) {
    sp <- spans[[i]]
    if (!is.list(sp) || is.null(sp$name)) next
    obj <- list(
      id = paste0("s", format(Sys.time(), "%H%M%OS3"), "x", i),
      name = as.character(sp$name),
      cat = as.character(sp$cat %||% "shiny"),
      task = "App_Session",
      run_id = tid,
      ts = round(as.numeric(sp$ts %||% 0), 3),
      dur_ms = round(as.numeric(sp$dur_ms %||% 0), 3),
      attrs = sp$attrs %||% list(lane = "shiny")
    )
    lines <- c(lines, jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null"))
  }
  if (!length(lines)) return(FALSE)
  payload <- paste0(paste(lines, collapse = "\n"), "\n")
  # Same lock convention as api/utils/app_profile.R — browser beacons write concurrently.
  lock <- paste0(jsonl_path, ".lock")
  acquired <- FALSE
  for (i in seq_len(40L)) {
    if (dir.create(lock, showWarnings = FALSE)) {
      acquired <- TRUE
      break
    }
    Sys.sleep(0.002)
  }
  if (acquired) on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)
  ok <- tryCatch({
    cat(payload, file = jsonl_path, append = TRUE, sep = "")
    TRUE
  }, error = function(e) FALSE)
  isTRUE(ok)
}

# Local JSONL is source of truth for Shiny spans (same dir as API).
shiny_app_profile_beacon <- function(spans, trace_id, t0_ms = NULL, return_ms = FALSE) {
  if (is.null(trace_id) || !nzchar(as.character(trace_id))) {
    return(if (isTRUE(return_ms)) 0 else invisible(NULL))
  }
  if (is.null(spans) || !length(spans)) {
    return(if (isTRUE(return_ms)) 0 else invisible(NULL))
  }
  written <- isTRUE(shiny_app_profile_write_local(spans, trace_id, t0_ms))
  dom <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  if (!is.null(dom)) {
    tryCatch({
      dom$sendCustomMessage("app_profile_spans", list(
        trace_id = as.character(trace_id),
        t0_ms = if (!is.null(t0_ms) && is.finite(t0_ms)) t0_ms else NULL,
        spans = spans
      ))
    }, error = function(e) NULL)
  }
  if (!written) {
    body <- list(trace_id = as.character(trace_id), spans = spans)
    if (!is.null(t0_ms) && is.finite(t0_ms)) body$t0_ms <- t0_ms
    payload <- tryCatch(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"), error = function(e) NULL)
    if (!is.null(payload)) {
      tryCatch({
        httr::POST(paste0(api_url(), "/app-profile/beacon"), httr::timeout(0.15),
                   httr::content_type_json(), body = payload)
      }, error = function(e) NULL)
    }
  }
  if (isTRUE(return_ms)) return(0)
  invisible(NULL)
}

shiny_app_profile_mark <- function(name, attrs = NULL) {
  if (!isTRUE(shiny_app_profile_active())) return(invisible(NULL))
  tid <- shiny_app_profile_trace()
  t0 <- shiny_app_profile_t0()
  if (is.null(tid) || is.null(t0)) return(invisible(NULL))
  ts <- as.numeric(Sys.time()) * 1000 - t0
  a <- c(list(lane = "shiny"), attrs %||% list())
  shiny_app_profile_beacon(list(list(
    name = as.character(name),
    cat = "shiny",
    ts = round(ts, 3),
    dur_ms = 0,
    attrs = a
  )), tid, t0)
  invisible(NULL)
}
# Fechas desde API JSON / listas / POSIX: evita "don't know how to convert ... to Date".
ec_serie_normalizar <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
  if ("valor" %in% names(df)) {
    df$valor <- suppressWarnings(as.numeric(df$valor))
    df$valor[is.nan(df$valor)] <- NA_real_
  }
  df
}

ec_coerce_una_fecha <- function(x) {
  if (is.null(x)) return(as.Date(NA))
  if (is.data.frame(x)) {
    if (nrow(x) < 1L || ncol(x) < 1L) return(as.Date(NA))
    return(ec_coerce_una_fecha(x[[1L]][[1L]]))
  }
  if (is.array(x) && length(x) == 1L) x <- as.vector(x)
  if (inherits(x, "Date")) {
    if (length(x) == 0L) return(as.Date(NA))
    return(x[1L])
  }
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    if (length(x) == 0L) return(as.Date(NA))
    return(as.Date(x[1L]))
  }
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    xs <- trimws(as.character(x[1L]))
    if (!nzchar(xs) || is.na(xs)) return(as.Date(NA))
    d <- suppressWarnings(as.Date(xs))
    if (!is.na(d)) return(d)
    for (fmt in c("%Y-%m-%dT%H:%M:%OS", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%OS",
                  "%Y-%m-%d %H:%M:%S", "%Y-%m-%d")) {
      p <- suppressWarnings(strptime(xs, format = fmt, tz = "UTC"))
      if (!is.na(p)) return(as.Date(p))
    }
    return(suppressWarnings(as.Date(substr(xs, 1L, 10L))))
  }
  if (is.numeric(x) && length(x) >= 1L && !is.na(x[1L])) {
    return(as.Date(as.numeric(x[1L]), origin = "1970-01-01"))
  }
  if (is.list(x)) return(ec_coerce_una_fecha(x[[1L]]))
  suppressWarnings(tryCatch(as.Date(x), error = function(e) as.Date(NA)))
}

# Modulos (funcionalidad)
source("modules/mapa.R", local = FALSE)
source("modules/grafico.R", local = FALSE)
source("modules/tabla.R", local = FALSE)
source("modules/tabla_estaciones.R", local = FALSE)
source("modules/reporte.R", local = FALSE)
source("modules/wizard_estacion.R", local = FALSE)
source("modules/onboarding.R", local = FALSE)
# Componentes UI (estructura)
source("ui_components.R", local = FALSE)
