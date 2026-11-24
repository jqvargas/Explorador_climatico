# Cliente Open-Meteo batch para pronostico diario + horario (mismo GET).

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

OPENMETEO_DAILY_FIELDS <- c(
  "temperature_2m_max",
  "temperature_2m_min",
  "precipitation_sum",
  "wind_speed_10m_mean",
  "wind_speed_10m_max",
  "weather_code"
)

# freezing_level_height: isoterma 0 C en metros. Solo este GET horario.
# No entra a OPENMETEO_DAILY_FIELDS: el historico de sesgo no lo pide.
OPENMETEO_HOURLY_FIELDS <- c(
  "temperature_2m",
  "precipitation",
  "wind_speed_10m",
  "weather_code",
  "is_day",
  "precipitation_probability",
  "freezing_level_height"
)

# Pronostico 16d. No entra en OPENMETEO_DAILY_FIELDS: ese vector arma el lead 0
# historico de temp/pp/viento maximo, y la rafaga espera su propia compuerta.
OPENMETEO_FORECAST_DAILY_FIELDS <- c(OPENMETEO_DAILY_FIELDS, "wind_gusts_10m_max")

OPENMETEO_N_VARS <- length(OPENMETEO_FORECAST_DAILY_FIELDS) + length(OPENMETEO_HOURLY_FIELDS)
stopifnot(OPENMETEO_N_VARS == 14L)
stopifnot(!"wind_gusts_10m_max" %in% OPENMETEO_DAILY_FIELDS)

OPENMETEO_VAR_MAP <- list(
  temperature_2m_max = "t_max",
  temperature_2m_min = "t_min",
  precipitation_sum = "pp",
  wind_speed_10m_mean = "u2_mean",
  wind_speed_10m_max = NULL,
  weather_code = "weather_code"
)

openmeteo_pg_text_array <- function(vals) {
  if (is.null(vals) || length(vals) == 0) return(NA_character_)
  vals <- as.character(unlist(vals))
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) return(NA_character_)
  paste0("{", paste(vals, collapse = ","), "}")
}

openmeteo_pg_int_array <- function(vals) {
  if (is.null(vals) || length(vals) == 0) return(NA_character_)
  vals <- unique(as.integer(unlist(vals)))
  vals <- vals[!is.na(vals) & vals > 0L]
  if (length(vals) == 0) return(NA_character_)
  paste0("{", paste(vals, collapse = ","), "}")
}

openmeteo_config <- function() {
  base <- trimws(Sys.getenv("OPENMETEO_BASE_URL", "https://api.open-meteo.com/v1"))
  key <- trimws(Sys.getenv("OPENMETEO_API_KEY", ""))
  if (nzchar(key) && !grepl("^https://", base)) {
    base <- "https://customer-api.open-meteo.com/v1"
  }
  list(
    base_url = sub("/$", "", base),
    api_key = key,
    forecast_days = as.integer(Sys.getenv("OPENMETEO_FORECAST_DAYS", "16")),
    # ponytail: 1000 coords fits Open-Meteo API but nginx 414 on long GET URLs; 100 is safe
    chunk_size = as.integer(Sys.getenv("OPENMETEO_CHUNK_SIZE", "100")),
    timezone = Sys.getenv("OPENMETEO_TIMEZONE", "America/Santiago"),
    chunk_pause_ms = as.integer(Sys.getenv("OPENMETEO_CHUNK_PAUSE_MS", "3000"))
  )
}

openmeteo_calls_estimate <- function(n, days = 16L, vars = OPENMETEO_N_VARS) {
  n <- as.numeric(n)
  days <- as.numeric(days)
  vars <- as.numeric(vars)
  ceiling(n * (days / 14) * (vars / 10))
}

openmeteo_load_variable_ids <- function(conn) {
  abrs <- c("pp", "t_max", "t_min", "u2_mean", "u_max_diario", "u_10_kt", "weather_code", "u_rafaga")
  df <- DBI::dbGetQuery(
    conn,
    "SELECT id, nombre_abr FROM magnitud WHERE lower(nombre_abr) = ANY($1::text[])",
    params = list(openmeteo_pg_text_array(abrs))
  )
  if (nrow(df) == 0) stop("No se encontraron variables de pronostico en BD")
  ids <- stats::setNames(as.integer(df$id), tolower(df$nombre_abr))
  u_rafaga <- ids[["u_rafaga"]]
  if (length(u_rafaga) != 1L || is.na(u_rafaga) || as.integer(u_rafaga) != 19L) {
    stop("u_rafaga debe ser id 19")
  }
  list(
    pp = ids[["pp"]],
    t_max = ids[["t_max"]],
    t_min = ids[["t_min"]],
    u2_mean = ids[["u2_mean"]],
    u_max_diario = ids[["u_max_diario"]],
    u_10_kt = ids[["u_10_kt"]],
    weather_code = ids[["weather_code"]],
    u_rafaga = as.integer(u_rafaga)
  )
}

openmeteo_fetch_estaciones_calidad <- function(conn, eids = NULL, limit = NULL) {
  # 0 = todas buena|excelente (sin piso de anios). Alertas no usan este filtro.
  min_anios <- 0L
  if (exists("pronostico_min_anios_display", mode = "function")) {
    min_anios <- as.integer(pronostico_min_anios_display())
  }
  if (is.na(min_anios) || min_anios < 0L) min_anios <- 0L
  q <- paste0(
    "
    SELECT e.id, e.lat, e.lon, e.nombre, ec.nivel
    FROM mirador e
    INNER JOIN estacion_calidad ec ON ec.id_estacion = e.id
    WHERE ec.nivel IN ('excelente', 'buena')
      AND COALESCE(ec.anios_span, 0) >= ", as.integer(min_anios), "
      AND e.lat IS NOT NULL AND e.lon IS NOT NULL
      AND e.lat BETWEEN -56 AND -17 AND e.lon BETWEEN -76 AND -66
  "
  )
  params <- list()
  if (!is.null(eids) && length(eids) > 0) {
    eids <- unique(as.integer(eids))
    eids <- eids[!is.na(eids) & eids > 0L]
    q <- paste0(q, " AND e.id = ANY($1::int[])")
    params <- list(openmeteo_pg_int_array(eids))
  }
  q <- paste0(q, " ORDER BY e.id")
  if (!is.null(limit)) {
    lim <- as.integer(limit)
    if (!is.na(lim) && lim > 0L) q <- paste0(q, " LIMIT ", lim)
  }
  if (length(params)) DBI::dbGetQuery(conn, q, params = params) else DBI::dbGetQuery(conn, q)
}

openmeteo_wind_map <- function(conn, eids, var_ids) {
  if (length(eids) == 0) return(data.frame())
  df <- DBI::dbGetQuery(
    conn,
    "SELECT o.id_estacion AS id,
            bool_or(v.nombre_abr = 'u_max_diario') AS has_u2,
            bool_or(v.nombre_abr = 'u_10_kt') AS has_u10
     FROM bitacora_oficial o
     JOIN magnitud v ON v.id = o.id_variable
     WHERE o.id_estacion = ANY($1::int[])
       AND v.nombre_abr IN ('u_max_diario', 'u_10_kt')
     GROUP BY o.id_estacion",
    params = list(openmeteo_pg_int_array(eids))
  )
  if (nrow(df) == 0) {
    return(data.frame(
      id = integer(0), windspeed_unit = character(0), stringsAsFactors = FALSE
    ))
  }
  windspeed_unit <- ifelse(isTRUE(df$has_u2), "ms",
                           ifelse(isTRUE(df$has_u10), "kn", "ms"))
  data.frame(
    id = as.integer(df$id),
    windspeed_unit = as.character(windspeed_unit),
    stringsAsFactors = FALSE
  )
}

openmeteo_chunk_df <- function(df, size = 1000L) {
  if (nrow(df) == 0) return(list())
  size <- max(1L, as.integer(size))
  split(df, ceiling(seq_len(nrow(df)) / size))
}

openmeteo_build_url <- function(lats, lons, cfg, windspeed_unit = "ms") {
  if (!requireNamespace("httr", quietly = TRUE)) stop("Paquete httr requerido")
  lats <- as.numeric(lats)
  lons <- as.numeric(lons)
  if (length(lats) != length(lons) || length(lats) == 0) {
    stop("lats/lons vacios o longitud distinta")
  }
  days <- cfg$forecast_days
  if (is.na(days) || days < 1L || days > 16L) days <- 16L
  query <- list(
    latitude = paste(lats, collapse = ","),
    longitude = paste(lons, collapse = ","),
    daily = paste(OPENMETEO_FORECAST_DAILY_FIELDS, collapse = ","),
    hourly = paste(OPENMETEO_HOURLY_FIELDS, collapse = ","),
    forecast_days = days,
    timezone = cfg$timezone,
    windspeed_unit = windspeed_unit
  )
  if (nzchar(cfg$api_key)) query$apikey <- cfg$api_key
  httr::modify_url(paste0(cfg$base_url, "/forecast"), query = query)
}

# simplifyVector = FALSE deja cada null de un arreglo como NULL dentro de la lista;
# unlist() los descarta y corre las fechas (2024 leads 1-3 quedo ~19 dias antes).
# Aqui el null sigue siendo NA en su posicion.
openmeteo_num <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x)) {
    x <- vapply(x, function(v) if (length(v) == 0L) NA_real_ else suppressWarnings(as.numeric(v[[1]])),
                numeric(1), USE.NAMES = FALSE)
  }
  suppressWarnings(as.numeric(x))
}

# Previous Runs a veces devuelve HTTP 200 con texto plano
# ("Unexpected error while streaming...") o JSON truncado. jsonlite tira
# "lexical error"; eso es transitorio, no un 200 exitoso.
openmeteo_parse_json_body <- function(txt) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Paquete jsonlite requerido")
  raw <- if (is.null(txt) || length(txt) == 0L) "" else paste(as.character(txt), collapse = "\n")
  t <- trimws(raw)
  retry_stop <- function(msg) {
    e <- simpleError(msg)
    class(e) <- c("openmeteo_retryable", class(e))
    stop(e)
  }
  if (!nzchar(t) || !(substr(t, 1L, 1L) %in% c("{", "["))) {
    snippet <- substr(gsub("[\\r\\n\\t]+", " ", t), 1L, 120L)
    retry_stop(paste0("Open-Meteo cuerpo no-JSON: ", snippet))
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(t, simplifyVector = FALSE),
    error = function(e) retry_stop(paste0("Open-Meteo JSON invalido: ", conditionMessage(e)))
  )
  if (is.list(parsed) && isTRUE(parsed$error)) {
    stop("Open-Meteo error: ", parsed$reason %||% "sin reason", call. = FALSE)
  }
  parsed
}

openmeteo_http_get <- function(url, retries = 5L) {
  if (!requireNamespace("httr", quietly = TRUE)) stop("Paquete httr requerido")
  last_err <- NULL
  last_retryable <- FALSE
  sc <- NULL
  resp_ok <- NULL
  t0 <- Sys.time()
  retryable <- c(429L, 500L, 502L, 503L, 504L)
  for (attempt in seq_len(retries)) {
    resp <- tryCatch(
      httr::GET(url, httr::timeout(120), httr::user_agent("Rodaja-Pronostico/1.0")),
      error = function(e) e
    )
    if (inherits(resp, "error")) {
      last_err <- conditionMessage(resp)
      last_retryable <- FALSE
      sc <- 0L
    } else {
      sc <- httr::status_code(resp)
      if (sc == 200L) {
        txt <- httr::content(resp, as = "text", encoding = "UTF-8")
        parsed <- tryCatch(openmeteo_parse_json_body(txt), error = function(e) e)
        if (!inherits(parsed, "error")) {
          attr(resp, "openmeteo_parsed") <- parsed
          resp_ok <- resp
          break
        }
        last_err <- conditionMessage(parsed)
        last_retryable <- inherits(parsed, "openmeteo_retryable")
        if (!last_retryable) break
        sc <- 502L
      } else {
        last_err <- paste0("HTTP ", sc, ": ", httr::content(resp, as = "text", encoding = "UTF-8"))
        last_retryable <- FALSE
        # Cuota diaria: reintentar no la devuelve y gasta el resto de la hora.
        if (grepl("limit exceeded", last_err, ignore.case = TRUE)) break
        if (!sc %in% retryable) break
      }
    }
    if (attempt < retries) {
      pause <- if (!is.null(sc) && sc == 429L) 65 else 2^attempt
      Sys.sleep(pause)
    }
  }
  wall_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
  if (exists("profile_record_http", mode = "function")) {
    parent <- if (exists("profile_current_parent", mode = "function")) {
      profile_current_parent(c("phase", "step"))
    } else {
      NULL
    }
    tryCatch(
      profile_record_http(
        response = resp_ok,
        fuente = "openmeteo",
        http_code = if (is.null(resp_ok)) as.integer(sc %||% 0L) else as.integer(httr::status_code(resp_ok)),
        timeout = is.null(resp_ok) && identical(as.integer(sc %||% -1L), 0L),
        parent = parent,
        wall_ms = wall_ms,
        force = TRUE
      ),
      error = function(e) NULL
    )
  }
  if (!is.null(resp_ok)) return(resp_ok)
  msg <- paste0("Open-Meteo fallo: ", last_err %||% "desconocido")
  if (isTRUE(last_retryable)) {
    e <- simpleError(msg)
    class(e) <- c("openmeteo_retryable", class(e))
    stop(e)
  }
  stop(msg, call. = FALSE)
}

# El stop() final de openmeteo_http_get a veces llega envuelto. La clase
# openmeteo_retryable o el texto del stream bastan para partir el chunk.
openmeteo_error_retryable <- function(e) {
  if (inherits(e, "openmeteo_retryable")) return(TRUE)
  msg <- tryCatch(conditionMessage(e), error = function(err) "")
  grepl("JSON invalido|cuerpo no-JSON|lexical error|Unexpected error while stream",
        msg, ignore.case = TRUE)
}

openmeteo_bind_rows <- function(parts) {
  parts <- parts[!vapply(parts, is.null, logical(1))]
  parts <- parts[vapply(parts, function(p) is.data.frame(p) && nrow(p) > 0L, logical(1))]
  if (!length(parts)) return(NULL)
  do.call(rbind, parts)
}

openmeteo_combine_fetched <- function(parts) {
  dailies <- list()
  hourlies <- list()
  hourly_failed <- FALSE
  for (p in parts) {
    if (is.null(p)) next
    if (is.data.frame(p$daily) && nrow(p$daily) > 0L) dailies[[length(dailies) + 1L]] <- p$daily
    if (is.data.frame(p$hourly) && nrow(p$hourly) > 0L) hourlies[[length(hourlies) + 1L]] <- p$hourly
    if (isTRUE(p$hourly_failed)) hourly_failed <- TRUE
  }
  list(
    daily = if (length(dailies)) do.call(rbind, dailies) else data.frame(),
    hourly = if (length(hourlies)) do.call(rbind, hourlies) else openmeteo_empty_hora(),
    hourly_failed = hourly_failed
  )
}

# Un GET de 100 estaciones a veces vuelve cortado ("Unexpected error while streaming").
# Reintentar el mismo URL no alcanza: se parte una vez. Por debajo de min_n se rinde
# ese subconjunto y el llamador lo reintenta despues, sin abortar el resto.
# fetch_fn(stations_df, retries) debe devolver el list de openmeteo_fetch_chunk o tirar.
openmeteo_fetch_stations_resilient <- function(stations_df, fetch_fn, min_n = 25L,
                                                retries_wide = 2L, retries_leaf = 3L) {
  n <- nrow(stations_df)
  empty_fail <- stations_df[0, , drop = FALSE]
  if (n < 1L) {
    return(list(chunk = openmeteo_combine_fetched(list()), failed = empty_fail, error = NULL))
  }
  retries <- if (n > min_n) as.integer(retries_wide) else as.integer(retries_leaf)
  got <- tryCatch(
    list(ok = TRUE, value = fetch_fn(stations_df, retries)),
    error = function(e) list(ok = FALSE, error = e)
  )
  if (isTRUE(got$ok)) {
    return(list(chunk = got$value, failed = empty_fail, error = NULL))
  }
  if (!openmeteo_error_retryable(got$error)) stop(got$error)
  if (n <= min_n) {
    message("[openmeteo] JSON/stream agotado n=", n, ": ", conditionMessage(got$error))
    return(list(chunk = openmeteo_combine_fetched(list()), failed = stations_df, error = got$error))
  }
  mid <- max(1L, n %/% 2L)
  message("[openmeteo] JSON/stream en n=", n, "; parto ", mid, "+", n - mid)
  left <- openmeteo_fetch_stations_resilient(
    stations_df[seq_len(mid), , drop = FALSE], fetch_fn, min_n, retries_wide, retries_leaf
  )
  right <- openmeteo_fetch_stations_resilient(
    stations_df[seq.int(mid + 1L, n), , drop = FALSE], fetch_fn, min_n, retries_wide, retries_leaf
  )
  failed <- openmeteo_bind_rows(list(left$failed, right$failed))
  if (is.null(failed)) failed <- empty_fail
  list(
    chunk = openmeteo_combine_fetched(list(left$chunk, right$chunk)),
    failed = failed,
    error = left$error %||% right$error
  )
}

openmeteo_http_json <- function(url, retries = 5L) {
  resp <- openmeteo_http_get(url, retries = retries)
  parsed <- attr(resp, "openmeteo_parsed")
  if (!is.null(parsed)) return(parsed)
  openmeteo_parse_json_body(httr::content(resp, as = "text", encoding = "UTF-8"))
}

openmeteo_normalize_locations <- function(parsed) {
  if (is.null(parsed)) return(list())
  if (!is.null(parsed$latitude) && !is.null(parsed$daily)) {
    return(list(parsed))
  }
  if (is.list(parsed) && length(parsed) > 0 && !is.null(parsed[[1]]$latitude)) {
    return(parsed)
  }
  stop("Respuesta Open-Meteo no reconocida")
}

openmeteo_parse_location <- function(loc, eid, var_ids, wind_info, windspeed_unit = "ms") {
  daily <- loc$daily
  if (is.null(daily) || is.null(daily$time)) return(data.frame())
  times <- as.Date(as.character(unlist(daily$time)))
  rows <- list()
  add_rows <- function(field, var_id, transform = identity, max_idx = NULL) {
    vals <- openmeteo_num(daily[[field]])
    if (is.null(vals) || is.null(var_id) || is.na(var_id)) return(invisible(NULL))
    n_keep <- length(times)
    if (!is.null(max_idx)) n_keep <- min(n_keep, as.integer(max_idx))
    if (n_keep < 1L) return(invisible(NULL))
    for (i in seq_len(n_keep)) {
      v <- suppressWarnings(as.numeric(vals[i]))
      if (is.na(v)) next
      v <- transform(v)
      if (is.na(v)) next
      rows[[length(rows) + 1L]] <<- data.frame(
        id_estacion = eid,
        fecha_objetivo = times[i],
        id_variable = var_id,
        valor = v,
        stringsAsFactors = FALSE
      )
    }
    invisible(NULL)
  }
  add_rows("temperature_2m_max", var_ids$t_max)
  add_rows("temperature_2m_min", var_ids$t_min)
  add_rows("precipitation_sum", var_ids$pp)
  # ponytail: viento siempre en request; sin histórico se guarda u2_* (ms) para el reporte
  mean_xform <- if (identical(windspeed_unit, "kn")) function(x) x * 0.514444 else identity
  add_rows("wind_speed_10m_mean", var_ids$u2_mean, transform = mean_xform)
  add_rows("wind_speed_10m_max", var_ids$u_max_diario, transform = mean_xform)
  # Leads 0-3 = primeros 4 dias del pronostico de 16. El resto de la rafaga no se guarda.
  add_rows("wind_gusts_10m_max", var_ids$u_rafaga, transform = mean_xform, max_idx = 4L)
  add_rows("weather_code", var_ids$weather_code)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

openmeteo_empty_hora <- function() {
  data.frame(
    id_estacion = integer(0),
    ts = as.POSIXct(character(0), tz = "UTC"),
    fecha_objetivo = as.Date(character(0)),
    t_2m = numeric(0),
    pp = numeric(0),
    u10 = numeric(0),
    weather_code = integer(0),
    is_day = integer(0),
    pop = integer(0),
    isoterma_m = numeric(0),
    stringsAsFactors = FALSE
  )
}

openmeteo_parse_hourly <- function(loc, eid, windspeed_unit = "ms", timezone = "America/Santiago") {
  hourly <- loc$hourly
  if (is.null(hourly) || is.null(hourly$time)) return(openmeteo_empty_hora())
  time_chr <- as.character(unlist(hourly$time, use.names = FALSE))
  n <- length(time_chr)
  if (n == 0L) return(openmeteo_empty_hora())
  ts <- as.POSIXct(time_chr, tz = timezone, format = "%Y-%m-%dT%H:%M")
  if (all(is.na(ts))) ts <- as.POSIXct(time_chr, tz = timezone)
  fecha_objetivo <- as.Date(substr(time_chr, 1, 10))
  num_field <- function(name) {
    v <- hourly[[name]]
    if (is.null(v)) return(rep(NA_real_, n))
    out <- openmeteo_num(v)
    if (length(out) < n) out <- c(out, rep(NA_real_, n - length(out)))
    if (length(out) > n) out <- out[seq_len(n)]
    out
  }
  t_2m <- num_field("temperature_2m")
  pp <- num_field("precipitation")
  u10 <- num_field("wind_speed_10m")
  wc <- num_field("weather_code")
  is_day <- num_field("is_day")
  pop <- num_field("precipitation_probability")
  isoterma_m <- num_field("freezing_level_height")
  if (identical(windspeed_unit, "kn")) u10 <- u10 * 0.514444
  keep <- !is.na(ts)
  if (!any(keep)) return(openmeteo_empty_hora())
  out <- data.frame(
    id_estacion = as.integer(eid),
    ts = ts[keep],
    fecha_objetivo = fecha_objetivo[keep],
    t_2m = t_2m[keep],
    pp = pp[keep],
    u10 = u10[keep],
    weather_code = as.integer(round(wc[keep])),
    is_day = as.integer(round(is_day[keep])),
    pop = as.integer(round(pop[keep])),
    isoterma_m = isoterma_m[keep],
    stringsAsFactors = FALSE
  )
  # ponytail: DST spring-forward maps a nonexistent local hour onto the next;
  # 23h is valid (ARQ). Drop the collapsed duplicate, do not fail the UPSERT batch.
  dup <- duplicated(out$ts)
  if (any(dup)) out <- out[!dup, , drop = FALSE]
  out
}

openmeteo_fetch_chunk <- function(stations_df, var_ids, wind_map, cfg, bias_coefs = NULL, var_abr_map = NULL, retries = 5L) {
  eids <- as.integer(stations_df$id)
  wind_sub <- wind_map[match(eids, wind_map$id), , drop = FALSE]
  units <- unique(na.omit(wind_sub$windspeed_unit))
  if (length(units) == 0) units <- "ms"
  daily_parts <- list()
  hourly_parts <- list()
  hourly_failed <- FALSE
  tz <- cfg$timezone %||% "America/Santiago"
  for (unit in units) {
    idx <- is.na(wind_sub$windspeed_unit) | wind_sub$windspeed_unit == unit
    if (!any(idx)) next
    sub <- stations_df[idx, , drop = FALSE]
    url <- openmeteo_build_url(sub$lat, sub$lon, cfg, windspeed_unit = unit)
    parsed <- openmeteo_http_json(url, retries = retries)
    locs <- openmeteo_normalize_locations(parsed)
    if (length(locs) != nrow(sub)) {
      stop("Open-Meteo devolvio ", length(locs), " locations, esperadas ", nrow(sub))
    }
    for (j in seq_len(nrow(sub))) {
      wi <- wind_sub[idx, , drop = FALSE][j, , drop = FALSE]
      eid_j <- as.integer(sub$id[j])
      part <- openmeteo_parse_location(locs[[j]], eid_j, var_ids, wi, windspeed_unit = unit)
      if (nrow(part) > 0) daily_parts[[length(daily_parts) + 1L]] <- part
      h <- tryCatch(
        openmeteo_parse_hourly(locs[[j]], eid_j, windspeed_unit = unit, timezone = tz),
        error = function(e) {
          message("[openmeteo] hourly parse fail eid=", eid_j, ": ", conditionMessage(e))
          hourly_failed <<- TRUE
          openmeteo_empty_hora()
        }
      )
      if (!is.null(h) && nrow(h) > 0) hourly_parts[[length(hourly_parts) + 1L]] <- h
    }
  }
  daily_out <- if (length(daily_parts) > 0) do.call(rbind, daily_parts) else data.frame()
  hourly_out <- if (length(hourly_parts) > 0) do.call(rbind, hourly_parts) else openmeteo_empty_hora()
  if (nrow(daily_out) > 0 && nrow(hourly_out) == 0) {
    message("[openmeteo] hourly vacio con daily presente")
    hourly_failed <- TRUE
  }
  if (!is.null(bias_coefs) && nrow(daily_out) > 0 && exists("openmeteo_apply_bias", mode = "function")) {
    if (is.null(var_abr_map) && exists("openmeteo_bias_build_var_abr_map", mode = "function")) {
      var_abr_map <- openmeteo_bias_build_var_abr_map(var_ids)
    }
    daily_out <- openmeteo_apply_bias(daily_out, bias_coefs, var_abr_map)
  }
  list(daily = daily_out, hourly = hourly_out, hourly_failed = hourly_failed)
}

openmeteo_upsert_rows <- function(conn, rows, corrida_id = NULL) {
  if (nrow(rows) == 0) return(0L)
  has_crudo <- "valor_crudo" %in% names(rows)
  n <- 0L
  batch_size <- 500L
  for (start in seq(1L, nrow(rows), by = batch_size)) {
    end <- min(nrow(rows), start + batch_size - 1L)
    chunk <- rows[start:end, , drop = FALSE]
    values <- vapply(seq_len(nrow(chunk)), function(i) {
      crudo_sql <- if (has_crudo && is.finite(chunk$valor_crudo[i])) {
        as.character(chunk$valor_crudo[i])
      } else {
        "NULL"
      }
      sprintf(
        "(%d, '%s'::date, %d, %s, NOW(), %s, %s)",
        chunk$id_estacion[i],
        chunk$fecha_objetivo[i],
        chunk$id_variable[i],
        chunk$valor[i],
        if (is.null(corrida_id)) "NULL" else sprintf("'%s'::uuid", corrida_id),
        crudo_sql
      )
    }, character(1))
    cols <- "id_estacion, fecha_objetivo, id_variable, valor, fetched_at, corrida_id, valor_crudo"
    updates <- "valor = EXCLUDED.valor, fetched_at = EXCLUDED.fetched_at, corrida_id = EXCLUDED.corrida_id, valor_crudo = EXCLUDED.valor_crudo"
    q <- paste0(
      "INSERT INTO carta_del_mirador (", cols, ") VALUES ",
      paste(values, collapse = ", "),
      " ON CONFLICT (id_estacion, fecha_objetivo, id_variable) DO UPDATE SET ",
      updates
    )
    n <- n + DBI::dbExecute(conn, q)
  }
  as.integer(n)
}

openmeteo_sql_dbl <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (length(x) != 1L || is.na(x) || !is.finite(x)) return("NULL")
  sprintf("%.10g", x)
}

openmeteo_sql_int <- function(x) {
  x <- suppressWarnings(as.integer(x)[1])
  if (length(x) != 1L || is.na(x)) return("NULL")
  as.character(x)
}

openmeteo_sql_ts <- function(x) {
  if (length(x) != 1L || is.na(x)) return("NULL")
  ts <- as.POSIXct(x, tz = "UTC")
  sprintf("'%s'::timestamptz", format(ts, tz = "UTC", format = "%Y-%m-%d %H:%M:%S+00"))
}

openmeteo_upsert_hora_rows <- function(conn, rows, corrida_id = NULL) {
  if (is.null(rows) || nrow(rows) == 0) return(0L)
  rows <- rows[!duplicated(paste(rows$id_estacion, as.numeric(rows$ts))), , drop = FALSE]
  n <- 0L
  batch_size <- 2000L
  corrida_sql <- if (is.null(corrida_id)) "NULL" else sprintf("'%s'::uuid", corrida_id)
  for (start in seq(1L, nrow(rows), by = batch_size)) {
    end <- min(nrow(rows), start + batch_size - 1L)
    chunk <- rows[start:end, , drop = FALSE]
    values <- vapply(seq_len(nrow(chunk)), function(i) {
      sprintf(
        "(%d, %s, '%s'::date, %s, %s, %s, %s, %s, %s, %s, NOW(), %s)",
        as.integer(chunk$id_estacion[i]),
        openmeteo_sql_ts(chunk$ts[i]),
        as.character(chunk$fecha_objetivo[i]),
        openmeteo_sql_dbl(chunk$t_2m[i]),
        openmeteo_sql_dbl(chunk$pp[i]),
        openmeteo_sql_dbl(chunk$u10[i]),
        openmeteo_sql_int(chunk$weather_code[i]),
        openmeteo_sql_int(chunk$is_day[i]),
        openmeteo_sql_int(chunk$pop[i]),
        openmeteo_sql_dbl(chunk$isoterma_m[i]),
        corrida_sql
      )
    }, character(1))
    q <- paste0(
      "INSERT INTO carta_por_hora (",
      "id_estacion, ts, fecha_objetivo, t_2m, pp, u10, weather_code, is_day, pop, isoterma_m, fetched_at, corrida_id",
      ") VALUES ",
      paste(values, collapse = ", "),
      " ON CONFLICT (id_estacion, ts) DO UPDATE SET ",
      "fecha_objetivo = EXCLUDED.fecha_objetivo, t_2m = EXCLUDED.t_2m, pp = EXCLUDED.pp, ",
      "u10 = EXCLUDED.u10, weather_code = EXCLUDED.weather_code, ",
      "is_day = EXCLUDED.is_day, pop = EXCLUDED.pop, isoterma_m = EXCLUDED.isoterma_m, ",
      "fetched_at = EXCLUDED.fetched_at, corrida_id = EXCLUDED.corrida_id"
    )
    n <- n + DBI::dbExecute(conn, q)
  }
  as.integer(n)
}

openmeteo_historico_valor_crudo <- function(valor, valor_crudo) {
  vc <- suppressWarnings(as.numeric(valor_crudo)[1])
  if (length(vc) == 1L && is.finite(vc)) return(vc)
  v <- suppressWarnings(as.numeric(valor)[1])
  if (length(v) == 1L && is.finite(v)) return(v)
  NA_real_
}

openmeteo_historico_dias_pct <- function(dias, esperados) {
  dias <- suppressWarnings(as.numeric(dias)[1])
  esperados <- suppressWarnings(as.numeric(esperados)[1])
  if (!is.finite(esperados) || esperados <= 0) return(100)
  if (!is.finite(dias) || dias < 0) dias <- 0
  round(100 * dias / esperados, 1)
}

# Filas crudas del pronostico de hoy que pasan al archivo historico.
# Leads 0-3: t_max, t_min, pp. Lead 0 ademas: viento medio, viento maximo y weather.
# La rafaga no entra: tiene su propio archivo. Si hay correccion, se guarda el crudo.
openmeteo_historico_filas_desde_diario <- function(rows, fecha_emision, var_ids) {
  empty <- data.frame(
    id_estacion = integer(0), fecha = as.Date(character(0)),
    id_variable = integer(0), valor = numeric(0), lead_days = integer(0),
    stringsAsFactors = FALSE
  )
  if (is.null(rows) || !is.data.frame(rows) || nrow(rows) == 0 || is.null(var_ids)) {
    return(empty)
  }
  emit <- as.Date(fecha_emision)
  lead <- as.integer(as.Date(rows$fecha_objetivo) - emit)
  ok <- is.finite(lead) & lead >= 0L & lead <= 3L & is.finite(suppressWarnings(as.numeric(rows$valor)))
  sub <- rows[ok, , drop = FALSE]
  if (nrow(sub) == 0) return(empty)
  sub$lead_days <- as.integer(as.Date(sub$fecha_objetivo) - emit)
  id_of <- function(name) {
    x <- var_ids[[name]]
    if (is.null(x) || length(x) != 1L || is.na(x)) return(NA_integer_)
    as.integer(x)
  }
  keep_any <- c(id_of("t_max"), id_of("t_min"), id_of("pp"))
  keep_lead0 <- c(id_of("u2_mean"), id_of("u_max_diario"), id_of("weather_code"))
  keep_any <- keep_any[!is.na(keep_any)]
  keep_lead0 <- keep_lead0[!is.na(keep_lead0)]
  sel <- sub$id_variable %in% keep_any |
    (sub$lead_days == 0L & sub$id_variable %in% keep_lead0)
  sub <- sub[sel, , drop = FALSE]
  if (nrow(sub) == 0) return(empty)
  vc <- if ("valor_crudo" %in% names(sub)) sub$valor_crudo else rep(NA_real_, nrow(sub))
  valor <- vapply(seq_len(nrow(sub)), function(i) {
    openmeteo_historico_valor_crudo(sub$valor[i], vc[i])
  }, numeric(1))
  fin <- is.finite(valor)
  if (!any(fin)) return(empty)
  data.frame(
    id_estacion = as.integer(sub$id_estacion[fin]),
    fecha = as.Date(sub$fecha_objetivo[fin]),
    id_variable = as.integer(sub$id_variable[fin]),
    valor = as.numeric(valor[fin]),
    lead_days = as.integer(sub$lead_days[fin]),
    stringsAsFactors = FALSE
  )
}

openmeteo_archivar_historico <- function(conn, rows, fecha_emision, var_ids) {
  sub <- openmeteo_historico_filas_desde_diario(rows, fecha_emision, var_ids)
  if (nrow(sub) == 0) return(0L)
  n <- 0L
  batch <- 500L
  for (start in seq(1L, nrow(sub), by = batch)) {
    end <- min(nrow(sub), start + batch - 1L)
    chunk <- sub[start:end, , drop = FALSE]
    values <- vapply(seq_len(nrow(chunk)), function(i) {
      sprintf(
        "(%d, '%s'::date, %d, %.10g, NOW(), %d)",
        as.integer(chunk$id_estacion[i]),
        as.character(as.Date(chunk$fecha[i])),
        as.integer(chunk$id_variable[i]),
        as.numeric(chunk$valor[i]),
        as.integer(chunk$lead_days[i])
      )
    }, character(1))
    q <- paste0(
      "INSERT INTO archivo_openmeteo ",
      "(id_estacion, fecha, id_variable, valor, fetched_at, lead_days) VALUES ",
      paste(values, collapse = ", "),
      " ON CONFLICT (id_estacion, fecha, id_variable, lead_days) DO NOTHING"
    )
    n <- n + as.integer(DBI::dbExecute(conn, q))
  }
  if (exists("openmeteo_historico_cobertura_rebuild", mode = "function")) {
    tryCatch(
      openmeteo_historico_cobertura_rebuild(conn, fechas = unique(sub$fecha)),
      error = function(e) message("[openmeteo] cobertura historico: ", conditionMessage(e))
    )
  }
  n
}

openmeteo_publicado_abrs <- function() c("pp", "t_max", "t_min", "weather_code")

# Filas que van al archivo: cuatro variables, anticipacion 0-3. No toca la red.
openmeteo_publicado_preparar <- function(rows, fecha_emision, var_ids, modo_pp = "off",
                                          corrida_id = NULL) {
  if (is.null(rows) || !is.data.frame(rows) || nrow(rows) == 0) {
    return(if (is.data.frame(rows)) rows[0, , drop = FALSE] else data.frame())
  }
  if (is.null(var_ids)) return(rows[0, , drop = FALSE])
  ids <- var_ids[openmeteo_publicado_abrs()]
  ids <- ids[!vapply(ids, function(x) is.null(x) || length(x) == 0L || is.na(x), logical(1))]
  id_keep <- as.integer(unlist(ids, use.names = FALSE))
  if (length(id_keep) == 0L) return(empty)
  sub <- rows[rows$id_variable %in% id_keep, , drop = FALSE]
  if (nrow(sub) == 0) return(sub)
  emit <- as.Date(fecha_emision)
  lead <- as.integer(as.Date(sub$fecha_objetivo) - emit)
  ok <- is.finite(lead) & lead >= 0L & lead <= 3L & is.finite(sub$valor)
  sub <- sub[ok, , drop = FALSE]
  if (nrow(sub) == 0) return(sub)
  sub$fecha_emision <- emit
  sub$lead_days <- as.integer(as.Date(sub$fecha_objetivo) - emit)
  modo <- as.character(modo_pp %||% "off")[1]
  modo <- gsub("[^a-z]", "", tolower(modo))
  if (!nzchar(modo)) modo <- "off"
  sub$modo_pp <- modo
  if (!is.null(corrida_id) && nzchar(as.character(corrida_id)[1])) {
    sub$corrida_id <- as.character(corrida_id)[1]
  } else if (!"corrida_id" %in% names(sub)) {
    sub$corrida_id <- NA_character_
  }
  if (!"valor_crudo" %in% names(sub)) sub$valor_crudo <- NA_real_
  sub
}

openmeteo_publicado_sql_uuid <- function(x) {
  x <- as.character(x)[1]
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return("NULL")
  sprintf("'%s'::uuid", x)
}

openmeteo_archivar_publicado <- function(conn, rows, corrida_id, fecha_emision, var_ids,
                                          modo_pp = "off") {
  sub <- openmeteo_publicado_preparar(
    rows, fecha_emision, var_ids, modo_pp = modo_pp, corrida_id = corrida_id
  )
  if (nrow(sub) == 0) return(0L)
  n <- 0L
  batch <- 500L
  for (start in seq(1L, nrow(sub), by = batch)) {
    end <- min(nrow(sub), start + batch - 1L)
    chunk <- sub[start:end, , drop = FALSE]
    values <- vapply(seq_len(nrow(chunk)), function(i) {
      crudo <- chunk$valor_crudo[i]
      crudo_sql <- if (is.finite(crudo)) sprintf("%.10g", as.numeric(crudo)) else "NULL"
      modo <- chunk$modo_pp[i]
      modo_sql <- if (is.na(modo) || !nzchar(modo)) "NULL" else sprintf("'%s'", modo)
      sprintf(
        "(%d, %d, '%s'::date, '%s'::date, %d, %.10g, %s, %s, %s)",
        as.integer(chunk$id_estacion[i]),
        as.integer(chunk$id_variable[i]),
        as.character(as.Date(chunk$fecha_objetivo[i])),
        as.character(as.Date(chunk$fecha_emision[i])),
        as.integer(chunk$lead_days[i]),
        as.numeric(chunk$valor[i]),
        crudo_sql,
        modo_sql,
        openmeteo_publicado_sql_uuid(chunk$corrida_id[i])
      )
    }, character(1))
    q <- paste0(
      "INSERT INTO carta_impresa ",
      "(id_estacion, id_variable, fecha_objetivo, fecha_emision, lead_days, ",
      "valor, valor_crudo, modo_pp, corrida_id) VALUES ",
      paste(values, collapse = ", "),
      " ON CONFLICT (id_estacion, id_variable, fecha_objetivo, fecha_emision) DO NOTHING"
    )
    n <- n + as.integer(DBI::dbExecute(conn, q))
  }
  n
}

# Misma fecha civil que el filtro de lectura. CURRENT_DATE en UTC, a las 21:00
# de Chile, borraria el hoy que el usuario todavia no ha vivido.
OPENMETEO_SQL_HOY_CL <- "(CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date"

openmeteo_prune_old <- function(conn) {
  DBI::dbExecute(conn, paste0(
    "DELETE FROM carta_del_mirador WHERE fecha_objetivo < ", OPENMETEO_SQL_HOY_CL
  ))
  DBI::dbExecute(conn, paste0(
    "DELETE FROM carta_por_hora WHERE fecha_objetivo < ", OPENMETEO_SQL_HOY_CL
  ))
}

openmeteo_prune_future_beyond <- function(conn, station_ids, forecast_days) {
  forecast_days <- as.integer(forecast_days)
  if (is.na(forecast_days) || forecast_days < 1L) return(0L)
  station_ids <- unique(as.integer(unlist(station_ids)))
  station_ids <- station_ids[!is.na(station_ids) & station_ids > 0L]
  if (length(station_ids) == 0) return(0L)
  n_d <- DBI::dbExecute(
    conn,
    paste0(
      "DELETE FROM carta_del_mirador
     WHERE id_estacion = ANY($1::int[])
       AND fecha_objetivo > ", OPENMETEO_SQL_HOY_CL, " + ($2::int - 1)"
    ),
    params = list(openmeteo_pg_int_array(station_ids), forecast_days)
  )
  n_h <- DBI::dbExecute(
    conn,
    paste0(
      "DELETE FROM carta_por_hora
     WHERE id_estacion = ANY($1::int[])
       AND fecha_objetivo > ", OPENMETEO_SQL_HOY_CL, " + ($2::int - 1)"
    ),
    params = list(openmeteo_pg_int_array(station_ids), forecast_days)
  )
  as.integer(n_d + n_h)
}

openmeteo_corrida_start <- function(conn) {
  df <- DBI::dbGetQuery(
    conn,
    "INSERT INTO corrida_de_carta (started_at, status) VALUES (NOW(), 'running') RETURNING id"
  )
  as.character(df$id[1])
}

openmeteo_corrida_finish <- function(conn, corrida_id, status, n_estaciones, n_chunks, n_upserts, api_calls_est, error_msg = NULL) {
  err <- error_msg
  if (is.null(err) || length(err) == 0 || !nzchar(as.character(err)[1])) err <- NA_character_
  else err <- as.character(err)[1]
  DBI::dbExecute(
    conn,
    "UPDATE corrida_de_carta SET finished_at = NOW(), status = $2, n_estaciones = $3,
     n_chunks = $4, n_upserts = $5, api_calls_est = $6, error_msg = $7 WHERE id = $1::uuid",
    params = list(corrida_id, status, as.integer(n_estaciones), as.integer(n_chunks),
                  as.integer(n_upserts), as.numeric(api_calls_est), err)
  )
}

openmeteo_refresh_all <- function(conn, eids = NULL, limit = NULL, dry_run = FALSE) {
  cfg <- openmeteo_config()
  var_ids <- openmeteo_load_variable_ids(conn)
  bias_coefs <- if (exists("openmeteo_load_bias_coefs", mode = "function")) {
    openmeteo_load_bias_coefs(conn)
  } else {
    data.frame()
  }
  var_abr_map <- if (exists("openmeteo_bias_build_var_abr_map", mode = "function")) {
    openmeteo_bias_build_var_abr_map(var_ids)
  } else {
    NULL
  }

  sp_fetch <- NULL
  if (exists("profile_span_begin", mode = "function")) {
    sp_fetch <- tryCatch(
      profile_span_begin("phase.fetch_estaciones", "phase", attrs = list(step = "refresh_pronostico")),
      error = function(e) NULL
    )
  }
  stations <- openmeteo_fetch_estaciones_calidad(conn, eids = eids, limit = limit)
  if (exists("profile_span_end", mode = "function") && !is.null(sp_fetch)) {
    tryCatch(profile_span_end(sp_fetch, list(n_estaciones = nrow(stations))), error = function(e) NULL)
  }
  if (nrow(stations) == 0) {
    message("[openmeteo] Sin estaciones elegibles")
    return(list(ok = TRUE, n_estaciones = 0L, n_upserts = 0L))
  }
  wind_map <- openmeteo_wind_map(conn, stations$id, var_ids)
  chunks <- openmeteo_chunk_df(stations, cfg$chunk_size)
  api_calls_est <- openmeteo_calls_estimate(nrow(stations), cfg$forecast_days, OPENMETEO_N_VARS)
  message("[openmeteo] Estaciones=", nrow(stations), " chunks=", length(chunks),
          " calls_est=", api_calls_est, " dry_run=", dry_run)

  if (dry_run) {
    for (i in seq_along(chunks)) {
      sub <- chunks[[i]]
      message("[openmeteo] chunk ", i, ": ", nrow(sub), " estaciones (", sub$id[1], "..", sub$id[nrow(sub)], ")")
    }
    return(list(ok = TRUE, n_estaciones = nrow(stations), n_upserts = 0L, dry_run = TRUE))
  }

  corrida_id <- openmeteo_corrida_start(conn)
  fecha_emision <- as.Date(Sys.time(), tz = "America/Santiago")
  modo_pp <- if (exists("openmeteo_bias_pp_intensidad", mode = "function")) {
    openmeteo_bias_pp_intensidad()
  } else {
    "off"
  }
  total_upserts <- 0L
  total_upserts_hora <- 0L
  status <- "ok"
  err_msg <- NULL
  fetch_fn <- function(sub, retries) {
    openmeteo_fetch_chunk(
      sub, var_ids, wind_map, cfg, bias_coefs, var_abr_map, retries = retries
    )
  }
  # Diario y horario de la estacion salen juntos. Si el horario falla, no queda
  # el diario de esta corrida pegado a las horas de la anterior.
  store_fetched <- function(chunk, label) {
    daily_rows <- chunk$daily
    hora_rows <- chunk$hourly
    n_rows <- if (is.data.frame(daily_rows)) nrow(daily_rows) else 0L
    stored <- tryCatch(
      DBI::dbWithTransaction(conn, {
        n_up <- 0L
        if (is.data.frame(daily_rows) && nrow(daily_rows) > 0L) {
          n_up <- openmeteo_upsert_rows(conn, daily_rows, corrida_id)
        }
        if (isTRUE(chunk$hourly_failed) && n_up > 0L) {
          stop("hourly vacio con daily presente", call. = FALSE)
        }
        n_h <- openmeteo_upsert_hora_rows(conn, hora_rows, corrida_id)
        list(n_up = n_up, n_h = n_h)
      }),
      error = function(e) {
        message("[openmeteo] upsert fail ", label, ": ", conditionMessage(e))
        status <<- "partial"
        list(n_up = 0L, n_h = 0L)
      }
    )
    total_upserts <<- total_upserts + stored$n_up
    total_upserts_hora <<- total_upserts_hora + stored$n_h
    if (stored$n_up > 0L) {
      tryCatch(
        openmeteo_archivar_publicado(
          conn, daily_rows, corrida_id, fecha_emision, var_ids, modo_pp
        ),
        error = function(e) {
          message("[openmeteo] archivo publicado fail ", label, ": ", conditionMessage(e))
        }
      )
      tryCatch(
        openmeteo_archivar_historico(conn, daily_rows, fecha_emision, var_ids),
        error = function(e) {
          message("[openmeteo] archivo historico fail ", label, ": ", conditionMessage(e))
        }
      )
    }
    list(n_up = stored$n_up, n_h = stored$n_h, n_rows = n_rows)
  }
  tryCatch({
    failed_sets <- list()
    consecutive_dead <- 0L
    park_rest <- FALSE
    for (i in seq_along(chunks)) {
      sub <- chunks[[i]]
      message("[openmeteo] chunk ", i, "/", length(chunks), " (n=", nrow(sub), ")")
      if (park_rest) {
        failed_sets[[length(failed_sets) + 1L]] <- sub
        next
      }
      sp_chunk <- NULL
      if (exists("profile_span_begin", mode = "function")) {
        sp_chunk <- tryCatch(
          profile_span_begin(
            paste0("phase.chunk.", i), "phase",
            attrs = list(step = "refresh_pronostico", chunk = i, n_estaciones = nrow(sub))
          ),
          error = function(e) NULL
        )
      }
      # Una particion: 100 -> 50+50. El mismo URL grande es el que Open-Meteo corta.
      res <- openmeteo_fetch_stations_resilient(
        sub, fetch_fn, min_n = max(1L, nrow(sub) %/% 2L)
      )
      stored <- store_fetched(res$chunk, paste0("chunk ", i))
      if (is.data.frame(res$failed) && nrow(res$failed) > 0L) {
        failed_sets[[length(failed_sets) + 1L]] <- res$failed
      }
      n_daily <- stored$n_rows
      dead <- n_daily == 0L && is.data.frame(res$failed) && nrow(res$failed) > 0L
      if (dead) {
        consecutive_dead <- consecutive_dead + 1L
        if (consecutive_dead >= 2L) {
          message("[openmeteo] dos chunks sin datos; el resto espera el reintento")
          park_rest <- TRUE
        }
      } else {
        consecutive_dead <- 0L
      }
      if (exists("profile_span_end", mode = "function") && !is.null(sp_chunk)) {
        tryCatch(
          profile_span_end(sp_chunk, list(
            n_upserts = stored$n_up, n_rows = stored$n_rows, n_upserts_hora = stored$n_h
          )),
          error = function(e) NULL
        )
      }
      if (cfg$chunk_pause_ms > 0 && i < length(chunks) && !park_rest) {
        pause_ms <- cfg$chunk_pause_ms
        if (exists("profile_span_instant", mode = "function")) {
          tryCatch(
            profile_span_instant(
              "sleep.chunk_pause", "sleep", pause_ms,
              parent = if (exists("profile_orch_parent", mode = "function")) profile_orch_parent() else NULL,
              attrs = list(step = "refresh_pronostico", fuente = "openmeteo"),
              fine = TRUE
            ),
            error = function(e) NULL
          )
        }
        Sys.sleep(pause_ms / 1000)
      }
    }
    if (length(failed_sets) > 0L) {
      n_pending <- sum(vapply(failed_sets, nrow, integer(1)))
      pause_sec <- suppressWarnings(as.integer(Sys.getenv("OPENMETEO_STREAM_RETRY_PAUSE_SEC", "30")))
      if (length(pause_sec) != 1L || is.na(pause_sec) || pause_sec < 0L) pause_sec <- 30L
      message("[openmeteo] reintento de ", n_pending, " estaciones tras ", pause_sec, "s")
      if (pause_sec > 0L) Sys.sleep(pause_sec)
      still <- list()
      last_err <- NULL
      for (j in seq_along(failed_sets)) {
        sub <- failed_sets[[j]]
        if (!is.data.frame(sub) || nrow(sub) < 1L) next
        message("[openmeteo] reintento grupo ", j, "/", length(failed_sets), " (n=", nrow(sub), ")")
        res <- openmeteo_fetch_stations_resilient(
          sub, fetch_fn, min_n = max(1L, nrow(sub) %/% 2L)
        )
        store_fetched(res$chunk, paste0("reintento ", j))
        if (is.data.frame(res$failed) && nrow(res$failed) > 0L) {
          still[[length(still) + 1L]] <- res$failed
        }
        if (!is.null(res$error)) last_err <- conditionMessage(res$error)
      }
      if (length(still) > 0L) {
        n_left <- sum(vapply(still, nrow, integer(1)))
        status <<- if (total_upserts > 0L) "partial" else "error"
        detail <- if (!is.null(last_err) && nzchar(last_err)) paste0(" ", last_err) else ""
        stop(
          "Open-Meteo fallo tras reintento: ", n_left,
          " estaciones sin pronostico.", detail,
          call. = FALSE
        )
      }
    }
    openmeteo_prune_old(conn)
    openmeteo_prune_future_beyond(conn, stations$id, cfg$forecast_days)
  }, error = function(e) {
    status <<- if (total_upserts > 0L) "partial" else "error"
    err_msg <<- conditionMessage(e)
    stop(e)
  }, finally = {
    openmeteo_corrida_finish(
      conn, corrida_id, status, nrow(stations), length(chunks),
      total_upserts, api_calls_est, err_msg
    )
  })
  message("[openmeteo] upserts_hora=", total_upserts_hora)
  list(
    ok = TRUE, n_estaciones = nrow(stations), n_upserts = total_upserts,
    n_upserts_hora = total_upserts_hora, corrida_id = corrida_id
  )
}
