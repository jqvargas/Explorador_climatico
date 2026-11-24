# Catalogo y pronostico de hoy para el widget de la TWA.
# Al servidor solo llega el id. Este archivo no arma rankings ni viento.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

widget_hoy_date_sql <- function() {
  if (exists("SQL_HOY_CL", inherits = TRUE)) {
    as.character(get("SQL_HOY_CL", inherits = TRUE))
  } else {
    "(CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date"
  }
}

# Entero >= 1. Rechaza "1.5", "abc" y vacio. El 400 HTTP lo pone el endpoint.
widget_parse_id <- function(id) {
  if (is.null(id) || length(id) == 0L) return(NULL)
  s <- trimws(as.character(id[[1]]))
  if (!nzchar(s) || !grepl("^[0-9]+$", s) || nchar(s) > 9L) return(NULL)
  eid <- suppressWarnings(as.integer(s))
  if (length(eid) != 1L || is.na(eid) || eid < 1L) return(NULL)
  eid
}

# Indice de la hora cuyo intervalo [ts, ts+1h) contiene `now`. NA si no hay.
widget_hour_index <- function(ts, now) {
  if (length(ts) == 0L) return(NA_integer_)
  if (!inherits(ts, "POSIXct")) ts <- as.POSIXct(ts, tz = "UTC")
  if (!inherits(now, "POSIXct")) now <- as.POSIXct(now, tz = "UTC")
  now <- now[[1]]
  if (is.na(now)) return(NA_integer_)
  hit <- which(!is.na(ts) & ts <= now & now < (ts + 3600))
  if (length(hit) == 0L) return(NA_integer_)
  as.integer(hit[[length(hit)]])
}

widget_num <- function(valor, nombre_abr) {
  v <- suppressWarnings(as.numeric(valor))
  if (length(v) == 0L || !is.finite(v[[1]])) return(NULL)
  v <- v[[1]]
  if (exists("display_valor", mode = "function")) {
    v <- suppressWarnings(as.numeric(display_valor(v, nombre_abr)))
  }
  if (length(v) != 1L || !is.finite(v)) return(NULL)
  v
}

widget_int <- function(valor) {
  v <- suppressWarnings(as.integer(valor))
  if (length(v) != 1L || is.na(v)) return(NULL)
  v
}

widget_station_name <- function(nombre, nom_com, id_fuente, nombre_publico = "") {
  if (exists("estacion_nombre_view", mode = "function")) {
    view <- estacion_nombre_view(nombre, nom_com, id_fuente)
    nm <- if (is.list(view)) trimws(as.character(view$name %||% "")) else ""
    if (length(nm) == 1L && !is.na(nm) && nzchar(nm)) return(nm)
  }
  pub <- trimws(as.character(nombre_publico %||% ""))
  if (length(pub) == 1L && !is.na(pub) && nzchar(pub)) return(pub)
  raw <- trimws(as.character(nombre %||% ""))
  if (length(raw) != 1L || is.na(raw)) return("")
  raw
}

widget_comuna <- function(nom_com) {
  s <- trimws(as.character(nom_com %||% ""))
  if (length(s) != 1L || is.na(s)) return("")
  s
}

widget_fold <- function(x) {
  if (exists("estacion_fold", mode = "function")) return(estacion_fold(x))
  tolower(trimws(as.character(x)))
}

# ponytail: el id de t_max va en el pkey. Filtrar nombre_abr despues del join
# lee las 7 variables (~1.5 s). El escalar entra al indice (~35 ms, 2026-11-15).
# Techo: si hubiera dos filas t_max, LIMIT 1 elige una. nombre_abr no es unique.
widget_catalog_df <- function() {
  min_anios <- if (exists("pronostico_min_anios_display", mode = "function")) {
    as.integer(pronostico_min_anios_display())
  } else {
    0L
  }
  if (length(min_anios) != 1L || is.na(min_anios) || min_anios < 0L) min_anios <- 0L
  q <- paste0(
    "SELECT e.id, e.nombre, COALESCE(e.nombre_publico, '') AS nombre_publico, ",
    "COALESCE(e.nom_com, '') AS nom_com, e.id_fuente, e.lat, e.lon ",
    "FROM mirador e ",
    "JOIN estacion_calidad ec ON ec.id_estacion = e.id ",
    "JOIN carta_del_mirador p ON p.id_estacion = e.id ",
    " AND p.fecha_objetivo = ", widget_hoy_date_sql(), " ",
    " AND p.id_variable = (SELECT id FROM magnitud WHERE nombre_abr = 't_max' LIMIT 1) ",
    "WHERE ec.nivel IN ('excelente', 'buena') ",
    " AND COALESCE(ec.anios_span, 0) >= ", min_anios, " ",
    " AND e.lat BETWEEN -56 AND -17 AND e.lon BETWEEN -76 AND -66 ",
    " AND e.lat IS NOT NULL AND e.lon IS NOT NULL ",
    "ORDER BY e.id"
  )
  db_query(q)
}

widget_public_station <- function(nombre, nom_com, id_fuente, nombre_publico, id, lat, lon) {
  list(
    id = as.integer(id),
    name = widget_station_name(nombre, nom_com, id_fuente, nombre_publico),
    comuna = widget_comuna(nom_com),
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  )
}

widget_public_list <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(list())
  lapply(seq_len(nrow(df)), function(i) {
    r <- df[i, , drop = FALSE]
    widget_public_station(
      r$nombre, r$nom_com, r$id_fuente, r$nombre_publico, r$id, r$lat, r$lon
    )
  })
}

widget_catalog_build <- function() {
  widget_public_list(widget_catalog_df())
}

# DMC (id_fuente 2) cuyo nombre contiene "Quinta Normal" y esta en el catalogo.
# NULL si no hay: el caller aborta. No se elige otra ciudad.
widget_pick_default <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(NULL)
  fuente <- suppressWarnings(as.integer(df$id_fuente))
  dmc <- df[!is.na(fuente) & fuente == 2L, , drop = FALSE]
  if (nrow(dmc) == 0L) return(NULL)
  nm <- vapply(seq_len(nrow(dmc)), function(i) {
    widget_station_name(dmc$nombre[i], dmc$nom_com[i], dmc$id_fuente[i], dmc$nombre_publico[i])
  }, character(1))
  needle <- "quinta normal"
  ok <- grepl(needle, widget_fold(nm), fixed = TRUE) |
    grepl(needle, widget_fold(dmc$nombre_publico), fixed = TRUE)
  if (!any(ok)) return(NULL)
  hit <- dmc[ok, , drop = FALSE]
  hit_nm <- nm[ok]
  ord <- order(nchar(hit_nm), suppressWarnings(as.integer(hit$id)))
  hit[ord[[1]], , drop = FALSE]
}

widget_asset_payload <- function(df = NULL) {
  if (is.null(df)) df <- widget_catalog_df()
  def <- widget_pick_default(df)
  if (is.null(def) || nrow(def) == 0L) {
    stop("No hay DMC con nombre publico Quinta Normal y pronostico de hoy")
  }
  list(
    default_station_id = as.integer(def$id[[1]]),
    stations = widget_public_list(df)
  )
}

widget_chip <- function(df, abr) {
  if (is.null(df) || nrow(df) == 0L || !"nombre_abr" %in% names(df)) return(NULL)
  hit <- df[tolower(as.character(df$nombre_abr)) == abr, , drop = FALSE]
  if (nrow(hit) == 0L) return(NULL)
  widget_num(hit$valor[[1]], abr)
}

widget_updated_at <- function(fetched_at) {
  if (length(fetched_at) == 0L) return(NULL)
  ts <- fetched_at[!is.na(fetched_at)]
  if (length(ts) == 0L) return(NULL)
  if (!inherits(ts, "POSIXct")) ts <- suppressWarnings(as.POSIXct(ts, tz = "UTC"))
  ts <- ts[!is.na(ts)]
  if (length(ts) == 0L) return(NULL)
  if (!exists("pronostico_ts_iso_santiago", mode = "function")) return(NULL)
  out <- as.character(pronostico_ts_iso_santiago(max(ts)))
  if (length(out) != 1L || is.na(out) || !nzchar(out)) return(NULL)
  out
}

widget_hoy_hours <- function(eid) {
  if (exists("pronostico_hora_tabla_existe", mode = "function") &&
      !isTRUE(pronostico_hora_tabla_existe())) {
    return(NULL)
  }
  q <- paste0(
    "SELECT ts, t_2m, weather_code, is_day, pop, pp ",
    "FROM carta_por_hora ",
    "WHERE id_estacion = $1 AND fecha_objetivo = ", widget_hoy_date_sql(), " ",
    "ORDER BY ts"
  )
  tryCatch(db_query(q, params = list(as.integer(eid))), error = function(e) NULL)
}

widget_santiago_hour <- function(ts) {
  if (length(ts) == 0L) return(NULL)
  if (!inherits(ts, "POSIXct")) ts <- suppressWarnings(as.POSIXct(ts, tz = "UTC"))
  ts <- ts[[1]]
  if (is.na(ts)) return(NULL)
  h <- suppressWarnings(as.integer(format(ts, tz = "America/Santiago", "%H")))
  if (length(h) != 1L || is.na(h)) return(NULL)
  h
}

# Seis horas desde la que contiene `now`, inclusive. Sin fila o sin mm, se omite.
widget_next_hours <- function(hours, now = Sys.time()) {
  if (is.null(hours) || nrow(hours) == 0L || !"pp" %in% names(hours)) return(list())
  idx <- widget_hour_index(hours$ts, now)
  if (is.na(idx)) return(list())
  last <- min(nrow(hours), idx + 5L)
  out <- list()
  for (i in seq.int(idx, last)) {
    hh <- widget_santiago_hour(hours$ts[[i]])
    mm <- widget_num(hours$pp[[i]], "pp")
    if (is.null(hh) || is.null(mm)) next
    out[[length(out) + 1L]] <- list(h = hh, mm = mm)
  }
  out
}

widget_hour_pop <- function(hours, now = Sys.time()) {
  if (is.null(hours) || nrow(hours) == 0L || !"pop" %in% names(hours)) return(NULL)
  idx <- widget_hour_index(hours$ts, now)
  if (is.na(idx)) return(NULL)
  widget_int(hours$pop[[idx]])
}

# Mejor alerta extrema vigente de esta estación. Sin ranking ni coordenadas.
widget_alerta <- function(eid) {
  if (!exists("alerta_temprana_tabla_existe", mode = "function") ||
      !isTRUE(alerta_temprana_tabla_existe())) {
    return(NULL)
  }
  if (!exists("alerta_temprana_alerta_valida_sql", mode = "function") ||
      !exists("alerta_temprana_historia_clause", mode = "function")) {
    return(NULL)
  }
  q <- paste0(
    "SELECT v.nombre_abr AS variable, a.fecha_objetivo, a.valor_pronostico, ",
    "COALESCE(v.unidad, '') AS unidad ",
    "FROM aviso_de_madrugada a ",
    "JOIN magnitud v ON v.id = a.id_variable ",
    "WHERE a.id_estacion = $1 ",
    "AND a.fecha_objetivo >= CURRENT_DATE ",
    "AND ", alerta_temprana_historia_clause("a.id_estacion"), " ",
    "AND ", alerta_temprana_alerta_valida_sql("a"), " ",
    "ORDER BY a.ranking ASC, a.fecha_objetivo ASC LIMIT 1"
  )
  df <- tryCatch(db_query(q, params = list(as.integer(eid))), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0L) return(NULL)
  variable <- as.character(df$variable[[1]] %||% "")
  valor <- widget_num(df$valor_pronostico[[1]], variable)
  fecha <- as.character(as.Date(df$fecha_objetivo[[1]]))
  if (!nzchar(variable) || !nzchar(fecha) || fecha == "NA") return(NULL)
  list(
    variable = variable,
    fecha = fecha,
    valor = valor,
    unidad = as.character(df$unidad[[1]] %||% "")
  )
}

widget_pop_max <- function(hours) {
  if (is.null(hours) || nrow(hours) == 0L || !"pop" %in% names(hours)) return(NULL)
  pops <- suppressWarnings(as.integer(hours$pop))
  pops <- pops[!is.na(pops)]
  if (length(pops) == 0L) return(NULL)
  as.integer(max(pops))
}

widget_hoy_build <- function(eid) {
  eid <- widget_parse_id(eid)
  if (is.null(eid)) return(list(error = "id inv\u00e1lido"))
  if (!exists("pronostico_estacion_visible", mode = "function") ||
      !isTRUE(pronostico_estacion_visible(eid))) {
    return(list(error = "Sin pron\u00f3stico para esta estaci\u00f3n"))
  }
  q <- paste0(
    "SELECT e.id, e.nombre, COALESCE(e.nombre_publico, '') AS nombre_publico, ",
    "COALESCE(e.nom_com, '') AS nom_com, e.id_fuente, ",
    "v.nombre_abr, p.valor, p.fetched_at ",
    "FROM mirador e ",
    "LEFT JOIN carta_del_mirador p ON p.id_estacion = e.id ",
    " AND p.fecha_objetivo = ", widget_hoy_date_sql(), " ",
    "LEFT JOIN magnitud v ON v.id = p.id_variable ",
    " AND v.nombre_abr IN ('t_min', 't_max', 'pp') ",
    "WHERE e.id = $1"
  )
  rows <- db_query(q, params = list(eid))
  if (is.null(rows) || nrow(rows) == 0L) {
    return(list(error = "Sin pron\u00f3stico para esta estaci\u00f3n"))
  }
  abr <- as.character(rows$nombre_abr)
  chips <- rows[!is.na(abr) & nzchar(abr), , drop = FALSE]
  tmax <- widget_chip(chips, "t_max")
  if (is.null(tmax)) {
    return(list(error = "Sin pron\u00f3stico para esta estaci\u00f3n"))
  }
  hours <- widget_hoy_hours(eid)
  idx <- if (is.null(hours) || nrow(hours) == 0L) {
    NA_integer_
  } else {
    widget_hour_index(hours$ts, Sys.time())
  }
  current_temp <- NULL
  condition_code <- NULL
  is_day <- NULL
  if (!is.na(idx)) {
    current_temp <- widget_num(hours$t_2m[[idx]], "t_2m")
    condition_code <- widget_int(hours$weather_code[[idx]])
    is_day <- widget_int(hours$is_day[[idx]])
    if (!is.null(is_day) && !is_day %in% c(0L, 1L)) is_day <- NULL
  }
  list(
    station_id = eid,
    name = widget_station_name(
      rows$nombre[[1]], rows$nom_com[[1]], rows$id_fuente[[1]], rows$nombre_publico[[1]]
    ),
    comuna = widget_comuna(rows$nom_com[[1]]),
    current_temp = current_temp,
    tmin = widget_chip(chips, "t_min"),
    tmax = tmax,
    precip_prob = widget_pop_max(hours),
    precip_mm = widget_chip(chips, "pp"),
    condition_code = condition_code,
    is_day = is_day,
    hour_pop = widget_hour_pop(hours),
    hours = widget_next_hours(hours),
    alert = widget_alerta(eid),
    updated_at = widget_updated_at(chips$fetched_at)
  )
}

# ponytail: self-check al sourcear. Sin base.
local({
  base <- as.POSIXct("2026-11-15 11:00:00", tz = "UTC")
  ts <- base + c(0, 3600, 7200)
  stopifnot(identical(widget_hour_index(ts, base + 1800), 1L))
  stopifnot(identical(widget_hour_index(ts, base), 1L))
  stopifnot(identical(widget_hour_index(ts, base + 3600), 2L))
  stopifnot(is.na(widget_hour_index(ts, base - 1)))
  stopifnot(is.na(widget_hour_index(ts, base + 3 * 3600)))
  empty <- as.POSIXct(numeric(0), origin = "1970-01-01", tz = "UTC")
  stopifnot(is.na(widget_hour_index(empty, base)))
  sample <- data.frame(
    ts = as.POSIXct("2026-11-16 15:00:00", tz = "UTC") + (0:6) * 3600,
    pp = c(0.1, 0.2, 0, 1, 2, 3, 9),
    pop = c(10L, 20L, 30L, 40L, 50L, 60L, 70L)
  )
  nxt <- widget_next_hours(sample, as.POSIXct("2026-11-16 15:30:00", tz = "UTC"))
  stopifnot(length(nxt) == 6L)
  stopifnot(identical(nxt[[1]]$mm, 0.1))
  stopifnot(identical(nxt[[6]]$mm, 3))
  stopifnot(identical(widget_santiago_hour(sample$ts[[1]]), 12L))
  stopifnot(identical(widget_hour_pop(sample, as.POSIXct("2026-11-16 15:30:00", tz = "UTC")), 10L))
  stopifnot(identical(widget_parse_id("3939"), 3939L))
  stopifnot(identical(widget_parse_id("007"), 7L))
  stopifnot(is.null(widget_parse_id("1.5")))
  stopifnot(is.null(widget_parse_id("abc")))
  stopifnot(is.null(widget_parse_id("0")))
  stopifnot(is.null(widget_parse_id("-3")))
  stopifnot(is.null(widget_parse_id("")))
  stopifnot(is.null(widget_parse_id("10000000000")))
  invisible(NULL)
})
