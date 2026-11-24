# Payload JSON pronostico (lectura SQL + rankings desde aviso_de_madrugada / fallback SQL)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

PRONOSTICO_VAR_ABRS <- c("pp", "t_max", "t_min", "u2_mean", "u_max_diario", "u_10_kt")

pronostico_tabla_existe <- function() {
  chk <- tryCatch(
    db_query("SELECT 1 FROM information_schema.tables WHERE table_name = 'carta_del_mirador' LIMIT 1"),
    error = function(e) NULL
  )
  !is.null(chk) && nrow(chk) > 0
}

# Minimo de anios para mostrar/descargar OM (0 = todas buena|excelente).
# Alertas tempranas / rankings siguen en pronostico_min_anios_rank() (~20).
PRONOSTICO_MIN_ANIOS_DISPLAY <- 0L

pronostico_min_anios_rank <- function() {
  if (exists("alerta_temprana_min_anios", mode = "function")) {
    alerta_temprana_min_anios()
  } else {
    20L
  }
}

pronostico_min_anios_display <- function() PRONOSTICO_MIN_ANIOS_DISPLAY

pronostico_estacion_calidad <- function(eid) {
  eid <- as.integer(eid)
  if (is.na(eid) || eid < 1L) return(NULL)
  df <- db_query(
    "SELECT COALESCE(ec.nivel, 'limitada') AS nivel,
            COALESCE(ec.anios_span, 0) AS anios_span
     FROM mirador e
     LEFT JOIN estacion_calidad ec ON ec.id_estacion = e.id
     WHERE e.id = $1",
    params = list(eid)
  )
  if (nrow(df) == 0) return(NULL)
  list(
    nivel = as.character(df$nivel[1]),
    anios_span = as.integer(df$anios_span[1])
  )
}

pronostico_estacion_visible <- function(eid) {
  cal <- pronostico_estacion_calidad(eid)
  if (is.null(cal)) return(FALSE)
  cal$nivel %in% c("excelente", "buena") &&
    cal$anios_span >= pronostico_min_anios_display()
}

#' Elegible para alertas por pronostico y rankings historicos completos (>= 20 anos).
pronostico_estacion_elegible <- function(eid) {
  cal <- pronostico_estacion_calidad(eid)
  if (is.null(cal)) return(FALSE)
  cal$nivel %in% c("excelente", "buena") &&
    cal$anios_span >= pronostico_min_anios_rank()
}

pronostico_historia_limitada <- function(eid) {
  cal <- pronostico_estacion_calidad(eid)
  is.null(cal) || cal$anios_span < pronostico_min_anios_rank()
}

pronostico_fetched_iso <- function(fetched_at) {
  if (length(fetched_at) != 1L || is.na(fetched_at)) return(as.character(fetched_at))
  ts <- if (inherits(fetched_at, "POSIXt")) {
    as.POSIXct(fetched_at)
  } else {
    raw <- sub("\\.[0-9]+", "", trimws(as.character(fetched_at)))
    raw <- sub("(Z|\\+00)$", "", raw)
    suppressWarnings(as.POSIXct(raw, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
  }
  if (length(ts) != 1L || is.na(ts)) return(as.character(fetched_at))
  format(ts, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}

pronostico_meta_base <- function(fetched_at, dias_n, historia_limitada = FALSE) {
  stale_h <- as.numeric(difftime(Sys.time(), fetched_at, units = "hours"))
  stale <- is.finite(stale_h) && stale_h > 36
  list(
    fetched_at = pronostico_fetched_iso(fetched_at),
    stale = stale,
    fuente = "open-meteo",
    horizonte_dias = dias_n,
    atribucion = "Datos de pron\u00f3stico por Open-Meteo (CC BY 4.0)",
    historia_limitada = isTRUE(historia_limitada),
    horas_horizonte_dias = 3L,
    horas_alineadas = TRUE
  )
}

pronostico_json_encode <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null", digits = 10)
}

pronostico_source_rank_helpers <- function() {
  rank_path <- file.path("/app", "utils", "pronostico_rank_sql.R")
  if (file.exists(rank_path) && !exists("pronostico_fetch_alerta_rankings", mode = "function")) {
    source(rank_path, local = FALSE)
  }
}

pronostico_hora_tabla_existe <- function() {
  chk <- tryCatch(
    db_query("SELECT 1 FROM information_schema.tables WHERE table_name = 'carta_por_hora' LIMIT 1"),
    error = function(e) NULL
  )
  !is.null(chk) && nrow(chk) > 0
}

# Postgres y la API estan en UTC. Entre las 21:00 y las 24:00 de Chile
# CURRENT_DATE ya es manana y se deja fuera el hoy civil.
SQL_HOY_CL <- "(CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date"

pronostico_fetch_forecast_rows <- function(eid) {
  db_query(
    paste0(
      "SELECT p.fecha_objetivo, p.id_variable, p.valor, p.valor_crudo, p.fetched_at,
            v.nombre_abr, v.nombre, COALESCE(v.unidad, '') AS unidad
     FROM carta_del_mirador p
     JOIN magnitud v ON v.id = p.id_variable
     WHERE p.id_estacion = $1 AND p.fecha_objetivo >= ", SQL_HOY_CL, "
     ORDER BY p.fecha_objetivo, v.nombre_abr"
    ),
    params = list(as.integer(eid))
  )
}

pronostico_fetch_hora_rows <- function(eid) {
  if (!isTRUE(pronostico_hora_tabla_existe())) return(NULL)
  tryCatch(
    db_query(
      paste0(
        "SELECT ts, fecha_objetivo, t_2m, pp, u10, weather_code, is_day, pop
       FROM carta_por_hora
       WHERE id_estacion = $1
         AND fecha_objetivo >= ", SQL_HOY_CL, "
       ORDER BY ts"
      ),
      params = list(as.integer(eid))
    ),
    error = function(e) NULL
  )
}

pronostico_ts_iso_santiago <- function(ts) {
  fmt <- format(
    as.POSIXct(ts, tz = "UTC"),
    tz = "America/Santiago",
    format = "%Y-%m-%dT%H:%M:%S%z"
  )
  sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", fmt)
}

pronostico_chip_valor <- function(sub, abr) {
  hit <- sub[tolower(as.character(sub$nombre_abr)) == abr, , drop = FALSE]
  if (nrow(hit) == 0) return(NA_real_)
  display_valor(as.numeric(hit$valor[1]), abr)
}

pronostico_dia_tiene_crudo <- function(sub) {
  abrs <- tolower(as.character(sub$nombre_abr))
  crudo <- suppressWarnings(as.numeric(sub$valor_crudo))
  any(abrs %in% c("t_max", "t_min", "pp") & is.finite(crudo))
}

pronostico_alinear_horas <- function(hora_sub, chips) {
  if (is.null(hora_sub) || nrow(hora_sub) == 0) return(NULL)
  t <- as.numeric(hora_sub$t_2m)
  pp <- as.numeric(hora_sub$pp)
  u <- display_valor(as.numeric(hora_sub$u10), "u2_mean")
  wc <- suppressWarnings(as.integer(hora_sub$weather_code))
  day <- if ("is_day" %in% names(hora_sub)) {
    suppressWarnings(as.integer(hora_sub$is_day))
  } else {
    rep(NA_integer_, nrow(hora_sub))
  }
  pop <- if ("pop" %in% names(hora_sub)) {
    suppressWarnings(as.integer(hora_sub$pop))
  } else {
    rep(NA_integer_, nrow(hora_sub))
  }
  t_min_c <- chips$t_min
  t_max_c <- chips$t_max
  t_ok <- is.finite(t)
  if (is.finite(t_min_c) && is.finite(t_max_c) && any(t_ok)) {
    t_min_h <- min(t[t_ok])
    t_max_h <- max(t[t_ok])
    span <- t_max_h - t_min_h
    if (abs(span) < 1e-12) {
      t[t_ok] <- t[t_ok] + (t_max_c - t_max_h)
    } else {
      t[t_ok] <- t_min_c + (t[t_ok] - t_min_h) / span * (t_max_c - t_min_c)
    }
  }
  pp_chip <- chips$pp
  pp_sum <- sum(pp[is.finite(pp)], na.rm = TRUE)
  if (!is.finite(pp_chip) || pp_chip <= 0) {
    pp[is.finite(pp)] <- 0
  } else if (is.finite(pp_sum) && pp_sum > 0) {
    pp <- pp * (pp_chip / pp_sum)
  } else {
    pp[is.finite(pp)] <- 0
  }
  # Mismo criterio que la lluvia: un factor para que el pico coincida con el chip.
  # El viento medio queda aparte (su corrección es otra). Máximo horario 0 no se inventa.
  u_chip <- suppressWarnings(as.numeric(chips$u_max_diario)[1])
  u_ok <- is.finite(u)
  u_max_h <- if (any(u_ok)) max(u[u_ok]) else NA_real_
  if (is.finite(u_chip) && u_chip >= 0 && is.finite(u_max_h) && u_max_h > 0) {
    u <- u * (u_chip / u_max_h)
  }
  list(
    ts = as.list(unname(pronostico_ts_iso_santiago(hora_sub$ts))),
    t = as.list(unname(as.numeric(t))),
    pp = as.list(unname(as.numeric(pp))),
    u = as.list(unname(as.numeric(u))),
    wc = as.list(unname(as.integer(wc))),
    day = as.list(unname(as.integer(day))),
    pop = as.list(unname(as.integer(pop)))
  )
}

pronostico_hora_pad_int <- function(x, n) {
  if (is.null(x)) return(rep(NA_integer_, n))
  x <- suppressWarnings(as.integer(unlist(x, use.names = FALSE)))
  if (length(x) == 0L) return(rep(NA_integer_, n))
  if (length(x) < n) x <- c(x, rep(NA_integer_, n - length(x)))
  if (length(x) > n) x <- x[seq_len(n)]
  x
}

pronostico_attach_horas <- function(dias, hora_rows, daily_rows) {
  if (is.null(dias) || length(dias) == 0) return(dias)
  has_hora <- !is.null(hora_rows) && nrow(hora_rows) > 0
  lapply(dias, function(d) {
    f <- as.character(d$fecha)
    sub_d <- if (is.null(daily_rows) || nrow(daily_rows) == 0) {
      daily_rows
    } else {
      daily_rows[as.character(daily_rows$fecha_objetivo) == f, , drop = FALSE]
    }
    if (is.null(sub_d)) {
      sub_d <- data.frame(
        nombre_abr = character(0), valor_crudo = numeric(0), valor = numeric(0),
        stringsAsFactors = FALSE
      )
    }
    chips <- list(
      t_min = pronostico_chip_valor(sub_d, "t_min"),
      t_max = pronostico_chip_valor(sub_d, "t_max"),
      pp = pronostico_chip_valor(sub_d, "pp"),
      u_max_diario = pronostico_chip_valor(sub_d, "u_max_diario")
    )
    hora_sub <- if (has_hora) {
      hora_rows[as.character(hora_rows$fecha_objetivo) == f, , drop = FALSE]
    } else {
      hora_rows
    }
    aligned <- if (!is.null(hora_sub) && nrow(hora_sub) > 0) {
      pronostico_alinear_horas(hora_sub, chips)
    } else {
      NULL
    }
    list(
      fecha = d$fecha,
      variables = d$variables,
      horas = aligned,
      horas_caption = isTRUE(pronostico_dia_tiene_crudo(sub_d)) && !is.null(aligned)
    )
  })
}

pronostico_normalize_horas <- function(h) {
  if (is.null(h) || (is.logical(h) && (length(h) == 0 || all(is.na(h))))) return(NULL)
  if (is.data.frame(h)) {
    h <- list(ts = h$ts, t = h$t, pp = h$pp, u = h$u, wc = h$wc, day = h$day, pop = h$pop)
  }
  if (!is.list(h) || is.null(h$ts)) return(NULL)
  ts <- as.character(unlist(h$ts, use.names = FALSE))
  t <- as.numeric(unlist(h$t, use.names = FALSE))
  if (length(ts) == 0L || length(t) != length(ts)) return(NULL)
  list(
    ts = ts,
    t = t,
    pp = as.numeric(unlist(h$pp, use.names = FALSE)),
    u = as.numeric(unlist(h$u, use.names = FALSE)),
    wc = as.integer(unlist(h$wc, use.names = FALSE)),
    day = pronostico_hora_pad_int(h$day, length(ts)),
    pop = pronostico_hora_pad_int(h$pop, length(ts))
  )
}

# jsonlite::fromJSON(simplifyVector=TRUE) convierte una lista de objetos
# iguales en data.frame (filas=días, cols=campos). [[i]] es la COLUMNA i,
# no el día i: 16 días vs 5 campos de horas → subscript out of bounds.
pronostico_row_cell <- function(col, i) {
  if (is.null(col)) return(NULL)
  i <- as.integer(i)[1]
  if (is.na(i) || i < 1L) return(NULL)
  if (is.data.frame(col)) {
    if (i > nrow(col)) return(NULL)
    as.list(col[i, , drop = FALSE])
  } else if (is.list(col) || is.vector(col)) {
    if (i > length(col)) return(NULL)
    col[[i]]
  } else {
    NULL
  }
}

#' Normaliza dias/variables tras cache Redis (fromJSON simplifyVector=TRUE).
pronostico_lite_day_list <- function(dias) {
  if (is.null(dias) || length(dias) == 0L) return(list())
  if (!is.data.frame(dias)) {
    return(lapply(dias, function(d) {
      if (!is.list(d)) return(d)
      d[["horas"]] <- pronostico_normalize_horas(d[["horas"]])
      d
    }))
  }
  lapply(seq_len(nrow(dias)), function(i) {
    vars <- pronostico_row_cell(dias$variables, i)
    if (is.data.frame(vars)) {
      vars <- lapply(seq_len(nrow(vars)), function(j) as.list(vars[j, , drop = FALSE]))
    }
    horas <- if ("horas" %in% names(dias)) {
      pronostico_normalize_horas(pronostico_row_cell(dias$horas, i))
    } else {
      NULL
    }
    caption <- if ("horas_caption" %in% names(dias)) {
      cap <- pronostico_row_cell(dias$horas_caption, i)
      if (is.list(cap) && length(cap) == 1L) cap <- cap[[1]]
      isTRUE(cap)
    } else {
      FALSE
    }
    list(
      fecha = as.character(dias$fecha[i]),
      variables = vars,
      horas = horas,
      horas_caption = caption
    )
  })
}

pronostico_rows_to_dias_lite <- function(rows) {
  fechas <- sort(unique(as.character(rows$fecha_objetivo)))
  lapply(fechas, function(f) {
    sub <- rows[as.character(rows$fecha_objetivo) == f, , drop = FALSE]
    vars <- lapply(seq_len(nrow(sub)), function(i) {
      r <- sub[i, ]
      list(
        id_variable = as.integer(r$id_variable),
        nombre_abr = r$nombre_abr,
        nombre = r$nombre,
        unidad = display_unidad(r$nombre_abr, r$unidad),
        valor = display_valor(as.numeric(r$valor), r$nombre_abr)
      )
    })
    list(fecha = f, variables = vars)
  })
}

#' Payload liviano (sin rankings) para carrusel embebido en reporte.
pronostico_build_lite_payload <- function(eid) {
  if (!pronostico_tabla_existe()) return(NULL)
  eid <- as.integer(eid)
  if (is.na(eid) || eid < 1L || !pronostico_estacion_visible(eid)) return(NULL)

  rows <- pronostico_fetch_forecast_rows(eid)
  if (is.null(rows) || nrow(rows) == 0) return(NULL)

  fetched_at <- max(rows$fetched_at, na.rm = TRUE)
  dias <- pronostico_rows_to_dias_lite(rows)
  dias <- pronostico_attach_horas(dias, pronostico_fetch_hora_rows(eid), rows)

  list(
    meta = pronostico_meta_base(fetched_at, length(dias), pronostico_historia_limitada(eid)),
    dias = dias
  )
}

pronostico_build_payload <- function(eid) {
  if (!pronostico_tabla_existe()) {
    return(list(error = "Pron\u00f3stico no disponible"))
  }
  eid <- as.integer(eid)
  if (is.na(eid) || eid < 1L) return(list(error = "id inv\u00e1lido"))
  if (!pronostico_estacion_visible(eid)) {
    return(list(error = "Pron\u00f3stico solo disponible para estaciones con calidad excelente o buena"))
  }

  pronostico_source_rank_helpers()
  pronostico_obs_diaria_disponible()
  historia_limitada <- pronostico_historia_limitada(eid)

  est <- db_query(
    "SELECT e.id, e.nombre, e.lat, e.lon, e.nom_com, e.id_fuente, ec.nivel
     FROM mirador e
     LEFT JOIN estacion_calidad ec ON ec.id_estacion = e.id
     WHERE e.id = $1",
    params = list(eid)
  )
  if (nrow(est) == 0) return(list(error = "Estaci\u00f3n no encontrada"))

  rows <- pronostico_fetch_forecast_rows(eid)
  if (nrow(rows) == 0) {
    return(list(error = "Sin pron\u00f3stico para esta estaci\u00f3n. Intenta m\u00e1s tarde."))
  }

  fetched_at <- max(rows$fetched_at, na.rm = TRUE)

  rank_lookup <- list()
  if (!isTRUE(historia_limitada)) {
    alerta <- pronostico_fetch_alerta_rankings(eid)
    use_sql_fallback <- pronostico_use_sql_fallback(fetched_at, alerta$sync_at)
    rank_lookup <- if (isTRUE(use_sql_fallback)) {
      pronostico_fetch_fallback_rankings_batch(eid)
    } else {
      alerta$lookup
    }
  }

  fechas <- sort(unique(as.character(rows$fecha_objetivo)))
  dias <- lapply(fechas, function(f) {
    sub <- rows[as.character(rows$fecha_objetivo) == f, , drop = FALSE]
    vars <- lapply(seq_len(nrow(sub)), function(i) {
      r <- sub[i, ]
      rankings <- if (isTRUE(historia_limitada)) {
        list()
      } else {
        pronostico_rankings_from_lookup(
          as.integer(r$id_variable), r$fecha_objetivo, r$nombre_abr, rank_lookup
        )
      }
      list(
        id_variable = as.integer(r$id_variable),
        nombre_abr = r$nombre_abr,
        nombre = r$nombre,
        unidad = display_unidad(r$nombre_abr, r$unidad),
        valor = display_valor(as.numeric(r$valor), r$nombre_abr),
        rankings = rankings
      )
    })
    list(fecha = f, variables = vars)
  })
  dias <- pronostico_attach_horas(dias, pronostico_fetch_hora_rows(eid), rows)
  view <- if (exists("estacion_nombre_view", mode = "function")) {
    estacion_nombre_view(est$nombre[1], est$nom_com[1], est$id_fuente[1])
  } else {
    NULL
  }

  list(
    station = list(
      eid = eid,
      nombre = if (!is.null(view) && nzchar(view$label %||% "")) view$label else est$nombre[1],
      fuente = if (!is.null(view)) view$fuente else "",
      lat = est$lat[1],
      lon = est$lon[1],
      nivel = est$nivel[1]
    ),
    meta = pronostico_meta_base(fetched_at, length(fechas), historia_limitada),
    dias = dias
  )
}

pronostico_favorita_usuario <- function(user_id) {
  df <- db_query(
    "SELECT eid FROM miradores_pinchados WHERE user_id = $1::uuid AND es_primario = TRUE LIMIT 1",
    params = list(user_id)
  )
  if (nrow(df) == 0) return(NULL)
  as.integer(df$eid[1])
}

pronostico_mapa_parse_ts <- function(raw) {
  s <- trimws(as.character(raw %||% ""))
  if (!nzchar(s)) return(as.POSIXct(NA, tz = "UTC"))
  s <- sub("\\.[0-9]+Z$", "Z", s)
  if (grepl("Z$", s)) {
    return(as.POSIXct(sub("Z$", "", s), tz = "UTC", format = "%Y-%m-%dT%H:%M:%S"))
  }
  tryCatch(as.POSIXct(s, tz = "UTC"), error = function(e) as.POSIXct(NA, tz = "UTC"))
}

pronostico_mapa_hour_utc <- function(ts) {
  ts <- as.POSIXct(ts, tz = "UTC")
  if (length(ts) != 1L || is.na(ts)) return(as.POSIXct(NA, tz = "UTC"))
  origin <- as.POSIXct("1970-01-01", tz = "UTC")
  origin + (as.numeric(ts) %/% 3600) * 3600
}

pronostico_mapa_iso_utc <- function(ts) {
  ts <- as.POSIXct(ts, tz = "UTC")
  if (length(ts) != 1L || is.na(ts)) return(NA_character_)
  format(ts, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}

pronostico_mapa_nombre <- function(n) {
  n <- as.character(n %||% "")
  n <- gsub("_(DMC|DGA|INIA)$", "", n, ignore.case = TRUE)
  n <- gsub("_", " ", n, fixed = TRUE)
  gsub("\\s+", " ", trimws(n))
}

pronostico_mapa_points_list <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(list())
  nms <- vapply(df$n, pronostico_mapa_nombre, character(1), USE.NAMES = FALSE)
  lapply(seq_len(nrow(df)), function(i) {
    t <- suppressWarnings(as.numeric(df$t[i]))
    pp <- suppressWarnings(as.numeric(df$pp[i]))
    u_ms <- if ("u_ms" %in% names(df)) suppressWarnings(as.numeric(df$u_ms[i])) else NA_real_
    u <- display_valor(u_ms, "u2_mean")
    list(
      id = as.integer(df$id[i]),
      lat = as.numeric(df$lat[i]),
      lon = as.numeric(df$lon[i]),
      n = nms[i],
      t = if (length(t) == 1L && is.finite(t)) t else NA_real_,
      pp = if (length(pp) == 1L && is.finite(pp)) pp else NA_real_,
      u = if (length(u) == 1L && is.finite(u)) u else NA_real_,
      r = as.integer(df$r[i])
    )
  })
}

pronostico_mapa_build <- function(t_raw) {
  if (!isTRUE(pronostico_hora_tabla_existe())) {
    return(list(t = NA_character_, matched = FALSE, fuente = "open-meteo", points = list()))
  }
  hour <- pronostico_mapa_hour_utc(pronostico_mapa_parse_ts(t_raw))
  if (is.na(hour)) return(NULL)
  iso <- pronostico_mapa_iso_utc(hour)
  df <- tryCatch(
    db_query(
      "SELECT h.id_estacion AS id,
              ROUND(e.lat::numeric, 5)::float8 AS lat,
              ROUND(e.lon::numeric, 5)::float8 AS lon,
              e.nombre AS n,
              ROUND(h.t_2m::numeric, 1)::float8 AS t,
              ROUND(pp3.pp_3h::numeric, 2)::float8 AS pp,
              h.u10::float8 AS u_ms,
              (CASE WHEN COALESCE(ec.nivel, '') = 'excelente' THEN 0 ELSE 1 END * 100000
               - COALESCE(ec.anios_span, 0)) AS r
       FROM carta_por_hora h
       JOIN mirador e ON e.id = h.id_estacion
       LEFT JOIN estacion_calidad ec ON ec.id_estacion = e.id
       LEFT JOIN LATERAL (
         SELECT SUM(w.pp) AS pp_3h
         FROM carta_por_hora w
         WHERE w.id_estacion = h.id_estacion
           AND w.ts > h.ts - INTERVAL '3 hours'
           AND w.ts <= h.ts
       ) pp3 ON TRUE
       WHERE h.ts = $1::timestamptz
         AND e.lat IS NOT NULL AND e.lon IS NOT NULL",
      params = list(hour)
    ),
    error = function(e) data.frame()
  )
  list(
    t = iso,
    matched = is.data.frame(df) && nrow(df) > 0L,
    fuente = "open-meteo",
    points = pronostico_mapa_points_list(df)
  )
}

local({
  if (!exists("display_valor", mode = "function")) return(invisible(NULL))
  hora <- data.frame(
    ts = as.POSIXct("2026-09-01 12:00:00", tz = "UTC") + c(0, 3600, 7200),
    t_2m = c(10, 20, 15),
    pp = c(0, 1, 1),
    u10 = c(1, 2, 3),
    weather_code = c(0L, 1L, 2L),
    stringsAsFactors = FALSE
  )
  aligned <- pronostico_alinear_horas(hora, list(t_min = 8, t_max = 28, pp = 4))
  t <- unlist(aligned$t)
  stopifnot(abs(max(t) - 28) < 1e-8)
  stopifnot(abs(min(t) - 8) < 1e-8)
  stopifnot(abs(sum(unlist(aligned$pp)) - 4) < 1e-8)
  stopifnot(abs(max(unlist(aligned$u)) - 10.8) < 1e-8)
  aligned_w <- pronostico_alinear_horas(hora, list(t_min = 8, t_max = 28, pp = 4, u_max_diario = 21.6))
  u_w <- unlist(aligned_w$u)
  stopifnot(abs(max(u_w) - 21.6) < 1e-6)
  stopifnot(abs(u_w[1] - 7.2) < 1e-6)
  aligned_0 <- pronostico_alinear_horas(hora, list(t_min = 8, t_max = 28, pp = 4, u_max_diario = 0))
  stopifnot(max(unlist(aligned_0$u)) < 1e-8)
  h <- pronostico_mapa_hour_utc(as.POSIXct("2026-09-18 03:17:42", tz = "UTC"))
  stopifnot(identical(pronostico_mapa_iso_utc(h), "2026-09-18T03:00:00Z"))
  stopifnot(identical(
    pronostico_mapa_iso_utc(pronostico_mapa_hour_utc(pronostico_mapa_parse_ts("2026-09-18T03:00:00Z"))),
    "2026-09-18T03:00:00Z"
  ))
  stopifnot(identical(
    pronostico_fetched_iso(as.POSIXct("2026-10-09 12:12:33", tz = "UTC")),
    "2026-10-09T12:12:33Z"
  ))
  stopifnot(identical(pronostico_mapa_nombre("Quinta_normal_santiago_DMC"), "Quinta normal santiago"))
  stopifnot(identical(pronostico_mapa_nombre("RIO CAQUENA EN VERTEDERO"), "RIO CAQUENA EN VERTEDERO"))
  hora2 <- hora
  hora2$is_day <- c(1L, 0L, 1L)
  hora2$pop <- c(10L, 40L, NA_integer_)
  aligned_p <- pronostico_alinear_horas(hora2, list(t_min = 8, t_max = 28, pp = 4))
  stopifnot(identical(as.integer(unlist(aligned_p$day)), c(1L, 0L, 1L)))
  pop_u <- suppressWarnings(as.integer(unlist(aligned_p$pop)))
  stopifnot(identical(pop_u[1:2], c(10L, 40L)))
  stopifnot(is.na(pop_u[3]))
  norm <- pronostico_normalize_horas(list(
    ts = c("a", "b"), t = c(1, 2), pp = c(0, 1), u = c(1, 1), wc = c(0L, 1L),
    day = c(1L, 0L), pop = c(5L, NA_integer_)
  ))
  stopifnot(identical(norm$day, c(1L, 0L)))
  stopifnot(is.na(norm$pop[2]))
  norm0 <- pronostico_normalize_horas(list(
    ts = c("a", "b"), t = c(1, 2), pp = c(0, 1), u = c(1, 1), wc = c(0L, 1L)
  ))
  stopifnot(length(norm0$day) == 2L && all(is.na(norm0$day)))
})
