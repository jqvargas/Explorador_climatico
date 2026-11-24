# Ranking pronostico via jornada_cerrada (batch SQL + lookup aviso_de_madrugada).

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

.PRON_OBS_DIARIA_OK <- NULL

PRONOSTICO_RANKABLE_ABRS <- c("pp", "t_max", "t_min", "u_max_diario")

pronostico_obs_diaria_disponible <- function() {
  if (!is.null(.PRON_OBS_DIARIA_OK)) return(.PRON_OBS_DIARIA_OK)
  .PRON_OBS_DIARIA_OK <<- tryCatch({
    reg <- db_query("SELECT to_regclass('public.jornada_cerrada') AS t")
    !is.null(reg) && nrow(reg) > 0 && !is.na(reg$t[1])
  }, error = function(e) FALSE)
  .PRON_OBS_DIARIA_OK
}

pronostico_var_rankable <- function(nombre_abr) {
  tolower(trimws(nombre_abr %||% "")) %in% PRONOSTICO_RANKABLE_ABRS
}

pronostico_rank_spec_abr <- function(nombre_abr) {
  abr <- tolower(trimws(nombre_abr %||% ""))
  if (abr %in% c("t_min")) {
    return(list(decreasing = FALSE, direccion = "min", widget = "minimos"))
  }
  list(decreasing = TRUE, direccion = "max", widget = "maximos")
}

pronostico_rank_entry <- function(nombre_abr, ranking, n_dias_historico) {
  spec <- pronostico_rank_spec_abr(nombre_abr)
  list(list(
    direccion = spec$direccion,
    ranking_estimado = as.integer(ranking),
    n_dias_historico = as.integer(n_dias_historico),
    widget = spec$widget
  ))
}

alerta_temprana_tabla_existe_pron <- function() {
  if (exists("alerta_temprana_tabla_existe", mode = "function")) {
    return(alerta_temprana_tabla_existe())
  }
  chk <- tryCatch(
    db_query("SELECT 1 FROM information_schema.tables WHERE table_name = 'aviso_de_madrugada' LIMIT 1"),
    error = function(e) NULL
  )
  !is.null(chk) && nrow(chk) > 0
}

pronostico_alerta_lookup_key <- function(vid, fecha) {
  paste0(as.integer(vid), "|", as.character(fecha))
}

# Fallback SQL solo si el pronostico es mas nuevo que la ultima corrida global de alertas.
pronostico_use_sql_fallback <- function(fetched_at, alerta_sync_at) {
  if (is.na(fetched_at)) return(TRUE)
  if (is.null(alerta_sync_at) || length(alerta_sync_at) == 0 || is.na(alerta_sync_at)) {
    return(TRUE)
  }
  sync <- tryCatch(as.POSIXct(alerta_sync_at, tz = "UTC"), error = function(e) NA)
  fet <- tryCatch(as.POSIXct(fetched_at, tz = "UTC"), error = function(e) NA)
  if (is.na(sync) || is.na(fet)) return(TRUE)
  isTRUE(fet > sync)
}

pronostico_alerta_sync_at <- function() {
  if (!alerta_temprana_tabla_existe_pron()) return(NA)
  gen <- tryCatch(
    db_query(
      "SELECT COALESCE(",
      " (SELECT MAX(generado_en) FROM aviso_de_madrugada WHERE fecha_objetivo >= CURRENT_DATE),",
      " (SELECT MAX(generado_en) FROM aviso_de_madrugada)",
      ") AS t"
    ),
    error = function(e) NULL
  )
  if (is.null(gen) || nrow(gen) == 0) return(NA)
  gen$t[1]
}

pronostico_fetch_alerta_rankings <- function(eid) {
  if (!alerta_temprana_tabla_existe_pron()) {
    return(list(rows = NULL, lookup = list(), sync_at = NA, generado_at = NA))
  }
  hist <- if (exists("alerta_temprana_historia_clause", mode = "function")) {
    alerta_temprana_historia_clause("a.id_estacion")
  } else {
    "TRUE"
  }
  val <- if (exists("alerta_temprana_alerta_valida_sql", mode = "function")) {
    alerta_temprana_alerta_valida_sql("a")
  } else {
    "TRUE"
  }
  df <- tryCatch(
    db_query(
      paste0(
        "SELECT id_variable, fecha_objetivo, direccion, ranking, n_dias_historico",
        " FROM aviso_de_madrugada a",
        " WHERE a.id_estacion = $1 AND a.fecha_objetivo >= CURRENT_DATE",
        " AND ", hist, " AND ", val
      ),
      params = list(as.integer(eid))
    ),
    error = function(e) NULL
  )
  sync_at <- pronostico_alerta_sync_at()
  lookup <- list()
  if (!is.null(df) && nrow(df) > 0) {
    for (i in seq_len(nrow(df))) {
      r <- df[i, ]
      key <- pronostico_alerta_lookup_key(r$id_variable, r$fecha_objetivo)
      widget <- if (identical(r$direccion, "min")) "minimos" else "maximos"
      lookup[[key]] <- list(list(
        direccion = r$direccion,
        ranking_estimado = as.integer(r$ranking),
        n_dias_historico = as.integer(r$n_dias_historico),
        widget = widget
      ))
    }
  }
  list(rows = df, lookup = lookup, sync_at = sync_at, generado_at = sync_at)
}

#' Rankings top-10 para toda la estacion en una sola query (ventana sin aviso_de_madrugada).
pronostico_fetch_fallback_rankings_batch <- function(eid) {
  if (!pronostico_obs_diaria_disponible()) return(list())
  eid <- as.integer(eid)
  if (is.na(eid) || eid < 1L) return(list())
  min_anios <- if (exists("alerta_temprana_min_anios", mode = "function")) {
    alerta_temprana_min_anios()
  } else {
    20L
  }
  min_dias <- if (exists("alerta_temprana_min_dias", mode = "function")) {
    alerta_temprana_min_dias()
  } else {
    365L
  }
  var_arr <- if (exists("pg_text_array_param", mode = "function")) {
    pg_text_array_param(PRONOSTICO_RANKABLE_ABRS)
  } else {
    paste0("{", paste(PRONOSTICO_RANKABLE_ABRS, collapse = ","), "}")
  }
  q <- paste0(
    "WITH forecast AS (",
    " SELECT p.id_variable, p.fecha_objetivo, p.valor, lower(v.nombre_abr) AS abr",
    " FROM carta_del_mirador p",
    " JOIN magnitud v ON v.id = p.id_variable",
    " WHERE p.id_estacion = $1 AND p.fecha_objetivo >= CURRENT_DATE",
    " AND lower(v.nombre_abr) = ANY($2::text[])",
    "), ranked AS (",
    " SELECT f.id_variable, f.fecha_objetivo, f.abr,",
    " CASE WHEN f.abr = 't_min'",
    "   THEN 1 + COUNT(*) FILTER (WHERE d.valor <= f.valor)",
    "   ELSE 1 + COUNT(*) FILTER (WHERE d.valor >= f.valor)",
    " END::int AS ranking,",
    " COUNT(d.valor)::int AS n_dias_historico",
    " FROM forecast f",
    " JOIN jornada_cerrada d",
    "   ON d.id_estacion = $1 AND d.id_variable = f.id_variable",
    " WHERE d.valor IS NOT NULL",
    " GROUP BY f.id_variable, f.fecha_objetivo, f.valor, f.abr",
    " HAVING COUNT(DISTINCT EXTRACT(YEAR FROM d.fecha)) >= ", min_anios,
    " AND COUNT(d.valor) >= ", min_dias,
    ")",
    " SELECT id_variable, fecha_objetivo, abr, ranking, n_dias_historico",
    " FROM ranked WHERE ranking <= 10"
  )
  df <- tryCatch(
    db_query(q, params = list(eid, var_arr)),
    error = function(e) NULL
  )
  lookup <- list()
  if (is.null(df) || nrow(df) == 0) return(lookup)
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    key <- pronostico_alerta_lookup_key(r$id_variable, r$fecha_objetivo)
    lookup[[key]] <- pronostico_rank_entry(r$abr, r$ranking, r$n_dias_historico)
  }
  lookup
}

pronostico_rankings_from_lookup <- function(vid, fecha, nombre_abr, lookup) {
  if (!pronostico_var_rankable(nombre_abr)) return(list())
  key <- pronostico_alerta_lookup_key(vid, fecha)
  hit <- lookup[[key]]
  if (is.null(hit)) list() else hit
}

# ponytail: compat legacy; el payload usa batch + lookup (no N+1).
pronostico_rankings_for_cell <- function(eid, vid, fecha, forecast_val, nombre_abr,
                                         alerta_lookup, use_sql_fallback, memo = NULL) {
  hit <- pronostico_rankings_from_lookup(vid, fecha, nombre_abr, alerta_lookup)
  if (length(hit) > 0L || !isTRUE(use_sql_fallback)) return(hit)
  pronostico_rank_from_diaria_sql(eid, vid, forecast_val, nombre_abr, memo = memo)
}

pronostico_rank_memo_key <- function(eid, vid, forecast_val = NULL) {
  base <- paste0(as.integer(eid), ":", as.integer(vid))
  if (is.null(forecast_val)) return(base)
  paste0(base, ":", round(as.numeric(forecast_val), 4))
}

pronostico_rank_from_diaria_sql <- function(eid, vid, forecast_val, nombre_abr,
                                            memo = NULL, allow_rank = TRUE) {
  if (!isTRUE(allow_rank) || !pronostico_var_rankable(nombre_abr) ||
      !pronostico_obs_diaria_disponible()) {
    return(list())
  }
  eid <- as.integer(eid)
  vid <- as.integer(vid)
  fv <- suppressWarnings(as.numeric(forecast_val))
  if (is.na(eid) || is.na(vid) || is.na(fv)) return(list())

  key <- pronostico_rank_memo_key(eid, vid, fv)
  if (!is.null(memo) && exists(key, envir = memo, inherits = FALSE)) {
    return(get(key, envir = memo, inherits = FALSE))
  }

  spec <- pronostico_rank_spec_abr(nombre_abr)
  cmp <- if (isTRUE(spec$decreasing)) ">=" else "<="
  min_anios <- if (exists("alerta_temprana_min_anios", mode = "function")) {
    alerta_temprana_min_anios()
  } else {
    20L
  }
  min_dias <- if (exists("alerta_temprana_min_dias", mode = "function")) {
    alerta_temprana_min_dias()
  } else {
    365L
  }
  q <- paste0(
    "SELECT COUNT(*)::int AS n_days,",
    " COUNT(DISTINCT EXTRACT(YEAR FROM fecha))::int AS anios_con_datos,",
    " 1 + COUNT(*) FILTER (WHERE valor ", cmp, " $3)::int AS rank",
    " FROM jornada_cerrada",
    " WHERE id_estacion = $1 AND id_variable = $2 AND valor IS NOT NULL"
  )
  df <- tryCatch(
    db_query(q, params = list(eid, vid, fv)),
    error = function(e) NULL
  )
  out <- list()
  if (!is.null(df) && nrow(df) > 0 && !is.na(df$n_days[1]) && df$n_days[1] > 0L) {
    anios_con_datos <- as.integer(df$anios_con_datos[1])
    historia_ok <- if (exists("alerta_par_rank_historia_ok", mode = "function")) {
      alerta_par_rank_historia_ok(df$n_days[1], anios_con_datos, min_anios, min_dias)
    } else {
      !is.na(anios_con_datos) && anios_con_datos >= min_anios &&
        !is.na(df$n_days[1]) && df$n_days[1] >= min_dias
    }
    if (isTRUE(historia_ok)) {
      rank <- as.integer(df$rank[1])
      if (!is.na(rank) && rank <= 10L) {
        out <- pronostico_rank_entry(nombre_abr, rank, df$n_days[1])
      }
    }
  }
  if (!is.null(memo)) assign(key, out, envir = memo)
  out
}

# ponytail: self-check rankable + sync_at fallback gate
if (identical(Sys.getenv("PRONOSTICO_RANK_SELF_CHECK", "1"), "1")) {
  stopifnot(
    pronostico_var_rankable("pp"),
    pronostico_var_rankable("t_max"),
    !pronostico_var_rankable("weather_code"),
    !pronostico_var_rankable("u2_mean"),
    !pronostico_var_rankable("u_10_kt")
  )
  t_sync <- as.POSIXct("2026-07-05 12:00:00", tz = "UTC")
  t_old <- as.POSIXct("2026-07-05 11:00:00", tz = "UTC")
  t_new <- as.POSIXct("2026-07-05 13:00:00", tz = "UTC")
  stopifnot(
    isFALSE(pronostico_use_sql_fallback(t_old, t_sync)),
    isTRUE(pronostico_use_sql_fallback(t_new, t_sync))
  )
}
