# API helpers alerta temprana (catalogo global pronostico top 10)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

ALERTA_TEMPRANA_MIN_ANIOS <- 10L
ALERTA_TEMPRANA_MIN_DIAS <- 365L
ALERTA_TEMPRANA_NIVEL_MIN <- "excelente"
ALERTA_MAPA_HORIZONTE_CORTO <- 2L
ALERTA_MAPA_HORIZONTE_LARGO <- 2L
ALERTA_DIGEST_RANK_MAX <- 3L
# Producto EMIT: solo ranks criticos (1-3). Ranks 4-10 quedan en catalogo/shadow.
ALERTA_EMIT_RANK_MAX <- 3L
# Pausar t_min/min en producto hasta calibrar (precision ~4%).
ALERTA_PAUSE_T_MIN_MIN <- TRUE

alerta_mapa_horizonte_corto <- function() ALERTA_MAPA_HORIZONTE_CORTO
alerta_mapa_horizonte_largo <- function() {
  h <- ALERTA_MAPA_HORIZONTE_LARGO
  if (is.na(h) || h < 1L) 2L else min(h, 2L)
}
alerta_digest_rank_max <- function() ALERTA_DIGEST_RANK_MAX
alerta_emit_rank_max <- function() {
  # Preferir status del loop si existe.
  v <- tryCatch({
    if (!exists("db_query", mode = "function")) return(ALERTA_EMIT_RANK_MAX)
    st <- db_query("SELECT rank_max_emit FROM punteria_en_ronda WHERE id = 1")
    if (nrow(st) && !is.na(st$rank_max_emit[1])) as.integer(st$rank_max_emit[1]) else ALERTA_EMIT_RANK_MAX
  }, error = function(e) ALERTA_EMIT_RANK_MAX)
  if (is.na(v) || v < 1L) ALERTA_EMIT_RANK_MAX else v
}
alerta_pause_t_min_min <- function() {
  tryCatch({
    if (!exists("db_query", mode = "function")) return(ALERTA_PAUSE_T_MIN_MIN)
    st <- db_query("SELECT pause_t_min_min FROM punteria_en_ronda WHERE id = 1")
    if (nrow(st) && !is.na(st$pause_t_min_min[1])) isTRUE(as.logical(st$pause_t_min_min[1])) else ALERTA_PAUSE_T_MIN_MIN
  }, error = function(e) ALERTA_PAUSE_T_MIN_MIN)
}
alerta_temprana_var_abrs <- function() {
  if (!exists("alertas_catalogo_abrs", mode = "function")) {
    stop("alertas_catalog.R no cargado")
  }
  alertas_catalogo_abrs()
}

alerta_temprana_min_anios <- function() ALERTA_TEMPRANA_MIN_ANIOS
alerta_temprana_min_dias <- function() ALERTA_TEMPRANA_MIN_DIAS
alerta_temprana_nivel_minimo <- function() ALERTA_TEMPRANA_NIVEL_MIN

alerta_temprana_calidad_sql <- function(alias = "cal") {
  paste0("COALESCE(", alias, ".nivel, 'limitada') = '", ALERTA_TEMPRANA_NIVEL_MIN, "'")
}
alertas_min_anios_historia <- alerta_temprana_min_anios

alerta_anios_span_fechas <- function(fechas) {
  if (is.null(fechas) || length(fechas) == 0L) return(0L)
  d <- suppressWarnings(tryCatch(as.Date(fechas), error = function(e) NULL))
  if (is.null(d)) return(0L)
  d <- d[!is.na(d)]
  if (length(d) == 0L) return(0L)
  as.integer(format(max(d), "%Y")) - as.integer(format(min(d), "%Y")) + 1L
}

alerta_anios_con_datos_fechas <- function(fechas) {
  if (is.null(fechas) || length(fechas) == 0L) return(0L)
  d <- suppressWarnings(tryCatch(as.Date(fechas), error = function(e) NULL))
  if (is.null(d)) return(0L)
  d <- d[!is.na(d)]
  if (length(d) == 0L) return(0L)
  length(unique(format(d, "%Y")))
}

alerta_par_historia_suficiente <- function(fechas, min_anios = NULL, min_dias = NULL) {
  min_a <- as.integer(min_anios %||% alerta_temprana_min_anios())
  min_d <- as.integer(min_dias %||% alerta_temprana_min_dias())
  if (is.null(fechas) || length(fechas) == 0L) return(FALSE)
  d <- suppressWarnings(tryCatch(as.Date(fechas), error = function(e) NULL))
  if (is.null(d)) return(FALSE)
  d <- d[!is.na(d)]
  if (length(d) == 0L) return(FALSE)
  length(unique(d)) >= min_d && length(unique(format(d, "%Y"))) >= min_a
}

alerta_par_rank_historia_ok <- function(n_days, anios_con_datos, min_anios = NULL, min_dias = NULL) {
  min_a <- as.integer(min_anios %||% alerta_temprana_min_anios())
  min_d <- as.integer(min_dias %||% alerta_temprana_min_dias())
  !is.na(n_days) && n_days >= min_d && !is.na(anios_con_datos) && anios_con_datos >= min_a
}

# Cierres nuevos del aviso de 4 dias a 1 dia. No apaga el aviso.
# Con 30 o mas, y precision de cierre bajo 0,15, hay que revisar la frase de la tasa.
extremos_anticipada_revisar_tasa <- function(cumplidos, no_cumplidos, minimo = 30L, piso = 0.15) {
  cu <- suppressWarnings(as.integer(cumplidos))
  no <- suppressWarnings(as.integer(no_cumplidos))
  if (length(cu) != 1L || is.na(cu) || cu < 0L) cu <- 0L
  if (length(no) != 1L || is.na(no) || no < 0L) no <- 0L
  n <- cu + no
  prec <- if (n > 0L) cu / n else NA_real_
  list(
    n = n,
    minimo = as.integer(minimo),
    precision = prec,
    muestra = n >= as.integer(minimo),
    revisar = n >= as.integer(minimo) && is.finite(prec) && prec < as.numeric(piso)
  )
}

alerta_temprana_alerta_valida_sql <- function(alias = "a", include_shadow = FALSE,
                                             lead_max = NULL) {
  # Producto EMIT (defaults): historia OK + capa emit + lead <=1 + skill + rank + pausa.
  # include_shadow / lead_max: solo lecturas de mapa (coropletico 72 h). El correo
  # y /alertas/mapa siguen llamando sin extras.
  base <- paste0("COALESCE(", alias, ".n_dias_historico, 0) >= ", alerta_temprana_min_dias())
  lm <- if (is.null(lead_max)) NULL else as.integer(lead_max)[1]
  if (!is.null(lm) && (is.na(lm) || lm < 0L)) lm <- NULL
  if (isTRUE(include_shadow)) {
    if (is.null(lm)) {
      lm <- if (exists("alerta_skill_shadow_lead_max", mode = "function")) {
        as.integer(alerta_skill_shadow_lead_max())
      } else {
        2L
      }
    }
    emit_fila <- paste0(
      "COALESCE(", alias, ".capa, 'emit') IN ('emit','shadow')",
      " AND (", alias, ".fecha_objetivo - CURRENT_DATE) BETWEEN 0 AND ", lm
    )
  } else if (!is.null(lm)) {
    emit_fila <- paste0(
      "COALESCE(", alias, ".capa, 'emit') = 'emit'",
      " AND (", alias, ".fecha_objetivo - CURRENT_DATE) BETWEEN 0 AND ", lm
    )
  } else if (exists("alerta_skill_emit_fila_sql", mode = "function")) {
    emit_fila <- alerta_skill_emit_fila_sql(alias)
  } else {
    emit_fila <- paste0("COALESCE(", alias, ".capa, 'emit') = 'emit'")
  }
  skill <- if (exists("alerta_skill_emit_estacion_sql", mode = "function")) {
    alerta_skill_emit_estacion_sql(paste0(alias, ".id_estacion"))
  } else {
    "TRUE"
  }
  rank_max <- if (exists("alerta_emit_rank_max", mode = "function")) {
    as.integer(alerta_emit_rank_max())
  } else {
    3L
  }
  rank_ok <- paste0("COALESCE(", alias, ".ranking, 99) <= ", rank_max)
  pause_tmin <- if (exists("alerta_pause_t_min_min", mode = "function") &&
                    isTRUE(alerta_pause_t_min_min())) {
    paste0(
      "NOT (EXISTS (SELECT 1 FROM magnitud _vt ",
      " WHERE _vt.id = ", alias, ".id_variable AND lower(_vt.nombre_abr) = 't_min') ",
      " AND ", alias, ".direccion = 'min')"
    )
  } else {
    "TRUE"
  }
  # Mapa: pp/max en lead >= 1 queda fuera. Lead 0 (mismo dia / tardia clasica)
  # tambien: apagada 2026-11-16 en mapa y popup. Correo/push: extremos_anticipada_sin_clasica.
  pp_lejos <- paste0(
    "NOT (EXISTS (SELECT 1 FROM magnitud _vpp WHERE _vpp.id = ", alias, ".id_variable ",
    "AND lower(_vpp.nombre_abr) = 'pp') AND ", alias, ".direccion = 'max' ",
    "AND (", alias, ".fecha_objetivo - CURRENT_DATE) >= 1)"
  )
  sin_lead0 <- paste0("(", alias, ".fecha_objetivo - CURRENT_DATE) >= 1")
  paste0("(", base, " AND ", emit_fila, " AND ", skill,
         " AND ", rank_ok, " AND ", pause_tmin, " AND ", pp_lejos,
         " AND ", sin_lead0, ")")
}

alerta_par_diaria_span_sql <- function(eid_sql = "od.id_estacion", var_sql = "od.id_variable",
                                      min_anios = NULL, min_dias = NULL) {
  min_a <- as.integer(min_anios %||% alerta_temprana_min_anios())
  min_d <- as.integer(min_dias %||% alerta_temprana_min_dias())
  paste0(
    "(SELECT COUNT(DISTINCT EXTRACT(YEAR FROM od.fecha))::int FROM jornada_cerrada od",
    " WHERE od.id_estacion = ", eid_sql, " AND od.id_variable = ", var_sql,
    " AND od.valor IS NOT NULL) >= ", min_a,
    " AND (SELECT COUNT(DISTINCT od.fecha)::int FROM jornada_cerrada od",
    " WHERE od.id_estacion = ", eid_sql, " AND od.id_variable = ", var_sql,
    " AND od.valor IS NOT NULL) >= ", min_d
  )
}

alerta_temprana_historia_clause <- function(eid_sql = "e.id") {
  if (!exists("mv_estacion_calidad_nombre", mode = "function")) return("FALSE")
  mv <- mv_estacion_calidad_nombre()
  if (is.null(mv)) return("FALSE")
  paste0(
    "EXISTS (SELECT 1 FROM ", mv, " cal WHERE cal.id_estacion = ", eid_sql,
    " AND ", alerta_temprana_calidad_sql("cal"), ")"
  )
}

alerta_temprana_estacion_elegible <- function(eid) {
  eid_i <- as.integer(eid)
  if (is.na(eid_i) || eid_i <= 0L) return(FALSE)
  if (!exists("mv_estacion_calidad_nombre", mode = "function")) return(FALSE)
  mv <- mv_estacion_calidad_nombre()
  if (is.null(mv)) return(FALSE)
  q <- paste0(
    "SELECT 1 FROM ", mv, " cal WHERE cal.id_estacion = $1",
    " AND ", alerta_temprana_calidad_sql("cal"), " LIMIT 1"
  )
  df <- tryCatch(
    db_query(q, params = list(eid_i)),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(FALSE)

  # Skill allowlist: solo si hay masa critica de elegibles.
  n_eleg <- tryCatch(
    as.integer(db_query("SELECT COUNT(*)::int AS n FROM oficio_del_mirador WHERE elegible = TRUE")$n[1]),
    error = function(e) 0L
  )
  min_e <- if (exists("alerta_skill_min_elegibles_enforce", mode = "function")) {
    alerta_skill_min_elegibles_enforce()
  } else {
    30L
  }
  if (is.na(n_eleg) || n_eleg < min_e) return(TRUE)
  sk <- tryCatch(
    db_query(
      "SELECT elegible FROM oficio_del_mirador WHERE id_estacion = $1 LIMIT 1",
      params = list(eid_i)
    ),
    error = function(e) NULL
  )
  if (is.null(sk) || nrow(sk) == 0) return(FALSE)
  isTRUE(as.logical(sk$elegible[1]))
}

alerta_temprana_severidad <- function(ranking) {
  r <- suppressWarnings(as.integer(ranking))
  if (is.na(r) || r < 1L) return(NA_character_)
  if (r <= 3L) "critica" else "aviso_de_madrugada"
}

alerta_scalar_chr <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L) return(default)
  as.character(x[[1L]])
}

alerta_scalar_num <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L) return(default)
  suppressWarnings(as.numeric(x[[1L]]))
}

alerta_scalar_int <- function(x, default = NA_integer_) {
  if (is.null(x) || length(x) == 0L) return(default)
  suppressWarnings(as.integer(x[[1L]]))
}

alerta_mapa_join_sql <- function(alias = "am", horizonte_dias = NULL, col_prefix = "alerta") {
  if (!alerta_temprana_tabla_existe()) return(list(join = "", select = ""))
  horizonte_clause <- ""
  if (!is.null(horizonte_dias)) {
    h <- as.integer(horizonte_dias)
    if (!is.na(h) && h >= 1L) {
      horizonte_clause <- paste0(" AND a.fecha_objetivo <= CURRENT_DATE + ", h - 1L)
    }
  }
  pfx <- col_prefix
  list(
    join = paste0(
      " LEFT JOIN LATERAL (",
      "SELECT v.nombre_abr, a.ranking, a.fecha_objetivo, a.valor_pronostico,",
      " COALESCE(v.unidad, '') AS unidad, a.direccion, a.confianza",
      " FROM aviso_de_madrugada a",
      " JOIN magnitud v ON v.id = a.id_variable",
      " WHERE a.id_estacion = e.id AND a.fecha_objetivo >= CURRENT_DATE",
      horizonte_clause,
      " AND ", alerta_temprana_historia_clause("e.id"),
      " AND ", alerta_temprana_alerta_valida_sql("a"),
      " ORDER BY a.ranking ASC, a.fecha_objetivo ASC LIMIT 1",
      ") ", alias, " ON TRUE"
    ),
    select = paste0(
      ", ", alias, ".nombre_abr AS ", pfx, "_variable",
      ", ", alias, ".ranking AS ", pfx, "_ranking",
      ", ", alias, ".fecha_objetivo AS ", pfx, "_fecha_objetivo",
      ", ", alias, ".valor_pronostico AS ", pfx, "_valor_pronostico",
      ", ", alias, ".unidad AS ", pfx, "_unidad",
      ", ", alias, ".direccion AS ", pfx, "_direccion",
      ", ", alias, ".confianza AS ", pfx, "_confianza"
    )
  )
}

alerta_mapa_conteos_join_sql <- function(alias = "ac", horizonte_dias = NULL, col_prefix = "alerta_cnt") {
  if (!alerta_temprana_tabla_existe()) return(list(join = "", select = ""))
  horizonte_clause <- ""
  if (!is.null(horizonte_dias)) {
    h <- as.integer(horizonte_dias)
    if (!is.na(h) && h >= 1L) {
      horizonte_clause <- paste0(" AND a.fecha_objetivo <= CURRENT_DATE + ", h - 1L)
    }
  }
  pfx <- col_prefix
  list(
    join = paste0(
      " LEFT JOIN LATERAL (",
      "SELECT COUNT(*) FILTER (WHERE v.nombre_abr = 'pp')::int AS cnt_pp,",
      " COUNT(*) FILTER (WHERE v.nombre_abr IN ('t_max', 't_min') AND a.direccion = 'min')::int AS cnt_cold,",
      " COUNT(*) FILTER (WHERE v.nombre_abr IN ('t_max', 't_min') AND a.direccion = 'max')::int AS cnt_heat,",
      " COUNT(*) FILTER (WHERE v.nombre_abr IN ('u_max_diario', 'u2_mean', 'u_10_kt'))::int AS cnt_wind",
      " FROM aviso_de_madrugada a",
      " JOIN magnitud v ON v.id = a.id_variable",
      " WHERE a.id_estacion = e.id AND a.fecha_objetivo >= CURRENT_DATE",
      horizonte_clause,
      " AND ", alerta_temprana_historia_clause("e.id"),
      " AND ", alerta_temprana_alerta_valida_sql("a"),
      ") ", alias, " ON TRUE"
    ),
    select = paste0(
      ", COALESCE(", alias, ".cnt_pp, 0) AS ", pfx, "_pp",
      ", COALESCE(", alias, ".cnt_cold, 0) AS ", pfx, "_cold",
      ", COALESCE(", alias, ".cnt_heat, 0) AS ", pfx, "_heat",
      ", COALESCE(", alias, ".cnt_wind, 0) AS ", pfx, "_wind"
    )
  )
}

alerta_mapa_dual_join_sql <- function() {
  al5 <- alerta_mapa_join_sql("am5", alerta_mapa_horizonte_corto(), "alerta")
  al16 <- alerta_mapa_join_sql("am16", alerta_mapa_horizonte_largo(), "alerta_ext")
  cnt5 <- alerta_mapa_conteos_join_sql("ac5", alerta_mapa_horizonte_corto(), "alerta_cnt")
  cnt16 <- alerta_mapa_conteos_join_sql("ac16", alerta_mapa_horizonte_largo(), "alerta_ext_cnt")
  list(
    join = paste0(al5$join, al16$join, cnt5$join, cnt16$join),
    select = paste0(al5$select, al16$select, cnt5$select, cnt16$select)
  )
}

alerta_mapa_conteos_from_row <- function(row, prefix = "alerta_cnt") {
  pfx <- prefix
  list(
    pp = max(0L, alerta_scalar_int(row[[paste0(pfx, "_pp")]], 0L)),
    cold = max(0L, alerta_scalar_int(row[[paste0(pfx, "_cold")]], 0L)),
    heat = max(0L, alerta_scalar_int(row[[paste0(pfx, "_heat")]], 0L)),
    wind = max(0L, alerta_scalar_int(row[[paste0(pfx, "_wind")]], 0L))
  )
}

alerta_mapa_row_to_obj <- function(row, prefix = "alerta") {
  pfx <- prefix
  var <- alerta_scalar_chr(row[[paste0(pfx, "_variable")]])
  if (!nzchar(var)) return(NULL)
  rank <- alerta_scalar_int(row[[paste0(pfx, "_ranking")]])
  if (is.na(rank) || rank < 1L) return(NULL)
  val <- alerta_scalar_num(row[[paste0(pfx, "_valor_pronostico")]])
  uni <- alerta_scalar_chr(row[[paste0(pfx, "_unidad")]])
  list(
    variable = var,
    severidad = alerta_temprana_severidad(rank),
    ranking = rank,
    fecha_objetivo = alerta_scalar_chr(row[[paste0(pfx, "_fecha_objetivo")]]),
    valor_pronostico = display_valor(val, var),
    unidad = display_unidad(var, uni),
    direccion = alerta_scalar_chr(row[[paste0(pfx, "_direccion")]]),
    confianza = alerta_scalar_chr(row[[paste0(pfx, "_confianza")]], "alta")
  )
}

alerta_mapa_df_attach <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  cols5 <- c("alerta_variable", "alerta_ranking", "alerta_fecha_objetivo",
             "alerta_valor_pronostico", "alerta_unidad", "alerta_direccion", "alerta_confianza")
  cols16 <- c("alerta_ext_variable", "alerta_ext_ranking", "alerta_ext_fecha_objetivo",
              "alerta_ext_valor_pronostico", "alerta_ext_unidad", "alerta_ext_direccion", "alerta_ext_confianza")
  cols_cnt5 <- c("alerta_cnt_pp", "alerta_cnt_cold", "alerta_cnt_heat", "alerta_cnt_wind")
  cols_cnt16 <- c("alerta_ext_cnt_pp", "alerta_ext_cnt_cold", "alerta_ext_cnt_heat", "alerta_ext_cnt_wind")
  if (!all(cols5 %in% names(df))) {
    df$alerta <- vector("list", nrow(df))
    df$alerta_extendida <- vector("list", nrow(df))
    df$alerta_conteos <- replicate(nrow(df), list(pp = 0L, cold = 0L, heat = 0L, wind = 0L), simplify = FALSE)
    df$alerta_conteos_extendida <- df$alerta_conteos
    return(df)
  }
  df$alerta <- lapply(seq_len(nrow(df)), function(i) {
    alerta_mapa_row_to_obj(df[i, , drop = FALSE], "alerta")
  })
  df$alerta_extendida <- if (all(cols16 %in% names(df))) {
    lapply(seq_len(nrow(df)), function(i) {
      alerta_mapa_row_to_obj(df[i, , drop = FALSE], "alerta_ext")
    })
  } else {
    df$alerta
  }
  df$alerta_conteos <- if (all(cols_cnt5 %in% names(df))) {
    lapply(seq_len(nrow(df)), function(i) alerta_mapa_conteos_from_row(df[i, , drop = FALSE], "alerta_cnt"))
  } else {
    replicate(nrow(df), list(pp = 0L, cold = 0L, heat = 0L, wind = 0L), simplify = FALSE)
  }
  df$alerta_conteos_extendida <- if (all(cols_cnt16 %in% names(df))) {
    lapply(seq_len(nrow(df)), function(i) alerta_mapa_conteos_from_row(df[i, , drop = FALSE], "alerta_ext_cnt"))
  } else {
    df$alerta_conteos
  }
  drop <- intersect(c(cols5, cols16, cols_cnt5, cols_cnt16), names(df))
  if (length(drop) > 0L) df <- df[, setdiff(names(df), drop), drop = FALSE]
  df
}

alerta_mapa_df_to_rows <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(list())
  lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    al <- df$alerta[[i]]
    al_ext <- df$alerta_extendida[[i]]
    cnt <- df$alerta_conteos[[i]]
    cnt_ext <- df$alerta_conteos_extendida[[i]]
    row$alerta <- if (is.null(al)) NULL else al
    row$alerta_extendida <- if (is.null(al_ext)) NULL else al_ext
    row$alerta_conteos <- if (is.null(cnt)) list(pp = 0L, cold = 0L, heat = 0L, wind = 0L) else cnt
    row$alerta_conteos_extendida <- if (is.null(cnt_ext)) row$alerta_conteos else cnt_ext
    row
  })
}

alerta_temprana_row_to_item <- function(row) {
  rank <- alerta_scalar_int(row$ranking)
  abr <- alerta_scalar_chr(row$nombre_abr)
  val <- display_valor(alerta_scalar_num(row$valor_pronostico), abr)
  uni <- display_unidad(abr, alerta_scalar_chr(row$unidad))
  list(
    variable = abr,
    variable_nombre = alerta_scalar_chr(row$variable_nombre),
    severidad = alerta_temprana_severidad(rank),
    ranking = rank,
    fecha_objetivo = alerta_scalar_chr(row$fecha_objetivo),
    valor_pronostico = val,
    unidad = uni,
    direccion = alerta_scalar_chr(row$direccion),
    confianza = alerta_scalar_chr(row$confianza, "alta"),
    mensaje = alerta_temprana_fmt_mensaje(
      alerta_scalar_chr(row$estacion_nombre),
      alerta_scalar_chr(row$variable_nombre),
      alerta_scalar_chr(row$fecha_objetivo),
      val,
      uni,
      rank,
      abr,
      alerta_scalar_chr(row$direccion)
    )
  )
}

listar_alertas_temprana_estacion <- function(eid) {
  if (!alerta_temprana_tabla_existe()) return(list(alertas = list(), total = 0L))
  eid_i <- as.integer(eid)
  if (is.na(eid_i) || eid_i <= 0L) return(list(alertas = list(), total = 0L))
  if (!alerta_temprana_estacion_elegible(eid_i)) {
    return(list(alertas = list(), total = 0L))
  }
  hist <- alerta_temprana_historia_clause("a.id_estacion")
  df <- tryCatch(
    db_query(
      paste0(
        "SELECT e.nombre_publico AS estacion_nombre, v.nombre AS variable_nombre, v.nombre_abr,",
        " COALESCE(v.unidad, '') AS unidad, a.fecha_objetivo, a.direccion,",
        " a.ranking, a.valor_pronostico, a.confianza",
        " FROM aviso_de_madrugada a",
        " JOIN mirador e ON e.id = a.id_estacion",
        " JOIN magnitud v ON v.id = a.id_variable",
        " WHERE a.id_estacion = $1 AND a.fecha_objetivo >= CURRENT_DATE",
        " AND ", hist,
        " AND ", alerta_temprana_alerta_valida_sql("a"),
        " ORDER BY a.ranking ASC, a.fecha_objetivo ASC"
      ),
      params = list(eid_i)
    ),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0L) return(list(alertas = list(), total = 0L))
  alertas <- lapply(seq_len(nrow(df)), function(i) alerta_temprana_row_to_item(df[i, ]))
  list(alertas = alertas, total = as.integer(length(alertas)))
}

alerta_temprana_tabla_existe <- function() {
  chk <- tryCatch(
    db_query("SELECT 1 FROM information_schema.tables WHERE table_name = 'aviso_de_madrugada' LIMIT 1"),
    error = function(e) NULL
  )
  !is.null(chk) && nrow(chk) > 0
}

get_alerta_temprana_activa <- function(user_id) {
  if (!alerta_temprana_tabla_existe()) return(FALSE)
  chk <- tryCatch(
    db_query(
      "SELECT activa FROM pedido_temprano WHERE user_id = $1::uuid",
      params = list(user_id)
    ),
    error = function(e) NULL
  )
  if (is.null(chk) || nrow(chk) == 0) return(FALSE)
  isTRUE(chk$activa[1])
}

get_alerta_tardia_activa <- function(user_id) {
  if (!alertas_preferencias_tabla_existe()) return(TRUE)
  df <- tryCatch(
    db_query(
      "SELECT alerta_tardia FROM gusto_de_aviso WHERE user_id = $1::uuid",
      params = list(user_id)
    ),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(TRUE)
  if (!"alerta_tardia" %in% names(df)) return(TRUE)
  isTRUE(df$alerta_tardia[1])
}

usuario_eids_alcance_alertas <- function(user_id, tier = NULL) {
  eids <- integer(0)
  if (is.null(tier)) {
    u_row <- db_query("SELECT tier FROM tripulacion WHERE id = $1::uuid", params = list(user_id))
    tier <- if (nrow(u_row) > 0) u_row$tier[1] else "gratuito"
  }
  allowed <- scopes_permitidos_tier(tier)
  if (alertas_regla_tabla_existe()) {
    reglas <- db_query(
      "SELECT scope_type, scope_value FROM norma_propia WHERE user_id = $1::uuid AND activa = TRUE",
      params = list(user_id)
    )
    if (nrow(reglas) > 0) {
      for (i in seq_len(nrow(reglas))) {
        row <- reglas[i, ]
        if (!row$scope_type %in% allowed) next
        sv <- tryCatch(jsonlite::fromJSON(row$scope_value %||% "{}", simplifyVector = FALSE), error = function(e) list())
        got <- resolver_scope_eids(user_id, row$scope_type, sv)
        if (length(got) > 0) eids <- c(eids, got)
      }
    }
  } else if (alertas_tabla_existe()) {
    subs <- db_query(
      "SELECT scope_type FROM pedido_de_aviso WHERE user_id = $1::uuid AND activa = TRUE",
      params = list(user_id)
    )
    for (i in seq_len(nrow(subs))) {
      if (!subs$scope_type[i] %in% allowed) next
      got <- resolver_scope_eids(user_id, subs$scope_type[i], list())
      if (length(got) > 0) eids <- c(eids, got)
    }
  }
  unique(eids[!is.na(eids) & eids > 0L])
}

alerta_temprana_fmt_fecha <- function(fecha) {
  d <- suppressWarnings(as.Date(fecha))
  if (is.na(d)) return(as.character(fecha %||% ""))
  if (exists("reporte_fmt_fecha_larga", mode = "function")) {
    return(reporte_fmt_fecha_larga(d))
  }
  format(d, "%Y-%m-%d")
}

alerta_temprana_fmt_mensaje <- function(estacion, variable, fecha, valor, unidad, ranking, nombre_abr = "", direccion = "max") {
  if (exists("alerta_mensaje_linea", mode = "function")) {
    return(alerta_mensaje_linea(estacion, fecha, variable, nombre_abr, valor, unidad, ranking, direccion, "temprana"))
  }
  valor_txt <- if (exists("reporte_fmt_valor", mode = "function")) reporte_fmt_valor(valor) else as.character(valor)
  fecha_txt <- alerta_temprana_fmt_fecha(fecha)
  sprintf(
    "Estaci\u00f3n %s, %s \u00b7 Alerta %s pronosticada de %s %s, de suceder entrar\u00eda al ranking %d hist\u00f3rico",
    estacion, fecha_txt, variable, valor_txt, unidad %||% "", as.integer(ranking)
  )
}

listar_alertas_temprana_usuario <- function(user_id, eid = NULL, variable_abr = NULL, sort = "ranking", top3 = FALSE, limit = 100L) {
  if (!alerta_temprana_tabla_existe()) return(list(alertas = list(), total = 0L))
  eids <- usuario_eids_alcance_alertas(user_id)
  if (length(eids) == 0) return(list(alertas = list(), total = 0L))
  if (!is.null(eid) && nzchar(as.character(eid))) {
    eid_i <- as.integer(eid)
    if (!eid_i %in% eids) return(list(alertas = list(), total = 0L))
    eids <- eid_i
  }
  prefs <- if (exists("get_preferencias_usuario", mode = "function")) {
    get_preferencias_usuario(user_id, NULL)
  } else {
    list(variable_ids = NULL, alerta_max = TRUE, alerta_min = TRUE)
  }
  limit <- min(max(as.integer(limit %||% 100L), 1L), 200L)
  order_sql <- if (identical(sort, "fecha")) {
    "ORDER BY a.fecha_objetivo ASC, a.ranking ASC"
  } else {
    "ORDER BY a.ranking ASC, a.fecha_objetivo ASC"
  }
  top_clause <- if (isTRUE(parse_bool(top3))) " AND a.ranking <= 3" else ""
  var_clause <- ""
  dir_clause <- ""
  params <- list(pg_int_array_param(eids))
  param_i <- 2L
  if (!is.null(variable_abr) && nzchar(trimws(as.character(variable_abr)))) {
    var_clause <- paste0(" AND v.nombre_abr = $", param_i)
    params <- c(params, trimws(as.character(variable_abr)))
    param_i <- param_i + 1L
  } else if (!is.null(prefs$variable_ids) && length(prefs$variable_ids) > 0) {
    var_clause <- paste0(" AND a.id_variable = ANY($", param_i, "::int[])")
    params <- c(params, pg_int_array_param(as.integer(prefs$variable_ids)))
    param_i <- param_i + 1L
  }
  dir_parts <- character(0)
  if (!isTRUE(prefs$alerta_max)) dir_parts <- c(dir_parts, "a.direccion <> 'max'")
  if (!isTRUE(prefs$alerta_min)) dir_parts <- c(dir_parts, "a.direccion <> 'min'")
  if (length(dir_parts) > 0L) dir_clause <- paste0(" AND ", paste(dir_parts, collapse = " AND "))
  hist <- alerta_temprana_historia_clause("a.id_estacion")
  q <- paste0(
    "SELECT a.id_estacion, e.nombre_publico AS estacion_nombre, e.nom_reg, e.nom_com, e.cod_reg,
            COALESCE(e.macrozona, '') AS macrozona, a.id_variable, v.nombre AS variable_nombre,
            v.nombre_abr, COALESCE(v.unidad, '') AS unidad, a.fecha_objetivo, a.direccion,
            a.ranking, a.valor_pronostico, a.n_dias_historico, a.confianza, a.generado_en
     FROM aviso_de_madrugada a
     JOIN mirador e ON e.id = a.id_estacion
     JOIN magnitud v ON v.id = a.id_variable
     WHERE a.id_estacion = ANY($1::int[])
       AND a.fecha_objetivo >= CURRENT_DATE
       AND ", hist,
    " AND ", alerta_temprana_alerta_valida_sql("a"),
    top_clause, var_clause, dir_clause, " ", order_sql, " LIMIT ", limit
  )
  df <- db_query(q, params = params)
  if (nrow(df) == 0) {
    return(list(
      alertas = list(), total = 0L, vacio = TRUE,
      resumen = list(total = 0L, critica = 0L, destacada = 0L,
                     tardia = 0L, temprana = 0L, tipo_resumen = "0 confirmadas \u00b7 0 previstas"),
      agrupaciones = list(zona = list(), variable = list()),
      eventos = list()
    ))
  }
  eventos <- lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    abr <- row$nombre_abr %||% ""
    list(
      eid = as.integer(row$id_estacion),
      variable_id = as.integer(row$id_variable),
      fecha = as.character(row$fecha_objetivo),
      direccion = row$direccion,
      ranking = as.integer(row$ranking),
      valor = as.numeric(row$valor_pronostico),
      unidad = row$unidad %||% "",
      nombre_abr = abr,
      estacion_nombre = row$estacion_nombre %||% "",
      variable_nombre = row$variable_nombre %||% "",
      nom_reg = row$nom_reg %||% "",
      nom_com = row$nom_com %||% "",
      macrozona = row$macrozona %||% "",
      tipo = "temprana",
      confianza = row$confianza %||% "alta",
      generado_en = as.character(row$generado_en)
    )
  })
  app_url <- Sys.getenv("APP_PUBLIC_URL", "https://app.rodaja.cl")
  enriched <- if (exists("alerta_digest_eventos_enriquecer", mode = "function")) {
    alerta_digest_eventos_enriquecer(eventos, app_url)
  } else {
    eventos
  }
  resumen <- if (exists("alerta_digest_resumen", mode = "function")) {
    alerta_digest_resumen(eventos)
  } else {
    list(total = length(eventos), critica = 0L, destacada = length(eventos),
         tardia = 0L, temprana = length(eventos),
         tipo_resumen = paste0("0 confirmadas \u00b7 ", length(eventos), " previstas"))
  }
  agrupaciones <- if (exists("alerta_digest_agrupaciones", mode = "function")) {
    alerta_digest_agrupaciones(enriched)
  } else {
    list(zona = list(), variable = list())
  }
  list(
    total = length(enriched),
    vacio = FALSE,
    resumen = resumen,
    agrupaciones = agrupaciones
  )
}
