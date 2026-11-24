# Historial paginado por vista. Enriquece solo la pagina (o el CSV), no N+1.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
  }
}

alerta_resumen_hoy <- function() {
  df <- db_query("SELECT (NOW() AT TIME ZONE 'America/Santiago')::date AS hoy")
  as.Date(df$hoy[[1]])
}

.ar_schema <- new.env(parent = emptyenv())

alerta_resumen_schema <- function() {
  if (isTRUE(.ar_schema$ok)) return(.ar_schema)
  cols <- db_query(
    "SELECT column_name FROM information_schema.columns WHERE table_name = 'campanada'"
  )
  tabs <- db_query(
    "SELECT table_name FROM information_schema.tables
     WHERE table_name IN ('campanada_temprana', 'foto_del_aviso', 'jornada_cerrada')"
  )
  .ar_schema$cols <- if (nrow(cols)) as.character(cols$column_name) else character(0)
  .ar_schema$tabs <- if (nrow(tabs)) as.character(tabs$table_name) else character(0)
  .ar_schema$ok <- TRUE
  .ar_schema
}

alerta_resumen_tiene_col <- function(col) {
  col %in% alerta_resumen_schema()$cols
}

alerta_resumen_tiene_tabla <- function(nombre) {
  nombre %in% alerta_resumen_schema()$tabs
}

alerta_resumen_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || is.na(x)) NULL else x
}

alerta_resumen_fmt_num <- function(x, unidad = "") {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || is.na(x)) return("\u2014")
  dec <- if (abs(x * 10 - round(x * 10)) < 1e-6) 1L else 2L
  s <- formatC(x, format = "f", digits = dec, decimal.mark = ",")
  if (nzchar(unidad %||% "")) paste0(s, unidad) else s
}

alerta_resumen_fmt_diff <- function(x, unidad = "") {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || is.na(x)) return("\u2014")
  s <- alerta_resumen_fmt_num(abs(x), "")
  if (x < 0) s <- paste0("\u2212", s)
  else if (x > 0) s <- paste0("+", s)
  if (nzchar(unidad %||% "")) paste0(s, unidad) else s
}

alerta_resumen_fmt_fecha <- function(fecha) {
  d <- tryCatch(as.Date(fecha), error = function(e) as.Date(NA))
  if (length(d) != 1L || is.na(d)) return("")
  meses <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
  paste(as.integer(format(d, "%d")), meses[[as.integer(format(d, "%m"))]], format(d, "%Y"))
}

alerta_resumen_where_tipo <- function(tipo) {
  if (!nzchar(tipo %||% "")) return("")
  if (identical(tipo, "temprana")) return(" AND e.tipo = 'temprana'")
  " AND e.tipo = 'tardia'"
}

# Enrich de un conjunto ya acotado (pagina o ventana de metricas).
alerta_resumen_enriquecer_sql <- function(page_sql, hoy_param = "$4") {
  fue <- paste0(
    "EXISTS (SELECT 1 FROM foto_del_aviso s",
    " WHERE s.id_estacion = page.eid AND s.id_variable = page.variable_id",
    " AND s.fecha_objetivo = page.fecha AND s.direccion = page.direccion)"
  )
  if (alerta_resumen_tiene_tabla("campanada_temprana")) {
    fue <- paste0(
      fue,
      " OR EXISTS (SELECT 1 FROM campanada_temprana pr",
      " WHERE pr.eid = page.eid AND pr.variable_id = page.variable_id",
      " AND pr.fecha_objetivo = page.fecha AND pr.direccion = page.direccion)"
    )
  }
  fue <- paste0(
    fue,
    " OR EXISTS (SELECT 1 FROM campanada p",
    " WHERE p.eid = page.eid AND p.variable_id = page.variable_id",
    " AND p.fecha = page.fecha AND p.direccion = page.direccion",
    " AND p.tipo = 'temprana' AND p.id <> page.id)"
  )
  vp <- if (alerta_resumen_tiene_col("valor_pronostico")) {
    "page.valor_pronostico"
  } else {
    "NULL::float8"
  }
  paste0(
    "SELECT page.id, page.eid, page.variable_id, page.fecha, page.direccion,",
    " page.ranking, page.valor, page.tipo, ", vp, " AS valor_pronostico,",
    " est.nombre AS estacion_nombre,",
    " COALESCE(est.nombre_publico, '') AS nombre_publico,",
    " v.nombre AS variable_nombre, COALESCE(v.nombre_abr, '') AS nombre_abr,",
    " COALESCE(v.unidad, '') AS unidad,",
    " od.valor AS valor_observado, NULL::int AS n_mas,",
    " CASE WHEN page.tipo = 'tardia' AND (", fue, ") THEN TRUE ELSE FALSE END AS fue_prevista",
    " FROM (", page_sql, ") page",
    " LEFT JOIN mirador est ON est.id = page.eid",
    " LEFT JOIN magnitud v ON v.id = page.variable_id",
    " LEFT JOIN LATERAL (",
    "   SELECT d.valor FROM jornada_cerrada d",
    "   WHERE d.id_estacion = page.eid AND d.id_variable = page.variable_id",
    "     AND d.fecha = page.fecha AND d.valor IS NOT NULL",
    "   LIMIT 1",
    " ) od ON TRUE"
  )
}

alerta_resumen_page_sql <- function(tipo, con_limite) {
  vp <- if (alerta_resumen_tiene_col("valor_pronostico")) "e.valor_pronostico" else "NULL::float8 AS valor_pronostico"
  if (!alerta_resumen_tiene_col("valor_pronostico")) {
    vp <- "NULL::float8 AS valor_pronostico"
  } else {
    vp <- "e.valor_pronostico"
  }
  lim <- if (isTRUE(con_limite)) " LIMIT $2 OFFSET $3" else ""
  paste0(
    "SELECT e.id, e.eid, e.variable_id, e.fecha, e.direccion, e.ranking, e.valor,",
    " COALESCE(e.tipo, 'tardia') AS tipo, ", vp,
    " FROM campanada e",
    " WHERE e.user_id = $1::uuid",
    alerta_resumen_where_tipo(tipo),
    " ORDER BY e.fecha DESC, e.enviado_en DESC, e.id DESC",
    lim
  )
}

alerta_resumen_fila <- function(row, hoy) {
  abr <- tolower(trimws(row$nombre_abr %||% ""))
  tipo <- if (identical(row$tipo %||% "", "temprana")) "temprana" else "tardia"
  unidad <- if (exists("display_unidad", mode = "function")) {
    display_unidad(abr, row$unidad %||% "")
  } else {
    row$unidad %||% ""
  }
  mostrar <- function(v) {
    v <- suppressWarnings(as.numeric(v))
    if (length(v) != 1L || is.na(v)) return(NULL)
    if (exists("display_valor", mode = "function")) v <- display_valor(v, abr)
    alerta_resumen_num(v)
  }
  et <- alerta_resumen_etiqueta(abr, row$direccion, row$variable_nombre %||% "")
  valor_emit <- mostrar(row$valor)
  valor_obs <- mostrar(row$valor_observado)
  valor_pron <- mostrar(row$valor_pronostico)
  if (identical(tipo, "temprana") && is.null(valor_pron)) valor_pron <- valor_emit
  tiene_obs <- !is.null(valor_obs)
  n_mas <- suppressWarnings(as.integer(row$n_mas))
  resultado <- NULL
  diferencia <- NULL
  if (identical(tipo, "temprana")) {
    resultado <- alerta_resumen_resultado(row$fecha, hoy, tiene_obs, n_mas)
    if (!is.null(valor_obs) && !is.null(valor_pron)) {
      diferencia <- valor_obs - valor_pron
    }
  }
  fue <- NULL
  if (identical(tipo, "tardia")) fue <- isTRUE(row$fue_prevista)
  list(
    id = as.character(row$id),
    eid = as.integer(row$eid),
    variable_id = as.integer(row$variable_id),
    tipo = tipo,
    tipo_etiqueta = if (identical(tipo, "temprana")) "Prevista" else "Confirmada",
    fecha = as.character(as.Date(row$fecha)),
    fecha_txt = alerta_resumen_fmt_fecha(row$fecha),
    direccion = if (identical(tolower(row$direccion %||% ""), "min")) "min" else "max",
    ranking = as.integer(row$ranking),
    ranking_txt = alerta_resumen_ranking(abr, row$direccion, row$ranking),
    estacion = alerta_resumen_nombre_estacion(row$estacion_nombre, row$nombre_publico),
    alerta = et$etiqueta,
    alerta_definida = isTRUE(et$definida),
    variable = alerta_resumen_variable_nombre(abr, row$variable_nombre),
    nombre_abr = abr,
    unidad = unidad,
    valor = valor_emit,
    valor_pronostico = if (identical(tipo, "temprana")) valor_pron else NULL,
    valor_observado = valor_obs,
    diferencia = diferencia,
    resultado = resultado,
    resultado_txt = if (is.null(resultado)) NULL else alerta_resumen_resultado_etiqueta(resultado),
    fue_prevista = fue,
    reporte_url = paste0(
      "/mi-reporte/?e=", as.integer(row$eid), "&v=", as.integer(row$variable_id),
      if (identical(tipo, "temprana")) "&view=pronostico" else ""
    )
  )
}

alerta_resumen_pg_arr <- function(vals, quote = FALSE) {
  vals <- as.character(vals)
  if (isTRUE(quote)) vals <- gsub("\"", "", vals, fixed = TRUE)
  inner <- if (isTRUE(quote)) paste0("\"", vals, "\"", collapse = ",") else paste(vals, collapse = ",")
  paste0("{", inner, "}")
}

alerta_resumen_n_mas_dir <- function(df, idx, direccion) {
  if (!length(idx)) return(df)
  op <- if (identical(direccion, "max")) ">" else "<"
  ord <- if (identical(direccion, "max")) "DESC" else "ASC"
  sql <- paste0(
    "SELECT u.id::text AS id, (",
    " SELECT COUNT(*)::int FROM (",
    "   SELECT 1 FROM jornada_cerrada d",
    "   WHERE d.id_estacion = u.eid AND d.id_variable = u.vid",
    "     AND d.fecha <> u.fecha AND d.valor ", op, " u.obs",
    "   ORDER BY d.valor ", ord,
    "   LIMIT ", ALERTA_RESUMEN_RANK_MAX,
    " ) s) AS n_mas",
    " FROM unnest($1::uuid[], $2::int[], $3::int[], $4::date[], $5::float8[])",
    "   AS u(id, eid, vid, fecha, obs)"
  )
  out <- db_query(sql, params = list(
    alerta_resumen_pg_arr(df$id[idx]),
    alerta_resumen_pg_arr(as.integer(df$eid[idx])),
    alerta_resumen_pg_arr(as.integer(df$variable_id[idx])),
    alerta_resumen_pg_arr(as.character(as.Date(df$fecha[idx])), quote = TRUE),
    alerta_resumen_pg_arr(format(as.numeric(df$valor_observado[idx]), scientific = FALSE))
  ))
  if (nrow(out) == 0) return(df)
  hit <- match(as.character(out$id), as.character(df$id))
  ok <- !is.na(hit)
  df$n_mas[hit[ok]] <- as.integer(out$n_mas[ok])
  df
}

alerta_resumen_adjuntar_n_mas <- function(df, hoy) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (!("n_mas" %in% names(df))) df$n_mas <- NA_integer_
  hoy <- as.Date(hoy)
  tipo <- tolower(as.character(df$tipo))
  fecha <- as.Date(df$fecha)
  obs <- suppressWarnings(as.numeric(df$valor_observado))
  need <- tipo == "temprana" & !is.na(obs) & !is.na(fecha) & fecha < hoy
  if (!any(need)) return(df)
  dir <- tolower(as.character(df$direccion))
  df <- alerta_resumen_n_mas_dir(df, which(need & dir == "max"), "max")
  alerta_resumen_n_mas_dir(df, which(need & dir != "max"), "min")
}

alerta_resumen_filas <- function(df, hoy) {
  if (is.null(df) || nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) alerta_resumen_fila(df[i, ], hoy))
}

alerta_resumen_contar <- function(user_id, estado = "") {
  if (!alertas_tabla_existe()) return(0L)
  tipo <- alerta_resumen_tipo_sql(estado)
  df <- db_query(
    paste0(
      "SELECT COUNT(*)::int AS n FROM campanada e",
      " WHERE e.user_id = $1::uuid",
      alerta_resumen_where_tipo(tipo)
    ),
    params = list(user_id)
  )
  if (nrow(df) == 0) return(0L)
  as.integer(df$n[[1]])
}

alerta_resumen_query_page <- function(user_id, estado, limit, offset, hoy) {
  tipo <- alerta_resumen_tipo_sql(estado)
  sql <- alerta_resumen_enriquecer_sql(alerta_resumen_page_sql(tipo, TRUE), "$4")
  db_query(sql, params = list(user_id, as.integer(limit), as.integer(offset)))
}

alerta_resumen_query_all <- function(user_id, estado, hoy) {
  tipo <- alerta_resumen_tipo_sql(estado)
  inner <- alerta_resumen_page_sql(tipo, FALSE)
  sql <- alerta_resumen_enriquecer_sql(inner, "$2")
  db_query(sql, params = list(user_id))
}

alerta_resumen_metricas_periodo <- function(user_id, hoy) {
  vacio <- list(
    periodo_dias = ALERTA_RESUMEN_DIAS,
    etiqueta = paste0("\u00faltimos ", ALERTA_RESUMEN_DIAS, " d\u00edas"),
    aciertos = 0L,
    resueltas = 0L,
    falsas_alarmas = 0L,
    mae = NULL,
    unidad = NULL,
    mae_mixta = FALSE
  )
  if (!alertas_tabla_existe()) return(vacio)
  desde <- as.character(as.Date(hoy) - ALERTA_RESUMEN_DIAS)
  vp <- if (alerta_resumen_tiene_col("valor_pronostico")) "e.valor_pronostico" else "NULL::float8 AS valor_pronostico"
  if (alerta_resumen_tiene_col("valor_pronostico")) vp <- "e.valor_pronostico"
  inner <- paste0(
    "SELECT e.id, e.eid, e.variable_id, e.fecha, e.direccion, e.ranking, e.valor,",
    " 'temprana'::text AS tipo, ", vp,
    " FROM campanada e",
    " WHERE e.user_id = $1::uuid AND e.tipo = 'temprana'",
    " AND e.fecha >= $3::date AND e.fecha <= $2::date"
  )
  sql <- alerta_resumen_enriquecer_sql(inner, "$2")
  df <- db_query(sql, params = list(user_id, as.character(hoy), desde))
  df <- alerta_resumen_adjuntar_n_mas(df, hoy)
  filas <- alerta_resumen_filas(df, hoy)
  if (!length(filas)) return(vacio)
  resultados <- vapply(filas, function(f) f$resultado %||% "", character(1))
  errores <- vapply(filas, function(f) {
    d <- f$diferencia
    if (is.null(d) || !is.finite(d)) NA_real_ else as.numeric(d)
  }, numeric(1))
  errores <- abs(errores)
  m <- alerta_resumen_metricas(resultados, errores)
  resueltas <- which(resultados %in% c("cumplio", "no_cumplio"))
  unidades <- unique(vapply(filas[resueltas], function(f) f$unidad %||% "", character(1)))
  unidades <- unidades[nzchar(unidades)]
  mixta <- length(unidades) > 1L
  vacio$aciertos <- m$aciertos
  vacio$resueltas <- m$resueltas
  vacio$falsas_alarmas <- m$falsas_alarmas
  vacio$mae <- if (!mixta && is.finite(m$mae)) m$mae else NULL
  vacio$unidad <- if (!mixta && length(unidades) == 1L) unidades[[1]] else NULL
  vacio$mae_mixta <- mixta
  vacio
}

alerta_resumen_pagina <- function(user_id, estado = "", limit = 5L, offset = 0L) {
  est <- alerta_resumen_estado(estado)
  limit <- as.integer(limit)
  if (is.na(limit) || limit < 1L) limit <- 5L
  if (limit > 100L) limit <- 100L
  offset <- as.integer(offset)
  if (is.na(offset) || offset < 0L) offset <- 0L
  if (offset > 100000L) offset <- 100000L
  if (!alertas_tabla_existe()) {
    return(list(estado = est, eventos = list(), total = 0L, resumen_previstas = NULL))
  }
  hoy <- alerta_resumen_hoy()
  total <- alerta_resumen_contar(user_id, est)
  df <- if (total > 0L) alerta_resumen_query_page(user_id, est, limit, offset, hoy) else data.frame()
  df <- alerta_resumen_adjuntar_n_mas(df, hoy)
  resumen <- if (identical(est, "previstas")) alerta_resumen_metricas_periodo(user_id, hoy) else NULL
  list(
    estado = est,
    eventos = alerta_resumen_filas(df, hoy),
    total = total,
    resumen_previstas = resumen
  )
}

alerta_resumen_csv_fila <- function(f, estado) {
  u <- f$unidad %||% ""
  if (identical(estado, "confirmadas")) {
    return(c(
      f$estacion, f$alerta, f$fecha_txt,
      alerta_resumen_fmt_num(f$valor, u),
      f$ranking_txt,
      if (isTRUE(f$fue_prevista)) "S\u00ed" else "No"
    ))
  }
  if (identical(estado, "previstas")) {
    return(c(
      f$estacion, f$alerta, f$fecha_txt,
      alerta_resumen_fmt_num(f$valor_pronostico, u),
      alerta_resumen_fmt_num(f$valor_observado, u),
      alerta_resumen_fmt_diff(f$diferencia, u),
      f$resultado_txt %||% ""
    ))
  }
  suf <- if (identical(f$tipo, "temprana")) " pron." else " obs."
  valor <- if (identical(f$tipo, "temprana")) f$valor_pronostico else f$valor
  detalle <- if (identical(f$tipo, "temprana")) {
    paste0(f$resultado_txt %||% "", if (!is.null(f$valor_observado)) {
      paste0(" \u00b7 obs. ", alerta_resumen_fmt_num(f$valor_observado, u))
    } else "")
  } else {
    f$ranking_txt
  }
  c(f$tipo_etiqueta, f$estacion, f$alerta, f$fecha_txt, paste0(alerta_resumen_fmt_num(valor, u), suf), detalle)
}

alerta_resumen_csv_headers <- function(estado) {
  if (identical(estado, "confirmadas")) {
    return(c("Estaci\u00f3n", "Alerta", "Fecha", "Valor", "Ranking en la estaci\u00f3n", "\u00bfFue prevista?"))
  }
  if (identical(estado, "previstas")) {
    return(c("Estaci\u00f3n", "Alerta", "Fecha", "Pronosticado", "Observado", "Diferencia", "Resultado"))
  }
  c("Estado", "Estaci\u00f3n", "Alerta", "Fecha", "Valor", "Detalle")
}

generar_eventos_usuario_csv <- function(user_id, estado = "") {
  est <- alerta_resumen_estado(estado)
  if (!alertas_tabla_existe()) stop("sin eventos")
  hoy <- alerta_resumen_hoy()
  df <- alerta_resumen_adjuntar_n_mas(alerta_resumen_query_all(user_id, est, hoy), hoy)
  filas <- alerta_resumen_filas(df, hoy)
  if (!length(filas)) stop("sin eventos")
  hdr <- alerta_resumen_csv_headers(est)
  mat <- do.call(rbind, lapply(filas, alerta_resumen_csv_fila, estado = est))
  out <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(out) <- hdr
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write.csv(out, tmp, row.names = FALSE, fileEncoding = "UTF-8")
  readBin(tmp, "raw", file.info(tmp)$size)
}
