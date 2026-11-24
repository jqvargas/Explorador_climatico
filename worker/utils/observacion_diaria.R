# Mantenimiento incremental de jornada_cerrada (agregacion diaria precomputada).

observacion_diaria_tabla_existe <- function(conn) {
  row <- DBI::dbGetQuery(conn, "SELECT to_regclass('public.jornada_cerrada') AS t")
  !is.null(row) && nrow(row) > 0 && !is.na(row$t[1])
}

#' TRUE si jornada_cerrada va atrasada respecto a bitacora_oficial (o fecha_objetivo).
observacion_diaria_atrasada <- function(conn, fecha_objetivo = NULL) {
  if (!observacion_diaria_tabla_existe(conn)) return(FALSE)
  max_final <- DBI::dbGetQuery(conn, "SELECT MAX(fecha) AS m FROM bitacora_oficial")$m[1]
  if (is.null(max_final) || length(max_final) == 0L || is.na(max_final)) return(FALSE)
  target <- if (is.null(fecha_objetivo)) as.Date(max_final) else as.Date(fecha_objetivo)
  if (is.na(target)) return(FALSE)
  max_diaria <- DBI::dbGetQuery(conn, "SELECT MAX(fecha) AS m FROM jornada_cerrada")$m[1]
  if (is.null(max_diaria) || length(max_diaria) == 0L || is.na(max_diaria)) return(TRUE)
  as.Date(max_diaria) < target
}

observacion_diaria_pg_int_array <- function(vals) {
  if (is.null(vals) || length(vals) == 0) return(NULL)
  vals <- unique(as.integer(unlist(vals)))
  vals <- vals[!is.na(vals) & vals > 0L]
  if (length(vals) == 0) return(NULL)
  paste0("{", paste(vals, collapse = ","), "}")
}

observacion_diaria_qc_filter <- function(alias = "o") {
  a <- alias
  paste0(
    " AND (NOT EXISTS (",
    "SELECT 1 FROM information_schema.columns ",
    "WHERE table_schema = 'public' AND table_name = 'bitacora_oficial' AND column_name = 'qc_flag'",
    ") OR ", a, ".qc_flag IS NULL OR ", a, ".qc_flag = 0)"
  )
}

observacion_diaria_agg_valor_sql <- function() {
  paste0(
    "CASE WHEN lower(v.nombre_abr) = 'pp' THEN SUM(o.valor)",
    " WHEN lower(v.nombre_abr) IN ('u_max_diario', 'u_10_kt', 'u_rafaga') THEN MAX(o.valor)",
    " ELSE AVG(o.valor) END"
  )
}

observacion_diaria_agg_kind_sql <- function() {
  paste0(
    "CASE WHEN lower(v.nombre_abr) = 'pp' THEN 'sum'",
    " WHEN lower(v.nombre_abr) IN ('u_max_diario', 'u_10_kt', 'u_rafaga') THEN 'max'",
    " ELSE 'avg' END"
  )
}

#' Origen del upsert: la tabla entera o solo los triples que el QC acaba de tocar.
#'
#' Reagregar un rango completo cuesta ~1200 filas/s, asi que en el backfill (un
#' anio de millones de filas por iteracion, decenas de valores cambiados) hay que
#' entrar por el conjunto tocado y no por el rango. `claves` permite entrar por
#' (estacion, fecha) para el u_max_diario derivado, que no tiene su propio id_variable.
observacion_diaria_desde_sql <- function(conn, solo_tocadas, claves = c("id_estacion", "id_variable", "fecha")) {
  usable <- isTRUE(solo_tocadas) &&
    exists("qc_mv_tocadas_existe", mode = "function") && qc_mv_tocadas_existe(conn)
  if (!usable) return(" FROM bitacora_oficial o")
  paste0(
    " FROM (SELECT DISTINCT ", paste(claves, collapse = ", "), " FROM ", QC_MV_TOCADAS, ") t",
    " JOIN bitacora_oficial o ON ",
    paste0("o.", claves, " = t.", claves, collapse = " AND "))
}

#' Upsert u_max_diario canonico desde u_10_kt (kn -> m/s) donde no hay u_max_diario nativo.
observacion_diaria_upsert_u_max_diario_derivado <- function(conn, fecha_min, fecha_max, scope_estaciones = NULL,
                                                     solo_tocadas = FALSE) {
  # ponytail: u_10_kt ahora es predominante DMC (nudos), no un max de respaldo.
  # Fabricar u_max_diario = MAX(kt)*0.514444 mezclaba estadístico (moda vs máximo) y
  # fuente (DMC vs DGA). DGA ya persiste u2_*. Borrar esta función si nadie la extraña.
  invisible(0L)
}

#' Borra agregados diarios que se quedaron sin ninguna fila fuente con qc_flag = 0.
#'
#' El upsert solo hace INSERT/UPDATE: si todas las filas de bitacora_oficial de
#' un (estacion, variable, fecha) quedan marcadas, el SELECT no produce fila, el
#' ON CONFLICT no dispara y el agregado viejo sobrevive. Sin este purge, marcar
#' un valor es puramente cosmetico: sigue en el mapa, en el ranking y en alertas.
#'
#' Arranca desde las filas ya marcadas (qc_flag <> 0) y no desde jornada_cerrada.
#' La version que barria el rango completo hacia un anti-join contra las 42 M filas
#' de cada tabla y tardaba ~2 min por anio volcando a disco; partir del conjunto
#' marcado, que son miles de filas, deja el mismo resultado en segundos: un
#' agregado solo puede quedar rancio si su fuente fue marcada.
observacion_diaria_purge_rango <- function(conn, fecha_min, fecha_max, scope_estaciones = NULL) {
  if (!observacion_diaria_tabla_existe(conn)) return(invisible(0L))

  scope_sql <- ""
  params <- list(format(fecha_min, "%Y-%m-%d"), format(fecha_max, "%Y-%m-%d"))
  eids_arr <- observacion_diaria_pg_int_array(scope_estaciones)
  if (!is.null(eids_arr)) {
    scope_sql <- " AND o.id_estacion = ANY($3::int[])"
    params <- c(params, list(eids_arr))
  }

  marcadas <- paste0(
    "SELECT DISTINCT o.id_estacion, o.id_variable, o.fecha",
    " FROM bitacora_oficial o",
    " WHERE o.fecha >= $1::date AND o.fecha <= $2::date",
    " AND o.qc_flag <> 0", scope_sql)

  sin_fuente_valida <- paste0(
    " AND NOT EXISTS (",
    "   SELECT 1 FROM bitacora_oficial o2",
    "   WHERE o2.id_estacion = d.id_estacion",
    "     AND o2.id_variable = d.id_variable",
    "     AND o2.fecha = d.fecha",
    "     AND o2.valor IS NOT NULL AND o2.qc_flag = 0)")

  n <- DBI::dbExecute(conn, paste0(
    "DELETE FROM jornada_cerrada d USING (", marcadas, ") f",
    " WHERE d.id_estacion = f.id_estacion AND d.id_variable = f.id_variable",
    "   AND d.fecha = f.fecha", sin_fuente_valida), params = params)

  if (n > 0L) message("[obs_diaria] Purge ", fecha_min, "..", fecha_max, ": ", n, " agregado(s) sin fuente valida")
  invisible(n)
}

#' Upsert diarios para fechas/estaciones tocadas por ingesta.
#'
#' `solo_tocadas = TRUE` limita el recalculo a los triples que publica el QC
#' multinivel en su tabla temporal. Sirve para el backfill, donde el rango es un
#' anio entero; el pipeline diario lo deja en FALSE porque ahi hay que agregar
#' tambien las filas recien ingestadas, que el QC no marco.
observacion_diaria_upsert_rango <- function(conn, fecha_min, fecha_max, scope_estaciones = NULL,
                                            solo_tocadas = FALSE, id_variables = NULL) {
  if (!observacion_diaria_tabla_existe(conn)) {
    message("[obs_diaria] Tabla jornada_cerrada no existe; omitiendo upsert")
    return(invisible(0L))
  }
  fmin <- tryCatch(as.Date(fecha_min), error = function(e) NA)
  fmax <- tryCatch(as.Date(fecha_max), error = function(e) NA)
  if (is.na(fmin) || is.na(fmax)) {
    message("[obs_diaria] Rango de fechas invalido; omitiendo upsert")
    return(invisible(0L))
  }
  if (fmin > fmax) {
    tmp <- fmin
    fmin <- fmax
    fmax <- tmp
  }

  scope_sql <- ""
  params <- list(format(fmin, "%Y-%m-%d"), format(fmax, "%Y-%m-%d"))
  eids_arr <- observacion_diaria_pg_int_array(scope_estaciones)
  if (!is.null(eids_arr)) {
    scope_sql <- " AND o.id_estacion = ANY($3::int[])"
    params <- c(params, list(eids_arr))
  }
  vid_arr <- observacion_diaria_pg_int_array(id_variables)
  if (!is.null(vid_arr)) {
    idx <- length(params) + 1L
    scope_sql <- paste0(scope_sql, " AND o.id_variable = ANY($", idx, "::int[])")
    params <- c(params, list(vid_arr))
  }

  qc <- observacion_diaria_qc_filter("o")
  agg_v <- observacion_diaria_agg_valor_sql()
  agg_k <- observacion_diaria_agg_kind_sql()
  sql <- paste0(
    "INSERT INTO jornada_cerrada (id_estacion, id_variable, fecha, valor, agg_kind, updated_at)",
    " SELECT o.id_estacion, o.id_variable, o.fecha, ", agg_v, ", ", agg_k, ", NOW()",
    observacion_diaria_desde_sql(conn, solo_tocadas),
    " JOIN magnitud v ON v.id = o.id_variable",
    " WHERE o.fecha >= $1::date AND o.fecha <= $2::date",
    " AND o.valor IS NOT NULL", qc, scope_sql,
    " GROUP BY o.id_estacion, o.id_variable, o.fecha, v.nombre_abr",
    " ON CONFLICT (id_estacion, id_variable, fecha) DO UPDATE SET",
    " valor = EXCLUDED.valor, agg_kind = EXCLUDED.agg_kind, updated_at = NOW()"
  )

  t0 <- Sys.time()
  n <- DBI::dbExecute(conn, sql, params = params)
  n_deriv <- observacion_diaria_upsert_u_max_diario_derivado(conn, fmin, fmax, scope_estaciones, solo_tocadas)
  # El purge va aca y no en el llamador para que todo caller (pipeline diario y
  # backfill) quede cubierto por igual. Tambien con id_variables: si no, el
  # catchup de viento deja basura en diaria.
  n_purge <- observacion_diaria_purge_rango(conn, fmin, fmax, scope_estaciones)
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  message("[obs_diaria] Upsert ", fmin, "..", fmax,
          if (!is.null(eids_arr)) paste0(" (", length(unique(scope_estaciones)), " estaciones)") else "",
          ": ", n, " filas (+", n_deriv, " u_max_diario deriv., -", n_purge, " purgadas) en ", elapsed, " s")
  invisible(n + n_deriv)
}

observacion_diaria_n_days <- function(conn, eid, vid) {
  if (!observacion_diaria_tabla_existe(conn)) return(0L)
  df <- DBI::dbGetQuery(conn,
    "SELECT COUNT(*)::int AS n FROM jornada_cerrada WHERE id_estacion = $1 AND id_variable = $2",
    params = list(as.integer(eid), as.integer(vid))
  )
  if (nrow(df) == 0) 0L else as.integer(df$n[1])
}

observacion_diaria_fetch <- function(conn, eid, vid) {
  if (!observacion_diaria_tabla_existe(conn)) return(NULL)
  df <- DBI::dbGetQuery(conn,
    "SELECT fecha, valor FROM jornada_cerrada
     WHERE id_estacion = $1 AND id_variable = $2 ORDER BY fecha",
    params = list(as.integer(eid), as.integer(vid))
  )
  if (nrow(df) == 0) return(NULL)
  df$fecha <- as.Date(df$fecha)
  df$valor <- suppressWarnings(as.numeric(df$valor))
  df
}
