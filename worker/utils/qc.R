# Evaluacion QC en ingesta: reglas cacheadas desde regla_de_magnitud.

.qc_rules_cache <- new.env(parent = emptyenv())

qc_cargar_reglas <- function(conn, force = FALSE) {
  if (!force && exists("rules", envir = .qc_rules_cache, inherits = FALSE)) {
    return(get("rules", envir = .qc_rules_cache, inherits = FALSE))
  }
  if (!DBI::dbExistsTable(conn, "regla_de_magnitud")) {
    rules <- data.frame(
      id_variable = integer(), min_val = numeric(), max_val = numeric(),
      allow_null = logical(), stringsAsFactors = FALSE
    )
  } else {
    rules <- DBI::dbGetQuery(conn, "
      SELECT id_variable, min_val, max_val, allow_null
      FROM regla_de_magnitud
    ")
  }
  assign("rules", rules, envir = .qc_rules_cache)
  rules
}

qc_evaluar_valor <- function(id_variable, valor, rules = NULL) {
  vid <- suppressWarnings(as.integer(id_variable))
  if (is.na(vid)) return(0L)
  if (is.null(valor) || length(valor) == 0L || is.na(valor)) {
    if (is.null(rules)) return(0L)
    r <- rules[rules$id_variable == vid, , drop = FALSE]
    if (nrow(r) == 0L) return(0L)
    return(if (isTRUE(r$allow_null[1])) 0L else 1L)
  }
  v <- suppressWarnings(as.numeric(valor))
  if (is.na(v)) return(1L)
  if (is.null(rules)) return(0L)
  r <- rules[rules$id_variable == vid, , drop = FALSE]
  if (nrow(r) == 0L) return(0L)
  if (!is.na(r$min_val[1]) && v < r$min_val[1]) return(1L)
  if (!is.na(r$max_val[1]) && v > r$max_val[1]) return(1L)
  0L
}

qc_evaluar_vector <- function(id_variables, valores, rules) {
  mapply(qc_evaluar_valor, id_variables, valores, MoreArgs = list(rules = rules), USE.NAMES = FALSE)
}

qc_invalidar_cache <- function() {
  if (exists("rules", envir = .qc_rules_cache, inherits = FALSE)) {
    rm("rules", envir = .qc_rules_cache)
  }
  invisible(TRUE)
}

# Acota un self-join de consistencia a un rango de fechas.
# Sin esto la funcion escanea las 42 M filas completas en cada llamada, lo que en
# un backfill anio por anio significa un self-join de tabla entera por anio.
# Las fechas se interpolan como literales (vienen de as.Date, no de entrada libre)
# para no renumerar los placeholders existentes.
qc_rango_fechas_clause <- function(alias, fecha_min, fecha_max) {
  if (is.null(fecha_min) || is.null(fecha_max)) return("")
  fmin <- tryCatch(as.Date(fecha_min), error = function(e) NA)
  fmax <- tryCatch(as.Date(fecha_max), error = function(e) NA)
  if (is.na(fmin) || is.na(fmax)) return("")
  sprintf(" AND %s.fecha BETWEEN '%s'::date AND '%s'::date",
          alias, format(fmin, "%Y-%m-%d"), format(fmax, "%Y-%m-%d"))
}

# QC de consistencia logica entre variables de temperatura (mismo estacion+fecha).
# Detecta lo que el rango por-valor no puede: t_max<t_min, t_mean fuera de [t_min,t_max].
# ids: 2=t_mean, 3=t_max, 4=t_min. tol absorbe redondeos en origen.
# Si se pasa `scope_estaciones` (vector de id), limita el chequeo a esas estaciones
# (mas rapido tras una ingesta acotada); si es NULL revisa toda la tabla.
# `fecha_min`/`fecha_max` acotan el rango, imprescindible para el backfill.
qc_consistencia_temperatura <- function(conn, tol = 0.5, scope_estaciones = NULL,
                                        fecha_min = NULL, fecha_max = NULL) {
  # EXISTS por variable en vez de DISTINCT: el DISTINCT escaneaba la tabla entera
  # solo para saber si la variable tiene datos.
  ids_presentes <- DBI::dbGetQuery(conn, "
    SELECT 2 AS id_variable WHERE EXISTS (SELECT 1 FROM bitacora_oficial WHERE id_variable = 2)
    UNION ALL
    SELECT 3 WHERE EXISTS (SELECT 1 FROM bitacora_oficial WHERE id_variable = 3)
    UNION ALL
    SELECT 4 WHERE EXISTS (SELECT 1 FROM bitacora_oficial WHERE id_variable = 4)
  ")$id_variable
  if (length(intersect(ids_presentes, c(3L, 4L))) < 2L) {
    return(list(t_extremos = 0L, t_mean = 0L))
  }

  scope_clause <- ""
  params_extra <- list()
  if (!is.null(scope_estaciones) && length(scope_estaciones) > 0L) {
    placeholders <- paste(sprintf("$%d", 2:(length(scope_estaciones) + 1L)), collapse = ", ")
    scope_clause <- paste0(" AND tmax.id_estacion IN (", placeholders, ")")
    params_extra <- as.list(as.integer(scope_estaciones))
  }

  tryCatch(DBI::dbExecute(conn, "SET max_parallel_workers_per_gather = 0"),
           error = function(e) NULL)

  sql_extremos <- paste0("
    WITH inconsistentes AS (
      SELECT tmax.id_estacion, tmax.fecha
      FROM bitacora_oficial tmax
      JOIN bitacora_oficial tmin
        ON tmax.id_estacion = tmin.id_estacion AND tmax.fecha = tmin.fecha
      WHERE tmax.id_variable = 3 AND tmin.id_variable = 4
        AND tmax.valor IS NOT NULL AND tmin.valor IS NOT NULL
        AND tmax.valor < tmin.valor - $1", scope_clause,
        qc_rango_fechas_clause("tmax", fecha_min, fecha_max),
        qc_rango_fechas_clause("tmin", fecha_min, fecha_max), "
    )
    UPDATE bitacora_oficial o
    SET qc_flag = 1
    FROM inconsistentes i
    WHERE o.id_estacion = i.id_estacion AND o.fecha = i.fecha
      AND o.id_variable IN (3, 4) AND o.qc_flag = 0
  ")
  n_extremos <- DBI::dbExecute(conn, sql_extremos, params = c(list(tol), params_extra))

  scope_clause_mean <- gsub("tmax.id_estacion", "tmean.id_estacion", scope_clause, fixed = TRUE)
  sql_mean_gt_max <- paste0("
    WITH inconsistentes AS (
      SELECT tmean.id_estacion, tmean.fecha
      FROM bitacora_oficial tmean
      JOIN bitacora_oficial tmax
        ON tmean.id_estacion = tmax.id_estacion AND tmean.fecha = tmax.fecha
       AND tmax.id_variable = 3
      WHERE tmean.id_variable = 2
        AND tmean.valor IS NOT NULL AND tmax.valor IS NOT NULL
        AND tmean.valor > tmax.valor + $1", scope_clause_mean,
        qc_rango_fechas_clause("tmean", fecha_min, fecha_max),
        qc_rango_fechas_clause("tmax", fecha_min, fecha_max), "
    )
    UPDATE bitacora_oficial o
    SET qc_flag = 1
    FROM inconsistentes i
    WHERE o.id_estacion = i.id_estacion AND o.fecha = i.fecha
      AND o.id_variable = 2 AND o.qc_flag = 0
  ")
  n_mean_gt_max <- DBI::dbExecute(conn, sql_mean_gt_max, params = c(list(tol), params_extra))

  scope_clause_min <- gsub("tmax.id_estacion", "tmin.id_estacion", scope_clause, fixed = TRUE)
  sql_mean_lt_min <- paste0("
    WITH inconsistentes AS (
      SELECT tmean.id_estacion, tmean.fecha
      FROM bitacora_oficial tmean
      JOIN bitacora_oficial tmin
        ON tmean.id_estacion = tmin.id_estacion AND tmean.fecha = tmin.fecha
       AND tmin.id_variable = 4
      WHERE tmean.id_variable = 2
        AND tmean.valor IS NOT NULL AND tmin.valor IS NOT NULL
        AND tmean.valor < tmin.valor - $1", scope_clause_min,
        qc_rango_fechas_clause("tmean", fecha_min, fecha_max),
        qc_rango_fechas_clause("tmin", fecha_min, fecha_max), "
    )
    UPDATE bitacora_oficial o
    SET qc_flag = 1
    FROM inconsistentes i
    WHERE o.id_estacion = i.id_estacion AND o.fecha = i.fecha
      AND o.id_variable = 2 AND o.qc_flag = 0
  ")
  n_mean_lt_min <- DBI::dbExecute(conn, sql_mean_lt_min, params = c(list(tol), params_extra))

  list(
    t_extremos = as.integer(n_extremos),
    t_mean = as.integer(n_mean_gt_max + n_mean_lt_min),
    t_mean_gt_max = as.integer(n_mean_gt_max),
    t_mean_lt_min = as.integer(n_mean_lt_min)
  )
}

# Consistencia viento DGA: u_max_diario < u2_min, u2_mean > u_max_diario (swap max/mean).
# ids: 15=u2_mean, 16=u_max_diario, 17=u2_min.
qc_consistencia_viento <- function(conn, tol = 0.1, scope_estaciones = NULL,
                                   fecha_min = NULL, fecha_max = NULL) {
  ids_presentes <- DBI::dbGetQuery(conn, "
    SELECT 15 AS id_variable WHERE EXISTS (SELECT 1 FROM bitacora_oficial WHERE id_variable = 15)
    UNION ALL
    SELECT 16 WHERE EXISTS (SELECT 1 FROM bitacora_oficial WHERE id_variable = 16)
    UNION ALL
    SELECT 17 WHERE EXISTS (SELECT 1 FROM bitacora_oficial WHERE id_variable = 17)
  ")$id_variable
  if (length(intersect(ids_presentes, c(16L, 17L))) < 2L) {
    return(list(u_extremos = 0L, u_mean = 0L))
  }

  scope_clause <- ""
  params_extra <- list()
  if (!is.null(scope_estaciones) && length(scope_estaciones) > 0L) {
    placeholders <- paste(sprintf("$%d", 2:(length(scope_estaciones) + 1L)), collapse = ", ")
    scope_clause <- paste0(" AND umax.id_estacion IN (", placeholders, ")")
    params_extra <- as.list(as.integer(scope_estaciones))
  }

  tryCatch(DBI::dbExecute(conn, "SET max_parallel_workers_per_gather = 0"),
           error = function(e) NULL)

  sql_extremos <- paste0("
    WITH inconsistentes AS (
      SELECT umax.id_estacion, umax.fecha
      FROM bitacora_oficial umax
      JOIN bitacora_oficial umin
        ON umax.id_estacion = umin.id_estacion AND umax.fecha = umin.fecha
      WHERE umax.id_variable = 16 AND umin.id_variable = 17
        AND umax.valor IS NOT NULL AND umin.valor IS NOT NULL
        AND umax.valor < umin.valor - $1", scope_clause,
        qc_rango_fechas_clause("umax", fecha_min, fecha_max),
        qc_rango_fechas_clause("umin", fecha_min, fecha_max), "
    )
    UPDATE bitacora_oficial o
    SET qc_flag = 1
    FROM inconsistentes i
    WHERE o.id_estacion = i.id_estacion AND o.fecha = i.fecha
      AND o.id_variable IN (16, 17) AND o.qc_flag = 0
  ")
  n_extremos <- DBI::dbExecute(conn, sql_extremos, params = c(list(tol), params_extra))

  scope_clause_mean <- gsub("umax.id_estacion", "umean.id_estacion", scope_clause, fixed = TRUE)
  sql_mean <- paste0("
    WITH inconsistentes AS (
      SELECT umean.id_estacion, umean.fecha
      FROM bitacora_oficial umean
      JOIN bitacora_oficial umax
        ON umean.id_estacion = umax.id_estacion AND umean.fecha = umax.fecha
       AND umax.id_variable = 16
      WHERE umean.id_variable = 15
        AND umean.valor IS NOT NULL AND umax.valor IS NOT NULL
        AND umean.valor > umax.valor + $1", scope_clause_mean,
        qc_rango_fechas_clause("umean", fecha_min, fecha_max),
        qc_rango_fechas_clause("umax", fecha_min, fecha_max), "
    )
    UPDATE bitacora_oficial o
    SET qc_flag = 1
    FROM inconsistentes i
    WHERE o.id_estacion = i.id_estacion AND o.fecha = i.fecha
      AND o.id_variable IN (15, 16) AND o.qc_flag = 0
  ")
  n_mean <- DBI::dbExecute(conn, sql_mean, params = c(list(tol), params_extra))

  list(u_extremos = as.integer(n_extremos), u_mean = as.integer(n_mean))
}
