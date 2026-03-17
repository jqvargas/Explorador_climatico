#* @apiTitle Datos
#* Obtener datos de observaciones con JOIN a estacion y variable
#* @param estacion_id ID estacion (requerido)
#* @param variable_id ID variable (requerido)
#* @param fecha_inicio Fecha inicio (opcional)
#* @param fecha_fin Fecha fin (opcional, default: ultimo ano)
#* @get /datos
function(estacion_id = NULL, variable_id = NULL, fecha_inicio = NULL, fecha_fin = NULL, req, res) {
  if (is.null(estacion_id) || is.null(variable_id) || estacion_id == "" || variable_id == "") {
    res$status <- 400
    return(list(error = "estacion_id y variable_id son requeridos"))
  }
  estacion_id <- as.integer(estacion_id)
  variable_id <- as.integer(variable_id)
  if (is.na(estacion_id) || is.na(variable_id)) {
    res$status <- 400
    return(list(error = "estacion_id y variable_id deben ser numericos"))
  }

  # Default fecha_fin = hoy, fecha_inicio = hace 1 ano
  if (is.null(fecha_fin) || fecha_fin == "") {
    fecha_fin <- format(Sys.Date(), "%Y-%m-%d")
  }
  if (is.null(fecha_inicio) || fecha_inicio == "") {
    fecha_inicio <- format(Sys.Date() - 365, "%Y-%m-%d")
  }

  # Calcular rango en dias para limite anonimo
  f_ini <- as.Date(fecha_inicio)
  f_fin <- as.Date(fecha_fin)
  dias_rango <- as.numeric(difftime(f_fin, f_ini, units = "days"))
  tipo_usuario <- get_tipo_usuario()
  max_dias <- LIMITES[[tipo_usuario]]$max_dias
  if (dias_rango > max_dias) {
    res$status <- 400
    return(list(error = "Para series largas usa /solicitar-descarga"))
  }

  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn))
  q <- "
    SELECT o.fecha, o.valor, e.nombre AS estacion_nombre, v.nombre AS variable_nombre, v.unidad
    FROM observacion_final o
    JOIN estacion e ON o.id_estacion = e.id
    JOIN variable v ON o.id_variable = v.id
    WHERE o.id_estacion = $1 AND o.id_variable = $2 AND o.fecha >= $3 AND o.fecha <= $4
    ORDER BY o.fecha
    LIMIT 10000
  "
  df <- DBI::dbGetQuery(conn, q, params = list(estacion_id, variable_id, fecha_inicio, fecha_fin))
  jsonlite::toJSON(df, auto_unbox = TRUE)
}

#* Resumen de datos por estacion y variable
#* @param estacion_id ID estacion (requerido)
#* @param variable_id ID variable (requerido)
#* @get /datos/resumen
function(estacion_id = NULL, variable_id = NULL, req, res) {
  if (is.null(estacion_id) || is.null(variable_id) || estacion_id == "" || variable_id == "") {
    res$status <- 400
    return(list(error = "estacion_id y variable_id son requeridos"))
  }
  estacion_id <- as.integer(estacion_id)
  variable_id <- as.integer(variable_id)
  if (is.na(estacion_id) || is.na(variable_id)) {
    res$status <- 400
    return(list(error = "estacion_id y variable_id deben ser numericos"))
  }

  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn))
  q <- "
    SELECT
      MIN(valor) AS min,
      MAX(valor) AS max,
      AVG(valor) AS promedio,
      PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY valor) AS mediana,
      COUNT(*) AS conteo,
      MIN(fecha) AS fecha_min,
      MAX(fecha) AS fecha_max
    FROM observacion_final
    WHERE id_estacion = $1 AND id_variable = $2
  "
  df <- DBI::dbGetQuery(conn, q, params = list(estacion_id, variable_id))
  jsonlite::toJSON(df[1, ], auto_unbox = TRUE)
}
