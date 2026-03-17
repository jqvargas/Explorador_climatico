`%||%` <- function(x, y) if (is.null(x)) y else x

#* @apiTitle Descargas
#* Solicitar una descarga (JSON: estacion_ids, variable_ids, fecha_inicio, fecha_fin, email, formato)
#* @post /solicitar-descarga
function(req, res) {
  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)
  if (is.null(body)) {
    res$status <- 400
    return(list(error = "JSON invalido"))
  }
  estacion_ids <- as.integer(unlist(body$estacion_ids %||% list()))
  variable_ids <- as.integer(unlist(body$variable_ids %||% list()))
  fecha_inicio <- body$fecha_inicio
  fecha_fin <- body$fecha_fin
  email <- body$email %||% ""
  formato <- tolower(body$formato %||% "csv")

  if (length(estacion_ids) == 0 || length(variable_ids) == 0) {
    res$status <- 400
    return(list(error = "estacion_ids y variable_ids son requeridos"))
  }
  if (is.null(fecha_inicio) || is.null(fecha_fin)) {
    res$status <- 400
    return(list(error = "fecha_inicio y fecha_fin son requeridos"))
  }
  if (!formato %in% c("csv", "xlsx")) {
    res$status <- 400
    return(list(error = "formato debe ser csv o xlsx"))
  }

  tipo_usuario <- get_tipo_usuario()
  lim <- LIMITES[[tipo_usuario]]
  if (length(estacion_ids) > lim$max_estaciones) {
    res$status <- 400
    return(list(error = paste0("Maximo ", lim$max_estaciones, " estaciones permitidas")))
  }
  f_ini <- as.Date(fecha_inicio)
  f_fin <- as.Date(fecha_fin)
  dias_rango <- as.numeric(difftime(f_fin, f_ini, units = "days"))
  if (dias_rango > lim$max_dias) {
    res$status <- 400
    return(list(error = paste0("Rango maximo ", lim$max_dias, " dias")))
  }

  job_id <- uuid::UUIDgenerate()
  creado_en <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  expira_en <- format(Sys.time() + 7 * 24 * 3600, "%Y-%m-%dT%H:%M:%SZ")  # 7 dias

  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn))
  DBI::dbExecute(conn, "
    INSERT INTO trabajos_descarga (id, estado, email, params, expira_en)
    VALUES ($1, 'pendiente', $2, $3::jsonb, NOW() + INTERVAL '48 hours')
  ", params = list(
    job_id, creado_en, expira_en,
    jsonlite::toJSON(estacion_ids), jsonlite::toJSON(variable_ids),
    fecha_inicio, fecha_fin, email, formato
  ))

  redis_host <- Sys.getenv("REDIS_HOST", "127.0.0.1")
  r <- tryCatch(redux::hiredis(host = redis_host), error = function(e) NULL)
  if (!is.null(r)) {
    r$RPUSH("cola_descargas", job_id)
  }

  estado_url <- paste0("/descarga/estado/", job_id)
  list(
    job_id = job_id,
    mensaje = "Descarga en cola. Consulta el estado en estado_url.",
    estado_url = estado_url
  )
}

#* Estado de una descarga
#* @param job_id ID del trabajo
#* @get /descarga/estado/<job_id>
function(job_id) {
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn))
  df <- DBI::dbGetQuery(conn, "
    SELECT job_id, estado, link_descarga, creado_en, completado_en, expira_en
    FROM trabajos_descarga
    WHERE job_id = $1
  ", params = list(job_id))
  if (nrow(df) == 0) {
    return(list(error = "Trabajo no encontrado"))
  }
  row <- df[1, ]
  list(
    job_id = row$job_id,
    estado = row$estado,
    link_descarga = row$link_descarga,
    creado_en = row$creado_en,
    completado_en = row$completado_en,
    expira_en = row$expira_en
  )
}
