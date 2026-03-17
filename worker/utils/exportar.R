#' Genera archivo CSV con datos de observacion_final (chunked reads)
generar_archivo <- function(params, archivo_tmp, formato = "csv") {
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn))

  estacion_ids <- as.integer(params$estacion_ids)
  variable_ids <- as.integer(params$variable_ids)
  fecha_inicio <- params$fecha_inicio
  fecha_fin <- params$fecha_fin
  chunk_size <- 100000L
  offset <- 0L
  first_chunk <- TRUE

  ids_est <- paste(estacion_ids, collapse = ",")
  ids_var <- paste(variable_ids, collapse = ",")

  repeat {
    q <- paste0("
      SELECT o.fecha, o.valor, e.nombre AS estacion_nombre, v.nombre AS variable_nombre, v.unidad
      FROM observacion_final o
      JOIN estacion e ON o.id_estacion = e.id
      JOIN variable v ON o.id_variable = v.id
      WHERE o.id_estacion IN (", ids_est, ") AND o.id_variable IN (", ids_var, ")
        AND o.fecha >= $1 AND o.fecha <= $2
      ORDER BY o.fecha, o.id_estacion, o.id_variable
      LIMIT $3 OFFSET $4
    ")
    df <- DBI::dbGetQuery(conn, q, params = list(fecha_inicio, fecha_fin, chunk_size, offset))

    if (nrow(df) == 0) break

    write.table(df, archivo_tmp, sep = ",", row.names = FALSE, col.names = first_chunk, append = !first_chunk)
    first_chunk <- FALSE
    if (nrow(df) < chunk_size) break
    offset <- offset + chunk_size
  }

  invisible(archivo_tmp)
}
