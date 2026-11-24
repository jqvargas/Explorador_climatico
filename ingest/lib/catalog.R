# Resolucion codigo_nacional + fuente -> id_estacion / id_variable.

catalog_get_id_fuente <- function(conn, nombre_fuente) {
  df <- DBI::dbGetQuery(
    conn,
    "SELECT id FROM cantera WHERE lower(nombre) = lower($1)",
    params = list(nombre_fuente)
  )
  if (nrow(df) == 0L) {
    stop("Fuente no encontrada en BD: ", nombre_fuente)
  }
  as.integer(df$id[1])
}

catalog_get_id_variable <- function(conn, nombre_abr) {
  df <- DBI::dbGetQuery(
    conn,
    "SELECT id FROM magnitud WHERE lower(nombre_abr) = lower($1)",
    params = list(nombre_abr)
  )
  if (nrow(df) == 0L) {
    stop("Variable no encontrada en BD: ", nombre_abr)
  }
  as.integer(df$id[1])
}

catalog_cargar_estaciones <- function(conn, id_fuente) {
  df <- DBI::dbGetQuery(
    conn,
    "SELECT id, codigo_nacional FROM mirador WHERE id_fuente = $1",
    params = list(id_fuente)
  )
  stats::setNames(as.integer(df$id), as.character(df$codigo_nacional))
}

catalog_aplicar_a_filas <- function(rows, estacion_map, id_variable, log_unknown = TRUE) {
  if (nrow(rows) == 0L) {
    return(rows[, c("id_estacion", "id_variable", "fecha", "valor"), drop = FALSE])
  }

  id_est <- estacion_map[rows$codigo_nacional]
  ok <- !is.na(id_est)
  if (any(!ok) && log_unknown) {
    unknown <- unique(rows$codigo_nacional[!ok])
    n_skip <- sum(!ok)
    message("[catalog] Omitiendo ", n_skip, " fila(s) con estacion desconocida (ej. ",
            paste(head(unknown, 3), collapse = ", "), ").")
  }

  out <- data.frame(
    id_estacion = as.integer(id_est[ok]),
    id_variable = as.integer(id_variable),
    fecha = rows$fecha[ok],
    valor = rows$valor[ok],
    stringsAsFactors = FALSE
  )
  out
}
