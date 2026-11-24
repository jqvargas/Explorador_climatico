# Consultas de catalogo (lectura) para orquestacion MeteoVision.

obtener_id_fuente <- function(connection, fuente) {
  df <- DBI::dbGetQuery(
    connection,
    "SELECT id FROM cantera WHERE lower(nombre) = lower($1)",
    params = list(fuente)
  )
  if (nrow(df) == 0L) {
    stop("Fuente no valida: ", fuente)
  }
  as.integer(df$id[1])
}

obtener_id_variable <- function(connection, nombre_variable_abr) {
  df <- DBI::dbGetQuery(
    connection,
    "SELECT id FROM magnitud WHERE lower(nombre_abr) = lower($1)",
    params = list(nombre_variable_abr)
  )
  if (nrow(df) == 0L) {
    stop("Variable no valida: ", nombre_variable_abr)
  }
  as.integer(df$id[1])
}

obtener_nombre_abr_variable <- function(connection, id_variable) {
  df <- DBI::dbGetQuery(
    connection,
    "SELECT nombre_abr FROM magnitud WHERE id = $1",
    params = list(as.integer(id_variable))
  )
  df$nombre_abr[1]
}

obtener_estaciones_fuente <- function(connection, nombre_fuente) {
  if (identical(nombre_fuente, "all")) {
    return(DBI::dbGetQuery(connection, "SELECT * FROM mirador"))
  }
  id_fuente <- obtener_id_fuente(connection, nombre_fuente)
  DBI::dbGetQuery(
    connection,
    "SELECT * FROM mirador WHERE id_fuente = $1",
    params = list(id_fuente)
  )
}

obtener_estaciones_con_datos_variable <- function(connection, id_fuente, id_variable) {
  df <- DBI::dbGetQuery(
    connection,
    "SELECT DISTINCT e.codigo_nacional
     FROM mirador e
     JOIN bitacora_oficial o ON e.id = o.id_estacion
     WHERE e.id_fuente = $1 AND o.id_variable = $2",
    params = list(as.integer(id_fuente), as.integer(id_variable))
  )
  df$codigo_nacional
}

# Estaciones de la fuente sin fila en bitacora_oficial para (id_variable, fecha).
obtener_codigos_sin_fecha <- function(connection, id_fuente, id_variable, fecha) {
  df <- DBI::dbGetQuery(
    connection,
    "SELECT e.codigo_nacional
     FROM mirador e
     WHERE e.id_fuente = $1
       AND NOT EXISTS (
         SELECT 1 FROM bitacora_oficial o
         WHERE o.id_estacion = e.id
           AND o.id_variable = $2
           AND o.fecha = $3::date
       )",
    params = list(
      as.integer(id_fuente),
      as.integer(id_variable),
      format(as.Date(fecha), "%Y-%m-%d")
    )
  )
  as.character(df$codigo_nacional)
}
