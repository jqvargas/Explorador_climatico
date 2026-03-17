#* @apiTitle Fuentes / Operadores
#* Lista fuentes u operadores de estaciones (DGA, DMC, etc.)
#* @param estacion_id Filtrar por estacion (solo la fuente de esa estacion)
#* @get /fuentes
function(estacion_id = "", req, res) {
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn))
  if (nzchar(estacion_id)) {
    eid <- as.integer(estacion_id)
    if (!is.na(eid)) {
      df <- DBI::dbGetQuery(conn, "
        SELECT f.id, f.nombre
        FROM fuente f
        INNER JOIN estacion e ON e.id_fuente = f.id
        WHERE e.id = $1
      ", params = list(eid))
      return(df)
    }
  }
  df <- DBI::dbGetQuery(conn, "SELECT id, nombre FROM fuente ORDER BY nombre")
  df
}
