#* @apiTitle Variables
#* Lista todas las variables (id, nombre, unidad, descripcion)
#* @param estacion_id Filtrar por estacion (solo variables con datos en esa estacion)
#* @get /variables
function(estacion_id = "") {
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn))
  if (nzchar(estacion_id)) {
    eid <- as.integer(estacion_id)
    if (!is.na(eid)) {
      df <- tryCatch({
        DBI::dbGetQuery(conn, "
          SELECT DISTINCT v.id, COALESCE(v.nombre, v.nombre_abr) AS nombre, COALESCE(v.unidad, '') AS unidad
          FROM variable v
          INNER JOIN observacion_final o ON o.id_variable = v.id AND o.id_estacion = $1
          ORDER BY v.id
        ", params = list(eid))
      }, error = function(e) {
        DBI::dbGetQuery(conn, "SELECT id, nombre_abr AS nombre, '' AS unidad FROM variable WHERE id IN (SELECT DISTINCT id_variable FROM observacion_final WHERE id_estacion = $1) ORDER BY id", params = list(eid))
      })
      if (is.null(df) || nrow(df) == 0) return(list())
      if (!"nombre" %in% names(df)) df$nombre <- as.character(df$id)
      return(df)
    }
  }
  df <- tryCatch({
    DBI::dbGetQuery(conn, "SELECT id, COALESCE(nombre, nombre_abr) AS nombre, COALESCE(unidad, '') AS unidad FROM variable ORDER BY id")
  }, error = function(e1) {
    tryCatch({
      DBI::dbGetQuery(conn, "SELECT id, nombre_abr AS nombre FROM variable ORDER BY id")
    }, error = function(e2) {
      tryCatch({
        DBI::dbGetQuery(conn, "SELECT id, nombre, unidad FROM variable ORDER BY id")
      }, error = function(e3) {
        d <- DBI::dbGetQuery(conn, "SELECT * FROM variable LIMIT 1")
        if (nrow(d) == 0) return(data.frame(id = integer(), nombre = character(), stringsAsFactors = FALSE))
        nm <- names(d)
        id_col <- if ("id" %in% nm) "id" else nm[1]
        nom_col <- if ("nombre" %in% nm) "nombre" else if ("nombre_abr" %in% nm) "nombre_abr" else nm[2]
        DBI::dbGetQuery(conn, paste0("SELECT ", id_col, " AS id, ", nom_col, " AS nombre FROM variable ORDER BY id"))
      })
    })
  })
  if (is.null(df) || nrow(df) == 0) return(list())
  if (!"nombre" %in% names(df)) df$nombre <- as.character(df$id)
  df
}
