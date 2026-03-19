#* @apiTitle Metadata - variables, fuentes, regiones en una llamada
#* @param chile_only Si "1", regiones filtradas a Chile continental
#* @get /metadata
function(chile_only = "1") {
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn))
  variables <- tryCatch({
    df <- DBI::dbGetQuery(conn, "SELECT id, COALESCE(nombre, nombre_abr) AS nombre, COALESCE(unidad, '') AS unidad FROM variable ORDER BY id")
    if (is.null(df) || nrow(df) == 0) list() else df
  }, error = function(e) list())
  fuentes <- tryCatch({
    df <- DBI::dbGetQuery(conn, "SELECT id, nombre FROM fuente ORDER BY nombre")
    if (is.null(df) || nrow(df) == 0) list() else df
  }, error = function(e) list())
  q <- "SELECT DISTINCT cod_reg AS id, nom_reg AS nombre FROM estacion WHERE cod_reg IS NOT NULL AND nom_reg IS NOT NULL AND TRIM(nom_reg) != ''"
  if (tolower(chile_only) %in% c("1", "true", "yes", "si")) q <- paste0(q, " AND lat BETWEEN -56 AND -17 AND lon BETWEEN -76 AND -66")
  q <- paste0(q, " ORDER BY nom_reg")
  regiones <- tryCatch({
    df <- DBI::dbGetQuery(conn, q)
    if (is.null(df) || nrow(df) == 0) list() else df
  }, error = function(e) list())
  list(variables = variables, fuentes = fuentes, regiones = regiones)
}

#* Metadata
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
