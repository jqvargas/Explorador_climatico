#* @apiTitle Estaciones
#* Lista todas las estaciones con tiene_datos
#* @param macrozona Filtro opcional (Norte/Centro/Sur/Austral)
#* @param region Codigo region opcional
#* @param chile_only Si "1" o "true", filtra solo Chile (bbox). Default: 1 para cargar mas rapido
#* @param minimal Si "1", devuelve solo id,nombre,lat,lon,macrozona (para mapa, mas rapido)
#* @param con_datos Si "1", solo estaciones con al menos una observacion
#* @param id_variable Filtrar por variable (solo estaciones con datos de esa variable)
#* @param id_fuente Filtrar por operador/fuente (DGA, DMC, etc). ID numerico
#* @get /estaciones
function(macrozona = "", region = "", chile_only = "1", minimal = "", con_datos = "", id_fuente = "", id_variable = "") {
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn))
  sel_minimal <- tolower(minimal) %in% c("1", "true", "yes", "si")
  filtro_con_datos <- tolower(con_datos) %in% c("1", "true", "yes", "si")
  if (sel_minimal) {
    q <- "SELECT e.id, e.nombre, e.lat, e.lon, e.macrozona FROM estacion e"
    if (filtro_con_datos || (nzchar(id_variable) && !is.na(as.integer(id_variable)))) {
      subq <- "SELECT DISTINCT id_estacion FROM observacion_final"
      if (nzchar(id_variable)) {
        vid <- as.integer(id_variable)
        if (!is.na(vid)) subq <- paste0(subq, " WHERE id_variable = ", vid)
      }
      q <- paste0(q, " JOIN (", subq, ") o ON e.id = o.id_estacion")
    }
    q <- paste0(q, " WHERE 1=1")
  } else {
    q <- "SELECT e.id, e.nombre, e.lat, e.lon, e.tipo, e.macrozona, e.nom_reg, e.nom_com, e.id_fuente,
      e.fecha_ini,
      (o.id_estacion IS NOT NULL) AS tiene_datos
      FROM estacion e
      LEFT JOIN (SELECT DISTINCT id_estacion FROM observacion_final) o ON e.id = o.id_estacion
      WHERE 1=1"
  }
  params <- list()
  if (tolower(chile_only) %in% c("1", "true", "yes", "si")) {
    q <- paste0(q, " AND e.lat BETWEEN -56 AND -17 AND e.lon BETWEEN -76 AND -66")
  }
  if (!is.null(macrozona) && nzchar(macrozona)) {
    q <- paste0(q, " AND e.macrozona = $", length(params)+1)
    params <- c(params, macrozona)
  }
  if (!is.null(region) && nzchar(region)) {
    q <- paste0(q, " AND e.cod_reg = $", length(params)+1)
    params <- c(params, as.integer(region))
  }
  if (!is.null(id_fuente) && id_fuente != "" && id_fuente != "0") {
    fid <- as.integer(id_fuente)
    if (!is.na(fid) && fid > 0) {
      q <- paste0(q, " AND e.id_fuente = $", length(params)+1)
      params <- c(params, fid)
    }
  }
  q <- paste0(q, if (sel_minimal) " ORDER BY e.id LIMIT 5000" else "")
  df <- if (length(params) > 0) {
    DBI::dbGetQuery(conn, q, params = params)
  } else {
    DBI::dbGetQuery(conn, q)
  }
  if (!sel_minimal && "tiene_datos" %in% names(df)) df$tiene_datos <- as.logical(df$tiene_datos)
  df
}

#* Detalle de una estacion
#* @param id ID de la estacion
#* @get /estaciones/<id>
function(id) {
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn))
  df <- DBI::dbGetQuery(conn, "SELECT id, nombre, lat, lon, tipo, macrozona, nom_reg, nom_com, id_fuente,
    fecha_ini, altura, nom_cuen as cuenca, nom_subc as subcuenca FROM estacion WHERE id = $1", params = list(as.integer(id)))
  if (nrow(df) == 0) return(list(error = "No encontrada"))
  jsonlite::toJSON(df[1, ], auto_unbox = TRUE)
}

#* Variables disponibles en una estacion con rango de fechas
#* @param id ID de la estacion
#* @get /estaciones/<id>/variables
function(id) {
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn))
  df <- DBI::dbGetQuery(conn, "
    SELECT v.id, v.nombre, v.unidad,
      MIN(o.fecha) as fecha_min, MAX(o.fecha) as fecha_max,
      COUNT(*) as registros
    FROM variable v
    JOIN observacion_final o ON o.id_variable = v.id AND o.id_estacion = $1
    GROUP BY v.id, v.nombre, v.unidad
  ", params = list(as.integer(id)))
  df
}







