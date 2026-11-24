#* @apiTitle Metadata - variables, fuentes, regiones en una llamada
#* @param chile_only Si "1", regiones filtradas a Chile continental
#* @get /metadata
function(chile_only = "1", req, res) {
  cache_only <- tolower(chile_only) %in% c("1", "true", "yes", "si")
  cache_or_compute(cache_key("metadata", if (cache_only) 1 else 0), function() {
    variables <- tryCatch({
      df <- db_query("SELECT id, COALESCE(nombre, nombre_abr) AS nombre, COALESCE(unidad, '') AS unidad FROM magnitud ORDER BY id")
      if (is.null(df) || nrow(df) == 0) list() else df
    }, error = function(e) list())
    fuentes <- tryCatch({
      df <- db_query("SELECT id, nombre FROM cantera ORDER BY nombre")
      if (is.null(df) || nrow(df) == 0) list() else df
    }, error = function(e) list())
    q <- "SELECT DISTINCT cod_reg AS id, nom_reg AS nombre FROM mirador WHERE cod_reg IS NOT NULL AND nom_reg IS NOT NULL AND TRIM(nom_reg) != ''"
    if (cache_only) q <- paste0(q, " AND lat BETWEEN -56 AND -17 AND lon BETWEEN -76 AND -66")
    q <- paste0(q, " ORDER BY nom_reg")
    regiones <- tryCatch({
      df <- db_query(q)
      if (is.null(df) || nrow(df) == 0) list() else df
    }, error = function(e) list())
    list(variables = variables, fuentes = fuentes, regiones = regiones)
  }, ttl = 86400, res = res)
}

#* Metadata
#* Lista fuentes u operadores de estaciones (DGA, DMC, etc.)
#* @param estacion_id Filtrar por estacion (solo la fuente de esa estacion)
#* @get /fuentes
function(estacion_id = "", req, res) {
  if (nzchar(estacion_id)) {
    eid <- as.integer(estacion_id)
    if (!is.na(eid)) {
      return(db_query("
        SELECT f.id, f.nombre
        FROM cantera f
        INNER JOIN mirador e ON e.id_fuente = f.id
        WHERE e.id = $1", params = list(eid)))
    }
  }
  cache_or_compute(cache_key("fuentes"), function() {
    db_query("SELECT id, nombre FROM cantera ORDER BY nombre")
  }, ttl = 86400, res = res)
}
