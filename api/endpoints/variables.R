#* @apiTitle Variables
#* Lista todas las variables (id, nombre, unidad, descripcion)
#* @param estacion_id Filtrar por estacion (solo variables con datos en esa estacion)
#* @get /variables
function(estacion_id = "", req, res) {
  variables_apply_display_unidad <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    if ("nombre_abr" %in% names(df)) {
      df$unidad <- mapply(display_unidad, df$nombre_abr, df$unidad %||% "", USE.NAMES = FALSE)
    }
    df
  }

  if (nzchar(estacion_id)) {
    eid <- as.integer(estacion_id)
    if (!is.na(eid)) {
      df <- tryCatch({
        db_query("
          SELECT DISTINCT v.id, COALESCE(v.nombre, v.nombre_abr) AS nombre,
            COALESCE(v.nombre_abr, '') AS nombre_abr, COALESCE(v.unidad, '') AS unidad
          FROM magnitud v
          INNER JOIN bitacora_oficial o ON o.id_variable = v.id AND o.id_estacion = $1
          ORDER BY v.id", params = list(eid))
      }, error = function(e) {
        db_query(
          "SELECT id, nombre_abr AS nombre, nombre_abr, '' AS unidad FROM magnitud WHERE id IN (SELECT DISTINCT id_variable FROM bitacora_oficial WHERE id_estacion = $1) ORDER BY id",
          params = list(eid)
        )
      })
      if (is.null(df) || nrow(df) == 0) return(list())
      if (!"nombre" %in% names(df)) df$nombre <- as.character(df$id)
      return(variables_apply_display_unidad(df))
    }
  }
  cache_or_compute(cache_key("variables:kmh"), function() {
    df <- tryCatch({
      db_query("SELECT id, COALESCE(nombre, nombre_abr) AS nombre, COALESCE(nombre_abr, '') AS nombre_abr, COALESCE(unidad, '') AS unidad FROM magnitud ORDER BY id")
    }, error = function(e1) {
      tryCatch({
        db_query("SELECT id, nombre_abr AS nombre, nombre_abr FROM magnitud ORDER BY id")
      }, error = function(e2) {
        tryCatch({
          db_query("SELECT id, nombre, nombre_abr, unidad FROM magnitud ORDER BY id")
        }, error = function(e3) {
          d <- db_query("SELECT * FROM magnitud LIMIT 1")
          if (nrow(d) == 0) return(data.frame(id = integer(), nombre = character(), stringsAsFactors = FALSE))
          nm <- names(d)
          id_col <- if ("id" %in% nm) "id" else nm[1]
          nom_col <- if ("nombre" %in% nm) "nombre" else if ("nombre_abr" %in% nm) "nombre_abr" else nm[2]
          db_query(paste0("SELECT ", id_col, " AS id, ", nom_col, " AS nombre FROM magnitud ORDER BY id"))
        })
      })
    })
    if (is.null(df) || nrow(df) == 0) return(list())
    if (!"nombre" %in% names(df)) df$nombre <- as.character(df$id)
    if (!"nombre_abr" %in% names(df)) df$nombre_abr <- ""
    if (!"unidad" %in% names(df)) df$unidad <- ""
    variables_apply_display_unidad(df)
  }, ttl = 86400, res = res)
}
