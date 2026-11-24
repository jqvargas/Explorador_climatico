#* @apiTitle Datos

datos_var_nombre_abr <- function(variable_id) {
  v <- db_query(
    "SELECT COALESCE(nombre_abr, '') AS nombre_abr FROM magnitud WHERE id = $1",
    params = list(as.integer(variable_id))
  )
  if (nrow(v) == 0) return("")
  as.character(v$nombre_abr[1])
}

# Metadatos de la serie (nombre estacion/variable, unidad). Se consulta una sola
# vez por (estacion, variable) y se cachea: evita repetir strings en cada fila.
datos_meta <- function(estacion_id, variable_id) {
  cache_or_compute(
    cache_key("meta:serie:kmh", estacion_id, variable_id),
    function() {
      e <- db_query("SELECT nombre FROM mirador WHERE id = $1", params = list(estacion_id))
      v <- db_query(
        "SELECT nombre, COALESCE(nombre_abr, '') AS nombre_abr, COALESCE(unidad, '') AS unidad FROM magnitud WHERE id = $1",
        params = list(variable_id)
      )
      abr <- if (nrow(v) > 0) v$nombre_abr[1] else ""
      list(
        estacion_nombre = if (nrow(e) > 0) e$nombre[1] else "",
        variable_nombre = if (nrow(v) > 0) v$nombre[1] else "",
        nombre_abr = abr,
        unidad = display_unidad(abr, if (nrow(v) > 0) v$unidad[1] else "")
      )
    },
    ttl = 86400
  )
}

#* Serie temporal de una estacion/variable (proyeccion minima + paginacion por cursor)
#* @param estacion_id ID estacion (requerido)
#* @param variable_id ID variable (requerido)
#* @param fecha_inicio Fecha inicio (opcional, default: ultimo anio)
#* @param fecha_fin Fecha fin (opcional, default: hoy)
#* @param limit Maximo de filas por pagina (default 5000, tope 50000)
#* @param cursor Fecha desde la cual continuar (paginacion incremental)
#* @param include_meta Si "1" (default) incluye meta de la serie
#* @get /datos
function(estacion_id = "", variable_id = "", fecha_inicio = "", fecha_fin = "",
         limit = "5000", cursor = "", include_meta = "1", req, res) {
  if (is.null(estacion_id) || is.null(variable_id) || estacion_id == "" || variable_id == "") {
    res$status <- 400
    return(list(error = "estacion_id y variable_id son requeridos"))
  }
  estacion_id <- as.integer(estacion_id)
  variable_id <- as.integer(variable_id)
  if (is.na(estacion_id) || is.na(variable_id)) {
    res$status <- 400
    return(list(error = "estacion_id y variable_id deben ser num\u00e9ricos"))
  }

  n <- suppressWarnings(as.integer(limit))
  if (is.na(n) || n <= 0) n <- 5000L
  if (n > 50000L) n <- 50000L

  if (is.null(fecha_fin) || fecha_fin == "") fecha_fin <- format(Sys.Date(), "%Y-%m-%d")
  if (is.null(fecha_inicio) || fecha_inicio == "") fecha_inicio <- format(Sys.Date() - 365, "%Y-%m-%d")
  cursor_val <- if (!is.null(cursor) && nzchar(cursor)) cursor else NULL

  key <- cache_key("datos:kmh", estacion_id, variable_id, fecha_inicio, fecha_fin, n, cursor_val %||% "-")
  nombre_abr <- datos_var_nombre_abr(variable_id)
  has_qc <- observacion_tiene_qc_flag()
  qc_select <- if (has_qc) "fecha, valor, qc_flag" else "fecha, valor, 0 AS qc_flag"
  payload <- cache_or_compute(key, function() {
    if (is.null(cursor_val)) {
      q <- paste0(
        "SELECT ", qc_select, " FROM bitacora_oficial
         WHERE id_estacion = $1 AND id_variable = $2 AND fecha >= $3 AND fecha <= $4
         ORDER BY fecha LIMIT $5"
      )
      df <- db_query(q, params = list(estacion_id, variable_id, fecha_inicio, fecha_fin, n))
    } else {
      q <- paste0(
        "SELECT ", qc_select, " FROM bitacora_oficial
         WHERE id_estacion = $1 AND id_variable = $2 AND fecha > $3 AND fecha <= $4
         ORDER BY fecha LIMIT $5"
      )
      df <- db_query(q, params = list(estacion_id, variable_id, cursor_val, fecha_fin, n))
    }
    df <- df_display_valores(df, nombre_abr)
    next_cursor <- if (nrow(df) == n && nrow(df) > 0) as.character(df$fecha[nrow(df)]) else NULL
    n_imposibles <- if (has_qc && nrow(df) > 0) sum(df$qc_flag %in% c(1, 2), na.rm = TRUE) else 0L
    list(data = df, next_cursor = next_cursor, n_imposibles = n_imposibles)
  }, res = res)

  out <- list(
    data = payload$data,
    next_cursor = payload$next_cursor %||% NA
  )
  if (tolower(include_meta) %in% c("1", "true", "yes", "si")) {
    meta <- datos_meta(estacion_id, variable_id)
    if (!is.null(payload$n_imposibles)) meta$n_imposibles <- payload$n_imposibles
    out$meta <- meta
  }
  out
}

#* Resumen agregado de una serie (min/max/promedio/mediana/conteo)
#* @param estacion_id ID estacion (requerido)
#* @param variable_id ID variable (requerido)
#* @param fecha_inicio Fecha inicio (opcional, recomendado para acotar)
#* @param fecha_fin Fecha fin (opcional)
#* @get /datos/resumen
function(estacion_id = "", variable_id = "", fecha_inicio = "", fecha_fin = "", req, res) {
  if (is.null(estacion_id) || is.null(variable_id) || estacion_id == "" || variable_id == "") {
    res$status <- 400
    return(list(error = "estacion_id y variable_id son requeridos"))
  }
  estacion_id <- as.integer(estacion_id)
  variable_id <- as.integer(variable_id)
  if (is.na(estacion_id) || is.na(variable_id)) {
    res$status <- 400
    return(list(error = "estacion_id y variable_id deben ser num\u00e9ricos"))
  }
  fi <- if (!is.null(fecha_inicio) && nzchar(fecha_inicio)) fecha_inicio else NULL
  ff <- if (!is.null(fecha_fin) && nzchar(fecha_fin)) fecha_fin else NULL
  nombre_abr <- datos_var_nombre_abr(variable_id)

  key <- cache_key("resumen:kmh", estacion_id, variable_id, fi %||% "-", ff %||% "-")
  cache_or_compute(key, function() {
    base_q <- "SELECT MIN(valor) AS min, MAX(valor) AS max, AVG(valor) AS promedio,
                 PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY valor) AS mediana,
                 COUNT(*) AS conteo, MIN(fecha) AS fecha_min, MAX(fecha) AS fecha_max
               FROM bitacora_oficial WHERE id_estacion = $1 AND id_variable = $2"
    if (!is.null(fi) && !is.null(ff)) {
      df <- db_query(paste0(base_q, " AND fecha >= $3 AND fecha <= $4"),
                     params = list(estacion_id, variable_id, fi, ff))
    } else {
      df <- db_query(base_q, params = list(estacion_id, variable_id))
    }
    resumen_display_valores(as.list(df[1, ]), nombre_abr)
  }, ttl = 3600, res = res)
}
