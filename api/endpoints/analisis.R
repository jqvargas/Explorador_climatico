#* @apiTitle Analisis

#* Resumen por lote para la tabla de estaciones (reemplaza el patron N+1).
#* Devuelve, por estacion: n_variables y (si hay variable) registros y rango de fechas.
#* @param ids IDs de estacion separados por coma (max 100)
#* @param variable_id Variable opcional para registros/rango/completitud
#* @get /analisis-tabla
function(ids = "", variable_id = "", req, res) {
  est_ids <- suppressWarnings(as.integer(unlist(strsplit(ids, ","))))
  est_ids <- est_ids[!is.na(est_ids)]
  est_ids <- unique(est_ids)
  if (length(est_ids) == 0) return(list())
  if (length(est_ids) > 100) est_ids <- est_ids[seq_len(100)]
  vid <- suppressWarnings(as.integer(variable_id))

  key <- cache_key("analisis", paste(sort(est_ids), collapse = ","), if (is.na(vid)) "-" else vid)
  cache_or_compute(key, function() {
    in_list <- paste(est_ids, collapse = ",")

    vars <- db_query(paste0(
      "SELECT id_estacion, COUNT(DISTINCT id_variable) AS n_variables
       FROM bitacora_oficial WHERE id_estacion IN (", in_list, ") GROUP BY id_estacion"
    ))

    resumen <- NULL
    if (!is.na(vid) && vid > 0) {
      resumen <- db_query(paste0(
        "SELECT id_estacion, COUNT(*) AS registros, MIN(fecha) AS fecha_min, MAX(fecha) AS fecha_max
         FROM bitacora_oficial WHERE id_estacion IN (", in_list, ") AND id_variable = ", vid,
        " GROUP BY id_estacion"
      ))
    }

    calidad <- NULL
    mv <- mv_estacion_calidad_nombre()
    if (!is.null(mv) && identical(mv, "estacion_calidad")) {
      calidad <- db_query(paste0(
        "SELECT id_estacion, nivel, score_calidad, pct_completitud, n_imposibles, pct_imposibles
         FROM estacion_calidad WHERE id_estacion IN (", in_list, ")"
      ))
    }

    rows <- lapply(est_ids, function(eid) {
      nv <- vars$n_variables[match(eid, vars$id_estacion)]
      reg <- NA_integer_; fmin <- NA; fmax <- NA
      if (!is.null(resumen)) {
        m <- match(eid, resumen$id_estacion)
        if (!is.na(m)) {
          reg <- resumen$registros[m]
          fmin <- as.character(resumen$fecha_min[m])
          fmax <- as.character(resumen$fecha_max[m])
        }
      }
      completitud <- NA_real_
      if (!is.na(reg) && !is.na(fmin) && !is.na(fmax)) {
        dias <- as.numeric(as.Date(fmax) - as.Date(fmin)) + 1
        if (!is.na(dias) && dias > 0) completitud <- round(100 * reg / dias, 1)
      }
      cal_nivel <- NA_character_
      cal_score <- NA_integer_
      cal_pct <- NA_real_
      cal_nimp <- NA_integer_
      cal_pimp <- NA_real_
      if (!is.null(calidad)) {
        cm <- match(eid, calidad$id_estacion)
        if (!is.na(cm)) {
          cal_nivel <- as.character(calidad$nivel[cm])
          cal_score <- suppressWarnings(as.integer(calidad$score_calidad[cm]))
          cal_pct <- suppressWarnings(as.numeric(calidad$pct_completitud[cm]))
          cal_nimp <- suppressWarnings(as.integer(calidad$n_imposibles[cm]))
          cal_pimp <- suppressWarnings(as.numeric(calidad$pct_imposibles[cm]))
          if (!is.na(cal_pct)) completitud <- cal_pct
        }
      }
      list(
        id_estacion = eid,
        n_variables = if (is.na(nv)) 0L else as.integer(nv),
        registros = if (is.na(reg)) 0L else as.integer(reg),
        fecha_min = if (is.na(fmin)) NA else fmin,
        fecha_max = if (is.na(fmax)) NA else fmax,
        completitud = if (is.na(completitud)) NA else completitud,
        nivel = if (is.na(cal_nivel)) NA else cal_nivel,
        score_calidad = if (is.na(cal_score)) NA else cal_score,
        n_imposibles = if (is.na(cal_nimp)) NA else cal_nimp,
        pct_imposibles = if (is.na(cal_pimp)) NA else cal_pimp
      )
    })
    do.call(rbind.data.frame, c(rows, list(stringsAsFactors = FALSE)))
  }, ttl = 1800, res = res)
}
