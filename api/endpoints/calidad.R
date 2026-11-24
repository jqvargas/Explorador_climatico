#* @apiTitle Calidad estacion

#* Desglose de calidad precalculada para una estacion (mapa, reporte, popup)
#* @param id ID de la estacion
#* @get /estaciones/<id>/calidad
function(id, req, res) {
  eid <- as.integer(id)
  if (is.na(eid)) {
    res$status <- 400
    return(list(error = "id inv\u00e1lido"))
  }

  cache_or_compute(cache_key("estacion:calidad", eid), function() {
    mv <- mv_estacion_calidad_nombre()
    if (is.null(mv)) {
      return(list(error = "Vista de calidad no disponible"))
    }

    if (identical(mv, "estacion_calidad")) {
      df <- db_query(
        "SELECT nivel, score_calidad, pct_completitud, pct_continuidad, anios_span,
                n_variables, registros, n_imposibles, pct_imposibles,
                fecha_max, anio_ultimo, anio_referencia, datos_vigentes, s_vigencia
         FROM estacion_calidad WHERE id_estacion = $1",
        params = list(eid)
      )
    } else {
      df <- db_query(
        "SELECT n_variables, anios_datos AS anios_span, registros, fecha_max,
                0::numeric AS pct_completitud, 0::numeric AS pct_continuidad,
                0::bigint AS n_imposibles, 0::numeric AS pct_imposibles,
                NULL::int AS score_calidad, NULL::text AS nivel,
                NULL::int AS anio_ultimo, NULL::int AS anio_referencia,
                false AS datos_vigentes, NULL::float AS s_vigencia
         FROM estacion_cobertura WHERE id_estacion = $1",
        params = list(eid)
      )
    }

    if (nrow(df) == 0) return(list(error = "Sin datos de calidad para esta estaci\u00f3n"))

    row <- df[1, ]
    s_vig <- suppressWarnings(as.numeric(row$s_vigencia))
    if (is.na(s_vig)) {
      gap <- if (!is.null(row$anio_referencia) && !is.null(row$anio_ultimo)) {
        as.integer(row$anio_referencia) - as.integer(row$anio_ultimo)
      } else NA_integer_
      s_vig <- if (!is.na(gap)) {
        if (gap <= 0) 1.0 else max(0, 1 - gap / 3)
      } else NULL
    }
    comp <- calidad_componentes(
      row$pct_completitud, row$n_variables, row$registros, row$pct_imposibles,
      pct_continuidad = row$pct_continuidad,
      s_vigencia = s_vig,
      anio_ultimo = row$anio_ultimo,
      anio_referencia = row$anio_referencia,
      anios_span = row$anios_span
    )
    score <- suppressWarnings(as.integer(row$score_calidad))
    if (is.na(score)) {
      score <- round(100 * sum(unlist(comp)))
    }
    nivel <- as.character(row$nivel %||% "")
    if (!nzchar(nivel)) {
      nivel <- cut(
        score,
        breaks = c(-Inf, 45, 55, 95, Inf),
        labels = c("limitada", "regular", "buena", "excelente"),
        right = FALSE
      )
      nivel <- as.character(nivel)
    }

    list(
      nivel = nivel,
      score = score,
      pct_completitud = as.numeric(row$pct_completitud %||% 0),
      pct_continuidad = as.numeric(row$pct_continuidad %||% 0),
      anios_span = as.integer(row$anios_span %||% 0),
      n_variables = as.integer(row$n_variables %||% 0),
      registros = as.integer(row$registros %||% 0),
      n_imposibles = as.integer(row$n_imposibles %||% 0),
      pct_imposibles = as.numeric(row$pct_imposibles %||% 0),
      fecha_max = as.character(row$fecha_max %||% ""),
      anio_ultimo = as.integer(row$anio_ultimo %||% NA_integer_),
      anio_referencia = as.integer(row$anio_referencia %||% NA_integer_),
      datos_vigentes = isTRUE(row$datos_vigentes),
      componentes = comp
    )
  }, ttl = 1800, res = res)
}
