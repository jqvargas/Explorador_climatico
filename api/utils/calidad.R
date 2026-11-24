# Helpers para vista materializada estacion_calidad (fallback a estacion_cobertura).

# ponytail: esquema estable en caliente; evita to_regclass en cada reporte
.MV_CALIDAD_CACHE <- list(set = FALSE, val = NULL)

mv_estacion_calidad_nombre <- function() {
  if (.MV_CALIDAD_CACHE$set) return(.MV_CALIDAD_CACHE$val)
  chk <- tryCatch(
    db_query("SELECT to_regclass('public.estacion_calidad') AS t"),
    error = function(e) NULL
  )
  out <- if (!is.null(chk) && nrow(chk) > 0 && !is.na(chk$t[1])) {
    "estacion_calidad"
  } else {
    chk2 <- tryCatch(
      db_query("SELECT to_regclass('public.estacion_cobertura') AS t"),
      error = function(e) NULL
    )
    if (!is.null(chk2) && nrow(chk2) > 0 && !is.na(chk2$t[1])) "estacion_cobertura" else NULL
  }
  .MV_CALIDAD_CACHE <<- list(set = TRUE, val = out)
  out
}

tiene_mv_calidad <- function() {
  !is.null(mv_estacion_calidad_nombre())
}

calidad_join_sql <- function(alias = "cal") {
  mv <- mv_estacion_calidad_nombre()
  if (is.null(mv)) return(list(join = "", select = ""))
  if (identical(mv, "estacion_calidad")) {
    return(list(
      join = paste0(" LEFT JOIN estacion_calidad ", alias, " ON e.id = ", alias, ".id_estacion"),
      select = paste0(
        ", COALESCE(", alias, ".n_variables, 0) AS n_variables",
        ", COALESCE(", alias, ".anios_span, 0) AS anios_span",
        ", COALESCE(", alias, ".anios_span, 0) AS anios_datos",
        ", COALESCE(", alias, ".registros, 0) AS registros",
        ", COALESCE(", alias, ".pct_completitud, 0) AS pct_completitud",
        ", COALESCE(", alias, ".pct_continuidad, 0) AS pct_continuidad",
        ", COALESCE(", alias, ".n_imposibles, 0) AS n_imposibles",
        ", COALESCE(", alias, ".pct_imposibles, 0) AS pct_imposibles",
        ", COALESCE(", alias, ".score_calidad, 0) AS score_calidad",
        ", COALESCE(", alias, ".nivel, 'limitada') AS nivel",
        ", ", alias, ".fecha_max AS fecha_max",
        ", COALESCE(", alias, ".anio_ultimo, 0) AS anio_ultimo",
        ", COALESCE(", alias, ".datos_vigentes, false) AS datos_vigentes"
      )
    ))
  }
  list(
    join = paste0(" LEFT JOIN estacion_cobertura ", alias, " ON e.id = ", alias, ".id_estacion"),
    select = paste0(
      ", COALESCE(", alias, ".n_variables, 0) AS n_variables",
      ", COALESCE(", alias, ".anios_datos, 0) AS anios_span",
      ", COALESCE(", alias, ".anios_datos, 0) AS anios_datos",
      ", COALESCE(", alias, ".registros, 0) AS registros",
      ", 0::numeric AS pct_completitud",
      ", 0::numeric AS pct_continuidad",
      ", 0::bigint AS n_imposibles",
      ", 0::numeric AS pct_imposibles",
      ", NULL::int AS score_calidad",
      ", NULL::text AS nivel",
      ", ", alias, ".fecha_max AS fecha_max",
      ", NULL::int AS anio_ultimo",
      ", false AS datos_vigentes"
    )
  )
}

calidad_componentes <- function(pct_completitud, n_variables, registros, pct_imposibles,
                                pct_continuidad = NULL,
                                s_vigencia = NULL, anio_ultimo = NULL, anio_referencia = NULL,
                                anios_span = NULL) {
  pct <- as.numeric(pct_completitud %||% 0)
  nv  <- as.numeric(n_variables %||% 0)
  rg  <- as.numeric(registros %||% 0)
  pi  <- as.numeric(pct_imposibles %||% 0)
  cont <- as.numeric(pct_continuidad %||% 0)

  if (is.null(s_vigencia) && !is.null(anio_ultimo) && !is.null(anio_referencia)) {
    gap <- as.integer(anio_referencia) - as.integer(anio_ultimo)
    s_vigencia <- if (gap <= 0) 1.0 else max(0, 1 - gap / 3)
  }
  sv <- min(max(as.numeric(s_vigencia %||% 1), 0), 1)
  s_vig <- sv * 0.30
  s_comp <- min(pct / 100, 1)^1.2 * 0.25
  s_cont <- min(cont, 1) * 0.20
  s_vars <- min(nv / 4, 1) * 0.10
  s_qc   <- (1 - min(pi / 3, 1)) * 0.10
  s_vol  <- min(log10(rg + 1) / 4, 1) * 0.05

  list(
    vigencia    = round(s_vig, 3),
    completitud = round(s_comp, 3),
    continuidad = round(s_cont, 3),
    variables   = round(s_vars, 3),
    qc          = round(s_qc, 3),
    volumen     = round(s_vol, 3)
  )
}

# ponytail: columna qc_flag no cambia en caliente
.QC_FLAG_KNOWN <- NA

observacion_tiene_qc_flag <- function() {
  if (!is.na(.QC_FLAG_KNOWN)) return(.QC_FLAG_KNOWN)
  chk <- tryCatch(
    db_query("
      SELECT 1 AS ok FROM information_schema.columns
      WHERE table_schema = 'public' AND table_name = 'bitacora_oficial' AND column_name = 'qc_flag'
      LIMIT 1
    "),
    error = function(e) NULL
  )
  out <- !is.null(chk) && nrow(chk) > 0
  .QC_FLAG_KNOWN <<- out
  out
}
