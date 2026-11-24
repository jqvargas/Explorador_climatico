# Carga funciones de reporte compartidas con Shiny (montaje /shiny en contenedor API)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

# Clave y TTL compartidos por el endpoint y el warm posterior a la ingesta.
# Definirlos en los dos lados haria que un bump de version calentara una clave
# distinta de la que se sirve, sin ningun error visible.
REPORTE_PAYLOAD_CACHE_VERSION <- "v18"
# La frescura la da invalidate_warm_reporte_cache.R tras la ingesta diaria; el
# TTL es solo el techo de obsolescencia si ese paso llegara a fallar.
REPORTE_PAYLOAD_CACHE_TTL <- 21600L

# Mismo reloj que la grilla y Open-Meteo. Sys.Date() de la API es UTC.
reporte_hoy_cl <- function() {
  as.Date(Sys.time(), tz = "America/Santiago")
}

reporte_payload_cache_key <- function(eid, vid) {
  cache_key(
    paste0("reporte:payload:", REPORTE_PAYLOAD_CACHE_VERSION),
    eid, vid, format(reporte_hoy_cl(), "%Y-%m-%d")
  )
}

# Shell = header + pronostico + ultimas sin serie historica (chart/widgets/ciclo/tabla).
reporte_payload_shell_cache_key <- function(eid, vid) {
  cache_key(
    paste0("reporte:payload:shell:", REPORTE_PAYLOAD_CACHE_VERSION),
    eid, vid, format(reporte_hoy_cl(), "%Y-%m-%d")
  )
}

# max-age y stale-while-revalidate no cruzan la medianoche de Chile:
# un HIT no renueva frescura de un cuerpo del dia anterior.
reporte_cache_control_public <- function(now = Sys.time()) {
  partes <- as.POSIXlt(now, tz = "America/Santiago")
  secs <- as.integer((24L * 3600L) - (partes$hour * 3600L + partes$min * 60L + floor(partes$sec)))
  if (length(secs) != 1L || is.na(secs) || secs < 1L) secs <- 1L
  sprintf(
    "public, max-age=%d, stale-while-revalidate=%d",
    min(600L, secs), min(3600L, secs)
  )
}

# Unico punto que lee/escribe el cache del reporte. El endpoint y el warm
# nocturno deben coincidir en clave, TTL y serializador: si divergen, el warm
# llena una entrada que el endpoint nunca usa y nadie se entera.
reporte_payload_cached <- function(eid, vid, res = NULL) {
  cache_or_compute(
    reporte_payload_cache_key(eid, vid),
    function() reporte_api_build_payload(eid, vid),
    ttl = REPORTE_PAYLOAD_CACHE_TTL,
    res = res,
    encode = pronostico_json_encode
  )
}

reporte_payload_shell_cached <- function(eid, vid, res = NULL) {
  cache_or_compute(
    reporte_payload_shell_cache_key(eid, vid),
    function() reporte_api_build_shell_payload(eid, vid),
    ttl = REPORTE_PAYLOAD_CACHE_TTL,
    res = res,
    encode = pronostico_json_encode
  )
}

ec_serie_normalizar <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
  if ("valor" %in% names(df)) {
    df$valor <- suppressWarnings(as.numeric(df$valor))
    df$valor[is.nan(df$valor)] <- NA_real_
  }
  df
}

.REPORTE_SOURCED <- FALSE

reporte_source_shiny <- function() {
  if (.REPORTE_SOURCED) return(invisible(TRUE))
  shiny_root <- Sys.getenv("SHINY_ROOT", "/shiny")
  vl <- file.path(shiny_root, "variable_labels.R")
  rp <- file.path(shiny_root, "modules", "reporte.R")
  if (!file.exists(rp)) {
    stop("No se encuentra reporte.R en SHINY_ROOT=", shiny_root)
  }
  if (file.exists(vl)) source(vl, local = FALSE)
  source(rp, local = FALSE)
  .REPORTE_SOURCED <<- TRUE
  invisible(TRUE)
}

reporte_fecha_inicio_var <- function(sv, vid) {
  row <- sv[sv$id == vid, , drop = FALSE]
  if (nrow(row) == 0 || is.na(row$fecha_min[1])) return("1900-01-01")
  format(as.Date(row$fecha_min[1]), "%Y-%m-%d")
}

reporte_series_meta <- function(eid, vid, info, sv) {
  row <- sv[sv$id == vid, , drop = FALSE]
  if (exists("datos_meta", mode = "function")) {
    m <- datos_meta(eid, vid)
    return(list(
      estacion_nombre = as.character(info$nombre %||% m$estacion_nombre %||% ""),
      variable_nombre = as.character(m$variable_nombre %||% ""),
      unidad = as.character(m$unidad %||% "")
    ))
  }
  list(
    estacion_nombre = as.character(info$nombre %||% ""),
    variable_nombre = if (nrow(row) > 0) as.character(row$nombre[1]) else "",
    unidad = if (nrow(row) > 0) as.character(row$unidad[1]) else ""
  )
}

reporte_fetch_series_db <- function(eid, vid, fecha_inicio = "1900-01-01", fecha_fin = NULL,
                                    meta_override = NULL) {
  if (is.null(fecha_fin) || !nzchar(fecha_fin)) {
    fecha_fin <- format(Sys.Date(), "%Y-%m-%d")
  }
  has_qc <- observacion_tiene_qc_flag()
  qc_select <- if (has_qc) "fecha, valor, qc_flag" else "fecha, valor, 0 AS qc_flag"
  all_chunks <- list()
  cursor_val <- NULL
  meta <- meta_override

  for (page in seq_len(20)) {
    if (is.null(cursor_val)) {
      q <- paste0(
        "SELECT ", qc_select, " FROM bitacora_oficial
         WHERE id_estacion = $1 AND id_variable = $2 AND fecha >= $3 AND fecha <= $4
         ORDER BY fecha LIMIT 50000"
      )
      df <- db_query(q, params = list(eid, vid, fecha_inicio, fecha_fin))
    } else {
      q <- paste0(
        "SELECT ", qc_select, " FROM bitacora_oficial
         WHERE id_estacion = $1 AND id_variable = $2 AND fecha > $3 AND fecha <= $4
         ORDER BY fecha LIMIT 50000"
      )
      df <- db_query(q, params = list(eid, vid, cursor_val, fecha_fin))
    }
    if (is.null(df) || nrow(df) == 0) break
    all_chunks <- c(all_chunks, list(df))
    if (nrow(df) < 50000) break
    cursor_val <- as.character(df$fecha[nrow(df)])
  }

  if (length(all_chunks) == 0) return(NULL)
  out <- do.call(rbind, all_chunks)
  list(df = out, meta = meta)
}

# La diaria puede ir un dia atras del final. Pisa o agrega solo las fechas que
# el final trae; no borra filas que solo viven en la diaria (viento derivado).
reporte_merge_cola_final <- function(df_diaria, df_final) {
  if (is.null(df_final) || !is.data.frame(df_final) || nrow(df_final) == 0) {
    return(df_diaria)
  }
  df_final$fecha <- as.Date(df_final$fecha)
  df_final$valor <- suppressWarnings(as.numeric(df_final$valor))
  ok <- !is.na(df_final$fecha) & !is.na(df_final$valor)
  df_final <- df_final[ok, c("fecha", "valor"), drop = FALSE]
  if (nrow(df_final) == 0) return(df_diaria)
  df_final <- df_final[!duplicated(df_final$fecha, fromLast = TRUE), , drop = FALSE]
  if (is.null(df_diaria) || !is.data.frame(df_diaria) || nrow(df_diaria) == 0) {
    df_final <- df_final[order(df_final$fecha), , drop = FALSE]
    rownames(df_final) <- NULL
    return(df_final)
  }
  df_diaria$fecha <- as.Date(df_diaria$fecha)
  keep <- is.na(df_diaria$fecha) | !(df_diaria$fecha %in% df_final$fecha)
  out <- rbind(
    df_diaria[keep, c("fecha", "valor"), drop = FALSE],
    df_final
  )
  out <- out[order(out$fecha), , drop = FALSE]
  rownames(out) <- NULL
  out
}

reporte_fetch_cola_final <- function(eid, vid) {
  has_qc <- exists("observacion_tiene_qc_flag", mode = "function") && observacion_tiene_qc_flag()
  qc_where <- if (has_qc) " AND (qc_flag IS NULL OR qc_flag = 0)" else ""
  q <- paste0(
    "SELECT fecha, valor FROM bitacora_oficial",
    " WHERE id_estacion = $1 AND id_variable = $2",
    " AND fecha >= (CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date - 14",
    " AND fecha <= (CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date",
    " AND valor IS NOT NULL",
    qc_where
  )
  tryCatch(
    db_query(q, params = list(as.integer(eid), as.integer(vid))),
    error = function(e) NULL
  )
}

# Agregacion diaria en SQL (ranking alerta temprana; evita paginar bitacora_oficial en R).
reporte_fetch_daily_agg_db <- function(eid, vid, vtype = NULL, meta_override = NULL) {
  eid <- as.integer(eid)
  vid <- as.integer(vid)
  if (is.na(eid) || is.na(vid)) return(NULL)

  var_row <- db_query(
    "SELECT nombre, COALESCE(nombre_abr, '') AS nombre_abr, COALESCE(unidad, '') AS unidad FROM magnitud WHERE id = $1",
    params = list(vid)
  )
  if (nrow(var_row) == 0) return(NULL)
  var_nombre <- as.character(var_row$nombre[1])
  nombre_abr <- as.character(var_row$nombre_abr[1])
  if (is.null(vtype) || !nzchar(vtype)) {
    if (exists("reporte_detect_var_type", mode = "function")) {
      vtype <- reporte_detect_var_type(paste(var_nombre, nombre_abr))
    } else {
      vtype <- "generic"
    }
  }
  meta <- meta_override %||% list(variable_nombre = var_nombre, unidad = var_row$unidad[1])

  df_diaria <- tryCatch({
    reg <- db_query("SELECT to_regclass('public.jornada_cerrada') AS t")
    if (is.null(reg) || nrow(reg) == 0 || is.na(reg$t[1])) return(NULL)
    db_query(
      "SELECT fecha, valor FROM jornada_cerrada WHERE id_estacion = $1 AND id_variable = $2 ORDER BY fecha",
      params = list(eid, vid)
    )
  }, error = function(e) NULL)
  if (!is.null(df_diaria) && nrow(df_diaria) > 0) {
    df_diaria$fecha <- as.Date(df_diaria$fecha)
    df_diaria$valor <- suppressWarnings(as.numeric(df_diaria$valor))
    df_diaria <- reporte_merge_cola_final(df_diaria, reporte_fetch_cola_final(eid, vid))
    return(list(df = df_diaria, meta = meta, vtype = vtype))
  }

  has_qc <- exists("observacion_tiene_qc_flag", mode = "function") && observacion_tiene_qc_flag()
  qc_where <- if (has_qc) " AND (qc_flag IS NULL OR qc_flag = 0)" else ""
  abr <- tolower(trimws(nombre_abr %||% ""))
  agg <- if (identical(vtype, "precip") || identical(abr, "pp")) {
    "SUM(valor)"
  } else if (abr %in% c("u_max_diario", "u_rafaga")) {
    "MAX(valor)"
  } else if (identical(abr, "u2_min")) {
    "MIN(valor)"
  } else {
    "AVG(valor)"
  }
  q <- paste0(
    "SELECT fecha, ", agg, " AS valor FROM bitacora_oficial",
    " WHERE id_estacion = $1 AND id_variable = $2 AND valor IS NOT NULL",
    qc_where, " GROUP BY fecha ORDER BY fecha"
  )
  df <- db_query(q, params = list(eid, vid))
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df$fecha <- as.Date(df$fecha)
  df$valor <- suppressWarnings(as.numeric(df$valor))
  list(df = df, meta = meta, vtype = vtype)
}

# Diario en SQL primero; fallback a serie cruda desde fecha_min (no 1900 fijo).
reporte_fetch_series_for_report <- function(eid, vid, sv, info) {
  meta <- reporte_series_meta(eid, vid, info, sv)
  rd <- reporte_fetch_daily_agg_db(eid, vid, meta_override = meta)
  if (is.null(rd)) {
    fi <- reporte_fecha_inicio_var(sv, vid)
    rd <- reporte_fetch_series_db(eid, vid, fecha_inicio = fi, meta_override = meta)
  }
  rd
}

# Quita fechas anteriores al hoy de Chile. El carrusel no debe recibirlas
# y la grilla no debe hornearlas como pronostico.
reporte_filtrar_lite_desde_hoy <- function(lite, hoy = reporte_hoy_cl()) {
  if (is.null(lite) || !is.list(lite)) return(lite)
  dias <- lite$dias
  if (is.null(dias)) return(lite)
  hoy_c <- as.character(hoy)[1]
  if (!nzchar(hoy_c)) return(lite)
  if (is.data.frame(dias)) {
    if (!"fecha" %in% names(dias) || nrow(dias) == 0L) return(lite)
    lite$dias <- dias[as.character(dias$fecha) >= hoy_c, , drop = FALSE]
    return(lite)
  }
  if (!is.list(dias) || length(dias) == 0L) return(lite)
  keep <- vapply(dias, function(d) {
    if (is.null(d) || !is.list(d)) return(FALSE)
    f <- as.character(d$fecha %||% "")
    length(f) == 1L && !is.na(f) && nzchar(f) && f >= hoy_c
  }, logical(1))
  lite$dias <- dias[keep]
  lite
}

reporte_df_pronostico_from_lite <- function(pronostico_lite, vid) {
  if (is.null(pronostico_lite) || !is.list(pronostico_lite)) return(NULL)
  dias <- pronostico_lite$dias
  if (is.null(dias) || length(dias) == 0L) return(NULL)
  if (exists("pronostico_lite_day_list", mode = "function")) {
    dias <- pronostico_lite_day_list(dias)
  } else if (is.data.frame(dias)) {
    dias <- lapply(seq_len(nrow(dias)), function(i) {
      list(fecha = as.character(dias$fecha[i]), variables = dias$variables[[i]])
    })
  }
  vid <- as.integer(vid)
  if (is.na(vid)) return(NULL)
  fetched_at <- (pronostico_lite$meta %||% list())$fetched_at
  fechas <- character(0)
  valores <- numeric(0)
  for (d in dias) {
    if (is.null(d$variables)) next
    for (v in d$variables) {
      if (as.integer(v$id_variable %||% NA) != vid) next
      fechas <- c(fechas, as.character(d$fecha))
      valores <- c(valores, suppressWarnings(as.numeric(v$valor)))
      break
    }
  }
  if (length(fechas) == 0L) return(NULL)
  data.frame(
    fecha_objetivo = fechas,
    valor = valores,
    fetched_at = fetched_at,
    stringsAsFactors = FALSE
  )
}

reporte_fetch_pronostico_celdas <- function(eid, vid) {
  pron_path <- file.path("/app", "utils", "pronostico_payload.R")
  if (file.exists(pron_path) && !exists("pronostico_tabla_existe", mode = "function")) {
    source(pron_path, local = FALSE)
  }
  if (!exists("pronostico_tabla_existe", mode = "function") || !pronostico_tabla_existe()) {
    return(NULL)
  }
  if (!exists("pronostico_estacion_visible", mode = "function") ||
      !pronostico_estacion_visible(eid)) {
    return(NULL)
  }
  eid <- as.integer(eid)
  vid <- as.integer(vid)
  if (is.na(eid) || is.na(vid)) return(NULL)
  rows <- db_query(
    "SELECT fecha_objetivo, valor, fetched_at
     FROM carta_del_mirador
     WHERE id_estacion = $1 AND id_variable = $2
       AND fecha_objetivo >= (CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date
     ORDER BY fecha_objetivo",
    params = list(eid, vid)
  )
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  rows
}

# Variables del reporte (sin COUNT). Compartido por shell y full.
reporte_fetch_variables_sv <- function(eid) {
  cache_or_compute(cache_key("estacion:variables:reporte", eid), function() {
    db_query("
      SELECT v.id, v.nombre, v.unidad,
        MIN(o.fecha) AS fecha_min, MAX(o.fecha) AS fecha_max
      FROM magnitud v
      JOIN bitacora_oficial o ON o.id_variable = v.id AND o.id_estacion = $1
      GROUP BY v.id, v.nombre, v.unidad", params = list(eid))
  }, ttl = 1800)
}

# Shell usable: estacion + chips + pronostico + ultimas (30d). Sin serie completa.
reporte_api_build_shell_payload <- function(eid, vid) {
  reporte_source_shiny()
  eid <- as.integer(eid)
  vid <- as.integer(vid)
  if (is.na(eid) || is.na(vid)) return(list(error = "id inv\u00e1lido"))

  info_df <- db_query(
    "SELECT id, nombre, lat, lon, altura, nom_reg, nom_cuen AS cuenca, nom_com, id_fuente FROM mirador WHERE id = $1",
    params = list(eid)
  )
  if (nrow(info_df) == 0) return(list(error = "Estaci\u00f3n no encontrada"))
  info <- as.list(info_df[1, , drop = FALSE])

  sv <- reporte_fetch_variables_sv(eid)
  if (!exists("ec_label_variables_df", mode = "function")) {
    ec_label_variables_df <- function(df) df
  }
  sv <- ec_label_variables_df(sv)
  if (nrow(sv) == 0 || !vid %in% sv$id) {
    return(list(error = "Variable no disponible en esta estaci\u00f3n"))
  }

  abr_row <- db_query(
    "SELECT COALESCE(nombre_abr, '') AS nombre_abr, COALESCE(nombre, '') AS nombre,
            COALESCE(unidad, '') AS unidad FROM magnitud WHERE id = $1",
    params = list(vid)
  )
  nombre_abr <- if (nrow(abr_row) > 0) abr_row$nombre_abr[1] else ""
  variable_nombre_db <- if (nrow(abr_row) > 0) abr_row$nombre[1] else ""
  unidad <- display_unidad(nombre_abr, if (nrow(abr_row) > 0) abr_row$unidad[1] else "")
  vtype <- if (exists("reporte_detect_var_type", mode = "function")) {
    reporte_detect_var_type(paste(nombre_abr, variable_nombre_db))
  } else {
    "generic"
  }
  variable_display <- if (exists("ec_variable_label", mode = "function")) {
    ec_variable_label(variable_nombre_db)
  } else {
    as.character(variable_nombre_db)
  }

  row_sv <- sv[sv$id == vid, , drop = FALSE]
  periodo_str <- ""
  if (nrow(row_sv) > 0) {
    y0 <- suppressWarnings(as.integer(substr(as.character(row_sv$fecha_min[1]), 1, 4)))
    y1 <- suppressWarnings(as.integer(substr(as.character(row_sv$fecha_max[1]), 1, 4)))
    if (!is.na(y0) && !is.na(y1)) periodo_str <- paste0(y0, "\u2013", y1)
  }

  alt_str <- if (!is.null(info$altura) && !is.na(info$altura) && nzchar(as.character(info$altura))) {
    paste0(info$altura, " m.s.n.m.")
  } else ""
  cuenca_str <- if (!is.null(info$cuenca) && !is.na(info$cuenca) && nzchar(as.character(info$cuenca))) {
    as.character(info$cuenca)
  } else ""
  region_str <- if (!is.null(info$nom_reg) && !is.na(info$nom_reg) && nzchar(as.character(info$nom_reg))) {
    as.character(info$nom_reg)
  } else ""
  meta_geo <- paste(Filter(nzchar, c(alt_str, cuenca_str, region_str)), collapse = " \u00b7 ")

  var_chips <- list()
  sv_ok <- sv[!is.na(sv$id), , drop = FALSE]
  for (i in seq_len(min(6L, nrow(sv_ok)))) {
    var_chips[[i]] <- list(
      id = sv_ok$id[i],
      nombre = if (exists("ec_variable_label", mode = "function")) {
        ec_variable_label(as.character(sv_ok$nombre[i]), id = sv_ok$id[i])
      } else {
        as.character(sv_ok$nombre[i])
      },
      active = sv_ok$id[i] == vid
    )
  }

  # Ultimas: solo ~30d (sin ranking historico completo; el full lo corrige).
  fecha_min <- format(reporte_hoy_cl() - 30L, "%Y-%m-%d")
  df_u <- tryCatch({
    db_query(
      "SELECT fecha, valor FROM jornada_cerrada
       WHERE id_estacion = $1 AND id_variable = $2 AND fecha >= $3::date
       ORDER BY fecha",
      params = list(eid, vid, fecha_min)
    )
  }, error = function(e) NULL)
  if (!is.null(df_u) && nrow(df_u) > 0) {
    df_u$fecha <- as.Date(df_u$fecha)
    df_u$valor <- suppressWarnings(as.numeric(df_u$valor))
    df_u <- reporte_merge_cola_final(df_u, reporte_fetch_cola_final(eid, vid))
    df_u <- df_display_valores(df_u, nombre_abr)
  }
  last_date <- if (!is.null(df_u) && nrow(df_u) > 0) max(df_u$fecha, na.rm = TRUE) else reporte_hoy_cl()
  ultimas <- if (exists("reporte_build_ultimas_observaciones", mode = "function") &&
                 !is.null(df_u) && nrow(df_u) > 0) {
    reporte_build_ultimas_observaciones(df_u, last_date, unidad, variable_display, vtype)
  } else {
    NULL
  }

  calidad <- tryCatch({
    if (exists("calidad_estacion_payload", mode = "function")) calidad_estacion_payload(eid) else NULL
  }, error = function(e) NULL)

  pron_path <- file.path("/app", "utils", "pronostico_payload.R")
  if (file.exists(pron_path) && !exists("pronostico_build_lite_payload", mode = "function")) {
    source(pron_path, local = FALSE)
  }
  pronostico_lite <- if (exists("pronostico_build_lite_payload", mode = "function")) {
    cache_or_compute(
      cache_key("pronostico:lite", eid),
      function() pronostico_build_lite_payload(eid),
      ttl = 3600L
    )
  } else {
    NULL
  }
  pronostico_lite <- reporte_filtrar_lite_desde_hoy(pronostico_lite)

  view <- if (exists("estacion_nombre_view", mode = "function")) {
    estacion_nombre_view(info$nombre, info$nom_com, info$id_fuente)
  } else {
    NULL
  }
  est_nombre <- if (!is.null(view) && nzchar(view$label %||% "")) view$label else as.character(info$nombre %||% "")

  payload <- list(
    shell = TRUE,
    station = list(
      name = est_nombre,
      period = periodo_str,
      metaGeo = meta_geo,
      altitude = alt_str,
      cuenca = cuenca_str,
      region = region_str,
      variable = variable_display,
      variables = var_chips,
      fuente = if (!is.null(view)) view$fuente else "",
      eid = eid,
      vid = vid
    ),
    insightGlobal = "",
    widgets = list(),
    normalClimatology = NULL,
    chart = NULL,
    cicloAnual = NULL,
    tablaAnio = NULL,
    ultimasObservaciones = ultimas,
    nFlaggedExcluded = 0L
  )
  if (!is.null(calidad)) {
    payload$station$calidadNivel <- calidad$nivel %||% NULL
    payload$station$calidadScore <- calidad$score_calidad %||% NULL
    payload$station$nImposibles <- calidad$n_imposibles %||% NULL
    payload$station$pctImposibles <- calidad$pct_imposibles %||% NULL
  }
  if (!is.null(pronostico_lite) && is.list(pronostico_lite) && length(pronostico_lite$dias %||% list()) > 0L) {
    payload$pronosticoLite <- tryCatch({
      if (exists("pronostico_lite_day_list", mode = "function") && is.data.frame(pronostico_lite$dias)) {
        pronostico_lite$dias <- pronostico_lite_day_list(pronostico_lite$dias)
      }
      pronostico_lite
    }, error = function(e) NULL)
  }
  payload
}

reporte_api_build_payload <- function(eid, vid) {
  reporte_source_shiny()
  eid <- as.integer(eid)
  vid <- as.integer(vid)
  if (is.na(eid) || is.na(vid)) return(list(error = "id inv\u00e1lido"))

  info_df <- db_query(
    "SELECT id, nombre, lat, lon, altura, nom_reg, nom_cuen AS cuenca, nom_com, id_fuente FROM mirador WHERE id = $1",
    params = list(eid)
  )
  if (nrow(info_df) == 0) return(list(error = "Estaci\u00f3n no encontrada"))
  info <- as.list(info_df[1, , drop = FALSE])

  # Clave propia: /estaciones/<id>/variables cachea bajo estacion:variables una
  # forma distinta (con nombre_abr y registros), y compartir la clave hacia que
  # el segundo en llegar leyera columnas que su consulta no pidio.
  # El reporte solo usa id, nombre, unidad y fecha_min; el COUNT sobre
  # bitacora_oficial costaba 1,2 s (9 s en frio) para un campo que nadie lee.
  sv <- reporte_fetch_variables_sv(eid)

  if (!exists("ec_label_variables_df", mode = "function")) {
    ec_label_variables_df <- function(df) df
  }
  sv <- ec_label_variables_df(sv)

  if (nrow(sv) == 0 || !vid %in% sv$id) {
    return(list(error = "Variable no disponible en esta estaci\u00f3n"))
  }

  rd <- reporte_fetch_series_for_report(eid, vid, sv, info)
  if (is.null(rd)) return(list(error = "Sin datos para la serie"))

  abr_row <- db_query(
    "SELECT COALESCE(nombre_abr, '') AS nombre_abr FROM magnitud WHERE id = $1",
    params = list(vid)
  )
  nombre_abr <- if (nrow(abr_row) > 0) abr_row$nombre_abr[1] else ""
  rd$df <- df_display_valores(rd$df, nombre_abr)
  rd$meta$unidad <- display_unidad(nombre_abr, rd$meta$unidad %||% "")
  rd$meta$nombre_abr <- nombre_abr

  calidad <- tryCatch({
    if (exists("calidad_estacion_payload", mode = "function")) {
      calidad_estacion_payload(eid)
    } else {
      mv <- mv_estacion_calidad_nombre()
      if (is.null(mv)) NULL else {
        cal <- db_query(paste0(
          "SELECT nivel, score_calidad, pct_imposibles, n_imposibles, pct_completitud ",
          "FROM ", mv, " WHERE id_estacion = $1"
        ), params = list(eid))
        if (nrow(cal) == 0) NULL else as.list(cal[1, , drop = FALSE])
      }
    }
  }, error = function(e) NULL)

  variable_nombre_db <- if (!is.null(rd$meta$variable_nombre)) rd$meta$variable_nombre else ""

  pron_path <- file.path("/app", "utils", "pronostico_payload.R")
  if (file.exists(pron_path) && !exists("pronostico_build_lite_payload", mode = "function")) {
    source(pron_path, local = FALSE)
  }
  pronostico_lite <- if (exists("pronostico_build_lite_payload", mode = "function")) {
    cache_or_compute(
      cache_key("pronostico:lite", eid),
      function() pronostico_build_lite_payload(eid),
      ttl = 3600L
    )
  } else {
    NULL
  }
  # El lite dura 1 h y puede haber cruzado la medianoche con el dia de ayer adentro.
  pronostico_lite <- reporte_filtrar_lite_desde_hoy(pronostico_lite)

  df_pron <- tryCatch(
    reporte_df_pronostico_from_lite(pronostico_lite, vid),
    error = function(e) NULL
  )
  if (is.null(df_pron)) {
    df_pron <- reporte_fetch_pronostico_celdas(eid, vid)
    if (!is.null(df_pron) && nrow(df_pron) > 0 && is_wind_abr(nombre_abr)) {
      df_pron$valor <- display_valor(df_pron$valor, nombre_abr)
    }
  }

  payload <- reporte_build_report_payload(
    rd$df, rd$meta, info, sv, vid, variable_nombre_db,
    eid = eid, df_pronostico = df_pron
  )
  if (is.null(payload)) return(list(error = "Datos insuficientes para reporte"))
  if (!is.null(calidad)) {
    payload$station$calidadNivel <- calidad$nivel %||% NULL
    payload$station$calidadScore <- calidad$score_calidad %||% NULL
    payload$station$nImposibles <- calidad$n_imposibles %||% NULL
    payload$station$pctImposibles <- calidad$pct_imposibles %||% NULL
  }
  view <- if (exists("estacion_nombre_view", mode = "function")) {
    estacion_nombre_view(info$nombre, info$nom_com, info$id_fuente)
  } else {
    NULL
  }
  if (!is.null(view) && nzchar(view$label %||% "")) payload$station$name <- view$label
  payload$station$fuente <- if (!is.null(view)) view$fuente else ""
  payload$station$eid <- eid
  payload$station$vid <- vid
  payload$station$variables <- reporte_attach_chip_sparks(eid, payload$station$variables)
  payload$widgets <- lapply(payload$widgets, reporte_jsonify_labels)
  if (!is.null(pronostico_lite) && is.list(pronostico_lite) && length(pronostico_lite$dias %||% list()) > 0L) {
    payload$pronosticoLite <- tryCatch({
      if (exists("pronostico_lite_day_list", mode = "function") && is.data.frame(pronostico_lite$dias)) {
        pronostico_lite$dias <- pronostico_lite_day_list(pronostico_lite$dias)
      }
      pronostico_lite
    }, error = function(e) NULL)
  }
  payload
}

# Sparklines 14d para chips de variable (aditivo; no altera rankings).
reporte_fetch_chip_sparks <- function(eid, vids) {
  eid <- as.integer(eid)[1]
  vids <- unique(as.integer(vids))
  vids <- vids[!is.na(vids) & vids > 0L]
  out <- stats::setNames(vector("list", length(vids)), as.character(vids))
  if (is.na(eid) || eid < 1L || length(vids) == 0L) return(out)
  fecha_min <- format(reporte_hoy_cl() - 14L, "%Y-%m-%d")
  in_list <- paste(vids, collapse = ",")
  df <- tryCatch({
    db_query(
      paste0(
        "SELECT id_variable, fecha, valor FROM jornada_cerrada ",
        "WHERE id_estacion = $1 AND fecha >= $2::date ",
        "AND id_variable IN (", in_list, ") ",
        "ORDER BY id_variable, fecha"
      ),
      params = list(eid, fecha_min)
    )
  }, error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(out)
  df$valor <- suppressWarnings(as.numeric(df$valor))
  split_vals <- split(df$valor, as.character(as.integer(df$id_variable)))
  for (k in names(split_vals)) {
    v <- split_vals[[k]]
    v <- v[is.finite(v)]
    if (length(v) > 0L) out[[k]] <- as.numeric(v)
  }
  out
}

reporte_attach_chip_sparks <- function(eid, vars) {
  if (is.null(vars) || length(vars) == 0) return(vars)
  if (is.data.frame(vars)) {
    vars <- lapply(seq_len(nrow(vars)), function(i) as.list(vars[i, , drop = FALSE]))
  }
  vids <- vapply(vars, function(v) {
    suppressWarnings(as.integer(v$id %||% NA_integer_))
  }, integer(1))
  sparks <- reporte_fetch_chip_sparks(eid, vids)
  lapply(vars, function(v) {
    key <- as.character(as.integer(v$id %||% NA_integer_))
    sp <- sparks[[key]]
    if (!is.null(sp) && length(sp) >= 2L) v$spark <- as.numeric(sp)
    v
  })
}

calidad_estacion_payload <- function(eid) {
  mv <- mv_estacion_calidad_nombre()
  if (is.null(mv)) return(NULL)
  cal <- db_query(paste0(
    "SELECT nivel, score_calidad, pct_imposibles, n_imposibles, pct_completitud ",
    "FROM ", mv, " WHERE id_estacion = $1"
  ), params = list(eid))
  if (nrow(cal) == 0) return(NULL)
  as.list(cal[1, , drop = FALSE])
}
