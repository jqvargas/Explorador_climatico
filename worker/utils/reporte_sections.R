# Generadores Markdown por capitulo del reporte PDF.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

if (!exists("REPORTE_DOC_VERSION")) REPORTE_DOC_VERSION <- "1.0"

reporte_md_yaml <- function(title, subtitle = "", tags = character(), date = NULL,
                            version = NULL, extra = list()) {
  date <- date %||% format(Sys.Date(), "%Y-%m-%d")
  version <- version %||% reporte_doc_version_string(date)
  tag_line <- if (length(tags) > 0) paste0("\ntags: \"", paste(tags, collapse = ", "), "\"") else ""
  extra_lines <- vapply(names(extra), function(k) {
    val <- extra[[k]]
    if (is.null(val) || (length(val) == 1 && is.na(val))) return("")
    paste0(k, ": \"", gsub('"', '\\"', as.character(val)), "\"")
  }, character(1))
  extra_lines <- extra_lines[nzchar(extra_lines)]
  extra_block <- if (length(extra_lines)) paste0("\n", paste(extra_lines, collapse = "\n")) else ""
  paste0(
    "---\n",
    "title: \"", gsub('"', '\\"', title), "\"\n",
    "subtitle: \"", gsub('"', '\\"', subtitle), "\"\n",
    "institution: \"rodaja.cl\"\n",
    "department: \"Explorador Climático\"\n",
    "date: \"", date, "\"\n",
    "version: \"", gsub('"', '\\"', version), "\"\n",
    "color: \"#DF9531\"", tag_line, extra_block, "\n",
    "---\n\n"
  )
}

reporte_fmt_num <- function(x, digits = 1) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  x <- suppressWarnings(as.numeric(x[1]))
  if (is.na(x)) return("\u2014")
  formatC(round(x, digits), format = "f", digits = digits,
          decimal.mark = ",", big.mark = ".")
}

reporte_fmt_or_explain <- function(value, reason, digits = 1) {
  if (!is.null(value) && length(value) > 0) {
    v <- suppressWarnings(as.numeric(value[1]))
    if (!is.na(v)) return(reporte_fmt_num(v, digits))
  }
  if (!is.null(reason) && nzchar(as.character(reason))) as.character(reason)
  else "\u2014"
}

reporte_sin_validar_bloque <- function(vs) {
  sv <- vs$sin_validar %||% list()
  if (is.na(sv$fecha %||% NA)) return("")
  paste0(
    "> **Dato sin validar:** ", sv$fecha, " = ", reporte_fmt_num(sv$valor),
    " (\u00faltimo registro disponible; excluido de extremos hist\u00f3ricos e IQR de referencia).\n\n"
  )
}

reporte_iqr_bloque <- function(vs) {
  iqr <- vs$iqr %||% list()
  if (!isTRUE(iqr$available)) return("")
  paste0(
    "> **Referencia IQR** (datos validados, n=", iqr$n, "): Q1=", reporte_fmt_num(iqr$q1),
    ", Q3=", reporte_fmt_num(iqr$q3), "; at\u00edpicos fuera de [",
    reporte_fmt_num(iqr$fence_lo), ", ", reporte_fmt_num(iqr$fence_hi), "].\n\n"
  )
}

reporte_var_icon_label <- function(nombre_abr, nombre) {
  n <- tolower(paste(nombre_abr, nombre))
  if (grepl("precip|lluvia|^pp$|^p$", n)) return("Precipitaci\u00f3n")
  if (grepl("temp.*max|tmax|t_max", n)) return("Temperatura m\u00e1xima")
  if (grepl("temp.*min|tmin|t_min", n)) return("Temperatura m\u00ednima")
  if (grepl("temp", n)) return("Temperatura")
  if (grepl("humedad|humidity|hr", n)) return("Humedad")
  if (grepl("u_10_kt|predomin", n)) return("Viento predominante")
  if (grepl("u_rafaga|viento_maximo_dmc", n)) return("Viento m\u00e1ximo DMC")
  if (grepl("u_max_diario|viento.*max", n)) return("Viento m\u00e1ximo")
  if (grepl("u2_min|viento.*min", n)) return("Viento m\u00ednimo")
  if (grepl("u2_mean|viento", n)) return("Viento medio")
  as.character(nombre %||% nombre_abr %||% "Variable")
}

reporte_strip_md_yaml <- function(md) {
  sub("(?s)\\A---\\s*\\r?\\n(.+?)\\r?\\n---\\s*\\r?\\n", "", md, perl = TRUE)
}

reporte_cover_extra <- function(meta, state, calidad = NULL, variables_df = NULL) {
  nm <- meta$nombre %||% ""
  periodo <- "\u2014"
  if (!is.null(variables_df) && nrow(variables_df) > 0) {
    periodo <- paste0(min(variables_df$fecha_min, na.rm = TRUE), " \u2013 ",
                      max(variables_df$fecha_max, na.rm = TRUE))
  } else if (!is.null(state$last_obs_date)) {
    periodo <- paste0("hasta ", state$last_obs_date)
  }

  kpis <- reporte_extract_kpis(state, calidad)
  pin <- list(map_pin = "false", map_fx = "50%", map_fy = "50%")
  if (!is.null(state$id_estacion)) {
    pin_path <- file.path(reporte_media_dir(state$id_estacion), "map_pin.json")
    if (file.exists(pin_path)) {
      pj <- tryCatch(jsonlite::fromJSON(pin_path, simplifyVector = TRUE), error = function(e) NULL)
      if (!is.null(pj) && isTRUE(pj$osm)) {
        fx <- suppressWarnings(as.numeric(pj$fx))
        fy <- suppressWarnings(as.numeric(pj$fy))
        if (!is.na(fx) && !is.na(fy)) {
          pin$map_pin <- "true"
          pin$map_fx <- paste0(round(100 * fx), "%")
          pin$map_fy <- paste0(round(100 * fy), "%")
        }
      }
    }
  }
  list(
    station_code = meta$codigo %||% "\u2014",
    station_lat = reporte_fmt_num(meta$lat, 4),
    station_lon = reporte_fmt_num(meta$lon, 4),
    station_region = meta$nom_reg %||% "\u2014",
    station_comuna = meta$nom_com %||% "\u2014",
    station_operador = meta$operador %||% "\u2014",
    station_calidad = paste0(calidad$nivel %||% "\u2014",
      if (!is.null(calidad)) paste0(" (", calidad$score_calidad %||% calidad$score %||% "", "/100)") else ""),
    station_periodo = periodo,
    kpi1_label = kpis[[1]]$label, kpi1_value = kpis[[1]]$value, kpi1_tone = kpis[[1]]$tone,
    kpi2_label = kpis[[2]]$label, kpi2_value = kpis[[2]]$value, kpi2_tone = kpis[[2]]$tone,
    kpi3_label = kpis[[3]]$label, kpi3_value = kpis[[3]]$value, kpi3_tone = kpis[[3]]$tone,
    kpi4_label = kpis[[4]]$label, kpi4_value = kpis[[4]]$value, kpi4_tone = kpis[[4]]$tone,
    has_map = if (file.exists(file.path(reporte_media_dir(state$id_estacion), "mapa_estacion.png"))) "true" else "false",
    map_pin = pin$map_pin, map_fx = pin$map_fx, map_fy = pin$map_fy
  )
}

reporte_extract_kpis <- function(state, calidad = NULL) {
  score <- if (!is.null(calidad)) calidad$score_calidad %||% calidad$score else NA
  kpi1 <- list(label = "Calidad", value = paste0(
    calidad$nivel %||% "\u2014",
    if (!is.na(score)) paste0(" (", score, "/100)") else ""
  ), tone = "green")

  trend_txt <- "Sin tendencia sig."
  trend_tone <- "neutral"
  pick_sig <- function(prefer_sum) {
    for (vk in names(state$variables %||% list())) {
      vs <- state$variables[[vk]]
      if (prefer_sum && (vs$agg %||% "") != "sum") next
      mk <- vs$mann_kendall %||% list()
      if (isTRUE(mk$available) && !is.na(mk$p) && mk$p < 0.05) {
        return(list(
          txt = if (!is.na(mk$tau) && mk$tau > 0) "Tendencia +" else "Tendencia \u2212",
          tone = if (!is.na(mk$tau) && mk$tau > 0) "brand" else "water"
        ))
      }
    }
    NULL
  }
  mk_hit <- pick_sig(TRUE)
  if (is.null(mk_hit)) mk_hit <- pick_sig(FALSE)
  if (!is.null(mk_hit)) {
    trend_txt <- mk_hit$txt
    trend_tone <- mk_hit$tone
  }
  kpi2 <- list(label = "Tendencia principal", value = trend_txt, tone = trend_tone)

  spi_txt <- "\u2014"
  spi_tone <- "neutral"
  for (vk in names(state$variables %||% list())) {
    vs <- state$variables[[vk]]
    if ((vs$agg %||% "") != "sum") next
    sv <- reporte_spi_current_value(vs)
    if (!is.na(sv)) {
      spi_txt <- reporte_fmt_num(sv, 2)
      spi_tone <- if (sv < -1) "water" else if (sv > 1) "green" else "neutral"
      break
    }
  }
  kpi3 <- list(label = "SPI mes actual", value = spi_txt, tone = spi_tone)

  ext_txt <- "\u2014"
  for (vk in names(state$variables %||% list())) {
    ext <- state$variables[[vk]]$extremes$max
    if (length(ext) > 0) {
      e <- ext[[1]]
      ext_txt <- paste0(reporte_fmt_num(e$valor), " (", e$fecha, ")")
      break
    }
  }
  kpi4 <- list(label = "Extremo hist\u00f3rico", value = ext_txt, tone = "brand")
  list(kpi1, kpi2, kpi3, kpi4)
}

reporte_section_portada <- function(meta, state, calidad = NULL, variables_df = NULL) {
  nm <- meta$nombre %||% paste0("Estaci\u00f3n ", state$id_estacion)
  extra <- reporte_cover_extra(meta, state, calidad, variables_df)
  paste0(
    reporte_md_yaml(
      title = nm,
      subtitle = "Reporte clim\u00e1tico integral",
      tags = c("rodaja.cl", calidad$nivel %||% "excelente"),
      date = format(Sys.time(), "%Y-%m-%d"),
      extra = extra
    ),
    "<!-- portada renderizada en Typst -->\n"
  )
}

reporte_section_resumen <- function(meta, state, calidad = NULL, body_only = FALSE) {
  nm <- meta$nombre %||% ""
  yaml <- if (!body_only) reporte_md_yaml("Resumen ejecutivo", nm) else ""
  lines <- c(
    yaml,
    "# Resumen ejecutivo\n\n",
    "> S\u00edntesis interpretativa del estado clim\u00e1tico reciente de la estaci\u00f3n.\n\n"
  )
  for (vk in names(state$variables %||% list())) {
    vs <- state$variables[[vk]]
    lines <- c(lines,
      paste0("## ", reporte_var_icon_label(vs$nombre_abr, vs$nombre), "\n\n"),
      paste0("> ", reporte_interp_resumen_var(vs), "\n\n"),
      reporte_sin_validar_bloque(vs)
    )
  }
  paste0(lines, collapse = "")
}

reporte_section_ficha <- function(meta, variables_df, body_only = FALSE) {
  nm <- meta$nombre %||% ""
  yaml <- if (!body_only) reporte_md_yaml("Ficha t\u00e9cnica", nm) else ""
  body <- paste0(
    yaml,
    "# Ficha t\u00e9cnica\n\n",
    "| Campo | Valor |\n|-------|-------|\n",
    "| Nombre | ", nm, " |\n",
    "| C\u00f3digo | ", meta$codigo %||% "\u2014", " |\n",
    "| Operador | ", meta$operador %||% "\u2014", " |\n",
    "| Regi\u00f3n | ", meta$nom_reg %||% "\u2014", " |\n",
    "| Comuna | ", meta$nom_com %||% "\u2014", " |\n",
    "| Latitud | ", reporte_fmt_num(meta$lat, 4), " |\n",
    "| Longitud | ", reporte_fmt_num(meta$lon, 4), " |\n",
    "| Altitud | ", meta$altura %||% "\u2014", " m s.n.m. |\n\n",
    "## Variables disponibles\n\n",
    "| Variable | Registros | Desde | Hasta |\n|----------|-----------|-------|-------|\n"
  )
  if (!is.null(variables_df) && nrow(variables_df) > 0) {
    for (i in seq_len(nrow(variables_df))) {
      body <- paste0(body,
        "| ", variables_df$nombre[i], " | ", variables_df$n_registros[i],
        " | ", variables_df$fecha_min[i], " | ", variables_df$fecha_max[i], " |\n")
    }
  }
  body
}

reporte_section_calidad <- function(meta, calidad, body_only = FALSE) {
  nm <- meta$nombre %||% ""
  yaml <- if (!body_only) reporte_md_yaml("Calidad de datos", nm) else ""
  if (is.null(calidad)) {
    return(paste0(yaml, "# Calidad de datos\n\nSin datos de calidad.\n"))
  }
  paste0(
    yaml,
    "# Calidad de datos\n\n",
    "| Indicador | Valor |\n|-----------|-------|\n",
    "| Nivel | **", calidad$nivel %||% "\u2014", "** |\n",
    "| Score | ", calidad$score_calidad %||% calidad$score %||% "\u2014", "/100 |\n",
    "| Completitud | ", reporte_fmt_num(calidad$pct_completitud, 1), "% |\n",
    "| Continuidad | ", reporte_fmt_num({
      pc <- suppressWarnings(as.numeric(calidad$pct_continuidad))
      if (is.na(pc)) NA else if (pc <= 1) pc * 100 else pc
    }, 1), "% |\n",
    "| A\u00f1os de historia | ", calidad$anios_span %||% "\u2014", " |\n",
    "| Variables | ", calidad$n_variables %||% "\u2014", " |\n\n",
    "> Solo estaciones con calidad **excelente** tienen reporte PDF disponible.\n"
  )
}

reporte_clim_pct <- function(pct, m, p) {
  if (is.null(pct)) return(NA_real_)
  if (is.data.frame(pct) || is.matrix(pct)) {
    if (p %in% colnames(pct)) return(suppressWarnings(as.numeric(pct[m, p])))
    v <- suppressWarnings(as.numeric(pct[m, match(p, c("p10", "p50", "p90"))]))
    return(v)
  }
  if (is.list(pct)) {
    row <- pct[[m]] %||% pct[[as.character(m)]]
    if (is.null(row)) return(NA_real_)
    return(suppressWarnings(as.numeric(row[[p]] %||% row[p])))
  }
  NA_real_
}

reporte_section_ciclo <- function(meta, state, body_only = FALSE) {
  nm <- meta$nombre %||% ""
  meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  yaml <- if (!body_only) reporte_md_yaml("Ciclo estacional", nm) else ""
  body <- paste0(yaml, "# Ciclo estacional\n\n",
    "Comparaci\u00f3n del ciclo anual con bandas de referencia hist\u00f3rica (P10\u2013P90).\n\n")
  for (vk in names(state$variables %||% list())) {
    vs <- state$variables[[vk]]
    vid <- vs$id_variable
    chart <- reporte_chart_media_ref(vid, "ciclo")
    body <- paste0(body, "## ", reporte_var_icon_label(vs$nombre_abr, vs$nombre), "\n\n")
    eid <- state$id_estacion
    if (!is.null(eid) && file.exists(file.path(reporte_media_dir(eid), basename(chart)))) {
      body <- paste0(body, "![Ciclo estacional](", chart, ")\n\n")
    }
    body <- paste0(body, "### Detalle tabular\n\n",
      "| Mes | P10 | P50 | P90 | Media |\n|-----|-----|-----|-----|-------|\n")
    pct <- vs$climatology_pct
    for (m in 1:12) {
      acc <- vs$monthly[[m]] %||% list(mean = NA)
      body <- paste0(body, "| ", meses[m], " | ",
        reporte_fmt_or_explain(reporte_clim_pct(pct, m, "p10"), "Sin percentiles"), " | ",
        reporte_fmt_or_explain(reporte_clim_pct(pct, m, "p50"), "Sin percentiles"), " | ",
        reporte_fmt_or_explain(reporte_clim_pct(pct, m, "p90"), "Sin percentiles"), " | ",
        reporte_fmt_num(acc$mean), " |\n")
    }
    body <- paste0(body, "\n")
  }
  body
}

reporte_section_tendencias <- function(meta, state, body_only = FALSE) {
  nm <- meta$nombre %||% ""
  yaml <- if (!body_only) reporte_md_yaml("Tendencias", nm) else ""
  body <- paste0(yaml, "# Tendencias\n\n",
    "An\u00e1lisis Mann-Kendall y pendiente de Sen sobre series anuales. ",
    "Las bandas IQR en los gr\u00e1ficos marcan el rango intercuartil y los l\u00edmites 1,5\u00d7IQR ",
    "para identificar a\u00f1os at\u00edpicos; el \u00faltimo a\u00f1o puede incluir el dato sin validar (marcado en naranja).\n\n")
  for (vk in names(state$variables %||% list())) {
    vs <- state$variables[[vk]]
    mk <- vs$mann_kendall %||% list()
    vid <- vs$id_variable
    body <- paste0(body, "## ", reporte_var_icon_label(vs$nombre_abr, vs$nombre), "\n\n",
      "> ", reporte_interp_tendencia(vs$nombre, mk, vs$agg %||% "mean", vs$nombre_abr %||% ""), "\n\n",
      reporte_sin_validar_bloque(vs), reporte_iqr_bloque(vs))
    chart <- reporte_chart_media_ref(vid, "tendencia")
    eid <- state$id_estacion
    if (!is.null(eid) && file.exists(file.path(reporte_media_dir(eid), basename(chart)))) {
      body <- paste0(body, "![Tendencia anual](", chart, ")\n\n")
    }
    body <- paste0(body,
      "| Estad\u00edstico | Valor |\n|-------------|-------|\n",
      "| \u03c4 de Mann-Kendall | ", reporte_fmt_or_explain(mk$tau, mk$reason, 4), " |\n",
      "| p-valor | ", reporte_fmt_or_explain(mk$p, mk$reason, 4), " |\n",
      "| Pendiente Sen | ", reporte_fmt_or_explain(mk$sen_slope, mk$reason, 4), " /a\u00f1o |\n\n")
  }
  body
}

reporte_section_extremos <- function(meta, state, body_only = FALSE) {
  nm <- meta$nombre %||% ""
  yaml <- if (!body_only) reporte_md_yaml("Eventos extremos", nm) else ""
  body <- paste0(yaml, "# Eventos extremos hist\u00f3ricos\n\n",
    "Los rankings excluyen el \u00faltimo d\u00eda registrado (dato sin validar). ",
    "En los gr\u00e1ficos de serie diaria, la banda verde muestra el IQR (Q1\u2013Q3), ",
    "las l\u00edneas punteadas los l\u00edmites 1,5\u00d7IQR, los puntos rojos valores at\u00edpicos ",
    "y el diamante naranja el \u00faltimo dato sin validar.\n\n")
  for (vk in names(state$variables %||% list())) {
    vs <- state$variables[[vk]]
    ext <- vs$extremes %||% list(max = list(), min = list(), streaks = list())
    vid <- vs$id_variable
    body <- paste0(body, "## ", reporte_var_icon_label(vs$nombre_abr, vs$nombre), "\n\n",
      "> ", reporte_interp_extremos(vs$nombre, ext, vs$agg %||% "mean"), "\n\n",
      reporte_sin_validar_bloque(vs), reporte_iqr_bloque(vs))
    chart <- reporte_chart_media_ref(vid, "extremos")
    eid <- state$id_estacion
    if (!is.null(eid) && file.exists(file.path(reporte_media_dir(eid), basename(chart)))) {
      body <- paste0(body, "![Eventos extremos](", chart, ")\n\n")
    }
    body <- paste0(body, "### M\u00e1ximos hist\u00f3ricos\n\n",
      "| # | Fecha | Valor |\n|---|-------|-------|\n")
    for (j in seq_along(ext$max)) {
      e <- ext$max[[j]]
      body <- paste0(body, "| ", j, " | ", e$fecha, " | ", reporte_fmt_num(e$valor), " |\n")
    }
    sv <- vs$sin_validar %||% list()
    if (!is.na(sv$fecha %||% NA)) {
      body <- paste0(body, "\n### \u00daltimo dato (sin validar)\n\n",
        "| Fecha | Valor |\n|-------|-------|\n| ", sv$fecha, " | ",
        reporte_fmt_num(sv$valor), " |\n")
    }
    if (identical(vs$agg, "sum")) {
      st <- ext$streaks %||% list()
      body <- paste0(body, "\n### Rachas de precipitaci\u00f3n\n\n",
        "| M\u00e9trica | Valor | Per\u00edodo |\n|---------|-------|--------|\n",
        "| Racha seca m\u00e1xima | ", st$max_dry_streak %||% 0, " d\u00edas | ",
        st$max_dry_start %||% "\u2014", " \u2013 ", st$max_dry_end %||% "\u2014", " |\n",
        "| Racha h\u00fameda m\u00e1xima | ", st$max_wet_streak %||% 0, " d\u00edas | ",
        st$max_wet_start %||% "\u2014", " \u2013 ", st$max_wet_end %||% "\u2014", " |\n")
      if (identical(as.character(vs$nombre_abr %||% ""), "pp") &&
          exists("senales_curvas_md", mode = "function")) {
        body <- paste0(body, "\n", senales_curvas_md(vs$curvas_retorno))
      }
    } else {
      body <- paste0(body, "\n### M\u00ednimos hist\u00f3ricos\n\n",
        "| # | Fecha | Valor |\n|---|-------|-------|\n")
      for (j in seq_along(ext$min)) {
        e <- ext$min[[j]]
        body <- paste0(body, "| ", j, " | ", e$fecha, " | ", reporte_fmt_num(e$valor), " |\n")
      }
    }
    body <- paste0(body, "\n")
  }
  body
}

reporte_section_spi_spei <- function(meta, state, body_only = FALSE) {
  nm <- meta$nombre %||% ""
  yaml <- if (!body_only) reporte_md_yaml("SPI / sequ\u00eda", nm) else ""
  body <- paste0(yaml,
    "# \u00cdndices de sequ\u00eda (SPI)\n\n",
    "Par\u00e1metros de distribuci\u00f3n ajustados sobre totales mensuales hist\u00f3ricos.\n\n")
  for (vk in names(state$variables %||% list())) {
    vs <- state$variables[[vk]]
    if ((vs$agg %||% "") != "sum") next
    spi_meta <- vs$availability$spi %||% vs$spi
    spi_v <- reporte_spi_current_value(vs)
    body <- paste0(body, "## ", reporte_var_icon_label(vs$nombre_abr, vs$nombre), "\n\n",
      "> ", reporte_interp_spi(vs$nombre, spi_v, spi_meta), "\n\n",
      "- SPI mes en curso: **", reporte_fmt_or_explain(spi_v, spi_meta$reason, 2), "**\n",
      "- \u00daltimo re-ajuste: ", (vs$spi %||% list())$last_fit %||% "\u2014", "\n\n")
  }
  body
}

reporte_section_pronostico <- function(meta, pron_df, body_only = FALSE) {
  nm <- meta$nombre %||% ""
  n_dias <- if (!is.null(pron_df) && nrow(pron_df) > 0) {
    length(unique(pron_df$fecha_objetivo))
  } else {
    0L
  }
  titulo <- if (n_dias > 0L) {
    paste0("Pron\u00f3stico \u2014 pr\u00f3ximos ", n_dias, " d\u00edas")
  } else {
    "Pron\u00f3stico"
  }
  yaml <- if (!body_only) reporte_md_yaml(titulo, nm) else ""
  body <- paste0(yaml, "# ", titulo, "\n\n")
  if (is.null(pron_df) || nrow(pron_df) == 0) {
    return(paste0(body, "Sin pron\u00f3stico disponible para esta estaci\u00f3n.\n"))
  }
  body <- paste0(body, "| Fecha | Variable | Valor | Unidad |\n|-------|----------|-------|--------|\n")
  for (i in seq_len(nrow(pron_df))) {
    body <- paste0(body, "| ", pron_df$fecha_objetivo[i], " | ",
      pron_df$variable[i], " | ", reporte_fmt_num(pron_df$valor[i]),
      " | ", pron_df$unidad[i] %||% "", " |\n")
  }
  body
}

reporte_section_enso <- function(meta, state, body_only = FALSE) {
  nm <- meta$nombre %||% ""
  ref_n <- reporte_max_enso_n(state)
  yaml <- if (!body_only) reporte_md_yaml("Correlaci\u00f3n ENSO", nm) else ""
  body <- paste0(yaml, "# Correlaci\u00f3n con fases ENSO (ONI)\n\n")
  for (vk in names(state$variables %||% list())) {
    vs <- state$variables[[vk]]
    en <- vs$enso %||% list()
    body <- paste0(body, "## ", reporte_var_icon_label(vs$nombre_abr, vs$nombre), "\n\n",
      "> ", reporte_interp_enso(vs$nombre, en, ref_n), "\n\n",
      "| Estad\u00edstico | Valor |\n|-------------|-------|\n",
      "| Correlaci\u00f3n Pearson (ONI) | ", reporte_fmt_or_explain(en$r, en$reason, 3), " |\n",
      "| n (meses) | ", en$n %||% 0, " |\n\n")
  }
  body
}

reporte_section_metodologia <- function(meta, body_only = FALSE) {
  nm <- meta$nombre %||% ""
  yaml <- if (!body_only) reporte_md_yaml("Metodolog\u00eda", nm) else ""
  paste0(
    yaml,
    "# Metodolog\u00eda\n\n",
    "Este reporte fue generado por el pipeline pre-calculado de **rodaja.cl** (v", REPORTE_DOC_VERSION, ").\n\n",
    "- **Fase 1:** c\u00e1lculo hist\u00f3rico completo (percentiles, Mann-Kendall, SPI, extremos, ENSO).\n",
    "- **Fase 2:** actualizaci\u00f3n diaria incremental tras la ingesta observacional.\n",
    "- **Ciclo lento:** re-ajuste SPI y Mann-Kendall al cierre de mes/a\u00f1o.\n",
    "- **Dato sin validar:** el \u00faltimo d\u00eda con observaci\u00f3n se marca expl\u00edcitamente y se excluye ",
    "de extremos hist\u00f3ricos, percentiles estacionales e IQR de referencia en series de tiempo.\n\n",
    "Umbrales: tendencia \u2265 ", REPORTE_MIN_YEARS_MK, " a\u00f1os; SPI \u2265 ", REPORTE_MIN_MONTHS_SPI,
    " meses; ENSO \u2265 ", REPORTE_MIN_MONTHS_ENSO, " meses emparejados con ONI.\n\n",
    "Lluvia, duraci\u00f3n y per\u00edodo de retorno: m\u00e1ximo anual de la suma de 1 a 5 d\u00edas, ",
    "solo a\u00f1os con al menos 90 % de los d\u00edas observados. Ajuste GEV por momentos L. ",
    "Un per\u00edodo mayor que el doble de los a\u00f1os de registro se marca como extrapolaci\u00f3n. ",
    "Insumo cuantitativo, no reemplaza el criterio de un hidr\u00f3logo.\n\n",
    "Fuentes: DGA, DMC, INIA. Pron\u00f3stico: Open-Meteo. \u00cdndice ONI: NOAA.\n\n",
    "Solo estaciones con calidad **excelente** reciben actualizaciones.\n"
  )
}

REPORTE_ASSEMBLE_SECTIONS <- c(
  "01_resumen", "02_ficha_tecnica", "03_calidad", "04_ciclo_estacional", "05_tendencias",
  "06_extremos", "07_spi_spei", "08_pronostico", "09_enso", "10_metodologia"
)

reporte_assemble_document <- function(eid) {
  portada_path <- reporte_section_md_path(eid, "00_portada")
  if (!file.exists(portada_path)) stop("Falta portada MD para eid=", eid)
  portada_raw <- paste(readLines(portada_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  parts <- character(0)
  parts <- c(parts, portada_raw)
  for (sid in REPORTE_ASSEMBLE_SECTIONS) {
    p <- reporte_section_md_path(eid, sid)
    if (!file.exists(p)) next
    raw <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    parts <- c(parts, reporte_strip_md_yaml(raw))
  }
  out <- reporte_completo_md_path(eid)
  writeLines(paste(parts, collapse = "\n"), out, useBytes = TRUE)
  invisible(out)
}

reporte_write_all_sections <- function(eid, meta, state, calidad, pron_df,
                                         dirty_ids = NULL, variables_df = NULL) {
  vdf <- variables_df
  if (is.null(vdf) && !is.null(state$variables_df)) vdf <- state$variables_df
  state$id_estacion <- state$id_estacion %||% as.integer(eid)
  gens <- list(
    "00_portada" = function() reporte_section_portada(meta, state, calidad, vdf),
    "01_resumen" = function() reporte_section_resumen(meta, state, calidad),
    "02_ficha_tecnica" = function() reporte_section_ficha(meta, vdf),
    "03_calidad" = function() reporte_section_calidad(meta, calidad),
    "04_ciclo_estacional" = function() reporte_section_ciclo(meta, state),
    "05_tendencias" = function() reporte_section_tendencias(meta, state),
    "06_extremos" = function() reporte_section_extremos(meta, state),
    "07_spi_spei" = function() reporte_section_spi_spei(meta, state),
    "08_pronostico" = function() reporte_section_pronostico(meta, pron_df),
    "09_enso" = function() reporte_section_enso(meta, state),
    "10_metodologia" = function() reporte_section_metodologia(meta)
  )
  ids <- if (is.null(dirty_ids)) names(gens) else intersect(dirty_ids, names(gens))
  manifest <- reporte_manifest_get(eid)
  for (id in ids) {
    md <- gens[[id]]()
    path <- reporte_section_md_path(eid, id)
    writeLines(md, path, useBytes = TRUE)
    manifest$sections[[id]]$hash <- reporte_file_hash(path)
    manifest$sections[[id]]$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    manifest$sections[[id]]$dirty <- FALSE
  }
  reporte_save_manifest(eid, manifest)
  invisible(ids)
}
