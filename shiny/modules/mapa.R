#' @export
mapa_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "ec-map-shell",
      leaflet::leafletOutput(ns("mapa"), height = "100%"),
      shiny::tags$nav(
        id = "ec-choropleth-bc",
        class = "ec-choropleth-bc",
        `aria-label` = "Navegacion del mapa"
      ),
      shiny::tags$button(
        type = "button",
        id = "map_usar_geo",
        class = "ec-map-geo-btn leaflet-bar ec-map-legend-tip ec-map-control-tip",
        `data-tip` = "Ir a mi ubicaci\u00f3n.",
        `aria-label` = "Ir a mi ubicaci\u00f3n",
        onclick = "return ecRequestMapGeo();",
        shiny::tags$i(class = "fa fa-location-arrow", `aria-hidden` = "true")
      ),
      shiny::tags$div(
        id = "ec-icon-vars",
        class = "ec-icon-overlays",
        role = "radiogroup",
        `aria-label` = "Qu\u00e9 dato mostrar",
        shiny::tags$button(
          type = "button", class = "ec-icon-overlay", `data-var` = "pp1h",
          role = "radio", `aria-checked` = "false",
          `aria-label` = "Precipitaci\u00f3n 3 h",
          shiny::tags$span(
            class = "ec-icon-overlay-thumb is-pp1h", `aria-hidden` = "true",
            shiny::tags$img(src = "/icons/weather/lluvia.svg", alt = "")
          ),
          shiny::tags$span(class = "ec-icon-overlay-label", "Precipitaci\u00f3n")
        ),
        shiny::tags$button(
          type = "button", class = "ec-icon-overlay is-on", `data-var` = "t2m",
          role = "radio", `aria-checked` = "true",
          `aria-label` = "Temperatura",
          shiny::tags$span(
            class = "ec-icon-overlay-thumb is-t2m", `aria-hidden` = "true",
            shiny::tags$img(src = "/icons/weather/soleado.svg", alt = "")
          ),
          shiny::tags$span(class = "ec-icon-overlay-label", "Temperatura")
        ),
        shiny::tags$button(
          type = "button", class = "ec-icon-overlay", `data-var` = "u10m",
          role = "radio", `aria-checked` = "false",
          hidden = NA,
          `aria-label` = "Viento 10 m",
          shiny::tags$span(
            class = "ec-icon-overlay-thumb is-u10m", `aria-hidden` = "true",
            shiny::tags$img(src = "/icons/weather/viento.svg?v=wind2", alt = "")
          ),
          shiny::tags$span(class = "ec-icon-overlay-label", "Viento")
        )
      ),
      shiny::tags$aside(
        id = "ec-icon-legend",
        class = "ec-icon-legend",
        `aria-label` = "Escala de color",
        shiny::tags$span(id = "ec-icon-leg-unit", class = "ec-icon-leg-unit", "\u00b0C"),
        shiny::tags$div(
          class = "ec-icon-legend-track",
          shiny::tags$div(id = "ec-icon-legend-bar", class = "ec-icon-legend-bar"),
          shiny::tags$div(
            id = "ec-icon-legend-scale",
            class = "ec-icon-legend-scale",
            shiny::tags$span(id = "ec-icon-leg-min"),
            shiny::tags$span(id = "ec-icon-leg-mid"),
            shiny::tags$span(id = "ec-icon-leg-max")
          )
        )
      ),
      shiny::tags$aside(
        id = "ec-map-capa-icon",
        class = "ec-icon-dock",
        `aria-label` = "Fecha y hora del modelo",
        shiny::tags$p(id = "ec-icon-updated", class = "ec-icon-updated", hidden = NA),
        shiny::tags$div(
          class = "ec-icon-dock-row",
          shiny::tags$span(id = "ec-icon-time", class = "ec-icon-time", "\u2014"),
          shiny::tags$button(
            id = "ec-icon-play",
            type = "button",
            class = "ec-icon-play",
            title = "Reproducir horizonte de tiempo",
            `aria-label` = "Reproducir horizonte de tiempo",
            `aria-pressed` = "false",
            disabled = NA,
            shiny::tags$svg(
              viewBox = "0 0 16 16",
              `aria-hidden` = "true",
              focusable = "false",
              shiny::tags$path(fill = "currentColor", d = "M4 2.2v11.6L14 8z")
            )
          ),
          shiny::tags$div(
            class = "ec-icon-slider-wrap",
            shiny::tags$div(id = "ec-icon-days", class = "ec-icon-days", `aria-hidden` = "true"),
            shiny::tags$input(
              id = "ec-icon-slider",
              type = "range",
              min = "0",
              max = "0",
              value = "0",
              disabled = NA,
              `aria-label` = "Hora del pron\u00f3stico"
            )
          ),
          shiny::tags$div(
            class = "ec-icon-info-wrap",
            shiny::tags$button(
              id = "ec-icon-info",
              type = "button",
              class = "ec-icon-info",
              title = "Metodolog\u00eda del pron\u00f3stico",
              `aria-label` = "Metodolog\u00eda del pron\u00f3stico",
              `aria-expanded` = "false",
              `aria-controls` = "ec-icon-info-pop",
              shiny::tags$svg(
                viewBox = "0 0 16 16",
                `aria-hidden` = "true",
                focusable = "false",
                shiny::tags$circle(
                  cx = "8", cy = "8", r = "6.25", fill = "none",
                  stroke = "currentColor", `stroke-width` = "1.4"
                ),
                shiny::tags$path(
                  fill = "none", stroke = "currentColor", `stroke-width` = "1.4",
                  d = "M8 7.2v4M8 5.15h.01"
                )
              )
            ),
            shiny::tags$div(
              id = "ec-icon-info-pop",
              class = "ec-icon-info-pop",
              hidden = NA,
              role = "dialog",
              `aria-labelledby` = "ec-icon-info-title",
              shiny::tags$p(
                id = "ec-icon-info-title", class = "ec-icon-info-title",
                "Pron\u00f3stico en el mapa"
              ),
              shiny::tags$p(
                id = "ec-icon-info-res",
                "Capa espacial: modelo ICON del Deutscher Wetterdienst (DWD). Grilla nativa ~13 km; en el mapa, desagregaci\u00f3n topogr\u00e1fica ~5 km (no es un modelo nativo de 5 km)."
              ),
              shiny::tags$p("En estaciones: pron\u00f3stico puntual de Open-Meteo en las coordenadas de cada estaci\u00f3n."),
              shiny::tags$p("Actualizaci\u00f3n: dos veces al d\u00eda (corridas 00 y 12 UTC, cerca de las 00:20 y 12:20 hora de Chile). Los puntos de estaci\u00f3n se consultan al pintar el mapa."),
              shiny::tags$p(
                shiny::tags$a(
                  href = "/acerca/#pronostico",
                  target = "_blank",
                  rel = "noopener noreferrer",
                  "Metodolog\u00eda"
                )
              )
            )
          )
        )
      )
    ),
    shiny::div(style = "display:none;",
      shiny::numericInput(ns("ver_datos_mapa"), NULL, value = 0, min = 0)
    ),
    shiny::div(style = "display:none;",
      shiny::checkboxInput(ns("alerta_horizonte_ext"), NULL, value = FALSE)
    )
  )
}

# ============================================================
# Clasificacion de calidad de datos por estacion (4 niveles).
# Codificacion redundante (color + tamano + opacidad) para accesibilidad.
# ============================================================
EC_CALIDAD_NIVELES <- list(
  excelente = list(orden = 4L, etiqueta = "Excelente", color = "#1B7F4B",
                   radio = 10, opacidad = 0.95, weight = 2.4, forma = "estrella",
                   tip = "Datos al d\u00eda, alta completitud y continuidad (\u226520 a\u00f1os). Solo estas estaciones muestran alertas de pron\u00f3stico."),
  buena     = list(orden = 3L, etiqueta = "Buena",     color = "#378ADD",
                   radio = 8,  opacidad = 0.85, weight = 1.8, forma = "circulo",
                   tip = "Buena cobertura hist\u00f3rica y completitud. Adecuada para explorar y comparar series."),
  regular   = list(orden = 2L, etiqueta = "Regular",   color = "#EAB308",
                   radio = 6,  opacidad = 0.65, weight = 1.2, forma = "circulo",
                   tip = "Historia o completitud limitadas. Revisa el detalle de la estaci\u00f3n antes de analizar."),
  limitada  = list(orden = 1L, etiqueta = "Limitada",  color = "#DC2626",
                   radio = 4,  opacidad = 0.45, weight = 1.0, forma = "punto",
                   tip = "Pocos datos o calidad insuficiente. No se muestran alertas de evento extremo.")
)
EC_COBERTURA_NIVELES <- EC_CALIDAD_NIVELES
EC_USUARIO_GEO_COLOR <- "#9333EA"

EC_ALERTA_SEVERIDAD <- list(
  aviso_de_madrugada = list(etiqueta = "Alerta prevista (top 4\u201310)"),
  critica = list(etiqueta = "Cr\u00edtica (top 1\u20133)")
)
# Tamano unico en mapa (no codifica severidad). Leyenda usa el mismo SVG a escala menor.
EC_MAP_ALERTA_ICON_PX <- 28L
EC_MAP_ALERTA_LEYENDA_ICON_PX <- 20L
EC_MAP_ALERTA_HORIZONTE_CORTO <- 5L
EC_MAP_ALERTA_HORIZONTE_LARGO <- 16L

# Etiquetas cortas para civiles (leyenda del mapa).
# ponytail: chip wind apagado — 2 pares al gate no son cubo nacional (etapa 5).
# Bucket/cnt_wind siguen por si un dia hay masa; el chip no promete top-10.
EC_ALERTA_LEYENDA_TIPOS <- list(
  list(key = "pp", var = "pp", label = "Precipitaci\u00f3n intensa",
       tip = "Precipitaci\u00f3n pronosticada entre los d\u00edas m\u00e1s lluviosos del registro hist\u00f3rico (top 10)."),
  list(key = "cold", var = "t_min", direccion = "min", label = "Helada",
       tip = "Fr\u00edo extremo: temperatura prevista entre los d\u00edas m\u00e1s fr\u00edos de la historia (top 10)."),
  list(key = "heat", var = "t_max", direccion = "max", label = "Ola de calor",
       tip = "Calor extremo: temperatura prevista entre los d\u00edas m\u00e1s calurosos (top 10).")
)

EC_ALERTA_VAR_SYM <- list(
  pp = "rain",
  t_max = "thermo",
  t_min = "thermo",
  u2_mean = "wind",
  u_max_diario = "wind",
  u_10_kt = "wind",
  u_rafaga = "wind"
)
EC_ALERTA_VAR_LABELS <- list(
  pp = "Precipitaci\u00f3n",
  t_max = "Temperatura m\u00e1xima",
  t_min = "Temperatura m\u00ednima",
  u2_mean = "Viento medio",
  u_max_diario = "Viento m\u00e1ximo diario",
  u_10_kt = "Viento predominante",
  u_rafaga = "R\u00e1faga"
)

ec_alerta_sym_key <- function(var, direccion = NULL) {
  abr <- as.character(var)
  if (abr %in% c("t_max", "t_min")) {
    dir <- tolower(trimws(as.character(direccion %||% "max")))
    return(if (identical(dir, "min")) "ice" else "thermo")
  }
  EC_ALERTA_VAR_SYM[[abr]] %||% "alert"
}

# Termometro: color por direccion del extremo (frio vs calor), no por variable.
ec_alerta_var_color <- function(var_abr, direccion = NULL) {
  abr <- as.character(var_abr)
  if (abr %in% c("t_max", "t_min")) {
    dir <- tolower(trimws(as.character(direccion %||% "max")))
    return(if (identical(dir, "min")) "#4338CA" else "#DC2626")
  }
  switch(abr,
    u2_mean = "#475569",
    u_max_diario = "#475569",
    u_10_kt = "#475569",
    u_rafaga = "#475569",
    "#8A95A0"
  )
}

ec_alerta_valida <- function(a, anios_span = NULL, nivel = NULL) {
  if (is.null(a) || !is.list(a) || length(a) == 0L) return(FALSE)
  if (is.null(a$variable) || !nzchar(as.character(a$variable))) return(FALSE)
  niv <- as.character(nivel %||% "")
  if (nzchar(niv) && !identical(niv, "excelente")) return(FALSE)
  asp <- suppressWarnings(as.integer(anios_span))
  if (!is.null(anios_span) && !is.na(asp) && asp < 20L) return(FALSE)
  TRUE
}

ec_map_alerta_para_fila <- function(row, horizonte_largo = FALSE) {
  al <- if (isTRUE(horizonte_largo)) {
    if ("alerta_extendida" %in% names(row)) row$alerta_extendida[[1]] else NULL
  } else if ("alerta" %in% names(row)) {
    row$alerta[[1]]
  } else {
    NULL
  }
  if (is.null(al) || !is.list(al) || length(al) == 0L) return(NULL)
  if (is.null(al$variable) || !nzchar(as.character(al$variable))) return(NULL)
  al
}

ec_map_df_con_alerta_activa <- function(df, horizonte_largo = FALSE) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  df$alerta_mapa <- lapply(seq_len(nrow(df)), function(i) {
    ec_map_alerta_para_fila(df[i, , drop = FALSE], horizonte_largo)
  })
  df
}

ec_map_df_alerta_attrs <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  src <- if ("alerta_mapa" %in% names(df)) "alerta_mapa" else "alerta"
  if (!src %in% names(df)) {
    df$._alerta_sev <- character(nrow(df))
    df$._alerta_var <- character(nrow(df))
    df$._alerta_dir <- character(nrow(df))
    return(df)
  }
  df$._alerta_sev <- vapply(df[[src]], function(a) {
    if (is.null(a) || !is.list(a)) return("")
    as.character(a$severidad %||% "")
  }, character(1))
  df$._alerta_var <- vapply(df[[src]], function(a) {
    if (is.null(a) || !is.list(a)) return("")
    as.character(a$variable %||% "")
  }, character(1))
  df$._alerta_dir <- vapply(df[[src]], function(a) {
    if (is.null(a) || !is.list(a)) return("")
    as.character(a$direccion %||% "")
  }, character(1))
  df
}

ec_alerta_nivel_estacion <- function(row) {
  as.character(row$nivel %||% row$cobertura %||% "")
}

ec_alerta_var_label <- function(var) {
  abr <- as.character(var)
  if (exists("ec_variable_label_abr", mode = "function")) {
    lab <- ec_variable_label_abr(abr, "corta")
    if (nzchar(lab)) return(lab)
  }
  EC_ALERTA_VAR_LABELS[[abr]] %||% abr
}

ec_alerta_popup_fecha <- function(fecha) {
  d <- suppressWarnings(tryCatch(as.Date(fecha), error = function(e) NA))
  if (is.na(d)) return(as.character(fecha %||% ""))
  meses <- c(
    "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
  )
  sprintf("%d de %s", as.integer(format(d, "%d")), meses[as.integer(format(d, "%m"))])
}

ec_alerta_popup_var_corta <- function(var_abr) {
  if (exists("ec_variable_label_abr", mode = "function")) {
    lab <- ec_variable_label_abr(var_abr, "corta")
    if (nzchar(lab)) return(lab)
  }
  switch(as.character(var_abr),
    pp = "Precip",
    t_max = "Temp M\u00e1x",
    t_min = "Temp M\u00edn",
    u2_mean = "Viento medio",
    u_max_diario = "Viento m\u00e1ximo diario",
    u_10_kt = "Viento predominante",
    u_rafaga = "R\u00e1faga",
    "Alerta"
  )
}

ec_alerta_popup_valor_txt <- function(val, unidad, var_abr) {
  v <- suppressWarnings(as.numeric(val))
  if (is.na(v)) return("\u2014")
  txt <- gsub("\\.", ",", format(round(v, 1), nsmall = 1))
  abr <- as.character(var_abr)
  u <- trimws(as.character(unidad %||% ""))
  if (abr %in% c("t_max", "t_min") || grepl("\u00b0", u, fixed = TRUE)) return(paste0(txt, "\u00b0"))
  if (nzchar(u)) return(paste0(txt, " ", u))
  txt
}

ec_alerta_popup_rank_txt <- function(ranking, direccion, var_abr) {
  rank <- suppressWarnings(as.integer(ranking))
  if (is.na(rank)) return("")
  abr <- as.character(var_abr)
  dir <- as.character(direccion %||% "max")
  ranked <- if (exists("ec_variable_label_abr", mode = "function")) {
    ec_variable_label_abr(abr, "rank")
  } else {
    ""
  }
  tail <- if (identical(dir, "min")) {
    switch(abr, t_min = "m\u00e1s fr\u00edo", pp = "menos lluvia registrada", "m\u00ednimo hist\u00f3rico")
  } else if (nzchar(ranked)) {
    ranked
  } else {
    switch(abr,
      pp = "m\u00e1s lluvioso",
      t_max = "m\u00e1s caliente",
      u2_mean = "medio m\u00e1s alto",
      u_max_diario = "m\u00e1ximo diario m\u00e1s intenso",
      u_10_kt = "predominante m\u00e1s intenso",
      u_rafaga = "r\u00e1faga m\u00e1s intensa",
      "m\u00e1ximo hist\u00f3rico"
    )
  }
  paste0("puesto ", rank, ", ", tail)
}

# Overlay: un icono por estación. El anticipado entra con su nivel; /alertas/mapa no cambia.
ec_alertas_sumar_anticipada <- function(df, ant) {
  if (!exists("%||%", mode = "function")) {
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  }
  rows <- if (is.null(ant)) list() else ant$alertas %||% ant
  if (is.data.frame(rows)) {
    if (!nrow(rows)) rows <- list() else rows <- lapply(seq_len(nrow(rows)), function(i) as.list(rows[i, ]))
  }
  if (!is.list(rows) || !length(rows) || !is.null(rows$id)) rows <- if (!is.null(rows$id)) list(rows) else list()
  extra <- lapply(rows, function(x) {
    if (is.null(x$id)) return(NULL)
    niv <- as.character(x$nivel %||% x$severidad %||% "aviso")
    data.frame(
      id = as.integer(x$id), lat = as.numeric(x$lat), lon = as.numeric(x$lon),
      variable = as.character(x$variable %||% "pp"),
      direccion = as.character(x$direccion %||% "max"),
      ranking = if (identical(niv, "posibilidad")) 50L else 1L,
      severidad = if (identical(niv, "posibilidad")) "posibilidad" else "critica",
      stringsAsFactors = FALSE
    )
  })
  extra <- extra[!vapply(extra, is.null, logical(1))]
  if (!length(extra)) return(df)
  add <- do.call(rbind, extra)
  add <- add[!is.na(add$id) & !is.na(add$lat) & !is.na(add$lon), , drop = FALSE]
  if (!nrow(add)) return(df)
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(add)
  if (any(add$severidad == "posibilidad") && any(df$ranking <= 3, na.rm = TRUE)) {
    ocupadas <- unique(df$id[!is.na(df$ranking) & df$ranking <= 3])
    add <- add[!(add$severidad == "posibilidad" & add$id %in% ocupadas), , drop = FALSE]
  }
  if (!nrow(add)) return(df)
  rbind(df, add[, intersect(names(add), names(df)), drop = FALSE])
}
ec_alertas_mapa_one_per_station <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) <= 1L) return(df)
  rk <- suppressWarnings(as.integer(df$ranking))
  rk[is.na(rk)] <- 999999L
  df <- df[order(df$id, rk, df$variable), , drop = FALSE]
  df[!duplicated(df$id), , drop = FALSE]
}

ec_alertas_normalize_list <- function(x) {
  if (is.null(x) || length(x) == 0L) return(list())
  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  }
  if (is.list(x) && !is.null(x$variable)) return(list(x))
  if (is.list(x)) return(x)
  list()
}

# Porcentaje congelado el 2026-11-07 (extremo_prob_clasica_temp.json).
# Solo las celdas que pasaron posibilidad: t_max/max y t_min/max, leads 1-3.
# t_max/max une los rangos 1 a 10. t_min/max separa el rango 1.
ec_alerta_clasica_temp_posibilidad <- function(abr, direccion, lead, ranking) {
  abr <- tolower(as.character(abr %||% ""))
  direccion <- tolower(as.character(direccion %||% ""))
  lead <- suppressWarnings(as.integer(lead))
  ranking <- suppressWarnings(as.integer(ranking))
  if (length(lead) != 1L || length(ranking) != 1L || is.na(lead) || is.na(ranking)) return("")
  if (!lead %in% 1:3 || ranking < 1L || ranking > 10L) return("")
  celda <- paste(abr, direccion, lead, sep = "|")
  # p de rangos 1-10, o NA si el rango 1 va aparte; tasa normal de la celda.
  tab <- c(
    "t_max|max|1" = "0.1801409353|0.012292939608",
    "t_max|max|2" = "0.17289719626|0.012313281496",
    "t_max|max|3" = "0.17099973198|0.012265103991",
    "t_min|max|1" = "0.11356397584|0.01085298137|0.12578616352",
    "t_min|max|2" = "0.10956243574|0.01078565885|0.12689655172",
    "t_min|max|3" = "0.10984242978|0.010855424069|0.12628278221"
  )
  raw <- if (celda %in% names(tab)) unname(tab[[celda]]) else ""
  if (!nzchar(raw)) return("")
  n <- as.numeric(strsplit(raw, "|", fixed = TRUE)[[1]])
  p <- if (length(n) == 3L && ranking == 1L) n[3] else n[1]
  tasa <- n[2]
  if (!is.finite(p) || p < 0) return("")
  if (p < 0.05) {
    pct <- "menos de 5 %"
  } else {
    k <- as.integer(round(p * 20) * 5)
    pct <- if (k < 5L) "menos de 5 %" else paste0(min(k, 100L), " %")
  }
  normal <- if (!is.finite(tasa) || tasa < 0) {
    ""
  } else if (tasa * 100 < 1) {
    "menos de 1 %"
  } else {
    paste0(round(tasa * 100), " %")
  }
  if (!nzchar(normal)) return("")
  paste0("Posibilidad: ", pct, ". Lo normal es ", normal, ". No es un aviso oficial.")
}

ec_alerta_popup_linea <- function(a) {
  if (is.null(a) || !is.list(a)) return("")
  var <- as.character(a$variable %||% a$nombre_abr %||% "")
  hoy <- as.Date(Sys.time(), tz = "America/Santiago")
  fecha <- suppressWarnings(as.Date(a$fecha_objetivo))
  lead <- if (is.na(fecha) || is.na(hoy)) NA_integer_ else as.integer(fecha - hoy)
  # 2026-11-16: tardia / lead 0 clasica fuera del popup.
  if (is.finite(lead) && lead <= 0L) return("")
  extra <- ec_alerta_clasica_temp_posibilidad(var, a$direccion, lead, a$ranking)
  paste0(
    ec_alerta_popup_fecha(a$fecha_objetivo),
    " \u00b7 Alerta ", ec_alerta_popup_var_corta(var),
    " = ", ec_alerta_popup_valor_txt(a$valor_pronostico, a$unidad, var),
    ", ", ec_alerta_popup_rank_txt(a$ranking, a$direccion, var),
    if (nzchar(extra)) paste0(" \u00b7 ", extra) else ""
  )
}

# Fallback si la API no expone nivel precalculado (MV ausente).
ec_clasificar_cobertura <- function(anios, n_variables) {
  anios <- suppressWarnings(as.numeric(anios)); anios[is.na(anios)] <- 0
  nv    <- suppressWarnings(as.numeric(n_variables)); nv[is.na(nv)] <- 0
  score <- pmin(anios, 40) / 40 * 0.6 + pmin(nv, 6) / 6 * 0.4
  as.character(cut(
    score,
    breaks = c(-Inf, 0.25, 0.5, 0.75, Inf),
    labels = c("limitada", "regular", "buena", "excelente"),
    right = FALSE
  ))
}

ec_calidad_attr <- function(nivel, attr, default = NA) {
  vapply(nivel, function(k) {
    n <- EC_CALIDAD_NIVELES[[as.character(k)]]
    if (is.null(n)) return(default)
    n[[attr]]
  }, FUN.VALUE = default, USE.NAMES = FALSE)
}
ec_cobertura_attr <- ec_calidad_attr

ec_calidad_resumen_corto <- function(pct_completitud, anios, n_variables, registros) {
  pct <- suppressWarnings(as.numeric(pct_completitud)); if (is.na(pct)) pct <- 0
  an  <- suppressWarnings(as.integer(anios));             if (is.na(an)) an <- 0L
  nv  <- suppressWarnings(as.integer(n_variables));       if (is.na(nv)) nv <- 0L
  rg  <- suppressWarnings(as.numeric(registros));         if (is.na(rg)) rg <- 0
  reg_fmt <- if (rg >= 1000) paste0(format(round(rg / 1000, 1), nsmall = 1), "k") else as.character(as.integer(rg))
  paste0(
    round(pct, 0), "% completitud \u00b7 ", an, " ", if (an == 1L) "a\u00f1o" else "a\u00f1os",
    " \u00b7 ", nv, " ", if (nv == 1L) "var" else "vars",
    " \u00b7 ", reg_fmt, " reg."
  )
}
ec_cobertura_resumen_corto <- ec_calidad_resumen_corto

ec_calidad_tooltip_html <- function(nombre, nivel, pct_completitud, anios, n_variables, registros,
                                    n_imposibles = 0, pct_imposibles = 0, fecha_max = NULL,
                                    pct_continuidad = NULL) {
  niv <- EC_CALIDAD_NIVELES[[as.character(nivel)]]
  etiqueta <- if (is.null(niv)) "Sin clasificar" else niv$etiqueta
  color    <- if (is.null(niv)) "#8A95A0" else niv$color
  esc <- htmltools::htmlEscape
  imp <- suppressWarnings(as.integer(n_imposibles)); if (is.na(imp)) imp <- 0L
  pimp <- suppressWarnings(as.numeric(pct_imposibles)); if (is.na(pimp)) pimp <- 0
  imp_html <- ""
  if (imp > 0L) {
    imp_html <- paste0(
      "<div class=\"ec-map-tt-imp\">Valores imposibles: ", imp,
      " (", gsub("\\.", ",", sprintf("%.1f", pimp)), "%)</div>"
    )
  }
  ultimo_html <- ""
  if (!is.null(fecha_max) && length(fecha_max) > 0L && !is.na(fecha_max) && nzchar(as.character(fecha_max))) {
    fm <- tryCatch(as.Date(fecha_max), error = function(e) NA)
    if (!is.na(fm)) {
      ultimo_html <- paste0(
        "<div class=\"ec-map-tt-ultimo\">\u00daltimo dato: ",
        esc(format(fm, "%d/%m/%Y")), "</div>"
      )
    }
  }
  cont_html <- ""
  pc <- suppressWarnings(as.numeric(pct_continuidad))
  if (!is.null(pct_continuidad) && length(pc) > 0L && !is.na(pc)) {
    cont_html <- paste0(
      "<div class=\"ec-map-tt-ultimo\">Continuidad: ",
      esc(gsub("\\.", ",", sprintf("%.0f", pc * 100))),
      "% de a\u00f1os con dato</div>"
    )
  }
  paste0(
    "<div class=\"ec-map-tooltip\">",
    "<div class=\"ec-map-tt-name\">", esc(nombre), "</div>",
    "<div class=\"ec-map-tt-badge\"><span class=\"ec-map-tt-dot\" style=\"background:", color, "\"></span>",
    esc(etiqueta), "</div>",
    "<div class=\"ec-map-tt-stats\">", esc(ec_calidad_resumen_corto(pct_completitud, anios, n_variables, registros)), "</div>",
    ultimo_html,
    cont_html,
    imp_html,
    "<div class=\"ec-map-tt-hint\">Haz clic para ver detalle</div>",
    "</div>"
  )
}
ec_cobertura_tooltip_html <- function(nombre, cobertura, n_variables, anios, registros) {
  ec_calidad_tooltip_html(nombre, cobertura, 0, anios, n_variables, registros)
}

ec_calidad_leyenda_html <- function() {
  filas <- vapply(c("excelente", "buena", "regular", "limitada"), function(k) {
    n <- EC_CALIDAD_NIVELES[[k]]
    paste0(
      "<div class=\"ec-map-legend-row ec-map-legend-tip\" data-tip=\"",
      htmltools::htmlEscape(n$tip %||% "", attribute = TRUE),
      "\" tabindex=\"0\" role=\"note\" aria-label=\"",
      htmltools::htmlEscape(n$tip %||% "", attribute = TRUE), "\">",
      "<span class=\"ec-map-legend-dot\" style=\"--ec-lg-color:", n$color,
      ";--ec-lg-size:", n$radio * 2.5, "px;--ec-lg-opacity:", n$opacidad, "\"></span>",
      "<span class=\"ec-map-legend-label\">", n$etiqueta, "</span>",
      "</div>"
    )
  }, character(1))
  paste0(
    "<div class=\"ec-map-legend\" id=\"ec_map_legend\">",
    "<div class=\"ec-map-legend-title\">Calidad de datos</div>",
    paste(filas, collapse = ""),
    "</div>"
  )
}

ec_alerta_leyenda_bucket <- function(var, direccion) {
  abr <- as.character(var %||% "")
  if (identical(abr, "pp")) return("pp")
  if (abr %in% c("u_max_diario", "u2_mean", "u_10_kt", "u_rafaga")) return("wind")
  if (abr %in% c("t_max", "t_min")) {
    dir <- tolower(trimws(as.character(direccion %||% "max")))
    return(if (identical(dir, "min")) "cold" else "heat")
  }
  NA_character_
}

ec_alerta_leyenda_counts_empty <- function() {
  c(pp = 0L, cold = 0L, heat = 0L, wind = 0L)
}

ec_alerta_counts_from_alertas_df <- function(df) {
  keys <- ec_alerta_leyenda_counts_empty()
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(keys)
  if (!("variable" %in% names(df))) return(keys)
  dir_col <- if ("direccion" %in% names(df)) df$direccion else rep(NA_character_, nrow(df))
  has_id <- "id" %in% names(df)
  seen <- character(0)
  for (i in seq_len(nrow(df))) {
    k <- ec_alerta_leyenda_bucket(df$variable[i], dir_col[i])
    if (is.na(k) || !(k %in% names(keys))) next
    if (has_id) {
      key <- paste(df$id[i], k, sep = "\t")
      if (key %in% seen) next
      seen <- c(seen, key)
    }
    keys[[k]] <- keys[[k]] + 1L
  }
  keys
}

ec_alerta_leyenda_counts <- function(df, horizonte_largo = FALSE) {
  keys <- ec_alerta_leyenda_counts_empty()
  if (is.null(df) || nrow(df) == 0L) return(keys)
  if ("variable" %in% names(df) && !("alerta" %in% names(df)) &&
      !("alerta_conteos" %in% names(df)) && !("alerta_conteos_extendida" %in% names(df))) {
    return(ec_alerta_counts_from_alertas_df(df))
  }
  col_conteos <- if (isTRUE(horizonte_largo)) "alerta_conteos_extendida" else "alerta_conteos"
  if (col_conteos %in% names(df)) {
    for (i in seq_len(nrow(df))) {
      asp <- if ("anios_span" %in% names(df)) df$anios_span[i] else NA_integer_
      niv <- if ("nivel" %in% names(df)) df$nivel[i] else if ("cobertura" %in% names(df)) df$cobertura[i] else NULL
      cnt <- df[[col_conteos]][[i]]
      if (is.null(cnt) || !is.list(cnt)) next
      if (!ec_alerta_valida(list(variable = "pp"), asp, niv)) next
      for (k in names(keys)) {
        keys[[k]] <- keys[[k]] + max(0L, as.integer(cnt[[k]] %||% 0L))
      }
    }
    return(keys)
  }
  src <- if ("alerta_mapa" %in% names(df)) "alerta_mapa" else if ("alerta" %in% names(df)) "alerta" else NULL
  if (is.null(src)) return(keys)
  for (i in seq_len(nrow(df))) {
    asp <- if ("anios_span" %in% names(df)) df$anios_span[i] else NA_integer_
    niv <- if ("nivel" %in% names(df)) df$nivel[i] else if ("cobertura" %in% names(df)) df$cobertura[i] else NULL
    a <- df[[src]][[i]]
    if (!ec_alerta_valida(a, asp, niv)) next
    k <- ec_alerta_leyenda_bucket(a$variable %||% a$nombre_abr, a$direccion)
    if (!is.na(k) && k %in% names(keys)) keys[[k]] <- keys[[k]] + 1L
  }
  keys
}

ec_alerta_leyenda_html <- function(counts = NULL) {
  if (is.null(counts)) counts <- ec_alerta_leyenda_counts_empty()
  icon_sz <- EC_MAP_ALERTA_LEYENDA_ICON_PX
  icon_html <- paste(vapply(EC_ALERTA_LEYENDA_TIPOS, function(spec) {
    sym <- ec_alerta_badge_svg(
      "aviso_de_madrugada", spec$var, size = icon_sz, direccion = spec$direccion
    )
    n <- as.integer(counts[[spec$key]] %||% 0L)
    tip <- htmltools::htmlEscape(spec$tip %||% "", attribute = TRUE)
    label <- htmltools::htmlEscape(spec$label %||% spec$key)
    zero_cls <- if (n == 0L) " ec-map-legend-alerta-n--zero" else " ec-map-legend-alerta-n--on"
    paste0(
      "<button type=\"button\" class=\"ec-map-legend-alerta-chip ec-map-legend-tip\"",
      " data-alerta-key=\"", spec$key, "\" data-tip=\"", tip,
      "\" aria-pressed=\"false\" aria-label=\"", tip, "\" title=\"", tip, "\">",
      "<span class=\"ec-map-alerta-sym ec-map-legend-alerta-sym\" aria-hidden=\"true\">",
      sym, "</span>",
      "<span class=\"ec-map-legend-alerta-label\">", label, "</span>",
      "<span class=\"ec-map-legend-alerta-n", zero_cls, "\" data-alerta-key=\"",
      spec$key, "\">", n, "</span>",
      "</button>"
    )
  }, character(1)), collapse = "")
  h_tip <- htmltools::htmlEscape(
    paste0(
      "Por defecto: alertas en los pr\u00f3ximos ", EC_MAP_ALERTA_HORIZONTE_CORTO,
      " d\u00edas. Activar para ver hasta ", EC_MAP_ALERTA_HORIZONTE_LARGO, " d\u00edas."
    ),
    attribute = TRUE
  )
  paste0(
    "<div class=\"ec-map-legend-alerta\" id=\"ec_map_legend_alerta\" data-alerta-filter=\"\">",
    "<div class=\"ec-map-legend-alerta-head\">",
    "<div class=\"ec-map-legend-alerta-title\">Alertas evento extremo</div>",
    "<div class=\"ec-map-legend-alerta-head-actions\">",
    "<button type=\"button\" class=\"ec-map-legend-alerta-expand\" id=\"ec_map_alerta_expand\"",
    " aria-expanded=\"false\" aria-controls=\"ec_map_alerta_tipos\" title=\"Mostrar u ocultar nombres\">",
    "Nombres</button>",
    "<button type=\"button\" class=\"ec-map-legend-alerta-clear\" id=\"ec_map_alerta_clear\"",
    " hidden aria-label=\"Ver todos los tipos de alerta\">Ver todos</button>",
    "<label class=\"ec-map-legend-alerta-horizonte ec-map-legend-tip\" data-tip=\"", h_tip,
    "\" tabindex=\"0\" role=\"switch\" aria-label=\"Mostrar alertas de ", EC_MAP_ALERTA_HORIZONTE_LARGO, " d\u00edas\">",
    "<input type=\"checkbox\" id=\"ec_map_alerta_horizonte_ext\" />",
    "<span>", EC_MAP_ALERTA_HORIZONTE_LARGO, " d</span>",
    "</label>",
    "</div>",
    "</div>",
    "<div class=\"ec-map-legend-alerta-icons\" id=\"ec_map_alerta_tipos\">", icon_html, "</div>",
    "</div>"
  )
}
ec_cobertura_leyenda_html <- ec_calidad_leyenda_html

ec_map_bounds_chile <- function() {
  list(lng1 = -76, lat1 = -56, lng2 = -66, lat2 = -17)
}

# Bounds para flyToBounds/fitBounds segun estaciones visibles (filtradas).
ec_map_bounds_from_df <- function(df, padding_deg = 0.02) {
  chile <- ec_map_bounds_chile()
  if (is.null(df) || nrow(df) == 0L) return(chile)
  lon <- suppressWarnings(as.numeric(df$lon))
  lat <- suppressWarnings(as.numeric(df$lat))
  ok <- !is.na(lon) & !is.na(lat)
  if (!any(ok)) return(chile)
  lon <- lon[ok]
  lat <- lat[ok]
  pad <- padding_deg
  list(
    lng1 = min(lon) - pad,
    lat1 = min(lat) - pad,
    lng2 = max(lon) + pad,
    lat2 = max(lat) + pad
  )
}

# ponytail: offset circular pequeno; popup sigue usando lat/lon reales por id
ec_map_offset_coords_duplicadas <- function(df, radius = 0.00028) {
  if (is.null(df) || nrow(df) < 2L) return(df)
  if (!all(c("lat", "lon", "id") %in% names(df))) return(df)
  lat_r <- round(as.numeric(df$lat), 5)
  lon_r <- round(as.numeric(df$lon), 5)
  key <- paste(lat_r, lon_r, sep = ":")
  dup_keys <- unique(key[duplicated(key) | duplicated(key, fromLast = TRUE)])
  if (length(dup_keys) == 0L) return(df)
  df$lat_map <- as.numeric(df$lat)
  df$lon_map <- as.numeric(df$lon)
  for (k in dup_keys) {
    idx <- which(key == k)
    if (length(idx) < 2L) next
    idx <- idx[order(df$id[idx])]
    n <- length(idx)
    for (j in seq_len(n)) {
      ang <- 2 * pi * (j - 1L) / n
      i <- idx[j]
      df$lat_map[i] <- as.numeric(df$lat[i]) + radius * sin(ang)
      df$lon_map[i] <- as.numeric(df$lon[i]) + radius * cos(ang)
    }
  }
  df
}

ec_map_df_para_pintar <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  if (!"lat_map" %in% names(df)) return(df)
  out <- df
  out$lat <- out$lat_map
  out$lon <- out$lon_map
  out
}

ec_estaciones_coords_hermanas <- function(df, eid, digits = 4L) {
  eid <- suppressWarnings(as.integer(eid))
  if (is.null(df) || nrow(df) == 0L || is.na(eid)) return(character(0))
  row <- df[df$id == eid, , drop = FALSE]
  if (nrow(row) == 0L) return(character(0))
  lat <- round(as.numeric(row$lat[1]), digits)
  lon <- round(as.numeric(row$lon[1]), digits)
  if (is.na(lat) || is.na(lon)) return(character(0))
  sibs <- df[
    round(as.numeric(df$lat), digits) == lat &
      round(as.numeric(df$lon), digits) == lon &
      df$id != eid,
    ,
    drop = FALSE
  ]
  if (nrow(sibs) == 0L) return(character(0))
  as.character(sibs$nombre)
}

ec_map_coords_estacion <- function(df, eid) {
  if (is.null(df) || nrow(df) == 0L) return(list(lat = NA_real_, lon = NA_real_))
  off <- ec_map_offset_coords_duplicadas(df)
  row <- off[off$id == as.integer(eid), , drop = FALSE]
  if (nrow(row) == 0L) return(list(lat = NA_real_, lon = NA_real_))
  if ("lat_map" %in% names(row)) {
    return(list(lat = as.numeric(row$lat_map[1]), lon = as.numeric(row$lon_map[1])))
  }
  list(lat = as.numeric(row$lat[1]), lon = as.numeric(row$lon[1]))
}

# Circle markers above choropleth canvas (ecAdmin z 410). Labels stay at 450.
ec_map_circle_opts <- function(interactive = TRUE) {
  leaflet::pathOptions(
    pane = "ecStations",
    interactive = isTRUE(interactive),
    keyboard = isTRUE(interactive)
  )
}

# Atributos visuales para circleMarkers (sin tooltips HTML pesados).
ec_prepare_estaciones_mapa_df <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  if (!"cobertura" %in% names(df)) {
    if ("nivel" %in% names(df) && any(!is.na(df$nivel) & nzchar(df$nivel))) {
      df$cobertura <- as.character(df$nivel)
      df$cobertura[is.na(df$cobertura) | !nzchar(df$cobertura)] <- "limitada"
    } else {
      anios <- if ("anios_span" %in% names(df)) df$anios_span else df$anios_datos
      df$cobertura <- ec_clasificar_cobertura(anios %||% 0, df$n_variables %||% 0)
    }
  }
  if (!"pct_completitud" %in% names(df)) df$pct_completitud <- 0
  if (!"pct_continuidad" %in% names(df)) df$pct_continuidad <- NA_real_
  if (!"n_imposibles" %in% names(df)) df$n_imposibles <- 0L
  if (!"pct_imposibles" %in% names(df)) df$pct_imposibles <- 0
  df$._orden <- ec_calidad_attr(df$cobertura, "orden", default = 0)
  df <- df[order(df$._orden), , drop = FALSE]
  df$._color  <- ec_calidad_attr(df$cobertura, "color", default = "#8A95A0")
  df$._radio  <- ec_calidad_attr(df$cobertura, "radio", default = 5)
  df$._opac   <- ec_calidad_attr(df$cobertura, "opacidad", default = 0.6)
  df$._weight <- ec_calidad_attr(df$cobertura, "weight", default = 1)
  if ("alerta" %in% names(df) || "alerta_extendida" %in% names(df)) {
    niv_col <- if ("nivel" %in% names(df)) df$nivel else if ("cobertura" %in% names(df)) df$cobertura else NULL
    if (!is.null(niv_col)) {
      for (i in seq_len(nrow(df))) {
        if (!identical(as.character(niv_col[i] %||% ""), "excelente")) {
          if ("alerta" %in% names(df)) df$alerta[i] <- list(NULL)
          if ("alerta_extendida" %in% names(df)) df$alerta_extendida[i] <- list(NULL)
        }
      }
    }
    df$._alerta_sev <- vapply(df$alerta, function(a) {
      if (is.null(a) || !is.list(a)) return("")
      as.character(a$severidad %||% "")
    }, character(1))
    df$._alerta_var <- vapply(df$alerta, function(a) {
      if (is.null(a) || !is.list(a)) return("")
      as.character(a$variable %||% "")
    }, character(1))
    df$._alerta_dir <- vapply(df$alerta, function(a) {
      if (is.null(a) || !is.list(a)) return("")
      as.character(a$direccion %||% "")
    }, character(1))
  } else {
    df$._alerta_sev <- character(nrow(df))
    df$._alerta_var <- character(nrow(df))
    df$._alerta_dir <- character(nrow(df))
  }
  df
}

ec_alerta_badge_svg_shapes <- function(sym, color) {
  stroke <- paste0(" stroke=\"#fff\" stroke-width=\"1.15\" stroke-linejoin=\"round\"")
  switch(sym,
    rain = paste0(
      "<path fill=\"#94A3B8\"", stroke,
      " d=\"M4 11c0-2.2 1.6-4 3.8-4.4C8.5 4.8 10.4 3 13 3c2.8 0 5 2.1 5.2 4.8 1.5.3 2.8 1.6 2.8 3.2 0 1.8-1.5 3-3.2 3H4.8C3 14 2 12.8 2 11.2S3 11 4 11z\"/>",
      "<line x1=\"7.5\" y1=\"14.5\" x2=\"7.5\" y2=\"17.5\" stroke=\"#3B82F6\"",
      " stroke-width=\"1.6\" stroke-linecap=\"round\"/>",
      "<line x1=\"11\" y1=\"14.5\" x2=\"11\" y2=\"18\" stroke=\"#3B82F6\"",
      " stroke-width=\"1.6\" stroke-linecap=\"round\"/>",
      "<line x1=\"14.5\" y1=\"14.5\" x2=\"14.5\" y2=\"17.5\" stroke=\"#3B82F6\"",
      " stroke-width=\"1.6\" stroke-linecap=\"round\"/>"
    ),
    thermo = paste0(
      "<rect x=\"9.2\" y=\"3.5\" width=\"3.6\" height=\"11.5\" rx=\"1.8\" fill=\"", color, "\"", stroke, "/>",
      "<circle cx=\"11\" cy=\"17.2\" r=\"2.8\" fill=\"", color, "\"", stroke, "/>",
      "<rect x=\"10.1\" y=\"6\" width=\"1.8\" height=\"6.5\" rx=\"0.9\" fill=\"#fff\" opacity=\"0.85\"/>"
    ),
    ice = paste0(
      "<g fill=\"none\" stroke=\"", color, "\" stroke-width=\"1.45\" stroke-linecap=\"round\">",
      "<line x1=\"11\" y1=\"3.2\" x2=\"11\" y2=\"18.8\"/>",
      "<line x1=\"3.2\" y1=\"11\" x2=\"18.8\" y2=\"11\"/>",
      "<line x1=\"5.5\" y1=\"5.5\" x2=\"16.5\" y2=\"16.5\"/>",
      "<line x1=\"16.5\" y1=\"5.5\" x2=\"5.5\" y2=\"16.5\"/>",
      "<line x1=\"11\" y1=\"6.2\" x2=\"9.1\" y2=\"4.8\"/><line x1=\"11\" y1=\"6.2\" x2=\"12.9\" y2=\"4.8\"/>",
      "<line x1=\"11\" y1=\"15.8\" x2=\"9.1\" y2=\"17.2\"/><line x1=\"11\" y1=\"15.8\" x2=\"12.9\" y2=\"17.2\"/>",
      "<line x1=\"6.2\" y1=\"11\" x2=\"4.8\" y2=\"9.1\"/><line x1=\"6.2\" y1=\"11\" x2=\"4.8\" y2=\"12.9\"/>",
      "<line x1=\"15.8\" y1=\"11\" x2=\"17.2\" y2=\"9.1\"/><line x1=\"15.8\" y1=\"11\" x2=\"17.2\" y2=\"12.9\"/>",
      "</g>",
      "<circle cx=\"11\" cy=\"11\" r=\"1.6\" fill=\"", color, "\" stroke=\"#fff\" stroke-width=\"0.75\"/>"
    ),
    wind = paste0(
      "<path fill=\"none\" stroke=\"", color, "\" stroke-width=\"1.7\" stroke-linecap=\"round\"",
      " d=\"M4 7.5h9c1.4 0 2.3-.8 2.3-1.8S14.2 4 12.8 4H6\"/>",
      "<path fill=\"none\" stroke=\"", color, "\" stroke-width=\"1.7\" stroke-linecap=\"round\"",
      " d=\"M5 11.5h11c1.6 0 2.7-.9 2.7-2.1S16.4 7.5 14.8 7.5H7\"/>",
      "<path fill=\"none\" stroke=\"", color, "\" stroke-width=\"1.7\" stroke-linecap=\"round\"",
      " d=\"M6 15.5h8c1.2 0 2-.7 2-1.6s-.8-1.4-2-1.4H8\"/>"
    ),
    paste0(
      "<circle cx=\"11\" cy=\"11\" r=\"4.5\" fill=\"", color, "\"", stroke, "/>",
      "<text x=\"11\" y=\"13.5\" text-anchor=\"middle\" fill=\"#fff\" font-size=\"8\" font-weight=\"700\">!</text>"
    )
  )
}

# ponytail: sin bang "!" en critica — usuarios lo reportaban confuso; el icono de variable basta
ec_alerta_badge_svg <- function(severidad, var_abr, size = 22L, direccion = NULL) {
  sym <- ec_alerta_sym_key(var_abr, direccion)
  col <- ec_alerta_var_color(var_abr, direccion)
  sz <- as.integer(size)
    paste0(
      "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"", sz, "\" height=\"", sz,
      "\" viewBox=\"0 0 22 22\" class=\"ec-map-alerta-sym-svg\" aria-hidden=\"true\">",
      ec_alerta_badge_svg_shapes(sym, col),
      if (identical(as.character(severidad), "posibilidad")) {
        "<circle cx=\"11\" cy=\"11\" r=\"9.2\" fill=\"none\" stroke=\"#6b5b2a\" stroke-width=\"1.3\" stroke-dasharray=\"2 1.6\"/>"
      } else "",
      "</svg>"
    )
}

# ponytail: leaflet 2.1.2 no exporta divIcon; SVG + makeIcon funciona en el contenedor Shiny
ec_alerta_badge_leaflet_icon <- function(severidad, var_abr, radio_px = 8, direccion = NULL) {
  sz <- EC_MAP_ALERTA_ICON_PX
  svg <- ec_alerta_badge_svg(severidad, var_abr, size = sz, direccion = direccion)
  svg_uri <- paste0("data:image/svg+xml;charset=UTF-8,", utils::URLencode(svg, reserved = TRUE))
  anchor_y <- as.integer(radio_px + round(sz * 10 / 22))
  leaflet::makeIcon(
    iconUrl = svg_uri,
    iconWidth = sz,
    iconHeight = sz,
    iconAnchorX = -4L,
    iconAnchorY = anchor_y,
    className = "ec-map-alerta-marker"
  )
}

mapa_popup_normalizar_detalle_api <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    return(tryCatch(jsonlite::fromJSON(x, simplifyVector = TRUE), error = function(e) NULL))
  }
  if (is.data.frame(x) && nrow(x) >= 1L) return(x)
  if (is.list(x)) {
    if (length(x) == 1L && is.character(x[[1L]]) && nzchar(x[[1L]]) && grepl("^\\s*\\{", x[[1L]])) {
      pj <- tryCatch(jsonlite::fromJSON(x[[1L]], simplifyVector = TRUE), error = function(e) NULL)
      if (!is.null(pj)) return(pj)
    }
    return(x)
  }
  NULL
}

mapa_popup_normalizar_variables_api <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    x <- tryCatch(jsonlite::fromJSON(x, simplifyVector = TRUE), error = function(e) NULL)
  }
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (!is.list(x)) return(NULL)
  if (!is.null(x$error)) return(NULL)
  if (length(x) > 0L && is.list(x[[1L]]) && length(names(x[[1L]])) > 0L) {
    df <- tryCatch(
      jsonlite::fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE), simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.data.frame(df)) return(df)
  }
  if (length(x) == 0L) {
    return(structure(list(), class = "data.frame", row.names = integer()))
  }
  if (!is.null(names(x)) && all(nzchar(names(x)))) {
    lens <- vapply(x, length, 0L)
    if (length(unique(lens)) == 1L) {
      df <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
      if (is.data.frame(df)) return(df)
    }
  }
  if (is.list(x[[1L]])) {
    df <- tryCatch(
      jsonlite::fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE), simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.data.frame(df)) return(df)
  }
  NULL
}

mapa_popup_scalar <- function(v, default) {
  if (is.null(v)) return(default)
  if (length(v) == 0L) return(default)
  if (is.na(v[1L])) return(default)
  v[[1L]]
}

mapa_popup_texto_mostrar <- function(x, fallback = "\u2014") {
  s <- if (is.null(x)) "" else trimws(as.character(mapa_popup_scalar(x, "")))
  if (!nzchar(s) || tolower(s) == "na" || s == "NULL") fallback else s
}

mapa_popup_int <- function(x) {
  s <- suppressWarnings(as.integer(mapa_popup_scalar(x, NA_integer_)))
  if (length(s) != 1L || is.na(s)) NA_integer_ else s
}

mapa_popup_variables_nombres <- function(vdf) {
  if (is.null(vdf) || !is.data.frame(vdf) || nrow(vdf) == 0L) return(character())
  nm <- names(vdf)
  col <- NULL
  if ("nombre" %in% nm) {
    col <- vdf[["nombre"]]
  } else {
    hit <- nm[tolower(nm) %in% c("nombre", "variable_nombre", "nom_variable", "name")]
    if (length(hit) > 0L) col <- vdf[[hit[1L]]]
  }
  if (is.null(col)) return(character())
  raw <- trimws(as.character(col))
  ok <- !is.na(raw) & nzchar(raw)
  raw <- raw[ok]
  if (length(raw) == 0L) return(character())
  ids <- if ("id" %in% nm) as.integer(vdf$id[ok]) else rep(NA_integer_, length(raw))
  out <- ec_label_variable_vector(raw, ids = ids)
  sort(unique(out))
}

mapa_popup_parse_detalle <- function(det, eid, nombre0, lat0, lon0) {
  reg <- "\u2014"; com <- "\u2014"; id_fuente <- NA_integer_
  nombre <- nombre0; lat <- lat0; lon <- lon0
  if (is.null(det) || (is.list(det) && !is.null(det$error))) {
    return(list(
      nombre = nombre, lat = lat, lon = lon,
      nom_reg = reg, nom_com = com, id_fuente = id_fuente
    ))
  }
  pick <- NULL
  if (is.data.frame(det)) {
    if (nrow(det) < 1L) {
      return(list(nombre = nombre, lat = lat, lon = lon, nom_reg = reg, nom_com = com, id_fuente = id_fuente))
    }
    mrow <- match(eid, det$id)
    pick <- if (!is.na(mrow)) det[mrow, , drop = FALSE] else det[1L, , drop = FALSE]
  } else if (is.list(det)) {
    if (length(det) == 0L) {
      return(list(nombre = nombre, lat = lat, lon = lon, nom_reg = reg, nom_com = com, id_fuente = id_fuente))
    }
    if (!is.null(det$id)) {
      pick <- det
    } else if (is.list(det[[1L]]) && !is.null(det[[1L]]$id)) {
      for (it in det) {
        if (is.list(it) && identical(as.integer(it$id %||% NA_integer_), as.integer(eid))) {
          pick <- it
          break
        }
      }
      if (is.null(pick)) pick <- det[[1L]]
    } else {
      pick <- det
    }
  }
  if (is.data.frame(pick)) {
    nombre <- as.character(pick$nombre[1] %||% nombre)
    lat <- as.numeric(pick$lat[1] %||% lat)
    lon <- as.numeric(pick$lon[1] %||% lon)
    reg <- mapa_popup_texto_mostrar(
      pick$nom_reg[1] %||% pick$region[1] %||% pick$nombre_region[1], reg
    )
    com <- mapa_popup_texto_mostrar(
      pick$nom_com[1] %||% pick$comuna[1] %||% pick$nombre_comuna[1], com
    )
    id_fuente <- mapa_popup_int(pick$id_fuente[1])
  } else if (is.list(pick)) {
    nombre <- as.character(pick$nombre %||% nombre)
    lat <- as.numeric(pick$lat %||% lat)
    lon <- as.numeric(pick$lon %||% lon)
    reg <- mapa_popup_texto_mostrar(
      pick$nom_reg %||% pick$region %||% pick$nombre_region, reg
    )
    com <- mapa_popup_texto_mostrar(
      pick$nom_com %||% pick$comuna %||% pick$nombre_comuna, com
    )
    id_fuente <- mapa_popup_int(pick$id_fuente)
  }
  list(nombre = nombre, lat = lat, lon = lon, nom_reg = reg, nom_com = com, id_fuente = id_fuente)
}

MSG_VARS_DEF <- "Sin variables con datos en esta estaci\u00f3n."

# Operadores conocidos (sufijo en nombre interno …_DMC). Backend debería
# separar nombre_display / operador; esto es fallback de UI.
EC_POPUP_OPS_SUFFIX <- c("DMC", "DGA", "INIA", "CEAZA", "AGROMET", "SINCA")

# ponytail: humaniza snake_case + sufijo operador; techo = heurística de nombre.
# Upgrade: columna nombre_display en API/estacion.
ec_popup_nombre_partes <- function(nombre, operador_hint = NULL) {
  raw <- trimws(as.character(nombre %||% "")[1])
  if (is.na(raw) || !nzchar(raw)) {
    return(list(display = "Estaci\u00f3n", op_code = "", raw = ""))
  }
  op <- toupper(trimws(as.character(operador_hint %||% "")[1]))
  if (is.na(op) || identical(op, "\u2014") || identical(op, "-") || identical(op, "NA")) op <- ""
  base <- raw
  if (nzchar(op)) {
    re_op <- paste0("_", op, "$")
    if (grepl(re_op, raw, ignore.case = TRUE)) {
      base <- sub(re_op, "", raw, ignore.case = TRUE)
    }
  } else {
    for (code in EC_POPUP_OPS_SUFFIX) {
      re_op <- paste0("_", code, "$")
      if (grepl(re_op, raw, ignore.case = TRUE)) {
        op <- code
        base <- sub(re_op, "", raw, ignore.case = TRUE)
        break
      }
    }
  }
  parts <- strsplit(base, "_+", perl = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) parts <- raw
  title_one <- function(w) {
    w <- tolower(w)
    paste0(toupper(substr(w, 1L, 1L)), substring(w, 2L))
  }
  parts <- vapply(parts, title_one, character(1), USE.NAMES = FALSE)
  display <- if (length(parts) >= 2L) {
    paste0(parts[[1]], " \u2014 ", paste(parts[-1], collapse = " "))
  } else {
    parts[[1]]
  }
  list(display = display, op_code = op, raw = raw)
}

ec_popup_var_familia <- function(label) {
  s <- tolower(ec_strip_accents(as.character(label %||% "")))
  if (!nzchar(s)) return("otro")
  if (grepl("precip|lluv", s)) return("pp")
  if (grepl("humedad", s)) return("hr")
  if (grepl("temp", s)) return("temp")
  if (grepl("viento", s)) return("viento")
  if (grepl("caudal|pozo|nivel", s)) return("hidro")
  if (grepl("presion", s)) return("presion")
  if (grepl("radia|solar|nubos|cloud", s)) return("solar")
  if (grepl("particula|mp2|mp10", s)) return("aire")
  "otro"
}

ec_popup_var_icon <- function(fam) {
  # Deprecated: chips usan dot CSS (FA se rompia con Inter !important / zoom).
  switch(as.character(fam),
    pp = "fa-tint",
    hr = "fa-cloud",
    temp = "fa-sun-o",
    viento = "fa-flag",
    hidro = "fa-tint",
    presion = "fa-tachometer",
    solar = "fa-sun-o",
    aire = "fa-circle-o",
    "fa-area-chart"
  )
}

ec_popup_vars_chips_html <- function(nombres) {
  esc <- htmltools::htmlEscape
  labs <- unique(as.character(nombres))
  labs <- labs[nzchar(trimws(labs))]
  if (length(labs) == 0L) return("")
  fams <- vapply(labs, ec_popup_var_familia, character(1), USE.NAMES = FALSE)
  ord <- order(match(fams, c("temp", "pp", "hr", "viento", "hidro", "presion", "solar", "aire", "otro"), nomatch = 99L), labs)
  labs <- labs[ord]
  fams <- fams[ord]
  chips <- vapply(seq_along(labs), function(i) {
    paste0(
      "<span class=\"ec-map-popup-var-chip ec-map-popup-var-chip--", fams[[i]], "\">",
      "<span class=\"ec-map-popup-var-dot\" aria-hidden=\"true\"></span>",
      esc(labs[[i]]),
      "</span>"
    )
  }, character(1))
  paste0("<div class=\"ec-map-popup-vars-chips\">", paste(chips, collapse = ""), "</div>")
}

# Self-check (no tumbar el source si falla en un locale raro)
local({
  p <- tryCatch(ec_popup_nombre_partes("Malloa_los_maquis_DMC"), error = function(e) NULL)
  if (is.null(p) || !identical(p$op_code, "DMC") || !grepl("Malloa", p$display, fixed = TRUE)) {
    warning("ec_popup_nombre_partes self-check fallo: ", paste(capture.output(str(p)), collapse = " "))
  }
  vf <- tryCatch(ec_popup_var_familia("Temperatura Maxima"), error = function(e) "")
  if (!identical(vf, "temp")) {
    warning("ec_popup_var_familia(temp) self-check fallo")
  }
  dups <- data.frame(
    id = c(1L, 1L, 2L),
    ranking = c(3L, 1L, 2L),
    variable = c("pp", "t_max", "pp"),
    stringsAsFactors = FALSE
  )
  one <- tryCatch(ec_alertas_mapa_one_per_station(dups), error = function(e) NULL)
  if (is.null(one) || nrow(one) != 2L || !identical(as.integer(one$ranking[one$id == 1L]), 1L)) {
    warning("ec_alertas_mapa_one_per_station self-check fallo")
  }
  ant <- tryCatch(ec_alertas_sumar_anticipada(
    data.frame(id = 1L, lat = 1, lon = 1, variable = "t_max", direccion = "max",
               ranking = 1L, severidad = "critica", stringsAsFactors = FALSE),
    list(alertas = list(list(id = 1L, lat = 1, lon = 1, nivel = "posibilidad"),
                        list(id = 2L, lat = 2, lon = 2, nivel = "aviso")))
  ), error = function(e) NULL)
  if (is.null(ant) || !any(ant$id == 2L) || any(ant$id == 1L & ant$severidad == "posibilidad")) {
    warning("ec_alertas_sumar_anticipada self-check fallo")
  }
  if (!identical(
    ec_alerta_clasica_temp_posibilidad("t_max", "max", 1L, 2L),
    "Posibilidad: 20 %. Lo normal es 1 %. No es un aviso oficial."
  ) || !identical(
    ec_alerta_clasica_temp_posibilidad("t_min", "max", 2L, 1L),
    "Posibilidad: 15 %. Lo normal es 1 %. No es un aviso oficial."
  ) || nzchar(ec_alerta_clasica_temp_posibilidad("t_max", "min", 1L, 1L)) ||
    nzchar(ec_alerta_clasica_temp_posibilidad("t_min", "min", 3L, 1L)) ||
    nzchar(ec_alerta_clasica_temp_posibilidad("t_max", "max", 0L, 1L)) ||
    nzchar(ec_alerta_clasica_temp_posibilidad("pp", "max", 1L, 1L))) {
    warning("ec_alerta_clasica_temp_posibilidad self-check fallo")
  }
})

mapa_popup_alertas_resumen_html <- function(alertas) {
  if (is.null(alertas) || length(alertas) == 0L) return("")
  esc <- htmltools::htmlEscape
  items <- vapply(alertas, function(a) {
    if (is.null(a)) return("")
    sev <- as.character(a$severidad %||% "aviso_de_madrugada")
    linea <- ec_alerta_popup_linea(a)
    if (!nzchar(linea)) return("")
    paste0(
      "<div class=\"ec-map-popup-alerta-line ec-map-popup-alerta-line--",
      htmltools::htmlEscape(sev, attribute = TRUE), "\">",
      "<span class=\"ec-map-alerta-sym ec-map-popup-alerta-sym\" aria-hidden=\"true\">",
      ec_alerta_badge_svg(sev, a$variable %||% a$nombre_abr, size = 18L, direccion = a$direccion),
      "</span>",
      "<span class=\"ec-map-popup-alerta-line-txt\">", esc(linea), "</span>",
      "</div>"
    )
  }, character(1))
  items <- items[nzchar(items)]
  if (length(items) == 0L) return("")
  paste0("<div class=\"ec-map-popup-alertas-resumen\">", paste(items, collapse = ""), "</div>")
}

mapa_popup_alertas_html <- mapa_popup_alertas_resumen_html

mapa_popup_extremos_items <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  if (is.data.frame(x)) return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  if (is.list(x) && !is.null(x$copy)) return(list(x))
  x
}

mapa_popup_extremos_html <- function(avisos, resumenes) {
  avisos <- mapa_popup_extremos_items(avisos)
  resumenes <- mapa_popup_extremos_items(resumenes)
  esc <- htmltools::htmlEscape
  linea <- function(txt) {
    txt <- trimws(as.character(txt %||% ""))
    if (!nzchar(txt)) return("")
    paste0(
      "<div class=\"ec-map-popup-alerta-line\">",
      "<span class=\"ec-map-popup-alerta-line-txt\">", esc(txt), "</span>",
      "</div>"
    )
  }
  bloques <- c(
    vapply(avisos %||% list(), function(a) {
      paste0(
        linea(a$copy %||% a[["copy"]]),
        linea(a$pie %||% a[["pie"]])
      )
    }, character(1)),
    vapply(resumenes %||% list(), function(a) {
      linea(paste0("Resumen: ", a$copy %||% a[["copy"]] %||% ""))
    }, character(1))
  )
  bloques <- bloques[nzchar(bloques)]
  if (!length(bloques)) return("")
  paste0("<div class=\"ec-map-popup-alertas-resumen\">", paste(bloques, collapse = ""), "</div>")
}

mapa_popup_html_tabla <- function(
    nombre, operador, codigo, region, comuna, lon, lat,
    variables_nombres = character(),
    variables_miss_msg = MSG_VARS_DEF,
    ver_datos_input_id = NULL,
    estacion_id = NULL,
    calidad_nivel = NULL,
    score_calidad = NULL,
    pct_completitud = NULL,
    n_imposibles = NULL,
    pct_imposibles = NULL,
    alertas_temprana = list(),
    extremos_avisos = list(),
    extremos_resumenes = list(),
    ubicacion_compartida = character()
  ) {
  # Mismo HTML para desktop (Leaflet) y mobile (sheet); CSS diverge.
  # Orden: título legible+badge op → alertas → calidad → civil → tech → vars → CTAs.
  esc <- htmltools::htmlEscape
  lon_s <- if (is.na(lon)) "\u2014" else format(lon, digits = 6)
  lat_s <- if (is.na(lat)) "\u2014" else format(lat, digits = 6)

  popup_plain <- function(x) {
    s <- trimws(as.character(x %||% "")[1])
    if (is.na(s) || !nzchar(s) || s %in% c("\u2014", "-", "\u2013", "NA")) s <- "\u2014"
    list(value = esc(s), empty = identical(s, "\u2014"))
  }

  partes <- ec_popup_nombre_partes(nombre, operador)
  op_code <- partes$op_code
  if (!nzchar(op_code)) {
    op_try <- toupper(trimws(as.character(operador %||% "")))
    if (nzchar(op_try) && !(op_try %in% c("\u2014", "-", "NA"))) op_code <- op_try
  }
  op_badge <- if (nzchar(op_code)) {
    paste0("<span class=\"ec-map-popup-op-badge\">", esc(op_code), "</span>")
  } else {
    ""
  }

  reg <- popup_plain(region)
  com <- popup_plain(comuna)
  civil_html <- paste0(
    "<div class=\"ec-map-popup-meta ec-map-popup-meta--civil\">",
    "<div class=\"ec-map-popup-row\">",
    "<span class=\"ec-map-popup-label\">Regi\u00f3n</span>",
    "<span class=\"ec-map-popup-value", if (isTRUE(reg$empty)) " ec-map-popup-value--empty" else "", "\">",
    reg$value, "</span></div>",
    "<div class=\"ec-map-popup-row\">",
    "<span class=\"ec-map-popup-label\">Comuna</span>",
    "<span class=\"ec-map-popup-value", if (isTRUE(com$empty)) " ec-map-popup-value--empty" else "", "\">",
    com$value, "</span></div>",
    "</div>"
  )

  coords_s <- if (identical(lon_s, "\u2014") && identical(lat_s, "\u2014")) {
    "\u2014"
  } else {
    paste0(lon_s, ", ", lat_s)
  }
  tech_html <- paste0(
    "<details class=\"ec-map-popup-tech\">",
    "<summary>Detalles t\u00e9cnicos</summary>",
    "<div class=\"ec-map-popup-meta ec-map-popup-meta--tech\">",
    "<div class=\"ec-map-popup-row\">",
    "<span class=\"ec-map-popup-label\">ID</span>",
    "<span class=\"ec-map-popup-value\">", popup_plain(codigo)$value, "</span></div>",
    "<div class=\"ec-map-popup-row ec-map-popup-row--span2\">",
    "<span class=\"ec-map-popup-label\">Coords</span>",
    "<span class=\"ec-map-popup-value",
    if (identical(coords_s, "\u2014")) " ec-map-popup-value--empty" else "", "\">",
    esc(coords_s), "</span></div>",
    "</div></details>"
  )

  calidad_html <- ""
  niv_key <- as.character(calidad_nivel %||% "")
  if (nzchar(niv_key)) {
    niv <- EC_CALIDAD_NIVELES[[niv_key]]
    badge_color <- if (is.null(niv)) "#8A95A0" else niv$color
    badge_lbl <- if (is.null(niv)) niv_key else niv$etiqueta
    sc <- suppressWarnings(as.integer(score_calidad))
    score_txt <- if (!is.na(sc)) paste0(" (", sc, "/100)") else ""
    cal_val <- paste0(
      "<span class=\"ec-map-popup-calidad\"><span class=\"ec-map-tt-dot\" style=\"background:",
      badge_color, "\"></span>", esc(badge_lbl), esc(score_txt), "</span>"
    )
    if (!is.null(pct_completitud) && !is.na(suppressWarnings(as.numeric(pct_completitud)))) {
      cal_val <- paste0(cal_val, " \u00b7 ", round(as.numeric(pct_completitud), 0), "% completitud")
    }
    imp <- suppressWarnings(as.integer(n_imposibles)); if (is.na(imp)) imp <- 0L
    pimp <- suppressWarnings(as.numeric(pct_imposibles)); if (is.na(pimp)) pimp <- 0
    if (imp > 0L) {
      warn_cls <- if (pimp > 1) " ec-map-popup-imp-warn" else ""
      cal_val <- paste0(
        cal_val,
        "<br><span class=\"ec-map-popup-imp", warn_cls, "\">Valores imposibles: ",
        imp, " (", gsub("\\.", ",", sprintf("%.1f", pimp)), "%)</span>"
      )
    }
    cal_cls <- paste(
      "ec-map-popup-row ec-map-popup-row--calidad",
      paste0("ec-map-popup-row--calidad-", gsub("[^a-z0-9_-]", "", tolower(niv_key)))
    )
    calidad_html <- paste0(
      "<div class=\"", cal_cls, "\">",
      "<span class=\"ec-map-popup-value\">", cal_val, "</span>",
      "</div>"
    )
  }

  alertas_html <- paste0(
    mapa_popup_alertas_resumen_html(alertas_temprana),
    mapa_popup_extremos_html(extremos_avisos, extremos_resumenes)
  )

  ubicacion_html <- ""
  if (length(ubicacion_compartida) > 0L) {
    otros <- paste(htmltools::htmlEscape(as.character(ubicacion_compartida)), collapse = ", ")
    ubicacion_html <- paste0(
      "<p class=\"ec-map-popup-ubicacion-compartida\">",
      "Hay ", length(ubicacion_compartida) + 1L,
      " estaciones en este punto. Tambi\u00e9n: ", otros, "</p>"
    )
  }

  vars_body <- if (length(variables_nombres) > 0L) {
    ec_popup_vars_chips_html(variables_nombres)
  } else {
    paste0(
      "<span class=\"ec-map-popup-value ec-map-popup-value--empty\">",
      esc(if (nzchar(variables_miss_msg)) variables_miss_msg else MSG_VARS_DEF),
      "</span>"
    )
  }
  vars_html <- paste0(
    "<div class=\"ec-map-popup-row ec-map-popup-row--vars\">",
    "<span class=\"ec-map-popup-label\">Variables</span>",
    vars_body,
    "</div>"
  )

  btn_html <- ""
  eid <- suppressWarnings(as.integer(estacion_id))
  if (!is.null(ver_datos_input_id) && nzchar(ver_datos_input_id) && !is.na(eid) && eid > 0L) {
    inp <- esc(ver_datos_input_id)
    alias_arg <- partes$display
    if (!nzchar(alias_arg)) alias_arg <- as.character(nombre)[1]
    if (is.na(alias_arg)) alias_arg <- ""
    alias_arg <- gsub("\\", "\\\\", alias_arg, fixed = TRUE)
    alias_arg <- gsub("'", "\\'", alias_arg, fixed = TRUE)
    alias_arg <- gsub("[\r\n]", " ", alias_arg)
    alias_arg <- paste0("'", htmltools::htmlEscape(alias_arg, attribute = TRUE), "'")
    alias_data <- htmltools::htmlEscape(partes$display, attribute = TRUE)
    # PDF por estacion en pausa (PLAN_REPORTE_PERSONALIZADO.md); se reactiva con REPORTE_PDF_HABILITADO=1.
    pdf_on <- identical(trimws(Sys.getenv("REPORTE_PDF_HABILITADO", "0")), "1")
    pdf_btn <- if (pdf_on && identical(niv_key, "excelente")) {
      paste0(
        "<a class=\"ec-map-popup-reporte-pdf\" href=\"/mi-reporte/?e=", eid,
        "\" target=\"_blank\" rel=\"noopener\">",
        "<i class=\"fa fa-file-pdf-o\" aria-hidden=\"true\"></i> Informe PDF</a>"
      )
    } else {
      ""
    }
    btn_html <- paste0(
      "<div class=\"ec-map-popup-actions\">",
      "<div class=\"ec-map-popup-actions-primary\">",
      "<button type=\"button\" class=\"ec-map-popup-ver-datos\" ",
      "data-ec-shiny-input=\"", inp, "\" data-ec-eid=\"", eid, "\" ",
      "onclick=\"if(window.Shiny&&Shiny.setInputValue){Shiny.setInputValue('", inp, "',",
      eid, ",{priority:'event'});}return false;\">",
      "<i class=\"fa fa-play\" aria-hidden=\"true\"></i> Ver datos</button>",
      "</div>",
      "<div class=\"ec-map-popup-actions-secondary\">",
      "<button type=\"button\" class=\"ec-map-popup-descargar ec-map-popup-action-strong\" ",
      "data-ec-descargar-eid=\"", eid, "\" ",
      "onclick=\"window.open('/descargar/index.html?e=", eid, "','_blank');return false;\">",
      "<i class=\"fa fa-download\" aria-hidden=\"true\"></i> Descargar</button>",
      if (nzchar(pdf_btn)) pdf_btn else "",
      "<button type=\"button\" class=\"ec-map-popup-fijar-fav ec-map-popup-action-quiet\" ",
      "data-ec-requires-auth=\"1\" data-ec-eid=\"", eid, "\" data-ec-alias=\"", alias_data, "\" ",
      "onclick=\"if(window.ecFijarDesdeMapa){ecFijarDesdeMapa(", eid, ",", alias_arg, ");}",
      "else if(window.Shiny&&Shiny.setInputValue){Shiny.setInputValue('fijar_favorito_mapa',",
      eid, ",{priority:'event'});}return false;\">",
      "<i class=\"fa fa-star\" aria-hidden=\"true\"></i> Fijar</button>",
      "<button type=\"button\" class=\"ec-map-popup-grupo ec-map-popup-action-quiet\" ",
      "data-ec-requires-auth=\"1\" ",
      "onclick=\"if(window.ecAgregarAlGrupoDesdeMapa){ecAgregarAlGrupoDesdeMapa(", eid, ");}return false;\">",
      "<i class=\"fa fa-object-group\" aria-hidden=\"true\"></i> Al grupo</button>",
      "</div>",
      "</div>"
    )
  }

  paste0(
    "<div class=\"ec-map-popup\">",
    "<div class=\"ec-map-popup-mobile-header\">",
    "<div class=\"ec-map-popup-title-wrap\">",
    "<h2 class=\"ec-map-popup-mobile-title\">", esc(partes$display), "</h2>",
    op_badge,
    "</div>",
    "<button type=\"button\" class=\"ec-map-popup-mobile-close\" aria-label=\"Cerrar detalle\">",
    "<span aria-hidden=\"true\">&times;</span></button>",
    "</div>",
    "<div class=\"ec-map-popup-body\">",
    ubicacion_html,
    alertas_html,
    calidad_html,
    civil_html,
    tech_html,
    vars_html,
    btn_html,
    "</div>",
    "</div>"
  )
}


ec_coords_en_chile <- function(lat, lon) {
  is.finite(lat) && is.finite(lon) &&
    lat >= -56 && lat <= -17 && lon >= -76 && lon <= -66
}

mapa_popup_geo_html <- function(loading = FALSE, fuera_chile = FALSE) {
  body <- if (isTRUE(loading)) {
    paste0(
      "<div class=\"ec-map-popup-row\">",
      "<span class=\"ec-map-popup-value\">Buscando estaciones cercanas...</span>",
      "</div>",
      "<div class=\"ec-map-popup-skeleton\" aria-hidden=\"true\">",
      "<span></span><span></span><span></span>",
      "</div>"
    )
  } else if (isTRUE(fuera_chile)) {
    "<p class=\"ec-map-popup-geo-msg\">Est\u00e1s fuera del \u00e1rea de estaciones de Chile.</p>"
  } else {
    "<p class=\"ec-map-popup-geo-msg\">No hay estaciones cercanas con datos en esta zona.</p>"
  }
  paste0(
    "<div class=\"ec-map-popup ec-map-popup-geo\">",
    "<div class=\"ec-map-popup-mobile-header\">",
    "<h2 class=\"ec-map-popup-mobile-title\">Tu ubicaci\u00f3n</h2>",
    "<button type=\"button\" class=\"ec-map-popup-mobile-close\" aria-label=\"Cerrar detalle\">",
    "<span aria-hidden=\"true\">&times;</span></button>",
    "</div>",
    "<div class=\"ec-map-popup-body\">", body, "</div>",
    "</div>"
  )
}

mapa_popup_geo_cercanas_html <- function(rows) {
  esc <- htmltools::htmlEscape
  if (is.null(rows) || (is.data.frame(rows) && nrow(rows) == 0L)) {
    return(mapa_popup_geo_html(loading = FALSE))
  }
  if (!is.data.frame(rows)) {
    rows <- tryCatch(do.call(rbind, lapply(rows, as.data.frame)), error = function(e) NULL)
  }
  if (is.null(rows) || nrow(rows) == 0L) {
    return(mapa_popup_geo_html(loading = FALSE))
  }
  trs <- vapply(seq_len(nrow(rows)), function(i) {
    eid <- suppressWarnings(as.integer(rows$id[i]))
    nom <- esc(as.character(rows$nombre[i] %||% ""))
    dist <- suppressWarnings(as.numeric(rows$dist_km[i]))
    dist_s <- if (!is.na(dist)) paste0(round(dist, 1), " km") else "\u2014"
    if (is.na(eid) || eid < 1L) return("")
    paste0(
      "<tr><td>", nom, "</td><td>", dist_s, "</td><td>",
      "<button type=\"button\" class=\"ec-map-popup-geo-usar\" ",
      "onclick=\"if(window.Shiny&&Shiny.setInputValue){Shiny.setInputValue('map_geo_pick_eid',",
      eid, ",{priority:'event'});}return false;\">Usar</button>",
      "</td></tr>"
    )
  }, character(1))
  trs <- trs[nzchar(trs)]
  tbl <- if (length(trs) == 0L) {
    "<p class=\"ec-map-popup-geo-msg\">No hay estaciones cercanas con datos.</p>"
  } else {
    paste0(
      "<table class=\"ec-map-popup-geo-table\"><thead><tr>",
      "<th>Estaci\u00f3n</th><th>Dist.</th><th></th>",
      "</tr></thead><tbody>", paste(trs, collapse = ""), "</tbody></table>"
    )
  }
  paste0(
    "<div class=\"ec-map-popup ec-map-popup-geo\">",
    "<div class=\"ec-map-popup-mobile-header\">",
    "<h2 class=\"ec-map-popup-mobile-title\">Estaciones cercanas</h2>",
    "<button type=\"button\" class=\"ec-map-popup-mobile-close\" aria-label=\"Cerrar detalle\">",
    "<span aria-hidden=\"true\">&times;</span></button>",
    "</div>",
    "<div class=\"ec-map-popup-body\">", tbl, "</div>",
    "</div>"
  )
}


mapa_popup_html_loading <- function(nombre, estacion_id = NULL) {
  esc <- htmltools::htmlEscape
  partes <- ec_popup_nombre_partes(nombre)
  op_badge <- if (nzchar(partes$op_code)) {
    paste0("<span class=\"ec-map-popup-op-badge\">", esc(partes$op_code), "</span>")
  } else {
    ""
  }
  paste0(
    "<div class=\"ec-map-popup ec-map-popup-loading\">",
    "<div class=\"ec-map-popup-loadbar\" aria-hidden=\"true\">",
    "<span class=\"ec-map-popup-loadbar-ind\"></span></div>",
    "<div class=\"ec-map-popup-mobile-header\">",
    "<div class=\"ec-map-popup-title-wrap\">",
    "<h2 class=\"ec-map-popup-mobile-title\">", esc(partes$display), "</h2>",
    op_badge,
    "</div>",
    "<button type=\"button\" class=\"ec-map-popup-mobile-close\" aria-label=\"Cerrar detalle\">",
    "<span aria-hidden=\"true\">&times;</span></button>",
    "</div>",
    "<div class=\"ec-map-popup-body\">",
    "<div class=\"ec-map-popup-row\">",
    "<span class=\"ec-map-popup-label\">Estado</span>",
    "<span class=\"ec-map-popup-value\">Cargando detalle de la estaci\u00f3n...</span>",
    "</div>",
    "<div class=\"ec-map-popup-skeleton\" aria-hidden=\"true\">",
    "<span></span><span></span><span></span><span></span>",
    "</div>",
    "</div></div>"
  )
}

# Voyager land (nolabels). Labels: pane ecLabels in ec_icon_overlay.js.
# CARTO requires ?key=; attribution is the free-tier deal.
ec_leaflet_add_positron <- function(map) {
  key <- Sys.getenv("CARTO_BASEMAP_KEY", "")
  attr <- paste0(
    "Datos: <a href=\"https://www.dwd.de/\" rel=\"noopener noreferrer\" target=\"_blank\">DWD</a> (ICON) \u00b7 ",
    "<a href=\"https://open-meteo.com/\" rel=\"noopener noreferrer\" target=\"_blank\">Open-Meteo</a> | ",
    "&copy; <a href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a> ",
    "contributors &copy; <a href=\"https://carto.com/attributions\">CARTO</a>"
  )
  url <- "https://{s}.basemaps.cartocdn.com/rastertiles/voyager_nolabels/{z}/{x}/{y}{r}.png"
  if (nzchar(key)) url <- paste0(url, "?key=", key)
  # crossOrigin: ec_positron_ocean.js reads pixels to punch ocean fill.
  leaflet::addTiles(
    map,
    urlTemplate = url,
    attribution = attr,
    options = leaflet::tileOptions(
      subdomains = "abcd",
      maxZoom = 20,
      crossOrigin = "anonymous",
      noWrap = TRUE,
      updateWhenZooming = FALSE,
      keepBuffer = 4,
      bounds = list(c(-56, -76), c(-17, -66))
    )
  )
}

mapa_popup_options <- function() {
  leaflet::popupOptions(
    closeButton = FALSE,
    maxWidth = 460,
    minWidth = 400,
    # Sin maxHeight: el scrollbar interno cortaba el widget.
    # autoPan off: el JS centra el card del popup (no el marcador).
    className = "ec-station-leaflet-popup",
    autoPan = FALSE,
    keepInView = FALSE,
    offset = c(0, 6)
  )
}

#' @export
mapa_server <- function(id, estaciones, estacion_id, fuentes = NULL, on_ver_datos_mapa = NULL,
                        map_focus_tick = NULL, enabled = NULL,
                        grupo_estaciones_ids = NULL, favorito_eid = NULL,
                        sel_con_zoom = NULL, geo_usuario = NULL, id_variable = NULL,
                        geo_clear_tick = NULL, alertas_mapa = NULL,
                        anticipada_mapa = NULL,
                        on_alerta_click = NULL, map_ready = NULL,
                        alertas_pintadas = NULL, alertas_include_ext = NULL,
                        map_client_gen = NULL, comuna_filtro = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mapa_enabled <- if (is.null(enabled)) function() TRUE else enabled
    map_ready_rv <- map_ready
    alertas_pintadas_rv <- alertas_pintadas
    alertas_include_ext_rv <- alertas_include_ext
    # Parent reactiveVal/function: Date.now() de ec_map_ready (>0 = widget montado).
    map_client_gen_r <- if (is.null(map_client_gen)) {
      function() 1
    } else {
      map_client_gen
    }
    map_client_ok <- function() {
      g <- suppressWarnings(as.numeric(map_client_gen_r()))
      is.finite(g) && g > 0
    }
    chile_bounds <- ec_map_bounds_chile()
    fuentes_r <- if (is.null(fuentes)) function() data.frame(id = integer(), nombre = character()) else fuentes
    usuario_geo_r <- if (is.null(geo_usuario)) function() NULL else geo_usuario
    id_variable_r <- if (is.null(id_variable)) function() "" else id_variable
    geo_clear_r <- if (is.null(geo_clear_tick)) function() 0L else geo_clear_tick
    alertas_mapa_r <- if (is.null(alertas_mapa)) function() NULL else alertas_mapa
    anticipada_mapa_r <- if (is.null(anticipada_mapa)) function() NULL else anticipada_mapa
    on_alerta_click_f <- on_alerta_click
    comuna_filtro_r <- if (is.null(comuna_filtro)) function() "" else comuna_filtro
    last_geo_key <- shiny::reactiveVal("")
    last_client_gen <- shiny::reactiveVal(NULL)

    # Shell Leaflet listo = zoom cliente + montaje real (ec_map_ready), no zoom stale.
    # Fallback server: primer mapa_zoom de la sesión también abre el gate (JS ec_map_ready
    # no siempre propaga; profiler 20260726201417: zoom sí, ec_map_ready no).
    shiny::observe({
      on <- isTRUE(mapa_enabled())
      if (is.null(map_ready_rv)) return()
      if (!on) {
        if (isTRUE(map_ready_rv())) map_ready_rv(FALSE)
        if (!is.null(alertas_pintadas_rv) && isTRUE(alertas_pintadas_rv())) {
          alertas_pintadas_rv(FALSE)
        }
        return()
      }
      zoom_ok <- !is.null(input$mapa_zoom)
      if (zoom_ok && !map_client_ok()) {
        tryCatch({
          map_client_gen_r(as.numeric(Sys.time()) * 1000)
          shiny_app_profile_mark("shiny.map_client_gen_from_zoom")
        }, error = function(e) invisible(NULL))
      }
      shiny::req(zoom_ok)
      shiny::req(map_client_ok())
      if (!isTRUE(map_ready_rv())) {
        map_ready_rv(TRUE)
        shiny_app_profile_mark("shiny.mapa_listo")
      }
    }, priority = 200)

    # Memo corto por eid: evita ×3 detalle / ×2 alertas en el mismo click (sesión auditada).
    mapa_popup_memo_get <- function(kind, eid, fetch_fn, ttl_s = 90) {
      eid <- as.integer(eid)
      if (is.na(eid) || eid < 1L) return(fetch_fn())
      store <- session$userData$mapa_popup_memo
      if (is.null(store)) store <- new.env(parent = emptyenv())
      key <- paste0(kind, ":", eid)
      now <- as.numeric(Sys.time())
      hit <- store[[key]]
      if (!is.null(hit) && is.list(hit) && is.finite(hit$at) && (now - hit$at) < ttl_s) {
        return(hit$val)
      }
      val <- fetch_fn()
      store[[key]] <- list(at = now, val = val)
      session$userData$mapa_popup_memo <- store
      val
    }

    vars_nombres_estacion <- shiny::reactive({
      eid <- estacion_id()
      if (is.null(eid)) return(list(nombres = character(), miss = ""))
      # H5/H1: timeout 8s (antes 25s ≈ barra fantasma); fallar rápido al popup.
      vars_raw <- mapa_popup_memo_get("variables", eid, function() {
        tryCatch(
          api_get(paste0("/estaciones/", eid, "/variables"), timeout = 8),
          error = function(e) NULL
        )
      })
      vdf <- mapa_popup_normalizar_variables_api(vars_raw)
      vnoms <- if (is.null(vdf)) character() else mapa_popup_variables_nombres(vdf)
      miss <- ""
      if (is.null(vars_raw)) {
        miss <- "No se pudieron cargar las variables. Intenta de nuevo."
      } else if (is.null(vdf)) {
        miss <- "No se pudieron cargar las variables. Intenta de nuevo."
      } else if (nrow(vdf) == 0L || length(vnoms) == 0L) {
        miss <- MSG_VARS_DEF
      }
      list(nombres = vnoms, miss = miss)
    })

    popup_datos <- shiny::reactive({
      eid <- estacion_id()
      df <- estaciones()
      f <- fuentes_r()
      if (is.null(eid)) return(NULL)
      sel <- if (!is.null(df) && nrow(df) > 0L && eid %in% df$id) {
        df[df$id == eid, , drop = FALSE]
      } else {
        NULL
      }
      nombre0 <- if (!is.null(sel)) sel$nombre[1] %||% "" else ""
      lat0 <- if (!is.null(sel)) as.numeric(sel$lat[1]) else NA_real_
      lon0 <- if (!is.null(sel)) as.numeric(sel$lon[1]) else NA_real_

      # Si el catálogo ya trae fila usable, parsear desde sel; si no, un solo GET memoizado.
      has_cat <- !is.null(sel) &&
        nzchar(as.character(sel$nombre[1] %||% "")) &&
        is.finite(lat0) && is.finite(lon0)
      if (isTRUE(has_cat)) {
        det_raw <- sel[1, , drop = FALSE]
      } else {
        det_raw <- mapa_popup_memo_get("detalle", eid, function() {
          tryCatch(
            api_get(paste0("/estaciones/", eid), timeout = 8),
            error = function(e) NULL
          )
        })
      }
      det_raw <- mapa_popup_normalizar_detalle_api(det_raw)
      d <- mapa_popup_parse_detalle(det_raw, eid, nombre0, lat0, lon0)

      operador <- "\u2014"
      fid <- d$id_fuente
      if (is.na(fid) && !is.null(sel) && "id_fuente" %in% names(sel)) {
        fid <- suppressWarnings(as.integer(sel$id_fuente[1]))
      }
      if (!is.na(fid) && nrow(f) > 0L) {
        idx <- which(as.integer(f$id) == as.integer(fid))
        if (length(idx) > 0L) {
          operador <- toupper(as.character(f$nombre[idx[1L]] %||% "\u2014"))
        }
      }
      if (identical(operador, "\u2014") || !nzchar(operador)) {
        # Fallback: sufijo en nombre interno (…_DMC) → código de fuente
        partes_nm <- ec_popup_nombre_partes(d$nombre)
        if (nzchar(partes_nm$op_code)) {
          operador <- partes_nm$op_code
          if (nrow(f) > 0L) {
            idx2 <- which(toupper(as.character(f$nombre)) == operador)
            if (length(idx2) > 0L) {
              operador <- toupper(as.character(f$nombre[idx2[1L]]))
            }
          }
        }
      }

      vinfo <- vars_nombres_estacion()
      vnoms <- vinfo$nombres
      miss <- vinfo$miss

      alertas_raw <- mapa_popup_memo_get("alertas", eid, function() {
        tryCatch(
          api_get(paste0("/estaciones/", eid, "/alertas-temprana"), timeout = 5),
          error = function(e) NULL
        )
      })
      alertas_temprana <- list()
      if (is.list(alertas_raw) && !is.null(alertas_raw$alertas)) {
        alertas_temprana <- ec_alertas_normalize_list(alertas_raw$alertas)
      }
      extremos_raw <- mapa_popup_memo_get("extremos", eid, function() {
        tryCatch(
          api_get(paste0("/estaciones/", eid, "/extremos-aviso"), timeout = 5),
          error = function(e) NULL
        )
      })
      extremos_avisos <- if (is.list(extremos_raw) && !is.null(extremos_raw$avisos)) extremos_raw$avisos else list()
      extremos_resumenes <- if (is.list(extremos_raw) && !is.null(extremos_raw$resumenes)) extremos_raw$resumenes else list()
      if (length(alertas_temprana) == 0L && !is.null(sel) && "alerta" %in% names(sel)) {
        al <- ec_catalogo_parse_alerta(list(alerta = sel$alerta[[1]]))
        asp <- suppressWarnings(as.integer(sel$anios_span[1]))
        niv <- ec_alerta_nivel_estacion(sel)
        if (!is.null(al) && ec_alerta_valida(al, asp, niv)) {
          alertas_temprana <- list(al)
        }
      }

      mapa_popup_html_tabla(
        nombre = d$nombre,
        operador = operador,
        codigo = as.character(eid),
        region = d$nom_reg,
        comuna = d$nom_com,
        lon = d$lon,
        lat = d$lat,
        variables_nombres = vnoms,
        variables_miss_msg = if (nzchar(miss)) miss else MSG_VARS_DEF,
        ver_datos_input_id = ns("ver_datos_mapa"),
        estacion_id = eid,
        calidad_nivel = if (!is.null(sel)) sel$nivel[1] %||% sel$cobertura[1] else NULL,
        score_calidad = if (!is.null(sel)) sel$score_calidad[1] else NULL,
        pct_completitud = if (!is.null(sel)) sel$pct_completitud[1] else NULL,
        n_imposibles = if (!is.null(sel)) sel$n_imposibles[1] else NULL,
        pct_imposibles = if (!is.null(sel)) sel$pct_imposibles[1] else NULL,
        alertas_temprana = alertas_temprana,
        extremos_avisos = extremos_avisos,
        extremos_resumenes = extremos_resumenes,
        ubicacion_compartida = if (!is.null(sel)) {
          ec_estaciones_coords_hermanas(shiny::isolate(estaciones()), eid)
        } else {
          character()
        }
      )
    })

    # Fase 1: mapa base (tiles + leyenda). Marcadores en fase 2 via leafletProxy.
    output$mapa <- leaflet::renderLeaflet({
      shiny::req(mapa_enabled())
      b <- ec_map_bounds_chile()
      leaflet::leaflet(options = leaflet::leafletOptions(
        preferCanvas = FALSE,
        # Windy-like: scale one raster, don't crossfade two integer zooms.
        zoomSnap = 0,
        zoomDelta = 0.5,
        wheelPxPerZoomLevel = 100,
        fadeAnimation = FALSE,
        markerZoomAnimation = FALSE
      )) |>
        ec_leaflet_add_positron() |>
        leaflet::fitBounds(b$lng1, b$lat1, b$lng2, b$lat2) |>
        leaflet::addControl(
          html = ec_calidad_leyenda_html(),
          position = "bottomright",
          className = "ec-map-legend-ctrl"
        ) |>
        leaflet::addControl(
          html = ec_alerta_leyenda_html(),
          position = "bottomright",
          className = "ec-map-legend-alerta-ctrl"
        )
    })


    last_popup_html <- shiny::reactiveVal("")

    ec_mostrar_popup_estacion <- function(eid, popup_html, sel = NULL) {
      if (is.null(sel)) {
        df <- shiny::isolate(estaciones())
        if (!is.null(df) && nrow(df) > 0L && eid %in% df$id) {
          sel <- df[df$id == eid, , drop = FALSE]
        }
      }
      tryCatch({
        session$sendCustomMessage("ec_station_sheet", list(
          html = popup_html,
          eid = as.integer(eid)
        ))
      }, error = function(e) invisible(NULL))
      proxy <- leaflet::leafletProxy("mapa", session = session)
      proxy <- leaflet::clearPopups(proxy)
      lat <- if (!is.null(sel)) as.numeric(sel$lat[1]) else NA_real_
      lon <- if (!is.null(sel)) as.numeric(sel$lon[1]) else NA_real_
      if (is.na(lat) || is.na(lon)) {
        mk <- ec_map_coords_estacion(shiny::isolate(estaciones()), eid)
        lat <- mk$lat
        lon <- mk$lon
      }
      if (is.na(lat) || is.na(lon)) return(invisible())
      # layerId fijo: LayerManager reemplaza la capa previa (sin id, cada
      # addPopups apilaba otro popup → “doble borde” que se separa al zoom).
      leaflet::addPopups(proxy,
        lng = lon, lat = lat,
        popup = popup_html,
        layerId = "ec_station_popup",
        options = mapa_popup_options()
      )
      invisible()
    }
    grupo_r <- if (is.null(grupo_estaciones_ids)) function() integer() else grupo_estaciones_ids
    favorito_r <- if (is.null(favorito_eid)) function() NULL else favorito_eid

    # Círculo CSS (mismo tamaño que GPS). El JS pinta el divIcon; sin nombre.
    ec_pintar_favorito_estacion <- function(fav) {
      proxy <- leaflet::leafletProxy("mapa", session = session)
      proxy <- leaflet::clearGroup(proxy, "ec_favorito")
      send <- function(msg) {
        tryCatch(session$sendCustomMessage("ec_pintar_favorito", msg),
                 error = function(e) invisible(NULL))
      }
      if (is.null(fav)) {
        send(list(clear = TRUE))
        return(invisible())
      }
      eid <- NA_integer_
      lat <- NA_real_
      lon <- NA_real_
      if (is.list(fav) && !is.null(fav$eid)) {
        eid <- suppressWarnings(as.integer(fav$eid))
        lat <- suppressWarnings(as.numeric(fav$lat %||% NA_real_))
        lon <- suppressWarnings(as.numeric(fav$lon %||% NA_real_))
      } else {
        eid <- suppressWarnings(as.integer(fav))
      }
      if (is.na(eid) || eid < 1L) {
        send(list(clear = TRUE))
        return(invisible())
      }
      if (!is.finite(lat) || !is.finite(lon)) {
        df <- shiny::isolate(estaciones())
        if (!is.null(df) && nrow(df) > 0L && eid %in% df$id) {
          sel <- df[df$id == eid, , drop = FALSE]
          lat <- as.numeric(sel$lat[1])
          lon <- as.numeric(sel$lon[1])
        }
      }
      if (!is.finite(lat) || !is.finite(lon)) {
        send(list(clear = TRUE))
        return(invisible())
      }
      send(list(eid = eid, lat = lat, lon = lon))
      invisible()
    }

    ec_pintar_grupo_estaciones <- function(ids) {
      proxy <- leaflet::leafletProxy("mapa", session = session)
      proxy <- leaflet::clearGroup(proxy, "ec_grupo")
      if (length(ids) == 0L) return(invisible())
      df <- shiny::isolate(estaciones())
      if (is.null(df) || nrow(df) == 0L) return(invisible())
      sel <- df[df$id %in% ids, , drop = FALSE]
      if (nrow(sel) == 0L) return(invisible())
      leaflet::addCircleMarkers(
        proxy, data = sel, lng = ~lon, lat = ~lat,
        color = "#DF9531", fillColor = "#DF9531", fillOpacity = 0.35,
        radius = 9, weight = 3, group = "ec_grupo",
        layerId = ~paste0("ec_grupo_", id),
        options = ec_map_circle_opts(FALSE)
      )
      invisible()
    }

    ec_limpiar_ubicacion_usuario <- function() {
      tryCatch({
        proxy <- leaflet::leafletProxy("mapa", session = session)
        leaflet::clearGroup(proxy, "ec_usuario")
      }, error = function(e) invisible(NULL))
      invisible()
    }

    ec_clear_geo_ui <- function() {
      last_geo_key("")
      ec_limpiar_ubicacion_usuario()
      session$sendCustomMessage("ec_user_geo_clear", TRUE)
      tryCatch({
        proxy <- leaflet::leafletProxy("mapa", session = session)
        leaflet::clearPopups(proxy)
      }, error = function(e) invisible(NULL))
      invisible()
    }

    ec_pintar_ubicacion_usuario <- function(lat, lon, con_zoom = TRUE) {
      shiny::req(mapa_enabled())
      proxy <- leaflet::leafletProxy("mapa", session = session)
      proxy <- leaflet::clearGroup(proxy, "ec_usuario")
      df <- data.frame(lat = lat, lon = lon, stringsAsFactors = FALSE)
      proxy <- leaflet::addCircleMarkers(
        proxy, data = df, lng = ~lon, lat = ~lat,
        color = "#FFFFFF", fillColor = EC_USUARIO_GEO_COLOR, fillOpacity = 0.95,
        radius = 13, weight = 3.5, group = "ec_usuario",
        layerId = "ec_usuario_marker",
        options = ec_map_circle_opts(TRUE),
        label = "Tu ubicaci\u00f3n",
        labelOptions = leaflet::labelOptions(
          direction = "top", opacity = 0.95,
          className = "ec-map-tooltip-wrap ec-map-usuario-label",
          offset = c(0, -6)
        )
      )
      proxy <- leaflet::addCircleMarkers(
        proxy, data = df, lng = ~lon, lat = ~lat,
        color = EC_USUARIO_GEO_COLOR, fillColor = EC_USUARIO_GEO_COLOR, fillOpacity = 0.2,
        radius = 24, weight = 2, group = "ec_usuario",
        layerId = "ec_usuario_halo",
        options = ec_map_circle_opts(FALSE)
      )
      if (isTRUE(con_zoom)) {
        cur_zoom <- shiny::isolate(input$mapa_zoom)
        z <- if (is.null(cur_zoom) || cur_zoom < 9) 10 else cur_zoom
        proxy <- leaflet::setView(proxy, lng = lon, lat = lat, zoom = z)
      }
      invisible()
    }

    ec_repintar_ubicacion_usuario <- function() {
      g <- shiny::isolate(usuario_geo_r())
      if (is.null(g)) return(invisible())
      lat <- suppressWarnings(as.numeric(g$lat))
      lon <- suppressWarnings(as.numeric(g$lon))
      if (is.na(lat) || is.na(lon)) return(invisible())
      ec_pintar_ubicacion_usuario(lat, lon, con_zoom = FALSE)
      invisible()
    }

    ec_mostrar_popup_geo <- function(lat, lon, popup_html) {
      proxy <- leaflet::leafletProxy("mapa", session = session)
      proxy <- leaflet::clearPopups(proxy)
      leaflet::addPopups(proxy, lng = lon, lat = lat, popup = popup_html,
        layerId = "ec_station_popup",
        options = mapa_popup_options())
      invisible()
    }

    ec_cargar_cercanas_geo <- function(lat, lon) {
      if (!ec_coords_en_chile(lat, lon)) {
        session$sendCustomMessage("ec_fav_toast", list(
          text = "Est\u00e1s fuera del \u00e1rea de estaciones de Chile.",
          ok = FALSE
        ))
        ec_mostrar_popup_geo(lat, lon, mapa_popup_geo_html(fuera_chile = TRUE))
        return(invisible())
      }
      params <- list(
        lat = lat, lon = lon, limit = 5,
        con_datos = "1", chile_only = "1", minimal = "1"
      )
      vid <- id_variable_r()
      if (!is.null(vid) && nzchar(vid)) {
        vid_n <- suppressWarnings(as.integer(vid))
        if (!is.na(vid_n) && vid_n > 0L) params$id_variable <- vid_n
      }
      dat <- tryCatch(
        api_get("/estaciones/cercanas", params, timeout = API_TIMEOUT_ESTACIONES),
        error = function(e) NULL
      )
      rows <- if (is.null(dat)) NULL else dat$data %||% dat
      if (is.list(rows) && !is.data.frame(rows) && length(rows) > 0L) {
        rows <- tryCatch(do.call(rbind, lapply(rows, as.data.frame)), error = function(e) NULL)
      }
      popup_html <- mapa_popup_geo_cercanas_html(rows)
      ec_mostrar_popup_geo(lat, lon, popup_html)
      if (is.null(rows) || (is.data.frame(rows) && nrow(rows) == 0L)) {
        session$sendCustomMessage("ec_fav_toast", list(
          text = "No hay estaciones cercanas con datos.",
          ok = FALSE
        ))
      }
      invisible()
    }

    shiny::observeEvent(geo_clear_r(), {
      shiny::req(mapa_enabled())
      ec_clear_geo_ui()
    }, ignoreInit = TRUE)

    ec_pintar_alertas_livianas <- function(raw, horizonte_largo = FALSE) {
      proxy <- leaflet::leafletProxy("mapa", session = session)
      proxy <- leaflet::clearGroup(proxy, "ec_alertas")
      df_all <- ec_alertas_mapa_normalize(raw, horizonte_largo)
      ant_raw <- tryCatch(anticipada_mapa_r(), error = function(e) NULL)
      df_all <- ec_alertas_sumar_anticipada(df_all, ant_raw)
      if (is.null(df_all) || nrow(df_all) == 0L) {
        ec_actualizar_leyenda_alertas(NULL, horizonte_largo)
        if (!is.null(alertas_pintadas_rv) && !isTRUE(alertas_pintadas_rv())) {
          alertas_pintadas_rv(TRUE)
          shiny_app_profile_mark("shiny.alertas_pintadas", list(n = 0L))
        }
        return(invisible())
      }
      cnt <- as.list(ec_alerta_counts_from_alertas_df(df_all))
      df <- ec_alertas_mapa_one_per_station(df_all)
      # Un solo addMarkers + iconos deduplicados por (sev,var,dir) — evita N makeIcon.
      # Tamano fijo (no codifica severidad); mismo SVG/color que la leyenda.
      sz <- EC_MAP_ALERTA_ICON_PX
      sev <- as.character(df$severidad %||% "")
      var <- as.character(df$variable %||% "")
      dir <- as.character(df$direccion %||% "")
      buckets <- vapply(seq_len(nrow(df)), function(i) {
        k <- ec_alerta_leyenda_bucket(var[i], dir[i])
        if (is.na(k)) "other" else k
      }, character(1))
      keys <- paste(sev, var, dir, sep = "\t")
      uniq <- unique(keys)
      url_by_key <- character(length(uniq))
      names(url_by_key) <- uniq
      for (k in uniq) {
        parts <- strsplit(k, "\t", fixed = TRUE)[[1]]
        svg <- ec_alerta_badge_svg(parts[[1]], parts[[2]], size = sz, direccion = parts[[3]])
        url_by_key[[k]] <- paste0(
          "data:image/svg+xml;charset=UTF-8,",
          utils::URLencode(svg, reserved = TRUE)
        )
      }
      anchor_y <- as.integer(8L + round(sz * 10 / 22))
      class_names <- paste0("ec-map-alerta-marker ec-map-alerta-bucket-", buckets)
      proxy <- leaflet::addMarkers(
        proxy,
        lng = df$lon,
        lat = df$lat,
        icon = leaflet::icons(
          iconUrl = unname(url_by_key[keys]),
          iconWidth = sz,
          iconHeight = sz,
          iconAnchorX = -4L,
          iconAnchorY = anchor_y,
          className = class_names
        ),
        layerId = as.character(df$id),
        group = "ec_alertas",
        options = leaflet::markerOptions(interactive = TRUE, keyboard = FALSE)
      )
      tryCatch({
        session$sendCustomMessage("ec_map_alerta_legend_counts", cnt)
        session$sendCustomMessage("ec_map_alerta_horizonte_sync", list(
          checked = isTRUE(horizonte_largo)
        ))
      }, error = function(e) invisible(NULL))
      if (!is.null(alertas_pintadas_rv) && !isTRUE(alertas_pintadas_rv())) {
        alertas_pintadas_rv(TRUE)
        shiny_app_profile_mark("shiny.alertas_pintadas", list(n = nrow(df)))
      }
      invisible()
    }

    ec_actualizar_leyenda_alertas <- function(df, horizonte_largo = FALSE) {
      cnt <- if (is.null(df) || (!is.data.frame(df) && !is.list(df))) {
        as.list(ec_alerta_leyenda_counts_empty())
      } else {
        as.list(ec_alerta_leyenda_counts(df, horizonte_largo))
      }
      tryCatch({
        session$sendCustomMessage("ec_map_alerta_legend_counts", cnt)
        session$sendCustomMessage("ec_map_alerta_horizonte_sync", list(
          checked = isTRUE(horizonte_largo)
        ))
      }, error = function(e) invisible(NULL))
    }

    ec_repintar_alertas_mapa <- function(df, horizonte_largo = FALSE) {
      # Compat: si llega df de estaciones con alerta embebida, ignora — usamos /alertas/mapa
      raw <- shiny::isolate(alertas_mapa_r())
      ec_pintar_alertas_livianas(raw, horizonte_largo)
      invisible()
    }

    ec_pintar_marcadores_estaciones <- function(df, ajustar_vista = TRUE, horizonte_largo = FALSE) {
      proxy <- leaflet::leafletProxy("mapa", session = session)
      proxy <- leaflet::clearGroup(proxy, "ec_estaciones")
      if (is.null(df) || nrow(df) == 0L) {
        return(invisible())
      }
      df <- ec_prepare_estaciones_mapa_df(df)
      df <- ec_map_offset_coords_duplicadas(df)
      df_plot <- ec_map_df_para_pintar(df)
      if (ajustar_vista) {
        b <- ec_map_bounds_from_df(df)
        proxy <- leaflet::fitBounds(proxy, b$lng1, b$lat1, b$lng2, b$lat2)
      }
      leaflet::addCircleMarkers(
        proxy, data = df_plot,
        lng = ~lon, lat = ~lat,
        color = "#FFFFFF", weight = ~._weight,
        fillColor = ~._color, fillOpacity = ~._opac,
        radius = ~._radio, layerId = ~id,
        group = "ec_estaciones",
        options = ec_map_circle_opts(TRUE),
        label = ~nombre,
        labelOptions = leaflet::labelOptions(
          direction = "top", opacity = 0.9,
          className = "ec-map-tooltip-wrap",
          offset = c(0, -4)
        )
      )
      pts <- lapply(seq_len(nrow(df_plot)), function(i) {
        list(id = as.integer(df_plot$id[i]), lat = as.numeric(df_plot$lat[i]), lng = as.numeric(df_plot$lon[i]))
      })
      tryCatch(session$sendCustomMessage("ec_map_station_points", pts), error = function(e) invisible(NULL))
      ec_pintar_favorito_estacion(shiny::isolate(favorito_r()))
      ec_pintar_grupo_estaciones(shiny::isolate(grupo_r()))
      invisible()
    }

    ec_estaciones_paint_key <- function(df, horizonte_largo = FALSE) {
      if (is.null(df) || nrow(df) == 0L) return(NULL)
      df_alertas <- ec_map_df_con_alerta_activa(df, horizonte_largo)
      has_alerta <- if ("alerta_mapa" %in% names(df_alertas)) {
        vapply(seq_len(nrow(df_alertas)), function(i) {
          asp <- if ("anios_span" %in% names(df_alertas)) df_alertas$anios_span[i] else NA_integer_
          niv <- if ("nivel" %in% names(df_alertas)) df_alertas$nivel[i] else if ("cobertura" %in% names(df_alertas)) df_alertas$cobertura[i] else NULL
          ec_alerta_valida(df_alertas$alerta_mapa[[i]], asp, niv)
        }, logical(1))
      } else {
        rep(FALSE, nrow(df_alertas))
      }
      alerta_sig <- if (any(has_alerta)) {
        sum(df$id[has_alerta], na.rm = TRUE) %% 999983L
      } else {
        0L
      }
      paste0(
        nrow(df), ":",
        min(df$id, na.rm = TRUE), ":",
        max(df$id, na.rm = TRUE), ":",
        sum(df$id, na.rm = TRUE) %% 999983L, ":",
        sum(has_alerta), ":", alerta_sig, ":",
        if (isTRUE(horizonte_largo)) "H16" else "H5"
      )
    }

    marcadores_paint_key <- shiny::reactiveVal(NULL)
    last_map_enabled <- shiny::reactiveVal(NA)
    # Fuente de verdad server-side del horizonte (FALSE = 5 dias por defecto).
    # Evita depender del round-trip asincrono de updateCheckboxInput, que
    # generaba una carrera: el repintado leia input$alerta_horizonte_ext aun
    # en TRUE y mostraba las alertas de 16 dias antes de que el reset llegara.
    horizonte_largo_r <- shiny::reactiveVal(FALSE)

    # Remount: invalidar paint key para re-disparar leafletProxy contra widget nuevo.
    shiny::observeEvent(map_client_gen_r(), {
      g <- suppressWarnings(as.numeric(map_client_gen_r()))
      shiny::req(is.finite(g) && g > 0)
      prev <- last_client_gen()
      last_client_gen(g)
      if (is.null(prev)) return()
      marcadores_paint_key(NULL)
      if (!is.null(alertas_pintadas_rv) && isTRUE(alertas_pintadas_rv())) {
        alertas_pintadas_rv(FALSE)
      }
    }, ignoreInit = TRUE)

    shiny::observe({
      on <- isTRUE(mapa_enabled())
      prev <- last_map_enabled()
      if (!is.na(prev) && !prev && on) {
        horizonte_largo_r(FALSE)
        marcadores_paint_key(NULL)
        shiny::updateCheckboxInput(session, "alerta_horizonte_ext", value = FALSE)
        tryCatch(
          session$sendCustomMessage("ec_map_alerta_horizonte_sync", list(checked = FALSE)),
          error = function(e) invisible(NULL)
        )
      }
      last_map_enabled(on)
    })

    shiny::observeEvent(input$alerta_horizonte_ext, {
      shiny::req(mapa_enabled())
      shiny::req(!is.null(input$mapa_zoom))
      shiny::req(map_client_ok())
      horizonte_largo <- isTRUE(input$alerta_horizonte_ext)
      horizonte_largo_r(horizonte_largo)
      if (horizonte_largo && !is.null(alertas_include_ext_rv) &&
          !isTRUE(alertas_include_ext_rv())) {
        # Dispara refetch con include_ext=1; el observe de paint repinta al llegar.
        alertas_include_ext_rv(TRUE)
        return()
      }
      ec_pintar_alertas_livianas(alertas_mapa_r(), horizonte_largo)
    }, ignoreInit = TRUE)

    # Badges de evento extremo: Shiny siempre pinta el grupo; el overlay
    # Alertas (ec_choropleth.js) los muestra u oculta. Prioridad alta: T_load.
    shiny::observe({
      shiny::req(mapa_enabled())
      shiny::req(!is.null(input$mapa_zoom))
      shiny::req(map_client_ok())
      raw <- alertas_mapa_r()
      shiny::req(!is.null(raw))
      ec_pintar_alertas_livianas(raw, isTRUE(horizonte_largo_r()))
    }, priority = 15)

    # Espera mapa Leaflet listo (zoom + ec_map_ready) antes de pintar via proxy.
    shiny::observe({
      shiny::req(mapa_enabled())
      df <- estaciones()
      shiny::req(!is.null(input$mapa_zoom))
      shiny::req(map_client_ok())
      horizonte_largo <- isTRUE(horizonte_largo_r())
      if (is.null(df) || nrow(df) == 0L) {
        proxy <- leaflet::leafletProxy("mapa", session = session)
        leaflet::clearGroup(proxy, "ec_estaciones")
        marcadores_paint_key("0")
        return()
      }
      key <- ec_estaciones_paint_key(df, horizonte_largo)
      shiny::req(!is.null(key))
      prev <- marcadores_paint_key()
      if (identical(key, prev)) return()
      # Set key before fitBounds so zoom invalidation cannot re-enter paint.
      marcadores_paint_key(key)
      geo_activo <- !is.null(shiny::isolate(usuario_geo_r()))
      # Favorito/seleccion owns viewport via setView — avoid fitBounds fight.
      # Comuna: el choropleth hace fitBounds al shapefile; no pisar con estaciones.
      tiene_sel <- !is.null(shiny::isolate(estacion_id()))
      tiene_comuna <- nzchar(trimws(as.character(comuna_filtro_r() %||% "")))
      ec_pintar_marcadores_estaciones(
        df,
        ajustar_vista = !geo_activo && !tiene_sel && !tiene_comuna,
        horizonte_largo = horizonte_largo
      )
      eid <- shiny::isolate(estacion_id())
      if (!is.null(eid)) {
        ec_dibujar_seleccion(eid, con_popup = FALSE, con_zoom = FALSE)
      }
      ec_pintar_favorito_estacion(favorito_r())
      ec_pintar_grupo_estaciones(grupo_r())
    })

    shiny::observe({
      shiny::req(mapa_enabled())
      shiny::req(!is.null(input$mapa_zoom))
      shiny::req(map_client_ok())
      ec_pintar_favorito_estacion(favorito_r())
      ec_pintar_grupo_estaciones(grupo_r())
    })

    # Resalta la estacion seleccionada (marcador naranja + popup) y centra la
    # vista SIN reconstruir el mapa ni resetear el zoom. Rapido via leafletProxy.
    ec_dibujar_seleccion <- function(eid, con_popup = TRUE, con_zoom = TRUE) {
      proxy <- leaflet::leafletProxy("mapa", session = session)
      proxy <- leaflet::clearGroup(proxy, "ec_sel")
      proxy <- leaflet::clearPopups(proxy)
      if (is.null(eid)) return(invisible())
      df <- shiny::isolate(estaciones())
      sel <- NULL
      if (!is.null(df) && nrow(df) > 0L && eid %in% df$id) {
        sel <- df[df$id == eid, , drop = FALSE]
      } else {
        raw <- shiny::isolate(alertas_mapa_r())
        adf <- ec_alertas_mapa_normalize(raw, isTRUE(shiny::isolate(horizonte_largo_r())))
        if (!is.null(adf) && nrow(adf) > 0L && eid %in% adf$id) {
          row <- adf[adf$id == eid, , drop = FALSE]
          sel <- data.frame(
            id = eid, nombre = paste0("Estaci\u00f3n ", eid),
            lat = row$lat[1], lon = row$lon[1], stringsAsFactors = FALSE
          )
        }
      }
      if (is.null(sel) || nrow(sel) == 0L) return(invisible())
      # Halo de selección decorativo: sin hit-target (el marcador de estación debajo recibe el click)
      proxy <- leaflet::addCircleMarkers(proxy, data = sel, lng = ~lon, lat = ~lat,
        color = "#E85D24", fillColor = "#E85D24", fillOpacity = 0.9,
        radius = 12, weight = 2, group = "ec_sel", layerId = paste0("ec_sel_", eid),
        options = ec_map_circle_opts(FALSE))
      if (con_zoom) {
        # Solo subir zoom si está muy lejos. El centrado lo hace el JS sobre
        # el popup (no setView al marcador — cortaba el tope del widget).
        cur_zoom <- shiny::isolate(input$mapa_zoom)
        if (is.null(cur_zoom) || cur_zoom < 9) {
          proxy <- leaflet::setView(proxy, lng = sel$lon[1], lat = sel$lat[1], zoom = 10)
        }
      }
      if (con_popup) {
        popup_html <- mapa_popup_html_loading(sel$nombre[1] %||% "", eid)
        ec_mostrar_popup_estacion(eid, popup_html, sel)
      }
      invisible()
    }


    shiny::observe({
      shiny::req(mapa_enabled())
      eid <- estacion_id()
      if (is.null(eid)) {
        last_popup_html("")
        return()
      }
      html <- popup_datos()
      shiny::req(!is.null(html))
      prev <- shiny::isolate(last_popup_html())
      if (identical(prev, html)) return()
      last_popup_html(html)
      ec_mostrar_popup_estacion(eid, html)
    }, priority = -1)
    # Clic en estacion / seleccion desde el menu: resaltar + centrar (sin rebuild).
    shiny::observeEvent(estacion_id(), {
      con_zoom <- FALSE
      if (!is.null(sel_con_zoom)) {
        con_zoom <- isTRUE(shiny::isolate(sel_con_zoom()))
        if (con_zoom) sel_con_zoom(FALSE)
      }
      ec_dibujar_seleccion(estacion_id(), con_popup = con_zoom, con_zoom = con_zoom)
    }, ignoreNULL = FALSE)

    if (!is.null(map_focus_tick)) {
      shiny::observeEvent(map_focus_tick(), {
        shiny::req(mapa_enabled())
        eid <- shiny::isolate(estacion_id())
        html <- shiny::isolate({
          h <- last_popup_html()
          if (nzchar(as.character(h %||% ""))) h else if (!is.null(eid)) popup_datos() else NULL
        })
        ec_dibujar_seleccion(eid, con_popup = FALSE, con_zoom = TRUE)
        if (!is.null(html) && nzchar(as.character(html))) {
          ec_mostrar_popup_estacion(eid, as.character(html))
        }
      }, ignoreInit = TRUE)
    }

    shiny::observeEvent(input$ver_datos_mapa, {
      eid <- suppressWarnings(as.integer(input$ver_datos_mapa))
      if (is.na(eid) || eid < 1L) return()
      estacion_id(eid)
      if (is.function(on_ver_datos_mapa)) on_ver_datos_mapa(eid)
      shiny::updateNumericInput(session, "ver_datos_mapa", value = 0)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$mapa_marker_click, {
      click <- input$mapa_marker_click
      if (is.null(click$id)) return()
      eid <- suppressWarnings(as.integer(click$id))
      if (is.na(eid) || eid < 1L) {
        # Overlays con layerId prefijado (por si algún halo queda interactive)
        m <- regmatches(as.character(click$id),
          regexec("^ec_(?:sel|fav_halo|fav_core|grupo)_([0-9]+)$", as.character(click$id)))[[1]]
        if (length(m) >= 2L) eid <- suppressWarnings(as.integer(m[[2]]))
      }
      if (is.na(eid) || eid < 1L) return()
      if (!is.null(sel_con_zoom)) sel_con_zoom(TRUE)
      curr <- shiny::isolate(estacion_id())
      # Misma estación otra vez: reactiveVal no dispara. Reabrir con HTML
      # cacheado — NO loading: el observe de popup_datos ve HTML idéntico y
      # no reemplaza, dejando "Cargando..." colgado.
      if (!is.null(curr) && identical(as.integer(curr), as.integer(eid))) {
        html <- shiny::isolate({
          h <- last_popup_html()
          if (nzchar(as.character(h %||% ""))) h else popup_datos()
        })
        ec_dibujar_seleccion(eid, con_popup = FALSE, con_zoom = TRUE)
        if (!is.null(html) && nzchar(as.character(html))) {
          ec_mostrar_popup_estacion(eid, as.character(html))
        }
        return()
      }
      if (is.function(on_alerta_click_f)) {
        # Cubre click en icono de alerta (sin fila en estaciones aun)
        tryCatch(on_alerta_click_f(eid), error = function(e) {
          estacion_id(eid)
        })
      } else {
        estacion_id(eid)
      }
    }, ignoreNULL = TRUE)
  })
}
