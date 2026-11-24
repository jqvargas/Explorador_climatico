# Catalogo unificado alertas tardias + tempranas (6 tipos)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

parse_bool <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  if (is.logical(x)) return(isTRUE(x[[1]]))
  if (is.numeric(x)) return(!is.na(x[[1]]) && x[[1]] != 0)
  tolower(trimws(as.character(x[[1]]))) %in% c("true", "t", "1", "yes", "y", "si", "s\u00ed")
}

# Catalogo unificado alertas tardias + tempranas.
# u_max_diario fuera: gate vigente = 10 a + 365 d + excelente; masa 2026-10-08 = 2 pares
# (GLACIAR SAN FRANCISCO, HITO SILALA). 0 pares a 20 a. No bajar el gate.
# u_rafaga fuera aunque haya masa: 110 est, 75 con 10 a + 365 d, 40 al gate (2026-10-08).
# No hay pronostico de rafaga guardado (carta_del_mirador id 19 = 0). No es viento a 10 m.
# Rankings de extrema = reporte de estacion (etapa 4). Cubo del mapa apagado (P39).
ALERTAS_CATALOGO <- list(
  list(abr = "pp",     direccion = "max", widget = "maximos", solo_max = TRUE),
  list(abr = "t_max",  direccion = "max", widget = "maximos"),
  list(abr = "t_max",  direccion = "min", widget = "minimos"),
  list(abr = "t_min",  direccion = "min", widget = "minimos"),
  list(abr = "t_min",  direccion = "max", widget = "maximos")
)

alertas_catalogo_abrs <- function() {
  abrs <- vapply(ALERTAS_CATALOGO, function(x) x$abr, character(1))
  unique(abrs)
}

alertas_specs_por_abr <- function(abr) {
  abr <- tolower(trimws(abr %||% ""))
  Filter(function(x) identical(tolower(x$abr), abr), ALERTAS_CATALOGO)
}

alertas_es_solo_max <- function(abr) {
  specs <- alertas_specs_por_abr(abr)
  length(specs) > 0L && all(vapply(specs, function(s) isTRUE(s$solo_max), logical(1)))
}

alertas_catalog_spec <- function(abr, direccion) {
  abr <- tolower(trimws(abr %||% ""))
  dir <- tolower(trimws(direccion %||% ""))
  for (s in ALERTAS_CATALOGO) {
    if (identical(tolower(s$abr), abr) && identical(tolower(s$direccion), dir)) return(s)
  }
  NULL
}

#' Spec de widget para ranking de alerta (decreasing segun widget id).
alertas_widget_spec <- function(abr, direccion, vtype = NULL, nombre_var = NULL) {
  cat_spec <- alertas_catalog_spec(abr, direccion)
  if (is.null(cat_spec)) return(NULL)
  if (exists("reporte_widget_defs", mode = "function")) {
    vt <- vtype %||% if (exists("reporte_detect_var_type", mode = "function")) {
      reporte_detect_var_type(nombre_var %||% abr %||% "")
    } else {
      "generic"
    }
    defs <- reporte_widget_defs(vt)
    hit <- defs[vapply(defs, function(d) identical(d$id, cat_spec$widget), logical(1))]
    if (length(hit) > 0L) return(hit[[1L]])
  }
  list(
    id = cat_spec$widget,
    decreasing = identical(cat_spec$widget, "maximos"),
    skipDaily = identical(cat_spec$widget, "minimos") && identical(tolower(abr), "pp")
  )
}

alertas_specs_to_widgets <- function(abr, vtype = NULL, nombre_var = NULL) {
  specs <- alertas_specs_por_abr(abr)
  if (length(specs) == 0L) return(list())
  widgets <- lapply(specs, function(s) {
    alertas_widget_spec(s$abr, s$direccion, vtype = vtype, nombre_var = nombre_var)
  })
  Filter(Negate(is.null), widgets)
}
