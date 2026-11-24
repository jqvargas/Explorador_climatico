# Motor de reglas: texto interpretativo para el reporte PDF.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

reporte_interp_tendencia <- function(var_nombre, mk, agg = "mean", nombre_abr = "") {
  mk <- mk %||% list()
  n_years <- mk$n_years %||% 0L
  tau <- mk$tau; p <- mk$p; sen <- mk$sen_slope
  abr <- tolower(trimws(as.character(nombre_abr %||% "")))
  unidad <- if (identical(agg, "sum")) {
    "mm/a\u00f1o"
  } else if (abr %in% c("u2_mean", "u_max_diario", "u2_min", "u_10_kt", "u_rafaga") ||
             grepl("viento", tolower(as.character(var_nombre %||% "")))) {
    "km/h/a\u00f1o"
  } else {
    "\u00b0C/a\u00f1o"
  }

  if (!isTRUE(mk$available)) {
    reason <- mk$reason %||% paste0(
      "Serie insuficiente para test de tendencia (",
      n_years, " a\u00f1o", if (n_years != 1L) "s" else "",
      " de registro; se requieren m\u00ednimo ", REPORTE_MIN_YEARS_MK, " a\u00f1os)."
    )
    return(reason)
  }
  if (is.na(tau) || is.na(p)) {
    return(paste0("No fue posible calcular la tendencia de **", var_nombre, "**."))
  }

  sig <- !is.na(p) && p < 0.05
  dir <- if (!is.na(tau) && tau > 0) "creciente" else if (!is.na(tau) && tau < 0) "decreciente" else "estable"

  base <- if (sig) {
    paste0("**", var_nombre, "** muestra una tendencia ", dir,
           " estad\u00edsticamente significativa en el per\u00edodo analizado")
  } else {
    paste0("**", var_nombre, "** no muestra una tendencia estad\u00edsticamente significativa en el per\u00edodo analizado")
  }

  stats <- paste0("(τ = ", reporte_fmt_num(tau, 3), ", p = ", reporte_fmt_num(p, 2), ")")
  sen_txt <- if (!is.na(sen)) {
    paste0("; pendiente de Sen = ", reporte_fmt_num(sen, 3), " ", unidad)
  } else ""
  paste0(base, " ", stats, sen_txt, ".")
}

reporte_interp_spi <- function(var_nombre, spi_val, spi_meta = list()) {
  n_months <- spi_meta$n_months %||% 0L
  if (!isTRUE(spi_meta$available)) {
    reason <- spi_meta$reason %||% paste0(
      "SPI no disponible (", n_months, " meses en ajuste; se requieren m\u00ednimo ",
      REPORTE_MIN_MONTHS_SPI, ")."
    )
    return(reason)
  }
  if (is.na(spi_val)) {
    return(paste0("No fue posible calcular el SPI del mes en curso para **", var_nombre, "**."))
  }

  cond <- if (spi_val >= 2) "extremadamente h\u00famedo"
  else if (spi_val >= 1.5) "muy h\u00famedo"
  else if (spi_val >= 1) "moderadamente h\u00famedo"
  else if (spi_val > -1) "cerca de lo normal"
  else if (spi_val > -1.5) "sequ\u00eda moderada"
  else if (spi_val > -2) "sequ\u00eda severa"
  else "sequ\u00eda extrema"

  paste0("**", var_nombre, "**: ", cond, " (SPI = ", reporte_fmt_num(spi_val, 2), ").")
}

reporte_interp_enso <- function(var_nombre, en, ref_n = NULL) {
  en <- en %||% list()
  n <- en$n %||% 0L
  r <- en$r

  if (!isTRUE(en$available)) {
    reason <- en$reason %||% paste0(
      "Correlaci\u00f3n ENSO no disponible (n = ", n,
      " meses; se requieren m\u00ednimo ", REPORTE_MIN_MONTHS_ENSO, ")."
    )
    return(reason)
  }
  if (is.na(r)) {
    return(paste0("No fue posible estimar la correlaci\u00f3n con ONI para **", var_nombre, "**."))
  }

  abs_r <- abs(r)
  fuerza <- if (abs_r >= 0.5) "fuerte" else if (abs_r >= 0.3) "moderada" else "d\u00e9bil"
  signo <- if (r > 0) "positiva" else "negativa"

  note <- ""
  if (!is.null(ref_n) && n > 0 && ref_n > 0 && n < ref_n * 0.5) {
    note <- paste0(" Muestra reducida (n = ", n, " vs ~", ref_n,
                   " de otras variables); interpretar con cautela.")
  }

  paste0("**", var_nombre, "**: correlaci\u00f3n ", fuerza, " ", signo,
         " con ONI (r = ", reporte_fmt_num(r, 3), ", n = ", n, " meses).", note)
}

reporte_interp_extremos <- function(var_nombre, ext, agg = "mean") {
  ext <- ext %||% list(max = list(), min = list(), streaks = list())
  lines <- character(0)

  if (length(ext$max) > 0) {
    top <- ext$max[[1]]
    lines <- c(lines, paste0(
      "El evento m\u00e1ximo registrado de **", var_nombre, "** fue ",
      reporte_fmt_num(top$valor), " el ", top$fecha, "."
    ))
  }

  if (identical(agg, "sum")) {
    st <- ext$streaks %||% list()
    dry <- st$max_dry_streak %||% 0L
    wet <- st$max_wet_streak %||% 0L
    if (dry > 0) {
      lines <- c(lines, paste0(
        "La racha seca m\u00e1xima alcanz\u00f3 ", dry, " d\u00edas consecutivos",
        if (!is.null(st$max_dry_start) && !is.na(st$max_dry_start)) {
          paste0(" (", st$max_dry_start, " a ", st$max_dry_end, ")")
        } else "", "."
      ))
    }
    if (wet > 0) {
      lines <- c(lines, paste0(
        "La racha h\u00fameda m\u00e1xima alcanz\u00f3 ", wet, " d\u00edas consecutivos",
        if (!is.null(st$max_wet_start) && !is.na(st$max_wet_start)) {
          paste0(" (", st$max_wet_start, " a ", st$max_wet_end, ")")
        } else "", "."
      ))
    }
  } else if (length(ext$min) > 0) {
    bot <- ext$min[[1]]
    lines <- c(lines, paste0(
      "El m\u00ednimo hist\u00f3rico de **", var_nombre, "** fue ",
      reporte_fmt_num(bot$valor), " el ", bot$fecha, "."
    ))
  }

  if (length(lines) == 0) {
    return(paste0("Sin eventos extremos destacables para **", var_nombre, "**."))
  }
  paste(lines, collapse = " ")
}

reporte_interp_resumen_var <- function(vs) {
  parts <- c(
    reporte_interp_tendencia(vs$nombre, vs$mann_kendall, vs$agg %||% "mean", vs$nombre_abr %||% "")
  )
  if ((vs$agg %||% "") == "sum") {
    spi_meta <- vs$availability$spi %||% list()
    spi_val <- reporte_spi_current_value(vs)
    parts <- c(parts, reporte_interp_spi(vs$nombre, spi_val, spi_meta))
  }
  sv <- vs$sin_validar %||% list()
  if (!is.na(sv$fecha %||% NA)) {
    parts <- c(parts, paste0(
      "\u00daltimo registro (sin validar): ", sv$fecha, " = ", reporte_fmt_num(sv$valor), "."
    ))
  }
  paste(parts, collapse = " ")
}

reporte_max_enso_n <- function(state) {
  ns <- vapply(state$variables %||% list(), function(v) {
    (v$enso %||% list())$n %||% 0L
  }, integer(1))
  if (length(ns) == 0) return(0L)
  max(ns, na.rm = TRUE)
}
