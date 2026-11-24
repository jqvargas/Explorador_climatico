# Resumen de alertas: etiquetas, resultado de una prevista y match "fue prevista".
# Sin DB. El query vive en alertas_resumen_query.R.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
  }
}

# Misma regla que el motor tardio/temprano: entra si el ranking es <= 10
# (1 + cuantos dias historicos son estrictamente mas extremos). El correo
# solo manda rank <= 3; eso es un filtro de envio, no el umbral de la alerta.
# No aplica el margen de calibracion: ese corte es aparte del top-10.
ALERTA_RESUMEN_RANK_MAX <- 10L
ALERTA_RESUMEN_DIAS <- 30L

.ALERTA_RESUMEN_SMALL <- c("en", "de", "del", "la", "las", "los", "el", "y", "a", "e")

# Fenomeno ya usado en las tarjetas (digest_view titlePhrase, forma suave).
# Lo que no esta aqui no lleva frase inventada: se usa el nombre de la variable.
.ALERTA_RESUMEN_ETIQUETA <- c(
  "t_max|max" = "Calor poco habitual",
  "t_max|min" = "Fr\u00edo poco habitual",
  "t_min|max" = "Calor poco habitual",
  "t_min|min" = "Fr\u00edo poco habitual",
  "pp|max" = "Lluvia poco habitual",
  "pp|min" = "Poca lluvia"
)

.ALERTA_RESUMEN_VAR <- c(
  pp = "Precipitaci\u00f3n",
  t_max = "Temperatura m\u00e1xima",
  t_min = "Temperatura m\u00ednima",
  u_rafaga = "R\u00e1faga"
)

.ALERTA_RESUMEN_TEMP <- c("t_max", "t_min", "t_mean")

alerta_resumen_estado <- function(estado) {
  x <- tolower(trimws(as.character(estado %||% "")))
  if (x %in% c("previstas", "temprana")) return("previstas")
  if (x %in% c("confirmadas", "tardia")) return("confirmadas")
  "todas"
}

alerta_resumen_tipo_sql <- function(estado) {
  est <- alerta_resumen_estado(estado)
  if (identical(est, "previstas")) return("temprana")
  if (identical(est, "confirmadas")) return("tardia")
  ""
}

alerta_resumen_title_es <- function(s) {
  s <- s %||% ""
  if (length(s) != 1L || is.na(s)) return("")
  s <- enc2utf8(as.character(s))
  s <- gsub("_(DMC|DGA|INIA|CEAZA|SINCA|AGROMET)$", "", s, ignore.case = TRUE, perl = TRUE)
  s <- gsub("_", " ", s, fixed = TRUE)
  s <- gsub("\\s+", " ", trimws(s), perl = TRUE)
  if (!nzchar(s)) return("")
  words <- strsplit(s, " ", fixed = TRUE)[[1]]
  words <- words[nzchar(words)]
  if (!length(words)) return("")
  out <- vapply(seq_along(words), function(i) {
    w <- words[[i]]
    low <- tolower(w)
    if (i > 1L && low %in% .ALERTA_RESUMEN_SMALL) return(low)
    resto <- if (nchar(w) > 1L) tolower(substring(w, 2L)) else ""
    paste0(toupper(substring(w, 1L, 1L)), resto)
  }, character(1))
  paste(out, collapse = " ")
}

alerta_resumen_nombre_estacion <- function(nombre, nombre_publico = "") {
  pub <- trimws(enc2utf8(as.character(nombre_publico %||% "")))
  if (length(pub) != 1L || is.na(pub)) pub <- ""
  base <- if (nzchar(pub)) pub else nombre
  alerta_resumen_title_es(base)
}

alerta_resumen_variable_nombre <- function(abr, nombre = "") {
  abr <- tolower(trimws(as.character(abr %||% "")))
  if (nzchar(abr) && abr %in% names(.ALERTA_RESUMEN_VAR)) {
    return(unname(.ALERTA_RESUMEN_VAR[[abr]]))
  }
  alerta_resumen_title_es(gsub("_", " ", as.character(nombre %||% ""), fixed = TRUE))
}

alerta_resumen_etiqueta <- function(abr, direccion, variable_nombre = "") {
  abr <- tolower(trimws(as.character(abr %||% "")))
  dir <- tolower(trimws(as.character(direccion %||% "")))
  if (!identical(dir, "min")) dir <- "max"
  key <- paste(abr, dir, sep = "|")
  if (key %in% names(.ALERTA_RESUMEN_ETIQUETA)) {
    return(list(etiqueta = unname(.ALERTA_RESUMEN_ETIQUETA[[key]]), definida = TRUE))
  }
  fb <- alerta_resumen_variable_nombre(abr, variable_nombre)
  if (!nzchar(fb)) fb <- "Valor poco habitual"
  list(etiqueta = fb, definida = FALSE)
}

alerta_resumen_ordinal <- function(n) {
  n <- suppressWarnings(as.integer(n))
  if (length(n) != 1L || is.na(n) || n < 1L) return("")
  paste0(n, ".\u00ba")
}

alerta_resumen_ranking <- function(abr, direccion, ranking) {
  ord <- alerta_resumen_ordinal(ranking)
  if (!nzchar(ord)) return("")
  abr <- tolower(trimws(as.character(abr %||% "")))
  dir <- tolower(trimws(as.character(direccion %||% "")))
  if (!identical(dir, "min")) dir <- "max"
  if (abr %in% .ALERTA_RESUMEN_TEMP) {
    cola <- if (identical(dir, "min")) "m\u00e1s fr\u00edo" else "m\u00e1s c\u00e1lido"
  } else {
    cola <- if (identical(dir, "min")) "valor m\u00e1s bajo" else "valor m\u00e1s alto"
  }
  paste(ord, cola)
}

# n_mas_extremos: dias distintos con valor estrictamente mas extremo.
# ranking = 1 + n_mas. Se cumplio si ranking <= rank_max (motor: 10).
alerta_resumen_resultado <- function(fecha, hoy, tiene_obs, n_mas_extremos,
                                     rank_max = ALERTA_RESUMEN_RANK_MAX) {
  fecha <- tryCatch(as.Date(fecha), error = function(e) as.Date(NA))
  hoy <- tryCatch(as.Date(hoy), error = function(e) as.Date(NA))
  if (length(fecha) != 1L || length(hoy) != 1L || is.na(fecha) || is.na(hoy)) {
    return("sin_dato")
  }
  obs <- isTRUE(tiene_obs)
  if (fecha > hoy || (fecha == hoy && !obs)) return("pendiente")
  if (!obs) return("sin_dato")
  n <- suppressWarnings(as.integer(n_mas_extremos))
  if (length(n) != 1L || is.na(n) || n < 0L) return("sin_dato")
  corte <- suppressWarnings(as.integer(rank_max))
  if (length(corte) != 1L || is.na(corte) || corte < 1L) corte <- ALERTA_RESUMEN_RANK_MAX
  if ((1L + n) <= corte) "cumplio" else "no_cumplio"
}

alerta_resumen_resultado_etiqueta <- function(codigo) {
  switch(
    as.character(codigo %||% ""),
    cumplio = "Se cumpli\u00f3",
    no_cumplio = "No se cumpli\u00f3",
    pendiente = "Pendiente",
    sin_dato = "Sin dato",
    ""
  )
}

alerta_resumen_clave <- function(eid, variable_id, direccion, fecha) {
  dir <- tolower(trimws(as.character(direccion %||% "")))
  if (!identical(dir, "min")) dir <- "max"
  f <- tryCatch(as.character(as.Date(fecha)), error = function(e) "")
  paste(suppressWarnings(as.integer(eid)), suppressWarnings(as.integer(variable_id)), dir, f, sep = "|")
}

alerta_resumen_fue_prevista <- function(clave, claves_previstas) {
  if (length(claves_previstas) == 0L || !nzchar(clave %||% "")) return(FALSE)
  isTRUE(clave %in% as.character(claves_previstas))
}

alerta_resumen_metricas <- function(resultados, errores) {
  resultados <- as.character(resultados %||% character(0))
  errores <- suppressWarnings(as.numeric(errores %||% numeric(0)))
  ok <- resultados == "cumplio"
  no <- resultados == "no_cumplio"
  res <- ok | no
  err <- errores[res]
  err <- err[is.finite(err)]
  list(
    aciertos = as.integer(sum(ok)),
    resueltas = as.integer(sum(res)),
    falsas_alarmas = as.integer(sum(no)),
    mae = if (length(err)) as.numeric(mean(err)) else NA_real_
  )
}
