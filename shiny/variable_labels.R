# Convenio de nombres para el usuario.
#
# El código (nombre_abr) no cambia. El nombre dice el estadístico, no la fuente.
#   Predominante = moda del promedio de 10 min (u_10_kt / u_10_dire)
#   Máximo diario = el máximo del día, no la ráfaga (u_max_diario)
#   Ráfaga = máximo instantáneo del día (u_rafaga). Ráfaga horaria = u_rafaga_h
#   Medio / Mínimo = terna diaria (u2_mean / u2_min)
#   Dirección = u2_dire. No es la predominante.
# Picker/reporte: Title Case. Alertas/popup: misma palabra, minúscula en el apellido.

EC_VARIABLE_LABELS <- c(
  precipitacion              = "Precipitaci\u00f3n",
  temperatura_media          = "Temperatura Media",
  temperatura_maxima         = "Temperatura M\u00e1xima",
  temperatura_minima         = "Temperatura M\u00ednima",
  caudal                     = "Caudal",
  niveles_pozos              = "Nivel de Pozos",
  viento_velocidad           = "Viento Predominante",
  viento_direccion           = "Direcci\u00f3n del Viento Predominante",
  horas_frio                 = "Horas de Fr\u00edo",
  cloud_octas                = "Nubosidad (octas)",
  humedad_relativa_media     = "Humedad Relativa Media",
  presion                    = "Presi\u00f3n Atmosf\u00e9rica",
  material_particulado_2_5     = "Material Particulado 2.5",
  radiacion_solar_media      = "Radiaci\u00f3n Solar Media",
  viento_medio               = "Viento Medio",
  viento_maximo              = "Viento M\u00e1ximo Diario",
  viento_minimo              = "Viento M\u00ednimo"
)

EC_VARIABLE_LABELS_BY_ID <- stats::setNames(
  unname(EC_VARIABLE_LABELS),
  as.character(seq_along(EC_VARIABLE_LABELS))
)
EC_VARIABLE_LABELS <- c(
  EC_VARIABLE_LABELS,
  viento_maximo_diario = "Viento M\u00e1ximo Diario",
  viento_maximo_dmc = "R\u00e1faga",
  viento_rafaga = "R\u00e1faga",
  viento_rafaga_horaria = "R\u00e1faga Horaria",
  viento_direccion_dga = "Direcci\u00f3n del Viento"
)
EC_VARIABLE_LABELS_BY_ID[["19"]] <- "R\u00e1faga"
EC_VARIABLE_LABELS_BY_ID[["20"]] <- "Direcci\u00f3n del Viento"
EC_VARIABLE_LABELS_BY_ID[["21"]] <- "R\u00e1faga Horaria"

EC_VARIABLE_LABELS_ABR <- c(
  pp = "Precipitaci\u00f3n",
  t_mean = "Temperatura Media",
  t_max = "Temperatura M\u00e1xima",
  t_min = "Temperatura M\u00ednima",
  q = "Caudal",
  pz = "Nivel de Pozos",
  u_10_kt = "Viento Predominante",
  u_10_dire = "Direcci\u00f3n del Viento Predominante",
  hf = "Horas de Fr\u00edo",
  co = "Nubosidad (octas)",
  hr_mean = "Humedad Relativa Media",
  pa = "Presi\u00f3n Atmosf\u00e9rica",
  mp_25 = "Material Particulado 2.5",
  rs_mean = "Radiaci\u00f3n Solar Media",
  u2_mean = "Viento Medio",
  u_max_diario = "Viento M\u00e1ximo Diario",
  u2_min = "Viento M\u00ednimo",
  u_rafaga = "R\u00e1faga",
  u_rafaga_h = "R\u00e1faga Horaria",
  u2_dire = "Direcci\u00f3n del Viento"
)

EC_VARIABLE_LABELS_ABR_CORTA <- c(
  pp = "Precipitaci\u00f3n",
  t_max = "Temp. m\u00e1xima",
  t_min = "Temp. m\u00ednima",
  u_10_kt = "Viento predominante",
  u_10_dire = "Dir. viento predominante",
  u2_mean = "Viento medio",
  u_max_diario = "M\u00e1ximo diario",
  u2_min = "Viento m\u00ednimo",
  u_rafaga = "R\u00e1faga",
  u_rafaga_h = "R\u00e1faga horaria",
  u2_dire = "Dir. viento"
)

EC_VARIABLE_RANK_TAIL <- c(
  pp = "m\u00e1s lluvioso",
  t_max = "m\u00e1s caliente",
  t_min = "m\u00e1s fr\u00edo",
  u_10_kt = "predominante m\u00e1s intenso",
  u_max_diario = "m\u00e1ximo diario m\u00e1s intenso",
  u2_mean = "medio m\u00e1s alto",
  u2_min = "m\u00ednimo m\u00e1s bajo",
  u_rafaga = "r\u00e1faga m\u00e1s intensa"
)

ec_strip_accents <- function(x) {
  y <- tryCatch(iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT"), error = function(e) as.character(x))
  if (length(y) != 1L || is.na(y)) as.character(x) else y
}

ec_variable_key <- function(nombre) {
  raw <- if (is.null(nombre)) "" else ec_strip_accents(trimws(tolower(as.character(nombre))))
  if (!nzchar(raw)) return("")
  gsub("[\\s\\-]+", "_", raw)
}

ec_variable_label_fallback <- function(nombre) {
  key <- ec_variable_key(if (is.null(nombre)) "" else nombre)
  if (!nzchar(key)) return("")
  parts <- strsplit(key, "_", fixed = TRUE)[[1L]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) return(as.character(nombre))
  paste(vapply(parts, function(p) {
    if (nchar(p) <= 1L) toupper(p) else paste0(toupper(substr(p, 1L, 1L)), substr(p, 2L, nchar(p)))
  }, character(1)), collapse = " ")
}

ec_variable_label_abr <- function(abr, kind = "full") {
  a <- tolower(trimws(as.character(if (is.null(abr) || length(abr) == 0L) "" else abr[1])))
  if (!nzchar(a)) return("")
  tbl <- switch(
    as.character(kind),
    corta = EC_VARIABLE_LABELS_ABR_CORTA,
    rank = EC_VARIABLE_RANK_TAIL,
    EC_VARIABLE_LABELS_ABR
  )
  if (a %in% names(tbl)) unname(tbl[[a]]) else ""
}

#' Etiqueta legible para una variable (nombre BD, abr o id).
ec_variable_label <- function(nombre, id = NULL) {
  if (!is.null(id) && !is.na(id)) {
    id_chr <- as.character(as.integer(id))
    if (nzchar(id_chr) && id_chr %in% names(EC_VARIABLE_LABELS_BY_ID)) {
      return(unname(EC_VARIABLE_LABELS_BY_ID[[id_chr]]))
    }
  }
  raw <- trimws(as.character(if (is.null(nombre) || length(nombre) == 0L) "" else nombre[1]))
  abr_lab <- ec_variable_label_abr(raw, "full")
  if (nzchar(abr_lab)) return(abr_lab)
  key <- ec_variable_key(raw)
  if (nzchar(key) && key %in% names(EC_VARIABLE_LABELS)) {
    return(unname(EC_VARIABLE_LABELS[[key]]))
  }
  ec_variable_label_fallback(raw)
}

ec_label_variables_df <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L || !"nombre" %in% names(df)) {
    return(df)
  }
  has_id <- "id" %in% names(df)
  has_abr <- "nombre_abr" %in% names(df)
  df$nombre <- vapply(seq_len(nrow(df)), function(i) {
    if (has_abr) {
      lab <- ec_variable_label_abr(df$nombre_abr[i], "full")
      if (nzchar(lab)) return(lab)
    }
    ec_variable_label(df$nombre[i], id = if (has_id) df$id[i] else NULL)
  }, character(1))
  df
}

ec_label_variable_vector <- function(nombres, ids = NULL) {
  n <- length(nombres)
  if (n == 0L) return(character())
  if (is.null(ids)) ids <- rep(NA_integer_, n)
  vapply(seq_len(n), function(i) ec_variable_label(nombres[i], id = ids[i]), character(1))
}
