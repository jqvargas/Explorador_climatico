# Helpers de filtrado de estaciones (explorador) — misma idea que descargar/app.js

EC_CALIDAD_ETIQUETA <- c(
  excelente = "Excelente",
  buena = "Buena",
  regular = "Regular",
  limitada = "Limitada"
)

ec_calidad_nivel_key <- function(nivel) {
  key <- tolower(trimws(as.character(nivel %||% "")))
  ifelse(key %in% names(EC_CALIDAD_ETIQUETA), key, "")
}

ec_calidad_chip_html <- function(nivel) {
  key <- ec_calidad_nivel_key(nivel)
  if (!nzchar(key)) return("")
  lab <- EC_CALIDAD_ETIQUETA[[key]]
  paste0(
    "<span class=\"ec-cal-chip\" title=\"", lab, "\">",
    "<span class=\"ec-cal-dot ec-cal-dot-", key, "\" aria-hidden=\"true\"></span>",
    "<span class=\"ec-cal-lab\">", lab, "</span>",
    "</span>"
  )
}

ec_catalogo_cal_cols <- function() {
  c(
    "n_variables", "anios_datos", "anios_span", "registros",
    "pct_completitud", "pct_continuidad", "n_imposibles", "pct_imposibles",
    "score_calidad", "nivel", "fecha_max", "anio_ultimo", "datos_vigentes"
  )
}

ec_catalogo_parse_conteos <- function(x) {
  if (is.null(x) || identical(x, NA)) {
    return(list(pp = 0L, cold = 0L, heat = 0L, wind = 0L))
  }
  if (is.data.frame(x)) {
    if (nrow(x) == 0L) return(list(pp = 0L, cold = 0L, heat = 0L, wind = 0L))
    x <- as.list(x[1, , drop = FALSE])
  }
  list(
    pp = max(0L, as.integer(x$pp %||% 0L)),
    cold = max(0L, as.integer(x$cold %||% 0L)),
    heat = max(0L, as.integer(x$heat %||% 0L)),
    wind = max(0L, as.integer(x$wind %||% 0L))
  )
}

ec_catalogo_parse_alerta <- function(x) {
  if (is.null(x)) return(NULL)
  a <- if (is.list(x) && !is.null(x$alerta)) x$alerta else x
  if (is.null(a) || identical(a, NA)) return(NULL)
  if (is.data.frame(a)) {
    if (nrow(a) == 0L) return(NULL)
    a <- as.list(a[1, , drop = FALSE])
  }
  if (is.list(a) && length(a) == 0) return(NULL)
  if (!is.list(a) || is.null(a$variable) || !nzchar(as.character(a$variable))) return(NULL)
  list(
    variable = as.character(a$variable),
    severidad = as.character(a$severidad %||% ""),
    ranking = suppressWarnings(as.integer(a$ranking)),
    fecha_objetivo = as.character(a$fecha_objetivo %||% ""),
    valor_pronostico = suppressWarnings(as.numeric(a$valor_pronostico)),
    unidad = as.character(a$unidad %||% ""),
    direccion = as.character(a$direccion %||% ""),
    confianza = as.character(a$confianza %||% "alta")
  )
}

ec_catalogo_empty_df <- function() {
  cal_cols <- ec_catalogo_cal_cols()
  df <- data.frame(
    id = integer(), nombre = character(), lat = numeric(), lon = numeric(),
    macrozona = character(), cod_reg = integer(), nom_reg = character(),
    nom_com = character(), cuenca = character(), subcuenca = character(),
    stringsAsFactors = FALSE
  )
  for (c in cal_cols) {
    df[[c]] <- switch(c,
      nivel = character(),
      fecha_max = character(),
      pct_completitud = numeric(), pct_continuidad = numeric(),
      pct_imposibles = numeric(), registros = numeric(),
      datos_vigentes = logical(),
      integer()
    )
  }
  df$cobertura <- character()
  df$alerta <- vector("list", 0L)
  df$alerta_extendida <- vector("list", 0L)
  df$alerta_conteos <- vector("list", 0L)
  df$alerta_conteos_extendida <- vector("list", 0L)
  df
}

ec_catalogo_normalize_row <- function(x) {
  anios <- as.integer(x$anios_span %||% x$anios_datos %||% 0)
  nivel <- as.character(x$nivel %||% "")
  cob <- if (nzchar(nivel)) nivel else NA_character_
  data.frame(
    id = as.integer(x$id %||% NA),
    nombre = as.character(x$nombre %||% ""),
    lat = as.numeric(x$lat %||% NA),
    lon = as.numeric(x$lon %||% NA),
    macrozona = as.character(x$macrozona %||% ""),
    cod_reg = suppressWarnings(as.integer(x$cod_reg %||% NA_integer_)),
    nom_reg = as.character(x$nom_reg %||% ""),
    nom_com = as.character(x$nom_com %||% ""),
    cuenca = as.character(x$cuenca %||% x$nom_cuen %||% ""),
    subcuenca = as.character(x$subcuenca %||% x$nom_subc %||% ""),
    n_variables = as.integer(x$n_variables %||% 0),
    anios_datos = anios,
    anios_span = anios,
    registros = as.numeric(x$registros %||% 0),
    pct_completitud = as.numeric(x$pct_completitud %||% 0),
    pct_continuidad = as.numeric(x$pct_continuidad %||% 0),
    n_imposibles = as.integer(x$n_imposibles %||% 0),
    pct_imposibles = as.numeric(x$pct_imposibles %||% 0),
    score_calidad = suppressWarnings(as.integer(x$score_calidad %||% NA)),
    nivel = nivel,
    fecha_max = as.character(x$fecha_max %||% ""),
    anio_ultimo = suppressWarnings(as.integer(x$anio_ultimo %||% NA_integer_)),
    datos_vigentes = isTRUE(x$datos_vigentes),
    cobertura = cob,
    alerta = I(list(ec_catalogo_parse_alerta(x))),
    alerta_extendida = I(list(ec_catalogo_parse_alerta(list(alerta = x$alerta_extendida)))),
    alerta_conteos = I(list(ec_catalogo_parse_conteos(x$alerta_conteos))),
    alerta_conteos_extendida = I(list(ec_catalogo_parse_conteos(x$alerta_conteos_extendida))),
    stringsAsFactors = FALSE
  )
}

ec_catalogo_to_df <- function(dat) {
  empty_df <- ec_catalogo_empty_df()
  if (is.null(dat)) return(empty_df)
  cal_cols <- ec_catalogo_cal_cols()

  if (is.data.frame(dat)) {
    for (c in c("id", "nombre", "lat", "lon", "macrozona", "cod_reg", "nom_reg", "nom_com", "cuenca", "subcuenca")) {
      if (!c %in% names(dat)) dat[[c]] <- NA
    }
    for (c in cal_cols) {
      if (!c %in% names(dat)) {
        dat[[c]] <- switch(c,
          nivel = "",
          fecha_max = "",
          pct_completitud = 0, pct_continuidad = 0, pct_imposibles = 0, registros = 0,
          datos_vigentes = FALSE,
          0L
        )
      }
    }
    if (!"cobertura" %in% names(dat)) {
      dat$cobertura <- ifelse(!is.na(dat$nivel) & nzchar(dat$nivel), as.character(dat$nivel), NA_character_)
    }
    if (!"alerta" %in% names(dat)) {
      dat$alerta <- vector("list", nrow(dat))
    } else {
      dat$alerta <- lapply(seq_len(nrow(dat)), function(i) ec_catalogo_parse_alerta(list(alerta = dat$alerta[[i]])))
    }
    if (!"alerta_extendida" %in% names(dat)) {
      dat$alerta_extendida <- vector("list", nrow(dat))
    } else {
      dat$alerta_extendida <- lapply(seq_len(nrow(dat)), function(i) {
        ec_catalogo_parse_alerta(list(alerta = dat$alerta_extendida[[i]]))
      })
    }
    if (!"alerta_conteos" %in% names(dat)) {
      dat$alerta_conteos <- replicate(nrow(dat), list(pp = 0L, cold = 0L, heat = 0L, wind = 0L), simplify = FALSE)
    } else {
      dat$alerta_conteos <- lapply(seq_len(nrow(dat)), function(i) ec_catalogo_parse_conteos(dat$alerta_conteos[[i]]))
    }
    if (!"alerta_conteos_extendida" %in% names(dat)) {
      dat$alerta_conteos_extendida <- dat$alerta_conteos
    } else {
      dat$alerta_conteos_extendida <- lapply(seq_len(nrow(dat)), function(i) {
        ec_catalogo_parse_conteos(dat$alerta_conteos_extendida[[i]])
      })
    }
    keep <- c("id", "nombre", "lat", "lon", "macrozona", "cod_reg", "nom_reg", "nom_com",
              "cuenca", "subcuenca", cal_cols, "cobertura", "alerta", "alerta_extendida",
              "alerta_conteos", "alerta_conteos_extendida")
    keep <- keep[keep %in% names(dat)]
    return(dat[!is.na(dat$id), keep, drop = FALSE])
  }

  if (!is.list(dat) || length(dat) == 0) return(empty_df)
  rows <- lapply(dat, ec_catalogo_normalize_row)
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(empty_df)
  do.call(rbind, rows)
}

ec_catalogo_filtrar_geo <- function(df, region = "", comuna = "") {
  if (is.null(df) || nrow(df) == 0) return(df)
  out <- df
  if (!is.null(region) && nzchar(region)) {
    rid <- suppressWarnings(as.integer(region))
    if (!is.na(rid)) out <- out[out$cod_reg == rid, , drop = FALSE]
  }
  if (!is.null(comuna) && nzchar(comuna)) {
    com <- trimws(as.character(comuna))
    nom <- trimws(as.character(out$nom_com))
    out <- out[!is.na(nom) & nom == com, , drop = FALSE]
  }
  out
}

ec_region_para_comunas <- function(input_region, region_aplicada) {
  r <- as.character(region_aplicada %||% "")
  if (!nzchar(r) || r == "0") r <- as.character(input_region %||% "")
  if (!nzchar(r) || r == "0") return("")
  r
}

ec_catalogo_region_choices <- function(df) {
  ch <- c("Region" = "")
  if (is.null(df) || nrow(df) == 0) return(ch)
  ok <- !is.na(df$cod_reg)
  if (!any(ok)) return(ch)
  u <- unique(df[ok, c("cod_reg", "nom_reg"), drop = FALSE])
  u <- u[order(u$nom_reg, u$cod_reg), , drop = FALSE]
  ids <- as.character(u$cod_reg)
  labs <- as.character(u$nom_reg)
  labs <- ifelse(nzchar(labs), labs, ids)
  ch <- c(ch, stats::setNames(ids, labs))
  ch[!duplicated(ch)]
}

ec_catalogo_comuna_choices <- function(df, region_id = "") {
  ch <- c("Comuna" = "")
  if (is.null(df) || nrow(df) == 0) return(ch)
  scoped <- df
  if (!is.null(region_id) && nzchar(region_id)) {
    rid <- suppressWarnings(as.integer(region_id))
    if (!is.na(rid)) scoped <- scoped[scoped$cod_reg == rid, , drop = FALSE]
  }
  coms <- unique(trimws(as.character(scoped$nom_com)))
  coms <- coms[!is.na(coms) & nzchar(coms)]
  if (length(coms) == 0) return(ch)
  coms <- sort(coms)
  c(ch, stats::setNames(coms, coms))
}

ec_finalizar_estaciones_mapa <- function(df, ocultar_limitada = FALSE) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df <- df[!is.na(df$lat) & df$lat >= -56 & df$lat <= -17 & df$lon >= -76 & df$lon <= -66, , drop = FALSE]
  miss <- is.na(df$cobertura) | !nzchar(df$cobertura)
  if (any(miss)) {
    df$cobertura[miss] <- ec_clasificar_cobertura(df$anios_datos[miss], df$n_variables[miss])
  }
  if (isTRUE(ocultar_limitada)) {
    df <- df[df$cobertura != "limitada", , drop = FALSE]
  }
  df
}

ec_revalidar_filtros_geo <- function(session, catalogo, region_ap_rv, comuna_ap_rv, estacion_id_rv) {
  if (is.null(catalogo)) return(invisible(FALSE))
  reg <- region_ap_rv()
  com <- comuna_ap_rv()

  if (nzchar(reg)) {
    reg_ch <- ec_catalogo_region_choices(catalogo)
    if (!reg %in% reg_ch) {
      region_ap_rv("")
      reg <- ""
    }
  }
  if (nzchar(com)) {
    com_ch <- ec_catalogo_comuna_choices(catalogo, reg)
    if (!com %in% com_ch) {
      comuna_ap_rv("")
    }
  }
  invisible(TRUE)
}

# --- Geo conteos (boot liviano coropletico) ---

ec_geo_conteos_as_list <- function(dat) {
  if (is.null(dat) || !is.list(dat)) {
    return(list(macrozonas = list(), regiones = list(), comunas = list()))
  }
  rows <- function(x) {
    if (is.null(x)) return(list())
    if (is.data.frame(x)) {
      if (nrow(x) == 0L) return(list())
      return(lapply(seq_len(nrow(x)), function(i) {
        row <- as.list(x[i, , drop = FALSE])
        lapply(row, function(v) if (length(v) == 1L) v[[1]] else v)
      }))
    }
    if (!is.list(x)) return(list())
    x
  }
  list(
    macrozonas = rows(dat$macrozonas),
    regiones = rows(dat$regiones),
    comunas = rows(dat$comunas)
  )
}

ec_geo_region_choices <- function(conteos) {
  ch <- c("Region" = "")
  regs <- conteos$regiones %||% list()
  if (is.data.frame(regs)) {
    if (nrow(regs) == 0L) return(ch)
    regs <- lapply(seq_len(nrow(regs)), function(i) as.list(regs[i, , drop = FALSE]))
  }
  if (!is.list(regs) || length(regs) == 0L) return(ch)
  ids <- character()
  labs <- character()
  for (r in regs) {
    if (is.null(r)) next
    cr <- suppressWarnings(as.integer(r$cod_reg %||% NA))
    if (is.na(cr)) next
    ids <- c(ids, as.character(cr))
    labs <- c(labs, as.character(r$nom_reg %||% paste0("Region ", cr)))
  }
  if (length(ids) == 0L) return(ch)
  ord <- order(labs, ids)
  c(ch, stats::setNames(ids[ord], labs[ord]))
}

ec_geo_comuna_choices <- function(conteos, region_id = "") {
  ch <- c("Comuna" = "")
  coms <- conteos$comunas %||% list()
  if (is.data.frame(coms)) {
    if (nrow(coms) == 0L) return(ch)
    coms <- lapply(seq_len(nrow(coms)), function(i) as.list(coms[i, , drop = FALSE]))
  }
  if (!is.list(coms) || length(coms) == 0L) return(ch)
  rid <- suppressWarnings(as.integer(region_id))
  names_out <- character()
  for (c in coms) {
    if (is.null(c)) next
    if (!is.na(rid) && rid > 0L) {
      cr <- suppressWarnings(as.integer(c$cod_reg %||% NA))
      if (is.na(cr) || cr != rid) next
    }
    nom <- trimws(as.character(c$nom_com %||% ""))
    if (nzchar(nom)) names_out <- c(names_out, nom)
  }
  names_out <- sort(unique(names_out))
  if (length(names_out) == 0L) return(ch)
  c(ch, stats::setNames(names_out, names_out))
}

ec_geo_mz_id_for_cod_reg <- function(cod_reg) {
  cr <- suppressWarnings(as.integer(cod_reg))
  if (is.na(cr)) return("")
  map <- list(
    "1" = "norte", "2" = "norte", "15" = "norte",
    "3" = "centro_norte", "4" = "centro_norte",
    "5" = "centro", "6" = "centro", "7" = "centro", "13" = "centro",
    "8" = "sur", "9" = "sur", "10" = "sur", "14" = "sur", "16" = "sur",
    "11" = "austral", "12" = "austral"
  )
  as.character(map[[as.character(cr)]] %||% "")
}

ec_estacion_detalle_to_row <- function(info) {
  if (is.null(info) || !is.list(info)) return(NULL)
  eid <- suppressWarnings(as.integer(info$id %||% NA))
  if (is.na(eid) || eid < 1L) return(NULL)
  lat <- suppressWarnings(as.numeric(info$lat %||% NA))
  lon <- suppressWarnings(as.numeric(info$lon %||% NA))
  if (is.na(lat) || is.na(lon)) return(NULL)
  data.frame(
    id = eid,
    nombre = as.character(info$nombre %||% paste0("Estaci\u00f3n ", eid)),
    lat = lat,
    lon = lon,
    macrozona = as.character(info$macrozona %||% ""),
    cod_reg = suppressWarnings(as.integer(info$cod_reg %||% NA)),
    nom_reg = as.character(info$nom_reg %||% ""),
    nom_com = as.character(info$nom_com %||% ""),
    cuenca = as.character(info$cuenca %||% info$nom_cuen %||% ""),
    subcuenca = as.character(info$subcuenca %||% info$nom_subc %||% ""),
    n_variables = 0L,
    anios_datos = 0L,
    anios_span = 0L,
    registros = 0L,
    pct_completitud = 0,
    pct_continuidad = 0,
    n_imposibles = 0L,
    pct_imposibles = 0,
    score_calidad = 0L,
    nivel = "",
    fecha_max = "",
    anio_ultimo = NA_integer_,
    datos_vigentes = FALSE,
    cobertura = "regular",
    alerta = I(list(NULL)),
    alerta_extendida = I(list(NULL)),
    alerta_conteos = I(list(list(pp = 0L, cold = 0L, heat = 0L, wind = 0L))),
    alerta_conteos_extendida = I(list(list(pp = 0L, cold = 0L, heat = 0L, wind = 0L))),
    stringsAsFactors = FALSE
  )
}

ec_alertas_mapa_normalize <- function(dat, horizonte_largo = FALSE) {
  if (is.null(dat) || !is.list(dat)) return(data.frame())
  key <- if (isTRUE(horizonte_largo)) "alertas_ext" else "alertas"
  rows <- dat[[key]] %||% dat$alertas %||% list()
  if (is.data.frame(rows)) {
    if (nrow(rows) == 0L) return(data.frame())
    return(rows)
  }
  if (!is.list(rows) || length(rows) == 0L) return(data.frame())
  out <- lapply(rows, function(x) {
    if (is.null(x)) return(NULL)
    data.frame(
      id = as.integer(x$id %||% NA),
      lat = as.numeric(x$lat %||% NA),
      lon = as.numeric(x$lon %||% NA),
      variable = as.character(x$variable %||% ""),
      direccion = as.character(x$direccion %||% ""),
      ranking = as.integer(x$ranking %||% NA),
      severidad = as.character(x$severidad %||% ""),
      stringsAsFactors = FALSE
    )
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0L) return(data.frame())
  df <- do.call(rbind, out)
  df[!is.na(df$id) & !is.na(df$lat) & !is.na(df$lon), , drop = FALSE]
}

