#' @export
mapa_ui <- function(id) {
  ns <- shiny::NS(id)
  leaflet::leafletOutput(ns("mapa"), height = "100%")
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
  out <- trimws(as.character(col))
  out <- out[!is.na(out) & nzchar(out)]
  if (length(out) == 0L) return(character())
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

MSG_VARS_DEF <- "Sin variables con datos de observacion (API 0 filas), error de red o timeout."

mapa_popup_html_tabla <- function(
    nombre, operador, codigo, region, comuna, lon, lat,
    variables_nombres = character(),
    variables_miss_msg = MSG_VARS_DEF
  ) {
  esc <- htmltools::htmlEscape
  base_hdr <- c(
    "Nombre estaci\u00f3n", "Operador", "C\u00f3digo estaci\u00f3n", "Regi\u00f3n", "Comuna",
    "Longitud", "Latitud"
  )
  nvar <- length(variables_nombres)
  if (nvar > 0L) {
    var_hdr <- paste0("Variable ", seq_len(nvar))
    var_vals <- esc(as.character(variables_nombres))
  } else {
    var_hdr <- "Variables"
    var_vals <- esc(variables_miss_msg)
  }
  hdr <- c(base_hdr, var_hdr)
  lon_s <- if (is.na(lon)) "\u2014" else format(lon, digits = 6)
  lat_s <- if (is.na(lat)) "\u2014" else format(lat, digits = 6)
  vals_esc <- c(
    esc(as.character(nombre)), esc(as.character(operador)), esc(as.character(codigo)),
    esc(as.character(region)), esc(as.character(comuna)), esc(lon_s), esc(lat_s)
  )
  all_vals <- c(vals_esc, var_vals)
  th <- paste0(
    "<th style=\"padding:4px 6px;border:1px solid #ccc;background:#f5f5f5;font-size:10px;white-space:normal;max-width:130px;vertical-align:bottom;line-height:1.15;\">",
    esc(hdr), "</th>",
    collapse = ""
  )
  td <- paste0(
    "<td style=\"padding:4px 6px;border:1px solid #ccc;font-size:11px;vertical-align:top;text-align:left;white-space:normal;max-width:160px;\">",
    all_vals, "</td>",
    collapse = ""
  )
  paste0(
    "<div style=\"max-width:min(960px,96vw);overflow-x:auto;\">",
    "<table style=\"border-collapse:collapse;margin:0;\">",
    "<thead><tr>", th, "</tr></thead>",
    "<tbody><tr>", td, "</tr></tbody>",
    "</table></div>"
  )
}

#' @export
mapa_server <- function(id, estaciones, estacion_id, fuentes = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    bounds <- list(lng1 = -76, lat1 = -56, lng2 = -66, lat2 = -17)
    fuentes_r <- if (is.null(fuentes)) function() data.frame(id = integer(), nombre = character()) else fuentes

    vars_nombres_estacion <- shiny::reactive({
      eid <- estacion_id()
      df <- estaciones()
      if (is.null(eid) || nrow(df) == 0L || !eid %in% df$id) {
        return(list(nombres = character(), miss = ""))
      }
      vars_raw <- tryCatch(
        api_get(paste0("/estaciones/", eid, "/variables"), timeout = 25),
        error = function(e) NULL
      )
      vdf <- mapa_popup_normalizar_variables_api(vars_raw)
      vnoms <- if (is.null(vdf)) character() else mapa_popup_variables_nombres(vdf)
      miss <- ""
      if (is.null(vars_raw)) {
        miss <- "No se pudo obtener /estaciones/id/variables (timeout, red o no JSON)."
      } else if (is.null(vdf)) {
        miss <- "Respuesta de variables con formato inesperado."
      } else if (nrow(vdf) == 0L || length(vnoms) == 0L) {
        miss <- paste0(
          "API sin filas: solo variables con datos en observacion_final (INNER JOIN). id_estacion=",
          eid, "."
        )
      }
      list(nombres = vnoms, miss = miss)
    })

    popup_datos <- shiny::reactive({
      eid <- estacion_id()
      df <- estaciones()
      f <- fuentes_r()
      if (is.null(eid) || nrow(df) == 0L || !eid %in% df$id) return(NULL)
      sel <- df[df$id == eid, , drop = FALSE]
      nombre0 <- sel$nombre[1] %||% ""
      lat0 <- as.numeric(sel$lat[1])
      lon0 <- as.numeric(sel$lon[1])

      det_raw <- tryCatch(
        api_get(paste0("/estaciones/", eid), timeout = 8),
        error = function(e) NULL
      )
      det_raw <- mapa_popup_normalizar_detalle_api(det_raw)
      d <- mapa_popup_parse_detalle(det_raw, eid, nombre0, lat0, lon0)

      operador <- "\u2014"
      fid <- d$id_fuente
      if (!is.na(fid) && nrow(f) > 0L) {
        idx <- which(as.integer(f$id) == as.integer(fid))
        if (length(idx) > 0L) {
          operador <- as.character(f$nombre[idx[1L]] %||% "\u2014")
        }
      }

      vinfo <- vars_nombres_estacion()
      vnoms <- vinfo$nombres
      miss <- vinfo$miss

      mapa_popup_html_tabla(
        nombre = d$nombre,
        operador = operador,
        codigo = as.character(eid),
        region = d$nom_reg,
        comuna = d$nom_com,
        lon = d$lon,
        lat = d$lat,
        variables_nombres = vnoms,
        variables_miss_msg = if (nzchar(miss)) miss else MSG_VARS_DEF
      )
    })

    output$mapa <- leaflet::renderLeaflet({
      df <- estaciones()
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::fitBounds(bounds$lng1, bounds$lat1, bounds$lng2, bounds$lat2)
      if (is.null(df) || nrow(df) == 0) return(m)
      eid <- estacion_id()
      pal <- leaflet::colorFactor("Set1", domain = unique(df$macrozona))
      resto <- if (!is.null(eid) && eid %in% df$id) df[df$id != eid, , drop = FALSE] else df
      if (nrow(resto) > 0) {
        m <- leaflet::addCircleMarkers(m, data = resto,
          lng = ~lon, lat = ~lat, color = ~pal(macrozona), fillColor = ~pal(macrozona),
          fillOpacity = 0.7, radius = 8, layerId = ~id,
          clusterOptions = leaflet::markerClusterOptions())
      }
      if (!is.null(eid) && eid %in% df$id) {
        sel <- df[df$id == eid, , drop = FALSE]
        m <- leaflet::addCircleMarkers(m, data = sel, lng = ~lon, lat = ~lat,
          color = "#E85D24", fillColor = "#E85D24", fillOpacity = 0.9, radius = 12, weight = 2,
          layerId = ~id)
        popup_html <- popup_datos()
        if (is.null(popup_html)) {
          popup_html <- mapa_popup_html_tabla(
            nombre = sel$nombre[1] %||% "",
            operador = "\u2014",
            codigo = as.character(eid),
            region = "\u2014",
            comuna = "\u2014",
            lon = as.numeric(sel$lon[1]),
            lat = as.numeric(sel$lat[1]),
            variables_nombres = character(),
            variables_miss_msg = "Cargando..."
          )
        }
        m <- leaflet::addPopups(m,
          lng = sel$lon[1], lat = sel$lat[1],
          popup = popup_html,
          options = leaflet::popupOptions(closeButton = TRUE, maxWidth = 960, autoPan = TRUE))
      }
      m
    })

    shiny::observeEvent(estacion_id(), {
      eid <- estacion_id()
      if (is.null(eid)) return()
      df <- estaciones()
      r <- df[df$id == eid, , drop = FALSE]
      if (nrow(r) > 0) {
        leaflet::leafletProxy("mapa", session = session) |>
          leaflet::setView(lng = r$lon[1], lat = r$lat[1], zoom = 10)
      }
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$mapa_marker_click, {
      click <- input$mapa_marker_click
      if (is.null(click$id)) return()
      estacion_id(as.integer(click$id))
    }, ignoreNULL = TRUE)
  })
}
