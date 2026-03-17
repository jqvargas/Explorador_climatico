#' @export
mapa_ui <- function(id) {
  ns <- shiny::NS(id)
  leaflet::leafletOutput(ns("mapa"), height = 400)
}

#' @export
mapa_server <- function(id, api_url, variable_id, fechas, estacion_id, operador_id = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    estaciones <- shiny::reactive({
      base <- api_url()
      url <- paste0(base, "/estaciones?chile_only=1&minimal=1")
      oid <- if (!is.null(operador_id)) operador_id() else NULL
      vid <- if (!is.null(variable_id)) variable_id() else NULL
      if (!is.null(oid) && !is.na(oid)) url <- paste0(url, "&id_fuente=", oid)
      if (!is.null(vid) && !is.na(vid)) url <- paste0(url, "&id_variable=", vid)
      resp <- tryCatch(httr::GET(url, httr::timeout(60)), error = function(e) NULL)
      if (is.null(resp) || httr::status_code(resp) != 200) return(data.frame())
      dat <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"), error = function(e) NULL)
      if (is.null(dat)) return(data.frame())
      if (inherits(dat, "data.frame")) {
        df <- as.data.frame(dat)
        for (c in c("id", "nombre", "lat", "lon", "macrozona")) {
          if (!c %in% names(df)) df[[c]] <- if (c == "id") NA_integer_ else if (c %in% c("lat", "lon")) NA_real_ else NA_character_
        }
        return(df[, c("id", "nombre", "lat", "lon", "macrozona"), drop = FALSE])
      }
      if (is.list(dat) && length(dat) > 0) {
        if (identical(names(dat), "data") && is.list(dat$data)) dat <- dat$data
        rows <- lapply(dat, function(x) {
          if (is.atomic(x)) return(NULL)
          tryCatch(
            data.frame(
              id = x$id %||% NA_integer_,
              nombre = as.character(x$nombre %||% ""),
              lat = as.numeric(x$lat %||% NA),
              lon = as.numeric(x$lon %||% NA),
              macrozona = as.character(x$macrozona %||% ""),
              stringsAsFactors = FALSE
            ),
            error = function(e) NULL
          )
        })
        rows <- rows[!vapply(rows, is.null, logical(1))]
        if (length(rows) == 0) return(data.frame())
        df <- tryCatch(as.data.frame(do.call(rbind, rows)), error = function(e) data.frame())
        if (nrow(df) == 0) return(data.frame())
        df$id <- as.integer(df$id)
        df <- df[!is.na(df$lat) & !is.na(df$lon) & df$lat >= -56 & df$lat <= -17 & df$lon >= -76 & df$lon <= -66, , drop = FALSE]
        return(df)
      }
      data.frame()
    })

    chile_bounds <- list(lng1 = -76, lat1 = -56, lng2 = -66, lat2 = -17)

    output$mapa <- leaflet::renderLeaflet({
      df <- estaciones()
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::fitBounds(chile_bounds$lng1, chile_bounds$lat1, chile_bounds$lng2, chile_bounds$lat2)
      if (is.null(df) || nrow(df) == 0) return(m)
      eid <- estacion_id()
      dom <- unique(df$macrozona)
      dom <- dom[!is.na(dom) & nzchar(as.character(dom))]
      if (length(dom) == 0) dom <- "N/A"
      pal <- leaflet::colorFactor("Set1", domain = dom)
      df_resto <- if (!is.null(eid) && eid %in% df$id) { df[df$id != eid, , drop = FALSE] } else { df }
      if (nrow(df_resto) > 0) {
        m <- leaflet::addCircleMarkers(m, data = df_resto,
          lng = ~lon, lat = ~lat, color = ~pal(macrozona), fillColor = ~pal(macrozona),
          fillOpacity = 0.7, radius = 8, popup = ~as.character(nombre), layerId = ~id,
          clusterOptions = leaflet::markerClusterOptions())
      }
      if (!is.null(eid) && eid %in% df$id) {
        est <- df[df$id == eid, , drop = FALSE]
        m <- leaflet::addCircleMarkers(m, data = est, lng = ~lon, lat = ~lat,
          color = "#E85D24", fillColor = "#E85D24", fillOpacity = 0.9, radius = 12, weight = 2,
          popup = ~as.character(nombre), layerId = ~id)
      }
      m
    })

    shiny::observeEvent(estacion_id(), {
      eid <- estacion_id()
      if (is.null(eid)) return()
      df <- estaciones()
      est <- df[df$id == eid, , drop = FALSE]
      if (nrow(est) == 0) return()
      leaflet::leafletProxy("mapa", session = session) |>
        leaflet::setView(lng = est$lon[1], lat = est$lat[1], zoom = 10)
    }, ignoreNULL = TRUE)

    map_click_id <- paste0(ns("mapa"), "_marker_click")
    shiny::observeEvent(input[[map_click_id]], {
      click <- input[[map_click_id]]
      if (!is.null(click) && !is.null(click$id)) estacion_id(as.integer(click$id))
    }, ignoreNULL = TRUE)
  })
}
