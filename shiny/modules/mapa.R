#' @export
mapa_ui <- function(id) {
  ns <- shiny::NS(id)
  leaflet::leafletOutput(ns("mapa"), height = "100%")
}

#' @export
mapa_server <- function(id, estaciones, estacion_id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    bounds <- list(lng1 = -76, lat1 = -56, lng2 = -66, lat2 = -17)

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
          fillOpacity = 0.7, radius = 8, popup = ~nombre, layerId = ~id,
          clusterOptions = leaflet::markerClusterOptions())
      }
      if (!is.null(eid) && eid %in% df$id) {
        sel <- df[df$id == eid, , drop = FALSE]
        m <- leaflet::addCircleMarkers(m, data = sel, lng = ~lon, lat = ~lat,
          color = "#E85D24", fillColor = "#E85D24", fillOpacity = 0.9, radius = 12, weight = 2,
          popup = ~nombre, layerId = ~id)
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

    shiny::observeEvent(input[[paste0(ns("mapa"), "_marker_click")]], {
      click <- input[[paste0(ns("mapa"), "_marker_click")]]
      if (!is.null(click$id)) estacion_id(as.integer(click$id))
    }, ignoreNULL = TRUE)
  })
}



