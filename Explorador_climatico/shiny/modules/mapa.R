#' @title Módulo del mapa de estaciones
#' GET estaciones, render Leaflet con CartoDB.Positron, circleMarkers por macrozona
mapa_ui <- function(id) {
  ns <- shiny::NS(id)
  leaflet::leafletOutput(ns("mapa"), height = "100%")
}

mapa_server <- function(id, api_url, variable_id, fechas, estacion_id) {
  shiny::moduleServer(id, function(input, output, session) {
    estaciones <- shiny::reactive({
      base <- sub("/$", "", api_url())
      url <- paste0(base, "/estaciones")
      resp <- tryCatch({
        httr::GET(url, httr::timeout(10))
      }, error = function(e) NULL)
      if (is.null(resp) || httr::status_code(resp) != 200) {
        return(data.frame())
      }
      dat <- httr::content(resp, as = "parsed", type = "application/json")
      if (is.null(dat) || length(dat) == 0) return(data.frame())
      if (is.data.frame(dat)) return(dat)
      as.data.frame(do.call(rbind, lapply(dat, function(x) {
        data.frame(
          id = x$id %||% NA,
          nombre = x$nombre %||% "",
          lat = as.numeric(x$lat %||% NA),
          lon = as.numeric(x$lon %||% NA),
          macrozona = x$macrozona %||% "Otro",
          tipo = x$tipo %||% "observado",
          fecha_ini = x$fecha_ini %||% ""
        )
      })), stringsAsFactors = FALSE)
    })

    palette <- shiny::reactive({
      est <- estaciones()
      if (nrow(est) == 0) return(NULL)
      zonas <- unique(est$macrozona)
      leaflet::colorFactor("Set2", domain = zonas)
    })

    output$mapa <- leaflet::renderLeaflet({
      est <- estaciones()
      pal <- palette()
      if (nrow(est) == 0 || is.null(pal)) {
        m <- leaflet::leaflet() %>%
          leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
            attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> &copy; <a href="https://carto.com/attributions">CARTO</a>')
        return(m)
      }
      est <- est[!is.na(est$lat) & !is.na(est$lon), ]
      if (nrow(est) == 0) {
        m <- leaflet::leaflet() %>%
          leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
            attribution = '&copy; OpenStreetMap &copy; CARTO')
        return(m)
      }
      m <- leaflet::leaflet(est) %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addCircleMarkers(
          lng = ~lon,
          lat = ~lat,
          color = ~pal(macrozona),
          fillOpacity = 0.7,
          radius = 8,
          popup = ~paste0("<b>", nombre, "</b><br>Tipo: ", tipo, "<br>Desde: ", fecha_ini),
          layerId = ~as.character(id),
          clusterOptions = leaflet::markerClusterOptions()
        )
      m
    })

    shiny::observeEvent(input[[paste0(session$ns("mapa"), "_marker_click")]], {
      ev <- input[[paste0(session$ns("mapa"), "_marker_click")]]
      if (!is.null(ev) && !is.null(ev$id)) estacion_id(as.integer(ev$id))
    })
  })
}
