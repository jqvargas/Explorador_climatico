#' @title Módulo de tabla preview de datos
#' Misma fuente que gráfico, DT::renderDataTable
tabla_ui <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("tabla"))
}

tabla_server <- function(id, api_url, estacion_id, variable_id, fechas, ver_datos) {
  shiny::moduleServer(id, function(input, output, session) {
    datos <- shiny::reactive({
      ver_datos()
      eid <- estacion_id()
      vid <- variable_id()
      fch <- fechas()
      if (is.null(eid) || is.null(vid) || is.null(fch)) return(NULL)
      base <- sub("/$", "", api_url())
      url <- paste0(base, "/datos?estacion_id=", eid, "&variable_id=", vid,
        "&fecha_inicio=", format(fch[1], "%Y-%m-%d"),
        "&fecha_fin=", format(fch[2], "%Y-%m-%d"))
      resp <- tryCatch({
        httr::GET(url, httr::timeout(15))
      }, error = function(e) NULL)
      if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
      dat <- httr::content(resp, as = "parsed", type = "application/json")
      if (is.null(dat) || "error" %in% names(dat)) return(NULL)
      if (is.data.frame(dat)) return(dat)
      as.data.frame(do.call(rbind, dat), stringsAsFactors = FALSE)
    })

    output$tabla <- DT::renderDataTable({
      d <- datos()
      if (is.null(d) || nrow(d) == 0) return(DT::datatable(data.frame("Sin datos" = "Selecciona estación y pulsa Ver datos")))
      DT::datatable(d, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
  })
}
