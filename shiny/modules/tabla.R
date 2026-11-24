#' @export
tabla_ui <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("tabla"))
}

#' @export
tabla_server <- function(id, datos, ver_datos_click = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$tabla <- DT::renderDataTable({
      if (!is.null(ver_datos_click) && ver_datos_click() < 1L) {
        return(DT::datatable(data.frame(msg = "Selecciona estacion y variable, luego clic en Ver datos."), options = list(dom = "t"), rownames = FALSE))
      }
      res <- datos()
      if (!is.null(res$error)) {
        return(DT::datatable(data.frame(msg = res$error), options = list(dom = "t"), rownames = FALSE))
      }
      df <- res$df
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(msg = "No hay datos"), options = list(dom = "t"), rownames = FALSE))
      }
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE, language = list(url = "//cdn.datatables.net/plug-ins/1.10.24/i18n/Spanish.json")))
    })
  })
}
