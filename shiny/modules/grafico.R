#' @export
grafico_ui <- function(id) {
  ns <- shiny::NS(id)
  shinycssloaders::withSpinner(plotly::plotlyOutput(ns("grafico"), height = 350))
}

#' @export
grafico_server <- function(id, estacion_id, variable_id, ver_datos_click, datos) {
  shiny::moduleServer(id, function(input, output, session) {
    output$grafico <- plotly::renderPlotly({
      if (is.null(estacion_id()) || is.null(variable_id()) || ver_datos_click() == 0) {
        return(plotly::plot_ly() |>
          plotly::add_annotations(text = "Selecciona estacion, variable y clic en Ver datos", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE) |>
          plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
      }
      res <- datos()
      if (!is.null(res$error)) {
        return(plotly::plot_ly() |>
          plotly::add_annotations(text = res$error, x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE) |>
          plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
      }
      df <- res$df
      if (is.null(df) || nrow(df) == 0) {
        return(plotly::plot_ly() |>
          plotly::add_annotations(text = "No hay datos", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE) |>
          plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
      }
      n_valid <- sum(!is.na(df$valor))
      dias <- as.numeric(difftime(max(df$fecha), min(df$fecha), units = "days")) + 1
      cobertura <- round(100 * n_valid / max(dias, 1), 1)
      plotly::plot_ly(df[!is.na(df$valor), ], x = ~fecha, y = ~valor, type = "scatter", mode = "lines+markers",
        line = list(color = "#1f77b4"), marker = list(size = 6)) |>
        plotly::layout(
          title = paste0("Completitud: ", cobertura, "%"),
          xaxis = list(title = "Fecha", rangeslider = list(visible = TRUE)),
          yaxis = list(title = df$unidad[1] %||% "Valor")
        )
    })
  })
}
