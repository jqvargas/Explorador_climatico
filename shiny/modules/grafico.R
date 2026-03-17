#' @export
grafico_ui <- function(id) {
  ns <- shiny::NS(id)
  shinycssloaders::withSpinner(plotly::plotlyOutput(ns("grafico"), height = 350))
}

#' @export
grafico_server <- function(id, estacion_id, variable_id, ver_datos_click, datos, periodo_seleccionado = NULL, ...) {
  if (is.null(periodo_seleccionado)) periodo_seleccionado <- shiny::reactiveVal(NULL)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    to_date <- function(x) {
      if (is.null(x)) return(NULL)
      n <- suppressWarnings(as.numeric(x))
      if (is.na(n)) return(tryCatch(as.Date(x), error = function(e) NULL))
      if (n > 1e10) n <- n / 86400000
      tryCatch(as.Date(n, origin = "1970-01-01"), error = function(e) NULL)
    }
    shiny::observe({
      ed <- plotly::event_data("plotly_relayout", source = ns("grafico"))
      if (is.null(ed)) return()
      rng <- ed[["xaxis.range"]]
      if (is.null(rng)) rng <- c(ed[["xaxis.range[0]"]], ed[["xaxis.range[1]"]])
      if (!is.null(rng) && length(rng) >= 2) {
        d1 <- to_date(rng[1]); d2 <- to_date(rng[2])
        if (!is.null(d1) && !is.null(d2) && d1 <= d2) periodo_seleccionado(c(d1, d2))
      }
    })

    output$grafico <- plotly::renderPlotly({
      if (ver_datos_click() < 1L) {
        return(plotly::plot_ly() |>
          plotly::add_annotations(text = "Selecciona estacion y variable, luego clic en Ver datos", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE) |>
          plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
      }
      if (is.null(estacion_id()) || is.null(variable_id())) {
        return(plotly::plot_ly() |>
          plotly::add_annotations(text = "Selecciona estacion y variable, luego clic en Ver datos", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE) |>
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
          plotly::add_annotations(text = "No hay datos para esta estacion/variable", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE) |>
          plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
      }
      periodo_seleccionado(NULL)
      df_plot <- df[!is.na(df$valor), ]
      if (nrow(df_plot) == 0) {
        return(plotly::plot_ly() |>
          plotly::add_annotations(text = "No hay datos validos", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE) |>
          plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
      }
      n_valid <- sum(!is.na(df$valor))
      dias <- as.numeric(difftime(max(df$fecha), min(df$fecha), units = "days")) + 1
      cobertura <- round(100 * n_valid / max(dias, 1), 1)
      p <- plotly::plot_ly(df_plot, x = ~fecha, y = ~valor, type = "scatter", mode = "lines+markers",
        line = list(color = "#1f77b4"), marker = list(size = 6), source = ns("grafico")) |>
        plotly::layout(
          title = paste0("Completitud: ", cobertura, "% - Ajusta el rango para habilitar descarga"),
          xaxis = list(title = "Fecha", rangeslider = list(visible = TRUE)),
          yaxis = list(title = df$unidad[1] %||% "Valor")
        )
      plotly::event_register(p, "plotly_relayout")
      p
    })
  })
}

