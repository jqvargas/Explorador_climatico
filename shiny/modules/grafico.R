#' @export
grafico_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "ec-grafico-wrap",
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("grafico"), height = 200)),
    shiny::uiOutput(ns("rango_fechas_ui"))
  )
}

#' @export
grafico_server <- function(id, estacion_id, variable_id, ver_datos_click, datos, periodo_seleccionado = NULL, ...) {
  if (is.null(periodo_seleccionado)) periodo_seleccionado <- shiny::reactiveVal(NULL)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rango_aplicado <- shiny::reactiveVal(NULL)

    output$rango_fechas_ui <- shiny::renderUI({
      if (ver_datos_click() < 1L) return(NULL)
      res <- datos()
      if (is.null(res) || !is.null(res$error) || is.null(res$df) || nrow(res$df) == 0) return(NULL)
      df <- res$df
      df_plot <- df[!is.na(df$valor), ]
      if (nrow(df_plot) == 0) return(NULL)
      min_d <- min(df$fecha)
      max_d <- max(df$fecha)
      default_start <- max(min_d, as.Date("2024-01-01"))
      default_end <- min(max_d, as.Date("2025-12-31"))
      if (default_start > default_end) {
        default_start <- min_d
        default_end <- max_d
      }
      rango_aplicado(c(default_start, default_end))
      shiny::div(class = "ec-rango-fechas",
        shiny::dateRangeInput(
          ns("rango_fechas"),
          label = NULL,
          start = default_start,
          end = default_end,
          min = min_d,
          max = max_d,
          separator = " a ",
          format = "yyyy-mm-dd",
          startview = "decade",
          language = "es",
          width = "100%"
        )
      )
    })

    shiny::observeEvent(input$rango_fechas, {
      rng <- input$rango_fechas
      if (is.null(rng) || length(rng) < 2) return()
      fi <- as.Date(rng[1]); ff <- as.Date(rng[2])
      if (!is.null(fi) && !is.null(ff) && fi <= ff) {
        rango_aplicado(c(fi, ff))
        periodo_seleccionado(c(fi, ff))
      }
    }, ignoreNULL = TRUE)

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
      df_plot <- df[!is.na(df$valor), ]
      if (nrow(df_plot) == 0) {
        return(plotly::plot_ly() |>
          plotly::add_annotations(text = "No hay datos validos", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE) |>
          plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
      }
      n_valid <- sum(!is.na(df$valor))
      dias <- as.numeric(difftime(max(df$fecha), min(df$fecha), units = "days")) + 1
      cobertura <- round(100 * n_valid / max(dias, 1), 1)
      rango <- rango_aplicado()
      xaxis_cfg <- list(title = "Fecha", rangeslider = list(visible = FALSE))
      if (!is.null(rango) && length(rango) == 2) {
        xaxis_cfg$range <- c(rango[1], rango[2])
      }
      nombre_est <- if ("estacion_nombre" %in% names(df) && nzchar(df$estacion_nombre[1])) df$estacion_nombre[1] else "estacion"
      p <- plotly::plot_ly(df_plot, x = ~fecha, y = ~valor, type = "scatter", mode = "lines+markers",
        line = list(color = "#1f77b4"), marker = list(size = 6), source = ns("grafico")) |>
        plotly::layout(
          title = paste0("Serie de tiempo de ", nombre_est, ", completitud: ", cobertura, "%"),
          xaxis = xaxis_cfg,
          yaxis = list(title = df$unidad[1] %||% "Valor")
        )
      p |> plotly::config(displayModeBar = FALSE)
    })
  })
}


