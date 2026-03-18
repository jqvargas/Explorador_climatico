#' @export
grafico_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "ec-grafico-wrap",
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("grafico"), height = 350)),
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
      n_dias <- as.numeric(difftime(max_d, min_d, units = "days"))
      rango_aplicado(c(min_d, max_d))
      shiny::div(class = "ec-rango-fechas",
        shiny::div(class = "ec-rango-fechas-barra",
          shiny::sliderInput(
            ns("rango_slider"),
            label = NULL,
            min = 0, max = max(1L, n_dias), value = c(0L, max(1L, n_dias)),
            width = "100%"
          )
        ),
        shiny::div(class = "ec-rango-fechas-inputs",
          shiny::div(class = "ec-rango-input-group",
            shiny::tags$label("Fecha inicio"),
            shiny::dateInput(ns("fecha_inicio"), NULL, value = min_d, min = min_d, max = max_d, format = "yyyy-mm-dd")
          ),
          shiny::div(class = "ec-rango-input-group",
            shiny::tags$label("Fecha fin"),
            shiny::dateInput(ns("fecha_fin"), NULL, value = max_d, min = min_d, max = max_d, format = "yyyy-mm-dd")
          ),
          shiny::div(class = "ec-rango-input-group ec-rango-apply",
            shiny::actionButton(ns("aplicar_rango"), "Aplicar", class = "btn-primary btn-sm")
          )
        )
      )
    })

    min_max_dates <- shiny::reactive({
      res <- datos()
      if (is.null(res) || !is.null(res$error) || is.null(res$df) || nrow(res$df) == 0) return(NULL)
      df <- res$df[!is.na(res$df$valor), ]
      if (nrow(df) == 0) return(NULL)
      c(min(df$fecha), max(df$fecha))
    })

    shiny::observe({
      mm <- min_max_dates()
      if (is.null(mm)) return()
      rng <- input$rango_slider
      if (is.null(rng) || length(rng) < 2) return()
      min_d <- mm[1]
      d1 <- min_d + rng[1]
      d2 <- min_d + rng[2]
      shiny::updateDateInput(session, "fecha_inicio", value = d1, min = min_d, max = mm[2])
      shiny::updateDateInput(session, "fecha_fin", value = d2, min = min_d, max = mm[2])
    })

    shiny::observeEvent(input$aplicar_rango, {
      fi <- input$fecha_inicio
      ff <- input$fecha_fin
      if (is.null(fi) || is.null(ff)) return()
      if (fi <= ff) {
        rango_aplicado(c(fi, ff))
        periodo_seleccionado(c(fi, ff))
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
      p <- plotly::plot_ly(df_plot, x = ~fecha, y = ~valor, type = "scatter", mode = "lines+markers",
        line = list(color = "#1f77b4"), marker = list(size = 6), source = ns("grafico")) |>
        plotly::layout(
          title = paste0("Completitud: ", cobertura, "%"),
          xaxis = xaxis_cfg,
          yaxis = list(title = df$unidad[1] %||% "Valor")
        )
      p |> plotly::config(displayModeBar = FALSE)
    })
  })
}
