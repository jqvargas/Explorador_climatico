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
      df_plot <- ec_serie_normalizar(df)
      df_plot <- df_plot[!is.na(df_plot$fecha), ]
      if (nrow(df_plot) == 0) return(NULL)

      min_d <- min(df$fecha)
      max_d <- max(df$fecha)

      # Ultimos 2 meses por default
      default_start <- max(min_d, max_d - 60)
      default_end   <- max_d

      rango_aplicado(c(default_start, default_end))

      shiny::div(
        class = "ec-rango-fechas",
        style = "padding: 8px 16px;",
        shiny::sliderInput(
          ns("rango_fechas"),
          label = NULL,
          min   = min_d,
          max   = max_d,
          value = c(default_start, default_end),
          width = "100%",
          ticks = FALSE
        )
      )
    })

    shiny::observeEvent(input$rango_fechas, {
      rng <- input$rango_fechas
      if (is.null(rng) || length(rng) < 2) return()
      fi <- as.Date(rng[1], origin = "1970-01-01")
      ff <- as.Date(rng[2], origin = "1970-01-01")
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
      df <- ec_serie_normalizar(res$df)
      if (is.null(df) || nrow(df) == 0) {
        return(plotly::plot_ly() |>
          plotly::add_annotations(text = "No hay datos para esta estaci\u00f3n/variable", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE) |>
          plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
      }
      df_plot <- df[!is.na(df$fecha), ]
      # Grado 3 (dudoso sin arbitro) sigue en la linea: visible en el grafico y
      # fuera de alertas/records. 1 y 2 son descarte confirmado.
      df_line <- df_plot[(is.na(df_plot$qc_flag) | df_plot$qc_flag %in% c(0, 3)) & !is.na(df_plot$valor), ]
      if (nrow(df_line) == 0 && sum(df_plot$qc_flag %in% c(1L, 2L, 3L), na.rm = TRUE) == 0) {
        return(plotly::plot_ly() |>
          plotly::add_annotations(text = "No hay datos v\u00e1lidos", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE) |>
          plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
      }
      if (!"qc_flag" %in% names(df_plot)) df_plot$qc_flag <- 0L
      rango <- rango_aplicado()
      if (!is.null(rango) && length(rango) == 2) {
        df_plot <- df_plot[df_plot$fecha >= rango[1] & df_plot$fecha <= rango[2], , drop = FALSE]
        df_line <- df_line[df_line$fecha >= rango[1] & df_line$fecha <= rango[2], , drop = FALSE]
      }
      # Grados 1 y 2 se descartan; el 3 (dudoso sin arbitro) sigue visible con
      # marcador propio, porque queda fuera de alertas y records pero no del mapa.
      df_bad <- df_plot[df_plot$qc_flag %in% c(1L, 2L) & !is.na(df_plot$valor), , drop = FALSE]
      df_dud <- df_plot[df_plot$qc_flag == 3L & !is.na(df_plot$valor), , drop = FALSE]
      n_valid <- nrow(df_line)
      dias <- as.numeric(difftime(max(df_plot$fecha), min(df_plot$fecha), units = "days")) + 1
      cobertura <- round(100 * n_valid / max(dias, 1), 1)
      xaxis_cfg <- list(title = "Fecha", rangeslider = list(visible = FALSE), fixedrange = TRUE)
      if (!is.null(rango) && length(rango) == 2) {
        xaxis_cfg$range <- c(rango[1], rango[2])
      }
      nombre_est <- if ("estacion_nombre" %in% names(df) && nzchar(df$estacion_nombre[1])) df$estacion_nombre[1] else "estacion"
      var_label <- if ("variable_nombre" %in% names(df) && nzchar(df$variable_nombre[1])) {
        df$variable_nombre[1]
      } else if ("variable_nombre_db" %in% names(df) && nzchar(df$variable_nombre_db[1])) {
        ec_variable_label(df$variable_nombre_db[1])
      } else "variable"
      titulo <- paste0("Serie de ", var_label, " \u2014 ", nombre_est, " (completitud: ", cobertura, "%)")
      if (nrow(df_bad) > 0) {
        titulo <- paste0(titulo, " \u00b7 ", nrow(df_bad), " valor(es) descartado(s) por QC")
      }
      if (nrow(df_dud) > 0) {
        titulo <- paste0(titulo, " \u00b7 ", nrow(df_dud), " dudoso(s)")
      }
      p <- plotly::plot_ly(df_line, x = ~fecha, y = ~valor, type = "scatter", mode = "lines+markers",
        line = list(color = "#1f77b4"), marker = list(size = 6), name = "Serie",
        connectgaps = FALSE, source = ns("grafico"))
      if (nrow(df_bad) > 0) {
        p <- p |> plotly::add_markers(
          data = df_bad, x = ~fecha, y = ~valor,
          marker = list(color = "#DC2626", symbol = "x", size = 10),
          name = "Descartado por QC"
        )
      }
      if (nrow(df_dud) > 0) {
        p <- p |> plotly::add_markers(
          data = df_dud, x = ~fecha, y = ~valor,
          marker = list(color = "#F59E0B", symbol = "circle-open", size = 10,
                        line = list(width = 2)),
          name = "Dudoso (sin verificar)"
        )
      }
      p <- p |> plotly::layout(
          title = titulo,
          xaxis = xaxis_cfg,
          yaxis = list(title = df$unidad[1] %||% "Valor", fixedrange = TRUE),
          dragmode = FALSE,
          showlegend = nrow(df_bad) > 0 || nrow(df_dud) > 0
        )
      p |> plotly::config(displayModeBar = FALSE, scrollZoom = FALSE, doubleClick = FALSE)
    })
  })
}



