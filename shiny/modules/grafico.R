#' @export
grafico_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    shiny::h4("Grafico"),
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(ns("grafico"), height = 350)
    )
  )
}

#' @export
grafico_server <- function(id, api_url, estacion_id, variable_id, ver_datos_click) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    datos <- shiny::reactive({
      shiny::req(estacion_id(), variable_id(), ver_datos_click() > 0)
      base <- api_url()
      url <- paste0(base, "/datos?estacion_id=", estacion_id(), "&variable_id=", variable_id())
      resp <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
      if (is.null(resp)) return(list(df = data.frame(), error = "No se pudo conectar con la API. Verifica que el servicio este activo."))
      if (httr::status_code(resp) != 200) {
        cnt <- tryCatch(httr::content(resp, as = "parsed"), error = function(e) NULL)
        msg <- if (is.list(cnt) && !is.null(cnt$error)) {
          paste(unlist(cnt$error), collapse = " ")
        } else {
          paste0("Error HTTP ", httr::status_code(resp), ". Revisa los parametros.")
        }
        return(list(df = data.frame(), error = msg))
      }
      dat <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"), error = function(e) NULL)
      if (is.null(dat)) return(list(df = data.frame(), error = "La API devolvio respuesta vacia o no valida."))
      if (length(dat) == 0) return(list(df = data.frame(), error = NULL))
      if (is.list(dat) && !is.null(dat$error)) return(list(df = data.frame(), error = paste(unlist(dat$error), collapse = " ")))
      if (is.data.frame(dat)) return(list(df = dat, error = NULL))
      df <- tryCatch({
        as.data.frame(do.call(rbind, lapply(dat, function(x) {
          val <- x$valor
          if (is.null(val)) val <- NA_real_
          data.frame(
            fecha = as.Date(x$fecha),
            valor = as.numeric(val),
            estacion_nombre = as.character(x$estacion_nombre %||% ""),
            variable_nombre = as.character(x$variable_nombre %||% ""),
            unidad = as.character(x$unidad %||% ""),
            stringsAsFactors = FALSE
          )
        })))
      }, error = function(e) data.frame())
      if (nrow(df) == 0) return(list(df = data.frame(), error = "Error al parsear la respuesta de la API."))
      list(df = df, error = NULL)
    })

    output$grafico <- plotly::renderPlotly({
      if (shiny::isTruthy(ver_datos_click()) && ver_datos_click() > 0) {
        if (is.null(estacion_id())) {
          return(plotly::plot_ly() |>
            plotly::add_annotations(
              text = "Selecciona una estacion del menu y haz clic en Ver datos",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(size = 14)
            ) |>
            plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
        }
        if (is.null(variable_id())) {
          return(plotly::plot_ly() |>
            plotly::add_annotations(
              text = "Selecciona variable y rango de fechas",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(size = 14)
            ) |>
            plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
        }
      }
      res <- datos()
      df <- res$df
      err <- res$error
      if (!is.null(err)) {
        return(plotly::plot_ly() |>
          plotly::add_annotations(
            text = err,
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 11)
          ) |>
          plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
      }
      if (is.null(df) || nrow(df) == 0) {
        return(plotly::plot_ly() |>
          plotly::add_annotations(
            text = "No hay observaciones para esta estacion, variable y rango. Prueba otra estacion (ej. VISVIRI) o ajusta las fechas.",
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE, font = list(size = 11)
          ) |>
          plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
      }

      # Calcular calidad (cobertura) y NA
      n_total <- nrow(df)
      n_na <- sum(is.na(df$valor))
      n_valid <- n_total - n_na
      dias_periodo <- as.numeric(difftime(max(df$fecha), min(df$fecha), units = "days")) + 1
      cobertura <- if (dias_periodo > 0) round(100 * n_valid / dias_periodo, 1) else 0
      titulo_calidad <- sprintf("Porcentaje de completitud de datos: %s%%", cobertura)

      df_valid <- df[!is.na(df$valor), , drop = FALSE]
      df_na <- df[is.na(df$valor), , drop = FALSE]

      p <- plotly::plot_ly()
      if (nrow(df_valid) > 0) {
        p <- p |> plotly::add_trace(
          data = df_valid, x = ~fecha, y = ~valor,
          type = "scatter", mode = "lines+markers",
          line = list(color = "#1f77b4", width = 2),
          marker = list(size = 6, color = "#1f77b4"),
          connectgaps = FALSE,
          name = "Valor"
        )
      }
      if (nrow(df_na) > 0) {
        y_rng <- if (nrow(df_valid) > 0) range(df_valid$valor, na.rm = TRUE) else c(0, 1)
        y_na <- y_rng[1] - 0.05 * diff(y_rng)
        if (diff(y_rng) == 0) y_na <- y_rng[1] - 0.5
        df_na$y_marker <- y_na
        p <- p |> plotly::add_trace(
          data = df_na, x = ~fecha, y = ~y_marker,
          type = "scatter", mode = "markers",
          marker = list(size = 12, color = "red", symbol = "x", line = list(color = "darkred", width = 2)),
          name = "Dato faltante (NA)"
        )
      }

      p <- p |> plotly::layout(
        title = list(text = titulo_calidad, font = list(size = 12), x = 0.02, xanchor = "left"),
        xaxis = list(title = "Fecha", rangeslider = list(visible = TRUE)),
        yaxis = list(title = if (nzchar(df$unidad[1] %||% "")) paste(df$variable_nombre[1] %||% "Valor", "(", df$unidad[1], ")") else "Valor"),
        showlegend = nrow(df_na) > 0,
        legend = list(orientation = "h", y = 1.08),
        updatemenus = list(
          list(
            x = 0.05, y = 0.95,
            buttons = list(
              list(method = "relayout", args = list("xaxis.range", c(min(df$fecha), max(df$fecha))), label = "Todo"),
              list(method = "relayout", args = list("xaxis.range", c(Sys.Date() - 365, Sys.Date())), label = "1 ano"),
              list(method = "relayout", args = list("xaxis.range", c(Sys.Date() - 90, Sys.Date())), label = "3 meses"),
              list(method = "relayout", args = list("xaxis.range", c(Sys.Date() - 30, Sys.Date())), label = "1 mes")
            )
          )
        )
      )
      p
    })
  })
}
