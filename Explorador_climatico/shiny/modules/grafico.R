#' @title Módulo del gráfico de serie temporal
#' GET datos cuando estacion_id + variable_id + fechas, plot_ly, botones rango, stats
grafico_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "btn-group btn-group-sm mb-2",
      shiny::actionButton(ns("btn_1m"), "1M"),
      shiny::actionButton(ns("btn_6m"), "6M"),
      shiny::actionButton(ns("btn_1y"), "1Y"),
      shiny::actionButton(ns("btn_5y"), "5Y"),
      shiny::actionButton(ns("btn_all"), "Todo")
    ),
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("grafico"), height = "280px")),
    shiny::uiOutput(ns("stats_panel"))
  )
}

grafico_server <- function(id, api_url, estacion_id, variable_id, fechas, ver_datos) {
  shiny::moduleServer(id, function(input, output, session) {
    rango_activo <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$btn_1m, { rango_activo(30) })
    shiny::observeEvent(input$btn_6m, { rango_activo(180) })
    shiny::observeEvent(input$btn_1y, { rango_activo(365) })
    shiny::observeEvent(input$btn_5y, { rango_activo(365 * 5) })
    shiny::observeEvent(input$btn_all, { rango_activo(0) })

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
      df <- as.data.frame(do.call(rbind, dat), stringsAsFactors = FALSE)
      if (nrow(df) == 0) return(df)
      df$fecha <- as.Date(df$fecha)
      df$valor <- as.numeric(df$valor)
      df
    })

    datos_plot <- shiny::reactive({
      d <- datos()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      r <- rango_activo()
      if (!is.null(r) && r > 0) {
        fmax <- max(d$fecha, na.rm = TRUE)
        d <- d[d$fecha >= fmax - r, ]
      }
      d
    })

    output$grafico <- plotly::renderPlotly({
      d <- datos_plot()
      if (is.null(d) || nrow(d) == 0) {
        return(plotly::plot_ly() %>% plotly::config(displayModeBar = FALSE) %>%
          plotly::layout(title = "Selecciona estación y pulsa Ver datos"))
      }
      tit <- paste(unique(d$estacion_nombre)[1], "-", unique(d$variable_nombre)[1])
      n <- nrow(d)
      p <- plotly::plot_ly(d, x = ~fecha, y = ~valor, type = "scatter", mode = if (n < 100) "lines+markers" else "lines",
        line = list(color = "#1D9E75"), marker = list(size = 6))
      p %>% plotly::layout(
        title = tit,
        xaxis = list(title = ""),
        yaxis = list(title = unique(d$unidad)[1] %||% "Valor"),
        margin = list(t = 50),
        showlegend = FALSE
      ) %>% plotly::config(displayModeBar = TRUE)
    })

    output$stats_panel <- shiny::renderUI({
      d <- datos_plot()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      v <- d$valor
      v <- v[!is.na(v)]
      if (length(v) == 0) return(NULL)
      shiny::tags$div(
        class = "d-flex gap-3 small text-muted mt-1",
        shiny::tags$span(glue::glue("Mín: {round(min(v), 2)}")),
        shiny::tags$span(glue::glue("Máx: {round(max(v), 2)}")),
        shiny::tags$span(glue::glue("Promedio: {round(mean(v), 2)}"))
      )
    })
  })
}
