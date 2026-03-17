server <- function(input, output, session) {
  api_url <- shiny::reactive({
    u <- Sys.getenv("API_URL", "http://api:8000")
    sub("/$", "", u)
  })

  variables <- shiny::reactive({
    base <- api_url()
    url <- paste0(base, "/variables")
    assign("resp", tryCatch(httr::GET(url, httr::timeout(10)), error = function(e) NULL), envir = environment())
    resp <- get("resp", envir = environment())
    if (is.null(resp) || httr::status_code(resp) != 200) return(data.frame(id = integer(), nombre = character()))
    dat <- httr::content(resp, as = "parsed", type = "application/json")
    if (is.null(dat) || length(dat) == 0) return(data.frame(id = integer(), nombre = character()))
    if (is.data.frame(dat)) return(dat)
    df <- as.data.frame(do.call(rbind, lapply(dat, function(x) {
      data.frame(id = x$id %||% NA, nombre = x$nombre %||% "", stringsAsFactors = FALSE)
    })))
    df$id <- as.integer(df$id)
    df
  })

  shiny::observe({
    v <- variables()
    if (nrow(v) > 0) {
      ch <- setNames(as.character(v$id), v$nombre)
      shiny::updateSelectInput(session, "variable", choices = ch, selected = ch[1])
    }
  })

  estacion_id <- shiny::reactiveVal(NULL)
  ver_datos_click <- shiny::reactiveVal(0)

  shiny::observeEvent(input$ver_datos, {
    ver_datos_click(ver_datos_click() + 1)
  })

  fechas <- shiny::reactive({
    if (is.null(input$fechas)) return(NULL)
    c(input$fechas[1], input$fechas[2])
  })

  variable_id <- shiny::reactive({
    if (is.null(input$variable) || input$variable == "") return(NULL)
    as.integer(input$variable)
  })

  estacion_ids <- shiny::reactive({
    eid <- estacion_id()
    if (is.null(eid)) return(integer(0))
    eid
  })

  ver_datos <- shiny::reactive(ver_datos_click())
  mapa_server("mapa", api_url, variable_id, fechas, estacion_id)
  grafico_server("grafico", api_url, estacion_id, variable_id, fechas, ver_datos)
  tabla_server("tabla", api_url, estacion_id, variable_id, fechas, ver_datos)
  descarga_server("descarga", api_url,
    shiny::reactive({ v <- variable_id(); if (is.null(v)) integer(0) else c(v) }),
    fechas,
    shiny::reactive({ e <- estacion_id(); if (is.null(e)) integer(0) else c(e) }))
}
