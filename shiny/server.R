server <- function(input, output, session) {
  operador_aplicado <- shiny::reactiveVal("0")
  variable_aplicada <- shiny::reactiveVal("")
  estacion_id <- shiny::reactiveVal(NULL)
  ver_datos_click <- shiny::reactiveVal(0)

  variable_id <- shiny::reactive({
    v <- input$variable
    if (is.null(v) || v == "") return(NULL)
    as.integer(v)
  })

  estacion_id_ver <- shiny::reactive({
    e <- input$estacion
    if (is.null(e) || e == "") return(NULL)
    as.integer(e)
  })

  variables <- shiny::reactive({
    dat <- api_get("/variables")
    if (is.null(dat)) return(data.frame(id = integer(), nombre = character()))
    if (is.data.frame(dat)) return(dat[, c("id", "nombre"), drop = FALSE])
    rows <- lapply(dat, function(x) data.frame(id = as.integer(x$id %||% NA), nombre = as.character(x$nombre %||% x$nombre_abr %||% ""), stringsAsFactors = FALSE))
    if (length(rows) == 0) return(data.frame(id = integer(), nombre = character()))
    df <- do.call(rbind, rows)
    df$id <- as.integer(df$id)
    df[, c("id", "nombre")]
  })

  fuentes <- shiny::reactive({
    dat <- api_get("/fuentes")
    if (is.null(dat)) return(data.frame(id = integer(), nombre = character()))
    if (is.data.frame(dat)) return(dat[, c("id", "nombre"), drop = FALSE])
    rows <- lapply(dat, function(x) data.frame(id = as.integer(x$id %||% NA), nombre = as.character(x$nombre %||% ""), stringsAsFactors = FALSE))
    if (length(rows) == 0) return(data.frame(id = integer(), nombre = character()))
    do.call(rbind, rows)
  })

  estaciones <- shiny::reactive({
    params <- list(chile_only = "1", minimal = "1")
    op <- operador_aplicado()
    if (!is.null(op) && op != "" && op != "0") params$id_fuente <- as.integer(op)
    var <- variable_aplicada()
    if (!is.null(var) && var != "") params$id_variable <- as.integer(var)
    dat <- api_get("/estaciones", params, timeout = 30)
    if (is.null(dat)) return(data.frame(id = integer(), nombre = character(), lat = numeric(), lon = numeric(), macrozona = character()))
    if (is.data.frame(dat)) {
      for (c in c("id", "nombre", "lat", "lon", "macrozona")) if (!c %in% names(dat)) dat[[c]] <- NA
      dat <- dat[!is.na(dat$lat) & !is.na(dat$lon), c("id", "nombre", "lat", "lon", "macrozona"), drop = FALSE]
      return(dat)
    }
    rows <- lapply(dat, function(x) data.frame(id = as.integer(x$id %||% NA), nombre = as.character(x$nombre %||% ""), lat = as.numeric(x$lat %||% NA), lon = as.numeric(x$lon %||% NA), macrozona = as.character(x$macrozona %||% ""), stringsAsFactors = FALSE))
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) == 0) return(data.frame(id = integer(), nombre = character(), lat = numeric(), lon = numeric(), macrozona = character()))
    df <- do.call(rbind, rows)
    df <- df[!is.na(df$lat) & df$lat >= -56 & df$lat <= -17 & df$lon >= -76 & df$lon <= -66, , drop = FALSE]
    df
  })

  datos <- shiny::eventReactive(ver_datos_click(), {
    eid <- estacion_id_ver()
    vid <- variable_id()
    if (is.null(eid) || is.null(vid)) return(list(df = data.frame(), error = NULL))
    dat <- api_get("/datos", list(estacion_id = eid, variable_id = vid), timeout = 30)
    if (is.null(dat)) return(list(df = data.frame(), error = "Error de conexion"))
    if (is.list(dat) && !is.null(dat$error)) return(list(df = data.frame(), error = paste(dat$error, collapse = " ")))
    if (length(dat) == 0) return(list(df = data.frame(), error = NULL))
    if (is.data.frame(dat)) return(list(df = dat, error = NULL))
    df <- do.call(rbind, lapply(dat, function(x) data.frame(fecha = as.Date(x$fecha), valor = as.numeric(x$valor %||% NA), estacion_nombre = as.character(x$estacion_nombre %||% ""), variable_nombre = as.character(x$variable_nombre %||% ""), unidad = as.character(x$unidad %||% ""), stringsAsFactors = FALSE)))
    list(df = df, error = NULL)
  }, ignoreNULL = FALSE)

  shiny::observeEvent(input$aplicar_operador, { operador_aplicado(input$operador %||% "0") })
  shiny::observeEvent(input$aplicar_variable, { variable_aplicada(input$variable %||% "") })
  shiny::observeEvent(input$aplicar_estacion, {
    e <- input$estacion
    estacion_id(if (nzchar(e)) as.integer(e) else NULL)
  })

  shiny::observeEvent(input$ver_datos, { ver_datos_click(ver_datos_click() + 1) })

  shiny::observe({
    v <- variables()
    if (nrow(v) > 0) {
      ch <- c("Todas" = "", setNames(as.character(v$id), v$nombre))
      shiny::updateSelectInput(session, "variable", choices = ch, selected = "")
    }
  })

  shiny::observe({
    f <- fuentes()
    if (nrow(f) > 0) {
      ch <- c("Todas" = "0", setNames(as.character(f$id), f$nombre))
      shiny::updateSelectInput(session, "operador", choices = ch, selected = "0")
    }
  })

  shiny::observe({
    e <- estaciones()
    ch <- c("Selecciona estacion" = "")
    if (nrow(e) > 0) ch <- c(ch, setNames(as.character(e$id), e$nombre))
    eid <- estacion_id()
    sel <- if (!is.null(eid) && eid %in% e$id) as.character(eid) else ""
    shiny::updateSelectInput(session, "estacion", choices = ch, selected = sel)
  })

  output$datos_listos <- shiny::renderText({
    if (ver_datos_click() > 0) "1" else "0"
  })
  shiny::outputOptions(output, "datos_listos", suspendWhenHidden = FALSE)

  mapa_server("mapa", estaciones, estacion_id)
  grafico_server("grafico", estacion_id_ver, variable_id, ver_datos_click, datos)
  tabla_server("tabla", datos)
  descarga_server("descarga", variable_id, estacion_id_ver)
}

