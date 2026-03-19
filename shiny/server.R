server <- function(input, output, session) {
  operador_aplicado <- shiny::reactiveVal("0")
  variable_aplicada <- shiny::reactiveVal("")
  region_aplicada <- shiny::reactiveVal("")
  comuna_aplicada <- shiny::reactiveVal("")
  estacion_id <- shiny::reactiveVal(NULL)
  ver_datos_click <- shiny::reactiveVal(0)
  periodo_seleccionado <- shiny::reactiveVal(NULL)

  variable_id <- shiny::reactive({  v <- input$variable
    if (is.null(v) || v == "" || v == "0") return(NULL)
    as.integer(v)
  })

  estacion_id_ver <- shiny::reactive({  e <- input$estacion
    if (is.null(e) || e == "" || e == "0") return(NULL)
    as.integer(e)
  })

  variables <- shiny::reactive({
    dat <- api_get("/variables", timeout = API_TIMEOUT_DEFAULT)
    if (is.null(dat)) return(data.frame(id = integer(), nombre = character()))
    if (is.data.frame(dat)) return(dat[, c("id", "nombre"), drop = FALSE])
    rows <- lapply(dat, function(x) data.frame(id = as.integer(x$id %||% NA), nombre = as.character(x$nombre %||% x$nombre_abr %||% ""), stringsAsFactors = FALSE))
    if (length(rows) == 0) return(data.frame(id = integer(), nombre = character()))
    df <- do.call(rbind, rows)
    df$id <- as.integer(df$id)
    df[, c("id", "nombre")]
  })

  regiones <- shiny::reactive({
    dat <- api_get("/regiones", list(chile_only = "1"), timeout = API_TIMEOUT_DEFAULT)
    if (is.null(dat)) return(data.frame(id = integer(), nombre = character()))
    if (is.data.frame(dat)) return(dat[, c("id", "nombre"), drop = FALSE])
    rows <- lapply(dat, function(x) data.frame(id = as.integer(x$id %||% NA), nombre = as.character(x$nombre %||% ""), stringsAsFactors = FALSE))
    if (length(rows) == 0) return(data.frame(id = integer(), nombre = character()))
    do.call(rbind, rows)
  })

  comunas <- shiny::reactive({
    reg <- region_aplicada()
    params <- list(chile_only = "1")
    if (!is.null(reg) && reg != "" && !is.na(as.integer(reg))) params$region <- as.integer(reg)
    dat <- api_get("/comunas", params, timeout = API_TIMEOUT_DEFAULT)
    if (is.null(dat)) return(data.frame(nombre = character()))
    if (is.data.frame(dat)) return(dat[, "nombre", drop = FALSE])
    rows <- lapply(dat, function(x) data.frame(nombre = as.character(x$nombre %||% ""), stringsAsFactors = FALSE))
    if (length(rows) == 0) return(data.frame(nombre = character()))
    do.call(rbind, rows)
  })

  fuentes <- shiny::reactive({
    dat <- api_get("/fuentes", timeout = API_TIMEOUT_DEFAULT)
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
    if (!is.null(var) && var != "" && var != "0") params$id_variable <- as.integer(var)
    reg <- region_aplicada()
    if (!is.null(reg) && reg != "" && !is.na(as.integer(reg))) params$region <- as.integer(reg)
    com <- comuna_aplicada()
    if (!is.null(com) && com != "") params$comuna <- trimws(as.character(com))
    dat <- shiny::withProgress(message = "Cargando estaciones...", value = 0, {
      shiny::setProgress(value = 0.3, message = "Consultando API...")
      api_get("/estaciones", params, timeout = API_TIMEOUT_ESTACIONES)
    })
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
    if (is.null(eid) || is.null(vid)) return(list(df = data.frame(), error = "Selecciona estacion y variable primero."))
    hoy <- Sys.Date()
    f_fin <- format(hoy, "%Y-%m-%d")
    f_ini <- format(hoy - 730, "%Y-%m-%d")
    params <- list(estacion_id = eid, variable_id = vid, fecha_inicio = f_ini, fecha_fin = f_fin)
    dat <- shiny::withProgress(message = "Cargando serie temporal...", value = 0.3, {
      api_get("/datos", params, timeout = API_TIMEOUT_DATOS)
    })
    if (is.null(dat)) return(list(df = data.frame(), error = "Error de conexion o timeout. La API puede estar ocupada. Intenta de nuevo en unos segundos."))
    if (is.list(dat) && !is.null(dat$error)) return(list(df = data.frame(), error = paste(dat$error, collapse = " ")))
    if (length(dat) == 0) return(list(df = data.frame(), error = NULL))
    if (is.data.frame(dat)) return(list(df = dat, error = NULL))
    df <- do.call(rbind, lapply(dat, function(x) data.frame(fecha = as.Date(x$fecha), valor = as.numeric(x$valor %||% NA), estacion_nombre = as.character(x$estacion_nombre %||% ""), variable_nombre = as.character(x$variable_nombre %||% ""), unidad = as.character(x$unidad %||% ""), stringsAsFactors = FALSE)))
    list(df = df, error = NULL)
  }, ignoreNULL = FALSE)

  shiny::observeEvent(input$aplicar_operador, { operador_aplicado(input$operador %||% "0") })
  shiny::observeEvent(input$quitar_filtro_operador, {
    shiny::updateSelectInput(session, "operador", selected = "0")
    operador_aplicado("0")
  })
  shiny::observeEvent(input$aplicar_variable, {
    v <- input$variable
    variable_aplicada(if (is.null(v) || v == "" || v == "0") "" else v)
  })
  shiny::observeEvent(input$quitar_filtro_variable, {
    shiny::updateSelectInput(session, "variable", selected = "")
    variable_aplicada("")
  })
  shiny::observeEvent(input$aplicar_region, {
    r <- input$region
    region_aplicada(if (is.null(r) || r == "" || r == "0") "" else r)
  })
  shiny::observeEvent(input$quitar_filtro_region, {
    shiny::updateSelectInput(session, "region", selected = "")
    region_aplicada("")
  })
  shiny::observeEvent(input$aplicar_comuna, {
    c <- input$comuna
    comuna_aplicada(if (is.null(c) || c == "") "" else trimws(as.character(c)))
  })
  shiny::observeEvent(input$quitar_filtro_comuna, {
    shiny::updateSelectInput(session, "comuna", selected = "")
    comuna_aplicada("")
  })
  shiny::observeEvent(input$aplicar_estacion, {
    e <- input$estacion
    estacion_id(if (is.null(e) || e == "" || e == "0") NULL else as.integer(e))
  })
  shiny::observeEvent(input$quitar_filtro_estacion, {
    shiny::updateSelectInput(session, "estacion", selected = "")
    estacion_id(NULL)
  })

  ver_serie_estacion_id <- shiny::reactiveVal(NULL)

  n_estaciones_efectivo <- shiny::reactive({
    eid <- estacion_id()
    if (!is.null(eid) && length(eid) > 0L) return(1L)
    e <- estaciones()
    if (is.null(e) || nrow(e) == 0) return(0L)
    nrow(e)
  })

  panel_modo <- shiny::reactive({
    if (ver_datos_click() < 1L) return("serie")
    if (!is.null(ver_serie_estacion_id())) return("serie")
    n <- n_estaciones_efectivo()
    if (n == 1L) return("serie")
    if (n > 1L) return("tabla")
    "serie"
  })

  estaciones_full <- shiny::reactive({
    if (panel_modo() != "tabla") return(NULL)
    params <- list(chile_only = "1", minimal = "")
    op <- operador_aplicado()
    if (!is.null(op) && op != "" && op != "0") params$id_fuente <- as.integer(op)
    var <- variable_aplicada()
    if (!is.null(var) && var != "" && var != "0") params$id_variable <- as.integer(var)
    reg <- region_aplicada()
    if (!is.null(reg) && reg != "" && !is.na(as.integer(reg))) params$region <- as.integer(reg)
    com <- comuna_aplicada()
    if (!is.null(com) && com != "") params$comuna <- trimws(as.character(com))
    dat <- api_get("/estaciones", params, timeout = API_TIMEOUT_ESTACIONES)
    if (is.null(dat)) return(NULL)
    if (is.data.frame(dat) && "id_fuente" %in% names(dat)) return(dat)
    if (is.list(dat) && length(dat) > 0) {
      rows <- lapply(dat, function(x) data.frame(id = as.integer(x$id %||% NA), id_fuente = as.integer(x$id_fuente %||% NA), stringsAsFactors = FALSE))
      return(do.call(rbind, rows))
    }
    NULL
  })

  shiny::observe({
    session$sendCustomMessage("panel_modo", list(modo = panel_modo()))
  })

  shiny::observeEvent(input$ver_datos, {
    ver_serie_estacion_id(NULL)
    e <- estaciones()
    n <- if (is.null(e) || nrow(e) == 0) 0L else nrow(e)
    if (n == 1L) {
      eid <- as.integer(e$id[1])
      estacion_id(eid)
      shiny::updateSelectInput(session, "estacion", selected = as.character(eid))
    }
    ver_datos_click(ver_datos_click() + 1)
    session$sendCustomMessage("panel_datos_state", 1)
    session$sendCustomMessage("drawer_close", TRUE)
  })
  shiny::observeEvent(input$panel_close, { session$sendCustomMessage("panel_datos_state", 0) })
  shiny::observeEvent(input$panel_minimize, { session$sendCustomMessage("panel_datos_state", 2) })
  shiny::observeEvent(input$panel_maximize, {
    s <- input$panel_datos_state
    next_val <- if (is.null(s) || s == 2 || s == 3) 1L else 3L
    session$sendCustomMessage("panel_datos_state", next_val)
  })

  shiny::observe({
    v <- variables()
    if (nrow(v) > 0) {
      ch <- c("Variable" = "", setNames(as.character(v$id), v$nombre))
      curr <- input$variable
      sel <- if (!is.null(curr) && nzchar(curr) && curr %in% as.character(v$id)) curr else ""
      shiny::updateSelectInput(session, "variable", choices = ch, selected = sel)
    }
  })

  shiny::observe({
    f <- fuentes()
    if (nrow(f) > 0) {
      ch <- c("Operador" = "0", setNames(as.character(f$id), f$nombre))
      shiny::updateSelectInput(session, "operador", choices = ch, selected = "0")
    }
  })

  shiny::observe({
    r <- regiones()
    if (nrow(r) > 0) {
      ch <- c("Region" = "", setNames(as.character(r$id), r$nombre))
      reg_ap <- region_aplicada()
      sel <- if (!is.null(reg_ap) && nzchar(reg_ap) && reg_ap %in% as.character(r$id)) reg_ap else ""
      shiny::updateSelectInput(session, "region", choices = ch, selected = sel)
    }
  })

  shiny::observe({
    com <- comunas()
    if (nrow(com) > 0) {
      ch <- c("Comuna" = "", setNames(com$nombre, com$nombre))
      com_ap <- comuna_aplicada()
      sel <- if (!is.null(com_ap) && nzchar(com_ap) && com_ap %in% com$nombre) com_ap else ""
      shiny::updateSelectInput(session, "comuna", choices = ch, selected = sel)
    }
  })

  shiny::observe({
    e <- estaciones()
    ch <- c("Estacion" = "")
    if (nrow(e) > 0) ch <- c(ch, setNames(as.character(e$id), e$nombre))
    curr <- input$estacion
    eid <- estacion_id()
    sel <- if (!is.null(curr) && nzchar(curr) && curr %in% as.character(e$id)) curr
           else if (!is.null(eid) && length(eid) > 0 && eid %in% e$id) as.character(eid) else ""
    shiny::updateSelectInput(session, "estacion", choices = ch, selected = sel)
  })

  output$datos_listos <- shiny::renderText({
    if (ver_datos_click() > 0) "1" else "0"
  })
  output$contador_estaciones <- shiny::renderText({
    e <- estaciones()
    eid <- estacion_id()
    if (!is.null(eid) && length(eid) > 0) {
      "1 estacion seleccionada"
    } else if (nrow(e) > 0) {
      paste0(nrow(e), " estaciones visibles")
    } else {
      "Cargando estaciones..."
    }
  })
  shiny::observe({
    session$sendCustomMessage("set_btn_estado", list(id = "aplicar_operador", activo = operador_aplicado() != "0"))
  })
  shiny::observe({
    session$sendCustomMessage("set_btn_estado", list(id = "aplicar_variable", activo = variable_aplicada() != ""))
  })
  shiny::observe({
    session$sendCustomMessage("set_btn_estado", list(id = "aplicar_region", activo = region_aplicada() != ""))
  })
  shiny::observe({
    session$sendCustomMessage("set_btn_estado", list(id = "aplicar_comuna", activo = comuna_aplicada() != ""))
  })
  shiny::observe({
    session$sendCustomMessage("set_btn_estado", list(id = "aplicar_estacion", activo = !is.null(estacion_id()) && length(estacion_id()) > 0))
  })
  shiny::outputOptions(output, "datos_listos", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "contador_estaciones", suspendWhenHidden = FALSE)

  ver_serie_callback <- function(eid) {
    estacion_id(eid)
    shiny::updateSelectInput(session, "estacion", selected = as.character(eid))
    ver_serie_estacion_id(eid)
    ver_datos_click(ver_datos_click() + 1)
  }

  tabla_res <- tabla_estaciones_server("tabla_estaciones",
    estaciones = estaciones,
    fuentes = fuentes,
    variable_id = shiny::reactive(variable_aplicada()),
    ver_serie_callback = ver_serie_callback,
    estaciones_full = estaciones_full,
    panel_modo = panel_modo
  )

  estacion_id_descarga <- shiny::reactive({
    if (panel_modo() == "tabla") return(tabla_res$estaciones_seleccionadas())
    eid <- estacion_id()
    if (is.null(eid) || length(eid) == 0) return(integer(0))
    c(eid)
  })
  variable_id_descarga <- shiny::reactive({
    if (panel_modo() == "tabla") {
      v <- variable_aplicada()
      if (is.null(v) || v == "" || v == "0") return(NULL)
      return(as.integer(v))
    }
    variable_id()
  })
  periodo_descarga <- shiny::reactive({
    if (panel_modo() == "serie") return(periodo_seleccionado())
    c(Sys.Date() - 730, Sys.Date())
  })

  mapa_server("mapa", estaciones, estacion_id)
  grafico_server("grafico", estacion_id_ver, variable_id, ver_datos_click, datos, periodo_seleccionado)
  descarga_server("descarga", variable_id_descarga, estacion_id_descarga, periodo_descarga)
}



