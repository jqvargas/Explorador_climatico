server <- function(input, output, session) {
  api_url <- shiny::reactive({
    u <- Sys.getenv("API_URL", "http://api:8000")
    sub("/$", "", u)
  })

  parse_variables_response <- function(dat) {
    if (is.null(dat)) return(data.frame(id = integer(), nombre = character()))
    if (is.data.frame(dat)) {
      if (!"nombre" %in% names(dat)) dat$nombre <- as.character(dat$id)
      return(dat[, c("id", "nombre"), drop = FALSE])
    }
    if (is.list(dat) && length(dat) > 0) {
      first <- dat[[1]]
      if (is.atomic(first)) dat <- list(dat)
      rows <- lapply(dat, function(x) {
        if (is.atomic(x)) return(NULL)
        data.frame(id = x$id %||% NA_integer_, nombre = as.character(x$nombre %||% x$nombre_abr %||% ""), stringsAsFactors = FALSE)
      })
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) == 0) return(data.frame(id = integer(), nombre = character()))
      df <- do.call(rbind, rows)
      df$id <- as.integer(df$id)
      return(df)
    }
    data.frame(id = integer(), nombre = character())
  }

  variables <- shiny::reactive({
    base <- api_url()
    resp <- tryCatch(httr::GET(paste0(base, "/variables"), httr::timeout(5)), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) != 200) return(data.frame(id = integer(), nombre = character()))
    dat <- httr::content(resp, as = "parsed", type = "application/json")
    if (is.null(dat)) return(data.frame(id = integer(), nombre = character()))
    if (is.data.frame(dat)) {
      if (!"nombre" %in% names(dat)) dat$nombre <- as.character(dat$id)
      return(dat[, c("id", "nombre"), drop = FALSE])
    }
    if (is.list(dat) && length(dat) > 0) {
      first <- dat[[1]]
      if (is.atomic(first)) dat <- list(dat)
      rows <- lapply(dat, function(x) {
        if (is.atomic(x)) return(NULL)
        data.frame(id = x$id %||% NA_integer_, nombre = as.character(x$nombre %||% x$nombre_abr %||% ""), stringsAsFactors = FALSE)
      })
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) == 0) return(data.frame(id = integer(), nombre = character()))
      df <- do.call(rbind, rows)
      df$id <- as.integer(df$id)
      return(df)
    }
    data.frame(id = integer(), nombre = character())
  })


  estacion_id <- shiny::reactiveVal(NULL)

  variables_filtradas <- shiny::reactive({
    base <- api_url()
    eid <- estacion_id()
    url <- if (!is.null(eid)) paste0(base, "/variables?estacion_id=", eid) else paste0(base, "/variables")
    resp <- tryCatch(httr::GET(url, httr::timeout(10)), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) != 200) return(data.frame(id = integer(), nombre = character()))
    dat <- httr::content(resp, as = "parsed", type = "application/json")
    if (is.null(dat)) return(data.frame(id = integer(), nombre = character()))
    if (is.data.frame(dat)) {
      if (!"nombre" %in% names(dat)) dat$nombre <- as.character(dat$id)
      return(dat[, c("id", "nombre"), drop = FALSE])
    }
    if (is.list(dat) && length(dat) > 0) {
      rows <- lapply(dat, function(x) {
        if (is.atomic(x)) return(NULL)
        data.frame(id = x$id %||% NA_integer_, nombre = as.character(x$nombre %||% x$nombre_abr %||% ""), stringsAsFactors = FALSE)
      })
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) == 0) return(data.frame(id = integer(), nombre = character()))
      df <- do.call(rbind, rows)
      df$id <- as.integer(df$id)
      return(df)
    }
    data.frame(id = integer(), nombre = character())
  })

  fuentes_filtradas <- shiny::reactive({
    base <- api_url()
    eid <- estacion_id()
    url <- if (!is.null(eid)) paste0(base, "/fuentes?estacion_id=", eid) else paste0(base, "/fuentes")
    resp <- tryCatch(httr::GET(url, httr::timeout(10)), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) != 200) return(data.frame(id = integer(), nombre = character()))
    dat <- httr::content(resp, as = "parsed", type = "application/json")
    if (is.null(dat)) return(data.frame(id = integer(), nombre = character()))
    if (is.data.frame(dat)) return(dat[, c("id", "nombre"), drop = FALSE])
    if (is.list(dat) && length(dat) > 0) {
      rows <- lapply(dat, function(x) {
        if (is.atomic(x)) return(NULL)
        data.frame(id = x$id %||% NA_integer_, nombre = as.character(x$nombre %||% ""), stringsAsFactors = FALSE)
      })
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) == 0) return(data.frame(id = integer(), nombre = character()))
      df <- do.call(rbind, rows)
      df$id <- as.integer(df$id)
      return(df)
    }
    data.frame(id = integer(), nombre = character())
  })
  fuentes <- shiny::reactive({
    base <- api_url()
    resp <- tryCatch(httr::GET(paste0(base, "/fuentes"), httr::timeout(5)), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) != 200) return(data.frame(id = integer(), nombre = character()))
    dat <- httr::content(resp, as = "parsed", type = "application/json")
    if (is.null(dat)) return(data.frame(id = integer(), nombre = character()))
    if (is.data.frame(dat)) return(dat[, c("id", "nombre"), drop = FALSE])
    if (is.list(dat) && length(dat) > 0) {
      rows <- lapply(dat, function(x) {
        if (is.atomic(x)) return(NULL)
        data.frame(id = x$id %||% NA_integer_, nombre = as.character(x$nombre %||% ""), stringsAsFactors = FALSE)
      })
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) == 0) return(data.frame(id = integer(), nombre = character()))
      df <- do.call(rbind, rows)
      df$id <- as.integer(df$id)
      return(df)
    }
    data.frame(id = integer(), nombre = character())
  })
  estaciones <- shiny::reactive({
    base <- api_url()
    op <- input$operador
    vid <- variable_id()
    url <- paste0(base, "/estaciones?chile_only=1&minimal=1")
    if (!is.null(op) && op != "" && op != "0") url <- paste0(url, "&id_fuente=", op)
    if (!is.null(vid)) url <- paste0(url, "&id_variable=", vid)
    resp <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) != 200) return(data.frame(id = integer(), nombre = character(), lat = numeric(), lon = numeric(), macrozona = character()))
    dat <- httr::content(resp, as = "parsed", type = "application/json")
    if (is.null(dat)) return(data.frame(id = integer(), nombre = character(), lat = numeric(), lon = numeric(), macrozona = character()))
    cols_needed <- c("id", "nombre", "lat", "lon", "macrozona")
    if (inherits(dat, "data.frame")) {
      df <- as.data.frame(dat)
      for (c in cols_needed) if (!c %in% names(df)) df[[c]] <- if (c == "id") NA_integer_ else if (c %in% c("lat", "lon")) NA_real_ else NA_character_
      df <- df[, cols_needed[cols_needed %in% names(df)], drop = FALSE]
      df <- df[!is.na(df$lat) & !is.na(df$lon), , drop = FALSE]
      return(df)
    }
    if (is.list(dat) && length(dat) > 0) {
      if (identical(names(dat), "data") && is.list(dat$data)) dat <- dat$data
      rows <- lapply(dat, function(x) {
        if (is.atomic(x)) return(NULL)
        data.frame(id = as.integer(x$id %||% NA), nombre = as.character(x$nombre %||% ""), lat = as.numeric(x$lat %||% NA), lon = as.numeric(x$lon %||% NA), macrozona = as.character(x$macrozona %||% ""), stringsAsFactors = FALSE)
      })
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) == 0) return(data.frame(id = integer(), nombre = character(), lat = numeric(), lon = numeric(), macrozona = character()))
      df <- as.data.frame(do.call(rbind, rows))
      df$id <- as.integer(df$id)
      df <- df[!is.na(df$lat) & !is.na(df$lon) & df$lat >= -56 & df$lat <= -17 & df$lon >= -76 & df$lon <= -66, , drop = FALSE]
      return(df)
    }
    data.frame(id = integer(), nombre = character(), lat = numeric(), lon = numeric(), macrozona = character())
  })

  shiny::observe({
    v <- variables_filtradas()
    if (nrow(v) > 0) {
      ch <- setNames(as.character(v$id), v$nombre)
      cur <- input$variable
      sel <- if (!is.null(cur) && cur != "" && cur %in% as.character(v$id)) cur else as.character(v$id[1])
      shiny::updateSelectInput(session, "variable", choices = ch, selected = sel)
    } else {
      shiny::updateSelectInput(session, "variable", choices = setNames("Sin variables (revisa API)", ""), selected = "")
    }
  })

  shiny::observe({
    f <- fuentes_filtradas()
    if (nrow(f) > 0) {
      eid <- estacion_id()
      if (!is.null(eid)) {
        ch <- setNames(as.character(f$id), f$nombre)
        shiny::updateSelectInput(session, "operador", choices = ch, selected = as.character(f$id[1]))
      } else {
        ch <- c(setNames("0", "Todas"), setNames(as.character(f$id), f$nombre))
        shiny::updateSelectInput(session, "operador", choices = ch, selected = "0")
      }
    } else {
      shiny::updateSelectInput(session, "operador", choices = setNames("Todas", "0"), selected = "0")
    }
  })

  shiny::observe({
    e <- estaciones()
    ch <- c("Selecciona una estacion..." = "")
    if (nrow(e) > 0) ch <- c(ch, setNames(as.character(e$id), e$nombre))
    eid <- estacion_id()
    sel <- if (!is.null(eid) && eid %in% e$id) as.character(eid) else ""
    shiny::updateSelectInput(session, "estacion", choices = ch, selected = sel)
  })
  shiny::observeEvent(input$estacion, {
    if (!is.null(input$estacion) && input$estacion != "") {
      estacion_id(as.integer(input$estacion))
    } else {
      estacion_id(NULL)
    }
  })
  shiny::observeEvent(estacion_id(), {
    eid <- estacion_id()
    if (!is.null(eid) && (is.null(input$estacion) || input$estacion != as.character(eid))) {
      shiny::updateSelectInput(session, "estacion", selected = as.character(eid))
    }
  }, ignoreInit = TRUE)

  ver_datos_click <- shiny::reactiveVal(0)
  shiny::observeEvent(input$ver_datos, { ver_datos_click(ver_datos_click() + 1) })

  variable_id <- shiny::reactive({
    if (is.null(input$variable) || input$variable == "") return(NULL)
    as.integer(input$variable)
  })

  operador_id <- shiny::reactive({
    if (is.null(input$operador) || input$operador == "" || input$operador == "0") return(NULL)
    as.integer(input$operador)
  })

  mapa_server("mapa", api_url, variable_id, shiny::reactive(NULL), estacion_id, operador_id)
  grafico_server("grafico", api_url, estacion_id, variable_id, ver_datos_click)
  tabla_server("tabla", api_url, estacion_id, variable_id, ver_datos_click)
  descarga_server("descarga", api_url, shiny::reactive(c(variable_id())), shiny::reactive(c(estacion_id())))

}














