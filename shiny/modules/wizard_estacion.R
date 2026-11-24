# Wizard modal: configurar mi estacion favorita (3 pasos)

  wizard_estacion_server <- function(input, output, session, variables, fuentes, on_abrir, on_aplicar_favorito, on_fijar_favorito = NULL) {
  wizard_step <- shiny::reactiveVal(1L)
  wizard_lat <- shiny::reactiveVal(NA_real_)
  wizard_lon <- shiny::reactiveVal(NA_real_)
  wizard_results <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$wizard_open, {
    wizard_step(1L)
    wizard_lat(NA_real_)
    wizard_lon(NA_real_)
    wizard_results(NULL)
    shiny::showModal(shiny::modalDialog(
      title = "Configurar mi estaci\u00f3n",
      shiny::uiOutput("wizard_estacion_body"),
      footer = shiny::tagList(
        shiny::modalButton("Cancelar"),
        shiny::uiOutput("wizard_estacion_footer")
      ),
      easyClose = TRUE,
      size = "m"
    ))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$wizard_geo_lat, {
    lat <- suppressWarnings(as.numeric(input$wizard_geo_lat))
    lon <- suppressWarnings(as.numeric(input$wizard_geo_lon))
    if (!is.na(lat) && !is.na(lon)) {
      wizard_lat(lat)
      wizard_lon(lon)
      wizard_step(2L)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$wizard_buscar_cercanas, {
    lat <- wizard_lat()
    lon <- wizard_lon()
    if (is.na(lat) || is.na(lon)) {
      session$sendCustomMessage("ec_fav_toast", list(text = "Indica tu ubicaci\u00f3n primero.", ok = FALSE))
      return()
    }
    op <- input$wizard_operador
    var <- input$wizard_variable
    params <- list(
      lat = lat,
      lon = lon,
      limit = 5,
      con_datos = "1",
      chile_only = "1",
      minimal = "1"
    )
    if (!is.null(op) && nzchar(op) && op != "0") params$id_fuente <- as.integer(op)
    if (!is.null(var) && nzchar(var)) params$id_variable <- as.integer(var)
    dat <- api_get("/estaciones/cercanas", params, timeout = API_TIMEOUT_ESTACIONES)
    rows <- dat$data %||% dat
    if (is.null(rows) || (is.data.frame(rows) && nrow(rows) == 0) || (is.list(rows) && length(rows) == 0)) {
      wizard_results(data.frame())
      wizard_step(3L)
      return()
    }
    if (is.data.frame(rows)) {
      wizard_results(rows)
    } else {
      wizard_results(do.call(rbind, lapply(rows, function(x) {
        data.frame(
          id = as.integer(x$id %||% NA),
          nombre = as.character(x$nombre %||% ""),
          dist_km = as.numeric(x$dist_km %||% NA),
          macrozona = as.character(x$macrozona %||% ""),
          nivel = as.character(x$nivel %||% ""),
          stringsAsFactors = FALSE
        )
      })))
    }
    wizard_step(3L)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$wizard_usar_geo, {
    session$sendCustomMessage("ec_wizard_request_geo", TRUE)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$wizard_abrir_eid, {
    eid <- suppressWarnings(as.integer(input$wizard_abrir_eid))
    if (is.na(eid) || eid < 1L) return()
    shiny::removeModal()
    if (is.function(on_aplicar_favorito)) {
      fav <- list(
        eid = eid,
        variableId = suppressWarnings(as.integer(input$wizard_variable %||% NA)),
        fuenteId = suppressWarnings(as.integer(input$wizard_operador %||% NA)),
        alias = NULL
      )
      df <- wizard_results()
      if (!is.null(df) && is.data.frame(df) && eid %in% df$id) {
        fav$alias <- as.character(df$nombre[df$id == eid][1])
      }
      on_aplicar_favorito(fav)
    }
    if (is.function(on_abrir)) on_abrir(eid)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$wizard_guardar_eid, {
    eid <- suppressWarnings(as.integer(input$wizard_guardar_eid))
    if (is.na(eid) || eid < 1L) return()
    fav <- list(
      eid = eid,
      variableId = suppressWarnings(as.integer(input$wizard_variable %||% NA)),
      fuenteId = suppressWarnings(as.integer(input$wizard_operador %||% NA)),
      alias = NULL,
      lat = wizard_lat(),
      lon = wizard_lon()
    )
    df <- wizard_results()
    if (!is.null(df) && is.data.frame(df) && eid %in% df$id) {
      fav$alias <- as.character(df$nombre[df$id == eid][1])
    }
    if (is.function(on_fijar_favorito)) {
      on_fijar_favorito(fav)
    } else {
      session$sendCustomMessage("ec_fijar_favorito", fav)
    }
    shiny::removeModal()
  }, ignoreInit = TRUE)

  output$wizard_estacion_body <- shiny::renderUI({
    step <- wizard_step()
    if (step == 1L) {
      return(shiny::tagList(
        shiny::tags$div(class = "ec-wizard-step",
          shiny::tags$p("Encuentra la estaci\u00f3n m\u00e1s cercana con datos de la variable que te interesa."),
          shiny::actionButton("wizard_usar_geo", "Usar mi ubicaci\u00f3n",
            icon = shiny::icon("location-arrow"), class = "btn-primary"),
          shiny::tags$p(class = "text-muted", style = "margin-top:12px;",
            "Si deniegas el permiso, vuelve al mapa y selecciona una comuna o estaci\u00f3n manualmente.")
        )
      ))
    }
    if (step == 2L) {
      v <- variables()
      f <- fuentes()
      v_ch <- if (nrow(v) > 0) setNames(as.character(v$id), v$nombre) else character()
      f_ch <- if (nrow(f) > 0) c("Cualquiera" = "0", setNames(as.character(f$id), f$nombre)) else c("Cualquiera" = "0")
      return(shiny::tagList(
        shiny::tags$div(class = "ec-wizard-step",
          shiny::selectInput("wizard_operador", "Operador", choices = f_ch, selected = "0"),
          shiny::selectInput("wizard_variable", "Variable", choices = v_ch,
            selected = if (length(v_ch)) v_ch[1] else NULL),
          shiny::actionButton("wizard_buscar_cercanas", "Buscar estaciones cercanas",
            icon = shiny::icon("search"), class = "btn-primary")
        )
      ))
    }
    df <- wizard_results()
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(shiny::tags$div(class = "ec-wizard-step",
        shiny::tags$p("No se encontraron estaciones con esos criterios. Prueba otro operador o variable.")
      ))
    }
    shiny::tags$div(class = "ec-wizard-step ec-wizard-results",
      shiny::tags$table(class = "table table-condensed",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th("Estaci\u00f3n"),
          shiny::tags$th("Distancia"),
          shiny::tags$th("")
        )),
        shiny::tags$tbody(lapply(seq_len(nrow(df)), function(i) {
          eid <- as.integer(df$id[i])
          dist <- df$dist_km[i]
          dist_s <- if (!is.na(dist)) paste0(round(dist, 1), " km") else "\u2014"
          shiny::tags$tr(
            shiny::tags$td(
              htmltools::htmlEscape(as.character(df$nombre[i])),
              shiny::HTML(ec_calidad_chip_html(if ("nivel" %in% names(df)) df$nivel[i] else ""))
            ),
            shiny::tags$td(dist_s),
            shiny::tags$td(
              shiny::actionButton(
                paste0("wizard_usar_", eid),
                "Usar",
                class = "btn-xs btn-default",
                onclick = sprintf("Shiny.setInputValue('wizard_abrir_eid',%d,{priority:'event'})", eid)
              ),
              " ",
              shiny::actionButton(
                paste0("wizard_guardar_", eid),
                "Guardar",
                class = "btn-xs btn-primary",
                onclick = sprintf("Shiny.setInputValue('wizard_guardar_eid',%d,{priority:'event'})", eid)
              )
            )
          )
        }))
      )
    )
  })

  output$wizard_estacion_footer <- shiny::renderUI({
    step <- wizard_step()
    if (step == 1L) return(NULL)
    if (step == 2L) {
      return(shiny::actionButton("wizard_back_1", "Atr\u00e1s",
        onclick = "Shiny.setInputValue('wizard_back_step',1,{priority:'event'})"))
    }
    shiny::actionButton("wizard_back_2", "Atr\u00e1s",
      onclick = "Shiny.setInputValue('wizard_back_step',2,{priority:'event'})")
  })

  shiny::observeEvent(input$wizard_back_step, {
    s <- suppressWarnings(as.integer(input$wizard_back_step))
    if (!is.na(s) && s >= 1L && s <= 2L) wizard_step(s)
  }, ignoreInit = TRUE)
}
