# onboarding.R — tour guiado (cicerone) para el Explorador
# Solo a demanda: botón «Guía de Uso». Sin autostart.

ec_onboarding_step_catalog <- function() {
  list(
    welcome = list(
      el = "ec_explorador_viewport",
      is_id = TRUE,
      title = "Gu\u00eda de uso",
      description = paste0(
        "Empieza por el mapa: primero eliges una zona, despu\u00e9s una estaci\u00f3n. ",
        "Los datos observados (precipitaci\u00f3n, temperatura y viento) vienen de estaciones p\u00fablicas de Chile; el pron\u00f3stico, de cada estaci\u00f3n."
      ),
      position = "mid-center",
      class = "ec-cicerone-pop"
    ),
    mapa = list(
      el = "ec-choropleth-bc",
      is_id = TRUE,
      title = "Elige una zona",
      description = paste0(
        "Haz clic en el mapa: Chile \u2192 zona \u2192 regi\u00f3n \u2192 comuna. ",
        "Las estaciones aparecen al llegar a una comuna. Para volver atr\u00e1s, pulsa Chile o la regi\u00f3n en la barra de arriba."
      ),
      position = "bottom",
      class = "ec-cicerone-pop"
    ),
    pronostico = list(
      el = "ec-icon-vars",
      is_id = TRUE,
      title = "Pron\u00f3stico en el mapa",
      description = paste0(
        "El color del mapa muestra el pron\u00f3stico de temperatura, precipitaci\u00f3n o viento. ",
        "Los n\u00fameros sobre las estaciones son el pron\u00f3stico en ese punto. La barra de abajo cambia el d\u00eda."
      ),
      position = "bottom",
      class = "ec-cicerone-pop"
    ),
    pronostico_dock = list(
      el = "ec-map-capa-icon",
      is_id = TRUE,
      title = "Pron\u00f3stico en el mapa",
      description = paste0(
        "El color del mapa muestra el pron\u00f3stico de temperatura, precipitaci\u00f3n o viento. ",
        "Los n\u00fameros sobre las estaciones son el pron\u00f3stico en ese punto. La barra de abajo cambia el d\u00eda."
      ),
      position = "top",
      class = "ec-cicerone-pop"
    ),
    leyenda = list(
      el = "ec_map_legend",
      is_id = TRUE,
      title = "Color de las estaciones",
      description = paste0(
        "El color de cada punto indica la calidad del dato. Haz clic en una estaci\u00f3n ",
        "para ver el reporte, descargar o guardarla como Mi estaci\u00f3n. Los s\u00edmbolos extra marcan extremos pronosticados."
      ),
      position = "left",
      class = "ec-cicerone-pop"
    ),
    geo = list(
      el = "map_usar_geo",
      is_id = TRUE,
      title = "Tu ubicaci\u00f3n",
      description = paste0(
        "Este bot\u00f3n centra el mapa en ti y muestra las estaciones cercanas."
      ),
      position = "right",
      class = "ec-cicerone-pop"
    )
  )
}

# cicerone evalúa callbacks como new Function("return " + code)() — hay que devolver
# function(){ ... } o se ejecutan todos al init (ver cicerone.js en el paquete).
ec_onboarding_js_fn <- function(body) {
  body <- trimws(as.character(body))
  if (!nzchar(body)) return(NULL)
  sprintf("function(){ %s }", body)
}

ec_onboarding_start <- function(session, payload_json) {
  if (!requireNamespace("cicerone", quietly = TRUE)) {
    message("[onboarding] paquete cicerone no instalado")
    return(invisible(NULL))
  }
  payload <- tryCatch(
    jsonlite::fromJSON(payload_json, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(payload) || is.null(payload$steps) || length(payload$steps) == 0L) {
    return(invisible(NULL))
  }

  steps <- as.character(unlist(payload$steps))
  catalog <- ec_onboarding_step_catalog()
  total <- length(steps)

  guide <- cicerone::Cicerone$new(
    id = "ec_onboarding",
    opacity = 0.55,
    padding = 4L,
    allow_close = TRUE,
    overlay_click_next = FALSE,
    close_btn_text = "Salir",
    done_btn_text = "Finalizar",
    next_btn_text = "Siguiente",
    prev_btn_text = "Anterior",
    stage_background = "#F7F9FA",
    keyboard_control = TRUE
  )

  idx <- 0L
  for (key in steps) {
    def <- catalog[[key]]
    if (is.null(def)) next
    idx <- idx + 1L
    is_last <- idx >= total

    hs_body <- sprintf("ecOnboardingSetProgress(%d,%d,%s);", idx, total, shQuote(key, type = "cmd"))
    hn_body <- character(0)
    if (is_last) {
      hn_body <- c(hn_body, "ecOnboardingMarkDone();")
    }

    step_args <- list(
      el = def$el,
      title = def$title,
      description = def$description,
      position = def$position,
      class = def$class,
      is_id = def$is_id,
      on_highlight_started = ec_onboarding_js_fn(hs_body),
      on_next = ec_onboarding_js_fn(paste0(hn_body, collapse = ""))
    )
    if (isTRUE(def$on_highlighted)) {
      step_args$on_highlighted <- ec_onboarding_js_fn(
        "ecOnboardingScrollHighlight(); ecOnboardingScheduleFixPopover(window.__ecOnboardingStepKey);"
      )
    }
    guide <- do.call(guide$step, step_args)
  }

  session$sendCustomMessage("ec_onboarding_active", TRUE)
  guide$init(session = session)$start()
  invisible(NULL)
}

onboarding_server <- function(input, session, mapa_habilitado) {
  shiny::observeEvent(input$ec_onboarding_run, {
    raw <- input$ec_onboarding_run
    if (is.null(raw) || !nzchar(trimws(as.character(raw)))) return()
    if (!isTRUE(mapa_habilitado())) return()
    ec_onboarding_start(session, raw)
  }, ignoreInit = TRUE)
}
