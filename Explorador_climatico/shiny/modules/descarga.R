#' @title Módulo de solicitud de descarga
#' Valida email, POST a solicitar-descarga, toast, poll estado, botón descarga
descarga_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textInput(ns("email"), "Email", placeholder = "tu@email.com", width = "100%"),
    shiny::actionButton(
      ns("solicitar_descarga"),
      "Solicitar descarga",
      class = "btn-outline-primary",
      width = "100%"
    ),
    shiny::uiOutput(ns("estado_job"))
  )
}

descarga_server <- function(id, api_url, variable_id, fechas, estacion_ids) {
  shiny::moduleServer(id, function(input, output, session) {
    job_id <- shiny::reactiveVal(NULL)
    job_estado <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$solicitar_descarga, {
      email <- trimws(input$email)
      if (email == "" || !grepl(".+@.+\\..+", email)) {
        shiny::showNotification("Ingresa un email válido.", type = "error")
        return()
      }
      vars <- variable_id()
      fch <- fechas()
      ests <- estacion_ids()
      if (is.null(vars) || length(ests) == 0) {
        shiny::showNotification("Selecciona variable y estaciones.", type = "warning")
        return()
      }
      body <- list(
        email = email,
        variable_ids = vars,
        estacion_ids = ests,
        fecha_inicio = format(fch[1], "%Y-%m-%d"),
        fecha_fin = format(fch[2], "%Y-%m-%d"),
        formato = "csv"
      )
      base <- sub("/$", "", api_url())
      url <- paste0(base, "/solicitar-descarga")
      resp <- tryCatch({
        httr::POST(url, body = body, encode = "json", httr::timeout(15))
      }, error = function(e) {
        NULL
      })
      if (is.null(resp) || httr::status_code(resp) >= 400) {
        shiny::showNotification("Error al solicitar descarga.", type = "error")
        return()
      }
      dat <- httr::content(resp, as = "parsed", type = "application/json")
      jid <- dat$job_id
      if (is.null(jid) || jid == "") {
        shiny::showNotification("No se recibió job_id.", type = "error")
        return()
      }
      job_id(jid)
      job_estado("pendiente")
      shiny::showNotification("Recibirás un email en ~5 min cuando esté listo.", type = "message")
    })

    shiny::observe({
      jid <- job_id()
      if (is.null(jid)) return()
      shiny::invalidateLater(10000)
      url <- paste0(sub("/$", "", api_url()), "/descarga/estado/", jid)
      resp <- tryCatch({
        httr::GET(url, httr::timeout(5))
      }, error = function(e) NULL)
      if (is.null(resp) || httr::status_code(resp) != 200) return()
      dat <- httr::content(resp, as = "parsed", type = "application/json")
      est <- dat$estado %||% "pendiente"
      job_estado(est)
    })

    output$estado_job <- shiny::renderUI({
      est <- job_estado()
      jid <- job_id()
      if (is.null(jid)) return(shiny::tags$p(shiny::icon("info-circle"), " Sin solicitudes activas.", class = "text-muted small mt-2"))
      if (est == "listo") {
        link <- tryCatch({
          r <- httr::GET(paste0(sub("/$", "", api_url()), "/descarga/estado/", jid))
          if (httr::status_code(r) == 200) (httr::content(r, as = "parsed"))$link_descarga else NULL
        }, error = function(e) NULL)
        if (!is.null(link) && link != "") {
          return(
            shiny::tags$div(
              class = "mt-2",
              shiny::a(href = link, target = "_blank", class = "btn btn-success btn-sm", "Descargar")
            )
          )
        }
      }
      shiny::tags$p(
        shiny::icon("spinner", class = "fa-spin"),
        " Estado: ", est, " (", jid, ")",
        class = "text-muted small mt-2"
      )
    })
  })
}
