`%||%` <- function(x, y) if (is.null(x)) y else x

#' @export
descarga_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Descargar datos"),
    shiny::dateRangeInput(ns("fechas"), "Rango fechas (max 365 dias)",
      start = Sys.Date() - 365, end = Sys.Date(),
      max = Sys.Date(), language = "es", separator = " a "),
    shiny::textInput(ns("email"), "Email", placeholder = "correo@ejemplo.com"),
    shiny::actionButton(ns("solicitar"), "Solicitar descarga"),
    shiny::uiOutput(ns("estado"))
  )
}

#' @export
descarga_server <- function(id, api_url, variable_ids, estacion_ids) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$solicitar, {
      vid <- variable_ids()
      eid <- estacion_ids()
      f <- if (!is.null(input$fechas)) c(input$fechas[1], input$fechas[2]) else NULL
      if (length(vid) == 0 || length(eid) == 0 || is.null(f)) {
        output$estado <- shiny::renderUI({
          shiny::p(shiny::tags$span("Selecciona una estacion en el mapa y una variable.", class = "text-warning"))
        })
        return()
      }

      body <- list(
        estacion_ids = as.list(eid),
        variable_ids = as.list(vid),
        fecha_inicio = format(f[1], "%Y-%m-%d"),
        fecha_fin = format(f[2], "%Y-%m-%d"),
        email = input$email %||% "",
        formato = "csv"
      )

      base <- api_url()
      url <- paste0(base, "/solicitar-descarga")
      resp <- tryCatch(
        httr::POST(url, body = body, encode = "json", httr::content_type_json(), httr::timeout(10)),
        error = function(e) NULL
      )

      if (is.null(resp) || httr::status_code(resp) != 200) {
        msg <- "Error al solicitar descarga."
        if (!is.null(resp)) {
          cnt <- httr::content(resp, as = "parsed")
          if (is.list(cnt) && !is.null(cnt$error)) msg <- cnt$error
        }
        output$estado <- shiny::renderUI({
          shiny::p(shiny::tags$span(msg, class = "text-danger"))
        })
        return()
      }

      dat <- httr::content(resp, as = "parsed")
      job_id <- dat$job_id
      estado_url <- dat$estado_url %||% paste0(base, "/descarga/estado/", job_id)

      if (!grepl("^https?://", estado_url)) {
        estado_url <- paste0(base, estado_url)
      }

      poll_estado(job_id, estado_url, base, session, ns, output)
    })

    poll_estado <- function(job_id, estado_url, base, session, ns, output) {
      check <- function() {
        resp <- tryCatch(httr::GET(estado_url, httr::timeout(5)), error = function(e) NULL)
        if (is.null(resp) || httr::status_code(resp) != 200) return(list(estado = "error"))
        httr::content(resp, as = "parsed")
      }

      resultado <- check()
      estado <- resultado$estado %||% "desconocido"

      if (estado == "listo") {
        link <- resultado$link_descarga %||% ""
        if (!grepl("^https?://", link) && nzchar(link)) {
          link <- paste0(base, link)
        }
        output$estado <- shiny::renderUI({
          if (nzchar(link)) {
            shiny::p(
              shiny::tags$span("Listo. ", class = "text-success"),
              shiny::tags$a("Descargar archivo", href = link, target = "_blank")
            )
          } else {
            shiny::p(shiny::tags$span("Listo. Revisa tu email.", class = "text-success"))
          }
        })
        return()
      }

      if (estado %in% c("error", "fallido")) {
        output$estado <- shiny::renderUI({
          shiny::p(shiny::tags$span("La descarga fallÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â³.", class = "text-danger"))
        })
        return()
      }

      output$estado <- shiny::renderUI({
        shiny::p(shiny::tags$span(paste("Procesando...", estado), class = "text-info"))
      })

      done <- shiny::reactiveVal(FALSE)
      shiny::observe({
        if (shiny::isolate(done())) return()
        resultado <- check()
        estado <- resultado$estado %||% "desconocido"

        if (estado == "listo") {
          link <- resultado$link_descarga %||% ""
          if (!grepl("^https?://", link) && nzchar(link)) {
            link <- paste0(base, link)
          }
          output$estado <- shiny::renderUI({
            if (nzchar(link)) {
              shiny::p(
                shiny::tags$span("Listo. ", class = "text-success"),
                shiny::tags$a("Descargar archivo", href = link, target = "_blank")
              )
            } else {
              shiny::p(shiny::tags$span("Listo. Revisa tu email.", class = "text-success"))
            }
          })
          done(TRUE)
          return()
        }

        if (estado %in% c("error", "fallido")) {
          output$estado <- shiny::renderUI({
            shiny::p(shiny::tags$span("La descarga fallÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â³.", class = "text-danger"))
          })
          done(TRUE)
          return()
        }

        output$estado <- shiny::renderUI({
          shiny::p(shiny::tags$span(paste("Procesando...", estado), class = "text-info"))
        })
        shiny::invalidateLater(3000, session)
      })
    }
  })
}
