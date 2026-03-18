#' @export
descarga_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Descargar datos"),
    shiny::p(shiny::tags$small("Ajusta el rango en la serie temporal para habilitar la descarga."), class = "text-muted"),
    shiny::textInput(ns("email"), "Email", placeholder = "correo@ejemplo.com"),
    shiny::uiOutput(ns("rango_info")),
    shiny::actionButton(ns("solicitar"), "Solicitar descarga"),
    shiny::uiOutput(ns("estado"))
  )
}

#' @export
descarga_server <- function(id, variable_id, estacion_id, periodo_seleccionado) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    vid <- shiny::reactive({ v <- variable_id(); if (is.null(v)) integer(0) else c(v) })
    eid <- shiny::reactive({ e <- estacion_id(); if (is.null(e)) integer(0) else c(e) })

    # Boton habilitado solo cuando hay rango (shiny 4.2 no soporta disabled en updateActionButton)
    output$rango_info <- shiny::renderUI({
      per <- periodo_seleccionado()
      if (is.null(per)) return(shiny::p(shiny::tags$span("Sin rango seleccionado.", class = "text-muted")))
      shiny::p(shiny::tags$span(paste("Rango:", format(per[1], "%Y-%m-%d"), "a", format(per[2], "%Y-%m-%d")), class = "text-info"))
    })

    shiny::observeEvent(input$solicitar, {
      v <- vid(); e <- eid(); f <- periodo_seleccionado()
      if (length(v) == 0 || length(e) == 0 || is.null(f)) {
        output$estado <- shiny::renderUI(shiny::p(shiny::tags$span("Selecciona estacion, variable y rango en el grafico.", class = "text-warning")))
        return()
      }
      body <- list(estacion_ids = as.list(e), variable_ids = as.list(v), fecha_inicio = format(f[1], "%Y-%m-%d"), fecha_fin = format(f[2], "%Y-%m-%d"), email = input$email %||% "", formato = "csv")
      url <- paste0(api_url(), "/solicitar-descarga")
      resp <- tryCatch(httr::POST(url, body = body, api_headers(), encode = "json", httr::content_type_json(), httr::timeout(10)), error = function(e) NULL)
      if (is.null(resp) || httr::status_code(resp) != 200) {
        msg <- "Error al solicitar descarga."
        if (!is.null(resp)) { cnt <- httr::content(resp, as = "parsed"); if (is.list(cnt) && !is.null(cnt$error)) msg <- cnt$error }
        output$estado <- shiny::renderUI(shiny::p(shiny::tags$span(msg, class = "text-danger")))
        return()
      }
      dat <- httr::content(resp, as = "parsed")
      estado_url <- dat$estado_url %||% paste0(api_url(), "/descarga/estado/", dat$job_id)
      if (!grepl("^https?://", estado_url)) estado_url <- paste0(api_url(), estado_url)
      poll_estado(estado_url, output, session)
    })

    poll_estado <- function(estado_url, output, session) {
      show <- function(html) output$estado <- shiny::renderUI(html)
      check <- function() {
        r <- tryCatch(httr::content(httr::GET(estado_url, api_headers(), httr::timeout(5)), as = "parsed"), error = function(e) NULL)
        if (is.null(r)) list(estado = "error") else r
      }
      done <- shiny::reactiveVal(FALSE)
      shiny::observe({
        if (shiny::isolate(done())) return()
        r <- check()
        estado <- r$estado %||% "desconocido"
        if (estado == "listo") {
          link <- r$link_descarga %||% ""
          if (nzchar(link) && !grepl("^https?://", link)) link <- paste0(api_url(), link)
          show(if (nzchar(link)) shiny::p(shiny::tags$span("Listo. ", class = "text-success"), shiny::tags$a("Descargar", href = link, target = "_blank")) else shiny::p(shiny::tags$span("Listo. Revisa email.", class = "text-success")))
          done(TRUE)
        } else if (estado %in% c("error", "fallido")) {
          show(shiny::p(shiny::tags$span("La descarga fallo.", class = "text-danger")))
          done(TRUE)
        } else {
          show(shiny::p(shiny::tags$span(paste("Procesando...", estado), class = "text-info")))
          shiny::invalidateLater(3000, session)
        }
      })
    }
  })
}



