#' @export
tabla_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    shiny::h4("Tabla de datos"),
    DT::dataTableOutput(ns("tabla"))
  )
}

#' @export
tabla_server <- function(id, api_url, estacion_id, variable_id, ver_datos_click) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    datos <- shiny::reactive({
      shiny::req(estacion_id(), variable_id(), ver_datos_click() > 0)
      base <- api_url()
      url <- paste0(base, "/datos?estacion_id=", estacion_id(), "&variable_id=", variable_id())
      resp <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
      if (is.null(resp)) return(list(df = data.frame(), error = "No se pudo conectar con la API"))
      if (httr::status_code(resp) != 200) {
        cnt <- tryCatch(httr::content(resp, as = "parsed"), error = function(e) NULL)
        msg <- if (is.list(cnt) && !is.null(cnt$error)) {
          paste(unlist(cnt$error), collapse = " ")
        } else {
          paste0("Error ", httr::status_code(resp))
        }
        return(list(df = data.frame(), error = msg))
      }
      dat <- httr::content(resp, as = "parsed", type = "application/json")
      if (is.null(dat) || length(dat) == 0) return(list(df = data.frame(), error = NULL))
      if (is.data.frame(dat)) return(list(df = dat, error = NULL))
      if (is.list(dat) && !is.null(dat$error)) return(list(df = data.frame(), error = paste(unlist(dat$error), collapse = " ")))
      df <- as.data.frame(do.call(rbind, lapply(dat, function(x) {
        data.frame(
          fecha = as.Date(x$fecha),
          valor = as.numeric(x$valor %||% NA),
          estacion_nombre = x$estacion_nombre %||% "",
          variable_nombre = x$variable_nombre %||% "",
          unidad = x$unidad %||% "",
          stringsAsFactors = FALSE
        )
      })))
      list(df = df, error = NULL)
    })

    output$tabla <- DT::renderDataTable({
      res <- datos()
      df <- res$df
      if (!is.null(res$error)) {
        return(DT::datatable(
          data.frame(mensaje = res$error),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE,
          colnames = c("")
        ))
      }
      DT::datatable(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(
            url = "//cdn.datatables.net/plug-ins/1.10.24/i18n/Spanish.json"
          )
        )
      )
    })
  })
}
