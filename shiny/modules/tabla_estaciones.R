#' Tabla de analisis por estaciones (cuando hay multiples estaciones)
#' @export
tabla_estaciones_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "ec-tabla-estaciones-actions",
      shiny::actionButton(ns("sel_todas"), "Seleccionar todas", class = "btn-sm btn-outline-secondary"),
      shiny::actionButton(ns("sel_ninguna"), "Quitar seleccion", class = "btn-sm btn-outline-secondary")
    ),
    shiny::div(class = "ec-tabla-estaciones-wrap",
      DT::dataTableOutput(ns("tabla_estaciones"))
    ),
    shiny::div(style = "display:none;", shiny::numericInput(ns("ver_serie_id"), NULL, value = 0)),
    shiny::tags$script(shiny::HTML(sprintf(
      "$(document).on('click', '.ec-btn-ver-serie-row', function(){ var id = $(this).data('estacion-id'); if (id) Shiny.setInputValue('%s', id, {priority: 'event'}); });",
      ns("ver_serie_id")
    )))
  )
}

#' @param estaciones Reactive con data.frame de estaciones (id, nombre, lat, lon, ...)
#' @param estaciones_full Reactive con estaciones sin minimal (id_fuente) - se usa si existe
#' @param fuentes Reactive con id, nombre de fuentes/operadores
#' @param variable_id Reactive variable seleccionada (para % completitud)
#' @param ver_serie_callback Funcion(estacion_id) a llamar cuando se hace clic en Ver serie
#' @param panel_modo Reactive "serie" o "tabla" - solo calcula datos cuando es "tabla"
#' @return Reactive con IDs de estaciones seleccionadas (para descarga)
#' @export
tabla_estaciones_server <- function(id, estaciones, fuentes, variable_id,
                                    ver_serie_callback, estaciones_full = NULL, panel_modo = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    estaciones_seleccionadas <- shiny::reactiveVal(integer())

    MAX_ESTACIONES_TABLA <- 100L

    # Datos enriquecidos para la tabla (max 100 para evitar 200+ llamadas API)
    tabla_data <- shiny::reactive({
      if (!is.null(panel_modo) && panel_modo() != "tabla") return(NULL)
      e <- estaciones()
      f <- fuentes()
      vid <- variable_id()
      if (is.null(e) || nrow(e) == 0) return(NULL)
      n <- nrow(e)
      if (n > MAX_ESTACIONES_TABLA) {
        return(list(too_many = TRUE, n = n, msg = paste0(
          "Hay ", n, " estaciones visibles. Aplica filtros (operador y/o variable) para reducir ",
          "a ", MAX_ESTACIONES_TABLA, " o menos y vuelve a hacer clic en Ver datos."
        )))
      }
      e_full <- if (!is.null(estaciones_full)) estaciones_full() else NULL

      shiny::withProgress(message = "Cargando datos por estacion...", value = 0, {
        hoy <- Sys.Date()
        f_fin <- format(hoy, "%Y-%m-%d")
        f_ini <- format(hoy - 730, "%Y-%m-%d")
        rows <- vector("list", n)
        for (i in seq_len(n)) {
          shiny::setProgress(value = i / (n + 1), detail = paste(i, "de", n))
          eid <- as.integer(e$id[i])
          nom <- e$nombre[i] %||% ""
          codigo <- as.character(eid)

          operador <- ""
          if (!is.null(e_full) && "id_fuente" %in% names(e_full)) {
            m <- match(eid, e_full$id)
            if (!is.na(m) && !is.na(e_full$id_fuente[m]) && nrow(f) > 0) {
              fid <- e_full$id_fuente[m]
              op_row <- f[f$id == fid, , drop = FALSE]
              if (nrow(op_row) > 0) operador <- op_row$nombre[1] %||% ""
            }
          }

          vars_count <- NA_integer_
          comp <- NA_real_
          var_dat <- tryCatch(
            api_get(paste0("/estaciones/", eid, "/variables"), timeout = 8),
            error = function(e) NULL
          )
          if (!is.null(var_dat)) {
            if (is.data.frame(var_dat)) vars_count <- nrow(var_dat)
            else if (is.list(var_dat) && length(var_dat) > 0) vars_count <- length(var_dat)
          }

          if (!is.null(vid) && !is.na(vid) && vid != "" && vid != "0") {
            res <- tryCatch(
              api_get("/datos/resumen", list(estacion_id = eid, variable_id = as.integer(vid)), timeout = 6),
              error = function(e) NULL
            )
            if (!is.null(res) && is.list(res)) {
              cnt <- as.numeric(res$conteo %||% 0)
              dmn <- ec_coerce_una_fecha(res$fecha_min)
              dmx <- ec_coerce_una_fecha(res$fecha_max)
              if (!is.na(dmn) && !is.na(dmx)) {
                dias <- as.numeric(difftime(dmx, dmn, units = "days")) + 1
                if (!is.na(dias) && dias > 0) comp <- round(100 * cnt / dias, 1)
              }
            }
          }

          rows[[i]] <- list(
            id = eid,
            nombre = nom,
            codigo = codigo,
            operador = operador,
            n_variables = if (is.na(vars_count)) "-" else as.character(vars_count),
            completitud = if (is.na(comp)) "-" else paste0(comp, "%"),
            ver_serie_btn = sprintf('<button type="button" class="btn btn-sm ec-btn-ver-serie-row" data-estacion-id="%d">Ver serie</button>', eid)
          )
        }
      })

      df <- data.frame(
        nombre = vapply(rows, function(r) r$nombre, ""),
        codigo = vapply(rows, function(r) r$codigo, ""),
        operador = vapply(rows, function(r) r$operador, ""),
        variables = vapply(rows, function(r) r$n_variables, ""),
        completitud = vapply(rows, function(r) r$completitud, ""),
        ver_serie = vapply(rows, function(r) r$ver_serie_btn, ""),
        id = vapply(rows, function(r) r$id, 0L),
        stringsAsFactors = FALSE
      )
      df
    })

    output$tabla_estaciones <- DT::renderDataTable({
      td <- tabla_data()
      if (is.null(td)) {
        return(DT::datatable(
          data.frame(msg = "No hay estaciones que mostrar."),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE
        ))
      }
      if (is.list(td) && isTRUE(td$too_many)) {
        return(DT::datatable(
          data.frame(msg = td$msg %||% paste("Hay demasiadas estaciones. Aplica filtros.")),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE
        ))
      }
      if (nrow(td) == 0) {
        return(DT::datatable(
          data.frame(msg = "No hay estaciones que mostrar."),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE
        ))
      }
      df_display <- td[, c("nombre", "codigo", "operador", "variables", "completitud", "ver_serie")]
      names(df_display) <- c("Estacion", "Codigo", "Operador", "Variables", "% Completitud", "")
      DT::datatable(
        df_display,
        escape = c(rep(TRUE, 5), FALSE),
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          ordering = TRUE,
          orderSequence = c("desc", "asc"),
          language = list(url = "//cdn.datatables.net/plug-ins/1.10.24/i18n/Spanish.json"),
          columnDefs = list(
            list(orderable = TRUE, targets = c(0, 1, 2, 3, 4)),
            list(orderable = FALSE, targets = 5)
          )
        ),
        selection = list(mode = "multiple", target = "row", selected = NULL),
        rownames = FALSE
      )
    })

    # Sincronizar seleccion de filas con estaciones_seleccionadas
    shiny::observe({
      sel <- input$tabla_estaciones_rows_selected
      td <- tabla_data()
      if (is.null(td) || nrow(td) == 0 || length(sel) == 0) {
        estaciones_seleccionadas(integer())
        return()
      }
      ids <- td$id[sel]
      estaciones_seleccionadas(ids)
    })

    shiny::observeEvent(input$sel_todas, {
      DT::dataTableProxy(ns("tabla_estaciones")) |>
        DT::selectRows(seq_len(nrow(tabla_data())))
    })

    shiny::observeEvent(input$sel_ninguna, {
      DT::dataTableProxy(ns("tabla_estaciones")) |>
        DT::selectRows(NULL)
    })

    shiny::observeEvent(input$ver_serie_id, {
      eid <- as.integer(input$ver_serie_id)
      if (!is.na(eid) && eid > 0) ver_serie_callback(eid)
    }, ignoreInit = TRUE)

    list(estaciones_seleccionadas = estaciones_seleccionadas)
  })
}
