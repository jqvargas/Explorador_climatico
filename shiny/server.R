server <- function(input, output, session) {
  session$allowReconnect(TRUE)

  # region agent log
  # Instrumentación temporal (debug 5e3e0f) — velo gris / mapa ausente. Quitar al cerrar el caso.
  ec_dbg_log <- function(hyp, loc, msg, data = list()) {
    tryCatch({
      body <- jsonlite::toJSON(list(
        sessionId = "5e3e0f", runId = "run1", hypothesisId = hyp,
        location = loc, message = msg, data = data,
        timestamp = as.numeric(Sys.time()) * 1000
      ), auto_unbox = TRUE, null = "null")
      httr::POST(
        "http://host.docker.internal:7911/ingest/2d6a8a92-c9c8-483b-9bd9-66545d475d64",
        httr::add_headers(`X-Debug-Session-Id` = "5e3e0f"),
        body = body, httr::content_type_json(), httr::timeout(2)
      )
      invisible(NULL)
    }, error = function(e) invisible(NULL))
  }
  ec_dbg_started <- Sys.time()
  ec_dbg_ctx <- function() {
    list(
      token = substr(session$token %||% "", 1, 12),
      replica = Sys.info()[["nodename"]],
      uptime_sec = round(as.numeric(difftime(Sys.time(), ec_dbg_started, units = "secs")), 1)
    )
  }
  # endregion

  # Idle close + keepalive suave: pestañas olvidadas liberan el proceso R.
  SESSION_IDLE_SEC <- as.numeric(Sys.getenv("SHINY_SESSION_IDLE_SEC", "1200"))
  KEEPALIVE_MS <- as.integer(Sys.getenv("SHINY_KEEPALIVE_MS", "90000"))
  session$userData$last_active <- Sys.time()
  shiny::observeEvent(input$ec_user_active, {
    session$userData$last_active <- Sys.time()
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  keep_alive <- shiny::reactiveTimer(KEEPALIVE_MS)
  shiny::observe({
    keep_alive()
    last <- session$userData$last_active %||% Sys.time()
    idle <- as.numeric(difftime(Sys.time(), last, units = "secs"))
    if (is.finite(idle) && idle >= SESSION_IDLE_SEC) {
      # region agent log
      ec_dbg_log("C", "server.R:idle_close", "session$close() por inactividad",
                 c(ec_dbg_ctx(), list(idle_sec = round(idle, 1), limite_sec = SESSION_IDLE_SEC)))
      # endregion
      session$close()
    }
  }, priority = -999)

  # region agent log
  session$onSessionEnded(function() {
    last <- session$userData$last_active %||% Sys.time()
    ec_dbg_log("B,C", "server.R:session_ended", "sesión Shiny terminada",
               c(ec_dbg_ctx(), list(
                 idle_sec_al_cierre = round(as.numeric(difftime(Sys.time(), last, units = "secs")), 1)
               )))
  })
  # endregion

  # App profiler: correlacionar sesión Shiny ↔ API ↔ browser
  if (isTRUE(tryCatch(shiny_app_profile_active(), error = function(e) FALSE))) {
    tid <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), sprintf("%04d", sample.int(9999, 1)))
    if (requireNamespace("uuid", quietly = TRUE)) {
      tid <- gsub("-", "", uuid::UUIDgenerate(), fixed = TRUE)
    }
    t0 <- as.numeric(Sys.time()) * 1000
    session$userData$app_trace_id <- tid
    session$userData$app_t0_ms <- t0
    session$sendCustomMessage("app_profile_init", list(trace_id = tid, t0_ms = t0))
    shiny_app_profile_beacon(list(list(
      name = "session.start",
      cat = "session",
      ts = 0,
      dur_ms = 0,
      attrs = list(lane = "shiny", token = substr(session$token %||% "", 1, 12))
    )), tid, t0)
    session$onSessionEnded(function() {
      shiny_app_profile_beacon(list(list(
        name = "session.end",
        cat = "session",
        dur_ms = 0,
        attrs = list(lane = "shiny")
      )), tid, t0)
    })
  }

  operador_aplicado <- shiny::reactiveVal("0")
  variable_aplicada <- shiny::reactiveVal(ec_initial_variable_id(
    if (!is.null(session$request)) session$request$HTTP_COOKIE else NULL,
    if (!is.null(session$request)) session$request$QUERY_STRING else NULL
  ))
  region_aplicada <- shiny::reactiveVal("")
  comuna_aplicada <- shiny::reactiveVal("")
  estacion_id <- shiny::reactiveVal(NULL)
  geo_usuario <- shiny::reactiveVal(NULL)
  geo_request_pending <- shiny::reactiveVal(FALSE)
  geo_clear_tick <- shiny::reactiveVal(0L)
  ver_datos_click <- shiny::reactiveVal(0)
  # Solo abre/invalida el modal reporte — no dispare GET /datos del panel serie.
  reporte_open_tick <- shiny::reactiveVal(0)
  periodo_seleccionado <- shiny::reactiveVal(NULL)
  mapa_habilitado <- shiny::reactiveVal(TRUE)
  deeplink_reporte_mode <- shiny::reactiveVal(FALSE)

  variable_id <- shiny::reactive({  v <- input$variable
    if (is.null(v) || v == "" || v == "0") return(NULL)
    as.integer(v)
  })

  estacion_id_ver <- shiny::reactive({  e <- input$estacion
    if (is.null(e) || e == "" || e == "0") return(NULL)
    as.integer(e)
  })

  metadata <- shiny::reactive({
    # Boot: preferir payload del browser (evita httr en el event loop).
    client <- input$ec_metadata_json
    if (!is.null(client) && is.character(client) && nzchar(client[1])) {
      parsed <- tryCatch(jsonlite::fromJSON(client[1], simplifyVector = TRUE), error = function(e) NULL)
      if (is.list(parsed) && (!is.null(parsed$variables) || !is.null(parsed$fuentes))) {
        return(parsed)
      }
    }
    dat <- api_get("/metadata", list(chile_only = "1"), timeout = API_TIMEOUT_DEFAULT, reintentos = 0L)
    if (is.null(dat) || !is.list(dat)) {
      dat <- list(
        variables = api_get("/variables", timeout = API_TIMEOUT_DEFAULT, reintentos = 0L),
        fuentes = api_get("/fuentes", timeout = API_TIMEOUT_DEFAULT, reintentos = 0L),
        regiones = api_get("/regiones", list(chile_only = "1"), timeout = API_TIMEOUT_DEFAULT, reintentos = 0L)
      )
    }
    if (is.null(dat) || !is.list(dat)) dat <- list(variables = list(), fuentes = list(), regiones = list())
    dat
  })

  variables <- shiny::reactive({
    m <- metadata()
    v <- m$variables
    if (is.null(v) || length(v) == 0) return(data.frame(id = integer(), nombre = character()))
    if (is.data.frame(v)) {
      df <- v[, c("id", "nombre"), drop = FALSE]
    } else {
      rows <- lapply(v, function(x) data.frame(id = as.integer(x$id %||% NA), nombre = as.character(x$nombre %||% x$nombre_abr %||% ""), stringsAsFactors = FALSE))
      if (length(rows) == 0) return(data.frame(id = integer(), nombre = character()))
      df <- do.call(rbind, rows)
    }
    df$id <- as.integer(df$id)
    ec_label_variables_df(df[, c("id", "nombre"), drop = FALSE])
  })

  fuentes <- shiny::reactive({
    m <- metadata()
    f <- m$fuentes
    if (is.null(f) || length(f) == 0) return(data.frame(id = integer(), nombre = character()))
    if (is.data.frame(f)) return(f[, c("id", "nombre"), drop = FALSE])
    rows <- lapply(f, function(x) data.frame(id = as.integer(x$id %||% NA), nombre = as.character(x$nombre %||% ""), stringsAsFactors = FALSE))
    if (length(rows) == 0) return(data.frame(id = integer(), nombre = character()))
    do.call(rbind, rows)
  })

  regiones <- shiny::reactive({
    m <- metadata()
    r <- m$regiones
    if (is.null(r) || length(r) == 0) return(data.frame(id = integer(), nombre = character()))
    if (is.data.frame(r)) return(r[, c("id", "nombre"), drop = FALSE])
    rows <- lapply(r, function(x) data.frame(id = as.integer(x$id %||% NA), nombre = as.character(x$nombre %||% ""), stringsAsFactors = FALSE))
    if (length(rows) == 0) return(data.frame(id = integer(), nombre = character()))
    do.call(rbind, rows)
  })

  estaciones_boot_done <- shiny::reactiveVal(FALSE)
  # TRUE cuando Leaflet ya envió mapa_zoom (shell visible). Boot API espera esto.
  mapa_listo <- shiny::reactiveVal(FALSE)
  # Generación cliente: Date.now() de ec_map_ready (montaje real del widget).
  map_client_gen <- shiny::reactiveVal(0)
  # TRUE tras primer paint de capa ec_alertas (fin de T_load).
  alertas_pintadas <- shiny::reactiveVal(FALSE)
  # TRUE un tick después de alertas — metadata/geo/filtros no alargan el busy de T_load.
  post_alertas_idle <- shiny::reactiveVal(FALSE)
  post_alertas_tick <- shiny::reactiveVal(0L)
  catalogo_listo <- shiny::reactiveVal(FALSE)
  geo_conteos_listo <- shiny::reactiveVal(FALSE)
  estaciones_singleton <- shiny::reactiveVal(NULL)
  drill_suppress_drawer <- shiny::reactiveVal(FALSE)
  # Suppress region/comuna side-effects briefly after favorito restore (client echo).
  favorito_suppress_until <- shiny::reactiveVal(0)

  geo_conteos <- shiny::reactive({
    shiny::req(mapa_habilitado())
    geo_conteos_listo(FALSE)
    on.exit(geo_conteos_listo(TRUE), add = TRUE)
    params <- list(con_datos = "1")
    op <- operador_aplicado()
    if (!is.null(op) && op != "" && op != "0") params$id_fuente <- as.integer(op)
    var <- variable_aplicada()
    if (!is.null(var) && var != "" && var != "0") params$id_variable <- as.integer(var)
    # Boot: payload browser si coincide con la variable prefetched.
    client <- input$ec_geo_conteos_json
    if (ec_boot_use_client_payload(params$id_fuente, params$id_variable, client, input$ec_boot_variable)) {
      parsed <- tryCatch(jsonlite::fromJSON(client[1], simplifyVector = TRUE), error = function(e) NULL)
      if (!is.null(parsed)) return(ec_geo_conteos_as_list(parsed))
    }
    dat <- api_get("/geo/conteos", params, timeout = API_TIMEOUT_DEFAULT, reintentos = 0L)
    ec_geo_conteos_as_list(dat)
  })

  alertas_include_ext <- shiny::reactiveVal(FALSE)
  alertas_mapa_raw <- shiny::reactive({
    shiny::req(mapa_habilitado())
    params <- list()
    op <- operador_aplicado()
    if (!is.null(op) && op != "" && op != "0") params$id_fuente <- as.integer(op)
    var <- variable_aplicada()
    if (!is.null(var) && var != "" && var != "0") params$id_variable <- as.integer(var)
    if (isTRUE(alertas_include_ext())) params$include_ext <- "1"
    client <- input$ec_alertas_mapa_json
    if (!isTRUE(alertas_include_ext()) &&
        ec_boot_use_client_payload(params$id_fuente, params$id_variable, client, input$ec_boot_variable)) {
      parsed <- tryCatch(jsonlite::fromJSON(client[1], simplifyVector = TRUE), error = function(e) NULL)
      if (!is.null(parsed)) return(parsed)
    }
    # reintentos=0: un blip no suma Sys.sleep(1.5) dentro del busy de T_load
    api_get("/alertas/mapa", params, timeout = API_TIMEOUT_ESTACIONES, reintentos = 0L)
  })

  anticipada_mapa_raw <- shiny::reactive({
    shiny::req(mapa_habilitado())
    tryCatch(
      api_get("/alertas/anticipada-mapa", list(), timeout = API_TIMEOUT_DEFAULT, reintentos = 0L),
      error = function(e) list(alertas = list())
    )
  })

  # Catalogo solo cuando hay comuna (o se fusiona con singleton favorito/alerta).
  catalogo_estaciones <- shiny::reactive({
    shiny::req(mapa_habilitado())
    com <- comuna_aplicada()
    if (!nzchar(com %||% "")) {
      catalogo_listo(TRUE)
      return(ec_catalogo_empty_df())
    }
    catalogo_listo(FALSE)
    on.exit(catalogo_listo(TRUE), add = TRUE)
    params <- list(chile_only = "1", catalog = "1", con_datos = "1", comuna = trimws(as.character(com)))
    op <- operador_aplicado()
    if (!is.null(op) && op != "" && op != "0") params$id_fuente <- as.integer(op)
    var <- variable_aplicada()
    if (!is.null(var) && var != "" && var != "0") params$id_variable <- as.integer(var)
    reg <- region_aplicada()
    if (!is.null(reg) && nzchar(reg) && !is.na(suppressWarnings(as.integer(reg)))) {
      params$region <- as.integer(reg)
    }
    dat <- shiny::withProgress(message = "Cargando estaciones...", value = 0.3, {
      api_get("/estaciones", params, timeout = API_TIMEOUT_ESTACIONES)
    })
    if (is.null(dat)) return(ec_catalogo_empty_df())
    ec_catalogo_to_df(dat)
  })

  estaciones <- shiny::reactive({
    shiny::req(mapa_habilitado())
    cat <- catalogo_estaciones()
    out <- if (is.null(cat) || nrow(cat) == 0L) {
      ec_catalogo_empty_df()
    } else {
      out0 <- cat[!is.na(cat$lat) & !is.na(cat$lon), , drop = FALSE]
      ec_finalizar_estaciones_mapa(out0, isTRUE(input$ocultar_calidad_limitada))
    }
    sing <- estaciones_singleton()
    if (!is.null(sing) && nrow(sing) > 0L) {
      if (nrow(out) == 0L || !sing$id[1] %in% out$id) {
        out <- rbind(out, sing)
      }
    }
    out
  })

  # Choropleth / selects un flush después de alertas — no pelean el hilo con T_load.
  shiny::observe({
    shiny::req(post_alertas_idle())
    gc <- geo_conteos()
    session$sendCustomMessage("ec_choropleth_conteos", gc)
  }, priority = -10)

  ec_ensure_estacion_singleton <- function(eid) {
    eid <- suppressWarnings(as.integer(eid))
    if (is.na(eid) || eid < 1L) return(FALSE)
    cur <- estaciones_singleton()
    if (!is.null(cur) && nrow(cur) > 0L && cur$id[1] == eid) return(TRUE)
    # Reusar fila ya en catálogo/mapa — evita GET /estaciones/id duplicado.
    cat <- tryCatch(shiny::isolate(catalogo_estaciones()), error = function(e) NULL)
    if (!is.null(cat) && nrow(cat) > 0L && eid %in% cat$id) {
      estaciones_singleton(cat[cat$id == eid, , drop = FALSE][1, , drop = FALSE])
      return(TRUE)
    }
    info <- tryCatch(api_get(paste0("/estaciones/", eid), timeout = 10), error = function(e) NULL)
    row <- ec_estacion_detalle_to_row(info)
    if (is.null(row)) return(FALSE)
    estaciones_singleton(row)
    TRUE
  }

  datos <- shiny::eventReactive(ver_datos_click(), {
    eid <- estacion_id_ver()
    vid <- variable_id()
    if (is.null(eid) || is.null(vid)) return(list(df = data.frame(), error = "Selecciona estacion y variable primero."))
    hoy <- Sys.Date()
    f_fin <- format(hoy, "%Y-%m-%d")
    f_ini <- format(hoy - 365, "%Y-%m-%d")
    params <- list(estacion_id = eid, variable_id = vid, fecha_inicio = f_ini, fecha_fin = f_fin)
    dat <- shiny::withProgress(message = "Cargando serie temporal...", value = 0.3, {
      api_get("/datos", params, timeout = API_TIMEOUT_DATOS)
    })
    if (is.null(dat)) return(list(df = data.frame(), error = "Error de conexion o timeout. La API puede estar ocupada. Intenta de nuevo en unos segundos."))
    if (is.list(dat) && !is.null(dat$error)) return(list(df = data.frame(), error = paste(dat$error, collapse = " ")))

    # Nueva forma de /datos: { data: [{fecha, valor}], meta: {estacion_nombre, variable_nombre, unidad} }
    meta <- if (is.list(dat)) dat$meta else NULL
    rows <- if (is.list(dat) && !is.null(dat$data)) dat$data else dat
    est_nom <- as.character(meta$estacion_nombre %||% "")
    var_nom_db <- as.character(meta$variable_nombre %||% "")
    var_nom <- ec_variable_label(var_nom_db)
    uni     <- as.character(meta$unidad %||% "")
    if (is.null(rows) || length(rows) == 0) return(list(df = data.frame(), error = NULL))
    if (is.data.frame(rows)) {
      df <- ec_serie_normalizar(rows)
      df$fecha <- as.Date(df$fecha)
      if (!"qc_flag" %in% names(df)) df$qc_flag <- 0L
      else df$qc_flag <- as.integer(df$qc_flag)
    } else {
      df <- do.call(rbind, lapply(rows, function(x) {
        v <- suppressWarnings(as.numeric(x$valor))
        if (length(v) == 0L || is.na(v)) v <- NA_real_
        data.frame(
          fecha = as.Date(x$fecha), valor = v,
          qc_flag = as.integer(x$qc_flag %||% 0), stringsAsFactors = FALSE
        )
      }))
      df <- ec_serie_normalizar(df)
    }
    df$estacion_nombre <- est_nom
    df$variable_nombre <- var_nom
    df$variable_nombre_db <- var_nom_db
    df$unidad <- uni
    list(df = df, error = NULL)
  }, ignoreNULL = FALSE)

  filtros_syncing <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(input$operador, {
    if (isTRUE(filtros_syncing())) return()
    operador_aplicado(input$operador %||% "0")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$variable, {
    if (isTRUE(filtros_syncing())) return()
    v <- input$variable
    variable_aplicada(if (is.null(v) || v == "" || v == "0") "" else v)
  }, ignoreInit = TRUE)

  # Navegacion geografica: solo mapa + breadcrumb (region_aplicada / comuna_aplicada).
  shiny::observeEvent(input$ec_choropleth_drill, {
    d <- input$ec_choropleth_drill
    if (is.null(d) || !is.list(d)) return()
    drill_suppress_drawer(TRUE)
    on.exit({
      session$onFlushed(function() drill_suppress_drawer(FALSE), once = TRUE)
    }, add = TRUE)
    nivel <- as.character(d$nivel %||% "")
    mz <- as.character(d$mz_id %||% "")
    cod <- suppressWarnings(as.integer(d$cod_reg %||% NA))
    nom_com <- trimws(as.character(d$nom_com %||% ""))
    if (identical(nivel, "macrozona") || identical(nivel, "region") || !nzchar(nom_com)) {
      if (!is.na(cod) && cod > 0L) {
        region_aplicada(as.character(cod))
      } else if (identical(nivel, "macrozona") || !nzchar(mz)) {
        region_aplicada("")
      }
      comuna_aplicada("")
      estacion_id(NULL)
      shiny::updateSelectInput(session, "estacion", selected = "")
      if (!identical(nivel, "estaciones")) estaciones_singleton(NULL)
      return()
    }
    if (!is.na(cod) && cod > 0L) {
      region_aplicada(as.character(cod))
    }
    comuna_aplicada(nom_com)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$estacion, {
    if (isTRUE(filtros_syncing())) return()
    e <- input$estacion
    if (!is.null(e) && e != "" && e != "0") map_sel_con_zoom(TRUE)
    estacion_id(if (is.null(e) || e == "" || e == "0") NULL else as.integer(e))
  }, ignoreInit = TRUE)

  shiny::observe({
    show <- nzchar(comuna_aplicada() %||% "")
    session$sendCustomMessage("ec_estacion_filter_visible", list(show = show))
  })

  shiny::observeEvent(input$limpiar_todo, {
    filtros_syncing(TRUE)
    operador_aplicado("0")
    variable_aplicada(as.character(EC_DEFAULT_VARIABLE_ID))
    region_aplicada("")
    comuna_aplicada("")
    estacion_id(NULL)
    estaciones_singleton(NULL)
    geo_usuario(NULL)
    geo_request_pending(FALSE)
    geo_clear_tick(geo_clear_tick() + 1L)
    shiny::updateSelectInput(session, "operador", selected = "0")
    shiny::updateSelectInput(session, "variable", selected = as.character(EC_DEFAULT_VARIABLE_ID))
    shiny::updateSelectInput(session, "estacion", selected = "")
    session$sendCustomMessage("ec_choropleth_reset", TRUE)
    session$sendCustomMessage("ec_user_geo_clear", TRUE)
    session$onFlushed(function() {
      filtros_syncing(FALSE)
    }, once = TRUE)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$map_geo_pending, {
    geo_request_pending(TRUE)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$map_geo_payload, {
    raw <- input$map_geo_payload
    if (!is.character(raw) || !nzchar(raw)) return()
    parsed <- tryCatch(jsonlite::fromJSON(raw), error = function(e) NULL)
    if (is.null(parsed)) return()
    lat <- suppressWarnings(as.numeric(parsed[["lat"]]))
    lon <- suppressWarnings(as.numeric(parsed[["lon"]]))
    if (length(lat) != 1L || length(lon) != 1L || is.na(lat) || is.na(lon)) return()
    geo_request_pending(FALSE)
    geo_usuario(list(lat = lat, lon = lon))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$map_geo_pick_eid, {
    eid <- suppressWarnings(as.integer(input$map_geo_pick_eid))
    if (is.na(eid) || eid < 1L) return()
    df <- estaciones()
    aplicar_pick <- function() {
      map_sel_con_zoom(TRUE)
      estacion_id(eid)
      shiny::updateSelectInput(session, "estacion", selected = as.character(eid))
    }
    if (is.null(df) || nrow(df) == 0L || !eid %in% df$id) {
      filtros_syncing(TRUE)
      region_aplicada("")
      comuna_aplicada("")
      session$onFlushed(function() {
        filtros_syncing(FALSE)
        aplicar_pick()
      }, once = TRUE)
    } else {
      aplicar_pick()
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$descargar_datos, {
    e <- estaciones()
    if (is.null(e) || nrow(e) == 0) {
      session$sendCustomMessage("ec_fav_toast", list(
        text = "No hay estaciones visibles para descargar.",
        ok = FALSE
      ))
      return()
    }
    session$sendCustomMessage("ec_open_download_filtered", list(
      ids = as.list(as.integer(e$id)),
      nombre = "Explorador filtrado"
    ))
  }, ignoreInit = TRUE)

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
    shiny::req(mapa_habilitado())
    if (panel_modo() != "tabla") return(NULL)
    params <- list(chile_only = "1", minimal = "", con_datos = "1")
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

  reportes_abiertos <- shiny::reactiveVal(list())
  url_deeplink_done <- shiny::reactiveVal(FALSE)
  url_deeplink_pending_eid <- shiny::reactiveVal(NULL)
  mi_estacion_pending <- shiny::reactiveVal(NULL)
  favorito_inicial_hecho <- shiny::reactiveVal(FALSE)
  favorito_sync_aplicado <- shiny::reactiveVal(FALSE)
  estacion_selectize_loaded <- shiny::reactiveVal(FALSE)
  map_ready_for_deeplink <- shiny::reactiveVal(FALSE)
  favorito_restore_eid <- shiny::reactiveVal(NULL)
  favorito_servidor <- shiny::reactiveVal(NULL)
  map_focus_tick <- shiny::reactiveVal(0)
  map_sel_con_zoom <- shiny::reactiveVal(FALSE)

  mi_estacion_map_pending <- shiny::reactiveVal(NULL)
  active_report_eid <- shiny::reactiveVal(NULL)
  modal_reporte_abierto <- shiny::reactiveVal(FALSE)

  ec_reporte_nombre_tab <- function(eid, df = NULL, nombre_override = NULL) {
    nom <- if (!is.null(nombre_override) && nzchar(nombre_override)) {
      as.character(nombre_override)
    } else if (!is.null(df) && nrow(df) > 0 && eid %in% df$id) {
      as.character(df$nombre[df$id == eid][1])
    } else {
      NA_character_
    }
    if (is.na(nom) || !nzchar(nom)) return(paste0("Estaci\u00f3n ", eid))
    if (nchar(nom) > 32) paste0(substr(nom, 1, 29), "...") else nom
  }

  ec_nombre_desde_estacion_info <- function(info, eid) {
    if (is.null(info)) return(NULL)
    nom <- as.character(info$nombre %||% "")
    if (!nzchar(nom) || is.na(nom)) return(NULL)
    nom
  }

  # "Volver al mapa": oculta el modal pero CONSERVA el historial de pestanas.
  volver_al_mapa <- function() {
    session$sendCustomMessage("ec_report_destroy", TRUE)
    session$sendCustomMessage("ec_url_clear_report", TRUE)
    shiny::removeModal()
    modal_reporte_abierto(FALSE)
    if (!isTRUE(mapa_habilitado())) {
      mapa_habilitado(TRUE)
      mapa_listo(FALSE)
      map_client_gen(0)
      alertas_pintadas(FALSE)
      post_alertas_idle(FALSE)
      post_alertas_tick(0L)
      estaciones_boot_done(FALSE)
      session$sendCustomMessage("ec_report_first", FALSE)
      eid <- active_report_eid()
      if (!is.null(eid) && !is.na(eid)) {
        map_focus_tick(map_focus_tick() + 1L)
      }
    }
  }

  ec_sync_report_url <- function() {
    eid <- active_report_eid()
    if (is.null(eid) || is.na(eid) || !modal_reporte_abierto()) return()
    vid <- variable_aplicada()
    op <- operador_aplicado()
    session$sendCustomMessage("ec_url_sync", list(
      eid = as.integer(eid),
      variableId = if (!is.null(vid) && nzchar(vid) && vid != "0") as.integer(vid) else NULL,
      fuenteId = if (!is.null(op) && nzchar(op) && op != "0") as.integer(op) else NULL,
      reporte = TRUE
    ))
  }

  ec_aplicar_favorito_en_ui <- function(fav, seleccionar = TRUE) {
    if (is.null(fav) || is.null(fav$eid)) return(FALSE)
    eid <- as.integer(fav$eid)
    if (is.na(eid) || eid < 1L) return(FALSE)
    if (!is.null(fav$fuenteId) && !is.na(as.integer(fav$fuenteId))) {
      operador_aplicado(as.character(as.integer(fav$fuenteId)))
      shiny::updateSelectInput(session, "operador", selected = as.character(fav$fuenteId))
    }
    if (!is.null(fav$variableId) && !is.na(as.integer(fav$variableId))) {
      variable_aplicada(as.character(as.integer(fav$variableId)))
      shiny::updateSelectInput(session, "variable", selected = as.character(fav$variableId))
    }
    # Boot restore: mark on map only — selecting triggers popup + /variables + alertas.
    if (isTRUE(seleccionar)) {
      estacion_id(eid)
      shiny::updateSelectInput(session, "estacion", selected = as.character(eid))
    }
    TRUE
  }

  ec_parse_favorito_json <- function(json_txt) {
    if (is.null(json_txt) || !nzchar(trimws(json_txt))) return(NULL)
    if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
    o <- tryCatch(jsonlite::fromJSON(json_txt, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(o) || is.null(o$primary)) return(NULL)
    p <- o$primary
    lat <- suppressWarnings(as.numeric(p$lat %||% NA_real_))
    lon <- suppressWarnings(as.numeric(p$lon %||% NA_real_))
    list(
      eid = as.integer(p$eid %||% NA_integer_),
      variableId = suppressWarnings(as.integer(p$variableId %||% NA_integer_)),
      fuenteId = suppressWarnings(as.integer(p$fuenteId %||% NA_integer_)),
      alias = as.character(p$alias %||% ""),
      lat = if (length(lat) == 1L && is.finite(lat)) lat else NULL,
      lon = if (length(lon) == 1L && is.finite(lon)) lon else NULL
    )
  }

  ec_leer_favorito <- function() {
    fav <- favorito_servidor()
    if (!is.null(fav)) return(fav)
    ec_parse_favorito_json(input$ec_favorites_json)
  }

  ec_url_tiene_deeplink_estacion <- function() {
    qs <- session$clientData$url_search
    if (is.null(qs) || !nzchar(qs)) return(FALSE)
    par <- shiny::parseQueryString(sub("^\\?", "", qs))
    eid_url <- suppressWarnings(as.integer(par[[EC_URL_PARAM_ESTACION]]))
    length(eid_url) == 1L && !is.na(eid_url) && eid_url >= 1L
  }

  abrir_reporte <- function(eid_objetivo = NULL, estacion_info = NULL) {
    ver_serie_estacion_id(NULL)
    eid_resuelto <- NULL
    if (!is.null(eid_objetivo)) {
      eid_objetivo <- as.integer(eid_objetivo)
      if (!is.na(eid_objetivo) && eid_objetivo > 0L) {
        eid_resuelto <- eid_objetivo
        estacion_id(eid_objetivo)
        shiny::updateSelectInput(session, "estacion", selected = as.character(eid_objetivo))
      }
    } else {
      if (isTRUE(mapa_habilitado())) {
        e <- estaciones()
        n <- if (is.null(e) || nrow(e) == 0) 0L else nrow(e)
        if (n == 1L) {
          eid_resuelto <- as.integer(e$id[1])
          estacion_id(eid_resuelto)
          shiny::updateSelectInput(session, "estacion", selected = as.character(eid_resuelto))
        } else {
          eid <- estacion_id()
          if (!is.null(eid) && length(eid) > 0L) eid_resuelto <- as.integer(eid)
        }
      } else {
        eid <- estacion_id()
        if (!is.null(eid) && length(eid) > 0L) eid_resuelto <- as.integer(eid)
      }
    }
    if (is.null(eid_resuelto) || is.na(eid_resuelto)) return()

    df <- if (isTRUE(mapa_habilitado())) estaciones() else NULL
    nom_override <- ec_nombre_desde_estacion_info(estacion_info, eid_resuelto)
    tabs <- reportes_abiertos()
    ya_abierto <- any(vapply(tabs, function(t) as.integer(t$eid) == eid_resuelto, logical(1)))
    if (!ya_abierto) {
      # Anadir como mas reciente y mantener un maximo de EC_MAX_TABS_REPORTE
      # (descartando la pestana mas antigua).
      tabs <- c(tabs, list(list(
        eid = eid_resuelto,
        nombre = ec_reporte_nombre_tab(eid_resuelto, df, nom_override)
      )))
      if (length(tabs) > EC_MAX_TABS_REPORTE) {
        tabs <- tabs[(length(tabs) - EC_MAX_TABS_REPORTE + 1L):length(tabs)]
      }
      reportes_abiertos(tabs)
    }
    active_report_eid(eid_resuelto)
    session$sendCustomMessage("drawer_close", TRUE)
    session$sendCustomMessage("panel_datos_state", 0)

    # Modal antes del tick: si el fetch del shell corre con el output suspendido,
    # onFlushed pisa el shell con el full y nunca hay reporte_html.done.
    if (!modal_reporte_abierto()) {
      shiny::showModal(shiny::modalDialog(
        shiny::tags$div(
          class = "ec-report-modal",
          shiny::tagList(
            shiny::div(class = "ec-modal-header",
              shiny::actionButton("cerrar_reporte", "\u2190 Volver al mapa",
                                  class = "btn-sm btn-default ec-modal-back-btn"),
              shiny::uiOutput("reporte_tabs_bar")
            ),
            reporte_ui("reporte")
          )
        ),
        title     = NULL,
        footer    = NULL,
        size      = "l",
        easyClose = FALSE,
        fade      = FALSE
      ))
      modal_reporte_abierto(TRUE)
    }
    reporte_open_tick(reporte_open_tick() + 1L)
    shiny_app_profile_mark("shiny.reporte_open.start", list(eid = as.integer(eid_resuelto)))
    session$sendCustomMessage("ec_update_mi_reporte_link", list(
      eid = as.integer(eid_resuelto),
      variableId = {
        v <- variable_aplicada()
        if (!is.null(v) && nzchar(v) && v != "0") as.integer(v) else NULL
      },
      fuenteId = {
        o <- operador_aplicado()
        if (!is.null(o) && nzchar(o) && o != "0") as.integer(o) else NULL
      }
    ))
    ec_sync_report_url()
  }

  ec_favorito_desde_contexto <- function(eid) {
    eid <- as.integer(eid)
    if (is.na(eid) || eid < 1L) return(NULL)
    df <- if (isTRUE(mapa_habilitado())) estaciones() else NULL
    alias <- paste0("Estaci\u00f3n ", eid)
    lat <- NULL
    lon <- NULL
    fuenteId <- NULL
    if (!is.null(df) && nrow(df) > 0 && eid %in% df$id) {
      row <- df[df$id == eid, , drop = FALSE][1, , drop = FALSE]
      alias <- as.character(row$nombre[1] %||% alias)
      lat <- suppressWarnings(as.numeric(row$lat[1]))
      lon <- suppressWarnings(as.numeric(row$lon[1]))
      if (!is.null(row$id_fuente[1])) fuenteId <- suppressWarnings(as.integer(row$id_fuente[1]))
    } else if (!isTRUE(mapa_habilitado())) {
      info <- api_get(paste0("/estaciones/", eid), timeout = 10)
      if (!is.null(info) && is.null(info$error)) {
        alias <- as.character(info$nombre %||% alias)
        lat <- suppressWarnings(as.numeric(info$lat %||% NA))
        lon <- suppressWarnings(as.numeric(info$lon %||% NA))
        if (!is.null(info$id_fuente)) fuenteId <- suppressWarnings(as.integer(info$id_fuente))
      }
    }
    op <- operador_aplicado()
    if (!is.null(op) && nzchar(op) && op != "0") fuenteId <- as.integer(op)
    var <- variable_aplicada()
    variableId <- if (!is.null(var) && nzchar(var) && var != "0") as.integer(var) else NULL
    list(
      eid = eid,
      variableId = variableId,
      fuenteId = fuenteId,
      alias = alias,
      lat = if (!is.na(lat)) lat else NULL,
      lon = if (!is.na(lon)) lon else NULL
    )
  }

  ec_favorito_normalize_payload <- function(fav) {
    if (is.null(fav) || is.null(fav$eid)) return(NULL)
    eid <- as.integer(fav$eid)
    if (is.na(eid) || eid < 1L) return(NULL)
    norm_int <- function(x) {
      if (is.null(x)) return(NULL)
      v <- suppressWarnings(as.integer(x))
      if (is.na(v)) return(NULL)
      v
    }
    norm_num <- function(x) {
      if (is.null(x)) return(NULL)
      v <- suppressWarnings(as.numeric(x))
      if (is.na(v)) return(NULL)
      v
    }
    list(
      eid = eid,
      variableId = norm_int(fav$variableId),
      fuenteId = norm_int(fav$fuenteId),
      alias = as.character(fav$alias %||% ""),
      lat = norm_num(fav$lat),
      lon = norm_num(fav$lon)
    )
  }

  ec_send_fijar_favorito <- function(fav) {
    fav <- ec_favorito_normalize_payload(fav)
    if (is.null(fav)) {
      session$sendCustomMessage("ec_fav_toast", list(
        text = "No se pudo guardar la estaci\u00f3n favorita.",
        ok = FALSE
      ))
      return(invisible(FALSE))
    }
    favorito_servidor(fav)
    session$sendCustomMessage("ec_fijar_favorito", fav)
    invisible(TRUE)
  }

  ec_report_share_ui <- function() {
    shiny::tags$div(class = "ec-report-share-actions",
      shiny::tags$button(
        type = "button",
        id = "copiar_enlace_reporte",
        class = "btn btn-default btn-sm",
        title = "Copiar enlace para guardar en favoritos del navegador",
        onclick = "return (window.rodajaCopiarEnlaceReporte && window.rodajaCopiarEnlaceReporte());",
        shiny::icon("link"), " Copiar enlace"
      ),
      shiny::actionButton("fijar_favorito_reporte_btn", "Fijar mi estaci\u00f3n",
        icon = shiny::icon("star"), class = "btn-sm btn-default"),
      shiny::tags$a(
        class = "btn btn-sm btn-default ec-link-mi-reporte",
        href = "#",
        target = "_blank",
        id = "ec_link_mi_reporte",
        shiny::icon("external-link"), " Modo reporte"
      )
    )
  }

  output$reporte_tabs_bar <- shiny::renderUI({
    tabs <- reportes_abiertos()
    active <- active_report_eid()
    if (length(tabs) == 0L) {
      return(shiny::tagList(
        shiny::span("Reporte de estaci\u00f3n", class = "ec-modal-title-txt"),
        ec_report_share_ui()
      ))
    }
    shiny::tags$div(class = "ec-report-tabs-wrap",
      shiny::tags$div(class = "ec-report-tabs-header-row",
        shiny::tags$div(class = "ec-report-tabs",
        lapply(tabs, function(t) {
          eid <- as.integer(t$eid)
          is_active <- !is.null(active) && as.integer(active) == eid
          shiny::tags$div(
            class = paste("ec-report-tab", if (is_active) "active" else ""),
            shiny::tags$button(
              type = "button",
              class = "ec-report-tab-label",
              onclick = sprintf("Shiny.setInputValue('reporte_tab_select',%d,{priority:'event'})", eid),
              htmltools::htmlEscape(as.character(t$nombre))
            ),
            shiny::tags$button(
              type = "button",
              class = "ec-report-tab-close",
              title = "Cerrar reporte",
              onclick = sprintf("Shiny.setInputValue('reporte_tab_close',%d,{priority:'event'})", eid),
              "\u00d7"
            )
          )
        })
      ),
      ec_report_share_ui()
      )
    )
  })
  shiny::outputOptions(output, "reporte_tabs_bar", suspendWhenHidden = FALSE)

  shiny::observeEvent(input$ver_datos, {
    abrir_reporte()
  })

  shiny::observeEvent(input$cerrar_reporte, {
    volver_al_mapa()
  })

  # Clic en una pestana: equivalente a "Ver datos" de esa estacion.
  shiny::observeEvent(input$reporte_tab_select, {
    eid <- suppressWarnings(as.integer(input$reporte_tab_select))
    if (is.na(eid) || eid < 1L) return()
    if (!is.null(active_report_eid()) && active_report_eid() == eid) return()
    abrir_reporte(eid)
  }, ignoreInit = TRUE)

  shiny::observe({
    req(!url_deeplink_done())
    qs <- session$clientData$url_search
    if (is.null(qs) || !nzchar(qs)) {
      url_deeplink_done(TRUE)
      return()
    }
    par <- shiny::parseQueryString(sub("^\\?", "", qs))
    eid <- suppressWarnings(as.integer(par[[EC_URL_PARAM_ESTACION]]))
    if (length(eid) != 1L || is.na(eid) || eid < 1L) {
      url_deeplink_done(TRUE)
      return()
    }
    if (!is.null(par[[EC_URL_PARAM_OPERADOR]]) && nzchar(par[[EC_URL_PARAM_OPERADOR]])) {
      op <- as.character(par[[EC_URL_PARAM_OPERADOR]])
      operador_aplicado(op)
      shiny::updateSelectInput(session, "operador", selected = op)
    }
    if (!is.null(par[[EC_URL_PARAM_VARIABLE]]) && nzchar(par[[EC_URL_PARAM_VARIABLE]])) {
      var <- as.character(par[[EC_URL_PARAM_VARIABLE]])
      variable_aplicada(var)
      shiny::updateSelectInput(session, "variable", selected = var)
    }
    url_deeplink_done(TRUE)
    if (!is.null(par[[EC_URL_PARAM_REPORTE]]) && par[[EC_URL_PARAM_REPORTE]] == "1") {
      mapa_habilitado(FALSE)
      deeplink_reporte_mode(TRUE)
      session$sendCustomMessage("ec_report_first", TRUE)
      url_deeplink_pending_eid(eid)
    } else {
      map_sel_con_zoom(TRUE)
      estacion_id(eid)
      shiny::updateSelectInput(session, "estacion", selected = as.character(eid))
    }
  })

  shiny::observe({
    eid <- url_deeplink_pending_eid()
    req(!is.null(eid))
    if (isTRUE(deeplink_reporte_mode())) {
      info <- api_get(paste0("/estaciones/", eid), timeout = 15)
      if (is.null(info) || !is.null(info$error)) {
        session$sendCustomMessage("ec_fav_toast", list(
          text = paste0("Estaci\u00f3n ", eid, " no encontrada o no disponible."),
          ok = FALSE
        ))
        url_deeplink_pending_eid(NULL)
        mapa_habilitado(TRUE)
        deeplink_reporte_mode(FALSE)
        session$sendCustomMessage("ec_report_first", FALSE)
        return()
      }
      estacion_id(eid)
      url_deeplink_pending_eid(NULL)
      abrir_reporte(eid, estacion_info = info)
      return()
    }
    req(estaciones_boot_done())
    req(map_ready_for_deeplink())
    df <- estaciones()
    req(nrow(df) > 0)
    if (!eid %in% df$id) {
      session$sendCustomMessage("ec_fav_toast", list(
        text = paste0("Estaci\u00f3n ", eid, " no disponible con los filtros actuales."),
        ok = FALSE
      ))
      url_deeplink_pending_eid(NULL)
      return()
    }
    url_deeplink_pending_eid(NULL)
    abrir_reporte(eid)
  })

  shiny::observeEvent(input$ec_map_ready, {
    v <- suppressWarnings(as.numeric(input$ec_map_ready))
    if (!is.finite(v) || v <= 0) return()
    was_listo <- isTRUE(mapa_listo())
    map_client_gen(v)
    # Remount tras shell ya listo: forzar replay de capas (proxy vs widget nuevo).
    if (isTRUE(mapa_habilitado()) && was_listo) {
      alertas_pintadas(FALSE)
      post_alertas_idle(FALSE)
      post_alertas_tick(0L)
    }
    if (!isTRUE(map_ready_for_deeplink())) map_ready_for_deeplink(TRUE)
  }, ignoreInit = TRUE)

  # Critical path OK → cliente permite soft-reconnect (fail-closed mid-boot off).
  shiny::observeEvent(alertas_pintadas(), {
    if (!isTRUE(alertas_pintadas())) return()
    shiny_app_profile_mark("shiny.boot_healthy")
    tryCatch(
      session$sendCustomMessage("ec_boot_healthy", TRUE),
      error = function(e) invisible(NULL)
    )
    tryCatch(
      session$sendCustomMessage("ec_lazy_assets", TRUE),
      error = function(e) invisible(NULL)
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$fijar_favorito_reporte_btn, {
    eid <- active_report_eid()
    if (is.null(eid) || is.na(suppressWarnings(as.integer(eid)))) {
      session$sendCustomMessage("ec_fav_toast", list(
        text = "Abre un reporte de estaci\u00f3n antes de fijar favorita.",
        ok = FALSE
      ))
      return()
    }
    ec_send_fijar_favorito(ec_favorito_desde_contexto(eid))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$fijar_favorito_mapa, {
    eid <- suppressWarnings(as.integer(input$fijar_favorito_mapa))
    if (is.na(eid) || eid < 1L) {
      session$sendCustomMessage("ec_fav_toast", list(
        text = "Estaci\u00f3n no v\u00e1lida para fijar favorita.",
        ok = FALSE
      ))
      return()
    }
    ec_send_fijar_favorito(ec_favorito_desde_contexto(eid))
  }, ignoreInit = TRUE)

  # Alertas apenas hay shell (no esperan metadata/geo). Prioridad > boot.
  # Espera breve al fetch browser antes de caer a httr (libera el event loop).
  shiny::observe({
    req(mapa_habilitado())
    req(mapa_listo())
    client <- input$ec_alertas_mapa_json
    has_client <- !is.null(client) && is.character(client) && nzchar(client[1])
    ticks <- session$userData$alertas_boot_wait %||% 0L
    if (!has_client && ticks < 25L) {
      session$userData$alertas_boot_wait <- ticks + 1L
      shiny::invalidateLater(100, session)
      return()
    }
    shiny_app_profile_mark("shiny.alertas_mapa.start")
    invisible(alertas_mapa_raw())
    shiny_app_profile_mark("shiny.alertas_mapa.done")
  }, priority = 20)
  # Tras paint de alertas: 1ms tick (idle cliente), luego filtros/metadata/geo (prio -10).
  shiny::observe({
    req(mapa_habilitado())
    req(alertas_pintadas())
    req(!isTRUE(post_alertas_idle()))
    tick <- post_alertas_tick()
    if (tick < 1L) {
      shiny_app_profile_mark("shiny.boot.start")
      shiny_app_profile_mark("shiny.boot.deferred")
      post_alertas_tick(1L)
      shiny::invalidateLater(1, session)
      return()
    }
    post_alertas_idle(TRUE)
    estaciones_boot_done(TRUE)
    shiny_app_profile_mark("shiny.boot.done")
  }, priority = 10)

  shiny::observeEvent(input$ec_favorites_json, {
    if (favorito_inicial_hecho() || favorito_sync_aplicado()) return()
    if (ec_url_tiene_deeplink_estacion()) {
      favorito_inicial_hecho(TRUE)
      return()
    }
    req(estaciones_boot_done())
    fav <- ec_parse_favorito_json(input$ec_favorites_json)
    if (is.null(fav) || is.na(fav$eid) || fav$eid < 1L) {
      favorito_inicial_hecho(TRUE)
      return()
    }
    favorito_sync_aplicado(TRUE)
    # Solo identidad para barra + estrella. Sin zoom, sin API, sin seleccionar.
    favorito_servidor(fav)
    favorito_inicial_hecho(TRUE)
    session$sendCustomMessage("ec_favorites_refresh", TRUE)
  }, ignoreInit = TRUE)

  # Ir a la favorita solo cuando el usuario lo pide (p. ej. botón barra).
  shiny::observe({
    req(!favorito_inicial_hecho())
    eid <- favorito_restore_eid()
    req(!is.null(eid))
    shiny::req(mapa_habilitado())
    ok <- ec_ensure_estacion_singleton(eid)
    if (isTRUE(ok)) {
      favorito_suppress_until(as.numeric(Sys.time()) + 3)
      estacion_id(eid)
      map_sel_con_zoom(TRUE)
      shiny::updateSelectInput(session, "estacion", selected = as.character(eid))
    }
    favorito_inicial_hecho(TRUE)
    favorito_restore_eid(NULL)
    session$sendCustomMessage("ec_favorites_refresh", TRUE)
  })

  shiny::observeEvent(input$abrir_mi_estacion, {
    fav <- ec_leer_favorito()
    if (is.null(fav) || is.na(fav$eid)) {
      session$sendCustomMessage("ec_wizard_open", TRUE)
      return()
    }
    favorito_servidor(fav)
    ec_aplicar_favorito_en_ui(fav)
    ec_ensure_estacion_singleton(fav$eid)
    mi_estacion_map_pending(fav$eid)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$abrir_reporte_mi_estacion, {
    fav <- ec_leer_favorito()
    if (is.null(fav) || is.na(fav$eid)) {
      session$sendCustomMessage("ec_wizard_open", TRUE)
      return()
    }
    favorito_servidor(fav)
    ec_aplicar_favorito_en_ui(fav)
    mi_estacion_pending(fav$eid)
  }, ignoreInit = TRUE)

  shiny::observe({
    eid <- mi_estacion_pending()
    req(!is.null(eid))
    df <- estaciones()
    req(nrow(df) > 0)
    if (!eid %in% df$id) {
      session$sendCustomMessage("ec_fav_toast", list(
        text = "Tu estaci\u00f3n favorita no est\u00e1 visible con los filtros actuales.",
        ok = FALSE
      ))
      mi_estacion_pending(NULL)
      return()
    }
    mi_estacion_pending(NULL)
    abrir_reporte(eid)
  })

  shiny::observe({
    eid <- mi_estacion_map_pending()
    req(!is.null(eid))
    df <- estaciones()
    req(nrow(df) > 0)
    if (!eid %in% df$id) {
      session$sendCustomMessage("ec_fav_toast", list(
        text = "Tu estaci\u00f3n favorita no est\u00e1 visible con los filtros actuales.",
        ok = FALSE
      ))
      mi_estacion_map_pending(NULL)
      return()
    }
    nom <- as.character(df$nombre[df$id == eid][1])
    if (is.na(nom) || !nzchar(nom)) nom <- paste0("Estaci\u00f3n ", eid)
    mi_estacion_map_pending(NULL)
    map_focus_tick(map_focus_tick() + 1L)
    abrir_reporte(eid)
    session$sendCustomMessage("ec_fav_toast", list(
      text = paste0("Mostrando: ", nom),
      ok = TRUE
    ))
  })

  shiny::observeEvent(input$reporte_tab_close, {
    eid_close <- suppressWarnings(as.integer(input$reporte_tab_close))
    if (is.na(eid_close) || eid_close < 1L) return()
    tabs <- reportes_abiertos()
    if (length(tabs) == 0L) return()
    tabs <- Filter(function(t) as.integer(t$eid) != eid_close, tabs)
    reportes_abiertos(tabs)
    if (length(tabs) == 0L) {
      volver_al_mapa()
      active_report_eid(NULL)
      return()
    }
    if (!is.null(active_report_eid()) && active_report_eid() == eid_close) {
      new_active <- as.integer(tabs[[length(tabs)]]$eid)
      active_report_eid(new_active)
      estacion_id(new_active)
      shiny::updateSelectInput(session, "estacion", selected = as.character(new_active))
      session$sendCustomMessage("ec_report_destroy", TRUE)
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$panel_close, { session$sendCustomMessage("panel_datos_state", 0) })
  shiny::observeEvent(input$panel_minimize, { session$sendCustomMessage("panel_datos_state", 2) })
  shiny::observeEvent(input$panel_maximize, {
    s <- input$panel_datos_state
    next_val <- if (is.null(s) || s == 2 || s == 3) 1L else 3L
    session$sendCustomMessage("panel_datos_state", next_val)
  })

  shiny::observe({
    shiny::req(post_alertas_idle())
    v <- variables()
    if (nrow(v) > 0) {
      ch <- c("Variable" = "", setNames(as.character(v$id), v$nombre))
      ids <- as.character(v$id)
      def <- as.character(EC_DEFAULT_VARIABLE_ID)
      var_ap <- variable_aplicada()
      sel <- if (!is.null(var_ap) && nzchar(var_ap) && var_ap %in% ids) {
        var_ap
      } else if (is.null(var_ap) || !nzchar(var_ap) || identical(var_ap, "0")) {
        var_ap %||% ""
      } else if (def %in% ids) {
        variable_aplicada(def)
        def
      } else {
        ""
      }
      shiny::updateSelectInput(session, "variable", choices = ch, selected = sel)
    }
  }, priority = -10)

  shiny::observe({
    shiny::req(post_alertas_idle())
    f <- fuentes()
    if (nrow(f) > 0) {
      ch <- c("Operador" = "0", setNames(as.character(f$id), f$nombre))
      op_ap <- operador_aplicado()
      sel <- if (!is.null(op_ap) && op_ap %in% c("0", as.character(f$id))) op_ap else "0"
      shiny::updateSelectInput(session, "operador", choices = ch, selected = sel)
    }
  }, priority = -10)

  shiny::observeEvent(input$ec_estacion_lazy_load, {
    estacion_selectize_loaded(TRUE)
  }, ignoreInit = TRUE)

  ec_estacion_choices <- function(e, eid = NULL) {
    ch <- c("Estacion" = "")
    if (is.null(e) || nrow(e) == 0L) return(ch)
    if (!is.null(eid) && !is.na(eid) && eid %in% e$id) {
      row <- e[e$id == eid, , drop = FALSE]
      ch <- c(ch, setNames(as.character(eid), as.character(row$nombre[1])))
    } else {
      ch <- c(ch, setNames(as.character(e$id), e$nombre))
    }
    ch
  }

  ec_estacion_selected <- function(e, curr, eid) {
    if (is.null(eid) || length(eid) == 0L) return("")
    if (eid %in% e$id) return(as.character(eid))
    if (!is.null(curr) && nzchar(curr) && curr %in% as.character(e$id)) return(curr)
    ""
  }

  shiny::observe({
    req(mapa_habilitado())
    e <- estaciones()
    eid <- estacion_id()

    if (!estacion_selectize_loaded()) {
      if (!is.null(eid) && !is.na(eid) && nrow(e) > 0L && eid %in% e$id) {
        shiny::updateSelectInput(
          session, "estacion",
          choices = ec_estacion_choices(e, eid),
          selected = as.character(eid)
        )
      }
      return()
    }

    if (nrow(e) > 0L) {
      shiny::updateSelectInput(
        session, "estacion",
        choices = ec_estacion_choices(e),
        selected = ec_estacion_selected(e, NULL, eid)
      )
    } else {
      shiny::updateSelectInput(session, "estacion", choices = c("Estacion" = ""), selected = "")
    }
  })

  output$datos_listos <- shiny::renderText({
    if (ver_datos_click() > 0) "1" else "0"
  })
  output$contador_estaciones <- shiny::renderText({
    if (!isTRUE(mapa_habilitado())) {
      eid <- estacion_id()
      if (!is.null(eid) && length(eid) > 0) return("1 estacion seleccionada")
      return("")
    }
    e <- estaciones()
    eid <- estacion_id()
    com <- comuna_aplicada()
    grupo_n <- 0L
    if (nzchar(input$ec_station_group_json %||% "")) {
      g <- tryCatch(jsonlite::fromJSON(input$ec_station_group_json), error = function(e) NULL)
      if (!is.null(g) && !is.null(g$ids)) grupo_n <- length(unlist(g$ids))
    }
    base <- if (!is.null(eid) && length(eid) > 0) {
      "1 estacion seleccionada"
    } else if (!nzchar(com %||% "")) {
      if (!isTRUE(geo_conteos_listo())) {
        "Cargando mapa..."
      } else {
        "Elige una comuna en el mapa para ver estaciones"
      }
    } else if (!isTRUE(catalogo_listo())) {
      "Cargando estaciones..."
    } else if (nrow(e) > 0) {
      paste0(nrow(e), " estaciones visibles")
    } else {
      "0 estaciones en esta comuna"
    }
    if (grupo_n > 0L) paste0(base, " \u00b7 Grupo: ", grupo_n) else base
  })

  shiny::observeEvent(input$agregar_visibles_al_grupo, {
    if (!nzchar(comuna_aplicada() %||% "")) {
      session$sendCustomMessage("ec_fav_toast", list(
        text = "Elige una comuna en el mapa para agregar estaciones al grupo.",
        ok = FALSE
      ))
      return()
    }
    e <- estaciones()
    if (nrow(e) == 0) {
      session$sendCustomMessage("ec_fav_toast", list(
        text = "No hay estaciones visibles en esta comuna.",
        ok = FALSE
      ))
      return()
    }
    session$sendCustomMessage("ec_station_group_add_ids", as.list(e$id))
  }, ignoreInit = TRUE)

  output$mapa_mount <- shiny::renderUI({
    shiny::req(mapa_habilitado())
    mapa_ui("mapa")
  })
  shiny::outputOptions(output, "mapa_mount", suspendWhenHidden = FALSE)
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

  wizard_estacion_server(
    input, output, session,
    variables = variables,
    fuentes = fuentes,
    on_abrir = abrir_reporte,
    on_aplicar_favorito = ec_aplicar_favorito_en_ui,
    on_fijar_favorito = ec_send_fijar_favorito
  )

  grupo_estaciones_ids <- shiny::reactive({
    raw <- input$ec_station_group_json
    if (is.null(raw) || !nzchar(raw)) return(integer())
    g <- tryCatch(jsonlite::fromJSON(raw), error = function(e) NULL)
    if (is.null(g) || is.null(g$ids)) return(integer())
    ids <- as.integer(unlist(g$ids))
    ids[!is.na(ids) & ids > 0L]
  })

  favorito_eid_mapa <- shiny::reactive({
    fav <- favorito_servidor()
    if (is.null(fav)) fav <- ec_parse_favorito_json(input$ec_favorites_json)
    if (is.null(fav)) return(NULL)
    eid <- suppressWarnings(as.integer(fav$eid))
    if (is.na(eid) || eid < 1L) return(NULL)
    list(
      eid = eid,
      nombre = as.character(fav$alias %||% ""),
      lat = fav$lat,
      lon = fav$lon
    )
  })

  mapa_server("mapa", estaciones, estacion_id, fuentes, on_ver_datos_mapa = abrir_reporte,
    map_focus_tick = map_focus_tick,
    enabled = mapa_habilitado, grupo_estaciones_ids = grupo_estaciones_ids,
    favorito_eid = favorito_eid_mapa, sel_con_zoom = map_sel_con_zoom,
    geo_usuario = geo_usuario, id_variable = variable_aplicada,
    geo_clear_tick = geo_clear_tick, alertas_mapa = alertas_mapa_raw,
    anticipada_mapa = anticipada_mapa_raw,
    map_ready = mapa_listo, alertas_pintadas = alertas_pintadas,
    alertas_include_ext = alertas_include_ext,
    map_client_gen = map_client_gen,
    comuna_filtro = comuna_aplicada,
    on_alerta_click = function(eid) {
      ec_ensure_estacion_singleton(eid)
      map_sel_con_zoom(TRUE)
      estacion_id(as.integer(eid))
    })
  grafico_server("grafico", estacion_id_ver, variable_id, ver_datos_click, datos, periodo_seleccionado)
  reporte_server("reporte", active_eid_rv = active_report_eid, variable_id_rv = variable_id,
    ver_datos_click_rv = reporte_open_tick)

  onboarding_server(input, session, mapa_habilitado)
}





