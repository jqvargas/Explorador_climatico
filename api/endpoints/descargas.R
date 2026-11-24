#* @apiTitle Descargas

descarga_post_body <- function(req) {
  tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = TRUE), error = function(e) NULL)
}

descarga_estimar_run <- function(eids, vids, fecha_inicio, fecha_fin, grans, formato_descarga,
                                 modo_cobertura, req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)

  if (length(eids) == 0L || length(vids) == 0L) {
    res$status <- 400
    return(list(error = "estacion_id y variable_id son requeridos"))
  }
  if (!nzchar(fecha_inicio) || !nzchar(fecha_fin)) {
    res$status <- 400
    return(list(error = "fecha_inicio y fecha_fin son requeridos"))
  }

  tier <- get_tipo_usuario(req)
  val <- descarga_validar_solicitud(tier, eids, vids, formato_descarga)
  if (!isTRUE(val$ok)) {
    res$status <- if (identical(val$code, "formato_no_permitido")) 403 else 400
    return(val)
  }
  fmt <- val$formato_descarga

  bulk <- descarga_estimar_bulk(
    val$estacion_ids, val$variable_ids, grans, fecha_inicio, fecha_fin, fmt, modo_cobertura
  )
  if (bulk$registros_total < 1) {
    res$status <- 400
    return(list(error = "No hay datos en el rango seleccionado"))
  }

  cuota <- get_cuota_usuario(get_user_id(req), tier)
  bytes_est <- descarga_bytes_num(bulk$bytes_estimados)
  chk <- descarga_excede_cuota(get_user_id(req), tier, bytes_est)
  bytes_fmt <- format_bytes(bytes_est)

  list(
    registros = bulk$registros_total,
    bytes_estimados = bytes_est,
    bytes_estimados_label = bytes_fmt$label,
    formato_descarga = fmt,
    granularidades = grans,
    desglose = bulk$desglose,
    combinaciones = bulk$combinaciones,
    estacion_ids = bulk$estacion_ids,
    variable_ids = bulk$variable_ids,
    estaciones_con_datos = bulk$estaciones_con_datos,
    aproximado = isTRUE(bulk$aproximado),
    modo_cobertura = bulk$modo_cobertura,
    dentro_cuota = chk$dentro_cuota,
    cuota = cuota,
    bytes_restantes = max(0, cuota$bytes_disponibles - bytes_est),
    bytes_restantes_label = format_bytes(max(0, cuota$bytes_disponibles - bytes_est))$label,
    on_demand = chk$on_demand,
    on_demand_disponible = chk$on_demand_disponible,
    costo_overage_clp = chk$costo_overage_clp
  )
}

#* Estima tamano aproximado de una solicitud (filas + bytes).
#* GET queda para un pedido chico. El wizard manda los ids por POST.
#* @get /descarga/estimar
function(estacion_id = "", variable_id = "", estacion_ids = list(), variable_ids = list(),
         fecha_inicio = "", fecha_fin = "", granularidad = "anual", granularidades = list(),
         formato_descarga = "liviano", modo_cobertura = "solo_con_datos", req, res) {
  descarga_estimar_run(
    descarga_parse_id_list(estacion_ids, estacion_id),
    descarga_parse_id_list(variable_ids, variable_id),
    fecha_inicio, fecha_fin,
    descarga_parse_granularidades(granularidades, granularidad),
    formato_descarga, modo_cobertura, req, res
  )
}

#* @post /descarga/estimar
function(req, res) {
  body <- descarga_post_body(req)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }
  descarga_estimar_run(
    descarga_ids_ok(body$estacion_ids),
    descarga_ids_ok(body$variable_ids),
    body$fecha_inicio %||% "",
    body$fecha_fin %||% "",
    descarga_parse_granularidades(body$granularidades, body$granularidad %||% "anual"),
    body$formato_descarga %||% "liviano",
    body$modo_cobertura %||% "solo_con_datos",
    req, res
  )
}

descarga_flag_true <- function(x) {
  if (isTRUE(x)) return(TRUE)
  tolower(trimws(as.character(x %||% ""))) %in% c("1", "true", "yes", "si")
}

descarga_cobertura_run <- function(eids, vids, fecha_inicio, fecha_fin, incluir_ids, req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  if (length(eids) == 0L || length(vids) == 0L) {
    res$status <- 400
    return(list(error = "estacion_ids y variable_ids son requeridos"))
  }
  if (!nzchar(fecha_inicio) || !nzchar(fecha_fin)) {
    res$status <- 400
    return(list(error = "fecha_inicio y fecha_fin son requeridos"))
  }
  tier <- get_tipo_usuario(req)
  val <- descarga_validar_solicitud(tier, eids, vids, "liviano")
  if (!isTRUE(val$ok)) {
    res$status <- 400
    return(val)
  }
  descarga_cobertura_resumen(
    val$estacion_ids, val$variable_ids, fecha_inicio, fecha_fin,
    incluir_ids = isTRUE(incluir_ids)
  )
}

#* Cobertura de variables por estacion. Conteos; los ids solo si incluir_ids.
#* @get /descarga/cobertura
function(estacion_id = "", variable_id = "", estacion_ids = list(), variable_ids = list(),
         fecha_inicio = "", fecha_fin = "", incluir_ids = "", req, res) {
  descarga_cobertura_run(
    descarga_parse_id_list(estacion_ids, estacion_id),
    descarga_parse_id_list(variable_ids, variable_id),
    fecha_inicio, fecha_fin, descarga_flag_true(incluir_ids), req, res
  )
}

#* @post /descarga/cobertura
function(req, res) {
  body <- descarga_post_body(req)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }
  descarga_cobertura_run(
    descarga_ids_ok(body$estacion_ids),
    descarga_ids_ok(body$variable_ids),
    body$fecha_inicio %||% "",
    body$fecha_fin %||% "",
    descarga_flag_true(body$incluir_ids),
    req, res
  )
}

#* Pares estacion-variable ya agregados, para filtrar el catalogo en el cliente.
#* @get /descarga/pares-variable
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  cache_or_compute("descarga:pares-variable:h", descarga_pares_por_variable, ttl = 900, res = res)
}

#* @post /solicitar-descarga
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) {
    auth_err$code <- "auth_required"
    return(auth_err)
  }

  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }

  estacion_ids <- as.integer(unlist(body$estacion_ids %||% list()))
  variable_ids <- as.integer(unlist(body$variable_ids %||% list()))
  fecha_inicio <- body$fecha_inicio
  fecha_fin <- body$fecha_fin
  email <- body$email %||% req$user$email %||% ""
  formato_descarga <- body$formato_descarga %||% body$formato %||% "liviano"
  granularidad <- tolower(trimws(body$granularidad %||% "anual"))

  estacion_ids <- estacion_ids[!is.na(estacion_ids) & estacion_ids > 0L]
  variable_ids <- variable_ids[!is.na(variable_ids) & variable_ids > 0L]

  if (length(estacion_ids) == 0 || length(variable_ids) == 0) {
    res$status <- 400
    return(list(error = "estacion_ids y variable_ids son requeridos"))
  }
  if (is.null(fecha_inicio) || is.null(fecha_fin)) {
    res$status <- 400
    return(list(error = "fecha_inicio y fecha_fin son requeridos"))
  }
  if (!granularidad %in% GRANULARIDADES_VALIDAS) {
    res$status <- 400
    return(list(error = "granularidad inv\u00e1lida"))
  }

  tier <- get_tipo_usuario(req)
  lim <- get_limite_tier(tier)
  user_id <- get_user_id(req)

  email_body <- tolower(trimws(body$email %||% ""))
  if (nzchar(email_body) && !is.null(req$user)) {
    req$user <- sync_usuario_email(req$user, email_body)
    tier <- get_tipo_usuario(req)
    lim <- get_limite_tier(tier)
    user_id <- get_user_id(req)
  }

  val <- descarga_validar_solicitud(tier, estacion_ids, variable_ids, formato_descarga)
  if (!isTRUE(val$ok)) {
    res$status <- if (identical(val$code, "formato_no_permitido")) 403 else 400
    return(val)
  }
  fmt <- val$formato_descarga
  estacion_ids <- val$estacion_ids
  variable_ids <- val$variable_ids

  if (!granularidad %in% lim$granularidades) {
    res$status <- 403
    return(list(
      error = paste0("Granularidad '", granularidad, "' no v\u00e1lida"),
      code = "granularidad_no_permitida",
      tier = tier
    ))
  }

  registros <- descarga_estimar_registros_rapido(
    fecha_inicio, fecha_fin, granularidad, length(estacion_ids), length(variable_ids)
  )
  bytes_est <- descarga_estimar_bytes(registros, fmt, length(estacion_ids), length(variable_ids))
  chk <- descarga_excede_cuota(user_id, tier, bytes_est)
  if (isTRUE(chk$excede)) {
    res$status <- 429
    return(descarga_respuesta_cuota_excedida(user_id, tier, bytes_est))
  }

  if (registros < 1L) {
    registros <- descarga_estimar_registros(estacion_ids, variable_ids, fecha_inicio, fecha_fin, granularidad)
    bytes_est <- descarga_estimar_bytes(registros, fmt, length(estacion_ids), length(variable_ids))
    chk <- descarga_excede_cuota(user_id, tier, bytes_est)
    if (isTRUE(chk$excede)) {
      res$status <- 429
      return(descarga_respuesta_cuota_excedida(user_id, tier, bytes_est))
    }
  }
  if (registros < 1L) {
    res$status <- 400
    return(list(error = "No hay datos en el rango seleccionado"))
  }

  if (bytes_est < 1L) {
    res$status <- 400
    return(list(error = "No se pudo estimar el tama\u00f1o de la descarga"))
  }

  reserve <- reservar_bytes_descarga(user_id, tier, bytes_est)
  if (!isTRUE(reserve$ok)) {
    res$status <- 429
    return(descarga_respuesta_cuota_excedida(user_id, tier, bytes_est, reserve$code %||% "cuota_excedida"))
  }

  job_id <- uuid::UUIDgenerate()
  params_json <- jsonlite::toJSON(list(
    estacion_ids = estacion_ids,
    variable_ids = variable_ids,
    fecha_inicio = fecha_inicio,
    fecha_fin = fecha_fin,
    formato_descarga = fmt,
    granularidad = granularidad,
    app_trace_id = {
      tid <- tryCatch(app_profile_ctx_trace(), error = function(e) NULL)
      if (is.null(tid) || !nzchar(as.character(tid))) NULL else as.character(tid)
    }
  ), auto_unbox = TRUE, null = "null")

  ok_insert <- tryCatch({
    db_execute(
      "INSERT INTO encargo_de_archivo (id, estado, email, params, expira_en, user_id, registros_estimados, bytes_estimados, bytes_overage, bytes_extra, granularidad, formato_descarga)
       VALUES ($1, 'pendiente', $2, $3::jsonb, NOW() + INTERVAL '48 hours', $4::uuid, $5, $6::bigint, $7, $8, $9, $10)",
      params = list(job_id, email, params_json, user_id, if (registros > 2147483647) 2147483647L else as.integer(registros), bytes_est, reserve$overage_bytes %||% 0L, reserve$extra_bytes %||% 0L, granularidad, fmt)
    )
    TRUE
  }, error = function(e) {
    liberar_bytes_cuota(user_id, tier, bytes_est, reserve$overage_bytes %||% 0L, reserve$extra_bytes %||% 0L)
    FALSE
  })

  if (!ok_insert) {
    res$status <- 500
    return(list(error = "No se pudo encolar la descarga"))
  }

  redis_host <- Sys.getenv("REDIS_HOST", "127.0.0.1")
  r <- tryCatch(redux::hiredis(host = redis_host), error = function(e) NULL)
  if (!is.null(r)) r$RPUSH("cola_descargas", job_id)

  list(
    job_id = job_id,
    registros_estimados = registros,
    bytes_estimados = bytes_est,
    bytes_estimados_label = format_bytes(bytes_est)$label,
    formato_descarga = fmt,
    mensaje = "Descarga en cola. Consulta el estado en estado_url.",
    estado_url = paste0("/descarga/estado/", job_id)
  )
}

#* @get /descarga/estado/<job_id>
function(job_id, req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)

  df <- db_query(
    "SELECT id, estado, link_descarga, creado_en, completado_en, expira_en, user_id,
            registros_estimados, registros_reales, bytes_estimados, bytes_reales,
            formato_descarga, params, archivo_nombre
     FROM encargo_de_archivo WHERE id = $1",
    params = list(job_id)
  )
  if (nrow(df) == 0) {
    res$status <- 404
    return(list(error = "Trabajo no encontrado"))
  }
  row <- df[1, ]
  uid <- get_user_id(req)
  if (!is.null(row$user_id) && !is.na(row$user_id) && nzchar(as.character(row$user_id))) {
    if (is.null(uid) || !identical(as.character(uid), as.character(row$user_id))) {
      res$status <- 403
      return(list(error = "No autorizado"))
    }
  }

  bytes_reales <- row$bytes_reales
  est <- row$estado
  params_estado <- tryCatch(jsonlite::fromJSON(row$params[1]), error = function(e) list())
  archivo_nombre <- descarga_nombre_desde_trabajo(row)
  progreso <- switch(as.character(est),
    listo = 100L,
    procesando = 72L,
    pendiente = 38L,
    error = 100L,
    fallido = 100L,
    15L
  )
  etapa <- switch(as.character(est),
    listo = "Archivo listo",
    procesando = "Generando archivo",
    pendiente = "En cola",
    error = "Error en exportaci\u00f3n",
    fallido = "Exportacion fallida",
    "Registrado"
  )
  list(
    job_id = row$id,
    estado = est,
    etapa_label = etapa,
    progreso_pct = progreso,
    link_descarga = row$link_descarga,
    creado_en = row$creado_en,
    completado_en = row$completado_en,
    expira_en = row$expira_en,
    registros_estimados = row$registros_estimados,
    registros_reales = row$registros_reales,
    bytes_estimados = row$bytes_estimados,
    bytes_reales = bytes_reales,
    bytes_reales_label = if (!is.na(bytes_reales)) format_bytes(bytes_reales)$label else NULL,
    formato_descarga = row$formato_descarga,
    archivo_nombre = archivo_nombre
  )
}

#* @get /descarga/archivo/<job_id>
#* @serializer contentType list(type="application/octet-stream")
function(job_id, req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) {
    res$status <- 401
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(auth_err, auto_unbox = TRUE)))
  }

  df <- db_query(
    "SELECT id, estado, user_id, expira_en, params, formato_descarga, registros_estimados, archivo_nombre
     FROM encargo_de_archivo WHERE id = $1",
    params = list(job_id)
  )
  if (nrow(df) == 0) {
    res$status <- 404
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(list(error = "Trabajo no encontrado"), auto_unbox = TRUE)))
  }
  row <- df[1, ]
  uid <- get_user_id(req)
  if (!is.null(row$user_id) && !is.na(row$user_id) && nzchar(as.character(row$user_id))) {
    if (is.null(uid) || !identical(as.character(uid), as.character(row$user_id))) {
      res$status <- 403
      res$setHeader("Content-Type", "application/json; charset=utf-8")
      return(charToRaw(jsonlite::toJSON(list(error = "No autorizado"), auto_unbox = TRUE)))
    }
  }
  if (!identical(row$estado, "listo")) {
    res$status <- 409
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(list(error = "Archivo no disponible"), auto_unbox = TRUE)))
  }
  if (!is.na(row$expira_en) && as.POSIXct(row$expira_en) < Sys.time()) {
    res$status <- 410
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(list(error = "Enlace expirado"), auto_unbox = TRUE)))
  }

  descargas_dir <- Sys.getenv("DESCARGAS_DIR", "/tmp/descargas")
  path_csv <- file.path(descargas_dir, paste0(job_id, ".csv"))
  path_zip <- file.path(descargas_dir, paste0(job_id, ".zip"))
  path_tgz <- file.path(descargas_dir, paste0(job_id, ".tar.gz"))
  path <- if (file.exists(path_zip)) path_zip else if (file.exists(path_tgz)) path_tgz else path_csv
  if (!file.exists(path)) {
    res$status <- 404
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(list(error = "Archivo no encontrado"), auto_unbox = TRUE)))
  }

  params <- tryCatch(jsonlite::fromJSON(row$params[1]), error = function(e) list())
  fname <- descarga_sanitize_filename(descarga_nombre_desde_trabajo(row) %||% basename(path))
  ctype <- if (grepl("\\.zip$", fname, ignore.case = TRUE)) {
    "application/zip"
  } else if (grepl("\\.tar\\.gz$", fname, ignore.case = TRUE)) {
    "application/gzip"
  } else {
    "text/csv; charset=utf-8"
  }
  res$setHeader("Content-Type", ctype)
  res$setHeader("Content-Disposition", descarga_content_disposition(fname))
  res$setHeader("Cache-Control", "private, no-store")
  readBin(path, "raw", file.info(path)$size)
}

#* Solicitud bulk: lote con un job por granularidad
#* @post /solicitar-descarga-bulk
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) {
    auth_err$code <- "auth_required"
    return(auth_err)
  }

  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }

  estacion_ids <- as.integer(unlist(body$estacion_ids %||% list()))
  variable_ids <- as.integer(unlist(body$variable_ids %||% list()))
  granularidades <- descarga_parse_granularidades(body$granularidades, body$granularidad %||% "anual")
  fecha_inicio <- body$fecha_inicio
  fecha_fin <- body$fecha_fin
  email <- body$email %||% req$user$email %||% ""
  formato_descarga <- body$formato_descarga %||% body$formato %||% "liviano"
  nombre_lote <- trimws(body$nombre_lote %||% "descarga_bulk")
  modo_cobertura <- body$modo_cobertura %||% "solo_con_datos"

  if (length(estacion_ids) == 0 || length(variable_ids) == 0) {
    res$status <- 400
    return(list(error = "estacion_ids y variable_ids son requeridos"))
  }
  if (is.null(fecha_inicio) || is.null(fecha_fin)) {
    res$status <- 400
    return(list(error = "fecha_inicio y fecha_fin son requeridos"))
  }

  tier <- get_tipo_usuario(req)
  user_id <- get_user_id(req)
  email_body <- tolower(trimws(body$email %||% ""))
  if (nzchar(email_body) && !is.null(req$user)) {
    req$user <- sync_usuario_email(req$user, email_body)
    tier <- get_tipo_usuario(req)
    user_id <- get_user_id(req)
  }

  val <- descarga_validar_solicitud(tier, estacion_ids, variable_ids, formato_descarga)
  if (!isTRUE(val$ok)) {
    res$status <- if (identical(val$code, "formato_no_permitido")) 403 else 400
    return(val)
  }
  fmt <- val$formato_descarga

  bulk <- descarga_estimar_bulk(
    val$estacion_ids, val$variable_ids, granularidades,
    fecha_inicio, fecha_fin, fmt, modo_cobertura
  )
  if (bulk$registros_total < 1) {
    res$status <- 400
    return(list(error = "No hay datos en el rango seleccionado"))
  }

  bytes_est <- descarga_bytes_num(bulk$bytes_estimados)
  chk <- descarga_excede_cuota(user_id, tier, bytes_est)
  if (isTRUE(chk$excede)) {
    res$status <- 429
    return(descarga_respuesta_cuota_excedida(user_id, tier, bytes_est))
  }
  reserve <- reservar_bytes_descarga(user_id, tier, bytes_est)
  if (!isTRUE(reserve$ok)) {
    res$status <- 429
    return(descarga_respuesta_cuota_excedida(user_id, tier, bytes_est, reserve$code %||% "cuota_excedida"))
  }

  lote_id <- uuid::UUIDgenerate()
  lote_params <- list(
    estacion_ids = bulk$estacion_ids,
    variable_ids = bulk$variable_ids,
    granularidades = granularidades,
    fecha_inicio = fecha_inicio,
    fecha_fin = fecha_fin,
    formato_descarga = fmt,
    nombre_lote = nombre_lote,
    modo_cobertura = bulk$modo_cobertura,
    bytes_overage_reservado = reserve$overage_bytes %||% 0L,
    bytes_extra_reservado = reserve$extra_bytes %||% 0L
  )
  lote_json <- jsonlite::toJSON(lote_params, auto_unbox = TRUE)

  ok_lote <- tryCatch({
    db_execute(
      "INSERT INTO fardo_de_datos (id, user_id, nombre, estado, params, bytes_estimados, expira_en)
       VALUES ($1, $2::uuid, $3, 'pendiente', $4::jsonb, $5::bigint, NOW() + INTERVAL '48 hours')",
      params = list(lote_id, user_id, nombre_lote, lote_json, bytes_est)
    )
    TRUE
  }, error = function(e) {
    liberar_bytes_cuota(user_id, tier, bytes_est, reserve$overage_bytes %||% 0L, reserve$extra_bytes %||% 0L)
    FALSE
  })

  if (!ok_lote) {
    res$status <- 500
    return(list(error = "No se pudo crear el lote de descarga"))
  }

  job_ids <- character(0)
  redis_host <- Sys.getenv("REDIS_HOST", "127.0.0.1")
  r <- tryCatch(redux::hiredis(host = redis_host), error = function(e) NULL)

  for (gran in granularidades) {
    desg <- NULL
    for (d in bulk$desglose) {
      if (identical(d$granularidad, gran)) {
        desg <- d
        break
      }
    }
    regs_gran <- if (!is.null(desg)) descarga_bytes_num(desg$registros) else 0
    bytes_gran <- if (!is.null(desg)) descarga_bytes_num(desg$bytes) else 0
    if (regs_gran < 1) next
    regs_ins <- if (regs_gran > 2147483647) 2147483647 else as.integer(regs_gran)

    job_id <- uuid::UUIDgenerate()
    params_json <- jsonlite::toJSON(list(
      estacion_ids = bulk$estacion_ids,
      variable_ids = bulk$variable_ids,
      fecha_inicio = fecha_inicio,
      fecha_fin = fecha_fin,
      formato_descarga = fmt,
      granularidad = gran,
      lote_id = lote_id,
      modo_cobertura = bulk$modo_cobertura
    ), auto_unbox = TRUE)

    ok_job <- tryCatch({
      db_execute(
        "INSERT INTO encargo_de_archivo (id, estado, email, params, expira_en, user_id, lote_id,
          registros_estimados, bytes_estimados, granularidad, formato_descarga)
         VALUES ($1, 'pendiente', $2, $3::jsonb, NOW() + INTERVAL '48 hours', $4::uuid, $5, $6, $7::bigint, $8, $9)",
        params = list(job_id, email, params_json, user_id, lote_id, regs_ins, bytes_gran, gran, fmt)
      )
      TRUE
    }, error = function(e) FALSE)

    if (!ok_job) next
    job_ids <- c(job_ids, job_id)
    if (!is.null(r)) r$RPUSH("cola_descargas", job_id)
  }

  if (length(job_ids) == 0L) {
    db_execute("UPDATE fardo_de_datos SET estado = 'error' WHERE id = $1", params = list(lote_id))
    liberar_bytes_cuota(user_id, tier, bytes_est, reserve$overage_bytes %||% 0L, reserve$extra_bytes %||% 0L)
    res$status <- 500
    return(list(error = "No se pudieron encolar trabajos del lote"))
  }

  db_execute("UPDATE fardo_de_datos SET estado = 'procesando' WHERE id = $1", params = list(lote_id))

  list(
    lote_id = lote_id,
    job_ids = job_ids,
    registros_estimados = bulk$registros_total,
    bytes_estimados = bytes_est,
    bytes_estimados_label = format_bytes(bytes_est)$label,
    formato_descarga = fmt,
    granularidades = granularidades,
    mensaje = "Lote en cola. Consulta estado en estado_url.",
    estado_url = paste0("/descarga/lote/", lote_id)
  )
}

#* Estado de un lote bulk
#* @get /descarga/lote/<lote_id>
function(lote_id, req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)

  df <- db_query(
    "SELECT id, user_id, nombre, estado, params, bytes_estimados, bytes_reales,
            archivo_nombre, link_descarga, creado_en, completado_en, expira_en
     FROM fardo_de_datos WHERE id = $1",
    params = list(lote_id)
  )
  if (nrow(df) == 0) {
    res$status <- 404
    return(list(error = "Lote no encontrado"))
  }
  row <- df[1, ]
  uid <- get_user_id(req)
  if (!is.null(row$user_id) && !is.na(row$user_id) && nzchar(as.character(row$user_id))) {
    if (is.null(uid) || !identical(as.character(uid), as.character(row$user_id))) {
      res$status <- 403
      return(list(error = "No autorizado"))
    }
  }

  hijos <- db_query(
    "SELECT id, estado, granularidad, bytes_estimados, bytes_reales, archivo_nombre, completado_en
     FROM encargo_de_archivo WHERE lote_id = $1 ORDER BY creado_en",
    params = list(lote_id)
  )

  jobs <- if (nrow(hijos) > 0) {
    lapply(seq_len(nrow(hijos)), function(i) {
      h <- hijos[i, ]
      list(
        job_id = h$id,
        estado = h$estado,
        granularidad = h$granularidad,
        bytes_estimados = h$bytes_estimados,
        bytes_reales = h$bytes_reales,
        archivo_nombre = h$archivo_nombre
      )
    })
  } else {
    list()
  }

  n_ok <- sum(hijos$estado == "listo", na.rm = TRUE)
  n_err <- sum(hijos$estado %in% c("error", "fallido"), na.rm = TRUE)
  n_total <- nrow(hijos)
  progreso <- if (n_total > 0) as.integer(round(100 * n_ok / n_total)) else 0L
  if (identical(row$estado, "listo")) progreso <- 100L

  list(
    lote_id = row$id,
    nombre = row$nombre,
    estado = row$estado,
    progreso_pct = progreso,
    jobs = jobs,
    jobs_listos = n_ok,
    jobs_total = n_total,
    jobs_error = n_err,
    bytes_estimados = row$bytes_estimados,
    bytes_reales = row$bytes_reales,
    archivo_nombre = row$archivo_nombre,
    link_descarga = row$link_descarga,
    creado_en = row$creado_en,
    completado_en = row$completado_en,
    expira_en = row$expira_en
  )
}

#* Descargar archivo ZIP de un lote
#* @get /descarga/lote/<lote_id>/archivo
#* @serializer contentType list(type="application/octet-stream")
function(lote_id, req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) {
    res$status <- 401
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(auth_err, auto_unbox = TRUE)))
  }

  df <- db_query(
    "SELECT id, estado, user_id, expira_en, archivo_nombre FROM fardo_de_datos WHERE id = $1",
    params = list(lote_id)
  )
  if (nrow(df) == 0) {
    res$status <- 404
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(list(error = "Lote no encontrado"), auto_unbox = TRUE)))
  }
  row <- df[1, ]
  uid <- get_user_id(req)
  if (!is.null(row$user_id) && !is.na(row$user_id) && nzchar(as.character(row$user_id))) {
    if (is.null(uid) || !identical(as.character(uid), as.character(row$user_id))) {
      res$status <- 403
      res$setHeader("Content-Type", "application/json; charset=utf-8")
      return(charToRaw(jsonlite::toJSON(list(error = "No autorizado"), auto_unbox = TRUE)))
    }
  }
  if (!row$estado %in% c("listo", "parcial")) {
    res$status <- 409
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(list(error = "Archivo no disponible"), auto_unbox = TRUE)))
  }
  if (!is.na(row$expira_en) && as.POSIXct(row$expira_en) < Sys.time()) {
    res$status <- 410
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(list(error = "Enlace expirado"), auto_unbox = TRUE)))
  }

  descargas_dir <- Sys.getenv("DESCARGAS_DIR", "/tmp/descargas")
  path <- file.path(descargas_dir, paste0("lote_", lote_id, ".zip"))
  if (!file.exists(path)) {
    res$status <- 404
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(list(error = "Archivo no encontrado"), auto_unbox = TRUE)))
  }

  fname <- row$archivo_nombre %||% paste0("rodaja_lote_", lote_id, ".zip")
  res$setHeader("Content-Type", "application/zip")
  res$setHeader("Content-Disposition", descarga_content_disposition(fname))
  res$setHeader("Cache-Control", "private, no-store")
  readBin(path, "raw", file.info(path)$size)
}

#* Cuota para exportar vista PDF del reporte (preview, sin reservar)
#* @get /me/reporte-vista/cuota
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  user_id <- req$user$id
  tier <- get_tipo_usuario(req)
  bytes_est <- reporte_vista_pdf_bytes_estimar()
  chk <- descarga_excede_cuota(user_id, tier, bytes_est)
  fmt <- format_bytes(bytes_est)
  cuota <- get_cuota_usuario(user_id, tier)
  jsonlite::toJSON(list(
    bytes_estimados = bytes_est,
    bytes_estimados_label = fmt$label,
    dentro_cuota = !isTRUE(chk$excede),
    on_demand_disponible = isTRUE(chk$on_demand_disponible),
    cuota = cuota
  ), auto_unbox = TRUE, null = "null")
}

#* Consumir cuota al exportar vista PDF (tarifa plana; requiere cuenta)
#* @post /me/reporte-vista/consumir
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) {
    res$status <- 401
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(auth_err, auto_unbox = TRUE)))
  }

  user_id <- req$user$id
  tier <- get_tipo_usuario(req)
  bytes_est <- reporte_vista_pdf_bytes_estimar()

  chk <- descarga_excede_cuota(user_id, tier, bytes_est)
  if (isTRUE(chk$excede)) {
    res$status <- 429
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(
      descarga_respuesta_cuota_excedida(user_id, tier, bytes_est),
      auto_unbox = TRUE
    )))
  }

  reserve <- reservar_bytes_descarga(user_id, tier, bytes_est)
  if (!isTRUE(reserve$ok)) {
    res$status <- 429
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(
      descarga_respuesta_cuota_excedida(user_id, tier, bytes_est, reserve$code %||% "cuota_excedida"),
      auto_unbox = TRUE
    )))
  }

  fmt <- format_bytes(bytes_est)
  cuota <- get_cuota_usuario(user_id, tier)
  jsonlite::toJSON(list(
    ok = TRUE,
    bytes = bytes_est,
    bytes_label = fmt$label,
    cuota = cuota
  ), auto_unbox = TRUE, null = "null")
}

#* CSV de la tabla anual del reporte. El cliente manda el texto; se cobra ese tamano y se devuelve el archivo.
#* @post /me/reporte-tabla/consumir
#* @serializer contentType list(type="text/csv; charset=utf-8")
function(req, res) {
  csv_json <- function(status, payload) {
    res$status <- status
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE))
  }

  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(csv_json(401L, auth_err))

  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)
  if (is.null(body)) return(csv_json(400L, list(error = "JSON inv\u00e1lido")))

  payload <- reporte_tabla_csv_payload(body$csv, body$anio)
  if (!isTRUE(payload$ok)) return(csv_json(400L, list(error = payload$error %||% "CSV inv\u00e1lido")))

  user_id <- req$user$id
  tier <- get_tipo_usuario(req)
  bytes_est <- payload$bytes
  chk <- descarga_excede_cuota(user_id, tier, bytes_est)
  if (isTRUE(chk$excede)) {
    return(csv_json(429L, descarga_respuesta_cuota_excedida(user_id, tier, bytes_est)))
  }

  reserve <- reservar_bytes_descarga(user_id, tier, bytes_est)
  if (!isTRUE(reserve$ok)) {
    return(csv_json(
      429L,
      descarga_respuesta_cuota_excedida(user_id, tier, bytes_est, reserve$code %||% "cuota_excedida")
    ))
  }

  res$setHeader("Content-Disposition", descarga_content_disposition(payload$filename))
  res$setHeader("Cache-Control", "private, no-store")
  payload$raw
}
