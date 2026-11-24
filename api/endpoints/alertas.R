#* @apiTitle Alertas top 10 por email

#* Estado de abono de alertas del usuario
#* @get /me/alertas
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  u <- req$user
  tier <- get_tipo_usuario(req)
  lim <- get_limite_tier(tier)
  prefs <- get_preferencias_usuario(u$id, u$email)
  max_subs <- lim$max_alertas_suscripcion
  max_vars <- lim$max_alertas_variables
  list(
    email = as.character(u$email %||% ""),
    email_efectivo = as.character(prefs$email_efectivo %||% ""),
    tier = as.character(tier),
    max_alertas_suscripcion = if (is.null(max_subs)) NULL else as.integer(max_subs),
    max_alertas_variables = if (is.null(max_vars)) NULL else as.integer(max_vars),
    alertas_scopes = as.character(scopes_permitidos_tier(tier)),
    variables_alertas = listar_variables_alertas(),
    prueba_enviada_hoy = prueba_alerta_enviada_hoy(u$id),
    abono = get_suscripciones_usuario(u$id),
    reglas = listar_reglas_usuario(u$id),
    umbrales = if (exists("listar_umbrales_usuario", mode = "function")) listar_umbrales_usuario(u$id) else list(),
    umbral_contexto = if (exists("umbral_contexto_usuario", mode = "function")) umbral_contexto_usuario(u$id) else list(),
    preferencias = prefs,
    alerta_temprana_activa = get_alerta_temprana_activa(u$id),
    alerta_tardia_activa = get_alerta_tardia_activa(u$id),
    carta_devuelta = correo_rebote_payload_usuario(u, prefs)
  )
}

#* Activar o desactivar alertas por favorita o grupo (compat legacy)
#* @put /me/alertas
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }

  scope_type <- tolower(trimws(as.character(body$scope_type %||% "")))
  activa <- if (is.null(body$activa)) TRUE else parse_bool(body$activa)
  exclusivo <- isTRUE(parse_bool(body$exclusivo)) || identical(scope_type, "avanzada")

  u <- req$user
  tier <- get_tipo_usuario(req)
  if (exclusivo) {
    if (!scope_type %in% c("favorita", "grupo", "avanzada")) {
      res$status <- 400
      return(list(error = "scope_type debe ser favorita, grupo o avanzada"))
    }
    if (!alertas_tabla_existe()) {
      res$status <- 503
      return(list(error = "Sistema de alertas no disponible"))
    }
    out <- aplicar_alcance_exclusivo(u$id, tier, scope_type, activa)
    if (!isTRUE(out$ok)) {
      res$status <- 403
      return(list(error = out$error))
    }
    return(list(ok = TRUE, abono = get_suscripciones_usuario(u$id), reglas = listar_reglas_usuario(u$id)))
  }
  if (!scope_type %in% ALERTAS_SCOPE_LEGACY) {
    res$status <- 400
    return(list(error = "scope_type debe ser favorita o grupo"))
  }

  chk <- puede_activar_suscripcion(u$id, tier, scope_type, activa)
  if (!isTRUE(chk$ok)) {
    res$status <- 403
    return(list(error = chk$error))
  }

  if (!alertas_tabla_existe()) {
    res$status <- 503
    return(list(error = "Sistema de alertas no disponible"))
  }

  upsert_suscripcion(u$id, scope_type, activa)
  list(ok = TRUE, abono = get_suscripciones_usuario(u$id), reglas = listar_reglas_usuario(u$id))
}

#* Preferencias globales de alertas
#* @get /me/alertas/preferencias
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  u <- req$user
  get_preferencias_usuario(u$id, u$email)
}

#* Actualizar preferencias de alertas
#* @put /me/alertas/preferencias
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }
  u <- req$user
  tier <- get_tipo_usuario(req)
  chk <- upsert_preferencias_usuario(u$id, body, tier)
  if (!isTRUE(chk$ok)) {
    res$status <- 400
    return(list(error = chk$error))
  }
  list(ok = TRUE, preferencias = get_preferencias_usuario(u$id, u$email))
}

#* Clave publica VAPID (tambien sin cuenta, al elegir estacion)
#* @get /alertas/push/clave
function(req, res) {
  pub <- if (exists("alertas_push_vapid_public", mode = "function")) alertas_push_vapid_public() else ""
  if (!nzchar(pub)) {
    res$status <- 503
    return(list(error = "Avisos del celular no configurados"))
  }
  list(publicKey = pub)
}

#* Registrar el celular al elegir estacion. Con sesion activa alertas y boletin.
#* @post /alertas/push
function(req, res) {
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }
  keys <- body$keys %||% list()
  uid <- if (!is.null(req$user)) req$user$id else NULL
  chk <- alertas_push_reclamar(
    uid, body$endpoint %||% "", keys$p256dh %||% "", keys$auth %||% "",
    body$eid %||% NA, req$HTTP_USER_AGENT %||% ""
  )
  if (!isTRUE(chk$ok)) {
    res$status <- 400
    return(list(error = chk$error))
  }
  list(ok = TRUE, cuenta = isTRUE(chk$cuenta))
}

#* Clave publica VAPID para suscribir el celular
#* @get /me/alertas/push/clave
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  pub <- if (exists("alertas_push_vapid_public", mode = "function")) alertas_push_vapid_public() else ""
  if (!nzchar(pub)) {
    res$status <- 503
    return(list(error = "Avisos del celular no configurados"))
  }
  list(publicKey = pub)
}

#* Registrar este telefono
#* @put /me/alertas/push
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }
  keys <- body$keys %||% list()
  ua <- req$HTTP_USER_AGENT %||% ""
  chk <- alertas_push_guardar_suscripcion(
    req$user$id, body$endpoint %||% "", keys$p256dh %||% "", keys$auth %||% "", ua
  )
  if (!isTRUE(chk$ok)) {
    res$status <- 400
    return(list(error = chk$error))
  }
  list(ok = TRUE)
}

#* Quitar este telefono (o todos si no viene endpoint)
#* @delete /me/alertas/push
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  body <- tryCatch(jsonlite::fromJSON(req$postBody %||% "", simplifyVector = FALSE), error = function(e) list())
  if (is.null(body)) body <- list()
  alertas_push_borrar_suscripcion(req$user$id, body$endpoint %||% "")
  list(ok = TRUE)
}

#* Estadisticas de alertas del usuario
#* @get /me/alertas/estadisticas
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  estadisticas_alertas_usuario(req$user$id)
}

#* Listar reglas de alerta
#* @get /me/alertas/reglas
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  tier <- get_tipo_usuario(req)
  list(
    reglas = listar_reglas_usuario(req$user$id),
    alertas_scopes = scopes_permitidos_tier(tier)
  )
}

#* Crear regla de alerta
#* @post /me/alertas/reglas
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }
  if (!alertas_regla_tabla_existe()) {
    res$status <- 503
    return(list(error = "Reglas de alertas no disponibles"))
  }
  u <- req$user
  tier <- get_tipo_usuario(req)
  out <- crear_regla(u$id, tier, body)
  if (!isTRUE(out$ok)) {
    res$status <- 403
    return(list(error = out$error))
  }
  out
}

#* Actualizar regla de alerta
#* @put /me/alertas/reglas/<id>
function(req, res, id) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }
  u <- req$user
  tier <- get_tipo_usuario(req)
  out <- actualizar_regla(u$id, tier, id, body)
  if (!isTRUE(out$ok)) {
    res$status <- if (identical(out$error, "Regla no encontrada")) 404 else 403
    return(list(error = out$error))
  }
  out
}

#* Eliminar regla de alerta
#* @delete /me/alertas/reglas/<id>
function(req, res, id) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  out <- eliminar_regla(req$user$id, id)
  if (!isTRUE(out$ok)) {
    res$status <- 404
    return(list(error = "Regla no encontrada"))
  }
  list(ok = TRUE)
}

#* Reglas de umbral absoluto del usuario
#* @get /me/alertas/umbrales
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  if (!exists("umbral_respuesta", mode = "function")) {
    res$status <- 503
    return(list(error = "Alertas personalizadas no disponibles"))
  }
  umbral_respuesta(req$user$id)
}

#* Crear una regla de umbral
#* @post /me/alertas/umbrales
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }
  out <- crear_umbral(req$user$id, get_tipo_usuario(req), body)
  if (!isTRUE(out$ok)) {
    res$status <- if (identical(out$error, "Alertas personalizadas no disponibles")) 503 else 403
    return(list(error = out$error))
  }
  out
}

#* Historico de la estacion favorita, para la barra del constructor
#* @get /me/alertas/referencia-umbral
function(req, res, abr = "pp", ventana = "24") {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  if (!exists("umbral_referencia_estacion", mode = "function")) {
    res$status <- 503
    return(list(error = "Alertas personalizadas no disponibles"))
  }
  umbral_referencia_estacion(req$user$id, abr, ventana)
}

#* Actualizar una regla de umbral
#* @put /me/alertas/umbrales/<id>
function(req, res, id) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }
  out <- actualizar_umbral(req$user$id, get_tipo_usuario(req), id, body)
  if (!isTRUE(out$ok)) {
    res$status <- if (identical(out$error, "Regla no encontrada")) 404 else 403
    return(list(error = out$error))
  }
  out
}

#* Eliminar una regla de umbral
#* @delete /me/alertas/umbrales/<id>
function(req, res, id) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  out <- eliminar_umbral(req$user$id, id)
  if (!isTRUE(out$ok)) {
    res$status <- if (identical(out$error, "Regla no encontrada")) 404 else 503
    return(list(error = out$error %||% "Regla no encontrada"))
  }
  out
}

#* Historial de alertas. estado=todas|previstas|confirmadas
#* @get /me/alertas/eventos
function(req, res, limit = "5", offset = "0", estado = "") {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  n <- suppressWarnings(as.integer(limit))
  off <- suppressWarnings(as.integer(offset))
  alerta_resumen_pagina(req$user$id, estado, n, off)
}

#* Historial de la vista activa en CSV (cuenta en cuota de descarga)
#* @get /me/alertas/eventos.csv
#* @serializer contentType list(type="text/csv; charset=utf-8")
function(req, res, estado = "") {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) {
    res$status <- 401
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(auth_err, auto_unbox = TRUE)))
  }
  if (!alertas_tabla_existe()) {
    res$status <- 503
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(list(error = "Sistema de alertas no disponible"), auto_unbox = TRUE)))
  }

  user_id <- req$user$id
  tier <- get_tipo_usuario(req)
  n <- alerta_resumen_contar(user_id, estado)
  if (n < 1L) {
    res$status <- 404
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    return(charToRaw(jsonlite::toJSON(list(error = "No hay alertas en el historial"), auto_unbox = TRUE)))
  }

  bytes_est <- alertas_historial_csv_estimar_bytes(n)
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

  tryCatch({
    raw <- generar_eventos_usuario_csv(user_id, estado)
    real_bytes <- length(raw)
    ajustar_cuota_post_job(
      user_id, tier, bytes_est, real_bytes,
      overage_est = reserve$overage_bytes %||% 0L,
      extra_est = reserve$extra_bytes %||% 0L
    )
    res$setHeader("Content-Disposition", descarga_content_disposition("rodaja_alertas_historial.csv"))
    res$setHeader("Cache-Control", "private, no-store")
    raw
  }, error = function(e) {
    liberar_bytes_cuota(user_id, tier, bytes_est, reserve$overage_bytes %||% 0L, reserve$extra_bytes %||% 0L)
    res$status <- 500
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    charToRaw(jsonlite::toJSON(list(error = "No se pudo generar el CSV"), auto_unbox = TRUE))
  })
}

#* Variables disponibles para alertas
#* @get /me/alertas/variables
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  list(variables = listar_variables_alertas())
}

#* Enviar correo de prueba de alerta (maximo 1 por dia)
#* @post /me/alertas/probar
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  u <- req$user
  out <- enviar_prueba_alerta_usuario(u$id, u$email)
  if (!isTRUE(out$ok)) {
    res$status <- if (grepl("Ya enviaste", out$error %||% "")) 429 else 400
    return(list(error = out$error))
  }
  list(ok = TRUE, email = out$email, prueba_enviada_hoy = TRUE)
}

correo_rebote_emails_usuario <- function(u, prefs = NULL) {
  if (is.null(prefs)) {
    prefs <- tryCatch(get_preferencias_usuario(u$id, u$email), error = function(e) list())
  }
  unique(c(
    as.character(u$email %||% ""),
    as.character(prefs$email_efectivo %||% ""),
    as.character(prefs$email_alternativo %||% "")
  ))
}

correo_rebote_payload_usuario <- function(u, prefs = NULL) {
  if (!exists("correo_rebote_estado_emails", mode = "function")) {
    return(list(bloqueado = FALSE, email = NULL, rebotes = 0L, ultimo_codigo = NULL))
  }
  correo_rebote_estado_emails(correo_rebote_emails_usuario(u, prefs))
}

#* Reactivar avisos por correo tras resolver un rebote (buzon lleno, etc.)
#* @post /me/alertas/correo-rebote/reactivar
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  u <- req$user
  prefs <- get_preferencias_usuario(u$id, u$email)
  emails <- correo_rebote_emails_usuario(u, prefs)
  if (!exists("correo_rebote_reactivar", mode = "function")) {
    res$status <- 503
    return(list(error = "Sistema de rebotes no disponible"))
  }
  out <- correo_rebote_reactivar(emails)
  if (!isTRUE(out$ok)) {
    res$status <- 400
    return(list(error = out$error %||% "No se pudo reactivar"))
  }
  list(ok = TRUE, carta_devuelta = correo_rebote_payload_usuario(u, prefs))
}

#* Alertas tempranas activas (pronostico vs top 10) en alcance del usuario
#* @get /me/alertas/temprana
function(req, res, eid = "", variable = "", sort = "ranking", top3 = "0", limit = "100") {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  if (!exists("listar_alertas_temprana_usuario", mode = "function")) {
    source(file.path("/app", "utils", "alerta_temprana.R"), local = FALSE)
  }
  listar_alertas_temprana_usuario(
    req$user$id,
    eid = if (nzchar(eid)) eid else NULL,
    variable_abr = if (nzchar(variable)) variable else NULL,
    sort = sort,
    top3 = parse_bool(top3),
    limit = limit
  )
}

#* Toggle alerta tardia (post-evento, dato observado)
#* @put /me/alertas/tardia
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }
  activa <- if (is.null(body$activa)) TRUE else parse_bool(body$activa)
  out <- upsert_alerta_tardia_usuario(req$user$id, activa)
  if (!isTRUE(out$ok)) {
    res$status <- 400
    return(list(error = out$error))
  }
  list(ok = TRUE, alerta_tardia = out$alerta_tardia)
}


#* Resumen diario de alertas (cola de hoy o ultimo correo enviado)
#* @get /me/alertas/digest
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  app_url <- Sys.getenv("APP_URL", "https://app.rodaja.cl")
  listar_digest_usuario(req$user$id, app_url)
}

#* Iconos de alerta para el mapa del monitor (solo estaciones con alerta activa).
#* Payload minimo: id, lat, lon, severidad, variable, direccion, ranking.
#* Cold path (include_ext != 1): solo horizonte corto. Ext bajo demanda.
#* @param id_fuente Filtro operador (opcional)
#* @param id_variable Filtro variable (opcional)
#* @param include_ext "1" para incluir alertas_ext (horizonte largo)
#* @serializer unboxedJSON
#* @get /alertas/mapa
function(id_fuente = "", id_variable = "", include_ext = "", req, res) {
  if (!alerta_temprana_tabla_existe()) {
    return(list(alertas = list(), alertas_ext = list()))
  }
  want_ext <- identical(as.character(include_ext %||% ""), "1")
  key <- cache_key(
    "alertas_mapa", id_fuente, id_variable,
    alerta_mapa_horizonte_corto(),
    if (want_ext) alerta_mapa_horizonte_largo() else 0L,
    alerta_temprana_min_anios(), alerta_temprana_nivel_minimo(),
    if (want_ext) 8L else 7L
  )
  raw <- cache_or_compute(key, function() {
    alertas_mapa_compute(id_fuente, id_variable, include_ext = want_ext)
  }, ttl = 180, res = res)
  list(
    alertas = geo_rows_unbox(raw$alertas %||% list()),
    alertas_ext = geo_rows_unbox(raw$alertas_ext %||% list())
  )
}

#* Iconos del anticipado nuevo. No cambia /alertas/mapa.
#* @serializer unboxedJSON
#* @get /alertas/anticipada-mapa
function(req, res) {
  tiene <- tryCatch(db_query("
    SELECT 1 FROM information_schema.tables
    WHERE table_name = 'umbral_antes_del_dia' LIMIT 1"), error = function(e) NULL)
  if (is.null(tiene) || nrow(tiene) == 0) return(list(alertas = list()))
  df <- tryCatch(db_query("
    SELECT s.id_estacion::int AS id, e.lat, e.lon,
           CASE
             WHEN s.criterio LIKE 'tmax%' THEN 't_max'
             WHEN s.criterio LIKE 'tmin%' THEN 't_min'
             ELSE 'pp'
           END AS variable,
           CASE WHEN s.criterio LIKE 'tmin%' THEN 'min' ELSE 'max' END AS direccion,
           CASE WHEN s.nivel = 'posibilidad' THEN 'posibilidad' ELSE 'critica' END AS severidad,
           CASE WHEN s.nivel = 'posibilidad' THEN 50 ELSE 1 END AS ranking,
           s.nivel
    FROM umbral_antes_del_dia s
    JOIN mirador e ON e.id = s.id_estacion
    WHERE s.unidad = 'estacion' AND s.sale
      AND s.nivel IN ('aviso', 'posibilidad')
      AND s.fecha_fin >= CURRENT_DATE
      AND s.fecha_inicio <= CURRENT_DATE + 3
      AND e.lat BETWEEN -56 AND -17 AND e.lon BETWEEN -76 AND -66"), error = function(e) NULL)
  if (is.null(df) || !nrow(df)) return(list(alertas = list()))
  list(alertas = lapply(seq_len(nrow(df)), function(i) {
    list(
      id = as.integer(df$id[i]), lat = as.numeric(df$lat[i]), lon = as.numeric(df$lon[i]),
      variable = as.character(df$variable[i]), direccion = as.character(df$direccion[i]),
      severidad = as.character(df$severidad[i]),
      ranking = as.integer(df$ranking[i]), nivel = as.character(df$nivel[i])
    )
  }))
}

alertas_mapa_fetch_horizonte <- function(horizonte_dias, id_fuente, id_variable) {
  h <- as.integer(horizonte_dias)
  if (is.na(h) || h < 1L) h <- 5L
  has_var <- nzchar(id_variable) && !is.na(suppressWarnings(as.integer(id_variable)))
  fid_present <- nzchar(id_fuente) && id_fuente != "" && id_fuente != "0" &&
    !is.na(suppressWarnings(as.integer(id_fuente)))

  where_e <- paste0(
    " e.lat BETWEEN -56 AND -17 AND e.lon BETWEEN -76 AND -66"
  )
  params <- list()
  if (fid_present) {
    where_e <- paste0(where_e, " AND e.id_fuente = $", length(params) + 1L)
    params <- c(params, as.integer(id_fuente))
  }
  if (has_var) {
    vids <- estaciones_expand_id_variable(id_variable)
    where_e <- paste0(where_e, estaciones_exists_variable_sql(vids, "e.id"))
  }

  q <- paste0(
    "SELECT DISTINCT ON (a.id_estacion, a.id_variable, COALESCE(a.direccion, ''), a.fecha_objetivo)",
    " a.id_estacion AS id, e.lat, e.lon, v.nombre_abr AS variable,",
    " a.ranking, a.direccion, a.fecha_objetivo",
    " FROM aviso_de_madrugada a",
    " JOIN mirador e ON e.id = a.id_estacion",
    " JOIN magnitud v ON v.id = a.id_variable",
    " WHERE ", where_e,
    " AND a.fecha_objetivo >= CURRENT_DATE",
    " AND a.fecha_objetivo <= CURRENT_DATE + ", h - 1L,
    " AND ", alerta_temprana_alerta_valida_sql("a"),
    " AND ", alerta_temprana_historia_clause("a.id_estacion"),
    " ORDER BY a.id_estacion, a.id_variable, COALESCE(a.direccion, ''),",
    " a.fecha_objetivo, a.ranking ASC"
  )
  df <- tryCatch(db_query(q, params = params), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0L) return(list())
  lapply(seq_len(nrow(df)), function(i) {
    rank <- as.integer(df$ranking[i])
    list(
      id = as.integer(df$id[i]),
      lat = as.numeric(df$lat[i]),
      lon = as.numeric(df$lon[i]),
      variable = as.character(df$variable[i] %||% ""),
      direccion = as.character(df$direccion[i] %||% ""),
      ranking = rank,
      severidad = alerta_temprana_severidad(rank),
      fecha_objetivo = as.character(as.Date(df$fecha_objetivo[i]))
    )
  })
}

alertas_mapa_compute <- function(id_fuente, id_variable, include_ext = FALSE) {
  list(
    alertas = alertas_mapa_fetch_horizonte(alerta_mapa_horizonte_corto(), id_fuente, id_variable),
    alertas_ext = if (isTRUE(include_ext)) {
      alertas_mapa_fetch_horizonte(alerta_mapa_horizonte_largo(), id_fuente, id_variable)
    } else {
      list()
    }
  )
}

alertas_conteos_clamp_dias <- function(dias) {
  d <- suppressWarnings(as.integer(dias)[1])
  if (length(d) != 1L || is.na(d) || d < 1L) 2L else as.integer(min(d, 3L))
}

alertas_conteos_empty <- function(dias) {
  d <- alertas_conteos_clamp_dias(dias)
  list(
    horizonte_dias = d,
    incluye_shadow = identical(d, 3L),
    total_alertas = 0L,
    total_estaciones = 0L,
    macrozonas = list(),
    regiones = list(),
    comunas = list(),
    puntos = list()
  )
}

#* Conteos de alertas tempranas por macrozona / region / comuna (mapa coropletico).
#* dias=1 hoy, 2=48h emit, 3=72h incluye shadow. No relaja el predicado de correo.
#* @param dias Horizonte en dias calendario (1, 2 o 3)
#* @serializer unboxedJSON
#* @get /alertas/conteos
function(dias = "2", req, res) {
  d <- alertas_conteos_clamp_dias(dias)
  if (!alerta_temprana_tabla_existe()) {
    return(alertas_conteos_empty(d))
  }
  key <- cache_key(
    "alertas_conteos", d,
    alerta_temprana_min_dias(),
    alerta_emit_rank_max(),
    as.integer(isTRUE(alerta_pause_t_min_min()))
  )
  raw <- cache_or_compute(key, function() {
    alertas_conteos_compute(d)
  }, ttl = 180, res = res)
  list(
    horizonte_dias = as.integer(raw$horizonte_dias %||% d),
    incluye_shadow = isTRUE(raw$incluye_shadow),
    total_alertas = as.integer(raw$total_alertas %||% 0L),
    total_estaciones = as.integer(raw$total_estaciones %||% 0L),
    macrozonas = geo_rows_unbox(raw$macrozonas %||% list()),
    regiones = geo_rows_unbox(raw$regiones %||% list()),
    comunas = geo_rows_unbox(raw$comunas %||% list()),
    puntos = geo_rows_unbox(raw$puntos %||% list())
  )
}

alertas_conteos_mz_meta <- function() {
  if (exists("geo_conteos_mz_meta", mode = "function")) {
    return(geo_conteos_mz_meta())
  }
  list(
    list(id = "norte", etiqueta = "Macrozona Norte", orden = 1L, cod_regs = c(1L, 2L, 15L)),
    list(id = "centro_norte", etiqueta = "Macrozona Centro Norte", orden = 2L, cod_regs = c(3L, 4L)),
    list(id = "centro", etiqueta = "Macrozona Centro", orden = 3L, cod_regs = c(5L, 6L, 7L, 13L)),
    list(id = "sur", etiqueta = "Macrozona Sur", orden = 4L, cod_regs = c(8L, 9L, 10L, 14L, 16L)),
    list(id = "austral", etiqueta = "Macrozona Austral", orden = 5L, cod_regs = c(11L, 12L))
  )
}

alertas_conteos_mz_id <- function(cod_reg) {
  if (exists("geo_conteos_mz_id", mode = "function")) {
    return(geo_conteos_mz_id(cod_reg))
  }
  cr <- suppressWarnings(as.integer(cod_reg))
  if (length(cr) == 0L || is.na(cr) || cr <= 0L) return("sin_zona")
  for (mz in alertas_conteos_mz_meta()) {
    if (cr %in% mz$cod_regs) return(mz$id)
  }
  "sin_zona"
}

alertas_conteos_compute <- function(dias) {
  d <- alertas_conteos_clamp_dias(dias)
  include_shadow <- identical(d, 3L)
  lead_max <- d - 1L
  valida <- alerta_temprana_alerta_valida_sql(
    "a", include_shadow = include_shadow, lead_max = lead_max
  )
  hist <- alerta_temprana_historia_clause("a.id_estacion")
  where_a <- paste0(
    " a.fecha_objetivo >= CURRENT_DATE",
    " AND a.fecha_objetivo <= CURRENT_DATE + ", lead_max,
    " AND ", valida,
    " AND ", hist
  )
  where_e <- paste0(
    " e.lat BETWEEN -56 AND -17 AND e.lon BETWEEN -76 AND -66",
    " AND e.cod_reg IS NOT NULL",
    " AND TRIM(COALESCE(e.nom_com, '')) <> ''"
  )

  q <- paste0(
    "SELECT e.cod_reg, MAX(TRIM(COALESCE(e.nom_reg, ''))) AS nom_reg,",
    " TRIM(COALESCE(e.nom_com, '')) AS nom_com,",
    " COUNT(*)::int AS n_alertas,",
    " COUNT(DISTINCT a.id_estacion)::int AS n_estaciones",
    " FROM aviso_de_madrugada a",
    " JOIN mirador e ON e.id = a.id_estacion",
    " WHERE ", where_e, " AND ", where_a,
    " GROUP BY e.cod_reg, TRIM(COALESCE(e.nom_com, ''))",
    " ORDER BY e.cod_reg, nom_com"
  )
  df <- tryCatch(db_query(q), error = function(e) NULL)
  empty <- alertas_conteos_empty(d)
  if (is.null(df) || nrow(df) == 0L) return(empty)

  sum_i <- function(x) as.integer(sum(x, na.rm = TRUE))
  comunas <- lapply(seq_len(nrow(df)), function(i) {
    list(
      nom_com = as.character(df$nom_com[i]),
      cod_reg = as.integer(df$cod_reg[i]),
      n_alertas = as.integer(df$n_alertas[i] %||% 0L),
      n_estaciones = as.integer(df$n_estaciones[i] %||% 0L)
    )
  })

  reg_ids <- unique(as.integer(df$cod_reg))
  regiones <- lapply(reg_ids, function(cr) {
    sub <- df[as.integer(df$cod_reg) == cr, , drop = FALSE]
    list(
      cod_reg = cr,
      nom_reg = as.character(sub$nom_reg[1] %||% paste0("Region ", cr)),
      macrozona_id = alertas_conteos_mz_id(cr),
      n_alertas = sum_i(sub$n_alertas),
      n_estaciones = sum_i(sub$n_estaciones)
    )
  })

  macrozonas <- lapply(alertas_conteos_mz_meta(), function(mz) {
    regs <- Filter(function(r) identical(r$macrozona_id, mz$id), regiones)
    sum_field <- function(f) {
      as.integer(sum(vapply(regs, function(r) as.integer(r[[f]] %||% 0L), integer(1)), na.rm = TRUE))
    }
    list(
      id = mz$id,
      etiqueta = mz$etiqueta,
      n_alertas = sum_field("n_alertas"),
      n_estaciones = sum_field("n_estaciones")
    )
  })

  qp <- paste0(
    "SELECT DISTINCT ON (a.id_estacion)",
    " a.id_estacion AS id, e.lat, e.lon, e.nombre_publico AS nombre,",
    " TRIM(COALESCE(e.nom_com, '')) AS nom_com, e.cod_reg,",
    " v.nombre_abr AS variable, a.ranking, a.direccion,",
    " COALESCE(a.capa, 'emit') AS capa",
    " FROM aviso_de_madrugada a",
    " JOIN mirador e ON e.id = a.id_estacion",
    " JOIN magnitud v ON v.id = a.id_variable",
    " WHERE ", where_e, " AND ", where_a,
    " ORDER BY a.id_estacion, a.ranking ASC, a.fecha_objetivo ASC"
  )
  pts <- tryCatch(db_query(qp), error = function(e) NULL)
  puntos <- if (is.null(pts) || nrow(pts) == 0L) {
    list()
  } else {
    lapply(seq_len(nrow(pts)), function(i) {
      rank <- as.integer(pts$ranking[i])
      list(
        id = as.integer(pts$id[i]),
        lat = as.numeric(pts$lat[i]),
        lon = as.numeric(pts$lon[i]),
        nombre = as.character(pts$nombre[i] %||% ""),
        nom_com = as.character(pts$nom_com[i] %||% ""),
        cod_reg = as.integer(pts$cod_reg[i]),
        variable = as.character(pts$variable[i] %||% ""),
        direccion = as.character(pts$direccion[i] %||% ""),
        ranking = rank,
        capa = as.character(pts$capa[i] %||% "emit"),
        severidad = alerta_temprana_severidad(rank)
      )
    })
  }

  list(
    horizonte_dias = d,
    incluye_shadow = include_shadow,
    total_alertas = sum_i(df$n_alertas),
    total_estaciones = as.integer(sum(vapply(regiones, function(r) {
      as.integer(r$n_estaciones %||% 0L)
    }, integer(1)), na.rm = TRUE)),
    macrozonas = macrozonas,
    regiones = regiones,
    comunas = comunas,
    puntos = puntos
  )
}