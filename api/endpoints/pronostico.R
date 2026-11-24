#* Pronostico por estacion (lectura SQL; horizonte segun OPENMETEO_FORECAST_DAYS)

#* @param id ID estacion
#* @serializer contentType list(type="application/json")
#* @get /estaciones/<id>/pronostico
function(id, req, res) {
  eid <- as.integer(id)
  if (is.na(eid) || eid < 1L) {
    res$status <- 400
    return(pronostico_json_encode(list(error = "id inv\u00e1lido")))
  }
  if (!exists("pronostico_build_payload", mode = "function")) {
    source(file.path("/app", "utils", "pronostico_payload.R"), local = FALSE)
  }
  key <- cache_key("pronostico", eid)
  out <- cache_or_compute(key, function() {
    pronostico_build_payload(eid)
  }, ttl = 3600, res = res)

  if (is.list(out) && !is.null(out$error)) {
    if (grepl("no encontrada", out$error, ignore.case = TRUE)) {
      res$status <- 404
    } else if (grepl("excelente|buena", out$error)) {
      res$status <- 422
    } else if (grepl("Sin pronostico", out$error)) {
      res$status <- 404
    } else if (is.null(res$status) || res$status == 200) {
      res$status <- 400
    }
    return(pronostico_json_encode(out))
  }
  pronostico_json_encode(out)
}

#* Pronostico de la estacion favorita del usuario
#* @serializer contentType list(type="application/json")
#* @get /me/pronostico
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  if (!exists("pronostico_build_payload", mode = "function")) {
    source(file.path("/app", "utils", "pronostico_payload.R"), local = FALSE)
  }
  eid <- pronostico_favorita_usuario(req$user$id)
  if (is.null(eid)) {
    res$status <- 404
    return(pronostico_json_encode(list(error = "Debes fijar una estaci\u00f3n favorita")))
  }
  key <- cache_key("pronostico", eid)
  out <- cache_or_compute(key, function() {
    pronostico_build_payload(eid)
  }, ttl = 3600, res = res)
  if (is.list(out) && !is.null(out$error)) {
    res$status <- if (grepl("excelente|buena", out$error)) 422 else 404
    return(pronostico_json_encode(out))
  }
  pronostico_json_encode(out)
}

#* Estado suscripcion alertas tempranas por pronostico
#* @get /me/alertas/pronostico
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  if (!pronostico_alertas_tabla_existe()) {
    return(list(activa = FALSE, disponible = FALSE))
  }
  df <- db_query(
    "SELECT activa FROM pedido_temprano WHERE user_id = $1::uuid",
    params = list(req$user$id)
  )
  activa <- nrow(df) > 0 && isTRUE(df$activa[1])
  eid <- pronostico_favorita_usuario(req$user$id)
  list(
    activa = activa,
    disponible = TRUE,
    favorita_configurada = !is.null(eid),
    estacion_elegible = if (is.null(eid)) FALSE else pronostico_estacion_elegible(eid)
  )
}

#* Activar/desactivar alertas tempranas por pronostico (favorita)
#* @put /me/alertas/pronostico
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  if (!pronostico_alertas_tabla_existe()) {
    res$status <- 503
    return(list(error = "Alertas de pron\u00f3stico no disponibles"))
  }
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }
  activa <- if (is.null(body$activa)) TRUE else isTRUE(body$activa)
  u <- req$user
  if (isTRUE(activa)) {
    eid <- pronostico_favorita_usuario(u$id)
    if (is.null(eid)) {
      res$status <- 403
      return(list(error = "Debes fijar una estaci\u00f3n favorita"))
    }
    if (!pronostico_estacion_elegible(eid)) {
      res$status <- 422
      return(list(error = "Tu estaci\u00f3n favorita no tiene calidad excelente o buena"))
    }
  }
  db_execute(
    "INSERT INTO pedido_temprano (user_id, activa, updated_at)
     VALUES ($1::uuid, $2, NOW())
     ON CONFLICT (user_id) DO UPDATE SET activa = EXCLUDED.activa, updated_at = NOW()",
    params = list(u$id, activa)
  )
  list(ok = TRUE, activa = activa)
}

pronostico_alertas_tabla_existe <- function() {
  chk <- tryCatch(
    db_query("SELECT 1 FROM information_schema.tables WHERE table_name = 'pedido_temprano' LIMIT 1"),
    error = function(e) NULL
  )
  !is.null(chk) && nrow(chk) > 0
}

#* Puntos horarios Open-Meteo para el mapa (misma hora UTC que el paso ICON)
#* @param t Timestamp ISO-8601 UTC del slider ICON
#* @serializer contentType list(type="application/json")
#* @get /pronostico/mapa
function(t = "", req, res) {
  if (!exists("pronostico_mapa_build", mode = "function")) {
    source(file.path("/app", "utils", "pronostico_payload.R"), local = FALSE)
  }
  hour <- pronostico_mapa_hour_utc(pronostico_mapa_parse_ts(t))
  if (is.na(hour)) {
    res$status <- 400
    return(pronostico_json_encode(list(error = "t inv\u00e1lido")))
  }
  iso <- pronostico_mapa_iso_utc(hour)
  key <- cache_key("pronostico:mapa", iso)
  out <- cache_or_compute(key, function() {
    pronostico_mapa_build(iso)
  }, ttl = 3600, res = res, encode = pronostico_json_encode)
  if (is.null(out)) {
    res$status <- 400
    return(pronostico_json_encode(list(error = "t inv\u00e1lido")))
  }
  tryCatch(res$setHeader("Cache-Control", "public, max-age=120, stale-while-revalidate=600"), error = function(e) NULL)
  out
}
