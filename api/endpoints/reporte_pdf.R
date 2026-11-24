#* Status del reporte PDF pre-generado por estacion
#* @param id ID estacion
#* @serializer contentType list(type="application/json")
#* @get /estaciones/<id>/reporte/status
function(id, req, res) {
  if (!exists("reporte_pdf_offer", mode = "function")) {
    source(file.path("/app", "utils", "reporte_pdf_offer.R"), local = FALSE)
  }
  offer <- reporte_pdf_offer(id)
  if (!isTRUE(offer$ok)) {
    res$status <- offer$http %||% 400L
    return(list(error = offer$error %||% "id inv\u00e1lido"))
  }
  jsonlite::toJSON(list(
    disponible = offer$disponible,
    estado = offer$estado,
    elegible = offer$elegible,
    solicitable = offer$solicitable,
    ultima_actualizacion = offer$ultima_actualizacion,
    congelado_desde = offer$congelado_desde,
    pdf_url = offer$pdf_url,
    mensaje = offer$mensaje
  ), auto_unbox = TRUE, null = "null")
}

#* Descarga reporte PDF pre-generado. Exige cuenta y reserva el tamano del archivo.
#* @param id ID estacion
#* @serializer contentType list(type="application/pdf")
#* @get /estaciones/<id>/reporte.pdf
function(id, req, res) {
  pdf_json <- function(status, payload) {
    res$status <- status
    res$setHeader("Content-Type", "application/json; charset=utf-8")
    charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE))
  }

  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(pdf_json(401L, auth_err))

  if (!exists("reporte_pdf_offer", mode = "function")) {
    source(file.path("/app", "utils", "reporte_pdf_offer.R"), local = FALSE)
  }
  offer <- reporte_pdf_offer(id)
  if (!isTRUE(offer$ok)) {
    return(pdf_json(offer$http %||% 400L, list(error = offer$error %||% "id inv\u00e1lido")))
  }
  if (!isTRUE(offer$disponible)) {
    return(pdf_json(offer$http %||% 403L, list(error = offer$error %||% offer$mensaje)))
  }

  bytes_est <- suppressWarnings(as.integer(file.info(offer$pdf_path)$size))
  if (is.na(bytes_est) || bytes_est < 1L) {
    return(pdf_json(404L, list(error = "Reporte no disponible")))
  }

  user_id <- req$user$id
  tier <- get_tipo_usuario(req)
  chk <- descarga_excede_cuota(user_id, tier, bytes_est)
  if (isTRUE(chk$excede)) {
    return(pdf_json(429L, descarga_respuesta_cuota_excedida(user_id, tier, bytes_est)))
  }

  reserve <- reservar_bytes_descarga(user_id, tier, bytes_est)
  if (!isTRUE(reserve$ok)) {
    return(pdf_json(
      429L,
      descarga_respuesta_cuota_excedida(user_id, tier, bytes_est, reserve$code %||% "cuota_excedida")
    ))
  }

  tryCatch({
    raw <- readBin(offer$pdf_path, "raw", bytes_est)
    real_bytes <- length(raw)
    ajustar_cuota_post_job(
      user_id, tier, bytes_est, real_bytes,
      overage_est = reserve$overage_bytes %||% 0L,
      extra_est = reserve$extra_bytes %||% 0L
    )
    fname <- sprintf("rodaja_reporte_%s.pdf", offer$eid)
    res$setHeader("Content-Disposition", descarga_content_disposition(fname))
    res$setHeader("Cache-Control", "private, no-store")
    res$setHeader("X-Reporte-Estado", offer$estado)
    if (!is.null(offer$ultima_actualizacion)) {
      res$setHeader("Last-Modified", format(as.POSIXct(offer$ultima_actualizacion, tz = "UTC"),
                                            "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT"))
    }
    raw
  }, error = function(e) {
    liberar_bytes_cuota(user_id, tier, bytes_est, reserve$overage_bytes %||% 0L, reserve$extra_bytes %||% 0L)
    pdf_json(500L, list(error = "No se pudo leer el reporte"))
  })
}
