source(file.path("/app", "utils", "widget_pronostico.R"), local = FALSE)

#* Catalogo de estaciones con pronostico de hoy (widget). Sin login.
#* @serializer contentType list(type = "application/json")
#* @get /widget/estaciones
function(req, res) {
  key <- cache_key("widget:estaciones", "v1")
  out <- cache_or_compute(
    key, widget_catalog_build, ttl = 3600L, res = res, encode = pronostico_json_encode
  )
  tryCatch(res$setHeader("Cache-Control", "public, max-age=3600"), error = function(e) NULL)
  out
}

#* Pronostico de hoy de una estacion (widget). Sin rankings ni coordenadas.
#* @param id ID de estacion
#* @serializer contentType list(type = "application/json")
#* @get /widget/estaciones/<id>/hoy
function(id, req, res) {
  eid <- widget_parse_id(id)
  if (is.null(eid)) {
    res$status <- 400
    tryCatch(res$setHeader("Cache-Control", "no-store"), error = function(e) NULL)
    return(pronostico_json_encode(list(error = "id inv\u00e1lido")))
  }
  key <- cache_key("widget:hoy", "v2", eid)
  out <- cache_or_compute(
    key, function() widget_hoy_build(eid), ttl = 600L, res = res, encode = pronostico_json_encode
  )
  if (is.list(out) && !is.null(out$error)) {
    res$status <- 404
    tryCatch(res$setHeader("Cache-Control", "no-store"), error = function(e) NULL)
    return(pronostico_json_encode(out))
  }
  if (!is.character(out) || length(out) != 1L) {
    res$status <- 404
    tryCatch(res$setHeader("Cache-Control", "no-store"), error = function(e) NULL)
    return(pronostico_json_encode(list(error = "Sin pron\u00f3stico para esta estaci\u00f3n")))
  }
  tryCatch(res$setHeader("Cache-Control", "public, max-age=600"), error = function(e) NULL)
  out
}
