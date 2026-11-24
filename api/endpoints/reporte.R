reporte_endpoint_load_utils <- function() {
  if (!exists("reporte_api_build_payload", mode = "function")) {
    source(file.path("/app", "utils", "reporte_payload.R"), local = FALSE)
  }
  if (!exists("pronostico_json_encode", mode = "function")) {
    source(file.path("/app", "utils", "pronostico_payload.R"), local = FALSE)
  }
}

reporte_endpoint_reply <- function(out, res, shell = FALSE) {
  if (is.list(out) && !is.null(out$error)) {
    if (is.null(res$status) || res$status == 200) {
      res$status <- if (grepl("no encontrada", out$error, ignore.case = TRUE)) 404 else 400
    }
    return(pronostico_json_encode(out))
  }
  # Path separado para shell: CF a veces reusa el HIT del full si solo cambia
  # el query string. Cache-Tag distinto permite purgar ambos.
  tryCatch({
    res$setHeader("Cache-Control", reporte_cache_control_public())
    res$setHeader("Cache-Tag", if (isTRUE(shell)) "reporte-shell" else "reporte")
  }, error = function(e) NULL)
  out
}

#* Payload JSON del reporte climatico (compartido con Shiny y PWA)
#* @param id ID estacion
#* @param id_variable ID variable (requerido)
#* @serializer contentType list(type="application/json")
#* @get /estaciones/<id>/reporte
function(id, id_variable = "", req, res) {
  eid <- as.integer(id)
  vid <- suppressWarnings(as.integer(id_variable))
  if (is.na(eid) || eid < 1L) {
    res$status <- 400
    return(pronostico_json_encode(list(error = "id inv\u00e1lido")))
  }
  if (is.na(vid) || vid < 1L) {
    res$status <- 400
    return(pronostico_json_encode(list(error = "id_variable requerido")))
  }
  reporte_endpoint_load_utils()
  reporte_endpoint_reply(reporte_payload_cached(eid, vid, res = res), res, shell = FALSE)
}

#* Shell liviano del reporte (header + pronostico + ultimas). Path propio por CF.
#* @param id ID estacion
#* @param id_variable ID variable (requerido)
#* @serializer contentType list(type="application/json")
#* @get /estaciones/<id>/reporte/shell
function(id, id_variable = "", req, res) {
  eid <- as.integer(id)
  vid <- suppressWarnings(as.integer(id_variable))
  if (is.na(eid) || eid < 1L) {
    res$status <- 400
    return(pronostico_json_encode(list(error = "id inv\u00e1lido")))
  }
  if (is.na(vid) || vid < 1L) {
    res$status <- 400
    return(pronostico_json_encode(list(error = "id_variable requerido")))
  }
  reporte_endpoint_load_utils()
  reporte_endpoint_reply(reporte_payload_shell_cached(eid, vid, res = res), res, shell = TRUE)
}
