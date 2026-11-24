# config.R - Configuracion del dashboard (cambiar aqui para rediseno)
# Cargar primero desde global.R

# Tema activo: nombre del archivo CSS en www/themes/ (sin extension)
# Ej: "default", "claro", "oscuro", "moderno"
APP_THEME <- "default"

# Titulo del dashboard
APP_TITLE <- "Explorador Climatico"

# Bump al cambiar CSS/JS estaticos (evita cache del browser en assets del reporte)
APP_ASSET_VERSION <- "20261116skel1"

# Reporte modal: maximo de pestanas abiertas
EC_MAX_TABS_REPORTE <- 5L

# Deep links (query params; base path autodetectado en JS, ver EC_EXPLORADOR_BASE_PATH)
EC_EXPLORADOR_BASE_PATH <- Sys.getenv("EC_EXPLORADOR_BASE_PATH", unset = "")
EC_URL_PARAM_ESTACION <- "estacion"
EC_URL_PARAM_VARIABLE <- "variable"
EC_URL_PARAM_OPERADOR <- "operador"
EC_URL_PARAM_REPORTE  <- "reporte"

# Favoritos (localStorage compartido con /mi-reporte/)
EC_FAVORITES_STORAGE_KEY <- "rodaja_favorites_v1"
EC_MI_REPORTE_PATH <- "/mi-reporte/"

# Primera visita: precipitacion. Luego cookie de ultima variable del mapa.
# Cookie tecnica (necesaria); JS la lee en el boot fetch. id=1 = pp en catalogo.
EC_DEFAULT_VARIABLE_ID <- 1L
EC_VARIABLE_COOKIE <- "rodaja_variable"

ec_cookie_value <- function(header, name) {
  if (is.null(header) || !nzchar(as.character(header[1]))) return(NA_character_)
  parts <- strsplit(as.character(header[1]), ";", fixed = TRUE)[[1]]
  for (p in parts) {
    kv <- strsplit(trimws(p), "=", fixed = TRUE)[[1]]
    if (length(kv) >= 1L && identical(kv[[1]], name)) {
      return(if (length(kv) >= 2L) kv[[2]] else "")
    }
  }
  NA_character_
}

ec_query_param <- function(qs, name) {
  if (is.null(qs) || !nzchar(as.character(qs[1]))) return(NA_character_)
  q <- sub("^\\?", "", as.character(qs[1]))
  parts <- strsplit(q, "&", fixed = TRUE)[[1]]
  for (p in parts) {
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    key <- tryCatch(utils::URLdecode(kv[[1]]), error = function(e) kv[[1]])
    if (identical(key, name)) {
      raw <- if (length(kv) >= 2L) kv[[2]] else ""
      return(tryCatch(utils::URLdecode(raw), error = function(e) raw))
    }
  }
  NA_character_
}

ec_variable_id_from_cookie <- function(header, default = EC_DEFAULT_VARIABLE_ID) {
  vid <- suppressWarnings(as.integer(ec_cookie_value(header, EC_VARIABLE_COOKIE)))
  if (length(vid) == 1L && !is.na(vid) && vid > 0L) return(as.character(vid))
  as.character(as.integer(default))
}

# Prioridad: ?variable= > cookie > precipitacion.
ec_initial_variable_id <- function(cookie_header, query_string,
                                   default = EC_DEFAULT_VARIABLE_ID) {
  url_vid <- suppressWarnings(as.integer(ec_query_param(query_string, EC_URL_PARAM_VARIABLE)))
  if (length(url_vid) == 1L && !is.na(url_vid) && url_vid > 0L) return(as.character(url_vid))
  ec_variable_id_from_cookie(cookie_header, default)
}

# Payload del boot JS solo si coincide con el filtro actual (sin operador).
ec_boot_use_client_payload <- function(id_fuente, id_variable, client_raw, boot_var) {
  if (!is.null(id_fuente) && !is.na(id_fuente) && nzchar(as.character(id_fuente))) {
    return(FALSE)
  }
  if (is.null(client_raw) || !is.character(client_raw) || !nzchar(client_raw[1])) {
    return(FALSE)
  }
  if (is.null(id_variable) || is.na(id_variable)) return(TRUE)
  bv <- suppressWarnings(as.integer(boot_var))
  vid <- suppressWarnings(as.integer(id_variable))
  length(bv) == 1L && length(vid) == 1L && !is.na(bv) && !is.na(vid) && identical(vid, bv)
}

