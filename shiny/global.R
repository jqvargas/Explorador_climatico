# global.R - se ejecuta PRIMERO
`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

library(shinyWidgets)
library(shinythemes)

source("config.R", local = FALSE)
# API helpers (inline) - timeouts evitan que queries en cola bloqueen la app
api_url <- function() sub("/$", "", Sys.getenv("API_URL", "http://api:8000"))
API_TIMEOUT_DEFAULT <- 10
API_TIMEOUT_ESTACIONES <- 20
API_TIMEOUT_DATOS <- 45
api_headers <- function() {
  key <- Sys.getenv("API_KEY", "")
  if (nchar(key) > 0) httr::add_headers(`X-API-Key` = key) else httr::add_headers()
}

api_get <- function(path, params = NULL, timeout = API_TIMEOUT_DEFAULT,
                    reintentos = 2, pausa = 1.5) {
  url <- paste0(api_url(), path)
  if (!is.null(params) && length(params) > 0) {
    q <- paste(names(params), params, sep = "=", collapse = "&")
    url <- paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", q)
  }
  for (intento in seq_len(reintentos + 1)) {
    resp <- tryCatch(
      httr::GET(url, api_headers(), httr::timeout(timeout)),
      error = function(e) NULL
    )
    if (!is.null(resp) && httr::status_code(resp) == 200) {
      return(tryCatch(
        httr::content(resp, as = "parsed", type = "application/json"),
        error = function(e) NULL
      ))
    }
    if (intento <= reintentos) Sys.sleep(pausa)
  }
  return(NULL)
}

# Fechas desde API JSON / listas / POSIX: evita "don't know how to convert ... to Date".
ec_coerce_una_fecha <- function(x) {
  if (is.null(x)) return(as.Date(NA))
  if (is.data.frame(x)) {
    if (nrow(x) < 1L || ncol(x) < 1L) return(as.Date(NA))
    return(ec_coerce_una_fecha(x[[1L]][[1L]]))
  }
  if (is.array(x) && length(x) == 1L) x <- as.vector(x)
  if (inherits(x, "Date")) {
    if (length(x) == 0L) return(as.Date(NA))
    return(x[1L])
  }
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    if (length(x) == 0L) return(as.Date(NA))
    return(as.Date(x[1L]))
  }
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    xs <- trimws(as.character(x[1L]))
    if (!nzchar(xs) || is.na(xs)) return(as.Date(NA))
    d <- suppressWarnings(as.Date(xs))
    if (!is.na(d)) return(d)
    for (fmt in c("%Y-%m-%dT%H:%M:%OS", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%OS",
                  "%Y-%m-%d %H:%M:%S", "%Y-%m-%d")) {
      p <- suppressWarnings(strptime(xs, format = fmt, tz = "UTC"))
      if (!is.na(p)) return(as.Date(p))
    }
    return(suppressWarnings(as.Date(substr(xs, 1L, 10L))))
  }
  if (is.numeric(x) && length(x) >= 1L && !is.na(x[1L])) {
    return(as.Date(as.numeric(x[1L]), origin = "1970-01-01"))
  }
  if (is.list(x)) return(ec_coerce_una_fecha(x[[1L]]))
  suppressWarnings(tryCatch(as.Date(x), error = function(e) as.Date(NA)))
}

# Modulos (funcionalidad)
source("modules/mapa.R", local = FALSE)
source("modules/grafico.R", local = FALSE)
source("modules/tabla.R", local = FALSE)
source("modules/tabla_estaciones.R", local = FALSE)
source("modules/descarga.R", local = FALSE)
# Componentes UI (estructura)
source("ui_components.R", local = FALSE)
