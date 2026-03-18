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

api_get <- function(path, params = NULL, timeout = API_TIMEOUT_DEFAULT) {
  url <- paste0(api_url(), path)
  if (!is.null(params) && length(params) > 0) {
    q <- paste(names(params), params, sep = "=", collapse = "&")
    url <- paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", q)
  }
  resp <- tryCatch(
    httr::GET(url, api_headers(), httr::timeout(timeout)),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  tryCatch(httr::content(resp, as = "parsed", type = "application/json"), error = function(e) NULL)
}
# Modulos (funcionalidad)
source("modules/mapa.R", local = FALSE)
source("modules/grafico.R", local = FALSE)
source("modules/tabla.R", local = FALSE)
source("modules/descarga.R", local = FALSE)
# Componentes UI (estructura)
source("ui_components.R", local = FALSE)
