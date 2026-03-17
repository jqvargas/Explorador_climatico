# global.R - se ejecuta PRIMERO
`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

library(fresh)
tema_rodaja <- fresh::create_theme(
  fresh::adminlte_color(light_blue = "#DF9531"),
  fresh::adminlte_sidebar(
    dark_bg              = "#D5DADC",
    dark_hover_bg        = "#b8bec0",
    dark_color           = "#394553",
    dark_hover_color     = "#000000",
    dark_submenu_bg      = "#b8bec0",
    dark_submenu_color   = "#394553",
    dark_submenu_hover_color = "#000000"
  ),
  fresh::adminlte_global(
    content_bg  = "#D5DADC",
    box_bg      = "#FFFFFF",
    info_box_bg = "#FFFFFF"
  )
)

source("config.R", local = FALSE)
# API helpers (inline)
api_url <- function() sub("/$", "", Sys.getenv("API_URL", "http://api:8000"))
api_get <- function(path, params = NULL, timeout = 15) {
  url <- paste0(api_url(), path)
  if (!is.null(params) && length(params) > 0) {
    q <- paste(names(params), params, sep = "=", collapse = "&")
    url <- paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", q)
  }
  resp <- tryCatch(httr::GET(url, httr::timeout(timeout)), error = function(e) NULL)
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


