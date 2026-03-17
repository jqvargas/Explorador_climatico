# global.R - se ejecuta PRIMERO, antes que ui.R y server.R
`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

# Cargar todos los modulos
source("modules/mapa.R", local = FALSE)
source("modules/grafico.R", local = FALSE)
source("modules/tabla.R", local = FALSE)
source("modules/descarga.R", local = FALSE)