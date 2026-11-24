#!/usr/bin/env Rscript
# Escribe el JSON de respaldo del widget. Abort si no hay DMC Quinta Normal.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || !nzchar(args[[1]])) {
  stop("Uso: Rscript export_widget_estaciones.R <archivo.json>")
}

base <- Sys.getenv("API_ROOT", "/app")
if (!dir.exists(base)) base <- file.path(getwd(), "..")
source(file.path(base, "utils", "db.R"), local = FALSE)
source(file.path(base, "utils", "unidades_display.R"), local = FALSE)
source(file.path(base, "utils", "estacion_label.R"), local = FALSE)
source(file.path(base, "utils", "pronostico_payload.R"), local = FALSE)
source(file.path(base, "utils", "widget_pronostico.R"), local = FALSE)

payload <- widget_asset_payload()
json <- jsonlite::toJSON(
  payload, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE, digits = 10
)
out <- args[[1]]
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
con <- file(out, open = "wb")
writeBin(charToRaw(enc2utf8(paste0(json, "\n"))), con)
close(con)
if (!is.null(.DB_POOL)) tryCatch(pool::poolClose(.DB_POOL), error = function(e) NULL)
cat(sprintf(
  "default_station_id=%s stations=%d -> %s\n",
  payload$default_station_id, length(payload$stations), out
))
