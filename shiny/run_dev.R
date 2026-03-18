# run_dev.R - Ejecutar Shiny localmente para desarrollo rapido
# Requiere: API corriendo (docker compose up -d timescaledb redis api)
# Uso: desde la raiz del proyecto, ejecutar: Rscript shiny/run_dev.R

pkgs <- c(
  "shiny", "leaflet", "httr", "jsonlite", "plotly", "DT", "bslib",
  "shinycssloaders", "glue", "dotenv", "shinydashboard", "fresh",
  "shinyWidgets", "shinythemes"
)
missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  message("Instalando paquetes faltantes: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

Sys.setenv(API_URL = "http://localhost:8000")
options(shiny.autoreload = TRUE)

app_dir <- if (file.exists("shiny/ui.R")) "shiny" else if (file.exists("ui.R")) "." else NULL
if (is.null(app_dir)) stop("Ejecuta desde la raiz del proyecto: Rscript shiny/run_dev.R")

shiny::runApp(app_dir, host = "0.0.0.0", port = 3838)
