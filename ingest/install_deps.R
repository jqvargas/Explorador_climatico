#!/usr/bin/env Rscript
# Instala dependencias R para ingesta (host Windows o contenedor worker).

install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      message("Instalando ", p, "...")
      install.packages(p, repos = "https://cloud.r-project.org", dependencies = FALSE)
    }
  }
}

install_if_missing(c(
  "DBI", "RPostgres", "dotenv", "redux", "jsonlite",
  "dplyr", "tidyr", "httr", "rvest", "xml2", "lubridate"
))

message("Dependencias de ingesta listas.")
