# Carga scrapers MeteoVision y dependencias de scraping.

.meteovision_env <- new.env(parent = emptyenv())

meteovision_dir <- function() {
  file.path(get("INGEST_DIR", envir = .GlobalEnv), "vendor", "meteovision")
}

meteovision_load_deps <- function() {
  pkgs <- c("dplyr", "tidyr", "httr", "rvest", "lubridate", "xml2")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop("Paquetes R faltantes para scraping: ", paste(missing, collapse = ", "),
         ". Ejecute: Rscript ingest/install_deps.R")
  }
  invisible(TRUE)
}

meteovision_init <- function() {
  if (exists("loaded", envir = .meteovision_env, inherits = FALSE) &&
      isTRUE(get("loaded", envir = .meteovision_env))) {
    return(invisible(TRUE))
  }
  meteovision_load_deps()
  suppressPackageStartupMessages({
    library(dplyr, warn.conflicts = FALSE)
    library(tidyr, warn.conflicts = FALSE)
    library(httr, warn.conflicts = FALSE)
    library(rvest, warn.conflicts = FALSE)
    library(lubridate, warn.conflicts = FALSE)
    library(xml2, warn.conflicts = FALSE)
  })
  assign("dbGetQuery", DBI::dbGetQuery, envir = .GlobalEnv)
  mv <- meteovision_dir()
  source(file.path(mv, "lib", "queries_catalog.R"), local = FALSE)
  if (file.exists(file.path(mv, "scrape", "dga_scrape_helpers.R"))) {
    source(file.path(mv, "scrape", "dga_scrape_helpers.R"), local = FALSE)
  }
  source(file.path(mv, "scrape", "descargar_DGA.R"), local = FALSE)
  if (file.exists(file.path(mv, "scrape", "dmc_scrape_helpers.R"))) {
    source(file.path(mv, "scrape", "dmc_scrape_helpers.R"), local = FALSE)
  }
  source(file.path(mv, "scrape", "descargar_DMC.R"), local = FALSE)
  assign("loaded", TRUE, envir = .meteovision_env)
  invisible(TRUE)
}
