#* @apiTitle Explorador Climatico API
library(plumber)

base <- "/app"
source(file.path(base, "utils", "db.R"))
source(file.path(base, "utils", "limits.R"))
source(file.path(base, "utils", "auth.R"))

`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

log_filter <- function(req, res) {
  t0 <- Sys.time()
  plumber::forward()
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  message(sprintf("[%s] %s %s - %s s", format(Sys.time(), "%H:%M:%S"), req$REQUEST_METHOD, req$PATH_INFO, elapsed))
}

api_key_filter <- function(req, res) {
  if (req$REQUEST_METHOD == "OPTIONS") { plumber::forward(); return(invisible(NULL)) }
  required_key <- Sys.getenv("API_KEY", "")
  if (nchar(required_key) == 0) { plumber::forward(); return(invisible(NULL)) }
  key <- req$HTTP_X_API_KEY %||% req$headers[["x-api-key"]] %||% ""
  if (key != required_key) {
    res$status <- 401
    return(list(error = "API key requerida. Envia X-API-Key en el header."))
  }
  plumber::forward()
}

cors_filter <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, X-API-Key")
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

header <- c(
  'base <- "/app"',
  'source(file.path(base, "utils", "db.R"))',
  'source(file.path(base, "utils", "limits.R"))',
  'source(file.path(base, "utils", "auth.R"))',
  '`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x',
  ""
)
endpoint_files <- c("ping.R", "fuentes.R", "estaciones.R", "variables.R", "datos.R", "descargas.R")
combined <- c(header, unlist(lapply(endpoint_files, function(f) {
  readLines(file.path(base, "endpoints", f), warn = FALSE)
})))
tmp <- tempfile(fileext = ".R")
writeLines(combined, tmp)
sub_pr <- plumber::plumb(tmp)
file.remove(tmp)

pr <- plumber::pr() %>%
  plumber::pr_filter("apikey", api_key_filter) %>%
  plumber::pr_filter("log", log_filter) %>%
  plumber::pr_filter("cors", cors_filter) %>%
  plumber::pr_mount("/", sub_pr) %>%
  plumber::pr_set_docs(FALSE)

pr %>% plumber::pr_run(host = "0.0.0.0", port = 8000)