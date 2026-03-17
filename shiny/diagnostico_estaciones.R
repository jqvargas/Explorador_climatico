# Diagnostico: por que no se muestran estaciones en mapa ni dropdown
# Ejecutar: Rscript shiny/diagnostico_estaciones.R
# O desde el contenedor: docker compose exec shiny Rscript /srv/shiny-server/diagnostico_estaciones.R

base <- Sys.getenv("API_URL", "http://api:8000")
base <- sub("/$", "", base)

cat("=== DIAGNOSTICO ESTACIONES ===\n\n")
cat("API base:", base, "\n\n")

# 1. Variables - obtener primera variable
cat("1. GET /variables ...\n")
url_v <- paste0(base, "/variables")
resp_v <- tryCatch(httr::GET(url_v, httr::timeout(10)), error = function(e) { cat("   ERROR:", conditionMessage(e), "\n"); NULL })
if (is.null(resp_v)) stop("No se pudo conectar a /variables")
cat("   Status:", httr::status_code(resp_v), "\n")
dat_v <- tryCatch(httr::content(resp_v, as = "parsed", type = "application/json"), error = function(e) NULL)
if (!is.null(dat_v)) {
  if (is.data.frame(dat_v)) { cat("   Filas:", nrow(dat_v), "\n"); vid <- dat_v$id[1] }
  else if (is.list(dat_v) && length(dat_v) > 0) { cat("   Elementos:", length(dat_v), "\n"); vid <- dat_v[[1]]$id %||% dat_v[[1]][["id"]] }
  else vid <- 1L
  cat("   Primera variable id:", vid, "\n")
} else { vid <- 1L; cat("   Usando variable_id=1 por defecto\n") }
cat("\n")

# 2. Estaciones SIN filtros (todas)
cat("2. GET /estaciones?chile_only=1&minimal=1 (sin variable ni operador) ...\n")
url_e1 <- paste0(base, "/estaciones?chile_only=1&minimal=1")
resp_e1 <- tryCatch(httr::GET(url_e1, httr::timeout(30)), error = function(e) { cat("   ERROR:", conditionMessage(e), "\n"); NULL })
if (is.null(resp_e1)) { cat("   FALLO: timeout o conexion\n"); } else {
  cat("   Status:", httr::status_code(resp_e1), "\n")
  dat_e1 <- tryCatch(httr::content(resp_e1, as = "parsed", type = "application/json"), error = function(e) NULL)
  if (is.null(dat_e1)) cat("   FALLO: no se pudo parsear JSON\n")
  else if (is.data.frame(dat_e1)) cat("   Filas:", nrow(dat_e1), " | Columnas:", paste(names(dat_e1), collapse=", "), "\n")
  else if (is.list(dat_e1)) {
    cat("   Tipo: list, length:", length(dat_e1), "\n")
    if (length(dat_e1) > 0) {
      prim <- dat_e1[[1]]
      cat("   Primer elemento: list con", length(prim), "nombres:", paste(names(prim), collapse=", "), "\n")
    }
  }
  else cat("   Tipo inesperado:", class(dat_e1), "\n")
}
cat("\n")

# 3. Estaciones CON id_variable
cat("3. GET /estaciones?chile_only=1&minimal=1&id_variable=", vid, " ...\n", sep="")
url_e2 <- paste0(base, "/estaciones?chile_only=1&minimal=1&id_variable=", vid)
resp_e2 <- tryCatch(httr::GET(url_e2, httr::timeout(30)), error = function(e) { cat("   ERROR:", conditionMessage(e), "\n"); NULL })
if (is.null(resp_e2)) { cat("   FALLO: timeout o conexion\n"); } else {
  cat("   Status:", httr::status_code(resp_e2), "\n")
  dat_e2 <- tryCatch(httr::content(resp_e2, as = "parsed", type = "application/json"), error = function(e) NULL)
  if (is.null(dat_e2)) cat("   FALLO: no se pudo parsear JSON\n")
  else if (is.data.frame(dat_e2)) cat("   Filas:", nrow(dat_e2), "\n")
  else if (is.list(dat_e2)) cat("   Tipo: list, length:", length(dat_e2), "\n")
}
cat("\n")

# 4. Estaciones - tamanio respuesta
cat("4. Tamanio respuesta (bytes) /estaciones sin filtros:", NA, "\n")
if (!is.null(resp_e1)) cat("   ", nchar(paste(httr::content(resp_e1, as="text", encoding="UTF-8"), collapse="")), " bytes\n", sep="")
cat("\n")
cat("=== FIN DIAGNOSTICO ===\n")
