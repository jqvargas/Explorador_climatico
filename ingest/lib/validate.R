# Validacion de formatos scrape y pipeline.

validate_scrape_long <- function(df, label = "scrape") {
  req <- c("year", "month", "day", "codigo_nacional")
  missing <- setdiff(req, names(df))
  if (length(missing) > 0L) {
    stop("[", label, "] Faltan columnas: ", paste(missing, collapse = ", "))
  }
  vcol <- setdiff(names(df), c(req, "status_code", "fecha"))[1]
  if (is.na(vcol)) stop("[", label, "] Sin columna de valor")
  if (nrow(df) == 0L) stop("[", label, "] DataFrame vacio")

  fechas <- as.Date(sprintf("%04d-%02d-%02d", df$year, df$month, df$day))
  if (any(is.na(fechas))) stop("[", label, "] Fechas invalidas en muestra")
  if (anyDuplicated(data.frame(df$codigo_nacional, fechas)) > 0L) {
    warning("[", label, "] Duplicados codigo_nacional+fecha en muestra")
  }
  message("[OK] ", label, ": ", nrow(df), " filas, valor=", vcol,
          ", estaciones=", length(unique(df$codigo_nacional)))
  invisible(list(valor_col = vcol, n = nrow(df)))
}

validate_obs_rows <- function(obs, label = "pipeline") {
  req <- c("id_estacion", "id_variable", "fecha", "valor")
  if (!all(req %in% names(obs))) {
    stop("[", label, "] Faltan columnas observacion: ", paste(setdiff(req, names(obs)), collapse = ", "))
  }
  if (nrow(obs) == 0L) {
    warning("[", label, "] Sin filas tras catalogo")
    return(invisible(list(n = 0L)))
  }
  message("[OK] ", label, ": ", nrow(obs), " filas listas para persist")
  invisible(list(n = nrow(obs)))
}
