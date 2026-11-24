# Normalizacion wide -> long y fechas (portado desde sql-backend/funciones_utilidad.R).

check_fecha_valid <- function(fecha) {
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", fecha)) {
    message("Formato de fecha incorrecto: ", fecha, " (esperado YYYY-MM-DD)")
    return(FALSE)
  }
  fecha_convertida <- tryCatch(
    as.Date(fecha, format = "%Y-%m-%d"),
    error = function(e) NA
  )
  result <- !is.na(fecha_convertida) && format(fecha_convertida, "%Y-%m-%d") == fecha
  if (!result) {
    message("Fecha invalida: ", fecha)
  }
  result
}

df_dga_a_formato_largo <- function(df, variable) {
  if (!requireNamespace("tidyr", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Instale tidyr y dplyr para normalizar datos DGA.")
  }
  df <- df |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),
      names_to = "codigo_nacional",
      values_to = variable
    ) |>
    dplyr::filter(!is.na(.data[[variable]]))

  df$codigo_nacional <- gsub("^X", "", df$codigo_nacional)
  df$codigo_nacional <- gsub("\\.", "-", df$codigo_nacional)

  df <- df |>
    dplyr::rename(year = Year, month = Month, day = Day) |>
    dplyr::select(-dplyr::any_of(c("Date", "Id")))

  df
}

df_dmc_a_formato_largo <- function(df, variable) {
  if (!requireNamespace("tidyr", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Instale tidyr y dplyr para normalizar datos DMC.")
  }
  df <- df |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),
      names_to = "codigo_nacional",
      values_to = variable
    )
  df$codigo_nacional <- gsub("X", "", df$codigo_nacional)
  df <- df |> dplyr::filter(!is.na(.data[[variable]]))

  if (variable == "pp") {
    df <- df |> dplyr::rename(year = Año, month = Mes, day = Dia)
  } else {
    df <- df |>
      dplyr::mutate(
        year = as.numeric(format(as.Date(Fecha, format = "%d-%m-%Y"), "%Y")),
        month = as.numeric(format(as.Date(Fecha, format = "%d-%m-%Y"), "%m")),
        day = as.numeric(format(as.Date(Fecha, format = "%d-%m-%Y"), "%d"))
      ) |>
      dplyr::select(-Fecha)
  }
  df
}

normalize_long_to_observaciones <- function(df, valor_col = "valor") {
  if (!all(c("codigo_nacional", "year", "month", "day") %in% names(df))) {
    stop("Faltan columnas codigo_nacional, year, month, day.")
  }
  if (!valor_col %in% names(df)) {
    stop("Columna de valor no encontrada: ", valor_col)
  }

  out <- data.frame(
    codigo_nacional = as.character(df$codigo_nacional),
    fecha = as.Date(sprintf("%04d-%02d-%02d", df$year, df$month, df$day)),
    valor = suppressWarnings(as.numeric(df[[valor_col]])),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$fecha) & !is.na(out$valor), , drop = FALSE]
  out
}
