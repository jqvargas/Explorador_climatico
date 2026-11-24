# Calculo de rango de fechas para ingesta diaria.
# Contrato: dia_ejecucion = America/Santiago; fin nunca > hoy; ventana <= max_days.

ingest_flag_true <- function(name) {
  tolower(Sys.getenv(name, "")) %in% c("1", "true", "yes", "si")
}

# Fecha calendario en Chile. Requiere TZ=America/Santiago (compose + bootstrap).
ingest_hoy <- function() {
  if (!identical(Sys.getenv("TZ", ""), "America/Santiago")) {
    Sys.setenv(TZ = "America/Santiago")
  }
  Sys.Date()
}

# Rango automatico: desde MAX(fecha)-lag hasta hoy, acotado a max_days.
# Manual: INGEST_FECHA_INI / INGEST_FECHA_FIN (si fin > hoy y no BACKFILL, se corta a hoy).
# ultima: opcional (tests); si NULL, lee MAX(fecha) de bitacora_oficial.
ingest_fechas_diarias <- function(conn, lag_days = 7L, max_days = 14L, ultima = NULL) {
  hoy <- ingest_hoy()
  lag_days <- as.integer(lag_days)
  max_days <- as.integer(max_days)
  if (is.na(lag_days) || lag_days < 0L) stop("lag_days invalido")
  if (is.na(max_days) || max_days < 1L) stop("max_days invalido")

  fin_env <- trimws(Sys.getenv("INGEST_FECHA_FIN", ""))
  ini_env <- trimws(Sys.getenv("INGEST_FECHA_INI", ""))

  fin <- if (nzchar(fin_env)) as.Date(fin_env) else hoy
  if (is.na(fin)) stop("INGEST_FECHA_FIN invalida: ", fin_env)

  # Observacion nunca scrapea futuro respecto al dia de ejecucion.
  if (fin > hoy && !ingest_flag_true("INGEST_BACKFILL")) {
    message("[fechas] fin=", fin, " > dia_ejecucion=", hoy, "; recortando a hoy")
    fin <- hoy
  }

  if (nzchar(ini_env)) {
    ini <- as.Date(ini_env)
    if (is.na(ini)) stop("INGEST_FECHA_INI invalida: ", ini_env)
    if (ini > fin) stop("INGEST_FECHA_INI (", ini, ") > fin (", fin, ")")
    n_dias <- as.integer(fin - ini) + 1L
    message(
      "[fechas] Rango manual: ", ini, " -> ", fin,
      " (dia_ejecucion=", hoy, ", n_dias=", n_dias, ")"
    )
    return(list(ini = ini, fin = fin, hoy = hoy, ultima = as.Date(NA), cap_aplicado = FALSE))
  }

  yr <- as.integer(format(fin, "%Y"))
  year_start <- as.Date(paste0(yr, "-01-01"))

  if (is.null(ultima)) {
    row <- DBI::dbGetQuery(conn, "SELECT MAX(fecha) AS ultima FROM bitacora_oficial")
    ultima <- row$ultima[1]
  }
  if (is.null(ultima) || length(ultima) == 0 || is.na(ultima)) {
    ini <- fin - (max_days - 1L)
    if (ini < year_start) ini <- year_start
    message(
      "[fechas] Sin datos previos; dia_ejecucion=", hoy,
      " ini=", ini, " fin=", fin, " n_dias=", as.integer(fin - ini) + 1L
    )
    return(list(ini = ini, fin = fin, hoy = hoy, ultima = as.Date(NA), cap_aplicado = FALSE))
  }

  ultima <- as.Date(ultima)
  # Hacia el pasado: revisar lag_days desde ultima obs; fin = dia de ejecucion.
  ini <- ultima - lag_days
  if (ini < year_start) ini <- year_start

  if (!ingest_flag_true("INGEST_BACKFILL") && as.integer(format(ini, "%Y")) != yr) {
    message("[fechas] ini cruza ano; forzando ", year_start)
    ini <- year_start
  }

  if (ini > fin) {
    # BD adelantada o igual a hoy: re-scrape corto (hoy + lag hacia atras acotado).
    ini <- fin - lag_days
    if (ini < year_start) ini <- year_start
  }

  n_dias <- as.integer(fin - ini) + 1L
  cap_aplicado <- FALSE
  if (n_dias > max_days) {
    ini <- fin - (max_days - 1L)
    n_dias <- as.integer(fin - ini) + 1L
    cap_aplicado <- TRUE
  }

  message(
    "[fechas] dia_ejecucion=", hoy,
    " ultima_obs=", ultima,
    " ini=", ini, " fin=", fin,
    " n_dias=", n_dias,
    " lag=", lag_days, "d",
    " cap_aplicado=", cap_aplicado,
    if (cap_aplicado) paste0(" (max_days=", max_days, ")") else ""
  )
  list(ini = ini, fin = fin, hoy = hoy, ultima = ultima, cap_aplicado = cap_aplicado)
}

