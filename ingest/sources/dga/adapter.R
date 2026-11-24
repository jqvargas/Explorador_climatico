# Adapter DGA -> orquestador MeteoVision.

dga_run_variable <- function(variable, fecha_ini = NULL, fecha_fin = NULL, modo = NULL) {
  f <- ingest_fechas_default()
  if (is.null(fecha_ini)) fecha_ini <- f$ini
  if (is.null(fecha_fin)) fecha_fin <- f$fin
  if (is.null(modo)) modo <- Sys.getenv("INGEST_MODO", "completo")
  dry_run <- ingest_flag_true("INGEST_DRY_RUN")
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  meteovision_actualizar_variable(
    conn, "dga", variable, fecha_ini, fecha_fin,
    modo = modo, dry_run = dry_run,
    skip_post_ingesta = ingest_flag_true("INGEST_BATCH_MODE")
  )
}

dga_fetch_pp <- function(fecha_ini = NULL, fecha_fin = NULL, modo = NULL, ...) {
  dga_run_variable("pp", fecha_ini, fecha_fin, modo)
}

dga_fetch_t_min <- function(fecha_ini = NULL, fecha_fin = NULL, modo = NULL, ...) {
  dga_run_variable("t_min", fecha_ini, fecha_fin, modo)
}

dga_fetch_t_max <- function(fecha_ini = NULL, fecha_fin = NULL, modo = NULL, ...) {
  dga_run_variable("t_min", fecha_ini, fecha_fin, modo)
}

dga_fetch_q <- function(fecha_ini = NULL, fecha_fin = NULL, modo = NULL, ...) {
  dga_run_variable("q", fecha_ini, fecha_fin, modo)
}
