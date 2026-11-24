# Adapter DMC -> orquestador MeteoVision.

dmc_run_variable <- function(variable, fecha_ini = NULL, fecha_fin = NULL, modo = NULL) {
  f <- ingest_fechas_default()
  if (is.null(fecha_ini)) fecha_ini <- f$ini
  if (is.null(fecha_fin)) fecha_fin <- f$fin
  if (is.null(modo)) modo <- Sys.getenv("INGEST_MODO", "completo")
  dry_run <- ingest_flag_true("INGEST_DRY_RUN")
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  meteovision_actualizar_variable(
    conn, "dmc", variable, fecha_ini, fecha_fin,
    modo = modo, dry_run = dry_run,
    skip_post_ingesta = ingest_flag_true("INGEST_BATCH_MODE")
  )
}

dmc_fetch_pp <- function(...) { dmc_run_variable("pp", ...) }
dmc_fetch_t_max <- function(...) { dmc_run_variable("t_max", ...) }
dmc_fetch_t_min <- function(...) { dmc_run_variable("t_min", ...) }
dmc_fetch_t_mean <- function(...) { dmc_run_variable("t_mean", ...) }
dmc_fetch_hr_mean <- function(...) { dmc_run_variable("hr_mean", ...) }
dmc_fetch_rs_mean <- function(...) { dmc_run_variable("rs_mean", ...) }
