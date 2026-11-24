#!/usr/bin/env Rscript
# Actualiza DMC (pp, temperaturas, humedad, radiacion).

fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
run_dir <- normalizePath(dirname(sub("^--file=", "", fa[1])), winslash = "/")
source(file.path(run_dir, "..", "lib", "bootstrap.R"))
ingest_setup_from_script()

fechas <- ingest_fechas_default()
modo <- Sys.getenv("INGEST_MODO", "completo")
dry_run <- ingest_flag_true("INGEST_DRY_RUN")
Sys.setenv(INGEST_BATCH_MODE = "1")

conn <- get_db_connection()
on.exit(DBI::dbDisconnect(conn), add = TRUE)

variables <- c("pp", "t_max", "t_min", "t_mean", "hr_mean", "rs_mean")
res <- meteovision_run_fuente(
  conn, "dmc", variables, fechas$ini, fechas$fin,
  modo = modo, dry_run = dry_run
)

if (!isTRUE(res$ok)) {
  message("[FAIL] Variables con error: ", paste(res$failed, collapse = ", "))
  quit(status = 1)
}
message("[OK] DMC completado. Filas afectadas: ", res$n_inserted)
