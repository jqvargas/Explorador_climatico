#!/usr/bin/env Rscript
library(DBI)
library(RPostgres)
LOG_FILE <- "fix_duplicates_err.txt"
CHECKPOINT_FILE <- "migration_checkpoint.txt"
log_msg <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste(..., collapse = " "))
  cat(msg, "\n"); write(msg, LOG_FILE, append = TRUE)
}
conn <- dbConnect(Postgres(), host = "localhost", port = 5432, dbname = "clima", user = "postgres", password = "clima1234")
log_msg("=== observacion_modelo: DROP ===")
dbExecute(conn, "DROP TABLE IF EXISTS observacion_modelo CASCADE")
log_msg("Dropped")
cp <- if (file.exists(CHECKPOINT_FILE)) readLines(CHECKPOINT_FILE, warn = FALSE) else character(0)
cp <- cp[!grepl("^observacion_modelo:", cp)]
writeLines(cp, CHECKPOINT_FILE)
log_msg("Checkpoint cleared. Run migration.R to re-migrate.")
dbDisconnect(conn)
log_msg("Done.")