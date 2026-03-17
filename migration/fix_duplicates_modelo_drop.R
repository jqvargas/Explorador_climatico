#!/usr/bin/env Rscript
# DROP observacion_modelo and clear checkpoint for re-migration
# Run AFTER fix_duplicates.R finishes observacion_preliminar

library(DBI)
library(RPostgres)

LOG_FILE <- "fix_duplicates_err.txt"
CHECKPOINT_FILE <- "migration_checkpoint.txt"

log_msg <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste(..., collapse = " "))
  cat(msg, "\n")
  write(msg, LOG_FILE, append = TRUE)
}

conn <- dbConnect(Postgres(), host = "localhost", port = 5432, dbname = "clima", user = "postgres", password = "clima1234")

log_msg("=== observacion_modelo: DROP and prepare for re-migration ===")
n_before <- as.integer(dbGetQuery(conn, "SELECT COUNT(*) FROM observacion_modelo")[[1]])
log_msg("Rows before: ", format(n_before, big.mark = ","))

dbExecute(conn, "DROP TABLE IF EXISTS observacion_modelo CASCADE")
log_msg("Dropped observacion_modelo")

cp <- if (file.exists(CHECKPOINT_FILE)) readLines(CHECKPOINT_FILE, warn = FALSE) else character(0)
cp <- cp[!grepl("^observacion_modelo:", cp)]
writeLines(cp, CHECKPOINT_FILE)
log_msg("Removed observacion_modelo from checkpoint")

dbDisconnect(conn)
log_msg("Done. Run migration.R to re-migrate observacion_modelo.")