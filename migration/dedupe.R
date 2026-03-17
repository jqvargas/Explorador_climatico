#! /usr/bin/env Rscript
# =============================================================================
# Deduplication Script - removes duplicate rows by id (keeps one per id via ctid)
# Tables: observacion, observacion_final, observacion_preliminar
# Run: Rscript dedupe.R
# Requires: DBI, RPostgres
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
})

DEST_HOST <- "localhost"
DEST_PORT <- 5432
DEST_DB <- "clima"
DEST_USER <- "postgres"
DEST_PASSWORD <- "clima1234"
TABLES <- c("observacion", "observacion_final", "observacion_preliminar")

main <- function() {
  message("Duplicate Removal - connecting to destination database...")
  conn <- tryCatch({
    DBI::dbConnect(
      RPostgres::Postgres(),
      host = DEST_HOST,
      port = DEST_PORT,
      dbname = DEST_DB,
      user = DEST_USER,
      password = DEST_PASSWORD
    )
  }, error = function(e) {
    stop("Failed to connect: ", conditionMessage(e), ". Ensure TimescaleDB is running.")
  })
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  total_deleted <- 0L
  for (tbl in TABLES) {
    message("Processing ", tbl, "...")
    deleted <- tryCatch({
      DBI::dbExecute(conn, paste0(
        "DELETE FROM ", tbl, " a USING ", tbl, " b WHERE a.id = b.id AND a.ctid > b.ctid"
      ))
    }, error = function(e) {
      message("  Error: ", conditionMessage(e))
      return(0L)
    })
    total_deleted <- total_deleted + deleted
    message("  Removed ", deleted, " duplicate row(s)")
  }

  message("Deduplication complete. Total duplicate rows removed: ", total_deleted)
}

main()
