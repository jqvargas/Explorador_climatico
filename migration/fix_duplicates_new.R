#!/usr/bin/env Rscript
library(DBI)
library(RPostgres)

LOG_FILE <- "fix_duplicates_err.txt"
BATCH_SIZE <- 5000

log_msg <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste(..., collapse = " "))
  cat(msg, "\n")
  write(msg, LOG_FILE, append = TRUE)
}

conn <- dbConnect(Postgres(), host = "localhost", port = 5432, dbname = "clima", user = "postgres", password = "clima1234")

CHECKPOINT_FILE <- "migration_checkpoint.txt"

for (tbl in c("observacion_preliminar", "observacion_modelo")) {
  log_msg("=== ", tbl, " ===")

  if (tbl == "observacion_modelo") {
    log_msg("Strategy: DROP and re-migrate (skip slow delete)")
    n_before <- as.integer(dbGetQuery(conn, paste0("SELECT COUNT(*) FROM ", tbl))[[1]])
    log_msg("Rows before: ", format(n_before, big.mark = ","))
    dbExecute(conn, paste0("DROP TABLE IF EXISTS ", tbl, " CASCADE"))
    log_msg("Dropped ", tbl)
    cp <- if (file.exists(CHECKPOINT_FILE)) readLines(CHECKPOINT_FILE, warn = FALSE) else character(0)
    cp <- cp[!grepl(paste0("^", tbl, ":"), cp)]
    writeLines(cp, CHECKPOINT_FILE)
    log_msg("Removed ", tbl, " from checkpoint. Run migration.R to re-migrate.")
    next
  }

  n_before <- as.integer(dbGetQuery(conn, paste0("SELECT COUNT(*) FROM ", tbl))[[1]])
  log_msg("Rows before: ", format(n_before, big.mark = ","))
  log_msg("Creating temp table of duplicate ids (may take a few min)...")
  dbExecute(conn, paste0("DROP TABLE IF EXISTS _dup_ids_", tbl))
  dbExecute(conn, paste0("CREATE TEMP TABLE _dup_ids_", tbl, " AS SELECT id FROM ", tbl, " GROUP BY id HAVING COUNT(*) > 1"))
  n_dups <- as.integer(dbGetQuery(conn, paste0("SELECT COUNT(*) FROM _dup_ids_", tbl))[[1]])
  log_msg("Duplicate ids: ", format(n_dups, big.mark = ","))

  if (n_dups == 0) { log_msg("No duplicates - skip"); next }

  n_batches <- ceiling(n_dups / BATCH_SIZE)
  total_deleted <- 0
  for (i in seq_len(n_batches)) {
    dbExecute(conn, paste0("DROP TABLE IF EXISTS _batch_", tbl))
    dbExecute(conn, paste0("CREATE TEMP TABLE _batch_", tbl, " AS SELECT id FROM _dup_ids_", tbl, " LIMIT ", BATCH_SIZE))
    res <- dbExecute(conn, paste0(
      "WITH keep AS (SELECT id, MIN(ctid) as ct FROM ", tbl, " WHERE id IN (SELECT id FROM _batch_", tbl, ") GROUP BY id)
       DELETE FROM ", tbl, " WHERE id IN (SELECT id FROM _batch_", tbl, ") AND ctid NOT IN (SELECT ct FROM keep)"
    ))
    dbExecute(conn, paste0("DELETE FROM _dup_ids_", tbl, " WHERE id IN (SELECT id FROM _batch_", tbl, ")"))
    total_deleted <- total_deleted + res
    if (i %% 10 == 0 || i == n_batches) log_msg("  Batch ", i, "/", n_batches, " - deleted ", total_deleted, " so far")
    gc()
  }
  n_after <- as.integer(dbGetQuery(conn, paste0("SELECT COUNT(*) FROM ", tbl))[[1]])
  log_msg("Rows after: ", format(n_after, big.mark = ","))
}

dbDisconnect(conn)
log_msg("=== Done ===")