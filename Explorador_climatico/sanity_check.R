#!/usr/bin/env Rscript
# Quick sanity check: test both source and destination database connections
# Run: Rscript sanity_check.R

suppressPackageStartupMessages({ library(DBI); library(RPostgres); library(dotenv) })

cat("=== Migration Sanity Check ===\n\n")

# Load source config
env_paths <- c(".env", "sql-backend/.env", "sql-backend/ATT15580.env")
loaded <- FALSE
for (path in env_paths) {
  if (file.exists(path)) {
    dotenv::load_dot_env(path)
    loaded <- TRUE
    cat("Loaded:", path, "\n")
    break
  }
}
if (!loaded) { cat("ERROR: No .env found\n"); quit(status=1) }

src_host <- Sys.getenv("DB_HOST", Sys.getenv("POSTGRES_HOST", ""))
src_port <- as.integer(Sys.getenv("DB_PORT", Sys.getenv("POSTGRES_PORT", "5432")))
src_db <- Sys.getenv("DB_NAME", Sys.getenv("POSTGRES_DB", ""))
src_user <- Sys.getenv("DB_USER", Sys.getenv("POSTGRES_USER", ""))
src_pw <- Sys.getenv("DB_PASSWORD", Sys.getenv("POSTGRES_PASSWORD", ""))

# Source DB
cat("Source DB (", src_host, ":", src_port, "/", src_db, "): ", sep="")
src_ok <- tryCatch({
  c <- DBI::dbConnect(RPostgres::Postgres(), host=src_host, port=src_port, dbname=src_db, user=src_user, password=src_pw)
  n <- DBI::dbGetQuery(c, "SELECT 1")[1,1]
  DBI::dbDisconnect(c)
  TRUE
}, error = function(e) { cat("FAIL -", conditionMessage(e), "\n"); FALSE })
if (src_ok) cat("OK\n")

# Dest DB (TimescaleDB)
cat("Dest DB (localhost:5432/clima): ")
dest_ok <- tryCatch({
  c <- DBI::dbConnect(RPostgres::Postgres(), host="localhost", port=5432, dbname="clima", user="postgres", password="clima1234")
  n <- DBI::dbGetQuery(c, "SELECT 1")[1,1]
  DBI::dbDisconnect(c)
  TRUE
}, error = function(e) { cat("FAIL -", conditionMessage(e), "\n"); FALSE })
if (dest_ok) cat("OK\n")

# Checkpoint
if (file.exists("migration_checkpoint.txt")) {
  cp <- readLines("migration_checkpoint.txt", warn=FALSE)
  cat("\nCheckpoint:\n")
  for (l in cp) if (nchar(trimws(l))>0) cat("  ", l, "\n")
  if (any(grepl("observacion_modelo:", cp))) {
    m <- regmatches(cp, regexec("observacion_modelo:(\\d+)", cp))
    val <- as.integer(na.omit(sapply(m, function(x) x[2]))[1])
    cat("  -> Resume from chunk", val+1, "of observacion_modelo (1559 total)\n")
  }
}

cat("\n")
if (src_ok && dest_ok) {
  cat("All checks passed. Ready to run migration.\n")
  quit(status=0)
} else {
  cat("Some checks failed. Fix DB connectivity before migration.\n")
  quit(status=1)
}
