#!/usr/bin/env Rscript
# Migrate remaining: observacion_preliminar, observacion_modelo
suppressPackageStartupMessages({ library(DBI); library(RPostgres); library(glue); library(cli); library(dotenv) })
TABLES <- c("observacion_preliminar", "observacion_modelo")
CHUNK_SIZE <- list(observacion_preliminar=500000, observacion_modelo=200000)
CHECKPOINT_FILE <- "remaining_checkpoint.txt"
LOG_FILE <- "migrate_remaining_log.txt"
DEST <- list(host="localhost", port=5432, dbname="clima", user="postgres", password="clima1234")
log_msg <- function(...) { msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste(..., collapse=" ")); cat(msg, "\n"); tryCatch({ con <- file(LOG_FILE, open="at"); writeLines(msg, con); flush(con); close(con) }, error=function(e) NULL) }
