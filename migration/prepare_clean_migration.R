#!/usr/bin/env Rscript
# DROP tables that have diffs and clear checkpoint - for clean re-migration
# NO COUNT, NO temp tables - safe, fast, no locks
library(DBI)
library(RPostgres)
LOG <- "fix_duplicates_err.txt"
CP <- "migration_checkpoint.txt"
logm <- function(...) { m<-paste0("[",format(Sys.time(),"%Y-%m-%d %H:%M:%S"),"] ",paste(...,collapse=" ")); cat(m,"\n"); write(m,LOG,append=TRUE) }
conn <- dbConnect(Postgres(), host="localhost", port=5432, dbname="clima", user="postgres", password="clima1234")
for(tbl in c("observacion_preliminar","observacion_modelo")) {
  logm("Dropping ", tbl)
  dbExecute(conn, paste0("DROP TABLE IF EXISTS ", tbl, " CASCADE"))
  logm("Dropped ", tbl)
}
cp <- readLines(CP, warn=FALSE)
cp <- cp[!grepl("^(observacion_preliminar|observacion_modelo):", cp)]
writeLines(cp, CP)
logm("Checkpoint cleared for preliminar and modelo. Run migration.R")
dbDisconnect(conn)
logm("Done.")