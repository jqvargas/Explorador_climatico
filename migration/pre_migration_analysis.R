# Pre-migration analysis - READ ONLY, no heavy temp tables, no locks
library(DBI)
library(RPostgres)
library(dotenv)
dotenv::load_dot_env("../sql-backend/ATT15580.env")
src <- dbConnect(Postgres(), host=Sys.getenv("POSTGRES_HOST"), port=5434, dbname="hidroclimatica_diaria_v2", user=Sys.getenv("POSTGRES_USER"), password=Sys.getenv("POSTGRES_PASSWORD"))
dst <- dbConnect(Postgres(), host="localhost", port=5432, dbname="clima", user="postgres", password="clima1234")
tabs <- c("estacion","estacion_atributos_extra","fuente","modelo","variable","observacion","observacion_final","observacion_preliminar","observacion_modelo")
cat("\n=== PRE-MIGRATION ANALYSIS ===\n")
cat("Checkpoint: ", paste(readLines("migration_checkpoint.txt",warn=FALSE), collapse=" | "), "\n\n")
cat("Table                      | Source        | Dest          | Match\n")
cat(paste(rep("-",72),collapse=""),"\n")
all_ok <- TRUE
for(t in tabs){
  sc <- tryCatch(as.integer(dbGetQuery(src,paste0("SELECT COUNT(*) FROM ",t))[[1]]), error=function(e) NA)
  dc <- tryCatch(as.integer(dbGetQuery(dst,paste0("SELECT COUNT(*) FROM ",t))[[1]]), error=function(e) NA)
  if(is.na(dc)) dc <- "MISSING"
  st <- if(is.na(sc)||is.na(dc)||sc!=dc) { all_ok<-FALSE; "DIFF" } else "OK"
  cat(sprintf("%-26s | %12s | %12s | %s\n", t, if(is.na(sc))"err" else format(sc,big.mark=","), if(is.vector(dc))format(dc,big.mark=",") else dc, st))
}
dbDisconnect(src); dbDisconnect(dst)
cat(paste(rep("-",72),collapse=""),"\n")
if(!all_ok) cat("\nACTION: Run prepare_clean_migration.R to DROP diff tables and reset checkpoint before migration.\n") else cat("\nAll tables match. Safe to run migration.R\n")