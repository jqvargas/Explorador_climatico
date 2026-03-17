suppressPackageStartupMessages({ library(DBI); library(RPostgres); library(dotenv) })
dotenv::load_dot_env("../sql-backend/ATT15580.env")
cat("Source:", Sys.getenv("POSTGRES_HOST"), "\n")
src <- tryCatch({
  c <- dbConnect(RPostgres::Postgres(), host=Sys.getenv("POSTGRES_HOST"), port=5434, dbname="hidroclimatica_diaria_v2", user=Sys.getenv("POSTGRES_USER"), password=Sys.getenv("POSTGRES_PASSWORD"))
  dbGetQuery(c, "SELECT 1"); dbDisconnect(c); "OK"
}, error=function(e) e$message)
cat("Source DB:", src, "\n")
dest <- tryCatch({
  c <- dbConnect(RPostgres::Postgres(), host="localhost", port=5432, dbname="clima", user="postgres", password="clima1234")
  dbGetQuery(c, "SELECT 1"); dbDisconnect(c); "OK"
}, error=function(e) e$message)
cat("Dest DB:", dest, "\n")
if (file.exists("migration_checkpoint.txt")) cat("\nCheckpoint:\n", readLines("migration_checkpoint.txt"))
