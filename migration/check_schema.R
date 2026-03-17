library(DBI)
library(RPostgres)
library(dotenv)

dotenv::load_dot_env("../sql-backend/ATT15580.env")

src <- dbConnect(Postgres(), host=Sys.getenv("POSTGRES_HOST"), port=Sys.getenv("POSTGRES_PORT"), dbname=Sys.getenv("POSTGRES_DB"), user=Sys.getenv("POSTGRES_USER"), password=Sys.getenv("POSTGRES_PASSWORD"))

q <- "SELECT column_name, data_type FROM information_schema.columns WHERE table_schema='public' AND table_name='observacion_preliminar' ORDER BY ordinal_position;"
res <- dbGetQuery(src, q)
print(res)
dbDisconnect(src)
