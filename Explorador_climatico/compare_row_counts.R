library(DBI)
library(RPostgres)
library(dotenv)

dotenv::load_dot_env("sql-backend/ATT15580.env")

TABLES <- c(
  "estacion", "estacion_atributos_extra", "fuente", "modelo", "variable",
  "observacion", "observacion_final", "observacion_preliminar", "observacion_modelo"
)

src <- dbConnect(Postgres(),
  host = Sys.getenv("POSTGRES_HOST"), port = 5434,
  dbname = "hidroclimatica_diaria_v2",
  user = Sys.getenv("POSTGRES_USER"), password = Sys.getenv("POSTGRES_PASSWORD")
)
dst <- dbConnect(Postgres(),
  host = "localhost", port = 5432, dbname = "clima",
  user = "postgres", password = "clima1234"
)

cat("\nRow count comparison (source vs dest)\n")
cat(strrep("-", 55), "\n")
cat(sprintf("%-25s %12s %12s %6s\n", "Table", "Source", "Dest", "Match"))
cat(strrep("-", 55), "\n")

for (tbl in TABLES) {
  tryCatch({
    src_n <- as.integer(dbGetQuery(src, paste0("SELECT COUNT(*) FROM ", tbl))[[1]])
    dst_n <- as.integer(dbGetQuery(dst, paste0("SELECT COUNT(*) FROM ", tbl))[[1]])
    match <- if (src_n == dst_n) "OK" else "DIFF"
    cat(sprintf("%-25s %12s %12s %6s\n", tbl, format(src_n, big.mark = ","), format(dst_n, big.mark = ","), match))
  }, error = function(e) {
    cat(sprintf("%-25s %12s %12s %6s\n", tbl, "error", "error", "ERR"))
  })
}

cat(strrep("-", 55), "\n")
dbDisconnect(src)
dbDisconnect(dst)
