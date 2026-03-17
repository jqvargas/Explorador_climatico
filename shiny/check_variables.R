conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST", "timescaledb"),
  port = as.integer(Sys.getenv("DB_PORT", "5432")),
  dbname = Sys.getenv("DB_NAME", "clima"),
  user = Sys.getenv("DB_USER", "postgres"),
  password = Sys.getenv("DB_PASS", "clima1234")
)
cat("Variable table columns:\n")
print(DBI::dbGetQuery(conn, "SELECT column_name FROM information_schema.columns WHERE table_name = 'variable' ORDER BY ordinal_position"))
cat("\nVariable row count:\n")
print(DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM variable"))
cat("\nSample:\n")
print(DBI::dbGetQuery(conn, "SELECT * FROM variable LIMIT 3"))
DBI::dbDisconnect(conn)
