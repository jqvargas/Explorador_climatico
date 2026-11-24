# Conexion a TimescaleDB desde el host Windows (127.0.0.1:5432 por defecto).

get_db_connection <- function() {
  host <- Sys.getenv("DB_HOST", "127.0.0.1")
  port <- as.integer(Sys.getenv("DB_PORT", "5432"))
  dbname <- Sys.getenv("DB_NAME", "clima")
  user <- Sys.getenv("DB_USER", "postgres")
  password <- Sys.getenv("DB_PASS", Sys.getenv("DB_PASSWORD", ""))

  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RPostgres", quietly = TRUE)) {
    stop("Instale DBI y RPostgres en el host R.")
  }

  DBI::dbConnect(
    RPostgres::Postgres(),
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = password
  )
}

test_db_connection <- function() {
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  row <- DBI::dbGetQuery(conn, "SELECT current_database() AS db, NOW() AS ts")
  message("[ingest] Conexion OK: ", row$db[1], " @ ", row$ts[1])
  invisible(TRUE)
}
