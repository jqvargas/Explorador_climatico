#!/usr/bin/env Rscript
# PASO 2 - Verificación de estructura BD TimescaleDB
library(DBI)
library(RPostgres)
if (file.exists(".env") && requireNamespace("dotenv", quietly = TRUE)) {
  dotenv::load_dot_env(".env")
}

con <- DBI::dbConnect(RPostgres::Postgres(),
  host     = Sys.getenv("DB_HOST", "localhost"),
  port     = as.integer(Sys.getenv("DB_PORT", "5432")),
  dbname   = Sys.getenv("DB_NAME", "clima"),
  user     = Sys.getenv("DB_USER", "postgres"),
  password = Sys.getenv("DB_PASS", Sys.getenv("DB_PASSWORD", "clima1234"))
)

tablas <- DBI::dbListTables(con)
cat("Tablas en BD clima:\n")
print(tablas)

requeridas <- c("estacion", "variable", "observacion", "observacion_final", "observacion_preliminar", "observacion_modelo")
faltan <- setdiff(requeridas, tablas)
if (length(faltan) > 0) {
  stop("Faltan tablas: ", paste(faltan, collapse = ", "))
}

cat("\nConteo de filas:\n")
for (t in c("estacion", "variable", "observacion_final")) {
  n <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) as n FROM ", t))$n
  cat(t, "->", n, "filas\n")
}

# Inspeccionar estructura estacion para los endpoints
cat("\nColumnas estacion:\n")
print(DBI::dbGetQuery(con, "SELECT column_name, data_type FROM information_schema.columns WHERE table_name='estacion' ORDER BY ordinal_position"))

cat("\nColumnas observacion_final (sample):\n")
print(DBI::dbGetQuery(con, "SELECT column_name, data_type FROM information_schema.columns WHERE table_name='observacion_final' ORDER BY ordinal_position LIMIT 15"))

DBI::dbDisconnect(con)
cat("\nOK - BD verificada\n")
