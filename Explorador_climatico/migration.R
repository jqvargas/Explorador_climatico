#!/usr/bin/env Rscript
# Diagnose observacion_preliminar migration failure
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(glue)
  library(dotenv)
})

dotenv::load_dot_env("sql-backend/ATT15580.env")
src_cfg <- list(
  host = Sys.getenv("POSTGRES_HOST"),
  port = as.integer(Sys.getenv("POSTGRES_PORT", 5432)),
  dbname = Sys.getenv("POSTGRES_DB"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD")
)

cat("Connecting to SOURCE...\n")
src <- DBI::dbConnect(RPostgres::Postgres(), !!!src_cfg)

cat("Connecting to DEST (localhost)...\n")
dest <- DBI::dbConnect(RPostgres::Postgres(),
  host = "localhost", port = 5432, dbname = "clima",
  user = "postgres", password = "clima1234"
)

# Get table definition (same as migration.R)
get_table_definition <- function(conn, table_name) {
  q <- "
    SELECT column_name, data_type, character_maximum_length,
           numeric_precision, numeric_scale, is_nullable, column_default
    FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = $1
    ORDER BY ordinal_position
  "
  cols <- DBI::dbGetQuery(conn, q, params = list(table_name))
  if (nrow(cols) == 0) stop(glue("Table {table_name} not found"))
  defs <- apply(cols, 1, function(r) {
    dt <- r["data_type"]
    if (dt %in% c("character varying", "varchar")) {
      len <- r["character_maximum_length"]
      if (!is.na(len) && len != "") glue("{r['column_name']} VARCHAR({len})") else glue("{r['column_name']} VARCHAR")
    } else if (dt == "timestamp with time zone") glue("{r['column_name']} TIMESTAMPTZ")
    } else if (dt == "timestamp without time zone") glue("{r['column_name']} TIMESTAMP")
    } else if (dt == "double precision") glue("{r['column_name']} DOUBLE PRECISION")
    } else if (dt == "real") glue("{r['column_name']} REAL")
    } else if (dt == "integer") glue("{r['column_name']} INTEGER")
    } else if (dt == "bigint") glue("{r['column_name']} BIGINT")
    } else if (dt == "smallint") glue("{r['column_name']} SMALLINT")
    } else if (dt == "boolean") glue("{r['column_name']} BOOLEAN")
    } else if (dt == "text") glue("{r['column_name']} TEXT")
    } else if (dt == "date") glue("{r['column_name']} DATE")
    } else if (dt == "numeric") {
      prec <- r["numeric_precision"]; scale <- r["numeric_scale"]
      if (!is.na(prec) && prec != "") glue("{r['column_name']} NUMERIC({prec},{scale})") else glue("{r['column_name']} NUMERIC")
    } else glue("{r['column_name']} {toupper(dt)}")
  })
  paste(defs, collapse = ", ")
}

Ah wait - I had a syntax error. Let me fix that. The apply callback shouldn't have multiple } else if - they need to be chained properly.
</think>
Fixing syntax errors in the diagnostic script:
<｜tool▁calls▁begin｜><｜tool▁call▁begin｜>
Read