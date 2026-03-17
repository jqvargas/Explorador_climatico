#!/usr/bin/env Rscript
# =============================================================================
# TimescaleDB Migration Script
# Migrates source PostgreSQL database to TimescaleDB (destination)
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(glue)
  library(cli)
  library(dotenv)
})

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
SMALL_TABLES <- c("estacion", "estacion_atributos_extra", "fuente", "modelo", "variable")
LARGE_TABLES <- c("observacion", "observacion_final", "observacion_preliminar", "observacion_modelo")
CHUNK_SIZE <- list(
  observacion = 500000,
  observacion_final = 500000,
  observacion_preliminar = 500000,
  observacion_modelo = 200000
)
CHECKPOINT_FILE <- "migration_checkpoint.txt"
MIGRATION_LOG_FILE <- "migration_err.txt"
DEST_HOST <- "localhost"
DEST_PORT <- 5432
DEST_DB <- "clima"
DEST_USER <- "postgres"
DEST_PASSWORD <- "clima1234"

mig_log <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste(..., collapse = " "))
  cli_alert(msg)
  tryCatch({
    con <- file(MIGRATION_LOG_FILE, open = "at")
    writeLines(msg, con)
    flush(con)
    close(con)
  }, error = function(e) NULL)
}

# -----------------------------------------------------------------------------
# Load environment - support both DB_* and POSTGRES_* naming
# -----------------------------------------------------------------------------
load_env <- function() {
  env_paths <- c(".env", "sql-backend/.env", "sql-backend/ATT15580.env")
  loaded <- FALSE
  for (path in env_paths) {
    if (file.exists(path)) {
      dotenv::load_dot_env(path)
      loaded <- TRUE
      cli_alert_info("Loaded env from: {path}")
      break
    }
  }
  # When running migration on HOST: .env has DB_HOST=timescaledb (Docker network)
  # which doesn't resolve. Use source credentials from ATT15580.env instead.
  use_source_env <- FALSE
  if (loaded && Sys.getenv("DB_HOST") == "timescaledb" && file.exists("sql-backend/ATT15580.env")) {
    dotenv::load_dot_env("sql-backend/ATT15580.env")
    use_source_env <- TRUE
    cli_alert_info("Using sql-backend/ATT15580.env for SOURCE (migration on host)")
  }
  if (!loaded) {
    stop("No .env file found. Create one with DB_HOST, DB_PORT, DB_NAME, DB_USER, DB_PASSWORD (or POSTGRES_* equivalents)")
  }

  # Prefer DB_* vars, fall back to POSTGRES_*. When use_source_env, use POSTGRES_* (ATT15580)
  if (use_source_env) {
    src_host <- Sys.getenv("POSTGRES_HOST", "")
    src_port <- Sys.getenv("POSTGRES_PORT", "")
    src_db <- Sys.getenv("POSTGRES_DB", "")
    src_user <- Sys.getenv("POSTGRES_USER", "")
    src_password <- Sys.getenv("POSTGRES_PASSWORD", "")
  } else {
    src_host <- Sys.getenv("DB_HOST", Sys.getenv("POSTGRES_HOST", ""))
    src_port <- Sys.getenv("DB_PORT", Sys.getenv("POSTGRES_PORT", ""))
    src_db <- Sys.getenv("DB_NAME", Sys.getenv("POSTGRES_DB", ""))
    src_user <- Sys.getenv("DB_USER", Sys.getenv("POSTGRES_USER", ""))
    src_password <- Sys.getenv("DB_PASSWORD", Sys.getenv("POSTGRES_PASSWORD", ""))
  }

  if (any(c(src_host, src_db, src_user) == "")) {
    stop("Missing source credentials. Need: DB_HOST/POSTGRES_HOST, DB_NAME/POSTGRES_DB, DB_USER/POSTGRES_USER, DB_PASSWORD/POSTGRES_PASSWORD")
  }

  list(
    host = src_host,
    port = as.integer(if (src_port == "") 5432 else src_port),
    dbname = src_db,
    user = src_user,
    password = src_password
  )
}

# -----------------------------------------------------------------------------
# Database connections
# -----------------------------------------------------------------------------
connect_source <- function(src_cfg) {
  tryCatch({
    DBI::dbConnect(
      RPostgres::Postgres(),
      host = src_cfg$host,
      port = src_cfg$port,
      dbname = src_cfg$dbname,
      user = src_cfg$user,
      password = src_cfg$password
    )
  }, error = function(e) {
    stop(glue("Failed to connect to SOURCE database: {e$message}"))
  })
}

connect_dest <- function() {
  tryCatch({
    DBI::dbConnect(
      RPostgres::Postgres(),
      host = DEST_HOST,
      port = DEST_PORT,
      dbname = DEST_DB,
      user = DEST_USER,
      password = DEST_PASSWORD
    )
  }, error = function(e) {
    stop(glue("Failed to connect to DESTINATION database: {e$message}"))
  })
}

# -----------------------------------------------------------------------------
# Introspect table structure from source
# -----------------------------------------------------------------------------
get_table_definition <- function(conn, table_name) {
  q <- "
    SELECT column_name, data_type, character_maximum_length,
           numeric_precision, numeric_scale, is_nullable, column_default
    FROM information_schema.columns
    WHERE table_schema = 'public' AND table_name = $1
    ORDER BY ordinal_position
  "
  cols <- DBI::dbGetQuery(conn, q, params = list(table_name))
  if (nrow(cols) == 0) {
    stop(glue("Table {table_name} not found in source database"))
  }

  defs <- apply(cols, 1, function(r) {
    dt <- r["data_type"]
    if (dt %in% c("character varying", "varchar")) {
      len <- r["character_maximum_length"]
      if (!is.na(len) && len != "") {
        glue("{r['column_name']} VARCHAR({len})")
      } else {
        glue("{r['column_name']} VARCHAR")
      }
    } else if (dt == "timestamp with time zone") {
      glue("{r['column_name']} TIMESTAMPTZ")
    } else if (dt == "timestamp without time zone") {
      glue("{r['column_name']} TIMESTAMP")
    } else if (dt == "double precision") {
      glue("{r['column_name']} DOUBLE PRECISION")
    } else if (dt == "real") {
      glue("{r['column_name']} REAL")
    } else if (dt == "integer") {
      glue("{r['column_name']} INTEGER")
    } else if (dt == "bigint") {
      glue("{r['column_name']} BIGINT")
    } else if (dt == "smallint") {
      glue("{r['column_name']} SMALLINT")
    } else if (dt == "boolean") {
      glue("{r['column_name']} BOOLEAN")
    } else if (dt == "text") {
      glue("{r['column_name']} TEXT")
    } else if (dt == "date") {
      glue("{r['column_name']} DATE")
    } else if (dt == "numeric") {
      prec <- r["numeric_precision"]
      scale <- r["numeric_scale"]
      if (!is.na(prec) && prec != "") {
        glue("{r['column_name']} NUMERIC({prec},{scale})")
      } else {
        glue("{r['column_name']} NUMERIC")
      }
    } else {
      glue("{r['column_name']} {toupper(dt)}")
    }
  })
  paste(defs, collapse = ", ")
}

# -----------------------------------------------------------------------------
# Checkpoint system
# -----------------------------------------------------------------------------
read_checkpoint <- function() {
  if (!file.exists(CHECKPOINT_FILE)) {
    return(list())
  }
  lines <- readLines(CHECKPOINT_FILE, warn = FALSE)
  out <- list()
  for (l in lines) {
    l <- trimws(l)
    if (l == "") next
    parts <- strsplit(l, ":", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      tbl <- trimws(parts[1])
      chunk <- as.integer(trimws(parts[2]))
      out[[tbl]] <- chunk
    }
  }
  out
}

write_checkpoint <- function(table_name, last_chunk) {
  cp <- read_checkpoint()
  cp[[table_name]] <- last_chunk
  lines <- sapply(names(cp), function(t) glue("{t}:{cp[[t]]}"))
  writeLines(lines, CHECKPOINT_FILE)
}

# -----------------------------------------------------------------------------
# Migrate small table (full copy)
# -----------------------------------------------------------------------------
migrate_small_table <- function(src_conn, dest_conn, table_name) {
  cli_alert_info("Migrating small table: {table_name}")

  def <- get_table_definition(src_conn, table_name)
  create_sql <- glue("CREATE TABLE IF NOT EXISTS {table_name} ({def})")
  tryCatch({
    DBI::dbExecute(dest_conn, glue("DROP TABLE IF EXISTS {table_name} CASCADE"))
    DBI::dbExecute(dest_conn, create_sql)
  }, error = function(e) {
    stop(glue("Failed to create table {table_name}: {e$message}"))
  })

  data <- tryCatch({
    DBI::dbGetQuery(src_conn, glue("SELECT * FROM {table_name}"))
  }, error = function(e) {
    stop(glue("Failed to read table {table_name}: {e$message}"))
  })

  n <- nrow(data)
  if (n > 0) {
    tryCatch({
      DBI::dbWriteTable(dest_conn, table_name, data, append = TRUE, row.names = FALSE)
    }, error = function(e) {
      stop(glue("Failed to write table {table_name}: {e$message}"))
    })
  }

  write_checkpoint(table_name, -1L)  # -1 = fully migrated for small tables
  assign("total_rows", get("total_rows", envir = .GlobalEnv) + n, envir = .GlobalEnv)
  cli_alert_success("{table_name}: {n} rows copied")
  n
}

# -----------------------------------------------------------------------------
# Migrate large table (chunked, hypertable)
# -----------------------------------------------------------------------------
migrate_large_table <- function(src_conn, dest_conn, table_name) {
  chunk_size <- CHUNK_SIZE[[table_name]]
  if (is.null(chunk_size)) chunk_size <- 500000

  cli_alert_info("Migrating large table: {table_name} (chunk size: {chunk_size})")

  total_count <- tryCatch({
    as.integer(DBI::dbGetQuery(src_conn, glue("SELECT COUNT(*) FROM {table_name}"))[1, 1])
  }, error = function(e) {
    stop(glue("Failed to count rows in {table_name}: {e$message}"))
  })

  num_chunks <- ceiling(total_count / chunk_size)
  checkpoint <- read_checkpoint()
  start_chunk <- if (is.null(checkpoint[[table_name]])) 1 else checkpoint[[table_name]] + 1
  is_resuming <- start_chunk > 1

  # Skip if already complete (avoids R's 67:66 iterating as 67,66 and re-processing last chunk)
  if (start_chunk > num_chunks) {
    cli_alert_info("Skipping {table_name} (already migrated)")
    return(0L)
  }

  if (!is_resuming) {
    mig_log("Creating table ", table_name, " (DROP + CREATE + hypertable)...")
    def <- tryCatch(get_table_definition(src_conn, table_name), error = function(e) {
      mig_log("ERROR get_table_definition ", table_name, ": ", e$message)
      stop(glue("Failed to get table definition for {table_name}: {e$message}"))
    })
    create_sql <- glue("CREATE TABLE IF NOT EXISTS {table_name} ({def})")
    tryCatch({
      DBI::dbExecute(dest_conn, glue("DROP TABLE IF EXISTS {table_name} CASCADE"))
      DBI::dbExecute(dest_conn, create_sql)
    }, error = function(e) {
      mig_log("ERROR creating table ", table_name, ": ", e$message)
      stop(glue("Failed to create table {table_name}: {e$message}"))
    })
    mig_log("Table ", table_name, " created. Converting to hypertable...")
    tryCatch({
      DBI::dbExecute(dest_conn, glue(
        "SELECT create_hypertable('{table_name}', 'fecha', chunk_time_interval => INTERVAL '1 year')"
      ))
    }, error = function(e) {
      mig_log("ERROR create_hypertable ", table_name, ": ", e$message)
      stop(glue("Failed to create hypertable {table_name}: {e$message}"))
    })
    mig_log("Hypertable ", table_name, " ready. Starting chunk copy...")
  }

  rows_copied <- 0
  for (chunk_num in start_chunk:num_chunks) {
    offset <- (chunk_num - 1) * chunk_size
    data <- tryCatch({
      DBI::dbGetQuery(src_conn, glue(
        "SELECT * FROM {table_name} ORDER BY id OFFSET {offset} LIMIT {chunk_size}"
      ))
    }, error = function(e) {
      stop(glue("Failed to read chunk {chunk_num} of {table_name}: {e$message}"))
    })

    n <- nrow(data)
    if (n > 0) {
      tryCatch({
        DBI::dbWriteTable(dest_conn, table_name, data, append = TRUE, row.names = FALSE)
      }, error = function(e) {
        stop(glue("Failed to write chunk {chunk_num} of {table_name}: {e$message}"))
      })
    }

    rows_copied <- rows_copied + n
    write_checkpoint(table_name, chunk_num)
    assign("total_rows", get("total_rows", envir = .GlobalEnv) + n, envir = .GlobalEnv)

    cli_alert("table: {table_name} chunk {chunk_num}/{num_chunks} complete ({n} rows)")
    rm(data)
    gc(verbose = FALSE)
  }

  cli_alert_success("{table_name}: {rows_copied} rows migrated in {num_chunks} chunks")
  rows_copied
}

# -----------------------------------------------------------------------------
# Create indexes
# -----------------------------------------------------------------------------
create_indexes <- function(dest_conn) {
  indexes <- list(
    observacion = c("id_estacion", "id_variable"),
    observacion_final = c("id_estacion", "id_variable"),
    observacion_preliminar = c("id_estacion"),
    observacion_modelo = c("id_modelo", "id_estacion")
  )

  for (tbl in names(indexes)) {
    for (col in indexes[[tbl]]) {
      idx_name <- glue("idx_{tbl}_{col}")
      tryCatch({
        DBI::dbExecute(dest_conn, glue("CREATE INDEX IF NOT EXISTS {idx_name} ON {tbl} ({col})"))
        cli_alert_success("Created index: {idx_name}")
      }, error = function(e) {
        cli_alert_warning("Could not create index {idx_name}: {e$message}")
      })
    }
  }
}

# -----------------------------------------------------------------------------
# Run remaining tables only (observacion_preliminar, observacion_modelo)
# Call: main_remaining() instead of main()
# -----------------------------------------------------------------------------
main_remaining <- function() {
  REMAINING <- c("observacion_preliminar", "observacion_modelo")
  start_time <- Sys.time()
  assign("total_rows", 0L, envir = .GlobalEnv)

  tryCatch({
    cli_h1("Migrate remaining tables")
    src_cfg <- load_env()
    src_conn <- connect_source(src_cfg)
    on.exit(DBI::dbDisconnect(src_conn), add = TRUE)
    dest_conn <- connect_dest()
    on.exit(DBI::dbDisconnect(dest_conn), add = TRUE)

    tryCatch({
      DBI::dbExecute(dest_conn, "CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE")
    }, error = function(e) {
      stop(glue("TimescaleDB not available: {e$message}"))
    })

    for (tbl in REMAINING) {
      migrate_large_table(src_conn, dest_conn, tbl)
    }

    cli_h2("Creating indexes")
    create_indexes(dest_conn)

    elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
    total <- get("total_rows", envir = .GlobalEnv)
    cli_h1("Complete")
    cli_alert_success("Rows copied: {format(total, big.mark = ',')} in {elapsed}s")
  }, error = function(e) {
    mig_log("FATAL: ", e$message)
    cli_alert_danger("{e$message}")
    stop(e)
  })
}

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------
main <- function() {
  start_time <- Sys.time()
  assign("total_rows", 0L, envir = .GlobalEnv)
  tables_migrated <- character(0)

  tryCatch({
    cli_h1("TimescaleDB Migration")
    cli_alert_info("Loading environment...")
    src_cfg <- load_env()

    cli_alert_info("Connecting to databases...")
    src_conn <- connect_source(src_cfg)
    on.exit(DBI::dbDisconnect(src_conn), add = TRUE)
    dest_conn <- connect_dest()
    on.exit(DBI::dbDisconnect(dest_conn), add = TRUE)

    tryCatch({
      DBI::dbExecute(dest_conn, "CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE")
    }, error = function(e) {
      stop(glue("TimescaleDB extension not available: {e$message}. Ensure TimescaleDB container is running."))
    })

    checkpoint <- read_checkpoint()
    for (tbl in SMALL_TABLES) {
      if (!is.null(checkpoint[[tbl]]) && checkpoint[[tbl]] == -1L) {
        cli_alert_info("Skipping {tbl} (already migrated)")
        next
      }
      migrate_small_table(src_conn, dest_conn, tbl)
      tables_migrated <- c(tables_migrated, tbl)
    }

    for (tbl in LARGE_TABLES) {
      migrate_large_table(src_conn, dest_conn, tbl)
      tables_migrated <- c(tables_migrated, tbl)
    }

    cli_h2("Creating indexes")
    create_indexes(dest_conn)

    elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
    total <- get("total_rows", envir = .GlobalEnv)

    cli_h1("Migration Complete")
    cli_alert_success("Tables migrated: {paste(tables_migrated, collapse = ', ')}")
    cli_alert_success("Total rows copied: {format(total, big.mark = ',')}")
    cli_alert_success("Total time elapsed: {elapsed} seconds")

  }, error = function(e) {
    cli_alert_danger("Migration failed: {e$message}")
    stop(e)
  })
}

# Run with --remaining to migrate only observacion_preliminar and observacion_modelo
args <- commandArgs(trailingOnly = TRUE)
if ("--remaining" %in% args) main_remaining() else main()
