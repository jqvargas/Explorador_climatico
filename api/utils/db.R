# Acceso a base de datos con pool de conexiones (si 'pool' esta disponible).
# Centraliza el acceso para que los endpoints no gestionen conexiones a mano.

.DB_POOL <- NULL
.DB_POOL_TRIED <- FALSE

db_conn_args <- function() {
  list(
    drv = RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS")
  )
}

db_pool <- function() {
  if (.DB_POOL_TRIED) return(.DB_POOL)
  .DB_POOL_TRIED <<- TRUE
  if (!requireNamespace("pool", quietly = TRUE)) return(NULL)
  args <- db_conn_args()
  .DB_POOL <<- tryCatch(
    do.call(pool::dbPool, c(args, list(
      minSize = as.integer(Sys.getenv("DB_POOL_MIN", "1")),
      maxSize = as.integer(Sys.getenv("DB_POOL_MAX", "10")),
      idleTimeout = 60
    ))),
    error = function(e) NULL
  )
  .DB_POOL
}

# Devuelve una conexion lista para usar. Con pool hace checkout; sin pool conecta directo.
get_db_connection <- function() {
  p <- db_pool()
  if (!is.null(p)) return(pool::poolCheckout(p))
  do.call(DBI::dbConnect, db_conn_args())
}

# Libera la conexion segun su origen (pool o directa).
db_release <- function(conn) {
  if (is.null(conn)) return(invisible(NULL))
  if (!is.null(.DB_POOL)) {
    tryCatch(pool::poolReturn(conn), error = function(e) tryCatch(DBI::dbDisconnect(conn), error = function(e2) NULL))
  } else {
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL)
  }
  invisible(NULL)
}

# Helper de consulta: abre/cierra conexion y ejecuta SELECT. Proyeccion la define el SQL.
db_query <- function(q, params = NULL) {
  conn <- get_db_connection()
  on.exit(db_release(conn))
  t0 <- Sys.time()
  out <- if (is.null(params) || length(params) == 0) {
    DBI::dbGetQuery(conn, q)
  } else {
    DBI::dbGetQuery(conn, q, params = params)
  }
  if (exists("app_profile_record_db", mode = "function")) {
    tryCatch(
      app_profile_record_db(
        op = "query", sql = q,
        n_params = if (is.null(params)) 0L else length(params),
        rows = if (is.data.frame(out)) nrow(out) else NULL,
        dur_ms = as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
      ),
      error = function(e) NULL
    )
  }
  out
}

# Helper de ejecucion (INSERT/UPDATE/DELETE).
db_execute <- function(q, params = NULL) {
  conn <- get_db_connection()
  on.exit(db_release(conn))
  t0 <- Sys.time()
  out <- if (is.null(params) || length(params) == 0) {
    DBI::dbExecute(conn, q)
  } else {
    DBI::dbExecute(conn, q, params = params)
  }
  if (exists("app_profile_record_db", mode = "function")) {
    tryCatch(
      app_profile_record_db(
        op = "execute", sql = q,
        n_params = if (is.null(params)) 0L else length(params),
        rows = suppressWarnings(as.integer(out)[1]),
        dur_ms = as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
      ),
      error = function(e) NULL
    )
  }
  out
}

# ponytail: RPostgres/DBI no acepta NULL con length 0 en bind; usar NA tipado.
pg_na_int <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_integer_)
  v <- suppressWarnings(as.integer(x)[1])
  if (is.na(v)) NA_integer_ else v
}

pg_na_dbl <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  v <- suppressWarnings(as.numeric(x)[1])
  if (is.na(v)) NA_real_ else v
}

pg_text_param <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  v <- trimws(as.character(x)[1])
  if (!nzchar(v)) NA_character_ else v
}

pg_text_array_param <- function(vals) {
  if (is.null(vals) || length(vals) == 0) return("{}")
  vals <- as.character(unlist(vals))
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) return("{}")
  paste0("{", paste(vals, collapse = ","), "}")
}

pg_int_array_param <- function(vals) {
  if (is.null(vals) || length(vals) == 0) return(NA_character_)
  vals <- unique(as.integer(unlist(vals)))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_character_)
  paste0("{", paste(vals, collapse = ","), "}")
}

pg_uuid_array_param <- function(vals) {
  if (is.null(vals) || length(vals) == 0) return("{}")
  vals <- trimws(as.character(unlist(vals)))
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) return("{}")
  paste0("{", paste(vals, collapse = ","), "}")
}
