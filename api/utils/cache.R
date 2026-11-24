# Cache-aside con Redis para respuestas idempotentes (GET).
# Degrada con gracia: si Redis no esta disponible, ejecuta el computo igual.

.REDIS <- NULL
.REDIS_TRIED <- FALSE

redis_conn <- function() {
  if (.REDIS_TRIED) return(.REDIS)
  .REDIS_TRIED <<- TRUE
  if (!requireNamespace("redux", quietly = TRUE)) return(NULL)
  host <- Sys.getenv("REDIS_HOST", "redis")
  port <- as.integer(Sys.getenv("REDIS_PORT", "6379"))
  .REDIS <<- tryCatch(redux::hiredis(host = host, port = port), error = function(e) NULL)
  .REDIS
}

# TTL por defecto (segundos). Configurable via env CACHE_TTL.
cache_ttl_default <- function() {
  as.integer(Sys.getenv("CACHE_TTL", "600"))
}

# Construye una clave estable a partir de un prefijo y pares nombrados.
cache_key <- function(prefix, ...) {
  parts <- list(...)
  if (length(parts) == 0) return(prefix)
  vals <- vapply(parts, function(x) {
    if (is.null(x) || length(x) == 0) return("")
    paste(as.character(x), collapse = ",")
  }, character(1))
  paste0(prefix, ":", paste(vals, collapse = ":"))
}

.cache_decode_hit <- function(hit) {
  if (is.null(hit)) return(NULL)
  txt <- if (is.raw(hit)) rawToChar(hit) else hit
  tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
}

# Modo `encode`: se cachea la respuesta ya serializada y se devuelve tal cual.
.cache_raw_hit <- function(hit) {
  if (is.null(hit)) return(NULL)
  if (is.raw(hit)) rawToChar(hit) else as.character(hit)
}

# El lock debe sobrevivir al compute: si el que espera se rinde antes, todos
# recomputan a la vez y amplifican la carga en vez de contenerla (con el reporte
# a 30 s eran N computos de 30 s sobre 2 workers de R).
CACHE_LOCK_TTL_SEC <- 60L
# Tope de espera. El reporte entra por el location regex de nginx, que no fija
# proxy_read_timeout y por tanto corta a los 60 s: esperar mas devuelve un 504
# en vez de la respuesta.
CACHE_WAIT_MAX_SEC <- 25L

.cache_wait_for_key <- function(r, key, decode = .cache_decode_hit,
                                timeout_sec = CACHE_WAIT_MAX_SEC, pause_sec = 0.1) {
  lock_key <- paste0("sf:", key)
  deadline <- Sys.time() + timeout_sec
  repeat {
    Sys.sleep(pause_sec)
    val <- decode(tryCatch(r$GET(key), error = function(e) NULL))
    if (!is.null(val)) return(val)
    # cache_or_compute libera el lock despues de escribir la clave, asi que un
    # lock ausente sin valor significa que el holder murio: computar ya en vez
    # de agotar el presupuesto entero.
    held <- isTRUE(tryCatch(as.integer(r$EXISTS(lock_key)) > 0L, error = function(e) TRUE))
    if (!held) return(decode(tryCatch(r$GET(key), error = function(e) NULL)))
    if (Sys.time() >= deadline) return(NULL)
  }
}

# Patron principal: devuelve cache si existe, si no computa, guarda y devuelve.
# Singleflight: un solo compute por key en MISS (SET NX lock); el resto espera el SET.
# `compute` es una funcion sin argumentos que retorna un objeto serializable.
# `encode`: si se pasa un serializador, se cachea y devuelve el texto ya
# serializado en vez del objeto R. Evita el round-trip fromJSON+toJSON del HIT,
# que ademas hacia que HIT y MISS no devolvieran los mismos bytes.
cache_or_compute <- function(key, compute, ttl = cache_ttl_default(), res = NULL,
                             encode = NULL) {
  decode <- if (is.null(encode)) .cache_decode_hit else .cache_raw_hit
  r <- redis_conn()
  t_cache <- Sys.time()
  if (!is.null(r)) {
    hit <- tryCatch(r$GET(key), error = function(e) NULL)
    val <- decode(hit)
    if (!is.null(val)) {
      if (!is.null(res)) tryCatch(res$setHeader("X-Cache", "HIT"), error = function(e) NULL)
      if (exists("app_profile_record_cache", mode = "function")) {
        tryCatch(
          app_profile_record_cache(
            TRUE, key,
            dur_ms = as.numeric(difftime(Sys.time(), t_cache, units = "secs")) * 1000
          ),
          error = function(e) NULL
        )
      }
      return(val)
    }
  }

  lock_key <- paste0("sf:", key)
  got_lock <- FALSE
  if (!is.null(r)) {
    # EX 30s: si el holder muere, no bloquea forever
    got_lock <- isTRUE(tryCatch(
      identical(r$SET(lock_key, "1", NX = TRUE, EX = CACHE_LOCK_TTL_SEC), "OK"),
      error = function(e) FALSE
    ))
    if (!got_lock) {
      waited <- .cache_wait_for_key(r, key, decode = decode)
      if (!is.null(waited)) {
        if (!is.null(res)) tryCatch(res$setHeader("X-Cache", "HIT"), error = function(e) NULL)
        if (exists("app_profile_record_cache", mode = "function")) {
          tryCatch(
            app_profile_record_cache(TRUE, key, dur_ms = 0),
            error = function(e) NULL
          )
        }
        return(waited)
      }
      # Timeout esperando lock: computar igual (mejor respuesta que error).
    }
  }

  t_miss <- Sys.time()
  val <- compute()
  # Un error de negocio no se cachea: se devuelve el objeto para que el endpoint
  # pueda fijar el status HTTP.
  is_error <- is.list(val) && !is.null(val$error)
  if (!is.null(encode) && !is_error && !is.null(val)) {
    val <- tryCatch(as.character(encode(val)), error = function(e) NULL)
  }
  if (!is.null(r) && !is.null(val) && !is_error) {
    payload <- if (is.null(encode)) {
      tryCatch(jsonlite::toJSON(val, auto_unbox = TRUE, dataframe = "rows", na = "null"),
               error = function(e) NULL)
    } else {
      val
    }
    if (!is.null(payload)) {
      tryCatch(r$SET(key, charToRaw(payload), EX = ttl), error = function(e) NULL)
    }
  }
  if (got_lock && !is.null(r)) {
    tryCatch(r$DEL(lock_key), error = function(e) NULL)
  }
  if (!is.null(res)) tryCatch(res$setHeader("X-Cache", "MISS"), error = function(e) NULL)
  if (exists("app_profile_record_cache", mode = "function")) {
    tryCatch(
      app_profile_record_cache(
        FALSE, key,
        dur_ms = as.numeric(difftime(Sys.time(), t_miss, units = "secs")) * 1000
      ),
      error = function(e) NULL
    )
  }
  val
}

# Invalida por patron (usar tras ingesta de datos nuevos).
# ponytail: KEYS en lugar de SCAN (redux hiredis no expone SCAN usable aqui);
# patrones acotados p.ej. pronostico:* -> decenas de claves max.
cache_invalidate <- function(pattern) {
  r <- redis_conn()
  if (is.null(r)) return(invisible(FALSE))
  tryCatch({
    keys <- unlist(r$KEYS(pattern), use.names = FALSE)
    if (length(keys) == 0) return(invisible(TRUE))
    for (k in keys) r$DEL(k)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}
