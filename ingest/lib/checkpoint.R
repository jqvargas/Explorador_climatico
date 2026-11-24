# Checkpoint de ingesta diaria (resume por estacion/variable).
# ponytail: archivo texto clave=valor + mutex dir; upgrade = SQLite.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

ckpt_flag_true <- function(name) {
  tolower(Sys.getenv(name, "")) %in% c("1", "true", "yes", "si")
}

ckpt_path <- function() {
  ingest_dir <- get("INGEST_DIR", envir = .GlobalEnv)
  file.path(ingest_dir, "logs", "actualizar_diario.checkpoint")
}

ckpt_lock_path <- function() paste0(ckpt_path(), ".lockdir")

# Mutex via mkdir atomico (funciona en Linux container y Windows NTFS).
ckpt_with_lock <- function(expr) {
  lp <- ckpt_lock_path()
  deadline <- Sys.time() + 30
  while (Sys.time() < deadline) {
    if (dir.create(lp, showWarnings = FALSE)) {
      on.exit(unlink(lp, recursive = TRUE), add = TRUE)
      return(force(expr))
    }
    Sys.sleep(0.05)
  }
  warning("[ckpt] no se pudo adquirir lock en 30s; escribiendo sin lock")
  force(expr)
}

ckpt_enabled <- function() {
  !ckpt_flag_true("INGEST_DRY_RUN") &&
    !ckpt_flag_true("INGEST_RESET_CHECKPOINT") &&
    !identical(Sys.getenv("INGEST_MODO", "completo"), "test")
}

# Lectura tolerante: DGA∥DMC en paralelo + bind mount Windows pueden
# fallar con "cannot open the connection" a mitad de un rename.
ckpt_read <- function(retries = 8L, sleep_sec = 0.05) {
  path <- ckpt_path()
  if (!file.exists(path)) return(list())
  retries <- as.integer(retries)[1]
  if (is.na(retries) || retries < 1L) retries <- 1L
  last_err <- NULL
  for (attempt in seq_len(retries)) {
    lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) e)
    if (!inherits(lines, "error")) {
      out <- list()
      for (l in lines) {
        l <- trimws(l)
        if (l == "" || startsWith(l, "#")) next
        parts <- strsplit(l, "=", fixed = TRUE)[[1]]
        if (length(parts) < 2L) next
        key <- trimws(parts[1])
        val <- trimws(paste(parts[-1], collapse = "="))
        out[[key]] <- val
      }
      return(out)
    }
    last_err <- lines
    if (attempt < retries) Sys.sleep(sleep_sec * attempt)
  }
  stop(last_err)
}

ckpt_write <- function(fields) {
  if (!ckpt_enabled()) return(invisible(NULL))
  ckpt_with_lock({
    cur <- ckpt_read()
    for (nm in names(fields)) {
      v <- fields[[nm]]
      if (is.null(v) || (length(v) == 1L && is.na(v))) {
        cur[[nm]] <- NULL
      } else {
        cur[[nm]] <- as.character(v)
      }
    }
    dir.create(dirname(ckpt_path()), recursive = TRUE, showWarnings = FALSE)
    keys <- names(cur)
    lines <- vapply(keys, function(k) paste0(k, "=", cur[[k]]), character(1))
    tmp <- paste0(ckpt_path(), ".tmp")
    writeLines(lines, tmp)
    file.rename(tmp, ckpt_path())
    invisible(cur)
  })
}

ckpt_clear <- function() {
  path <- ckpt_path()
  if (file.exists(path)) file.remove(path)
  invisible(TRUE)
}

ckpt_completed_vars <- function(cp = NULL) {
  cp <- cp %||% ckpt_read()
  raw <- cp[["completed_vars"]] %||% ""
  if (!nzchar(raw)) return(character(0))
  strsplit(raw, ",", fixed = TRUE)[[1]]
}

ckpt_timeout_codes <- function(cp = NULL) {
  cp <- cp %||% ckpt_read()
  raw <- cp[["timeout_vars"]] %||% ""
  if (!nzchar(raw)) return(character(0))
  strsplit(raw, ",", fixed = TRUE)[[1]]
}

ckpt_var_key <- function(fuente, variable) paste0(fuente, "/", variable)

ckpt_variable_done <- function(fuente, variable, cp = NULL) {
  ckpt_var_key(fuente, variable) %in% ckpt_completed_vars(cp)
}

ckpt_init_run <- function(run_id, fecha_ini, fecha_fin) {
  if (!ckpt_enabled()) return(invisible(ckpt_read()))
  cp <- ckpt_read()
  ini_s <- format(as.Date(fecha_ini), "%Y-%m-%d")
  fin_s <- format(as.Date(fecha_fin), "%Y-%m-%d")
  if (length(cp) == 0L) {
    ckpt_write(list(
      run_id = run_id,
      fecha_ini = ini_s,
      fecha_fin = fin_s,
      pipeline_step = "ingest",
      fuente = "",
      variable = "",
      estacion_idx = "0",
      estacion_codigo = "",
      last_progress_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      timeout_vars = ""
    ))
    return(invisible(ckpt_read()))
  }
  raw_fin <- cp[["fecha_fin"]] %||% ""
  fin_cp <- if (nzchar(raw_fin)) suppressWarnings(as.Date(raw_fin)) else as.Date(NA)
  stale_fin <- length(fin_cp) != 1L || is.na(fin_cp) || fin_cp < Sys.Date()
  if (stale_fin) {
    message(
      "[ckpt] fecha_fin ",
      if (is.na(fin_cp) || length(fin_cp) != 1L) "(ausente)" else fin_cp,
      " < hoy o invalida; ventana nueva ", ini_s, " -> ", fin_s,
      " (limpiando completed_vars)."
    )
    ckpt_write(list(
      run_id = as.character(run_id),
      fecha_ini = ini_s,
      fecha_fin = fin_s,
      pipeline_step = "ingest",
      completed_vars = "",
      timeout_vars = "",
      fuente = "",
      variable = "",
      estacion_idx = "0",
      estacion_codigo = "",
      last_progress_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ))
  } else if (!identical(cp[["fecha_ini"]] %||% "", ini_s) ||
             !identical(cp[["fecha_fin"]] %||% "", fin_s)) {
    if (length(ckpt_completed_vars(cp)) == 0L) {
      message("[ckpt] Actualizando rango ", ini_s, " -> ", fin_s)
      ckpt_write(list(
        run_id = as.character(run_id),
        fecha_ini = ini_s,
        fecha_fin = fin_s,
        pipeline_step = "ingest"
      ))
    }
  }
  if (!identical(cp[["run_id"]] %||% "", as.character(run_id))) {
    run_day <- substr(cp[["run_id"]] %||% "", 1, 8)
    today <- format(Sys.Date(), "%Y%m%d")
    if (!identical(run_day, today) && length(ckpt_completed_vars(cp)) > 0L) {
      if (stale_fin) {
        message("[ckpt] WARN checkpoint incompleto de otro dia (", cp[["run_id"]], "); vars reseteadas.")
      } else {
        message("[ckpt] WARN checkpoint incompleto de otro dia (", cp[["run_id"]], "); resumiendo.")
      }
    }
  }
  invisible(ckpt_read())
}

ckpt_get_fechas <- function() {
  cp <- ckpt_read()
  if (length(cp) == 0L) return(NULL)
  raw_ini <- cp[["fecha_ini"]]
  raw_fin <- cp[["fecha_fin"]]
  if (is.null(raw_ini) || is.null(raw_fin) || !nzchar(raw_ini) || !nzchar(raw_fin)) {
    return(NULL)
  }
  ini <- suppressWarnings(as.Date(raw_ini))
  fin <- suppressWarnings(as.Date(raw_fin))
  if (length(ini) != 1L || length(fin) != 1L || is.na(ini) || is.na(fin)) return(NULL)
  list(ini = ini, fin = fin)
}

ckpt_begin_variable <- function(fuente, variable) {
  if (!ckpt_enabled()) return(invisible(NULL))
  ckpt_write(list(
    pipeline_step = "ingest",
    fuente = fuente,
    variable = variable,
    estacion_idx = "0",
    estacion_codigo = "",
    last_progress_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ))
}

ckpt_resume_estacion_idx <- function(fuente, variable) {
  if (!ckpt_enabled()) return(NA_integer_)
  cp <- ckpt_read()
  if (!identical(cp[["fuente"]] %||% "", fuente)) return(NA_integer_)
  if (!identical(cp[["variable"]] %||% "", variable)) return(NA_integer_)
  as.integer(cp[["estacion_idx"]] %||% 0L)
}

ckpt_should_skip_station <- function(fuente, variable, idx, resume_idx = NULL) {
  if (!ckpt_enabled()) return(FALSE)
  # Gapfill re-filtra por BD (fecha_fin); el idx posicional miente si la lista encoge.
  if (tolower(Sys.getenv("INGEST_GAPFILL", "")) %in% c("1", "true", "yes", "si")) {
    return(FALSE)
  }
  # resume_idx opcional: el loop paralelo debe cachearlo (1 lectura, no N).
  if (is.null(resume_idx)) {
    resume_idx <- ckpt_resume_estacion_idx(fuente, variable)
  }
  resume_idx <- suppressWarnings(as.integer(resume_idx)[1])
  if (is.na(resume_idx) || resume_idx <= 0L) return(FALSE)
  idx <= resume_idx
}

# Avance de una tanda con dos variables. estacion_idx del ckpt es una sola
# variable; estas claves no se pisan entre pp y t_min.
ckpt_batch_idx_key <- function(fuente, variable) {
  paste0("batch_idx_", fuente, "_", variable)
}

ckpt_batch_idx_get <- function(fuente, variable) {
  if (!ckpt_enabled()) return(0L)
  raw <- ckpt_read()[[ckpt_batch_idx_key(fuente, variable)]]
  n <- suppressWarnings(as.integer(raw))
  if (length(n) != 1L || is.na(n) || n < 0L) 0L else n
}

ckpt_batch_idx_set <- function(fuente, variable, idx, codigo = "") {
  if (!ckpt_enabled()) return(invisible(NULL))
  fields <- list(
    pipeline_step = "ingest",
    last_progress_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  fields[[ckpt_batch_idx_key(fuente, variable)]] <- as.character(as.integer(idx))
  ckpt_write(fields)
  invisible(NULL)
}

ckpt_mark_station_done <- function(fuente, variable, idx, codigo = "", timeout = FALSE) {
  if (!ckpt_enabled()) return(invisible(NULL))
  fields <- list(
    pipeline_step = "ingest",
    fuente = fuente,
    variable = variable,
    estacion_idx = as.character(as.integer(idx)),
    estacion_codigo = as.character(codigo),
    last_progress_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  if (isTRUE(timeout) && nzchar(codigo)) {
    key <- paste0(ckpt_var_key(fuente, variable), ":", codigo)
    done <- ckpt_timeout_codes()
    if (!key %in% done) done <- c(done, key)
    fields$timeout_vars <- paste(done, collapse = ",")
  }
  ckpt_write(fields)
}

ckpt_mark_variable_done <- function(fuente, variable) {
  if (!ckpt_enabled()) return(invisible(NULL))
  done <- ckpt_completed_vars()
  key <- ckpt_var_key(fuente, variable)
  if (!key %in% done) done <- c(done, key)
  ckpt_write(list(
    completed_vars = paste(done, collapse = ","),
    fuente = "",
    variable = "",
    estacion_idx = "0",
    estacion_codigo = "",
    last_progress_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ))
}

ckpt_reset_variable_progress <- function() {
  if (!ckpt_enabled()) return(invisible(NULL))
  ckpt_write(list(
    completed_vars = "",
    timeout_vars = "",
    fuente = "",
    variable = "",
    estacion_idx = "0",
    estacion_codigo = ""
  ))
}

ckpt_ingest_complete <- function(expected_vars) {
  if (!ckpt_enabled()) return(TRUE)
  done <- ckpt_completed_vars()
  all(expected_vars %in% done)
}

ckpt_pipeline_mark <- function(step) {
  if (!ckpt_enabled()) return(invisible(NULL))
  ckpt_write(list(pipeline_step = step))
}

ckpt_pipeline_done <- function(step) {
  if (!ckpt_enabled()) return(invisible(NULL))
  cp <- ckpt_read()
  done <- cp[["pipeline_done"]] %||% ""
  parts <- if (nzchar(done)) strsplit(done, ",", fixed = TRUE)[[1]] else character(0)
  if (!step %in% parts) parts <- c(parts, step)
  ckpt_write(list(pipeline_step = step, pipeline_done = paste(parts, collapse = ",")))
}

ckpt_pipeline_step_done <- function(step) {
  cp <- ckpt_read()
  step %in% strsplit(cp[["pipeline_done"]] %||% "", ",", fixed = TRUE)[[1]]
}
