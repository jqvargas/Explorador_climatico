#!/usr/bin/env Rscript
# Orquestador diario: preflight + ingesta DGA+DMC (un post_ingesta) + verify.

fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
run_dir <- normalizePath(dirname(sub("^--file=", "", fa[1])), winslash = "/")
source(file.path(run_dir, "..", "lib", "bootstrap.R"))
ingest_setup_from_script()
source(file.path(run_dir, "..", "lib", "fechas_ingesta.R"))

t_inicio <- Sys.time()
metrics <- list(
  exit_code = 0L,
  run_id = NA_character_,
  started_at = format(t_inicio, "%Y-%m-%dT%H:%M:%S%z")
)

# u_max_diario pide la terna SNIA (u2_min/max/mean) en una sola pagina, como t_min para T.
DGA_VARS <- c("pp", "t_min", "u_max_diario")
DMC_VARS <- c("pp", "t_max", "t_min", "t_mean", "hr_mean", "rs_mean", "u_10_kt", "u_rafaga")
ALL_INGEST_VARS <- c(
  vapply(DGA_VARS, function(v) ckpt_var_key("dga", v), character(1)),
  vapply(DMC_VARS, function(v) ckpt_var_key("dmc", v), character(1))
)

log_paso <- function(n, msg) {
  message(sprintf("[%s] PASO %s — %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), n, msg))
}

ultima_fecha_observacion <- function(conn) {
  row <- DBI::dbGetQuery(conn, "SELECT MAX(fecha) AS ultima FROM bitacora_oficial")
  u <- row$ultima[1]
  if (is.null(u) || length(u) == 0 || is.na(u)) return(NA)
  as.Date(u)
}

if (ingest_flag_true("INGEST_RESET_CHECKPOINT")) {
  message("[ckpt] INGEST_RESET_CHECKPOINT=1; borrando checkpoint.")
  ckpt_clear()
  # Tras el clear, reactivar ckpt para poder resumir si esta corrida larga falla a medias.
  Sys.unsetenv("INGEST_RESET_CHECKPOINT")
}

# --- PASO 0: preflight ---
log_paso(0, "Preflight (BD + indice UPSERT)")
test_db_connection()
conn <- get_db_connection()
on.exit(tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL), add = TRUE)

if (!persist_unique_index_existe(conn)) {
  message("[FAIL] Falta idx_bitacora_oficial_unique_key.")
  quit(status = 1)
}
message("[OK] Indice UPSERT presente.")

# --- PASO 1: fechas ---
log_paso(1, "Calcular rango de fechas")
run_id <- format(Sys.time(), "%Y%m%dT%H%M%S")
# Prefer run_id del orquestador PS para unificar JSONL del perfil
env_rid <- Sys.getenv("INGEST_PROFILE_RUN_ID", "")
if (nzchar(env_rid)) run_id <- env_rid
metrics$run_id <- run_id
# Profile run (JSONL + summary para hub /profiler/)
profile_task <- Sys.getenv("INGEST_PROFILE_TASK", "")
if (!nzchar(profile_task)) {
  profile_task <- if (ingest_flag_true("INGEST_GAPFILL")) {
    "Rodaja_Actualizar_Resume"
  } else {
    "Rodaja_Actualizar_Diario"
  }
}
profile_mode <- Sys.getenv("INGEST_PROFILE_MODE", "")
if (!nzchar(profile_mode)) {
  profile_mode <- if (ingest_flag_true("INGEST_GAPFILL")) "watchdog" else "full"
}
if (exists("profile_prune_old", mode = "function")) {
  tryCatch(profile_prune_old(14L), error = function(e) NULL)
}
.profile_root_span <- NULL
if (exists("profile_run_begin", mode = "function")) {
  tryCatch({
    profile_run_begin(run_id, task = profile_task, mode = profile_mode)
    .profile_root_span <<- profile_span_begin(
      "r.ingest", "step",
      attrs = list(phase = "ingest", step = "ingest", gapfill = ingest_flag_true("INGEST_GAPFILL"))
    )
  }, error = function(e) message("[profile] WARN begin: ", e$message))
}
hoy <- ingest_hoy()
metrics$dia_ejecucion <- format(hoy, "%Y-%m-%d")
fechas_ckpt <- ckpt_get_fechas()
# Resume de fechas solo intradía (mismo dia_ejecucion). Nunca extender ini vieja a un
# mega-rango multi-día: eso cuelga DGA. Otro día → recalcular desde BD con cap.
resume_intradia <- !is.null(fechas_ckpt) &&
  !ingest_flag_true("INGEST_RESET_CHECKPOINT") &&
  identical(as.Date(fechas_ckpt$fin), hoy)

if (resume_intradia) {
  ini <- fechas_ckpt$ini
  fin <- fechas_ckpt$fin
  message("[ckpt] Resume intradía: ", ini, " -> ", fin, " (dia_ejecucion=", hoy, ")")
} else {
  if (!is.null(fechas_ckpt) && !ingest_flag_true("INGEST_RESET_CHECKPOINT")) {
    message(
      "[ckpt] Checkpoint de otro día (fin=", fechas_ckpt$fin,
      "); recalculando ventana (no se extiende ini vieja)."
    )
    ckpt_reset_variable_progress()
  }
  fechas <- ingest_fechas_diarias(conn)
  ini <- fechas$ini
  fin <- fechas$fin
  metrics$ultima_obs_al_inicio <- if (is.na(fechas$ultima)) NA_character_ else format(fechas$ultima, "%Y-%m-%d")
  metrics$cap_aplicado <- isTRUE(fechas$cap_aplicado)
  ckpt_init_run(run_id, ini, fin)
}
Sys.setenv(
  INGEST_FECHA_INI = format(ini, "%Y-%m-%d"),
  INGEST_FECHA_FIN = format(fin, "%Y-%m-%d")
)
modo <- Sys.getenv("INGEST_MODO", "completo")
dry <- ingest_flag_true("INGEST_DRY_RUN")
message("[ingest] modo=", modo, " dry_run=", dry)

ingest_fresh_enough <- function(conn, fin, min_rows = 500L) {
  n_fin <- DBI::dbGetQuery(
    conn, "SELECT COUNT(*) AS n FROM bitacora_oficial WHERE fecha = $1",
    params = list(format(as.Date(fin), "%Y-%m-%d"))
  )$n[1]
  as.integer(n_fin) >= min_rows
}

run_post_ingesta <- function() {
  if (dry) return(invisible(FALSE))
  log_paso(3, "post_ingesta unico (DGA+DMC)")
  t_refresh <- Sys.time()
  post_ingesta_observaciones(conn, total, fecha_min = ini, fecha_max = fin)
  metrics$duracion_refresh_estacion_calidad <<- round(
    as.numeric(difftime(Sys.time(), t_refresh, units = "mins")), 2
  )
  invisible(TRUE)
}

# ckpt_ingest_complete() es TRUE si el checkpoint esta deshabilitado (RESET/DRY);
# solo tratar "vars completas" cuando el ckpt esta activo de verdad.
vars_marked_done <- ckpt_enabled() && ckpt_ingest_complete(ALL_INGEST_VARS)
ingest_already_done <- vars_marked_done && ingest_fresh_enough(conn, fin)
if (vars_marked_done && !ingest_already_done) {
  # Sin esto, meteovision_run_fuente hace skip de todas las vars "completadas"
  # y la corrida termina en 0 filas con exit 0 (falso exito).
  message("[ckpt] Checkpoint completo pero fecha_fin=", fin, " con pocos datos; re-ingestando.")
  ckpt_reset_variable_progress()
}

# gap-fill: solo vars incompletas; knobs DGA = Diario (sleep 0.4) si no override
if (ingest_flag_true("INGEST_GAPFILL")) {
  message("[ingest] modo GAPFILL=1")
  if (!nzchar(Sys.getenv("INGEST_SLEEP_MIN_DGA", ""))) {
    Sys.setenv(INGEST_SLEEP_MIN_DGA = "0.4")
  }
  # No saltar scrape por "already done" completo: gapfill solo toca vars faltantes
  if (vars_marked_done && !ckpt_ingest_complete(ALL_INGEST_VARS)) {
    # noop
  }
}

# Fuente/vars filtradas en gapfill via INGEST_SCRAPE_FUENTE (ya) + skip completed in meteovision_run_fuente
# Si INGEST_GAPFILL_VARS=dga/pp,dga/t_min,dga/u_max_diario limita el set
gapfill_filter_vars <- function(fuente, vars) {
  raw <- Sys.getenv("INGEST_GAPFILL_VARS", "")
  if (!nzchar(raw) || !ingest_flag_true("INGEST_GAPFILL")) return(vars)
  want <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
  keep <- character(0)
  for (v in vars) {
    key <- ckpt_var_key(fuente, v)
    if (key %in% want) keep <- c(keep, v)
  }
  if (length(keep) == 0L) return(character(0))
  keep
}

# --- PASO 2-3: ingesta DGA + DMC (un solo post_ingesta) ---
ingest_fuente_call <- function(label, expr) {
  tryCatch(
    ingest_coerce_fuente_result(expr, label),
    error = function(e) {
      message("[ingest] ERROR ", label, ": ", conditionMessage(e))
      ingest_coerce_fuente_result(conditionMessage(e), label)
    }
  )
}

ingest_parallel_fuentes <- function() {
  raw <- tolower(Sys.getenv("INGEST_PARALLEL_FUENTES", "1"))
  !raw %in% c("0", "false", "no")
}

run_fuentes_sequential <- function() {
  dga_vars <- gapfill_filter_vars("dga", DGA_VARS)
  dmc_vars <- gapfill_filter_vars("dmc", DMC_VARS)
  only <- tolower(Sys.getenv("INGEST_SCRAPE_FUENTE", ""))
  res_dga <- list(n_inserted = 0L, n_estaciones = 0L, failed = character(0), warnings = character(0), http_stats = NULL)
  res_dmc <- list(n_inserted = 0L, n_estaciones = 0L, failed = character(0), warnings = character(0), http_stats = NULL)
  if (only %in% c("", "dga") && length(dga_vars) > 0L) {
    log_paso(2, "Ingesta DGA (secuencial)")
    res_dga <- ingest_fuente_call(
      "dga",
      meteovision_run_fuente(conn, "dga", dga_vars, ini, fin, modo, dry)
    )
  } else if (length(dga_vars) == 0L && ingest_flag_true("INGEST_GAPFILL")) {
    message("[gapfill] skip dga (sin vars pendientes)")
  }
  if (only %in% c("", "dmc") && length(dmc_vars) > 0L) {
    log_paso(3, "Ingesta DMC (secuencial)")
    res_dmc <- ingest_fuente_call(
      "dmc",
      meteovision_run_fuente(conn, "dmc", dmc_vars, ini, fin, modo, dry)
    )
  } else if (length(dmc_vars) == 0L && ingest_flag_true("INGEST_GAPFILL")) {
    message("[gapfill] skip dmc (sin vars pendientes)")
  }
  list(dga = res_dga, dmc = res_dmc)
}

# DGA∥DMC: fork en Linux (contenedor). Cada hijo abre su propio conn.
run_fuentes_parallel <- function() {
  if (.Platform$OS.type != "unix") {
    message("[ingest] parallel fuentes no disponible en este OS; secuencial")
    return(run_fuentes_sequential())
  }
  log_paso(2, "Ingesta DGA∥DMC (parallel)")
  # Cerrar conn del padre antes del fork (DBI no es fork-safe).
  tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL)
  j_dga <- parallel::mcparallel({
    tryCatch({
      c <- get_db_connection()
      on.exit(tryCatch(DBI::dbDisconnect(c), error = function(e) NULL), add = TRUE)
      Sys.setenv(INGEST_SKIP_POST_INGESTA = "1", INGEST_SCRAPE_FUENTE = "dga")
      dga_vars <- gapfill_filter_vars("dga", DGA_VARS)
      out <- if (length(dga_vars) == 0L) {
        list(n_inserted = 0L, n_estaciones = 0L, failed = character(0), warnings = character(0), http_stats = NULL)
      } else {
        meteovision_run_fuente(c, "dga", dga_vars, ini, fin, modo, dry)
      }
      ingest_coerce_fuente_result(out, "dga")
    }, error = function(e) {
      message("[ingest] ERROR DGA: ", conditionMessage(e))
      ingest_coerce_fuente_result(conditionMessage(e), "dga")
    })
  })
  j_dmc <- parallel::mcparallel({
    tryCatch({
      c <- get_db_connection()
      on.exit(tryCatch(DBI::dbDisconnect(c), error = function(e) NULL), add = TRUE)
      Sys.setenv(INGEST_SKIP_POST_INGESTA = "1", INGEST_SCRAPE_FUENTE = "dmc")
      dmc_vars <- gapfill_filter_vars("dmc", DMC_VARS)
      out <- if (length(dmc_vars) == 0L) {
        list(n_inserted = 0L, n_estaciones = 0L, failed = character(0), warnings = character(0), http_stats = NULL)
      } else {
        meteovision_run_fuente(c, "dmc", dmc_vars, ini, fin, modo, dry)
      }
      ingest_coerce_fuente_result(out, "dmc")
    }, error = function(e) {
      message("[ingest] ERROR DMC: ", conditionMessage(e))
      ingest_coerce_fuente_result(conditionMessage(e), "dmc")
    })
  })
  collected <- parallel::mccollect(list(j_dga, j_dmc))
  # Reabrir conn del padre para post_ingesta / verify
  conn <<- get_db_connection()
  list(
    dga = ingest_coerce_fuente_result(collected[[1]], "dga"),
    dmc = ingest_coerce_fuente_result(collected[[2]], "dmc")
  )
}

if (ingest_already_done) {
  log_paso(2, "Ingesta ya completada (checkpoint); omitiendo scrape")
  metrics$filas_afectadas_dga <- 0L
  metrics$filas_afectadas_dmc <- 0L
  metrics$estaciones_dga <- 0L
  metrics$estaciones_dmc <- 0L
  failed <- character(0)
  soft_warns <- character(0)
  total <- 0L
  if (!dry) run_post_ingesta()
} else {
  Sys.setenv(INGEST_SKIP_POST_INGESTA = "1")
  t_ingesta <- Sys.time()

  res_pair <- tryCatch(
    if (ingest_parallel_fuentes()) run_fuentes_parallel() else run_fuentes_sequential(),
    error = function(e) {
      message("[WARN] parallel fuentes fallo (", e$message, "); reintento secuencial")
      # Reconectar si el fork dejo conn muerto
      conn <<- tryCatch(get_db_connection(), error = function(e2) conn)
      run_fuentes_sequential()
    }
  )
  res_dga <- ingest_coerce_fuente_result(res_pair$dga, "dga")
  res_dmc <- ingest_coerce_fuente_result(res_pair$dmc, "dmc")
  metrics$filas_afectadas_dga <- as.integer(res_dga$n_inserted %||% 0L)
  metrics$estaciones_dga <- as.integer(res_dga$n_estaciones %||% 0L)
  metrics$filas_afectadas_dmc <- as.integer(res_dmc$n_inserted %||% 0L)
  metrics$estaciones_dmc <- as.integer(res_dmc$n_estaciones %||% 0L)
  metrics$http_stats <- list(
    dga = res_dga$http_stats %||% NULL,
    dmc = res_dmc$http_stats %||% NULL
  )
  # Tiempo de respuesta / success rate por servidor — documentado en la corrida
  metrics$response_ms <- local({
    one <- function(st, name) {
      if (is.null(st)) return(NULL)
      avg <- suppressWarnings(as.numeric(st$avg_ms))[1]
      n <- suppressWarnings(as.integer(st$n_latency %||% st$requests %||% 0L))[1]
      nreq <- suppressWarnings(as.integer(st$requests %||% n %||% 0L))[1]
      nok <- suppressWarnings(as.integer(st$ok %||% 0L))[1]
      sp <- suppressWarnings(as.numeric(st$success_pct))[1]
      if (length(avg) != 1L || is.na(avg) || !is.finite(avg)) return(NULL)
      if ((length(sp) != 1L || is.na(sp) || !is.finite(sp)) &&
          length(nreq) == 1L && !is.na(nreq) && nreq > 0L &&
          length(nok) == 1L && !is.na(nok)) {
        sp <- round(100 * nok / nreq, 1)
      }
      list(
        avg_ms = round(avg, 1),
        n = if (length(n) == 1L && !is.na(n)) as.integer(n) else NA_integer_,
        timeouts = suppressWarnings(as.integer(st$timeouts %||% 0L))[1],
        ok = if (length(nok) == 1L && !is.na(nok)) as.integer(nok) else NA_integer_,
        success_pct = if (length(sp) == 1L && !is.na(sp) && is.finite(sp)) round(sp, 1) else NA_real_
      )
    }
    out <- list()
    dga <- one(res_dga$http_stats, "dga")
    dmc <- one(res_dmc$http_stats, "dmc")
    if (!is.null(dga)) out$dga <- dga
    if (!is.null(dmc)) out$dmc <- dmc
    if (!length(out)) NULL else out
  })
  Sys.unsetenv("INGEST_SKIP_POST_INGESTA")

  failed <- c(res_dga$failed %||% character(), res_dmc$failed %||% character())
  soft_warns <- c(res_dga$warnings %||% character(), res_dmc$warnings %||% character())
  total <- metrics$filas_afectadas_dga + metrics$filas_afectadas_dmc
  metrics$duracion_ingesta_minutos <- round(
    as.numeric(difftime(Sys.time(), t_ingesta, units = "mins")), 2
  )
  if (length(soft_warns) > 0L) {
    message("[WARN] soft-fail: ", paste(unique(soft_warns), collapse = " | "))
    metrics$warnings <- paste(unique(soft_warns), collapse = "; ")
  }

  fresh <- !dry && ingest_fresh_enough(conn, fin, min_rows = 500L)
  complete <- !dry && ckpt_ingest_complete(ALL_INGEST_VARS)
  if (length(failed) > 0L) {
    metrics$failed <- paste(unique(failed), collapse = ", ")
  }

  outcome <- ingest_decide_outcome(
    failed = failed,
    fresh = fresh,
    complete = complete,
    total = total,
    estaciones_dga = metrics$estaciones_dga,
    estaciones_dmc = metrics$estaciones_dmc,
    dry = dry
  )
  metrics$exit_code <- as.integer(outcome$exit_code)
  if (identical(outcome$message, "failed_no_fresh")) {
    message("[FAIL] Variables con error y sin datos frescos: ", paste(unique(failed), collapse = ", "))
  } else if (identical(outcome$message, "failed_but_fresh")) {
    message("[WARN] Variables con error pero datos frescos OK; exit 0. failed=",
            paste(unique(failed), collapse = ", "))
  } else if (identical(outcome$message, "fresh_rows")) {
    message("[WARN] Checkpoint incompleto pero hay filas/frescura; post_ingesta + exit 0")
  } else if (identical(outcome$message, "partial_fuente_ok") ||
             identical(outcome$message, "partial_fuente_ok_with_failed_vars")) {
    message(
      "[WARN] Ingesta parcial OK (dga_est=", metrics$estaciones_dga,
      " dmc_est=", metrics$estaciones_dmc, "); post_ingesta + exit 0. reason=",
      outcome$message
    )
  } else if (identical(outcome$message, "incomplete_no_fresh")) {
    message("[WARN] Ingesta incompleta sin frescura suficiente.")
  }
  if (isTRUE(outcome$do_post)) {
    run_post_ingesta()
    ckpt_pipeline_mark("ingest_done")
  }
}

# --- PASO 4: frescura ---
ultima <- ultima_fecha_observacion(conn)
metrics$ultima_fecha_observacion <- if (is.na(ultima)) NA_character_ else format(ultima, "%Y-%m-%d")
if (total == 0L && !is.na(ultima) && ultima < hoy - 2L) {
  message("[WARN] Ingesta sin filas nuevas; ultima_fecha=", ultima, " (atrasada >2 dias).")
}

# --- PASO 5: verify ---
if (metrics$exit_code == 0L) {
  log_paso(5, "Verificacion post-ingesta")
  year_check <- as.integer(format(fin, "%Y"))
  api_url <- Sys.getenv("API_URL", "http://api:8000")

  counts <- DBI::dbGetQuery(conn, "
    SELECT f.nombre AS fuente, o.year AS ano, COUNT(*) AS n
    FROM bitacora_oficial o
    JOIN mirador e ON e.id = o.id_estacion
    JOIN cantera f ON f.id = e.id_fuente
    WHERE o.year = $1
    GROUP BY f.nombre, o.year
    ORDER BY f.nombre
  ", params = list(year_check))

  if (nrow(counts) == 0L) {
    message("[WARN] Sin filas en bitacora_oficial para el ano ", year_check, ".")
  } else {
    print(counts)
  }

  mv_ok <- mv_estacion_calidad_existe(conn)
  message("[", if (mv_ok) "OK" else "WARN", "] Vista estacion_calidad ",
          if (mv_ok) "existe." else "no encontrada.")

  if (requireNamespace("httr", quietly = TRUE)) {
    ping <- tryCatch(httr::GET(paste0(api_url, "/ping"), httr::timeout(5)), error = function(e) NULL)
    if (!is.null(ping) && httr::status_code(ping) == 200L) {
      message("[OK] API responde en ", api_url)
    } else {
      message("[WARN] API no disponible en ", api_url)
    }
  }
}

# --- Resumen ---
metrics$duracion_total_minutos <- round(
  as.numeric(difftime(Sys.time(), t_inicio, units = "mins")), 2
)
metrics$filas_afectadas_total <- as.integer(
  (metrics$filas_afectadas_dga %||% 0L) + (metrics$filas_afectadas_dmc %||% 0L)
)
message("=== RESUMEN actualizar_diario ===")
for (k in names(metrics)) {
  message("  ", k, "=", metrics[[k]])
}
message("=== Fin actualizar_diario (exit=", metrics$exit_code, ") ===")

metrics_path <- file.path(
  run_dir, "..", "logs",
  paste0("ingest_metrics_", format(hoy, "%Y%m%d"), ".json")
)
tryCatch({
  if (!dir.exists(dirname(metrics_path))) dir.create(dirname(metrics_path), recursive = TRUE)
  jsonlite::write_json(metrics, metrics_path, auto_unbox = TRUE, null = "null")
  message("[metrics] escrito ", normalizePath(metrics_path, winslash = "/", mustWork = FALSE))
}, error = function(e) {
  message("[WARN] No se pudo escribir metrics JSON: ", e$message)
})

if (exists("profile_span_end", mode = "function") && !is.null(.profile_root_span)) {
  tryCatch(profile_span_end(.profile_root_span), error = function(e) NULL)
}
if (exists("profile_run_end", mode = "function")) {
  tryCatch(
    profile_run_end(if (identical(as.integer(metrics$exit_code), 0L)) "done" else "error"),
    error = function(e) message("[profile] WARN end: ", e$message)
  )
}

quit(status = metrics$exit_code)
