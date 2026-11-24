#* @apiTitle Admin (tier manual / ops)

require_admin_key <- function(req, res) {
  expected <- trimws(Sys.getenv("ADMIN_API_KEY", ""))
  if (!nzchar(expected)) {
    res$status <- 503
    return(list(error = "Admin no configurado"))
  }
  key <- req$HTTP_X_ADMIN_KEY %||% req$headers[["x-admin-key"]] %||% ""
  if (!identical(key, expected)) {
    res$status <- 401
    return(list(error = "Clave admin inv\u00e1lida"))
  }
  invisible(NULL)
}

#* Cambiar tier de usuario (manual, v1)
#* @patch /admin/usuarios/<user_id>/tier
function(user_id, req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)
  if (is.null(body) || is.null(body$tier)) {
    res$status <- 400
    return(list(error = "tier requerido"))
  }
  tier <- tolower(trimws(body$tier))
  if (!tier %in% c("gratuito", "pro", "enterprise")) {
    res$status <- 400
    return(list(error = "tier inv\u00e1lido"))
  }

  df <- db_query("SELECT id, email, tier FROM tripulacion WHERE id = $1::uuid", params = list(user_id))
  if (nrow(df) == 0) {
    res$status <- 404
    return(list(error = "Usuario no encontrado"))
  }

  db_execute(
    "UPDATE tripulacion SET tier = $2, updated_at = NOW() WHERE id = $1::uuid",
    params = list(user_id, tier)
  )

  list(
    ok = TRUE,
    user_id = user_id,
    email = df$email[1],
    tier_anterior = df$tier[1],
    tier = tier
  )
}

parse_pg_text_array <- function(x) {
  raw <- x
  if (is.list(raw) && !is.data.frame(raw)) {
    if (length(raw) == 1) raw <- raw[[1]]
  }
  if (length(raw) == 1 && is.character(raw) && grepl("^\\{", raw[1])) {
    inner <- sub("^\\{(.*)\\}$", "\\1", raw[1])
    if (!nzchar(inner)) return(character(0))
    return(strsplit(inner, ",")[[1]])
  }
  if (is.null(raw) || length(raw) == 0) return(character(0))
  as.character(unlist(raw))
}

PIPELINE_STEP_IDS <- c(
  "preflight", "ingest", "alertas_top10", "refresh_pronostico", "alertas_pronostico",
  "verificar_concordancia", "evaluar_precision_alertas", "actualizar_skill_alertas",
  "bias_historico"
)

BIAS_HISTORICO_JOB_BAT <- "descargar_openmeteo_historico.bat"

# Catálogo canónico Task Scheduler (hub).
PIPELINE_TASK_CATALOG <- list(
  list(task_name = "Rodaja_Docker_Warmup", mode = "warmup", job_bat = "ensure_docker.ps1",
       schedule = "00:00", label = "Precalentamiento del sistema", chained = FALSE),
  list(task_name = "Rodaja_Actualizar_Diario", mode = "full", job_bat = "actualizar_diario.bat",
       schedule = "00:10", label = "Pipeline diario", chained = FALSE),
  list(task_name = "Rodaja_Actualizar_Resume", mode = "watchdog", job_bat = "actualizar_diario.bat",
       schedule = "tras Diario · backup 22:00", label = "Vigilancia del pipeline", chained = TRUE),
  list(task_name = "Rodaja_Actualizar_Reportes_Mensual", mode = "reportes",
       job_bat = "actualizar_reportes_mensual.bat", schedule = "dia 1 · 02:00",
       label = "Reportes PDF mensuales", chained = FALSE),
  list(task_name = "Rodaja_Panorama_ICON", mode = "panorama",
       job_bat = "panorama_icon.ps1", schedule = "00:20 y 12:20",
       label = "Panorama ICON", chained = FALSE),
  list(task_name = "Rodaja_Backfill_BBDD", mode = "backfill",
       job_bat = "run_backfill_u10max.ps1", schedule = "tras Diario sano",
       label = "backfill BBDD", chained = TRUE),
  list(task_name = "Rodaja_Backfill_DGA_Dire", mode = "backfill_dga",
       job_bat = "run_backfill_u2_dire.ps1", schedule = "tras Diario sano",
       label = "direccion viento DGA", chained = TRUE),
  list(task_name = "Rodaja_Backfill_DGA_Rafaga", mode = "backfill_dga_rafaga",
       job_bat = "run_backfill_dga_rafaga.ps1", schedule = "tras Diario sano",
       label = "rafaga horaria DGA", chained = TRUE),
  list(task_name = "Rodaja_Generar_SEO", mode = "seo",
       job_bat = "run_generar_seo.ps1", schedule = "10:30",
       label = "SEO / IA", chained = FALSE),
  list(task_name = "Rodaja_Ingest_Scrape", mode = "scrape", job_bat = "actualizar_diario.bat",
       schedule = "deshabilitado", label = "Descarga DGA/DMC (legacy split)", chained = FALSE),
  list(task_name = "Rodaja_Ingest_Post", mode = "post", job_bat = "actualizar_diario.bat",
       schedule = "deshabilitado", label = "Productos (legacy split)", chained = FALSE),
  list(task_name = "Rodaja_Bias_Historico_Chained", mode = "bias",
       job_bat = "descargar_openmeteo_historico.bat", schedule = "encadenado",
       label = "Histórico Open-Meteo", chained = TRUE),
  list(task_name = "Rodaja_Pronostico_Precision_Chained", mode = "precision",
       job_bat = "actualizar_pronostico_precision.bat", schedule = "encadenado",
       label = "Corrección de sesgo", chained = TRUE),
  list(task_name = "Rodaja_Disable_Windows_AutoReboot", mode = "ops",
       job_bat = "disable-windows-auto-reboot.ps1", schedule = "domingo · 04:15",
       label = "Sin reinicio automático", chained = FALSE),
  list(task_name = "PlumberKeepAlive", mode = "keepalive",
       job_bat = "register-plumber-keepalive.ps1", schedule = "cada 5 min",
       label = "Ping de Plumber", chained = FALSE)
)

ingest_logs_dir <- function() {
  Sys.getenv("INGEST_LOGS_DIR", unset = "/ingest_logs")
}

# Reporte estructurado del día (JSON del Diario/Vigilancia)
admin_pipeline_error_report <- function(day = NULL) {
  if (is.null(day) || !nzchar(as.character(day))) {
    day <- format(Sys.Date(), "%Y%m%d")
  } else {
    day <- gsub("-", "", as.character(day))
  }
  path <- file.path(ingest_logs_dir(), sprintf("pipeline_error_report_%s.json", day))
  raw <- read_json_file_safe(path)
  if (is.null(raw)) {
    return(list(
      ok = TRUE,
      available = FALSE,
      day = day,
      status = "missing",
      errors = list(),
      by_code = list(),
      compact = list()
    ))
  }
  errs <- raw$errors %||% list()
  if (!is.list(errs)) errs <- list()
  compact <- lapply(errs, function(e) {
    list(
      code = as.character(e$code %||% ""),
      count = suppressWarnings(as.integer(e$count %||% 1L))[1],
      severity = as.character(e$severity %||% "error"),
      step = as.character(e$step %||% ""),
      fuente = as.character(e$fuente %||% "")
    )
  })
  # Top codes por count
  by_code <- raw$by_code %||% list()
  list(
    ok = TRUE,
    available = TRUE,
    day = as.character(raw$day %||% day),
    status = as.character(raw$status %||% "unknown"),
    generated_at = raw$generated_at %||% NULL,
    incomplete_vars = raw$incomplete_vars %||% list(),
    errors = errs,
    by_code = by_code,
    compact = compact,
    actions = lapply(raw$actions %||% list(), function(a) {
      list(
        ts = as.character(a$ts %||% ""),
        tool = as.character(a$tool %||% ""),
        result = as.character(a$result %||% ""),
        detail = as.character(a$detail %||% "")
      )
    }),
    actions_n = length(raw$actions %||% list()),
    watchdog_ran_at = raw$watchdog_ran_at %||% NULL,
    watchdog_run_id = raw$watchdog_run_id %||% NULL
  )
}

format_error_report_caption <- function(rep) {
  if (is.null(rep) || !isTRUE(rep$available)) return(NULL)
  codes <- rep$by_code %||% list()
  if (!length(codes)) {
    if (identical(rep$status, "healthy")) return(NULL)
    return(NULL)
  }
  # Preferir 403 / circuit / timeout en caption
  prefer <- c(
    "INGEST_DGA_HTTP_403", "INGEST_DMC_HTTP_403",
    "INGEST_DGA_CIRCUIT_OPEN", "INGEST_DMC_CIRCUIT_OPEN",
    "INGEST_DGA_TIMEOUT", "INGEST_DMC_TIMEOUT",
    "INGEST_INCOMPLETE_VARS"
  )
  bits <- character()
  for (nm in prefer) {
    n <- suppressWarnings(as.integer(codes[[nm]] %||% 0L))[1]
    if (!is.na(n) && n > 0L) {
      short <- sub("^INGEST_", "", nm)
      short <- gsub("_", " ", short)
      bits <- c(bits, sprintf("%s×%d", short, n))
    }
    if (length(bits) >= 3L) break
  }
  if (!length(bits)) {
    # fallback: primeros 2 del by_code
    nms <- names(codes)
    if (length(nms) > 2L) nms <- nms[1:2]
    for (nm in nms) {
      n <- suppressWarnings(as.integer(codes[[nm]] %||% 0L))[1]
      if (!is.na(n) && n > 0L) bits <- c(bits, sprintf("%s×%d", nm, n))
    }
  }
  if (!length(bits)) return(NULL)
  paste(bits, collapse = " · ")
}

format_response_ms_hub <- function(rm) {
  if (is.null(rm) || !length(rm)) return(NULL)
  parts <- character()
  for (nm in c("dga", "dmc", "openmeteo")) {
    rec <- rm[[nm]]
    if (is.null(rec)) next
    avg <- suppressWarnings(as.numeric(unlist(rec$avg_ms)[1]))
    if (length(avg) != 1L || is.na(avg) || !is.finite(avg)) next
    label <- switch(nm, openmeteo = "OM", dga = "DGA", dmc = "DMC", nm)
    sp <- suppressWarnings(as.numeric(unlist(rec$success_pct)[1]))
    if (length(sp) == 1L && !is.na(sp) && is.finite(sp)) {
      parts <- c(parts, sprintf("%s %.0fms %.1f%%", label, avg, sp))
    } else {
      parts <- c(parts, sprintf("%s %.0fms", label, avg))
    }
  }
  if (!length(parts)) NULL else paste(parts, collapse = " · ")
}

extract_response_ms <- function(metrics = NULL, day_health = NULL) {
  rm <- NULL
  if (!is.null(metrics) && !is.null(metrics$response_ms)) rm <- metrics$response_ms
  if (is.null(rm) && !is.null(day_health$ingest$response_ms)) {
    rm <- day_health$ingest$response_ms
  }
  if (is.null(rm) || !length(rm)) return(NULL)
  rm
}

admin_error_code_n <- function(codes, nm) {
  if (is.null(codes) || !length(codes)) return(0L)
  n <- suppressWarnings(as.integer(codes[[nm]] %||% 0L))[1]
  if (length(n) != 1L || is.na(n) || n < 0L) 0L else as.integer(n)
}

# Cache in-process: conteo live de huecos gapfill (API monta ingest_logs :ro).
.admin_gapfill_pares_cache <- new.env(parent = emptyenv())

admin_gapfill_tipo_sql <- function(fuente, abr) {
  if (!identical(fuente, "dga")) return("")
  if (identical(abr, "q")) return(" AND e.tipo ILIKE '%FLUVIOMETRICA%'")
  # pp / temps / viento DGA: mismo filtro METEOROLOGICA que orchestrate_meteovision
  " AND e.tipo ILIKE '%METEOROLOGICA%'"
}

admin_count_gapfill_var_pares <- function(fuente, abr, fecha_fin) {
  fuente <- tolower(trimws(as.character(fuente)[1]))
  abr <- tolower(trimws(as.character(abr)[1]))
  fecha <- format(as.Date(fecha_fin), "%Y-%m-%d")
  if (!nzchar(fuente) || !nzchar(abr) || is.na(as.Date(fecha))) return(0L)
  tipo <- admin_gapfill_tipo_sql(fuente, abr)
  sql <- paste0(
    "SELECT COUNT(*)::int AS n FROM mirador e ",
    "WHERE e.id_fuente = (SELECT id FROM cantera WHERE lower(nombre) = $1) ",
    tipo,
    " AND EXISTS (",
    "  SELECT 1 FROM bitacora_oficial o WHERE o.id_estacion = e.id ",
    "    AND o.id_variable = (SELECT id FROM magnitud WHERE lower(nombre_abr) = $2)",
    ") AND NOT EXISTS (",
    "  SELECT 1 FROM bitacora_oficial o WHERE o.id_estacion = e.id ",
    "    AND o.id_variable = (SELECT id FROM magnitud WHERE lower(nombre_abr) = $2) ",
    "    AND o.fecha = $3::date",
    ")"
  )
  df <- tryCatch(
    db_query(sql, params = list(fuente, abr, fecha)),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0L) return(0L)
  n <- suppressWarnings(as.integer(df$n[1]))[1]
  if (length(n) != 1L || is.na(n) || n < 0L) 0L else n
}

admin_read_gapfill_huecos_file <- function(day_tag, fecha_fin) {
  path <- file.path(
    ingest_logs_dir(),
    sprintf("pipeline_gapfill_huecos_%s.json", gsub("-", "", as.character(day_tag)))
  )
  raw <- read_json_file_safe(path)
  if (is.null(raw) || !is.list(raw)) return(list())
  vars <- raw$vars %||% list()
  if (!is.list(vars)) return(list())
  out <- list()
  for (nm in names(vars)) {
    rec <- vars[[nm]]
    if (!is.list(rec)) next
    n_init <- suppressWarnings(as.integer(rec$n_initial %||% rec$n_huecos %||% NA))[1]
    n_rem <- suppressWarnings(as.integer(rec$n_remaining %||% NA))[1]
    n0 <- suppressWarnings(as.integer(rec$n0 %||% NA))[1]
    out[[nm]] <- list(
      n_initial = if (length(n_init) == 1L && !is.na(n_init)) n_init else NULL,
      n_remaining = if (length(n_rem) == 1L && !is.na(n_rem)) n_rem else NULL,
      n0 = if (length(n0) == 1L && !is.na(n0)) n0 else NULL
    )
  }
  out
}

# Denominador Descargar: suma n0 (huecos json) o estaciones_dga/dmc por var esperada.
# Distinto de n_pares_ingest (solo huecos post-ingesta para Vigilancia/gapfill).
# Devuelve 0 si falta tamaño para alguna var esperada (caller usa fallback var-ratio).
admin_ingest_pares_total <- function(expected_vars, file_vars, est_dga = NULL, est_dmc = NULL) {
  expected_vars <- as.character(expected_vars %||% character(0))
  expected_vars <- expected_vars[nzchar(expected_vars)]
  if (!length(expected_vars)) return(0L)
  if (!is.list(file_vars)) file_vars <- list()
  seen <- character(0)
  total <- 0L
  for (key in expected_vars) {
    fkey <- admin_gapfill_file_key(key, file_vars)
    if (fkey %in% seen) next
    seen <- c(seen, fkey)
    fi <- file_vars[[fkey]]
    n0 <- if (!is.null(fi$n0)) suppressWarnings(as.integer(fi$n0)[1]) else NA_integer_
    if (length(n0) != 1L || is.na(n0) || n0 < 0L) {
      fuente <- strsplit(as.character(key)[1], "/", fixed = TRUE)[[1]][1]
      n0 <- if (identical(fuente, "dga")) {
        suppressWarnings(as.integer(est_dga %||% NA)[1])
      } else if (identical(fuente, "dmc")) {
        suppressWarnings(as.integer(est_dmc %||% NA)[1])
      } else {
        NA_integer_
      }
    }
    if (length(n0) != 1L || is.na(n0) || n0 <= 0L) return(0L)
    total <- total + as.integer(n0)
  }
  as.integer(total)
}

# Descargar % = (pares_total − huecos_vivos) / pares_total; gapfill % usa otro denom.
admin_ingest_pct_pares <- function(n_pares_total, n_pares_remaining) {
  n_tot <- suppressWarnings(as.integer(n_pares_total %||% 0L)[1])
  n_rem <- suppressWarnings(as.integer(n_pares_remaining %||% 0L)[1])
  if (length(n_tot) != 1L || is.na(n_tot) || n_tot <= 0L) return(NULL)
  if (length(n_rem) != 1L || is.na(n_rem) || n_rem < 0L) n_rem <- 0L
  if (n_rem > n_tot) n_rem <- n_tot
  as.integer(max(0L, min(100L, round(100 * (n_tot - n_rem) / n_tot))))
}

# n_pares_ingest = huecos al inicio del gapfill (lo que dejó la ingesta).
# n_pares_gapfill = huecos que aún faltan / rellenará (live BD, cache ttl_sec).
# n_done / pct = estaciones/pares ya rellenados desde n_initial.
admin_gapfill_file_key <- function(key, file_vars) {
  key <- as.character(key)[1]
  if (!nzchar(key)) return(key)
  if (!is.null(file_vars[[key]])) return(key)
  # DGA temp scrape → incomplete_vars usa dga/t_min; snapshot usa dga/temp
  if (identical(key, "dga/t_min") && !is.null(file_vars[["dga/temp"]])) return("dga/temp")
  if (identical(key, "dga/t_max") && !is.null(file_vars[["dga/temp"]])) return("dga/temp")
  if (identical(key, "dga/t_mean") && !is.null(file_vars[["dga/temp"]])) return("dga/temp")
  # DGA viento: ckpt dga/u_max_diario; loop label dga/viento
  if (identical(key, "dga/u_max_diario") && !is.null(file_vars[["dga/viento"]])) return("dga/viento")
  if (identical(key, "dga/u2_min") && !is.null(file_vars[["dga/viento"]])) return("dga/viento")
  if (identical(key, "dga/u2_mean") && !is.null(file_vars[["dga/viento"]])) return("dga/viento")
  key
}

admin_gapfill_pares_meta <- function(incomplete_vars, fecha_fin, day_tag,
                                     gapfill_completed = FALSE, ttl_sec = 120) {
  incomplete <- as.character(incomplete_vars %||% character(0))
  incomplete <- incomplete[nzchar(incomplete)]
  file_vars <- admin_read_gapfill_huecos_file(day_tag, fecha_fin)
  by_var <- list()
  n_ingest <- 0L
  n_gapfill <- 0L
  ttl_sec <- suppressWarnings(as.numeric(ttl_sec)[1])
  if (!is.finite(ttl_sec) || ttl_sec < 5) ttl_sec <- 120

  finish_meta <- function(n_ingest, n_gapfill, by_var, label) {
    n_ingest <- as.integer(n_ingest)
    n_gapfill <- as.integer(n_gapfill)
    n_done <- max(0L, n_ingest - n_gapfill)
    pct <- if (n_ingest > 0L) {
      as.integer(max(0L, min(100L, round(100 * n_done / n_ingest))))
    } else if (n_gapfill == 0L) {
      100L
    } else {
      0L
    }
    list(
      n_pares_ingest = n_ingest,
      n_pares_gapfill = n_gapfill,
      n_done = n_done,
      pct = pct,
      by_var = by_var,
      label = label
    )
  }

  if (isTRUE(gapfill_completed) || length(incomplete) == 0L) {
    for (nm in names(file_vars)) {
      ni <- file_vars[[nm]]$n_initial
      if (!is.null(ni)) n_ingest <- n_ingest + as.integer(ni)
    }
    return(finish_meta(
      n_ingest, 0L, file_vars,
      if (n_ingest > 0L) sprintf("gapfill OK · %d huecos", n_ingest) else "gapfill OK"
    ))
  }

  cache_key <- paste(
    format(as.Date(fecha_fin), "%Y-%m-%d"),
    paste(sort(incomplete), collapse = ","),
    sep = "|"
  )
  hit <- .admin_gapfill_pares_cache[[cache_key]]
  live_map <- NULL
  if (!is.null(hit) &&
      is.list(hit) &&
      !is.null(hit$at) &&
      as.numeric(difftime(Sys.time(), hit$at, units = "secs")) < ttl_sec) {
    live_map <- hit$value
  } else {
    live_map <- list()
    for (key in incomplete) {
      parts <- strsplit(key, "/", fixed = TRUE)[[1]]
      if (length(parts) < 2L) next
      live_map[[key]] <- admin_count_gapfill_var_pares(parts[1], parts[2], fecha_fin)
    }
    .admin_gapfill_pares_cache[[cache_key]] <- list(at = Sys.time(), value = live_map)
  }

  for (key in incomplete) {
    fkey <- admin_gapfill_file_key(key, file_vars)
    fi <- file_vars[[fkey]]
    n_live <- suppressWarnings(as.integer(live_map[[key]] %||% NA))[1]
    if (length(n_live) != 1L || is.na(n_live)) n_live <- NULL
    n_init <- if (!is.null(fi$n_initial)) as.integer(fi$n_initial) else n_live
    if (is.null(n_init)) n_init <- 0L
    if (is.null(n_live)) n_live <- n_init
    if (!is.null(fi$n_initial) && n_live > as.integer(fi$n_initial)) {
      n_init <- as.integer(n_live)
    }
    n_done_v <- max(0L, as.integer(n_init) - as.integer(n_live))
    by_var[[key]] <- list(
      n_initial = as.integer(n_init),
      n_remaining = as.integer(n_live),
      n_done = as.integer(n_done_v),
      n0 = fi$n0 %||% NULL,
      file_key = fkey
    )
    n_ingest <- n_ingest + as.integer(n_init)
    n_gapfill <- n_gapfill + as.integer(n_live)
  }

  n_done <- max(0L, n_ingest - n_gapfill)
  label <- if (n_ingest > 0L && n_gapfill == 0L) {
    sprintf("gapfill OK · %d huecos", n_ingest)
  } else if (n_ingest > 0L) {
    sprintf("hechas %d · faltan %d", n_done, n_gapfill)
  } else if (n_gapfill > 0L) {
    sprintf("%d huecos", n_gapfill)
  } else if (length(incomplete) > 0L) {
    paste0("gapfill: ", paste(incomplete, collapse = ", "))
  } else {
    "gapfill"
  }

  finish_meta(n_ingest, n_gapfill, by_var, label)
}

# Hub Descargar (ok|parcial|en_curso|error):
# parcial = progreso + huecos/timeout/circuit/wall 124 (Vigilancia puede reintentar).
# error   = sin progreso recuperable. CIRCUIT_OPEN no es catastrófico (gapfill).
classify_ingest_hub <- function(ec, has_progress, incomplete_vars, err_rep) {
  codes <- if (!is.null(err_rep) && isTRUE(err_rep$available)) err_rep$by_code else list()
  if (is.null(codes)) codes <- list()
  n_to <- admin_error_code_n(codes, "INGEST_DGA_TIMEOUT") +
    admin_error_code_n(codes, "INGEST_DMC_TIMEOUT")
  n_circuit <- admin_error_code_n(codes, "INGEST_DGA_CIRCUIT_OPEN") +
    admin_error_code_n(codes, "INGEST_DMC_CIRCUIT_OPEN")
  n_403 <- admin_error_code_n(codes, "INGEST_DGA_HTTP_403") +
    admin_error_code_n(codes, "INGEST_DMC_HTTP_403")
  incomplete <- as.character(incomplete_vars %||% character(0))
  incomplete <- incomplete[nzchar(incomplete)]
  n_inc <- length(incomplete)
  vars_txt <- if (n_inc > 0L) paste(incomplete, collapse = ", ") else NULL
  ec <- suppressWarnings(as.integer(ec %||% 0L))[1]
  if (length(ec) != 1L || is.na(ec)) ec <- 0L

  # Soft-continue Diario (deadline 07:00 → 124) o huecos/timeouts/circuit con progreso
  soft_recoverable <- isTRUE(has_progress) && (
    n_inc > 0L || n_to > 0L || n_circuit > 0L || identical(ec, 124L)
  )

  if (soft_recoverable) {
    n_pares <- if (n_to > 0L) n_to else n_inc
    if (n_to > 0L && !is.null(vars_txt)) {
      line <- sprintf(
        "parcial · faltan %d pares est/var por timeout · vars: %s",
        n_pares, vars_txt
      )
    } else if (n_to > 0L) {
      line <- sprintf("parcial · faltan %d pares est/var por timeout", n_pares)
    } else if (!is.null(vars_txt)) {
      line <- sprintf(
        "parcial · %d variable(s) incompleta(s): %s",
        n_inc, vars_txt
      )
    } else if (n_circuit > 0L) {
      line <- "parcial · circuit DGA (gapfill)"
    } else {
      line <- "parcial · ingesta incompleta (deadline)"
    }
    return(list(state = "parcial", line = line, n_pares = n_pares, incomplete_vars = incomplete))
  }

  if (isTRUE(ec != 0L) || n_403 > 0L || n_circuit > 0L) {
    return(list(state = "error", line = NULL, n_pares = n_to, incomplete_vars = incomplete))
  }
  list(state = "ok", line = NULL, n_pares = 0L, incomplete_vars = incomplete)
}

# Convención Hub ↔ profiler (corrida / paso del día):
#   ok       = listo por hoy; no se vuelve a correr
#   parcial  = incompleto; puede reintentar hoy
#   en_curso = ejecución en vivo
#   error    = fallo catastrófico; no reintenta hoy
# Alias legacy: done/success/exitoso→ok, failed/fallo→error, running→en_curso
pipeline_outcome_normalize <- function(status) {
  st <- tolower(trimws(as.character(status %||% "")[1]))
  if (!nzchar(st)) return(NA_character_)
  if (st %in% c("ok", "done", "success", "exitoso", "healthy")) return("ok")
  if (st %in% c("parcial", "issues", "deferred", "partial")) return("parcial")
  if (st %in% c("en_curso", "running", "live")) return("en_curso")
  if (st %in% c("error", "failed", "fallo", "fail")) return("error")
  NA_character_
}

# Vigilancia (metrics de Register-WatchdogRun): incomplete ≠ ok aunque exit_code=0
pipeline_outcome_from_watchdog_metrics <- function(metrics, exit_code = 0L) {
  ec <- suppressWarnings(as.integer(exit_code %||% 0L))[1]
  if (is.na(ec)) ec <- 0L
  if (ec != 0L) return("error")
  m <- metrics
  if (!is.list(m)) m <- tryCatch(jsonb_as_list(m), error = function(e) list())
  if (!is.list(m)) m <- list()
  wstat <- tolower(as.character(m$watchdog_status %||% "")[1])
  gap_ok <- m$gapfill_completed
  inc <- m$incomplete_vars %||% list()
  if (is.character(inc)) inc <- as.list(inc)
  n_inc <- sum(nzchar(as.character(unlist(inc, use.names = FALSE))))
  if (wstat %in% c("issues", "deferred") || n_inc > 0L || isFALSE(gap_ok)) {
    return("parcial")
  }
  "ok"
}

# % de avance del nodo Hub: 100 ok/omitido, 0 pendiente/error duro,
# o ratio/pct_direct para ingest parcial / bias historico.
hub_stage_pct <- function(state, ratio = NULL, pct_direct = NULL) {
  if (!is.null(pct_direct)) {
    p <- suppressWarnings(as.numeric(pct_direct)[1])
    if (length(p) == 1L && is.finite(p)) {
      return(as.integer(max(0L, min(100L, round(p)))))
    }
  }
  st <- as.character(state %||% "pendiente")[1]
  if (st %in% c("ok", "omitido")) return(100L)
  if (st %in% c("pendiente", "off", "en_curso")) return(0L)
  if (!is.null(ratio)) {
    r <- suppressWarnings(as.numeric(ratio)[1])
    if (length(r) == 1L && is.finite(r)) {
      return(as.integer(max(0L, min(100L, round(100 * r)))))
    }
  }
  if (identical(st, "parcial")) return(NULL)
  if (identical(st, "error")) return(0L)
  0L
}

# Ultima corrida_de_carta del dia pisa el exit del Diario.
# Un JSON cortado deja el paso en 1; el reintento que si escribio tiene que verse OK.
pronostico_hub_iso <- function(x) {
  if (is.null(x) || length(x) < 1L || is.na(x[1])) return(NULL)
  ts <- x[1]
  if (!inherits(ts, "POSIXct")) ts <- suppressWarnings(as.POSIXct(ts, tz = "UTC"))
  if (length(ts) != 1L || is.na(ts)) return(NULL)
  format(ts, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago")
}

pronostico_hub_from_corridas <- function(rows, n_est_hoy = NULL) {
  if (is.null(rows) || !is.data.frame(rows) || nrow(rows) < 1L) return(NULL)
  if (!"status" %in% names(rows)) return(NULL)
  latest <- rows[1, , drop = FALSE]
  st <- tolower(as.character(latest$status)[1])
  n_up <- suppressWarnings(as.integer(latest$n_upserts[1]))
  if (length(n_up) != 1L || is.na(n_up)) n_up <- 0L
  n_est <- suppressWarnings(as.integer(latest$n_estaciones[1]))
  err <- as.character(latest$error_msg[1])
  if (length(err) != 1L || is.na(err)) err <- ""
  err <- gsub("[[:space:]]+", " ", trimws(err))
  if (nchar(err) > 180L) err <- paste0(substr(err, 1L, 177L), "...")
  if (!nzchar(err)) err <- NULL
  started <- pronostico_hub_iso(latest$started_at[1])
  finished <- pronostico_hub_iso(latest$finished_at[1])
  dur <- NULL
  s <- suppressWarnings(as.POSIXct(latest$started_at[1], tz = "UTC"))
  f <- suppressWarnings(as.POSIXct(latest$finished_at[1], tz = "UTC"))
  if (length(s) == 1L && length(f) == 1L && !is.na(s) && !is.na(f)) {
    dur <- as.numeric(difftime(f, s, units = "secs"))
  }
  had_fail <- any(tolower(as.character(rows$status)) != "ok")
  pct_done <- NULL
  n_hoy <- suppressWarnings(as.integer(n_est_hoy)[1])
  if (length(n_est) == 1L && !is.na(n_est) && n_est > 0L &&
      length(n_hoy) == 1L && !is.na(n_hoy)) {
    pct_done <- as.integer(max(0L, min(100L, round(100 * n_hoy / n_est))))
  }
  base <- list(
    started_at = started, finished_at = finished, duration_sec = dur,
    line_suffix = NULL, error_preview = NULL, drop_step_error = FALSE
  )
  if (identical(st, "ok") && n_up > 0L) {
    base$state <- "ok"
    base$exit_code <- 0L
    base$pct <- 100L
    base$drop_step_error <- TRUE
    if (had_fail) base$line_suffix <- "reintento OK"
    return(base)
  }
  if (identical(st, "running")) {
    base$state <- "en_curso"
    base$exit_code <- NULL
    base$pct <- pct_done
    base$finished_at <- NULL
    base$duration_sec <- NULL
    base$drop_step_error <- TRUE
    return(base)
  }
  if (identical(st, "partial") || n_up > 0L) {
    base$state <- "parcial"
    base$exit_code <- 1L
    base$pct <- if (!is.null(pct_done)) pct_done else NULL
    base$error_preview <- err
    return(base)
  }
  base$state <- "error"
  base$exit_code <- 1L
  base$pct <- 0L
  base$error_preview <- err
  base
}

read_json_file_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch({
    # PowerShell Set-Content -Encoding UTF8 escribe BOM; jsonlite avisa y a veces falla.
    txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
    if (!length(txt)) return(NULL)
    txt[[1]] <- sub("^\ufeff", "", txt[[1]])
    jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE)
  }, error = function(e) NULL)
}

read_ckpt_kv <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  out <- list()
  for (l in lines) {
    l <- trimws(l)
    if (!nzchar(l) || startsWith(l, "#")) next
    parts <- strsplit(l, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2L) next
    out[[trimws(parts[1])]] <- trimws(paste(parts[-1], collapse = "="))
  }
  out
}

pg_scalar_int <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(NULL)
  v <- suppressWarnings(as.integer(x[1]))
  if (is.na(v)) NULL else v
}

pg_scalar_num <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(NULL)
  v <- suppressWarnings(as.numeric(x[1]))
  if (is.na(v)) NULL else v
}

pg_scalar_date <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(NULL)
  format(x[1], "%Y-%m-%d")
}

pg_scalar_text <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(NULL)
  out <- as.character(x[1])
  if (!nzchar(out)) NULL else out
}

normalize_pasos_detalle <- function(pasos) {
  if (is.null(pasos) || length(pasos) == 0) return(list())
  if (is.data.frame(pasos)) {
    return(lapply(seq_len(nrow(pasos)), function(i) {
      p <- as.list(pasos[i, , drop = FALSE])
      lapply(p, function(v) if (length(v) == 1) v[[1]] else v)
    }))
  }
  if (is.list(pasos) && !is.null(names(pasos)) && !is.null(pasos$id)) {
    return(list(pasos))
  }
  pasos
}

count_step_outcomes <- function(pasos_detalle) {
  pasos <- normalize_pasos_detalle(pasos_detalle)
  ok <- 0L
  fail <- 0L
  skip <- 0L
  for (p in pasos) {
    if (isTRUE(p$skipped)) {
      skip <- skip + 1L
      next
    }
    ec <- suppressWarnings(as.integer(p$exit_code %||% 0L))
    if (is.na(ec)) ec <- 0L
    if (ec == 0L) ok <- ok + 1L else fail <- fail + 1L
  }
  list(ok = ok, fail = fail, skip = skip, total = length(pasos))
}

has_errores_text <- function(errores) {
  txt <- pg_scalar_text(errores)
  !is.null(txt) && nzchar(trimws(txt))
}

detect_error_paso <- function(errores, pasos_detalle) {
  pasos <- normalize_pasos_detalle(pasos_detalle)
  for (p in pasos) {
    if (isTRUE(p$skipped)) next
    ec <- suppressWarnings(as.integer(p$exit_code %||% 0L))
    if (!is.na(ec) && ec != 0L) return(as.character(p$id %||% ""))
  }
  txt <- pg_scalar_text(errores)
  if (is.null(txt)) return(NULL)
  for (sid in PIPELINE_STEP_IDS) {
    if (grepl(sid, txt, fixed = TRUE)) return(sid)
  }
  if (grepl("FAIL en paso\\s+(\\S+)", txt, perl = TRUE)) {
    m <- regmatches(txt, regexpr("FAIL en paso\\s+(\\S+)", txt, perl = TRUE))
    sub("^FAIL en paso\\s+", "", m[1])
  } else {
    NULL
  }
}

bias_historico_status <- function(metrics) {
  if (is.null(metrics) || length(metrics) == 0) return(NULL)
  bias <- metrics$bias
  if (is.null(bias) || is.null(bias$status)) return(NULL)
  st <- as.character(bias$status)
  if (length(st) < 1L || is.na(st[1]) || !nzchar(st[1])) return(NULL)
  st[1]
}

derive_estado <- function(exit_code, skipped, pasos_detalle, errores,
                          job_bat = NULL, metrics = NULL) {
  exit_code <- suppressWarnings(as.integer(exit_code %||% 0L))
  if (is.na(exit_code)) exit_code <- 0L
  skipped <- isTRUE(skipped)
  counts <- count_step_outcomes(pasos_detalle)
  has_err <- has_errores_text(errores)

  job_bat <- pg_scalar_text(job_bat) %||% ""
  m <- if (is.list(metrics)) metrics else tryCatch(jsonb_as_list(metrics), error = function(e) list())
  if (!is.list(m)) m <- list()

  if (identical(job_bat, BIAS_HISTORICO_JOB_BAT)) {
    st <- bias_historico_status(metrics)
    if (!is.null(st)) {
      if (identical(st, "error") || exit_code != 0L) return("fallo")
      if (startsWith(st, "skipped_")) return("omitido")
      if (st %in% c("budget_exhausted", "ok", "backfill_complete", "noop_complete")) {
        return("exitoso")
      }
    }
  }

  # Vigilancia: exit 0 + huecos → parcial (Hub/profiler; no "exitoso" falso)
  if (identical(job_bat, "pipeline_watchdog.ps1") || !is.null(m$watchdog_status)) {
    out <- pipeline_outcome_from_watchdog_metrics(m, exit_code)
    if (identical(out, "error")) return("fallo")
    if (identical(out, "parcial")) return("parcial")
    if (identical(out, "ok")) return("exitoso")
  }

  intentional_skip <- skipped && exit_code == 0L && counts$fail == 0L && !has_err &&
    (counts$total == 0L || counts$skip == counts$total)

  if (intentional_skip) return("omitido")
  if (counts$fail > 0L && counts$ok > 0L) return("parcial")
  if (exit_code != 0L || counts$fail > 0L || has_err) return("fallo")
  "exitoso"
}

errores_preview_text <- function(errores, max_len = 200L) {
  txt <- pg_scalar_text(errores)
  if (is.null(txt)) return(NULL)
  if (nchar(txt) > max_len) paste0(substr(txt, 1, max_len), "...")
  else txt
}

jsonb_as_list <- function(x) {
  if (is.null(x) || length(x) == 0) return(list())
  if (is.list(x) && !is.character(x)) {
    if (is.data.frame(x)) return(normalize_pasos_detalle(x))
    if (!is.null(x$id)) return(list(x))
    if (length(x) == 1 && is.list(x[[1]])) return(normalize_pasos_detalle(x[[1]]))
    return(normalize_pasos_detalle(x))
  }
  if (length(x) == 1 && (is.na(x[1]) || !nzchar(as.character(x[1])))) return(list())
  tryCatch({
    parsed <- jsonlite::fromJSON(x[1], simplifyVector = TRUE)
    normalize_pasos_detalle(parsed)
  }, error = function(e) list())
}

actualizacion_row_list <- function(row, include_full_errores = FALSE) {
  errores_raw <- row$errores[[1]] %||% row$errores[1]
  pasos <- parse_pg_text_array(row$pasos_completados[[1]] %||% row$pasos_completados)
  pasos_detalle <- if ("pasos_detalle" %in% names(row)) {
    jsonb_as_list(row$pasos_detalle[[1]] %||% row$pasos_detalle[1])
  } else {
    list()
  }
  exit_code <- pg_scalar_int(row$exit_code[1]) %||% 0L
  skipped <- isTRUE(row$skipped[1])
  job_bat <- pg_scalar_text(row$job_bat[1]) %||% "actualizar_diario.bat"
  metrics_raw <- if ("metrics" %in% names(row)) {
    jsonb_as_list(row$metrics[[1]] %||% row$metrics[1])
  } else {
    list()
  }
  estado <- derive_estado(exit_code, skipped, pasos_detalle, errores_raw, job_bat, metrics_raw)
  # outcome = convención Hub↔profiler (ok|parcial|en_curso|error); estado = legacy exitoso/fallo
  outcome <- pipeline_outcome_normalize(estado)
  if (is.na(outcome)) outcome <- if (identical(estado, "omitido")) "ok" else "parcial"

  out <- list(
    id = as.integer(row$id[1]),
    run_id = row$run_id[1],
    started_at = format(row$started_at[1], "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago"),
    finished_at = if (is.na(row$finished_at[1])) NULL else format(row$finished_at[1], "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago"),
    exit_code = exit_code,
    skipped = skipped,
    estado = estado,
    outcome = outcome,
    pasos_completados = pasos,
    pasos_detalle = pasos_detalle,
    estaciones_dga = pg_scalar_int(row$estaciones_dga[1]),
    estaciones_dmc = pg_scalar_int(row$estaciones_dmc[1]),
    filas_afectadas_total = pg_scalar_int(row$filas_afectadas_total[1]),
    duracion_segundos = pg_scalar_num(row$duracion_segundos[1]),
    ultima_fecha_observacion = pg_scalar_date(row$ultima_fecha_observacion[1]),
    errores_preview = errores_preview_text(errores_raw),
    error_paso = detect_error_paso(errores_raw, pasos_detalle),
    log_dia = pg_scalar_text(row$log_dia[1]),
    job_bat = job_bat,
    task_name = if ("task_name" %in% names(row)) pg_scalar_text(row$task_name[1]) else NULL,
    mode = if ("mode" %in% names(row)) pg_scalar_text(row$mode[1]) else NULL,
    phase = if ("phase" %in% names(row)) pg_scalar_text(row$phase[1]) else NULL
  )
  if (include_full_errores) {
    out$errores <- pg_scalar_text(errores_raw)
    out$metrics <- metrics_raw
    if ("filas_afectadas_dga" %in% names(row)) {
      out$filas_afectadas_dga <- pg_scalar_int(row$filas_afectadas_dga[1])
      out$filas_afectadas_dmc <- pg_scalar_int(row$filas_afectadas_dmc[1])
    }
  } else if (length(metrics_raw) > 0) {
    out$metrics <- metrics_raw
  }
  out
}

#* Listar jobs (.bat) con registros
#* @get /admin/actualizaciones/jobs
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  df <- db_query("
    SELECT job_bat, COUNT(*) AS n, MAX(started_at) AS last_started_at
    FROM parte_de_guardia
    GROUP BY job_bat
    ORDER BY job_bat
  ")

  list(
    ok = TRUE,
    items = lapply(seq_len(nrow(df)), function(i) {
      list(
        job_bat = df$job_bat[i],
        count = as.integer(df$n[i]),
        last_started_at = if (is.na(df$last_started_at[i])) NULL else
          format(df$last_started_at[i], "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago")
      )
    })
  )
}

#* Listar ejecuciones del pipeline diario
#* @get /admin/actualizaciones
function(req, res, limit = 50, offset = 0, job_bat = "", task_name = "", mode = "",
         desde = "", hasta = "") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  limit <- max(1L, min(as.integer(limit), 500L))
  offset <- max(0L, as.integer(offset))
  job_filter <- trimws(job_bat %||% "")
  task_filter <- trimws(task_name %||% "")
  mode_filter <- trimws(mode %||% "")
  desde_s <- trimws(desde %||% "")
  hasta_s <- trimws(hasta %||% "")

  where <- character(0)
  params <- list()
  idx <- 0L
  add_f <- function(sql_frag, val) {
    idx <<- idx + 1L
    where <<- c(where, sprintf(sql_frag, idx))
    params[[idx]] <<- val
  }
  if (nzchar(job_filter)) add_f("job_bat = $%d", job_filter)
  if (nzchar(task_filter)) add_f("task_name = $%d", task_filter)
  if (nzchar(mode_filter)) add_f("mode = $%d", mode_filter)
  if (nzchar(desde_s)) add_f("started_at >= $%d::timestamptz", paste0(desde_s, "T00:00:00-04:00"))
  if (nzchar(hasta_s)) add_f("started_at < ($%d::date + 1)", hasta_s)

  where_sql <- if (length(where) > 0L) paste("WHERE", paste(where, collapse = " AND ")) else ""
  lim_i <- idx + 1L
  off_i <- idx + 2L
  params[[lim_i]] <- limit
  params[[off_i]] <- offset

  df <- db_query(sprintf("
    SELECT id, run_id, started_at, finished_at, exit_code, skipped,
           pasos_completados, pasos_detalle, estaciones_dga, estaciones_dmc,
           filas_afectadas_total, duracion_segundos, ultima_fecha_observacion,
           errores, log_dia, job_bat, metrics, task_name, mode, phase
    FROM parte_de_guardia
    %s
    ORDER BY started_at DESC
    LIMIT $%d OFFSET $%d
  ", where_sql, lim_i, off_i), params = params)

  count_params <- if (idx > 0L) params[seq_len(idx)] else list()
  total <- if (idx > 0L) {
    db_query(sprintf("SELECT COUNT(*) AS n FROM parte_de_guardia %s", where_sql),
             params = count_params)$n[1]
  } else {
    db_query("SELECT COUNT(*) AS n FROM parte_de_guardia")$n[1]
  }

  list(
    ok = TRUE,
    total = as.integer(total),
    limit = limit,
    offset = offset,
    job_bat = if (nzchar(job_filter)) job_filter else NULL,
    task_name = if (nzchar(task_filter)) task_filter else NULL,
    mode = if (nzchar(mode_filter)) mode_filter else NULL,
    items = lapply(seq_len(nrow(df)), function(i) actualizacion_row_list(df[i, , drop = FALSE]))
  )
}

#* Detalle de una ejecucion del pipeline diario
#* @get /admin/actualizaciones/<id:int>
function(id, req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  df <- db_query("
    SELECT *
    FROM parte_de_guardia
    WHERE id = $1
  ", params = list(as.integer(id)))

  if (nrow(df) == 0) {
    res$status <- 404
    return(list(error = "Registro no encontrado"))
  }

  row <- df[1, , drop = FALSE]
  actualizacion_row_list(row, include_full_errores = TRUE)
}

#* Progreso vivo del backfill historico Open-Meteo (bias correction)
#* @get /admin/bias-historico/progreso
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  chk <- tryCatch(
    db_query("SELECT 1 AS ok FROM information_schema.tables
              WHERE table_name = 'hito_del_archivo' LIMIT 1"),
    error = function(e) data.frame()
  )
  if (nrow(chk) == 0) {
    return(list(ok = TRUE, disponible = FALSE, mensaje = "Tablas bias no migradas"))
  }

  chunks_done <- as.integer(db_query(
    "SELECT COUNT(*)::int AS n FROM hito_del_archivo"
  )$n[1])
  staging_rows <- as.numeric(db_query(
    "SELECT COUNT(*)::bigint AS n FROM archivo_openmeteo"
  )$n[1])

  last_corrida <- tryCatch(
    db_query("
      SELECT status, chunks_total, chunks_done_after, rows_upserted,
             staging_rows_total, api_calls_used, backfill_complete, started_at,
             fase, fase_label, fase_idx, fase_total, mantencion_al_dia
      FROM corrida_del_archivo
      ORDER BY started_at DESC
      LIMIT 1
    "),
    error = function(e) {
      tryCatch(
        db_query("
          SELECT status, chunks_total, chunks_done_after, rows_upserted,
                 staging_rows_total, api_calls_used, backfill_complete, started_at
          FROM corrida_del_archivo
          ORDER BY started_at DESC
          LIMIT 1
        "),
        error = function(e2) data.frame()
      )
    }
  )

  chunks_total <- NA_integer_
  pct <- NA_real_
  backfill_complete <- FALSE
  last_status <- NULL
  last_started <- NULL
  fase <- NULL
  fase_label <- NULL
  fase_idx <- NULL
  fase_total <- NULL
  mantencion_al_dia <- NULL

  if (nrow(last_corrida) > 0) {
    chunks_total <- pg_scalar_int(last_corrida$chunks_total[1])
    last_status <- pg_scalar_text(last_corrida$status[1])
    backfill_complete <- isTRUE(last_corrida$backfill_complete[1])
    if (!is.na(last_corrida$started_at[1])) {
      last_started <- format(last_corrida$started_at[1], "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago")
    }
    done_after <- pg_scalar_int(last_corrida$chunks_done_after[1])
    if (!is.null(chunks_total) && chunks_total > 0L && !is.null(done_after)) {
      pct <- round(100 * done_after / chunks_total, 1)
    } else if (!is.null(chunks_total) && chunks_total > 0L) {
      pct <- round(100 * chunks_done / chunks_total, 1)
    }
    if ("fase" %in% names(last_corrida)) {
      fase <- pg_scalar_text(last_corrida$fase[1])
      fase_label <- pg_scalar_text(last_corrida$fase_label[1])
      fase_idx <- pg_scalar_int(last_corrida$fase_idx[1])
      fase_total <- pg_scalar_int(last_corrida$fase_total[1])
      if ("mantencion_al_dia" %in% names(last_corrida)) {
        mantencion_al_dia <- isTRUE(last_corrida$mantencion_al_dia[1])
      }
    }
  }

  # Checklist de fases vivo desde checkpoint (no depende de la ultima corrida).
  # Preferir % vivo (incluye año en curso / done del mes) sobre pct de la última corrida.
  fases <- tryCatch(unname(admin_bias_historico_fases()), error = function(e) list())
  if (length(fases) > 0L) {
    vista <- admin_bias_vista_nodo(fases)
    if (!is.null(vista$pct) && is.finite(as.numeric(vista$pct)[1])) {
      pct <- as.numeric(vista$pct)[1]
    }
    if (!is.null(vista$total) && is.finite(as.numeric(vista$total)[1]) &&
        as.numeric(vista$total)[1] > 0) {
      chunks_done <- as.integer(round(vista$done))
      chunks_total <- as.integer(round(vista$total))
    }
    fase <- vista$fase
    fase_label <- vista$fase_label
    fase_idx <- vista$fase_idx
    fase_total <- vista$fase_total
  }

  n_runs <- as.integer(db_query(
    "SELECT COUNT(*)::int AS n FROM parte_de_guardia
     WHERE job_bat = 'descargar_openmeteo_historico.bat'"
  )$n[1])

  list(
    ok = TRUE,
    disponible = TRUE,
    chunks_done = chunks_done,
    chunks_total = chunks_total,
    pct_complete = pct,
    staging_rows_total = staging_rows,
    backfill_complete = backfill_complete,
    last_corrida_status = last_status,
    last_corrida_started_at = last_started,
    n_runs_registradas = n_runs,
    fase = fase,
    fase_label = fase_label,
    fase_idx = fase_idx,
    fase_total = fase_total,
    mantencion_al_dia = mantencion_al_dia,
    fases = if (length(fases)) fases else list()
  )
}

# roadmap endpoints placeholder

roadmap_state_table_ok <- function() {
  chk <- tryCatch(
    db_query("SELECT 1 AS ok FROM information_schema.tables
              WHERE table_name = 'pizarra_del_plan' LIMIT 1"),
    error = function(e) data.frame()
  )
  nrow(chk) > 0
}

roadmap_read_completed <- function() {
  if (!roadmap_state_table_ok()) return(character(0))
  df <- db_query("SELECT completed_ids FROM pizarra_del_plan WHERE id = 1")
  if (nrow(df) == 0) return(character(0))
  raw <- df$completed_ids[1]
  if (is.null(raw) || length(raw) == 0) return(character(0))
  if (is.character(raw) && length(raw) == 1L) {
    parsed <- tryCatch(jsonlite::fromJSON(raw), error = function(e) NULL)
    if (is.character(parsed)) return(unique(trimws(parsed[nzchar(parsed)])))
    return(character(0))
  }
  if (is.list(raw)) return(unique(trimws(unlist(raw)[nzchar(unlist(raw))])))
  character(0)
}

roadmap_write_completed <- function(ids) {
  ids <- unique(trimws(as.character(ids)))
  ids <- ids[nzchar(ids)]
  json_ids <- jsonlite::toJSON(ids, auto_unbox = TRUE)
  db_execute(
    "INSERT INTO pizarra_del_plan (id, completed_ids, updated_at)
     VALUES (1, $1::jsonb, NOW())
     ON CONFLICT (id) DO UPDATE
     SET completed_ids = EXCLUDED.completed_ids, updated_at = NOW()",
    params = list(json_ids)
  )
}

#* Estado de tareas completadas del roadmap de lanzamiento
#* @get /admin/roadmap/state
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  if (!roadmap_state_table_ok()) {
    return(list(
      ok = TRUE,
      disponible = FALSE,
      completed = character(0),
      mensaje = "Falta la tabla pizarra_del_plan"
    ))
  }

  updated <- tryCatch(
    db_query("SELECT updated_at FROM pizarra_del_plan WHERE id = 1"),
    error = function(e) data.frame()
  )
  updated_at <- NULL
  if (nrow(updated) > 0 && !is.na(updated$updated_at[1])) {
    updated_at <- format(updated$updated_at[1], "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago")
  }

  list(
    ok = TRUE,
    disponible = TRUE,
    completed = as.list(roadmap_read_completed()),
    updated_at = updated_at
  )
}

#* Guardar tareas completadas del roadmap
#* @put /admin/roadmap/state
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  if (!roadmap_state_table_ok()) {
    res$status <- 503
    return(list(error = "Tabla pizarra_del_plan no existe"))
  }

  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)
  if (is.null(body) || is.null(body$completed)) {
    res$status <- 400
    return(list(error = "completed requerido (array de ids)"))
  }

  ids <- body$completed
  if (!is.character(ids) && !is.list(ids)) {
    res$status <- 400
    return(list(error = "completed debe ser array"))
  }

  roadmap_write_completed(ids)

  list(ok = TRUE, n = length(roadmap_read_completed()))
}

#* Eventos de la sombra de extremos (no es concordancia).
#* @get /admin/alertas/extremos-sombra
#* @param desde YYYY-MM-DD. Default hace 14 dias
#* @param limite Tope de filas. Default 200
function(req, res, desde = "", limite = "200") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  lim <- suppressWarnings(as.integer(limite))
  if (is.na(lim) || lim < 1L) lim <- 200L
  lim <- min(lim, 500L)
  desde_d <- if (nzchar(desde)) desde else as.character(Sys.Date() - 14L)
  filas <- tryCatch(db_query("
    SELECT s.id_estacion, e.nombre, lower(v.nombre_abr) AS variable,
           s.fecha, s.duracion, s.epoca, s.direccion,
           round(s.valor::numeric, 2) AS valor, s.rango,
           s.n_anios, round(s.t_empirico::numeric, 1) AS t_empirico,
           s.piso_ok, s.evento_id
    FROM extremo_en_sombra s
    JOIN magnitud v ON v.id = s.id_variable
    LEFT JOIN mirador e ON e.id = s.id_estacion
    WHERE s.origen = 'observado'
      AND s.fecha >= $1::date
      AND s.fecha < CURRENT_DATE
      AND COALESCE(s.t_empirico, 0) >= 5
    ORDER BY s.fecha DESC, s.t_empirico DESC NULLS LAST
    LIMIT $2", list(desde_d, lim)), error = function(e) {
      list(error = conditionMessage(e))
    })
  if (!is.null(filas$error)) {
    return(list(ok = FALSE, mensaje = filas$error, filas = list()))
  }
  filas$fecha <- as.character(filas$fecha)
  celda <- function(v) {
    if (length(v) != 1L || is.list(v)) return(v)
    if (is.na(v)) return(structure("null", class = "json"))
    jsonlite::unbox(v)
  }
  filas_out <- if (!nrow(filas)) list() else lapply(seq_len(nrow(filas)), function(i) {
    lapply(as.list(filas[i, , drop = FALSE]), celda)
  })
  total <- tryCatch(db_query("
    SELECT COUNT(*)::int AS n
    FROM extremo_en_sombra s
    WHERE s.origen = 'observado'
      AND s.fecha >= $1::date
      AND s.fecha < CURRENT_DATE
      AND COALESCE(s.t_empirico, 0) >= 5", list(desde_d)), error = function(e) data.frame())
  n_total <- if (!nrow(total)) nrow(filas) else as.integer(total$n[1])
  list(
    ok = jsonlite::unbox(TRUE), desde = jsonlite::unbox(desde_d),
    n = jsonlite::unbox(nrow(filas)),
    total = jsonlite::unbox(n_total),
    limite = jsonlite::unbox(as.integer(lim)),
    filas = filas_out
  )
}

#* Anticipado de lluvia: emitidos, cierre y fiabilidad. No es concordancia.
#* @get /admin/alertas/anticipada
#* @param desde YYYY-MM-DD. Default hace 14 dias
function(req, res, desde = "") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  desde_d <- if (nzchar(desde)) desde else as.character(Sys.Date() - 14L)
  vacio <- list(ok = jsonlite::unbox(TRUE), desde = jsonlite::unbox(desde_d),
                filas = list(), fiabilidad = list())
  tiene <- tryCatch(db_query("
    SELECT 1 FROM information_schema.tables
    WHERE table_name = 'umbral_antes_del_dia' LIMIT 1"), error = function(e) NULL)
  if (is.null(tiene) || !nrow(tiene)) return(vacio)
  filas <- tryCatch(db_query("
    SELECT fecha_emision::text AS fecha, criterio, lead::int AS lead, nivel,
           COUNT(*)::int AS filas,
           COUNT(*) FILTER (WHERE sale)::int AS salen,
           COUNT(*) FILTER (WHERE estado = 'cumplido')::int AS cumplidos,
           COUNT(*) FILTER (WHERE estado = 'no_cumplido')::int AS no_cumplidos,
           COUNT(*) FILTER (WHERE estado = 'sin_dato')::int AS sin_dato,
           COUNT(*) FILTER (WHERE estado = 'ya_caido')::int AS ya_caido,
           COUNT(*) FILTER (WHERE estado = 'pendiente')::int AS pendientes
    FROM umbral_antes_del_dia
    WHERE unidad = 'estacion' AND fecha_emision >= $1::date
    GROUP BY 1, 2, 3, 4
    ORDER BY 1 DESC, 2, 3", list(desde_d)), error = function(e) NULL)
  fiab <- tryCatch(db_query("
    SELECT criterio, lead::int AS lead,
           ROUND(probabilidad::numeric, 2) AS p,
           COUNT(*)::int AS n,
           ROUND(AVG((estado = 'cumplido')::int)::numeric, 3) AS frecuencia
    FROM umbral_antes_del_dia
    WHERE unidad = 'estacion' AND probabilidad IS NOT NULL
      AND estado IN ('cumplido', 'no_cumplido')
      AND fecha_fin >= $1::date
    GROUP BY 1, 2, 3
    ORDER BY 1, 2, 3", list(desde_d)), error = function(e) NULL)
  celda <- function(v) {
    if (length(v) != 1L || is.list(v)) return(v)
    if (is.na(v)) return(structure("null", class = "json"))
    jsonlite::unbox(v)
  }
  a_lista <- function(df) {
    if (is.null(df) || !nrow(df)) return(list())
    lapply(seq_len(nrow(df)), function(i) lapply(as.list(df[i, , drop = FALSE]), celda))
  }
  vig_df <- tryCatch(db_query("
    SELECT COUNT(*) FILTER (WHERE estado = 'cumplido')::int AS cumplidos,
           COUNT(*) FILTER (WHERE estado = 'no_cumplido')::int AS no_cumplidos
    FROM umbral_antes_del_dia
    WHERE unidad = 'estacion' AND criterio = 'pp_4d' AND lead = 1
      AND nivel = 'aviso' AND sale
      AND estado IN ('cumplido', 'no_cumplido')
      AND fecha_fin >= '2026-11-10'::date"), error = function(e) NULL)
  cu <- if (is.null(vig_df) || !nrow(vig_df)) 0L else as.integer(vig_df$cumplidos[1])
  no <- if (is.null(vig_df) || !nrow(vig_df)) 0L else as.integer(vig_df$no_cumplidos[1])
  vig <- if (exists("extremos_anticipada_revisar_tasa", mode = "function")) {
    extremos_anticipada_revisar_tasa(cu, no)
  } else {
    list(n = cu + no, minimo = 30L, precision = NA_real_, muestra = FALSE, revisar = FALSE)
  }
  pct <- if (is.finite(vig$precision)) {
    format(round(vig$precision * 100, 1), decimal.mark = ",", nsmall = 1, trim = TRUE)
  } else "0"
  texto <- if (!isTRUE(vig$muestra)) {
    sprintf(paste0(
      "Vigilancia de la frase: %d de %d cierres nuevos del aviso de 4 d\u00edas a 1 d\u00eda, ",
      "desde el 2026-11-10. Todav\u00eda no se revisa la tasa. El aviso sigue encendido."
    ), vig$n, vig$minimo)
  } else if (isTRUE(vig$revisar)) {
    sprintf(paste0(
      "Revisar la frase de la tasa: se cumplieron %d de %d (%s %%). Est\u00e1 bajo 15 %%. ",
      "El aviso sigue encendido."
    ), cu, vig$n, pct)
  } else {
    sprintf(paste0(
      "La frase de la tasa se mantiene: se cumplieron %d de %d (%s %%). El aviso sigue encendido."
    ), cu, vig$n, pct)
  }
  cierres_df <- tryCatch(db_query("
    SELECT DISTINCT ON (COALESCE(detalle->>'producto', 'lluvia'))
           fecha_eval::text AS fecha,
           ROUND(precision::numeric, 3) AS precision,
           tp::int AS tp, fp::int AS fp,
           COALESCE(detalle->>'producto', 'lluvia') AS producto,
           (detalle->'zonas')::text AS zonas
    FROM punteria_medida
    WHERE eval_tipo = 'anticipada_cierre'
    ORDER BY COALESCE(detalle->>'producto', 'lluvia'), id DESC"), error = function(e) data.frame())
  zonas_de <- function(raw) {
    if (is.null(raw) || !length(raw) || !is.character(raw) || !nzchar(raw[1]) || is.na(raw[1])) {
      return(list())
    }
    parsed <- tryCatch(jsonlite::fromJSON(raw[1]), error = function(e) NULL)
    if (is.null(parsed) || !is.data.frame(parsed) || !nrow(parsed)) return(list())
    lapply(seq_len(nrow(parsed)), function(i) {
      p <- suppressWarnings(as.numeric(parsed$precision[i]))
      list(
        macro = jsonlite::unbox(as.character(parsed$macro[i])),
        temporada = jsonlite::unbox(as.character(parsed$temporada[i])),
        tp = jsonlite::unbox(suppressWarnings(as.integer(parsed$tp[i]))),
        fp = jsonlite::unbox(suppressWarnings(as.integer(parsed$fp[i]))),
        precision = if (!is.finite(p)) NULL else jsonlite::unbox(p)
      )
    })
  }
  ajuste_df <- tryCatch(db_query("
    SELECT COUNT(*) FILTER (WHERE ajuste = 'M2' AND sale AND nivel = 'aviso')::int AS m2,
           COUNT(*) FILTER (WHERE ajuste = 'M2_sin_historia' AND sale AND nivel = 'aviso')::int AS sin_historia,
           COUNT(*) FILTER (WHERE ajuste = 'M0' AND sale AND nivel = 'aviso'
                            AND criterio LIKE 'tmin%')::int AS m0,
           COUNT(*) FILTER (WHERE unidad = 'estacion' AND nivel = 'aviso' AND NOT sale
                            AND fecha_emision >= CURRENT_DATE - 30)::int AS no_salen
    FROM umbral_antes_del_dia"), error = function(e) data.frame())
  aj_n <- function(col) {
    if (is.null(ajuste_df) || !nrow(ajuste_df) || !col %in% names(ajuste_df)) return(0L)
    v <- suppressWarnings(as.integer(ajuste_df[[col]][1]))
    if (!length(v) || is.na(v)) 0L else v
  }
  cierres <- if (is.null(cierres_df) || !nrow(cierres_df)) list() else {
    lapply(seq_len(nrow(cierres_df)), function(i) {
      p <- suppressWarnings(as.numeric(cierres_df$precision[i]))
      list(
        fecha = jsonlite::unbox(as.character(cierres_df$fecha[i])),
        producto = jsonlite::unbox(as.character(cierres_df$producto[i])),
        precision = if (!is.finite(p)) NULL else jsonlite::unbox(p),
        tp = jsonlite::unbox(suppressWarnings(as.integer(cierres_df$tp[i]))),
        fp = jsonlite::unbox(suppressWarnings(as.integer(cierres_df$fp[i]))),
        zonas = zonas_de(cierres_df$zonas[i])
      )
    })
  }
  list(ok = jsonlite::unbox(TRUE), desde = jsonlite::unbox(desde_d),
       filas = a_lista(filas), fiabilidad = a_lista(fiab),
       cierres = cierres,
       ajuste = list(
         m2 = jsonlite::unbox(aj_n("m2")),
         sin_historia = jsonlite::unbox(aj_n("sin_historia")),
         m0 = jsonlite::unbox(aj_n("m0")),
         no_salen_30d = jsonlite::unbox(aj_n("no_salen"))
       ),
       vigilancia = list(
         revisar = jsonlite::unbox(isTRUE(vig$revisar)),
         n = jsonlite::unbox(as.integer(vig$n)),
         minimo = jsonlite::unbox(as.integer(vig$minimo)),
         texto = jsonlite::unbox(texto)
       ))
}

#* Precision top-10 por horizonte (lead_days), filtrable
#* @get /admin/alertas/concordancia/por-horizonte
#* @param variable Abr (pp|t_max|t_min|all). Default pp
#* @param region cod_reg opcional
#* @param comuna nom_com exacto opcional
#* @param desde YYYY-MM-DD opcional
#* @param hasta YYYY-MM-DD opcional
function(req, res, variable = "pp", region = "", comuna = "", desde = "", hasta = "") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  out <- tryCatch(
    admin_concordancia_por_horizonte(variable, region, comuna, desde, hasta),
    error = function(e) list(disponible = FALSE, mensaje = conditionMessage(e), series = list())
  )
  out$ok <- TRUE
  out
}

#* Tipos de alerta: regla escrita, orden del admin y conteos de 30 dias.
#* @get /admin/alertas/tipos
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  if (!exists("alertas_tipo_inventario", mode = "function")) {
    res$status <- 503
    return(list(ok = FALSE, error = "Falta alertas_tipo_control.R"))
  }
  tryCatch(alertas_tipo_inventario(), error = function(e) {
    res$status <- 500
    list(ok = FALSE, error = conditionMessage(e))
  })
}

#* Ficha de una hoja del arbol: texto, regla y numeros de esa celda.
#* @get /admin/alertas/tipos/ficha
#* @param hoja Id de la hoja (por ejemplo ant_pp_1d_l1 o temprana_pp_max_l2)
function(req, res, hoja = "") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  if (exists("alertas_tipo_cargar_fichas", mode = "function")) alertas_tipo_cargar_fichas()
  if (!exists("alertas_tipo_ficha_armar", mode = "function")) {
    res$status <- 503
    return(list(ok = FALSE, error = "Falta alertas_tipo_fichas.R"))
  }
  out <- tryCatch(alertas_tipo_ficha_armar(hoja), error = function(e) {
    res$status <- 500
    list(ok = FALSE, error = conditionMessage(e))
  })
  if (is.null(out)) {
    res$status <- 404
    return(list(ok = FALSE, error = "Ficha desconocida"))
  }
  out
}

#* Encender, apagar o volver a la regla. Encender contra la regla pide forzar.
#* @post /admin/alertas/tipos/<tipo_id>
function(tipo_id, req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  if (!exists("alertas_tipo_por_id", mode = "function")) {
    res$status <- 503
    return(list(ok = FALSE, error = "Falta alertas_tipo_control.R"))
  }
  fila <- alertas_tipo_por_id(tipo_id)
  if (is.null(fila)) {
    res$status <- 404
    return(list(ok = FALSE, error = "Tipo desconocido"))
  }
  body <- tryCatch(jsonlite::fromJSON(req$postBody %||% "{}"), error = function(e) list())
  if (is.null(body)) body <- list()
  accion <- tolower(trimws(as.character(body$accion %||% "")))
  forzar <- isTRUE(body$forzar) || identical(tolower(as.character(body$forzar %||% "")), "true")
  if (!accion %in% c("encender", "apagar", "restaurar")) {
    res$status <- 400
    return(list(ok = FALSE, error = "accion debe ser encender, apagar o restaurar"))
  }
  if (identical(accion, "restaurar")) {
    alertas_tipo_borrar(fila$id)
    return(list(ok = TRUE, id = fila$id, accion = "restaurar"))
  }
  if (identical(accion, "encender") && !isTRUE(fila$condicion_cumplida) && !forzar) {
    res$status <- 409
    return(list(
      ok = FALSE, necesita_forzar = TRUE, id = fila$id, nombre = fila$nombre,
      regla = fila$regla, sin_emisor = isTRUE(fila$sin_emisor), reporte = fila$reporte,
      sentido = "encender"
    ))
  }
  if (identical(accion, "apagar") && isTRUE(fila$emite_regla) && isTRUE(fila$condicion_cumplida) && !forzar) {
    res$status <- 409
    return(list(
      ok = FALSE, necesita_forzar = TRUE, id = fila$id, nombre = fila$nombre,
      regla = fila$regla, sin_emisor = FALSE, reporte = fila$reporte,
      sentido = "apagar"
    ))
  }
  guardar <- if (identical(accion, "encender")) "encendida" else "apagada"
  forzada <- identical(accion, "encender") && !isTRUE(fila$condicion_cumplida)
  alertas_tipo_guardar(fila$id, guardar, forzada, fila$regla)
  list(
    ok = TRUE, id = fila$id, accion = guardar, forzada = forzada,
    incumple = forzada, sin_emisor = isTRUE(fila$sin_emisor), regla = fila$regla
  )
}

#* Concordancia alertas tempranas (pronostico) vs tardias (observado)
#* @get /admin/alertas/concordancia
#* @param lead_max Tope de lead_days para TP/FP (entero). Vacio/omitido = todos. FN siempre incluidos.
#* @param elegible Solo estaciones skill elegible=TRUE (1/true). Diagnostico; metrica norte = todas.
#* @param regla activa (default) = rank emit + pause + margen del loop; catalogo = verificacion cruda.
#* @param fuente all|live|hindcast (default all = Total live+hindcast para analisis).
#* @param tipos Ids clasicos separados por coma. Vacio = todos. ninguno = nada.
function(req, res, lead_max = "", elegible = "", regla = "activa", fuente = "all", tipos = "") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  tbl_ok <- tryCatch(
    nrow(db_query("SELECT 1 FROM information_schema.tables WHERE table_name = 'acta_de_chequeo' LIMIT 1")) > 0,
    error = function(e) FALSE
  )
  if (!tbl_ok) {
    return(list(ok = TRUE, disponible = FALSE,
                mensaje = "Faltan las tablas foto_del_aviso y acta_de_chequeo"))
  }

  lm_raw <- trimws(as.character(lead_max %||% ""))
  lm <- if (!nzchar(lm_raw) || identical(tolower(lm_raw), "all") ||
            identical(tolower(lm_raw), "todos")) {
    NULL
  } else {
    suppressWarnings(as.integer(lm_raw))
  }
  if (!is.null(lm) && (is.na(lm) || lm < 0L)) lm <- NULL

  el_raw <- tolower(trimws(as.character(elegible %||% "")))
  only_eleg <- el_raw %in% c("1", "true", "si", "yes", "elegible")

  regla_raw <- tolower(trimws(as.character(regla %||% "activa")))
  use_regla <- !(regla_raw %in% c("catalogo", "raw", "crudo", "baseline", "0", "off"))

  fuente_raw <- tolower(trimws(as.character(fuente %||% "all")))
  if (fuente_raw %in% c("live", "l")) {
    fuente_f <- "live"
  } else if (fuente_raw %in% c("hindcast", "hc", "historico", "hist")) {
    fuente_f <- "hindcast"
  } else {
    fuente_f <- "all"
  }

  tipos_raw <- trimws(as.character(tipos %||% ""))
  tipos_v <- if (exists("alertas_tipo_clasicos_clause", mode = "function")) {
    alertas_tipo_clasicos_clause(if (nzchar(tipos_raw)) tipos_raw else NULL, "var", "v")
  } else {
    "TRUE"
  }
  tipos_ver <- if (exists("alertas_tipo_clasicos_clause", mode = "function")) {
    alertas_tipo_clasicos_clause(if (nzchar(tipos_raw)) tipos_raw else NULL, "var", "ver")
  } else {
    "TRUE"
  }

  df_rows <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(list())
    lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
  }

  st <- if (exists("alerta_loop_status_row", mode = "function")) {
    alerta_loop_status_row()
  } else {
    data.frame()
  }
  rank_max <- if (nrow(st) && !is.na(st$rank_max_emit[1])) as.integer(st$rank_max_emit[1]) else 3L
  pause <- if (nrow(st) && !is.na(st$pause_t_min_min[1])) isTRUE(as.logical(st$pause_t_min_min[1])) else TRUE
  margenes_on <- if (nrow(st) && !is.na(st$enabled[1])) isTRUE(as.logical(st$enabled[1])) else TRUE
  por_est <- data.frame()
  por_var <- data.frame()
  por_var_live <- data.frame()
  por_var_hc <- data.frame()
  serie_semanal_live <- data.frame()
  recientes_live <- data.frame()
  resumen_live <- if (exists("alerta_concordancia_resumen_vacio", mode = "function")) {
    alerta_concordancia_resumen_vacio()
  } else {
    list(tp = 0L, fp = 0L, fn = 0L, n_alertas = 0L)
  }
  resumen_hc <- resumen_live

  if (use_regla && exists("alerta_concordancia_con_regla", mode = "function")) {
    scored <- alerta_concordancia_con_regla(
      lead_max = lm, only_elegible = only_eleg,
      rank_max = rank_max, pause_t_min_min = pause, use_margenes = margenes_on,
      fuente = fuente_f, tipos_clause = tipos_v
    )
    tp <- scored$tp; fp <- scored$fp; fn <- scored$fn
    prec <- scored$precision_top10; rec <- scored$recall_top10
    por_var <- scored$por_variable
    por_est <- scored$por_estacion
    desde <- scored$desde; hasta <- scored$hasta
    resumen_live <- scored$resumen_live %||% resumen_live
    resumen_hc <- scored$resumen_hindcast %||% resumen_hc
    por_var_live <- scored$por_variable_live %||% por_var_live
    por_var_hc <- scored$por_variable_hindcast %||% por_var_hc
    serie_semanal_live <- scored$serie_semanal_live %||% serie_semanal_live
    recientes_live <- scored$recientes_live %||% recientes_live
  } else {
    # Catalogo crudo: solo dias con cobertura; FN sin prediccion fuera del universo.
    fuente_clause <- if (exists("alerta_concordancia_fuente_clause", mode = "function")) {
      alerta_concordancia_fuente_clause(fuente_f, alias = "ver")
    } else {
      "TRUE"
    }
    pred_clause <- if (is.null(lm)) {
      paste0(
        "ver.cobertura_catalogo AND (",
        "  ver.clase IN ('TP','FP')",
        "  OR (ver.clase = 'FN' AND ver.ranking_pronostico IS NOT NULL)",
        ") AND ", fuente_clause, " AND ", tipos_ver
      )
    } else {
      paste0(
        "ver.cobertura_catalogo AND ver.clase IN ('TP','FP')",
        " AND COALESCE(ver.lead_days, 999) <= ", as.integer(lm),
        " AND ", fuente_clause, " AND ", tipos_ver
      )
    }
    eleg_join <- if (only_eleg) {
      " JOIN oficio_del_mirador sk ON sk.id_estacion = ver.id_estacion AND sk.elegible = TRUE "
    } else {
      ""
    }
    resumen <- db_query(paste0("
      SELECT
        COUNT(*) FILTER (WHERE ver.clase = 'TP') AS tp,
        COUNT(*) FILTER (WHERE ver.clase = 'FP') AS fp,
        COUNT(*) FILTER (WHERE ver.clase = 'FN') AS fn,
        MIN(ver.fecha_objetivo) AS desde,
        MAX(ver.fecha_objetivo) AS hasta
      FROM acta_de_chequeo ver
      JOIN magnitud var ON var.id = ver.id_variable
      ", eleg_join, "
      WHERE ", pred_clause))
    tp <- as.integer(resumen$tp[1] %||% 0L)
    fp <- as.integer(resumen$fp[1] %||% 0L)
    fn <- as.integer(resumen$fn[1] %||% 0L)
    prec <- if ((tp + fp) > 0) round(tp / (tp + fp), 3) else NULL
    rec <- if ((tp + fn) > 0) round(tp / (tp + fn), 3) else NULL
    desde <- if (is.na(resumen$desde[1])) NULL else as.character(resumen$desde[1])
    hasta <- if (is.na(resumen$hasta[1])) NULL else as.character(resumen$hasta[1])
    por_var <- db_query(paste0("
      SELECT
        ver.id_variable, var.nombre_abr, ver.direccion,
        COUNT(*) FILTER (WHERE ver.clase = 'TP') AS tp,
        COUNT(*) FILTER (WHERE ver.clase = 'FP') AS fp,
        COUNT(*) FILTER (WHERE ver.clase = 'FN') AS fn,
        COUNT(*) FILTER (WHERE ver.clase IN ('TP', 'FP')) AS predichos,
        COUNT(*) FILTER (WHERE ver.clase IN ('TP', 'FN')) AS observados,
        ROUND(COUNT(*) FILTER (WHERE ver.clase = 'TP')::numeric
              / NULLIF(COUNT(*) FILTER (WHERE ver.clase IN ('TP', 'FP')), 0), 3) AS precision_top10,
        ROUND(COUNT(*) FILTER (WHERE ver.clase = 'TP')::numeric
              / NULLIF(COUNT(*) FILTER (WHERE ver.clase IN ('TP', 'FN')), 0), 3) AS recall_top10
      FROM acta_de_chequeo ver
      JOIN magnitud var ON var.id = ver.id_variable
      ", eleg_join, "
      WHERE ", pred_clause, "
      GROUP BY ver.id_variable, var.nombre_abr, ver.direccion
      ORDER BY var.nombre_abr, ver.direccion"))
    por_est <- if (exists("alerta_concordancia_por_estacion_catalogo", mode = "function")) {
      alerta_concordancia_por_estacion_catalogo(
        lead_max = lm, only_elegible = only_eleg, fuente = fuente_f,
        tipos_clause = tipos_v
      )
    } else {
      data.frame()
    }
    if (exists("alerta_concordancia_resumen_por_fuente", mode = "function")) {
      resumen_live <- alerta_concordancia_resumen_por_fuente(
        lead_max = lm, only_elegible = only_eleg, fuente = "live",
        tipos_clause = tipos_ver
      )
      resumen_hc <- alerta_concordancia_resumen_por_fuente(
        lead_max = lm, only_elegible = only_eleg, fuente = "hindcast",
        tipos_clause = tipos_ver
      )
    }
    if (exists("alerta_concordancia_panel_extras", mode = "function") &&
        exists("alerta_concordancia_univ_clause", mode = "function")) {
      cat_lead <- if (is.null(lm)) "TRUE" else paste0("COALESCE(v.lead_days, 999) <= ", as.integer(lm))
      cat_lead <- paste0("(", cat_lead, ") AND (", tipos_v, ")")
      cat_eleg <- if (only_eleg) {
        " JOIN oficio_del_mirador sk ON sk.id_estacion = v.id_estacion AND sk.elegible = TRUE "
      } else {
        ""
      }
      extras <- tryCatch(
        alerta_concordancia_panel_extras(
          univ_clause = alerta_concordancia_univ_clause(),
          lead_clause = cat_lead,
          eleg_join = cat_eleg,
          margen_join = "",
          clase_sim = "v.clase"
        ),
        error = function(e) NULL
      )
      if (!is.null(extras)) {
        por_var_live <- extras$por_variable_live %||% por_var_live
        por_var_hc <- extras$por_variable_hindcast %||% por_var_hc
        serie_semanal_live <- extras$serie_semanal_live %||% serie_semanal_live
        recientes_live <- extras$recientes_live %||% recientes_live
      }
    }
  }

  por_hor <- if (!identical(tipos_v, "TRUE")) {
    tryCatch(db_query(paste0(
      "SELECT v.lead_days, ",
      " COUNT(*) FILTER (WHERE v.clase IN ('TP','FP'))::int AS predicciones, ",
      " COUNT(*) FILTER (WHERE v.clase = 'TP')::int AS aciertos, ",
      " ROUND(COUNT(*) FILTER (WHERE v.clase = 'TP')::numeric ",
      "   / NULLIF(COUNT(*) FILTER (WHERE v.clase IN ('TP','FP')), 0), 3) AS precision_top10 ",
      "FROM acta_de_chequeo v ",
      "JOIN magnitud var ON var.id = v.id_variable ",
      "WHERE v.cobertura_catalogo AND v.clase IN ('TP','FP','FN') AND ", tipos_v,
      " GROUP BY 1 ORDER BY 1"
    )), error = function(e) data.frame())
  } else {
    tryCatch(db_query("SELECT * FROM alerta_concordancia_por_horizonte"),
             error = function(e) data.frame())
  }
  por_capa <- tryCatch(db_query(paste0("
    SELECT
      CASE WHEN COALESCE(v.lead_days, 0) <= 1 THEN 'emit' ELSE 'shadow' END AS capa,
      COUNT(*) FILTER (WHERE v.clase = 'TP') AS tp,
      COUNT(*) FILTER (WHERE v.clase = 'FP') AS fp,
      COUNT(*) FILTER (WHERE v.clase = 'FN' AND v.ranking_pronostico IS NOT NULL) AS fn,
      ROUND(COUNT(*) FILTER (WHERE v.clase = 'TP')::numeric
            / NULLIF(COUNT(*) FILTER (WHERE v.clase IN ('TP','FP')), 0), 3) AS precision_top10
    FROM acta_de_chequeo v
    JOIN magnitud var ON var.id = v.id_variable
    WHERE v.cobertura_catalogo AND v.clase IN ('TP','FP','FN') AND ", tipos_v, "
    GROUP BY 1
    ORDER BY 1
  ")), error = function(e) data.frame())

  list(
    ok = TRUE,
    disponible = TRUE,
    lead_max = if (is.null(lm)) NULL else as.integer(lm),
    elegible_only = only_eleg,
    fuente = fuente_f,
    regla = if (use_regla) "activa" else "catalogo",
    regla_detalle = if (use_regla) {
      list(
        rank_max = rank_max,
        pause_t_min_min = pause,
        margenes = margenes_on,
        regla_version = if (nrow(st)) as.character(st$regla_version[1] %||% NA) else NULL
      )
    } else {
      list(rank_max = 10L, pause_t_min_min = FALSE, margenes = FALSE)
    },
    resumen = list(
      tp = tp, fp = fp, fn = fn,
      n_alertas = tp + fp,
      precision_top10 = prec,
      recall_top10 = rec,
      desde = desde,
      hasta = hasta
    ),
    resumen_live = resumen_live,
    resumen_hindcast = resumen_hc,
    por_variable = df_rows(por_var),
    por_variable_live = df_rows(por_var_live),
    por_variable_hindcast = df_rows(por_var_hc),
    serie_semanal_live = df_rows(serie_semanal_live),
    recientes_live = df_rows(recientes_live),
    por_estacion = df_rows(por_est),
    por_horizonte = df_rows(por_hor),
    por_capa = df_rows(por_capa),
    nota_tp = paste0(
      "TP = predijo entrada al top-10 y s\u00ed entr\u00f3 (no exige puesto exacto). ",
      "Total = live + hindcast (OM hist\u00f3rico recalculado). ",
      "Hindcast = simulaci\u00f3n; no son alertas enviadas a usuarios. ",
      if (use_regla) {
        paste0(
          "KPI con regla activa: ranking<=", rank_max,
          if (pause) " + pausa t_min/min" else "",
          if (margenes_on) " + margen calibrado" else "",
          ". "
        )
      } else {
        "KPI catalogo crudo (rank<=10, sin margen/pausa). "
      },
      "Universo: cobertura_catalogo. Fuente=", fuente_f, ". ",
      if (is.null(lm)) "Sin tope de lead (todos los horizontes)."
      else paste0("TP/FP/FN-con-pred con lead_days <= ", lm, "."),
      if (only_eleg) " Filtro diagnostico: solo estaciones skill elegibles."
      else ""
    )
  )
}

#* Serie de evaluaciones del loop de precision (backtest/live/rollback)
#* @get /admin/alertas/precision-eval
#* @param limit Max filas (default 60)
function(req, res, limit = "60") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  lim <- suppressWarnings(as.integer(limit))
  if (is.na(lim) || lim < 1L) lim <- 60L
  lim <- min(lim, 200L)

  tbl_ok <- tryCatch(
    nrow(db_query("SELECT 1 FROM information_schema.tables WHERE table_name='punteria_medida' LIMIT 1")) > 0,
    error = function(e) FALSE
  )
  if (!tbl_ok) {
    return(list(ok = TRUE, disponible = FALSE, mensaje = "Sin punteria_medida", series = list()))
  }

  rows <- tryCatch(db_query(paste0(
    "SELECT id, fecha_eval, eval_tipo, regla_version, lead_max, ventana_desde, ventana_hasta,",
    " tp, fp, fn, precision, recall, fp_dia, precision_esperada_backtest, baseline_precision,",
    " estado, detalle, created_at",
    " FROM punteria_medida ORDER BY id DESC LIMIT ", lim
  )), error = function(e) data.frame())

  st <- tryCatch(db_query("SELECT * FROM punteria_en_ronda WHERE id = 1"),
                 error = function(e) data.frame())

  margen_stats <- tryCatch(db_query(paste0(
    "SELECT COUNT(*) FILTER (WHERE margen < 1e8)::int AS n_margen_ok,",
    " COUNT(*) FILTER (WHERE margen >= 1e8)::int AS n_desactivadas,",
    " COUNT(*) FILTER (WHERE margen < 0 AND margen > -1e8)::int AS n_margen_neg ",
    "FROM margen_del_tope WHERE activo = TRUE"
  )), error = function(e) data.frame())

  # Extraer auc_pr del detalle mas reciente que lo tenga
  auc_pr <- NULL
  det_latest <- tryCatch(db_query(paste0(
    "SELECT detalle FROM punteria_medida ",
    "WHERE detalle IS NOT NULL AND detalle ? 'auc_pr' ",
    "ORDER BY id DESC LIMIT 1"
  )), error = function(e) data.frame())
  if (nrow(det_latest) > 0) {
    d <- det_latest$detalle[[1]] %||% det_latest$detalle[1]
    if (is.character(d)) {
      parsed <- tryCatch(jsonlite::fromJSON(d), error = function(e) NULL)
      if (!is.null(parsed) && !is.null(parsed$auc_pr)) auc_pr <- as.numeric(parsed$auc_pr)
    } else if (is.list(d) && !is.null(d$auc_pr)) {
      auc_pr <- as.numeric(d$auc_pr)
    }
  }

  df_rows <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(list())
    lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
  }

  list(
    ok = TRUE,
    disponible = TRUE,
    loop_status = if (nrow(st)) as.list(st[1, , drop = FALSE]) else NULL,
    diagnostico = list(
      auc_pr = auc_pr,
      n_margen_ok = if (nrow(margen_stats)) as.integer(margen_stats$n_margen_ok[1]) else NULL,
      n_desactivadas = if (nrow(margen_stats)) as.integer(margen_stats$n_desactivadas[1]) else NULL,
      n_margen_neg = if (nrow(margen_stats)) as.integer(margen_stats$n_margen_neg[1]) else NULL
    ),
    series = df_rows(rows)
  )
}

#* Ranking skill estaciones + elegibles (mejora continua alertas)
#* @get /admin/alertas/skill
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  if (exists("alerta_skill_ensure_schema", mode = "function")) {
    tryCatch(alerta_skill_ensure_schema(NULL), error = function(e) NULL)
  }

  tbl_ok <- tryCatch(
    nrow(db_query("SELECT 1 FROM information_schema.tables WHERE table_name = 'oficio_del_mirador' LIMIT 1")) > 0,
    error = function(e) FALSE
  )
  if (!tbl_ok) {
    return(list(ok = TRUE, disponible = FALSE,
                mensaje = "Falta la tabla oficio_del_mirador"))
  }

  resumen <- db_query("
    SELECT
      COUNT(*)::int AS n_estaciones,
      COUNT(*) FILTER (WHERE elegible)::int AS n_elegibles,
      COUNT(*) FILTER (WHERE NOT elegible AND n_pred >= $1 AND precision >= $2)::int AS n_casi_elegibles,
      COUNT(*) FILTER (WHERE n_pred < $1)::int AS n_muestra_insuficiente,
      ROUND(AVG(precision) FILTER (WHERE n_pred >= $1)::numeric, 3) AS prec_media_calificadas,
      SUM(tp)::int AS tp, SUM(fp)::int AS fp
    FROM oficio_del_mirador
  ", params = list(
    if (exists("alerta_skill_min_n", mode = "function")) alerta_skill_min_n() else 8L,
    if (exists("alerta_skill_enter_prec", mode = "function")) alerta_skill_enter_prec() else 0.25
  ))

  eventos_hoy <- tryCatch(db_query("
    SELECT
      COUNT(*) FILTER (WHERE tipo = 'graduacion')::int AS graduaciones,
      COUNT(*) FILTER (WHERE tipo = 'degradacion')::int AS degradaciones,
      COUNT(*) FILTER (WHERE tipo = 'bootstrap')::int AS bootstrap
    FROM oficio_cumplido
    WHERE created_at::date = CURRENT_DATE
  "), error = function(e) data.frame(graduaciones = 0L, degradaciones = 0L, bootstrap = 0L))

  ranking <- tryCatch(db_query("
    SELECT s.id_estacion, e.nombre AS estacion_nombre, e.nom_com, ec.nivel AS calidad_nivel,
           s.tp, s.fp, s.fn, s.precision, s.n_pred, s.fuente, s.elegible, s.motivo,
           s.prec_hindcast, s.prec_live, s.updated_at
    FROM oficio_del_mirador s
    LEFT JOIN mirador e ON e.id = s.id_estacion
    LEFT JOIN estacion_calidad ec ON ec.id_estacion = s.id_estacion
    ORDER BY s.elegible DESC, s.precision DESC NULLS LAST, s.n_pred DESC
    LIMIT 400
  "), error = function(e) data.frame())

  df_rows <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(list())
    lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
  }

  min_n <- if (exists("alerta_skill_min_n", mode = "function")) alerta_skill_min_n() else 8L
  enter_p <- if (exists("alerta_skill_enter_prec", mode = "function")) alerta_skill_enter_prec() else 0.25
  exit_p <- if (exists("alerta_skill_exit_prec", mode = "function")) alerta_skill_exit_prec() else 0.20
  emit_lead <- if (exists("alerta_skill_emit_lead_max", mode = "function")) alerta_skill_emit_lead_max() else 1L
  shadow_lead <- if (exists("alerta_skill_shadow_lead_max", mode = "function")) alerta_skill_shadow_lead_max() else 2L
  min_enforce <- if (exists("alerta_skill_min_elegibles_enforce", mode = "function")) {
    alerta_skill_min_elegibles_enforce()
  } else {
    30L
  }
  n_eleg <- as.integer(resumen$n_elegibles[1] %||% 0L)
  allowlist_activa <- isTRUE(n_eleg >= min_enforce)

  list(
    ok = TRUE,
    disponible = TRUE,
    resumen = as.list(resumen[1, , drop = FALSE]),
    eventos_hoy = as.list(eventos_hoy[1, , drop = FALSE]),
    ranking = df_rows(ranking),
    estado_allowlist = list(
      activa = allowlist_activa,
      n_elegibles = n_eleg,
      min_para_activar = as.integer(min_enforce),
      faltan_para_activar = max(0L, as.integer(min_enforce) - n_eleg),
      mensaje = if (allowlist_activa) {
        paste0(
          "ALLOWLIST ACTIVA: mapa/reporte/correo solo usan las ", n_eleg,
          " estaciones elegibles (umbral de activacion = ", min_enforce, ")."
        )
      } else {
        paste0(
          "ALLOWLIST APAGADA: hay ", n_eleg, " estaciones elegibles (marcadas en ranking), ",
          "pero el filtro aun NO se aplica en producto. Faltan ",
          max(0L, as.integer(min_enforce) - n_eleg),
          " elegibles para llegar a ", min_enforce,
          ". Mientras tanto EMIT = todas excelente + lead<=", emit_lead, "."
        )
      }
    ),
    umbrales = list(
      min_n = min_n,
      enter_prec = enter_p,
      exit_prec = exit_p,
      emit_lead_max = emit_lead,
      shadow_lead_max = shadow_lead,
      min_elegibles_enforce = min_enforce,
      digest_rank_max = if (exists("alerta_digest_rank_max", mode = "function")) {
        alerta_digest_rank_max()
      } else {
        3L
      },
      live_days = 90L
    ),
    monitores = list(
      "TP = predijo entrada al top-10 y SI entro (no exige puesto exacto).",
      "SHADOW: scorea excelente lead<=2 (graduacion). EMIT producto: lead<=1.",
      "Digest correo: ranking<=3 dentro del universo EMIT.",
      "Mapa/reporte: ranking<=10, lead<=1, excelente.",
      "Diario: alertas → concordancia → skill (hindcast+live mismo peso) → bias_historico (T+1).",
      "Bias apply: coefs por estacion desde OM hist; gate apply_alertas por Δ precisión hindcast.",
      "Allowlist: n_pred = n_hindcast + n_live (cada alerta hipotetica cuenta igual que live).",
      "Mirar cada dia: n_elegibles, n_apply_on, Δ alertas crudo/corr, graduaciones."
    ),
    nota = paste0(
      "Elegible si n>=", min_n, " y prec>=", enter_p, " (salida prec<", exit_p, "). ",
      "Allowlist producto se activa solo con >=", min_enforce, " elegibles."
    )
  )
}

#* Drilldown: alertas de una estacion (live verificadas + hindcast del gate)
#* @get /admin/alertas/skill/estacion
#* @param id_estacion:int
#* @param limit:int
#* @param include_hindcast:character
#* @param lead_max Tope lead para historial LIVE (default 1; vacio/all = todos)
function(req, res, id_estacion = 0, limit = 80, include_hindcast = "0", lead_max = "1") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  id_est <- suppressWarnings(as.integer(id_estacion))
  if (is.na(id_est) || id_est <= 0L) {
    res$status <- 400L
    return(list(ok = FALSE, error = "id_estacion requerido"))
  }
  lim <- suppressWarnings(as.integer(limit))
  if (is.na(lim) || lim < 1L) lim <- 80L
  lim <- max(1L, min(lim, 200L))
  include_hc <- tolower(trimws(as.character(include_hindcast %||% "0"))) %in%
    c("1", "true", "yes", "si")
  lm_raw <- trimws(as.character(lead_max %||% "1"))
  lm <- if (!nzchar(lm_raw) || identical(tolower(lm_raw), "all") ||
            identical(tolower(lm_raw), "todos")) {
    NULL
  } else {
    suppressWarnings(as.integer(lm_raw))
  }
  if (!is.null(lm) && (is.na(lm) || lm < 0L)) lm <- 1L
  lead_sql <- if (is.null(lm)) "TRUE" else paste0("COALESCE(av.lead_days, 999) <= ", as.integer(lm))

  if (!admin_tabla_existe("acta_de_chequeo")) {
    return(list(ok = TRUE, disponible = FALSE,
                mensaje = "Faltan las tablas foto_del_aviso y acta_de_chequeo"))
  }

  meta <- tryCatch(db_query("
    SELECT e.id AS id_estacion, e.nombre AS estacion_nombre, e.nom_com,
           ec.nivel AS calidad_nivel,
           s.tp, s.fp, s.fn, s.precision, s.n_pred, s.elegible, s.motivo, s.fuente,
           s.n_hindcast, s.n_live, s.prec_hindcast, s.prec_live
    FROM mirador e
    LEFT JOIN estacion_calidad ec ON ec.id_estacion = e.id
    LEFT JOIN oficio_del_mirador s ON s.id_estacion = e.id
    WHERE e.id = $1
    LIMIT 1
  ", params = list(id_est)), error = function(e) data.frame())

  if (is.null(meta) || nrow(meta) == 0) {
    res$status <- 404L
    return(list(ok = FALSE, error = paste0("Estacion ", id_est, " no encontrada")))
  }

  # Live: alertas verificadas con cobertura catalogo (valores + rankings).
  alertas_live <- tryCatch(db_query(paste0("
    SELECT
      av.fecha_objetivo,
      av.fecha_objetivo - COALESCE(av.lead_days, 0) AS fecha_pronostico,
      av.lead_days,
      CASE WHEN COALESCE(av.lead_days, 0) <= 1 THEN 'emit' ELSE 'shadow' END AS capa,
      v.nombre_abr,
      av.direccion,
      av.ranking_pronostico,
      av.ranking_observado,
      av.valor_pronostico,
      av.valor_observado,
      av.confianza,
      av.clase,
      CASE
        WHEN av.clase = 'TP' THEN 'acertada'
        WHEN av.clase = 'FP' THEN 'fallida'
        ELSE av.clase
      END AS resultado,
      CASE
        WHEN av.clase = 'TP' AND av.ranking_pronostico IS NOT NULL
             AND av.ranking_observado IS NOT NULL
          THEN ABS(av.ranking_pronostico - av.ranking_observado)
        ELSE NULL
      END AS delta_rank,
      av.verificado_en,
      COALESCE(av.origen, 'live')::text AS origen,
      'live'::text AS fuente_alerta
    FROM acta_de_chequeo av
    JOIN magnitud v ON v.id = av.id_variable
    WHERE av.id_estacion = $1
      AND av.cobertura_catalogo
      AND av.clase IN ('TP', 'FP')
      AND COALESCE(av.origen, 'live') = 'live'
      AND ", lead_sql, "
    ORDER BY av.fecha_objetivo DESC, av.lead_days ASC NULLS LAST, v.nombre_abr, av.direccion
    LIMIT $2
  "), params = list(id_est, lim)), error = function(e) data.frame())

  resumen_live <- tryCatch(db_query(paste0("
    SELECT
      COUNT(*) FILTER (WHERE clase = 'TP')::int AS tp,
      COUNT(*) FILTER (WHERE clase = 'FP')::int AS fp,
      COUNT(*) FILTER (WHERE clase = 'FN' AND ranking_pronostico IS NOT NULL)::int AS fn,
      COUNT(*) FILTER (WHERE clase IN ('TP','FP'))::int AS n_alertas,
      ROUND(COUNT(*) FILTER (WHERE clase = 'TP')::numeric
            / NULLIF(COUNT(*) FILTER (WHERE clase IN ('TP','FP')), 0), 3) AS precision,
      ROUND(COUNT(*) FILTER (WHERE clase = 'TP')::numeric
            / NULLIF(COUNT(*) FILTER (WHERE clase = 'TP'
              OR (clase = 'FN' AND ranking_pronostico IS NOT NULL)), 0), 3) AS recall,
      MIN(fecha_objetivo) AS desde,
      MAX(fecha_objetivo) AS hasta
    FROM acta_de_chequeo
    WHERE id_estacion = $1 AND cobertura_catalogo
      AND clase IN ('TP','FP')
      AND COALESCE(origen, 'live') = 'live'
      AND ", gsub("av\\.", "", lead_sql), "
  "), params = list(id_est)), error = function(e) data.frame())

  # Hindcast persistido (origen=hindcast). Fallback on-the-fly si no hay filas y include_hc.
  alertas_hindcast_df <- tryCatch(db_query(paste0("
    SELECT
      av.fecha_objetivo,
      av.fecha_objetivo - COALESCE(av.lead_days, 0) AS fecha_pronostico,
      av.lead_days,
      'emit'::text AS capa,
      v.nombre_abr,
      av.direccion,
      av.ranking_pronostico,
      av.ranking_observado,
      av.valor_pronostico,
      av.valor_observado,
      av.confianza,
      av.clase,
      CASE
        WHEN av.clase = 'TP' THEN 'acertada'
        WHEN av.clase = 'FP' THEN 'fallida'
        ELSE av.clase
      END AS resultado,
      CASE
        WHEN av.clase = 'TP' AND av.ranking_pronostico IS NOT NULL
             AND av.ranking_observado IS NOT NULL
          THEN ABS(av.ranking_pronostico - av.ranking_observado)
        ELSE NULL
      END AS delta_rank,
      av.verificado_en,
      'hindcast'::text AS origen,
      'hindcast'::text AS fuente_alerta
    FROM acta_de_chequeo av
    JOIN magnitud v ON v.id = av.id_variable
    WHERE av.id_estacion = $1
      AND av.cobertura_catalogo
      AND av.clase IN ('TP', 'FP')
      AND COALESCE(av.origen, 'live') = 'hindcast'
      AND ", lead_sql, "
    ORDER BY av.fecha_objetivo DESC, av.lead_days ASC NULLS LAST, v.nombre_abr, av.direccion
    LIMIT $2
  "), params = list(id_est, lim)), error = function(e) data.frame())

  resumen_hc_db <- tryCatch(db_query(paste0("
    SELECT
      COUNT(*) FILTER (WHERE clase = 'TP')::int AS tp,
      COUNT(*) FILTER (WHERE clase = 'FP')::int AS fp,
      COUNT(*) FILTER (WHERE clase = 'FN' AND ranking_pronostico IS NOT NULL)::int AS fn,
      COUNT(*) FILTER (WHERE clase IN ('TP','FP'))::int AS n_alertas,
      ROUND(COUNT(*) FILTER (WHERE clase = 'TP')::numeric
            / NULLIF(COUNT(*) FILTER (WHERE clase IN ('TP','FP')), 0), 3) AS precision,
      MIN(fecha_objetivo) AS desde,
      MAX(fecha_objetivo) AS hasta
    FROM acta_de_chequeo
    WHERE id_estacion = $1 AND cobertura_catalogo
      AND clase IN ('TP','FP')
      AND COALESCE(origen, 'live') = 'hindcast'
      AND ", gsub("av\\.", "", lead_sql), "
  "), params = list(id_est)), error = function(e) data.frame())

  alertas_hindcast <- list()
  if (nrow(alertas_hindcast_df) > 0) {
    alertas_hindcast <- alertas_hindcast_df
  } else if (include_hc && exists("admin_bias_hindcast_alertas_estacion", mode = "function")) {
    alertas_hindcast <- tryCatch(
      admin_bias_hindcast_alertas_estacion(id_est, min(lim, 40L)),
      error = function(e) {
        message("[skill-drill] hindcast rows: ", conditionMessage(e))
        list()
      }
    )
  }

  bias_apply <- list(disponible = FALSE)
  if (admin_tabla_existe("sesgo_pegado")) {
    ba <- tryCatch(db_query("
      SELECT apply_alertas, delta_precision, prec_raw, prec_corr, n_pred,
             tp_raw, fp_raw, tp_corr, fp_corr, confiable_any, motivo, updated_at
      FROM sesgo_pegado WHERE id_estacion = $1 LIMIT 1
    ", params = list(id_est)), error = function(e) data.frame())
    if (nrow(ba)) {
      bias_apply <- c(list(disponible = TRUE), as.list(ba[1, , drop = FALSE]))
    } else {
      bias_apply <- list(disponible = TRUE, apply_alertas = NULL,
                         motivo = "Sin fila de gate (fallback confiable en refresh)")
    }
  }

  # Resumen principal = LIVE; desglose hindcast desde filas persistidas.
  rl <- if (nrow(resumen_live)) as.list(resumen_live[1, , drop = FALSE]) else list()
  rh <- if (nrow(resumen_hc_db)) as.list(resumen_hc_db[1, , drop = FALSE]) else list()
  resumen_alertas <- list(
    tp = rl$tp %||% 0L, fp = rl$fp %||% 0L, fn = rl$fn %||% 0L,
    precision = rl$precision, recall = rl$recall,
    n_alertas = rl$n_alertas %||% 0L,
    n_pred = rl$n_alertas %||% 0L,
    n_live = rl$n_alertas %||% 0L,
    n_hindcast = rh$n_alertas %||% 0L,
    desde = rl$desde, hasta = rl$hasta,
    fuente = "verificacion_live"
  )

  list(
    ok = TRUE,
    disponible = TRUE,
    lead_max = if (is.null(lm)) NULL else as.integer(lm),
    estacion = as.list(meta[1, , drop = FALSE]),
    resumen = resumen_alertas,
    resumen_live = rl,
    resumen_skill = list(
      tp = meta$tp[1], fp = meta$fp[1], fn = meta$fn[1],
      precision = meta$precision[1], n_pred = meta$n_pred[1],
      n_hindcast = meta$n_hindcast[1], n_live = meta$n_live[1],
      prec_hindcast = meta$prec_hindcast[1], prec_live = meta$prec_live[1],
      fuente = meta$fuente[1]
    ),
    resumen_hindcast = list(
      tp = rh$tp %||% (if (length(bias_apply$tp_corr)) bias_apply$tp_corr else NULL),
      fp = rh$fp %||% (if (length(bias_apply$fp_corr)) bias_apply$fp_corr else NULL),
      precision = rh$precision %||% (if (length(bias_apply$prec_corr)) bias_apply$prec_corr else NULL),
      n_pred = rh$n_alertas %||% (if (length(bias_apply$n_pred)) bias_apply$n_pred else NULL),
      n_alertas = rh$n_alertas %||% 0L,
      desde = rh$desde, hasta = rh$hasta,
      fuente = "verificacion_hindcast"
    ),
    alertas = admin_df_rows(alertas_live),
    alertas_hindcast = if (is.list(alertas_hindcast) && !is.data.frame(alertas_hindcast)) {
      alertas_hindcast
    } else {
      admin_df_rows(as.data.frame(alertas_hindcast))
    },
    bias_apply = bias_apply,
    nota = paste0(
      "Historial = alertas previstas verificadas (cobertura cat\u00e1logo",
      if (is.null(lm)) "" else paste0(", lead≤", lm), "). ",
      "Live = emitidas/snapshoteadas; Hindcast = recalc top-10 sobre OM hist\u00f3rico. ",
      "Clic en una fila para ver valor/ranking predicho vs observado."
    )
  )
}

admin_df_rows <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

admin_tabla_existe <- function(name) {
  tryCatch(
    nrow(db_query(
      "SELECT 1 AS ok FROM information_schema.tables WHERE table_name = $1 LIMIT 1",
      params = list(name)
    )) > 0,
    error = function(e) FALSE
  )
}

# % del nodo: solo fases abiertas. Una fase en "completo" no entra al numerador ni al denominador.
admin_bias_pct_pendiente <- function(fases) {
  if (!length(fases)) {
    return(list(pct = NULL, done = NULL, total = NULL, faltan = NULL))
  }
  pend <- Filter(function(f) !identical(as.character(f$estado %||% ""), "completo"), fases)
  if (!length(pend)) {
    return(list(pct = 100, done = 0, total = 0, faltan = 0))
  }
  num <- function(f, field) {
    x <- suppressWarnings(as.numeric(f[[field]] %||% NA_real_)[1])
    if (length(x) != 1L || !is.finite(x) || x < 0) 0 else x
  }
  done <- sum(vapply(pend, function(f) num(f, "done"), numeric(1)))
  total <- sum(vapply(pend, function(f) num(f, "total"), numeric(1)))
  list(
    pct = if (total > 0) round(100 * done / total, 1) else 0,
    done = done,
    total = total,
    faltan = max(0, total - done)
  )
}

admin_bias_nodo_line <- function(fases, pend = NULL) {
  if (is.null(pend)) pend <- admin_bias_pct_pendiente(fases)
  if (is.null(pend$pct) || !is.finite(as.numeric(pend$pct)[1])) return(NULL)
  ntxt <- function(x) as.character(as.integer(round(as.numeric(x))))
  partes <- character()
  for (f in fases) {
    if (identical(as.character(f$grupo %||% ""), "modelos")) next
    if (identical(as.character(f$estado %||% ""), "en_curso")) {
      partes <- c(partes, paste0(f$label, " ", f$pct, "%"))
      break
    }
  }
  mods <- Filter(function(f) {
    identical(as.character(f$grupo %||% ""), "modelos") &&
      !identical(as.character(f$estado %||% ""), "completo")
  }, fases)
  if (length(mods)) {
    bits <- vapply(mods, function(f) paste0(f$label, " ", ntxt(f$done), "/", ntxt(f$total)), character(1))
    done_m <- sum(vapply(mods, function(f) as.numeric(f$done %||% 0), numeric(1)))
    tot_m <- sum(vapply(mods, function(f) as.numeric(f$total %||% 0), numeric(1)))
    pct_m <- if (tot_m > 0) round(100 * done_m / tot_m, 1) else 0
    partes <- c(partes, paste0(
      "Modelos ", pct_m, "% · faltan ", ntxt(tot_m - done_m), " · ",
      paste(bits, collapse = " · ")
    ))
  }
  if (!length(partes)) {
    if (identical(as.numeric(pend$pct), 100)) return("archivo completo")
    return(paste0(pend$pct, "%"))
  }
  paste(partes, collapse = " | ")
}

# Hindcast por modelo (openmeteo_modelos_hindcast.R): ECMWF, GFS, ICON.
# Misma grilla que modelos_hc_tareas: coord excelente >= 10 anios x ventana de 80 dias.
admin_bias_modelos_fases <- function() {
  if (!admin_tabla_existe("hito_del_hindcast")) return(list())
  desde <- suppressWarnings(as.Date(Sys.getenv("MODELOS_HC_DESDE", "2024-02-01")))
  hasta <- suppressWarnings(as.Date(Sys.getenv("MODELOS_HC_HASTA", "2026-09-30")))
  if (length(desde) != 1L || is.na(desde)) desde <- as.Date("2024-02-01")
  if (length(hasta) != 1L || is.na(hasta) || hasta < desde) hasta <- as.Date("2026-09-30")
  labels <- c(ecmwf_ifs025 = "ECMWF", gfs_seamless = "GFS", icon_seamless = "ICON")
  df <- tryCatch(db_query("
    WITH coords AS (
      SELECT DISTINCT ROUND(e.lat::numeric, 3) AS lat_r,
                      ROUND(e.lon::numeric, 3) AS lon_r
      FROM mirador e
      JOIN estacion_calidad ec ON ec.id_estacion = e.id
      WHERE ec.nivel = 'excelente'
        AND COALESCE(ec.anios_span, 0) >= 10
        AND e.lat IS NOT NULL AND e.lon IS NOT NULL
        AND e.lat BETWEEN -56 AND -17 AND e.lon BETWEEN -76 AND -66
    ),
    wins AS (
      SELECT g::date AS desde,
             LEAST((g + INTERVAL '79 days')::date, $2::date) AS hasta
      FROM generate_series($1::date, $2::date, INTERVAL '80 days') g
    ),
    grid AS (
      SELECT c.lat_r, c.lon_r, m.modelo, w.desde, w.hasta
      FROM coords c
      CROSS JOIN (VALUES ('ecmwf_ifs025'), ('gfs_seamless'), ('icon_seamless')) AS m(modelo)
      CROSS JOIN wins w
    )
    SELECT g.modelo, COUNT(*)::int AS total, COUNT(k.modelo)::int AS done
    FROM grid g
    LEFT JOIN hito_del_hindcast k
      ON k.lat_round = g.lat_r AND k.lon_round = g.lon_r
     AND k.modelo = g.modelo AND k.desde = g.desde AND k.hasta = g.hasta
    GROUP BY g.modelo
  ", params = list(as.character(desde), as.character(hasta))), error = function(e) data.frame())
  if (is.null(df) || !nrow(df)) return(list())
  out <- list()
  for (id in names(labels)) {
    hit <- df[df$modelo == id, , drop = FALSE]
    if (!nrow(hit)) next
    total <- as.numeric(hit$total[1])
    done <- as.numeric(hit$done[1])
    if (!is.finite(total) || total < 0) total <- 0
    if (!is.finite(done) || done < 0) done <- 0
    if (total > 0 && done > total) done <- total
    pct <- if (total > 0) round(100 * done / total, 1) else 100
    estado <- if (total > 0 && done >= total) "completo" else if (done > 0) "en_curso" else "pendiente"
    out[[length(out) + 1L]] <- list(
      id = paste0("modelo_", id),
      label = unname(labels[[id]]),
      grupo = "modelos",
      estado = estado,
      pct = as.numeric(pct),
      done = done,
      total = total
    )
  }
  out
}

admin_bias_vista_nodo <- function(fases) {
  pend <- admin_bias_pct_pendiente(fases)
  cur <- NULL
  for (f in fases) {
    if (identical(as.character(f$estado %||% ""), "en_curso") &&
        !identical(as.character(f$grupo %||% ""), "modelos")) {
      cur <- f
      break
    }
  }
  mods_open <- Filter(function(f) {
    identical(as.character(f$grupo %||% ""), "modelos") &&
      !identical(as.character(f$estado %||% ""), "completo")
  }, fases)
  fase <- NULL
  fase_label <- NULL
  fase_idx <- NULL
  fase_total <- if (length(fases)) length(fases) else NULL
  if (length(mods_open) && is.null(cur)) {
    n_cerradas <- sum(vapply(fases, function(f) {
      identical(as.character(f$estado %||% ""), "completo")
    }, logical(1)))
    fase <- "modelos"
    fase_label <- "Hindcast por modelo"
    fase_idx <- n_cerradas + 1L
  } else if (!is.null(cur)) {
    fase <- cur$id
    fase_label <- cur$label
    fase_idx <- cur$idx
  } else {
    last <- NULL
    for (f in fases) {
      if (identical(as.character(f$estado %||% ""), "completo")) last <- f
    }
    if (!is.null(last)) {
      fase <- last$id
      fase_label <- last$label
      fase_idx <- last$idx
    }
  }
  list(
    pct = pend$pct,
    done = if (!is.null(pend$total) && pend$total > 0) pend$done else NULL,
    total = if (!is.null(pend$total) && pend$total > 0) pend$total else NULL,
    faltan = pend$faltan,
    fase = fase,
    fase_label = fase_label,
    fase_idx = fase_idx,
    fase_total = fase_total,
    line = admin_bias_nodo_line(fases, pend)
  )
}

# Checklist vivo de fases del backfill OM (desde checkpoint; no depende de la ultima corrida).
admin_bias_historico_fases <- function() {
  cur_year <- as.integer(format(Sys.Date(), "%Y"))
  hist_start <- suppressWarnings(as.integer(format(
    as.Date(Sys.getenv("BIAS_HISTORICAL_START", "2021-01-01")), "%Y"
  )))
  if (is.na(hist_start)) hist_start <- 2021L
  lead_start <- suppressWarnings(as.integer(format(
    as.Date(Sys.getenv("BIAS_LEAD_START", "2024-01-01")), "%Y"
  )))
  if (is.na(lead_start)) lead_start <- 2024L
  alert_min <- suppressWarnings(as.integer(Sys.getenv("BIAS_ALERT_MIN_ANIOS", "20")))
  if (is.na(alert_min) || alert_min < 0L) alert_min <- 20L
  # En el contenedor api el env puede no haberse recreado; default a leads 1-3.
  leads_raw <- trimws(Sys.getenv("BIAS_HIST_LEADS", "0,1,2,3"))
  if (!nzchar(leads_raw)) leads_raw <- "0,1,2,3"
  leads_all <- unique(suppressWarnings(as.integer(unlist(strsplit(leads_raw, "[,;[:space:]]+")))))
  leads_all <- leads_all[is.finite(leads_all)]
  leads_pos <- sort(leads_all[leads_all >= 1L & leads_all <= 7L])

  n_coords <- function(scope) {
    q <- if (identical(scope, "alertas")) {
      paste0(
        "SELECT COUNT(*)::int AS n FROM (
           SELECT DISTINCT ROUND(e.lat::numeric, 3), ROUND(e.lon::numeric, 3)
           FROM mirador e
           INNER JOIN estacion_calidad ec ON ec.id_estacion = e.id
           WHERE ec.nivel = 'excelente'
             AND COALESCE(ec.anios_span, 0) >= ", alert_min, "
             AND e.lat IS NOT NULL AND e.lon IS NOT NULL
             AND e.lat BETWEEN -56 AND -17 AND e.lon BETWEEN -76 AND -66
         ) t"
      )
    } else {
      "SELECT COUNT(*)::int AS n FROM (
         SELECT DISTINCT ROUND(e.lat::numeric, 3), ROUND(e.lon::numeric, 3)
         FROM mirador e
         INNER JOIN estacion_calidad ec ON ec.id_estacion = e.id
         WHERE ec.nivel IN ('excelente', 'buena')
           AND e.lat IS NOT NULL AND e.lon IS NOT NULL
           AND e.lat BETWEEN -56 AND -17 AND e.lon BETWEEN -76 AND -66
       ) t"
    }
    suppressWarnings(as.integer(db_query(q)$n[1] %||% 0L))
  }

  scope_filter_sql <- function(scope) {
    if (identical(scope, "alertas")) {
      paste0(
        " AND (lat_round, lon_round) IN (
           SELECT DISTINCT ROUND(e.lat::numeric, 3), ROUND(e.lon::numeric, 3)
           FROM mirador e
           INNER JOIN estacion_calidad ec ON ec.id_estacion = e.id
           WHERE ec.nivel = 'excelente'
             AND COALESCE(ec.anios_span, 0) >= ", alert_min, "
             AND e.lat IS NOT NULL AND e.lon IS NOT NULL
         )"
      )
    } else {
      ""
    }
  }

  ckpt_done <- function(y0, y1, leads, scope) {
    leads <- as.integer(leads)
    if (length(leads) == 0L) return(0L)
    if (as.integer(y1) < as.integer(y0)) return(0L)
    leads_sql <- paste(leads, collapse = ",")
    q <- paste0(
      "SELECT COUNT(*)::int AS n FROM hito_del_archivo
       WHERE year_from >= ", as.integer(y0), "
         AND year_to <= ", as.integer(y1), "
         AND lead_days IN (", leads_sql, ")",
      scope_filter_sql(scope)
    )
    suppressWarnings(as.integer(db_query(q)$n[1] %||% 0L))
  }

  # Año en curso: solo ckpt del mes (igual que refresh_current_year en worker).
  ckpt_done_month <- function(year, leads, scope) {
    leads <- as.integer(leads)
    if (length(leads) == 0L) return(0L)
    leads_sql <- paste(leads, collapse = ",")
    q <- paste0(
      "SELECT COUNT(*)::int AS n FROM hito_del_archivo
       WHERE year_from = ", as.integer(year), "
         AND year_to = ", as.integer(year), "
         AND lead_days IN (", leads_sql, ")
         AND date_trunc('month', done_at AT TIME ZONE 'America/Santiago')
             = date_trunc('month', NOW() AT TIME ZONE 'America/Santiago')",
      scope_filter_sql(scope)
    )
    suppressWarnings(as.integer(db_query(q)$n[1] %||% 0L))
  }

  # done = históricos cerrados + año en curso fresco este mes.
  ckpt_done_with_current <- function(y0, y1, leads, scope, refresh_cur = FALSE) {
    y0 <- as.integer(y0)
    y1 <- as.integer(y1)
    if (!isTRUE(refresh_cur) || y1 < cur_year) {
      return(ckpt_done(y0, y1, leads, scope))
    }
    done_hist <- if (y0 <= cur_year - 1L) {
      ckpt_done(y0, min(y1, cur_year - 1L), leads, scope)
    } else {
      0L
    }
    done_cur <- ckpt_done_month(cur_year, leads, scope)
    as.integer((done_hist %||% 0L) + (done_cur %||% 0L))
  }

  n_todas <- n_coords("todas")
  n_alertas <- n_coords("alertas")
  # Lead 0 incluye año en curso (mantención mensual) en denom + done del mes.
  defs <- list(
    list(
      id = "lead0_historico",
      label = sprintf("Lead 0 - hist\u00f3rico %d-%d", hist_start, cur_year),
      y0 = hist_start, y1 = cur_year, leads = 0L, scope = "todas", n = n_todas,
      dia_scope = "todas", refresh_cur = FALSE
    )
  )
  if (length(leads_pos) > 0L) {
    defs[[length(defs) + 1L]] <- list(
      id = "leads_alertas",
      label = sprintf("Leads 1-3 - estaciones de alertas (%d+)", lead_start),
      y0 = lead_start, y1 = cur_year, leads = leads_pos, scope = "alertas", n = n_alertas,
      dia_scope = "alertas", refresh_cur = FALSE
    )
    defs[[length(defs) + 1L]] <- list(
      id = "leads_todas",
      label = sprintf("Leads 1-3 - resto de estaciones (%d+)", lead_start),
      y0 = lead_start, y1 = cur_year, leads = leads_pos, scope = "todas", n = n_todas,
      dia_scope = "resto", refresh_cur = FALSE
    )
    if (lead_start > hist_start) {
      defs[[length(defs) + 1L]] <- list(
        id = "leads_temp_hist",
        label = sprintf("Leads 1-3 - solo temperatura %d-%d", hist_start, lead_start - 1L),
        y0 = hist_start, y1 = lead_start - 1L, leads = leads_pos, scope = "todas", n = n_todas,
        dia_scope = "todas", refresh_cur = FALSE
      )
    }
  }

  n_stations <- function(scope) {
    where <- if (identical(scope, "alertas")) {
      paste0(
        "ec.nivel = 'excelente' AND COALESCE(ec.anios_span, 0) >= ", alert_min
      )
    } else if (identical(scope, "resto")) {
      paste0(
        "ec.nivel IN ('excelente', 'buena') AND NOT (ec.nivel = 'excelente' ",
        "AND COALESCE(ec.anios_span, 0) >= ", alert_min, ")"
      )
    } else {
      "ec.nivel IN ('excelente', 'buena')"
    }
    q <- paste0(
      "SELECT COUNT(*)::int AS n FROM mirador e
       INNER JOIN estacion_calidad ec ON ec.id_estacion = e.id
       WHERE ", where, "
         AND e.lat IS NOT NULL AND e.lon IS NOT NULL
         AND e.lat BETWEEN -56 AND -17 AND e.lon BETWEEN -76 AND -66"
    )
    suppressWarnings(as.integer(db_query(q)$n[1] %||% 0L))
  }
  n_st <- list(
    todas = n_stations("todas"),
    alertas = n_stations("alertas"),
    resto = n_stations("resto")
  )

  # Dias con dato (t_max) ya agregados. Si la tabla no existe, el % cae al
  # checkpoint sin vencer el mes: nunca otra vez 100 -> 66 por cambiar de mes.
  dias_sum <- function(desde, hasta, leads, scope) {
    if (!admin_tabla_existe("cobertura_del_archivo")) return(NA_real_)
    col <- switch(scope, alertas = "n_alertas", resto = "n_resto", "n_todas")
    leads_sql <- paste(as.integer(leads), collapse = ",")
    q <- paste0(
      "SELECT COALESCE(SUM(", col, "), 0)::bigint AS n
       FROM cobertura_del_archivo
       WHERE lead_days IN (", leads_sql, ")
         AND fecha >= '", desde, "' AND fecha <= '", hasta, "'"
    )
    suppressWarnings(as.numeric(db_query(q)$n[1]))
  }

  seen_current <- FALSE
  out <- vector("list", length(defs))
  for (i in seq_along(defs)) {
    d <- defs[[i]]
    n_years <- max(0L, as.integer(d$y1) - as.integer(d$y0) + 1L)
    n_leads <- max(1L, length(as.integer(d$leads)))
    total_ckpt <- as.numeric(d$n) * n_years * n_leads
    done_ckpt <- if (total_ckpt > 0) {
      ckpt_done(d$y0, d$y1, d$leads, d$scope)
    } else {
      0
    }
    if (is.na(done_ckpt)) done_ckpt <- 0
    desde <- as.Date(sprintf("%d-01-01", as.integer(d$y0)))
    hasta <- as.Date(sprintf("%d-12-31", as.integer(d$y1)))
    ayer <- Sys.Date() - 1L
    if (hasta > ayer) hasta <- ayer
    n_days <- if (hasta < desde) 0L else as.integer(hasta - desde) + 1L
    dia_scope <- if (!is.null(d$dia_scope)) d$dia_scope else "todas"
    esperados <- as.numeric(n_st[[dia_scope]] %||% 0L) * n_days * n_leads
    dias <- if (n_days > 0L && esperados > 0) {
      dias_sum(desde, hasta, d$leads, dia_scope)
    } else {
      0
    }
    complete <- total_ckpt > 0 && done_ckpt >= total_ckpt
    pct <- if (is.finite(dias) && esperados > 0) {
      round(100 * dias / esperados, 1)
    } else if (total_ckpt > 0) {
      round(100 * done_ckpt / total_ckpt, 1)
    } else {
      100
    }
    if (isTRUE(complete)) {
      estado <- "completo"
    } else if (!seen_current) {
      seen_current <- TRUE
      estado <- "en_curso"
    } else {
      estado <- "pendiente"
    }
    shown_done <- if (is.finite(dias)) dias else done_ckpt
    shown_total <- if (is.finite(dias) && esperados > 0) esperados else total_ckpt
    out[[i]] <- list(
      id = as.character(d$id),
      label = as.character(d$label),
      idx = as.integer(i),
      estado = estado,
      pct = as.numeric(pct),
      done = as.numeric(shown_done),
      total = as.numeric(shown_total)
    )
  }
  mods <- tryCatch(admin_bias_modelos_fases(), error = function(e) list())
  if (length(mods)) {
    for (m in mods) {
      m$idx <- length(out) + 1L
      out[[length(out) + 1L]] <- m
    }
  }
  out
}

admin_alertas_resumen <- function() {
  if (!admin_tabla_existe("acta_de_chequeo")) {
    return(list(disponible = FALSE, mensaje = "Faltan las tablas foto_del_aviso y acta_de_chequeo"))
  }
  resumen <- db_query("
    SELECT
      COUNT(*) FILTER (WHERE clase = 'TP') AS tp,
      COUNT(*) FILTER (WHERE clase = 'FP') AS fp,
      COUNT(*) FILTER (WHERE clase = 'FN' AND ranking_pronostico IS NOT NULL) AS fn,
      MIN(fecha_objetivo) AS desde,
      MAX(fecha_objetivo) AS hasta
    FROM acta_de_chequeo
    WHERE cobertura_catalogo
      AND (clase IN ('TP','FP') OR (clase = 'FN' AND ranking_pronostico IS NOT NULL))")
  tp <- as.integer(resumen$tp[1] %||% 0L)
  fp <- as.integer(resumen$fp[1] %||% 0L)
  fn <- as.integer(resumen$fn[1] %||% 0L)
  list(
    disponible = TRUE,
    tp = tp, fp = fp, fn = fn,
    precision_top10 = if ((tp + fp) > 0) round(tp / (tp + fp), 3) else NULL,
    recall_top10 = if ((tp + fn) > 0) round(tp / (tp + fn), 3) else NULL,
    desde = if (is.na(resumen$desde[1])) NULL else as.character(resumen$desde[1]),
    hasta = if (is.na(resumen$hasta[1])) NULL else as.character(resumen$hasta[1])
  )
}

admin_pronostico_resumen <- function() {
  if (!admin_tabla_existe("correccion_de_sesgo")) {
    return(list(disponible = FALSE, mensaje = "Tabla correccion_de_sesgo no existe"))
  }
  g <- db_query("
    SELECT
      COUNT(DISTINCT id_estacion) AS n_estaciones,
      COUNT(*) AS n_pares,
      SUM(CASE WHEN confiable THEN 1 ELSE 0 END) AS n_confiable,
      SUM(mae_antes * n_muestra) / NULLIF(SUM(n_muestra), 0) AS mae_antes,
      SUM(mae_despues * n_muestra) / NULLIF(SUM(n_muestra), 0) AS mae_despues
    FROM correccion_de_sesgo
    WHERE mes = 0 AND COALESCE(lead_bin, 0) = 0
      AND mae_antes IS NOT NULL AND mae_despues IS NOT NULL")
  if (nrow(g) == 0 || is.na(g$mae_antes[1])) {
    return(list(disponible = TRUE, n_estaciones = 0L, n_pares = 0L, n_confiable = 0L,
                mae_antes = NULL, mae_despues = NULL, mejora_pct = NULL))
  }
  mae_a <- as.numeric(g$mae_antes[1])
  mae_d <- as.numeric(g$mae_despues[1])
  mejora <- if (!is.na(mae_a) && mae_a > 0) round(100 * (mae_a - mae_d) / mae_a, 1) else NULL
  list(
    disponible = TRUE,
    n_estaciones = as.integer(g$n_estaciones[1]),
    n_pares = as.integer(g$n_pares[1]),
    n_confiable = as.integer(g$n_confiable[1]),
    mae_antes = round(mae_a, 3),
    mae_despues = round(mae_d, 3),
    mejora_pct = mejora
  )
}

admin_bias_apply_status <- function() {
  cfg_enabled <- tolower(Sys.getenv("OPENMETEO_BIAS_ENABLED", "0")) %in% c("1", "true", "yes", "si")
  emergency_env <- tolower(Sys.getenv("BIAS_APPLY_EMERGENCY_OFF", "0")) %in% c("1", "true", "yes", "si")
  emerg_file <- file.exists(file.path(
    Sys.getenv("PROJECT_ROOT", "."), "ingest", "logs", "bias_apply_emergency.off"
  ))
  # En contenedor api el root suele ser /app
  if (!emerg_file) {
    emerg_file <- file.exists("/app/ingest/logs/bias_apply_emergency.off")
  }
  emergency_off <- isTRUE(emergency_env) || isTRUE(emerg_file)
  enabled <- isTRUE(cfg_enabled) && !isTRUE(emergency_off)
  apply_abrs <- c("t_max", "t_min", "pp")
  pct_crudo <- NULL
  n_total <- NULL
  n_crudo <- NULL
  if (admin_tabla_existe("carta_del_mirador")) {
    st <- tryCatch(
      db_query("
        SELECT COUNT(*)::int AS n_total,
               COUNT(*) FILTER (WHERE valor_crudo IS NOT NULL)::int AS n_crudo
        FROM carta_del_mirador
        WHERE fecha_objetivo >= CURRENT_DATE"),
      error = function(e) data.frame()
    )
    if (nrow(st) > 0) {
      n_total <- as.integer(st$n_total[1])
      n_crudo <- as.integer(st$n_crudo[1])
      if (!is.na(n_total) && n_total > 0) {
        pct_crudo <- round(100 * n_crudo / n_total, 1)
      }
    }
  }

  loop <- list()
  n_apply_on <- NULL
  n_gate <- NULL
  delta_gate <- NULL
  if (admin_tabla_existe("sesgo_en_ronda")) {
    ls <- tryCatch(db_query("SELECT * FROM sesgo_en_ronda WHERE id = 1"), error = function(e) data.frame())
    if (nrow(ls)) {
      loop <- as.list(ls[1, , drop = FALSE])
      n_apply_on <- as.integer(ls$n_apply_on[1])
      n_gate <- as.integer(ls$n_estaciones_gate[1])
      delta_gate <- ls$delta_precision[1]
      if (isTRUE(ls$emergency_off[1])) emergency_off <- TRUE
    }
  }
  if (admin_tabla_existe("sesgo_pegado") && is.null(n_apply_on)) {
    n_apply_on <- tryCatch(
      as.integer(db_query("SELECT COUNT(*)::int AS n FROM sesgo_pegado WHERE apply_alertas")$n[1]),
      error = function(e) NULL
    )
  }

  list(
    enabled = enabled,
    enabled_flag = cfg_enabled,
    emergency_off = emergency_off,
    timing = "T+1",
    apply_abrs = apply_abrs,
    n_forecast_rows = n_total,
    n_with_valor_crudo = n_crudo,
    pct_valor_crudo = pct_crudo,
    n_apply_on = n_apply_on,
    n_estaciones_gate = n_gate,
    delta_precision_gate = if (length(delta_gate) && !is.na(delta_gate[1])) as.numeric(delta_gate[1]) else NULL,
    loop_status = loop,
    nota_loop = paste0(
      "T+1: refresh de hoy usa coefs/gates de ayer. ",
      "Apply por estacion si confiable + apply_alertas. ",
      "Kill-switch global: OPENMETEO_BIAS_ENABLED / BIAS_APPLY_EMERGENCY_OFF."
    )
  )
}

admin_bias_por_lead <- function() {
  if (!admin_tabla_existe("correccion_de_sesgo")) return(list())
  df <- tryCatch(
    db_query("
      SELECT COALESCE(b.lead_bin, 0)::int AS lead_bin,
        CASE COALESCE(b.lead_bin, 0)
          WHEN 0 THEN '0-1'
          WHEN 1 THEN '2-3'
          WHEN 2 THEN '4-7'
          ELSE '8-15'
        END AS lead_label,
        SUM(b.mae_antes * b.n_muestra) / NULLIF(SUM(b.n_muestra), 0) AS mae_antes,
        SUM(b.mae_despues * b.n_muestra) / NULLIF(SUM(b.n_muestra), 0) AS mae_despues,
        AVG(b.bias_antes) AS bias_medio,
        SUM(b.n_muestra)::int AS n_muestra,
        SUM(CASE WHEN b.confiable THEN 1 ELSE 0 END)::int AS n_confiable
      FROM correccion_de_sesgo b
      WHERE b.mes = 0 AND b.mae_antes IS NOT NULL AND b.confiable = TRUE
      GROUP BY COALESCE(b.lead_bin, 0)
      ORDER BY 1"),
    error = function(e) data.frame()
  )
  admin_df_rows(df)
}

admin_bias_monitor_rolling <- function() {
  # Compara valor (posible corregido) vs valor_crudo vs obs en ultimos 30 dias
  # solo donde hay valor_crudo y observacion.
  if (!admin_tabla_existe("carta_del_mirador") || !admin_tabla_existe("jornada_cerrada")) {
    return(NULL)
  }
  df <- tryCatch(
    db_query("
      SELECT v.nombre_abr,
        COUNT(*)::int AS n,
        AVG(ABS(p.valor_crudo - o.valor)) AS mae_raw,
        AVG(ABS(p.valor - o.valor)) AS mae_corr,
        AVG(p.valor_crudo - o.valor) AS bias_raw,
        AVG(p.valor - o.valor) AS bias_corr
      FROM carta_del_mirador p
      JOIN jornada_cerrada o
        ON o.id_estacion = p.id_estacion
       AND o.id_variable = p.id_variable
       AND o.fecha = p.fecha_objetivo
      JOIN magnitud v ON v.id = p.id_variable
      WHERE p.valor_crudo IS NOT NULL
        AND p.fecha_objetivo >= CURRENT_DATE - 30
        AND p.fecha_objetivo < CURRENT_DATE
        AND v.nombre_abr IN ('t_max','t_min','pp')
      GROUP BY v.nombre_abr
      ORDER BY v.nombre_abr"),
    error = function(e) data.frame()
  )
  if (nrow(df) == 0) return(NULL)
  rows <- lapply(seq_len(nrow(df)), function(i) {
    mae_r <- as.numeric(df$mae_raw[i])
    mae_c <- as.numeric(df$mae_corr[i])
    worse <- is.finite(mae_r) && is.finite(mae_c) && mae_c > mae_r
    list(
      nombre_abr = as.character(df$nombre_abr[i]),
      n = as.integer(df$n[i]),
      mae_raw = if (is.finite(mae_r)) round(mae_r, 3) else NULL,
      mae_corr = if (is.finite(mae_c)) round(mae_c, 3) else NULL,
      bias_raw = if (is.finite(df$bias_raw[i])) round(as.numeric(df$bias_raw[i]), 3) else NULL,
      bias_corr = if (is.finite(df$bias_corr[i])) round(as.numeric(df$bias_corr[i]), 3) else NULL,
      corrected_worse = worse
    )
  })
  list(ventana_dias = 30L, por_variable = rows)
}

admin_pipeline_stage_stats <- function() {
  empty <- function(state = "pendiente", line = NULL, ...) {
    out <- list(state = as.character(state)[1], line = if (is.null(line)) NULL else as.character(line)[1])
    extra <- list(...)
    nms <- names(extra)
    if (length(extra) && !is.null(nms)) {
      for (nm in nms) {
        if (!nzchar(nm)) next
        val <- extra[[nm]]
        if (is.null(val)) {
          out[[nm]] <- NULL
        } else if (is.atomic(val) && length(val) <= 1L) {
          out[[nm]] <- unname(val)[1]
        } else {
          out[[nm]] <- val
        }
      }
    }
    out
  }

  # Dia civil America/Santiago (no UTC del contenedor)
  hoy_df <- tryCatch(
    db_query("SELECT (CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date AS d"),
    error = function(e) data.frame()
  )
  hoy <- if (nrow(hoy_df) > 0L) as.Date(hoy_df$d[1]) else {
    as.Date(format(Sys.time(), tz = "America/Santiago"))
  }
  hoy_chr <- as.character(hoy)
  fmt_d <- function(d) {
    if (is.null(d) || length(d) < 1L || is.na(d[1])) return(NULL)
    format(as.Date(d[1]), "%d/%m")
  }
  no_hoy <- "sin corrida hoy"

  # Corrida Diario del dia (NUNCA mode=watchdog: pisaba pasos_detalle y dejaba nodos en pendiente)
  df <- tryCatch(db_query("
    SELECT id, run_id, started_at, finished_at, exit_code, skipped,
           pasos_detalle, errores, job_bat, metrics, task_name, mode, phase,
           estaciones_dga, estaciones_dmc, filas_afectadas_total,
           filas_afectadas_dga, filas_afectadas_dmc
    FROM parte_de_guardia
    WHERE task_name = 'Rodaja_Actualizar_Diario'
      AND (started_at AT TIME ZONE 'America/Santiago')::date = $1::date
      AND COALESCE(skipped, false) IS NOT TRUE
      AND COALESCE(mode, '') NOT IN ('skip', 'watchdog')
    ORDER BY started_at DESC
    LIMIT 1
  ", params = list(hoy_chr)), error = function(e) data.frame())

  ran_hoy <- nrow(df) > 0L
  pasos <- if (ran_hoy) {
    jsonb_as_list(df$pasos_detalle[[1]] %||% df$pasos_detalle[1])
  } else {
    list()
  }

  # Day health temprano: enriquecer pasos si PG incompleto / gapfill actualizo solo ckpt
  day_tag <- format(hoy, "%Y%m%d")
  day_health <- read_json_file_safe(file.path(
    ingest_logs_dir(), sprintf("pipeline_day_health_%s.json", day_tag)
  ))
  # Preferir day_health.pasos para enriquecer/completar; no pisar PG si ya hay el paso
  # (excepto: si health tiene el paso y PG no — típico tras fixes parciales).
  if (!is.null(day_health$pasos) && is.list(day_health$pasos)) {
    health_pasos <- list()
    nms <- names(day_health$pasos)
    if (!is.null(nms)) {
      for (nm in nms) {
        info <- day_health$pasos[[nm]]
        if (is.null(info)) next
        health_pasos[[length(health_pasos) + 1L]] <- list(
          id = nm,
          exit_code = info$exit_code %||% 0L,
          skipped = isTRUE(info$skipped),
          duration_sec = info$duration_sec %||% NULL,
          finished_at = info$finished_at %||% NULL,
          started_at = info$started_at %||% NULL
        )
      }
    }
    if (length(health_pasos)) {
      have <- vapply(pasos, function(p) as.character(p$id %||% ""), character(1))
      for (hp in health_pasos) {
        if (!(hp$id %in% have)) {
          pasos[[length(pasos) + 1L]] <- hp
        }
      }
      if (!ran_hoy && length(pasos) > 0L) ran_hoy <- TRUE
    }
  }

  step_meta <- function(id) {
    if (!length(pasos)) return(NULL)
    for (p in pasos) {
      if (identical(as.character(p$id %||% ""), id)) return(p)
    }
    NULL
  }
  step_state <- function(id) {
    if (!ran_hoy) return("pendiente")
    p <- step_meta(id)
    if (is.null(p)) return("pendiente")
    if (isTRUE(p$skipped)) return("omitido")
    ec <- suppressWarnings(as.integer(unlist(p$exit_code)[1] %||% 0L))
    if (length(ec) != 1L || is.na(ec[1])) ec <- 0L
    if (isTRUE(ec != 0L)) {
      # ORCH_LOG_FILE_LOCKED: orch marco exit=1 pero scrape metrics exit=0.
      # ponytail: isTRUE — metrics sin exit_code → integer(0)/NA no debe 500 el Hub.
      # Mid-run: ran_hoy puede venir de ckpt/day_health sin fila PG → [[1]] 500
      if (identical(id, "ingest") && nrow(df) > 0L) {
        m <- jsonb_as_list(df$metrics[[1]] %||% df$metrics[1])
        mec <- suppressWarnings(as.integer(unlist(m$exit_code)[1]))
        if (length(mec) == 1L && isTRUE(mec == 0L)) return("ok")
      }
      return("error")
    }
    "ok"
  }
  step_dur <- function(id) {
    p <- step_meta(id)
    if (is.null(p)) return(NULL)
    d <- suppressWarnings(as.numeric(p$duration_sec %||% NA_real_))
    if (is.na(d)) return(NULL)
    if (d < 60) paste0(round(d, 1), "s")
    else paste0(round(d / 60, 1), " min")
  }
  # Hora de fin del paso (ISO); Hub muestra HH:mm bajo el nodo
  step_finished_at <- function(id) {
    p <- step_meta(id)
    if (is.null(p)) return(NULL)
    at <- as.character(p$finished_at %||% p$started_at %||% "")[1]
    if (!nzchar(at %||% "")) return(NULL)
    at
  }
  step_started_at <- function(id) {
    p <- step_meta(id)
    if (is.null(p)) return(NULL)
    at <- as.character(p$started_at %||% "")[1]
    if (!nzchar(at %||% "")) return(NULL)
    at
  }
  step_exit_code <- function(id) {
    p <- step_meta(id)
    if (is.null(p)) return(NULL)
    ec <- suppressWarnings(as.integer(unlist(p$exit_code)[1]))
    if (length(ec) != 1L || is.na(ec[1])) return(NULL)
    as.integer(ec[1])
  }
  step_duration_sec <- function(id) {
    p <- step_meta(id)
    if (is.null(p)) return(NULL)
    d <- suppressWarnings(as.numeric(unlist(p$duration_sec)[1]))
    if (length(d) != 1L || is.na(d[1])) return(NULL)
    as.numeric(d[1])
  }
  # Errores del report filtrados por paso (códigos cortos para tooltip admin)
  step_error_codes <- function(id) {
    out <- character()
    comps <- err_rep$compact %||% list()
    if (!length(comps)) return(out)
    for (e in comps) {
      st <- as.character(e$step %||% "")
      code <- as.character(e$code %||% "")
      if (!nzchar(code)) next
      match_step <- identical(st, id) ||
        (identical(id, "vigilancia") && st %in% c("watchdog", "vigilancia", "gapfill")) ||
        (identical(id, "ingest") && (identical(st, "ingest") || grepl("^INGEST_", code)))
      if (!match_step) next
      n <- suppressWarnings(as.integer(e$count %||% 1L))[1]
      bit <- code
      if (!is.na(n) && n > 1L) bit <- sprintf("%s×%d", bit, n)
      fu <- as.character(e$fuente %||% "")
      if (nzchar(fu)) bit <- sprintf("%s (%s)", bit, fu)
      out <- c(out, bit)
      if (length(out) >= 6L) break
    }
    out
  }
  step_done <- function(id) identical(step_state(id), "ok")

  # --- ingest: solo metricas de la corrida de hoy ---
  est_dga <- NULL
  est_dmc <- NULL
  filas <- NULL
  m <- list()
  # Solo fila PG; ran_hoy≠nrow(df) cuando mid-run enriquece desde ckpt
  if (nrow(df) > 0L) {
    est_dga <- pg_scalar_int(df$estaciones_dga[1])
    est_dmc <- pg_scalar_int(df$estaciones_dmc[1])
    filas <- pg_scalar_int(df$filas_afectadas_total[1])
    m <- jsonb_as_list(df$metrics[[1]] %||% df$metrics[1])
    if ((is.null(est_dga) || est_dga == 0L) && !is.null(m$estaciones_dga)) {
      est_dga <- suppressWarnings(as.integer(unlist(m$estaciones_dga)[1]))
    }
    if ((is.null(est_dmc) || est_dmc == 0L) && !is.null(m$estaciones_dmc)) {
      est_dmc <- suppressWarnings(as.integer(unlist(m$estaciones_dmc)[1]))
    }
    if ((is.null(filas) || filas == 0L) && !is.null(m$filas_afectadas_total)) {
      filas <- suppressWarnings(as.integer(unlist(m$filas_afectadas_total)[1]))
    }
  }

  # Day health / error report: incompletas + timeouts → parcial (amarillo)
  # (day_health ya leído arriba para enriquecer pasos)
  # err_rep se lee abajo; step_error_codes lo usa — orden: leer err_rep ANTES de step_error_codes calls
  err_rep <- admin_pipeline_error_report(day_tag)
  if (is.null(day_health)) {
    day_health <- read_json_file_safe(file.path(
      ingest_logs_dir(), sprintf("pipeline_day_health_%s.json", day_tag)
    ))
  }
  incomplete_vars <- character(0)
  if (!is.null(day_health$ingest$incomplete_vars)) {
    incomplete_vars <- as.character(unlist(day_health$ingest$incomplete_vars))
  } else if (!is.null(err_rep$incomplete_vars)) {
    incomplete_vars <- as.character(unlist(err_rep$incomplete_vars))
  }
  incomplete_vars <- incomplete_vars[nzchar(incomplete_vars)]
  completed_vars <- character(0)
  if (!is.null(day_health$ingest$completed_vars)) {
    completed_vars <- as.character(unlist(day_health$ingest$completed_vars))
  }
  completed_vars <- completed_vars[nzchar(completed_vars)]

  # Preferir checkpoint (gapfill) sobre day_health/error_report congelados
  ckpt <- read_ckpt_kv(file.path(ingest_logs_dir(), "actualizar_diario.checkpoint"))
  # Mid-run: Register-Actualizacion es al finally → PG sin fila de hoy. Enriquecer pasos
  # desde pipeline_done del ckpt del día para que la tira no quede en pendiente/0%.
  raw_done <- as.character(ckpt[["pipeline_done"]] %||% "")
  if (nzchar(raw_done)) {
    ckpt_day_ok <- FALSE
    ff <- as.character(ckpt[["fecha_fin"]] %||% "")
    if (nzchar(ff)) {
      ckpt_day_ok <- identical(substr(ff, 1L, 10L), hoy_chr)
    } else {
      lp <- as.character(ckpt[["last_progress_at"]] %||% "")
      ckpt_day_ok <- nzchar(lp) && (
        grepl(hoy_chr, lp, fixed = TRUE) ||
          grepl(format(hoy, "%Y-%m-%d"), lp, fixed = TRUE)
      )
    }
    if (isTRUE(ckpt_day_ok)) {
      done_ids <- trimws(strsplit(raw_done, ",", fixed = TRUE)[[1]])
      done_ids <- done_ids[nzchar(done_ids)]
      have <- if (length(pasos)) {
        vapply(pasos, function(p) as.character(p$id %||% ""), character(1))
      } else {
        character(0)
      }
      for (id in done_ids) {
        if (!(id %in% have)) {
          pasos[[length(pasos) + 1L]] <- list(
            id = id,
            exit_code = 0L,
            skipped = FALSE
          )
          have <- c(have, id)
        }
      }
      if (!ran_hoy && length(pasos) > 0L) ran_hoy <- TRUE
    }
  }
  ckpt_completed <- character(0)
  raw_cv <- as.character(ckpt[["completed_vars"]] %||% "")
  if (nzchar(raw_cv)) {
    ckpt_completed <- trimws(strsplit(raw_cv, ",", fixed = TRUE)[[1]])
    ckpt_completed <- ckpt_completed[nzchar(ckpt_completed)]
  }
  if (length(ckpt_completed) > length(completed_vars)) {
    completed_vars <- ckpt_completed
  }
  expected_vars_pre <- character(0)
  if (!is.null(day_health$ingest$expected_vars)) {
    expected_vars_pre <- as.character(unlist(day_health$ingest$expected_vars))
  }
  expected_vars_pre <- expected_vars_pre[nzchar(expected_vars_pre)]
  if (!length(expected_vars_pre)) {
    expected_vars_pre <- ADMIN_EXPECTED_INGEST_VARS
  }
  if (length(ckpt_completed) > 0L) {
    incomplete_vars <- setdiff(expected_vars_pre, completed_vars)
  }
  gapfill_completed <- isTRUE(day_health$ingest$gapfill_completed) || (
    length(incomplete_vars) == 0L &&
      !is.null(day_health$watchdog_ran_at) &&
      length(completed_vars) >= length(expected_vars_pre)
  )

  ingest_ec <- 0L
  p_ing <- step_meta("ingest")
  if (!is.null(p_ing)) {
    ingest_ec <- suppressWarnings(as.integer(unlist(p_ing$exit_code)[1] %||% 0L))
    if (length(ingest_ec) != 1L || is.na(ingest_ec[1])) ingest_ec <- 0L
  }
  # Tras gapfill completo, no pintar error por exit=124 histórico del scrape
  if (isTRUE(gapfill_completed) && length(incomplete_vars) == 0L) {
    ingest_ec <- 0L
  }
  has_progress <- (
    (!is.null(est_dga) && isTRUE(est_dga > 0L)) ||
      (!is.null(est_dmc) && isTRUE(est_dmc > 0L)) ||
      length(completed_vars) > 0L
  )
  ingest_cls <- if (ran_hoy) {
    classify_ingest_hub(ingest_ec, has_progress, incomplete_vars, err_rep)
  } else {
    list(state = "pendiente", line = NULL, n_pares = 0L, incomplete_vars = character(0))
  }
  if (isTRUE(gapfill_completed) && length(incomplete_vars) == 0L) {
    ingest_cls <- list(
      state = "ok", line = "completado por gapfill",
      n_pares = 0L, incomplete_vars = character(0)
    )
  }

  ingest_line <- no_hoy
  if (ran_hoy) {
    if (isTRUE(gapfill_completed) && identical(ingest_cls$state, "ok")) {
      ingest_line <- "completado por gapfill"
    } else if (identical(ingest_cls$state, "parcial") && !is.null(ingest_cls$line)) {
      ingest_line <- ingest_cls$line
    } else if ((!is.null(est_dga) && est_dga > 0L) || (!is.null(est_dmc) && est_dmc > 0L)) {
      parts <- character()
      if (!is.null(est_dga) && est_dga > 0L) parts <- c(parts, paste0(est_dga, " DGA"))
      if (!is.null(est_dmc) && est_dmc > 0L) parts <- c(parts, paste0(est_dmc, " DMC"))
      if (!is.null(filas) && filas > 0L) parts <- c(parts, paste0(filas, " filas"))
      ingest_line <- paste(parts, collapse = " | ")
    } else if (!is.null(filas) && filas > 0L) {
      ingest_line <- paste0(filas, " filas")
    } else if (identical(ingest_cls$state, "error")) {
      ingest_line <- step_dur("ingest") %||% "fallo"
    } else if (identical(ingest_cls$state, "ok")) {
      ingest_line <- step_dur("ingest") %||% "ok"
    } else {
      ingest_line <- no_hoy
    }
  }
  # Caption de errores duros solo si NO es parcial-timeout ni gapfill ya cerrado
  if (!identical(ingest_cls$state, "parcial") && !isTRUE(gapfill_completed)) {
    err_cap <- format_error_report_caption(err_rep)
    if (!is.null(err_cap) && nzchar(err_cap)) {
      if (identical(ingest_line, no_hoy) || !nzchar(ingest_line)) {
        ingest_line <- err_cap
      } else {
        ingest_line <- paste(ingest_line, err_cap, sep = " · ")
      }
    }
  }
  ingest_state <- if (!ran_hoy) "pendiente" else ingest_cls$state

  response_ms <- if (ran_hoy) extract_response_ms(m, day_health) else NULL
  resp_line <- format_response_ms_hub(response_ms)
  if (!is.null(resp_line) && nzchar(resp_line)) {
    if (identical(ingest_line, no_hoy) || !nzchar(ingest_line %||% "")) {
      ingest_line <- resp_line
    } else {
      ingest_line <- paste(ingest_line, resp_line, sep = " · ")
    }
  }

  # --- alertas tardias solo enviadas hoy ---
  n_tardias <- NULL
  if (ran_hoy && admin_tabla_existe("campanada")) {
    n_tardias <- tryCatch({
      as.integer(db_query("
        SELECT COUNT(*)::int AS n FROM campanada
        WHERE tipo = 'tardia'
          AND (enviado_en AT TIME ZONE 'America/Santiago')::date = $1::date
      ", params = list(hoy_chr))$n[1])
    }, error = function(e) NULL)
  }
  tardia_line <- if (!ran_hoy) {
    no_hoy
  } else if (step_done("alertas_top10") || (!is.null(n_tardias) && n_tardias > 0L)) {
    paste0(n_tardias %||% 0L, " alertas")
  } else if (identical(step_state("alertas_top10"), "error")) {
    "fallo"
  } else {
    no_hoy
  }

  # --- pronostico OM: rango de fechas_objetivo descargadas HOY ---
  om_desde <- NULL
  om_hasta <- NULL
  om_n_est <- NULL
  if (admin_tabla_existe("carta_del_mirador")) {
    pr <- tryCatch(db_query("
      SELECT MIN(fecha_objetivo) AS desde, MAX(fecha_objetivo) AS hasta,
             COUNT(*)::int AS n_obs,
             COUNT(DISTINCT id_estacion)::int AS n_est
      FROM carta_del_mirador
      WHERE (fetched_at AT TIME ZONE 'America/Santiago')::date = $1::date
    ", params = list(hoy_chr)), error = function(e) data.frame())
    if (nrow(pr) > 0L && !is.na(pr$desde[1]) && as.integer(pr$n_obs[1] %||% 0L) > 0L) {
      om_desde <- as.character(pr$desde[1])
      om_hasta <- as.character(pr$hasta[1])
      om_n_est <- as.integer(pr$n_est[1] %||% 0L)
    }
  }
  om_corridas <- data.frame()
  if (admin_tabla_existe("corrida_de_carta")) {
    om_corridas <- tryCatch(db_query("
      SELECT status, n_estaciones, n_chunks, n_upserts, error_msg, started_at, finished_at
      FROM corrida_de_carta
      WHERE (started_at AT TIME ZONE 'America/Santiago')::date = $1::date
      ORDER BY started_at DESC
      LIMIT 5
    ", params = list(hoy_chr)), error = function(e) data.frame())
  }
  om_hub <- pronostico_hub_from_corridas(om_corridas, n_est_hoy = om_n_est)
  om_line <- if (!is.null(om_desde) && !is.null(om_hasta)) {
    paste0(fmt_d(om_desde), "-", fmt_d(om_hasta))
  } else if (ran_hoy && identical(step_state("refresh_pronostico"), "ok")) {
    step_dur("refresh_pronostico") %||% "ok"
  } else if (ran_hoy && identical(step_state("refresh_pronostico"), "error")) {
    "fallo"
  } else {
    no_hoy
  }

  # --- alertas tempranas + correos solo de hoy ---
  n_tempranas <- NULL
  n_correos <- NULL
  n_eventos_mail <- NULL
  if (admin_tabla_existe("aviso_de_madrugada")) {
    n_tempranas <- tryCatch({
      as.integer(db_query("
        SELECT COUNT(*)::int AS n FROM aviso_de_madrugada
        WHERE (generado_en AT TIME ZONE 'America/Santiago')::date = $1::date
      ", params = list(hoy_chr))$n[1])
    }, error = function(e) NULL)
  }
  if (admin_tabla_existe("sobre_del_resumen")) {
    dig <- tryCatch(db_query("
      SELECT COUNT(*)::int AS n_correos, COALESCE(SUM(n_eventos),0)::int AS n_eventos
      FROM sobre_del_resumen
      WHERE fecha_eval = $1::date AND exito
    ", params = list(hoy_chr)), error = function(e) data.frame())
    if (nrow(dig) > 0L) {
      n_correos <- as.integer(dig$n_correos[1] %||% 0L)
      n_eventos_mail <- as.integer(dig$n_eventos[1] %||% 0L)
    }
  }
  tempr_line <- no_hoy
  if (ran_hoy && (step_done("alertas_pronostico") ||
                  (!is.null(n_tempranas) && n_tempranas > 0L) ||
                  (!is.null(n_correos) && n_correos > 0L))) {
    tempr_parts <- character()
    if (!is.null(n_tempranas)) tempr_parts <- c(tempr_parts, paste0(n_tempranas, " alertas"))
    if (!is.null(n_correos) && n_correos > 0L) {
      tempr_parts <- c(tempr_parts, paste0(n_correos, " correo", if (n_correos != 1L) "s" else ""))
    }
    tempr_line <- if (length(tempr_parts)) paste(tempr_parts, collapse = " | ") else (step_dur("alertas_pronostico") %||% "ok")
  } else if (ran_hoy && identical(step_state("alertas_pronostico"), "error")) {
    tempr_line <- "fallo"
  }

  # --- QC: solo si el paso corrio hoy; metricas del resumen filtradas no existen dia-a-dia ---
  qc_line <- no_hoy
  if (ran_hoy && step_done("verificar_concordancia")) {
    alert_res <- tryCatch(admin_alertas_resumen(), error = function(e) list(disponible = FALSE))
    if (isTRUE(alert_res$disponible)) {
      prec <- alert_res$precision_top10
      prec_txt <- if (!is.null(prec)) paste0("prec ", round(100 * as.numeric(prec), 0), "%") else NULL
      tp_txt <- paste0("TP", alert_res$tp %||% 0, "/FP", alert_res$fp %||% 0, "/FN", alert_res$fn %||% 0)
      qc_line <- paste(c(prec_txt, tp_txt), collapse = " | ")
    } else {
      qc_line <- step_dur("verificar_concordancia") %||% "ok"
    }
  } else if (ran_hoy && identical(step_state("verificar_concordancia"), "error")) {
    qc_line <- "fallo"
  }

  # --- historico OM: fases siempre (vivo desde checkpoint); resto si corrio hoy ---
  bias_pct <- NULL
  bias_rows <- NULL
  bias_line <- no_hoy
  bias_fase <- NULL
  bias_fase_label <- NULL
  bias_fase_idx <- NULL
  bias_fase_total <- NULL
  bias_fases <- tryCatch(admin_bias_historico_fases(), error = function(e) list())
  bias_mantencion <- NULL
  bias_vista <- NULL
  if (length(bias_fases) > 0L) {
    bias_vista <- admin_bias_vista_nodo(bias_fases)
    bias_fase <- bias_vista$fase
    bias_fase_label <- bias_vista$fase_label
    bias_fase_idx <- bias_vista$fase_idx
    bias_fase_total <- bias_vista$fase_total
    bias_pct <- bias_vista$pct
  }
  if (ran_hoy && (step_done("bias_historico") || identical(step_state("bias_historico"), "error") ||
                  !is.null(step_meta("bias_historico")))) {
    if (admin_tabla_existe("corrida_del_archivo")) {
      br <- tryCatch(db_query("
        SELECT chunks_done_after, chunks_total, rows_upserted, backfill_complete, started_at,
               fase, fase_label, fase_idx, fase_total, mantencion_al_dia
        FROM corrida_del_archivo
        WHERE (started_at AT TIME ZONE 'America/Santiago')::date = $1::date
        ORDER BY started_at DESC NULLS LAST
        LIMIT 1
      ", params = list(hoy_chr)), error = function(e) {
        tryCatch(db_query("
          SELECT chunks_done_after, chunks_total, rows_upserted, backfill_complete, started_at
          FROM corrida_del_archivo
          WHERE (started_at AT TIME ZONE 'America/Santiago')::date = $1::date
          ORDER BY started_at DESC NULLS LAST
          LIMIT 1
        ", params = list(hoy_chr)), error = function(e2) data.frame())
      })
      if (nrow(br) > 0L) {
        # Filas de la corrida de hoy; el % de fase vigente ya viene de checkpoint vivo.
        bias_rows <- suppressWarnings(as.numeric(br$rows_upserted[1]))
        if ("mantencion_al_dia" %in% names(br)) {
          bias_mantencion <- isTRUE(br$mantencion_al_dia[1])
        }
      }
    }
    if (is.null(bias_rows)) {
      bias_path <- file.path(ingest_logs_dir(), "bias_historico.status.json")
      bias <- read_json_file_safe(bias_path)
      dl <- NULL
      if (!is.null(bias) && !is.null(bias$download)) dl <- bias$download
      fin <- if (!is.null(bias$finished_at)) as.character(unlist(bias$finished_at)[1]) else NULL
      started <- if (!is.null(bias$started_at)) as.character(unlist(bias$started_at)[1]) else NULL
      bias_day <- tryCatch({
        ts <- as.POSIXct(fin %||% started, tz = "UTC")
        as.Date(format(ts, tz = "America/Santiago"))
      }, error = function(e) NA)
      if (!is.na(bias_day) && identical(bias_day, hoy) && !is.null(dl)) {
        if (!is.null(dl$rows_upserted)) bias_rows <- suppressWarnings(as.numeric(unlist(dl$rows_upserted)[1]))
      }
    }
    if (!is.null(bias_vista) && !is.null(bias_vista$line) && nzchar(bias_vista$line)) {
      bias_line <- bias_vista$line
      if (!is.null(bias_rows) && !is.na(bias_rows) && bias_rows > 0) {
        bias_line <- paste0(bias_line, " | ",
          if (bias_rows >= 1e6) paste0(round(bias_rows / 1e6, 1), "M")
          else if (bias_rows >= 1000) paste0(round(bias_rows / 1000, 0), "k")
          else as.character(as.integer(bias_rows)),
          " filas")
      }
    } else if (identical(step_state("bias_historico"), "error")) {
      bias_line <- "fallo"
    } else if (step_done("bias_historico")) {
      bias_line <- step_dur("bias_historico") %||% "ok"
    }
  } else if (!is.null(bias_vista) && !is.null(bias_vista$line) && nzchar(bias_vista$line)) {
    bias_line <- bias_vista$line
  }

  pre_line <- if (!ran_hoy) no_hoy else (step_dur("preflight") %||% if (step_done("preflight")) "ok" else no_hoy)

  day_events <- admin_day_timeline_events(hoy_chr)

  # Avance % por nodo
  expected_vars <- character(0)
  if (!is.null(day_health$ingest$expected_vars)) {
    expected_vars <- as.character(unlist(day_health$ingest$expected_vars))
  }
  expected_vars <- expected_vars[nzchar(expected_vars)]
  if (!length(expected_vars)) {
    expected_vars <- ADMIN_EXPECTED_INGEST_VARS
  }
  ingest_ratio_vars <- if (length(expected_vars) > 0L) {
    length(intersect(completed_vars, expected_vars)) / length(expected_vars)
  } else {
    NULL
  }
  ingest_ratio <- ingest_ratio_vars
  if (isTRUE(gapfill_completed)) ingest_ratio <- 1

  # Vigilancia (watchdog): day_health / error_report de hoy — no depende de PG omitido viejo
  vig_state <- "pendiente"
  vig_line <- no_hoy
  vig_ran <- !is.null(day_health$watchdog_ran_at) && nzchar(as.character(day_health$watchdog_ran_at)[1])
  if (!ran_hoy) {
    vig_state <- "pendiente"
  } else if (vig_ran) {
    if (isTRUE(gapfill_completed) || length(incomplete_vars) == 0L) {
      vig_state <- "ok"
      vig_line <- if (isTRUE(gapfill_completed)) {
        "gapfill OK · vars completas"
      } else {
        "ok · sin huecos"
      }
    } else {
      vig_state <- "parcial"
      vig_line <- sprintf(
        "corrió · faltan %d var: %s",
        length(incomplete_vars),
        paste(incomplete_vars, collapse = ", ")
      )
    }
  } else if (ran_hoy) {
    vig_state <- "atrasado"
    vig_line <- "pendiente tras Diario"
  }

  st_pre <- step_state("preflight")
  st_al10 <- step_state("alertas_top10")
  st_ref <- step_state("refresh_pronostico")
  st_alp <- step_state("alertas_pronostico")
  st_qc <- step_state("verificar_concordancia")
  st_bias <- step_state("bias_historico")
  if (!ran_hoy) {
    st_pre <- st_al10 <- st_ref <- st_alp <- st_qc <- st_bias <- "pendiente"
  }
  om_exit <- step_exit_code("refresh_pronostico")
  om_started <- step_started_at("refresh_pronostico")
  om_finished <- step_finished_at("refresh_pronostico")
  om_dur <- step_duration_sec("refresh_pronostico")
  om_errs <- step_error_codes("refresh_pronostico")
  om_pct <- hub_stage_pct(st_ref)
  om_preview <- NULL
  if (!is.null(om_hub) && !is.null(om_hub$state)) {
    st_ref <- om_hub$state
    if (!is.null(om_hub$exit_code)) om_exit <- om_hub$exit_code
    if (!is.null(om_hub$pct)) om_pct <- om_hub$pct
    if (!is.null(om_hub$started_at)) om_started <- om_hub$started_at
    if (!is.null(om_hub$finished_at)) om_finished <- om_hub$finished_at
    if (!is.null(om_hub$duration_sec)) om_dur <- om_hub$duration_sec
    if (isTRUE(om_hub$drop_step_error)) {
      om_errs <- om_errs[!grepl("^STEP_REFRESH_PRONOSTICO_FAIL", om_errs)]
    }
    if (length(om_errs) < 1L) om_errs <- NULL
    suf <- as.character(om_hub$line_suffix %||% "")[1]
    if (!is.na(suf) && nzchar(suf)) {
      if (!is.null(om_line) && nzchar(om_line) && !identical(om_line, no_hoy)) {
        om_line <- paste(om_line, suf, sep = " · ")
      } else {
        om_line <- suf
      }
    }
    if (!is.null(om_hub$error_preview) && nzchar(om_hub$error_preview)) {
      om_preview <- om_hub$error_preview
    }
  }

  # Descargar cerrado por gapfill → hora de Vigilancia (cuando quedó completo)
  ingest_fin <- step_finished_at("ingest")
  if (isTRUE(gapfill_completed) && vig_ran) {
    ingest_fin <- as.character(day_health$watchdog_ran_at)[1]
  }
  vig_fin <- if (vig_ran) as.character(day_health$watchdog_ran_at)[1] else NULL

  ingest_err_preview <- NULL
  if (nrow(df) > 0L) {
    ingest_err_preview <- errores_preview_text(
      df$errores[[1]] %||% df$errores[1],
      max_len = 240L
    )
  }

  vig_actions <- list()
  for (a in err_rep$actions %||% list()) {
    tool <- as.character(a$tool %||% "")
    if (!nzchar(tool)) next
    vig_actions[[length(vig_actions) + 1L]] <- list(
      ts = as.character(a$ts %||% ""),
      tool = tool,
      result = as.character(a$result %||% ""),
      detail = as.character(a$detail %||% "")
    )
  }
  if (length(vig_actions) > 6L) {
    vig_actions <- vig_actions[(length(vig_actions) - 5L):length(vig_actions)]
  }

  run_started <- if (nrow(df) > 0L) {
    tryCatch(
      format(df$started_at[1], "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago"),
      error = function(e) NULL
    )
  } else {
    NULL
  }

  # Arco retrógrado Vigilancia → Descargar (huecos post-ingesta vs gapfill)
  gapfill_fecha <- tryCatch({
    fin_ckpt <- ckpt[["fecha_fin"]]
    if (!is.null(fin_ckpt) && nzchar(as.character(fin_ckpt)[1])) {
      as.Date(fin_ckpt)
    } else {
      hoy
    }
  }, error = function(e) hoy)
  gap_pares <- admin_gapfill_pares_meta(
    incomplete_vars = incomplete_vars,
    fecha_fin = gapfill_fecha,
    day_tag = day_tag,
    gapfill_completed = isTRUE(gapfill_completed),
    ttl_sec = 60
  )

  # Descargar % por pares est/var (denom ≠ huecos gapfill). Fallback: ratio vars + blend gapfill.
  file_huecos <- admin_read_gapfill_huecos_file(day_tag, gapfill_fecha)
  n_pares_total <- admin_ingest_pares_total(expected_vars, file_huecos, est_dga, est_dmc)
  n_pares_rem <- if (isTRUE(gapfill_completed) || length(incomplete_vars) == 0L) {
    0L
  } else {
    as.integer(gap_pares$n_pares_gapfill %||% 0L)
  }
  ingest_pct_pares <- admin_ingest_pct_pares(n_pares_total, n_pares_rem)
  base_var_pct <- if (!is.null(ingest_ratio_vars) && is.finite(ingest_ratio_vars)) {
    as.integer(max(0L, min(100L, round(100 * ingest_ratio_vars))))
  } else {
    0L
  }
  if (isTRUE(gapfill_completed)) {
    ingest_pct_out <- 100L
  } else if (!is.null(ingest_pct_pares)) {
    ingest_pct_out <- as.integer(ingest_pct_pares)
  } else {
    gf_pct <- suppressWarnings(as.integer(gap_pares$pct %||% NA)[1])
    n_in <- suppressWarnings(as.integer(gap_pares$n_pares_ingest %||% 0L)[1])
    if (length(gf_pct) == 1L && !is.na(gf_pct) && length(n_in) == 1L && !is.na(n_in) && n_in > 0L) {
      # Descargar avanza desde base → 100 mientras gapfill (Vigilancia) va 0 → 100 de huecos
      ingest_pct_out <- as.integer(max(
        base_var_pct,
        min(100L, round(base_var_pct + (100 - base_var_pct) * gf_pct / 100))
      ))
    } else {
      ingest_pct_out <- hub_stage_pct(ingest_state, ratio = ingest_ratio_vars)
    }
  }
  ingest_ratio <- if (!is.null(ingest_pct_out)) ingest_pct_out / 100 else ingest_ratio_vars

  # Vigilancia: % = avance gapfill (hechas/inicial); ok → 100
  vig_pct <- if (identical(vig_state, "ok") || isTRUE(gapfill_completed)) {
    100L
  } else if (!is.null(gap_pares$pct)) {
    as.integer(gap_pares$pct)
  } else {
    hub_stage_pct(vig_state)
  }
  if (identical(vig_state, "parcial") && !is.null(gap_pares$n_done) &&
      !is.null(gap_pares$n_pares_ingest) && as.integer(gap_pares$n_pares_ingest) > 0L) {
    vig_line <- sprintf(
      "corrió · hechas %d · faltan %d",
      as.integer(gap_pares$n_done),
      as.integer(gap_pares$n_pares_gapfill %||% 0L)
    )
  }

  list(
    day = hoy_chr,
    day_label = format(hoy, "%d/%m/%Y"),
    ran_today = ran_hoy,
    run_id = if (nrow(df) > 0L) pg_scalar_text(df$run_id[1]) else NULL,
    run_started_at = run_started,
    preflight = empty(st_pre, pre_line, pct = hub_stage_pct(st_pre),
                      started_at = step_started_at("preflight"),
                      finished_at = step_finished_at("preflight"),
                      duration_sec = step_duration_sec("preflight"),
                      exit_code = step_exit_code("preflight"),
                      errors = step_error_codes("preflight")),
    ingest = empty(ingest_state, ingest_line,
                   estaciones_dga = est_dga, estaciones_dmc = est_dmc, filas = filas,
                   n_pares_faltantes = ingest_cls$n_pares,
                   n_pares_total = if (n_pares_total > 0L) as.integer(n_pares_total) else NULL,
                   incomplete_vars = ingest_cls$incomplete_vars,
                   response_ms = response_ms,
                   gapfill_completed = isTRUE(gapfill_completed),
                   started_at = step_started_at("ingest"),
                   finished_at = ingest_fin,
                   duration_sec = step_duration_sec("ingest"),
                   exit_code = step_exit_code("ingest"),
                   errors = step_error_codes("ingest"),
                   error_preview = ingest_err_preview,
                   pct = ingest_pct_out),
    alertas_top10 = empty(st_al10, tardia_line, n_alertas = n_tardias,
                          started_at = step_started_at("alertas_top10"),
                          finished_at = step_finished_at("alertas_top10"),
                          duration_sec = step_duration_sec("alertas_top10"),
                          exit_code = step_exit_code("alertas_top10"),
                          errors = step_error_codes("alertas_top10"),
                          pct = hub_stage_pct(st_al10)),
    refresh_pronostico = empty(st_ref, om_line,
                               fecha_desde = om_desde, fecha_hasta = om_hasta,
                               started_at = om_started,
                               finished_at = om_finished,
                               duration_sec = om_dur,
                               exit_code = om_exit,
                               errors = om_errs,
                               error_preview = om_preview,
                               pct = om_pct),
    alertas_pronostico = empty(st_alp, tempr_line,
                               n_alertas = n_tempranas, n_correos = n_correos,
                               n_eventos_correo = n_eventos_mail,
                               started_at = step_started_at("alertas_pronostico"),
                               finished_at = step_finished_at("alertas_pronostico"),
                               duration_sec = step_duration_sec("alertas_pronostico"),
                               exit_code = step_exit_code("alertas_pronostico"),
                               errors = step_error_codes("alertas_pronostico"),
                               pct = hub_stage_pct(st_alp)),
    verificar_concordancia = empty(st_qc, qc_line,
                                   started_at = step_started_at("verificar_concordancia"),
                                   finished_at = step_finished_at("verificar_concordancia"),
                                   duration_sec = step_duration_sec("verificar_concordancia"),
                                   exit_code = step_exit_code("verificar_concordancia"),
                                   errors = step_error_codes("verificar_concordancia"),
                                   pct = hub_stage_pct(st_qc)),
    bias_historico = empty(st_bias, bias_line,
                           pct_complete = bias_pct, rows_upserted = bias_rows,
                           fase = bias_fase, fase_label = bias_fase_label,
                           fase_idx = bias_fase_idx, fase_total = bias_fase_total,
                           fases = bias_fases, mantencion_al_dia = bias_mantencion,
                           started_at = step_started_at("bias_historico"),
                           finished_at = step_finished_at("bias_historico"),
                           duration_sec = step_duration_sec("bias_historico"),
                           exit_code = step_exit_code("bias_historico"),
                           errors = step_error_codes("bias_historico"),
                           pct = hub_stage_pct(st_bias, pct_direct = bias_pct)),
    vigilancia = empty(vig_state, vig_line,
                       gapfill_completed = isTRUE(gapfill_completed),
                       watchdog_ran_at = day_health$watchdog_ran_at,
                       started_at = if (length(vig_actions)) {
                         as.character(vig_actions[[1]]$ts %||% "")[1]
                       } else {
                         NULL
                       },
                       finished_at = vig_fin,
                       errors = step_error_codes("vigilancia"),
                       actions = vig_actions,
                       watchdog_run_id = err_rep$watchdog_run_id %||% NULL,
                       n_done = gap_pares$n_done,
                       n_pares_ingest = gap_pares$n_pares_ingest,
                       n_pares_gapfill = gap_pares$n_pares_gapfill,
                       pct = vig_pct),
    gapfill_loop = list(
      from = "vigilancia",
      to = "ingest",
      active = isTRUE(vig_ran) || identical(vig_state, "atrasado") || identical(vig_state, "parcial") ||
        length(incomplete_vars) > 0L,
      completed = isTRUE(gapfill_completed),
      pending_vars = incomplete_vars,
      n_pares_ingest = gap_pares$n_pares_ingest,
      n_pares_gapfill = gap_pares$n_pares_gapfill,
      n_pares_total = if (n_pares_total > 0L) as.integer(n_pares_total) else NULL,
      n_done = gap_pares$n_done,
      pct = gap_pares$pct,
      ingest_pct = ingest_pct_out,
      by_var = gap_pares$by_var,
      label = gap_pares$label
    ),
    run_date = hoy_chr,
    day_events = day_events
  )
}

# Tramos aún abiertos al abrir el hub. No inserta en parte_de_guardia.
# Sidecar del Diario + locks. Una fila PG cerrada del mismo run gana.
admin_timeline_merge_open <- function(events, hoy_chr) {
  pg <- events
  logs <- ingest_logs_dir()

  tl_parse <- function(at_chr) {
    if (is.null(at_chr) || length(at_chr) == 0L) {
      return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
    }
    if (inherits(at_chr, "POSIXct")) return(at_chr[1])
    s <- as.character(at_chr)[1]
    if (!nzchar(s) || is.na(s) || identical(s, "null")) {
      return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
    }
    s <- sub("\\.\\d+", "", s)
    s <- sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", s)
    ts <- suppressWarnings(as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"))
    if (is.na(ts)) ts <- suppressWarnings(as.POSIXct(s, format = "%Y-%m-%d %H:%M:%S%z", tz = "UTC"))
    if (is.na(ts)) return(ts)
    as.POSIXct(format(ts, tz = "America/Santiago"), tz = "America/Santiago")
  }
  tl_fmt <- function(ts) {
    if (length(ts) != 1L || is.na(ts)) return(NULL)
    format(ts, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago")
  }
  tl_age <- function(at_chr) {
    ts <- tl_parse(at_chr)
    if (length(ts) != 1L || is.na(ts)) return(NA_real_)
    as.numeric(difftime(Sys.time(), ts, units = "mins"))
  }
  tl_file_age <- function(path) {
    if (!file.exists(path)) return(NA_real_)
    mt <- file.info(path)$mtime
    if (length(mt) != 1L || is.na(mt)) return(NA_real_)
    as.numeric(difftime(Sys.time(), mt, units = "mins"))
  }
  nz1 <- function(x) {
    s <- as.character(x %||% "")[1]
    if (length(s) != 1L || is.na(s)) return("")
    s
  }
  # run_id con un tramo de job ya cerrado en PG: el sidecar no se duplica.
  pg_span_closed <- function(run_id, span_ids) {
    if (!nzchar(run_id)) return(FALSE)
    for (ev in pg) {
      if (!identical(nz1(ev$run_id), run_id)) next
      if (!(nz1(ev$id) %in% span_ids)) next
      if (nzchar(nz1(ev$finished_at))) return(TRUE)
    }
    FALSE
  }
  # Misma corrida del lock: inicio a ±3 min y fin ya escrito.
  pg_lock_closed <- function(task_name, lock_start) {
    if (length(lock_start) != 1L || is.na(lock_start)) return(FALSE)
    for (ev in pg) {
      if (!identical(nz1(ev$task_name), task_name)) next
      if (!nzchar(nz1(ev$finished_at))) next
      st <- tl_parse(ev$started_at)
      if (length(st) != 1L || is.na(st)) next
      if (abs(as.numeric(difftime(st, lock_start, units = "mins"))) <= 3) return(TRUE)
    }
    FALSE
  }
  diario_alive <- function(run_id) {
    if (!nzchar(run_id)) return(FALSE)
    lock <- read_json_file_safe(file.path(logs, "actualizar_diario.lock"))
    if (is.null(lock)) return(FALSE)
    if (!identical(nz1(lock$run_id), run_id)) return(FALSE)
    ages <- c(tl_age(lock$ts), tl_file_age(file.path(logs, "actualizar_diario.checkpoint")))
    tags <- unique(c(
      format(Sys.Date(), "%Y%m%d"),
      format(Sys.time(), "%Y%m%d", tz = "America/Santiago")
    ))
    for (tag in tags) {
      ages <- c(ages, tl_file_age(file.path(logs, sprintf("actualizar_diario_%s.log", tag))))
    }
    ages <- ages[is.finite(ages)]
    if (!length(ages)) return(FALSE)
    min(ages) <= 90
  }
  as_steps <- function(steps) {
    if (is.null(steps) || length(steps) == 0L) return(list())
    if (is.list(steps) && !is.null(names(steps)) && !is.null(steps$id)) return(list(steps))
    if (is.list(steps)) return(unname(steps))
    list()
  }
  step_labels <- c(
    preflight = "Precalentar",
    ingest = "Descargar",
    alertas_top10 = "Alertas confirmadas",
    refresh_pronostico = "Pronóstico",
    alertas_pronostico = "Alertas+",
    verificar_concordancia = "QC",
    evaluar_precision_alertas = "Precisión alertas",
    actualizar_skill_alertas = "Skill alertas",
    bias_historico = "Histórico OM"
  )
  make_ev <- function(id, label, started_chr, finished_chr, outcome, source, run_id, task_name) {
    at <- if (!is.null(finished_chr) && nzchar(nz1(finished_chr))) finished_chr else started_chr
    ts <- tl_parse(at)
    if (length(ts) != 1L || is.na(ts)) return(NULL)
    if (!identical(as.character(as.Date(ts, tz = "America/Santiago")), hoy_chr)) return(NULL)
    mins <- as.integer(format(ts, "%H", tz = "America/Santiago")) * 60L +
      as.integer(format(ts, "%M", tz = "America/Santiago"))
    st_ts <- tl_parse(started_chr)
    fin_ts <- if (is.null(finished_chr) || !nzchar(nz1(finished_chr))) {
      as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
    } else {
      tl_parse(finished_chr)
    }
    dur <- if (length(st_ts) == 1L && !is.na(st_ts) && length(fin_ts) == 1L && !is.na(fin_ts)) {
      as.numeric(difftime(fin_ts, st_ts, units = "secs"))
    } else {
      NULL
    }
    list(
      id = as.character(id),
      label = as.character(label),
      at = tl_fmt(ts),
      hhmm = format(ts, "%H:%M", tz = "America/Santiago"),
      mins = mins,
      outcome = as.character(outcome),
      source = as.character(source),
      trigger = "manual",
      run_id = if (nzchar(run_id)) run_id else NULL,
      started_at = tl_fmt(st_ts),
      finished_at = tl_fmt(fin_ts),
      duration_sec = dur,
      task_name = if (nzchar(task_name)) task_name else NULL
    )
  }
  add <- function(ev) {
    if (is.null(ev)) return()
    events[[length(events) + 1L]] <<- ev
  }

  handled <- character(0)
  side <- read_json_file_safe(file.path(logs, "pipeline_timeline_open.json"))
  if (!is.null(side)) {
    run_id <- nz1(side$run_id)
    task <- nz1(side$task_name)
    if (!nzchar(task)) task <- "Rodaja_Actualizar_Diario"
    is_vig <- identical(task, "Rodaja_Actualizar_Resume")
    span_id <- if (is_vig) "vigilancia" else "orquestador"
    span_lab <- if (is_vig) "Vigilancia" else "Orquestador"
    handled <- c(handled, run_id)
    if (nzchar(run_id) && !pg_span_closed(run_id, c(span_id, "orquestador", "vigilancia"))) {
      alive <- diario_alive(run_id)
      job_start <- nz1(side$started_at)
      last_ts <- tl_parse(job_start)
      any_step <- FALSE
      all_ok <- TRUE
      for (p in as_steps(side$steps)) {
        if (!is.list(p) || isTRUE(p$skipped)) next
        sid <- nz1(p$id)
        if (!nzchar(sid)) next
        any_step <- TRUE
        started <- nz1(p$started_at)
        finished <- nz1(p$finished_at)
        open <- !nzchar(finished)
        for (cand in c(finished, started)) {
          t <- tl_parse(cand)
          if (length(t) == 1L && !is.na(t) && (length(last_ts) != 1L || is.na(last_ts) || t > last_ts)) {
            last_ts <- t
          }
        }
        ec <- suppressWarnings(as.integer(unlist(p$exit_code)[1]))
        bad <- length(ec) == 1L && !is.na(ec) && ec != 0L
        if (bad) all_ok <- FALSE
        # R 4.6: [[ de un nombre ausente en un vector atómico lanza subscript out of bounds.
        lab <- if (sid %in% names(step_labels)) unname(step_labels[[sid]]) else ""
        if (is.null(lab) || !nzchar(lab)) lab <- sid
        if (open && alive) {
          all_ok <- FALSE
          add(make_ev(sid, lab, started, NULL, "en_curso", "step", run_id, task))
        } else if (open) {
          all_ok <- FALSE
          add(make_ev(sid, lab, started, started, "parcial", "step", run_id, task))
        } else {
          add(make_ev(sid, lab, started, finished, if (bad) "error" else "ok", "step", run_id, task))
        }
      }
      if (nzchar(job_start) && (any_step || alive)) {
        if (alive) {
          add(make_ev(span_id, span_lab, job_start, NULL, "en_curso", "task", run_id, task))
        } else if (length(last_ts) == 1L && !is.na(last_ts)) {
          add(make_ev(
            span_id, span_lab, job_start, tl_fmt(last_ts),
            if (all_ok) "ok" else "parcial", "task", run_id, task
          ))
        }
      }
    }
  }

  lock <- read_json_file_safe(file.path(logs, "actualizar_diario.lock"))
  status <- read_json_file_safe(file.path(logs, "actualizar_diario.status.json"))
  if (!is.null(lock)) {
    lrid <- nz1(lock$run_id)
    if (!nzchar(lrid) && !is.null(status)) lrid <- nz1(status$run_id)
    if (nzchar(lrid) && !(lrid %in% handled) && diario_alive(lrid) &&
        !pg_span_closed(lrid, c("orquestador", "vigilancia"))) {
      task <- nz1(lock$task_name)
      if (!nzchar(task) && !is.null(status)) task <- nz1(status$task_name)
      mode <- nz1(lock$mode)
      if (!nzchar(mode) && !is.null(status)) mode <- nz1(status$mode)
      phase <- nz1(status$phase)
      if (!nzchar(phase)) phase <- nz1(lock$phase)
      is_vig <- identical(task, "Rodaja_Actualizar_Resume") ||
        mode %in% c("watchdog", "resume", "gapfill_post")
      if (!identical(phase, "done")) {
        start <- ""
        if (!is.null(status) && identical(nz1(status$run_id), lrid)) start <- nz1(status$started_at)
        if (!nzchar(start)) start <- nz1(lock$started_at)
        if (!nzchar(start)) start <- nz1(lock$ts)
        if (nzchar(start)) {
          if (is_vig) {
            add(make_ev("vigilancia", "Vigilancia", start, NULL, "en_curso", "task", lrid, task))
          } else if (identical(phase, "running") || !nzchar(phase)) {
            if (!nzchar(task)) task <- "Rodaja_Actualizar_Diario"
            add(make_ev("orquestador", "Orquestador", start, NULL, "en_curso", "task", lrid, task))
          }
        }
      }
    }
  }

  lock_specs <- list(
    list(file = "panorama_icon.lock", task = "Rodaja_Panorama_ICON",
         id = "download", label = "Panorama ICON", max_age = 90),
    list(file = "backfill_bbdd.lock", task = "Rodaja_Backfill_BBDD",
         id = "backfill", label = "backfill BBDD", max_age = 480),
    list(file = "backfill_u2_dire.lock", task = "Rodaja_Backfill_DGA_Dire",
         id = "backfill", label = "direccion viento DGA", max_age = 480),
    list(file = "generar_seo.lock", task = "Rodaja_Generar_SEO",
         id = "generar_seo", label = "SEO / IA", max_age = 90)
  )
  for (spec in lock_specs) {
    lk <- read_json_file_safe(file.path(logs, spec$file))
    if (is.null(lk)) next
    start <- nz1(lk$ts)
    age <- tl_age(start)
    if (!is.finite(age) || age > spec$max_age) next
    if (pg_lock_closed(spec$task, tl_parse(start))) next
    add(make_ev(spec$id, spec$label, start, NULL, "en_curso", "step", nz1(lk$run_id), spec$task))
  }

  events
}

# Eventos reales del día (pasos / tareas) para el timeline "ocurrido"
admin_day_timeline_events <- function(hoy_chr = NULL) {
  if (is.null(hoy_chr) || !nzchar(hoy_chr)) {
    hoy_df <- tryCatch(
      db_query("SELECT (CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date AS d"),
      error = function(e) data.frame()
    )
    hoy_chr <- if (nrow(hoy_df) > 0L) as.character(hoy_df$d[1]) else as.character(Sys.Date())
  }
  if (!admin_tabla_existe("parte_de_guardia")) {
    return(admin_timeline_merge_open(list(), hoy_chr))
  }

  step_labels <- c(
    preflight = "Precalentar",
    ingest = "Descargar",
    alertas_top10 = "Alertas confirmadas",
    refresh_pronostico = "Pronóstico",
    alertas_pronostico = "Alertas+",
    verificar_concordancia = "QC",
    evaluar_precision_alertas = "Precisión alertas",
    actualizar_skill_alertas = "Skill alertas",
    bias_historico = "Histórico OM"
  )
  task_labels <- c(
    Rodaja_Docker_Warmup = "ensure docker",
    Rodaja_Actualizar_Diario = "Pipeline",
    Rodaja_Actualizar_Resume = "Vigilancia",
    Rodaja_Actualizar_Reportes_Mensual = "Reportes"
  )

  df <- tryCatch(db_query("
    SELECT id, run_id, started_at, finished_at, exit_code, skipped,
           pasos_detalle, task_name, mode, job_bat, metrics
    FROM parte_de_guardia
    WHERE (started_at AT TIME ZONE 'America/Santiago')::date = $1::date
      AND COALESCE(skipped, false) IS NOT TRUE
      AND COALESCE(mode, '') <> 'skip'
    ORDER BY started_at ASC
  ", params = list(hoy_chr)), error = function(e) data.frame())

  if (nrow(df) == 0L) return(admin_timeline_merge_open(list(), hoy_chr))

  infer_trigger <- function(metrics, mode, task_name, started_mins) {
    raw <- NULL
    if (is.list(metrics)) {
      raw <- metrics$trigger_source %||% metrics$trigger %||% NULL
      if (is.list(raw)) raw <- raw[[1]]
    }
    raw <- as.character(raw %||% "")
    if (nzchar(raw)) {
      raw <- tolower(raw)
      if (raw %in% c("scheduled", "scheduler")) return("scheduler")
      if (raw %in% c("manual", "chained")) return(raw)
    }
    # Heurística por ventana horaria de Task Scheduler
    near <- function(target, tol = 25L) abs(as.integer(started_mins) - as.integer(target)) <= tol
    if (identical(task_name, "Rodaja_Docker_Warmup") || identical(mode, "warmup")) {
      if (near(55L)) return("scheduler")
    }
    if (identical(task_name, "Rodaja_Actualizar_Diario") || identical(mode, "full")) {
      if (near(65L)) return("scheduler")
      return("manual")
    }
    if (identical(task_name, "Rodaja_Actualizar_Resume") ||
        mode %in% c("watchdog", "resume")) {
      if (near(1320L)) return("scheduler")
      return("chained")
    }
    "manual"
  }

  events <- list()
  parse_event_ts <- function(at_chr) {
    if (is.null(at_chr) || !nzchar(as.character(at_chr)[1])) return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
    s <- as.character(at_chr)[1]
    s <- sub("\\.\\d+", "", s)
    s <- sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", s)
    ts <- suppressWarnings(as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"))
    if (is.na(ts)) {
      ts <- suppressWarnings(as.POSIXct(s, format = "%Y-%m-%d %H:%M:%S%z", tz = "UTC"))
    }
    if (is.na(ts)) return(ts)
    as.POSIXct(format(ts, tz = "America/Santiago"), tz = "America/Santiago")
  }
  fmt_span_ts <- function(x) {
    if (is.null(x) || length(x) == 0L) return(NULL)
    if (inherits(x, "POSIXct")) {
      if (is.na(x[1])) return(NULL)
      return(format(x[1], "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago"))
    }
    s <- as.character(x)[1]
    if (!nzchar(s) || is.na(s)) return(NULL)
    s
  }
  push_ev <- function(id, label, at_chr, outcome, source, run_id = NULL, trigger = "manual",
                      started_chr = NULL, finished_chr = NULL, duration_sec = NULL,
                      task_name = NULL) {
    if (is.null(at_chr) || !nzchar(as.character(at_chr)[1])) return()
    ts <- parse_event_ts(at_chr)
    if (is.na(ts)) return()
    if (!identical(as.character(as.Date(ts, tz = "America/Santiago")), hoy_chr)) return()
    mins <- as.integer(format(ts, "%H", tz = "America/Santiago")) * 60L +
      as.integer(format(ts, "%M", tz = "America/Santiago"))
    dur <- suppressWarnings(as.numeric(duration_sec)[1])
    if (length(dur) != 1L || is.na(dur)) dur <- NULL
    tn <- as.character(task_name %||% "")[1]
    events[[length(events) + 1L]] <<- list(
      id = as.character(id),
      label = as.character(label),
      at = format(ts, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago"),
      hhmm = format(ts, "%H:%M", tz = "America/Santiago"),
      mins = mins,
      outcome = as.character(outcome),
      source = as.character(source),
      trigger = as.character(trigger),
      run_id = if (is.null(run_id)) NULL else as.character(run_id),
      started_at = fmt_span_ts(started_chr),
      finished_at = fmt_span_ts(finished_chr),
      duration_sec = dur,
      task_name = if (nzchar(tn)) tn else NULL
    )
  }
  emit_row_span <- function(id, label, row, outcome, rid, trigger, task_name = NULL) {
    st <- row$started_at[1]
    fin <- row$finished_at[1]
    at <- if (!is.na(fin)) {
      format(fin, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago")
    } else if (!is.na(st)) {
      format(st, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago")
    } else {
      return()
    }
    dur <- if (!is.na(st) && !is.na(fin)) as.numeric(difftime(fin, st, units = "secs")) else NULL
    push_ev(
      id, label, at, outcome, "task", rid, trigger,
      started_chr = if (!is.na(st)) format(st, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago") else NULL,
      finished_chr = if (!is.na(fin)) format(fin, "%Y-%m-%dT%H:%M:%S%z", tz = "America/Santiago") else NULL,
      duration_sec = dur,
      task_name = task_name %||% id
    )
  }
  emit_paso <- function(tn, p, rid, trigger, id_override = NULL) {
    if (!is.list(p) || isTRUE(p$skipped)) return(FALSE)
    sid <- as.character(id_override %||% p$id %||% "")[1]
    if (!nzchar(sid)) return(FALSE)
    lab <- as.character(p$label %||% sid)[1]
    if (!nzchar(lab)) lab <- sid
    ec <- suppressWarnings(as.integer(p$exit_code %||% 0L))
    if (length(ec) != 1L || is.na(ec)) ec <- 0L
    outcome <- if (ec == 0L) "ok" else "error"
    at <- as.character(p$finished_at %||% p$started_at %||% "")[1]
    dur <- suppressWarnings(as.numeric(p$duration_sec %||% NA_real_)[1])
    if (length(dur) != 1L || is.na(dur)) dur <- NULL
    push_ev(sid, lab, at, outcome, "step", rid, trigger,
            started_chr = p$started_at, finished_chr = p$finished_at %||% p$started_at,
            duration_sec = dur, task_name = tn)
    TRUE
  }

  known_steps <- names(step_labels)
  allow_modes <- c("full", "watchdog", "resume", "warmup", "scrape", "post")

  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]
    tn <- pg_scalar_text(row$task_name[1]) %||% ""
    mode <- pg_scalar_text(row$mode[1]) %||% ""
    rid <- pg_scalar_text(row$run_id[1])
    metrics <- jsonb_as_list(row$metrics[[1]] %||% row$metrics[1])
    if (!is.list(metrics)) metrics <- list()
    started_mins <- tryCatch({
      st <- as.POSIXct(row$started_at[1], tz = "UTC")
      st_loc <- as.POSIXct(format(st, tz = "America/Santiago"), tz = "America/Santiago")
      as.integer(format(st_loc, "%H")) * 60L + as.integer(format(st_loc, "%M"))
    }, error = function(e) 0L)
    trigger <- infer_trigger(metrics, mode, tn, started_mins)

    pasos <- jsonb_as_list(row$pasos_detalle[[1]] %||% row$pasos_detalle[1])
    pasos <- normalize_pasos_detalle(pasos)

    ec_row <- suppressWarnings(as.integer(row$exit_code[1] %||% 0L))
    if (length(ec_row) != 1L || is.na(ec_row)) ec_row <- 0L
    outcome_row <- if (ec_row == 0L) "ok" else "error"
    # Carriles del timeline: un tramo por corrida (inicio→fin), no un punto al terminar.
    if (tn %in% c("Rodaja_Panorama_ICON", "Rodaja_Backfill_BBDD",
                  "Rodaja_Backfill_DGA_Dire", "Rodaja_Backfill_DGA_Rafaga",
                  "Rodaja_Docker_Warmup",
                  "Rodaja_Generar_SEO", "Rodaja_Actualizar_Reportes_Mensual",
                  "Rodaja_Bias_Historico_Chained", "Rodaja_Pronostico_Precision_Chained")) {
      n_pasos <- 0L
      if (length(pasos) > 0L) {
        for (p in pasos) {
          if (isTRUE(emit_paso(tn, p, rid, trigger))) n_pasos <- n_pasos + 1L
        }
      }
      if (n_pasos == 0L) {
        lab_row <- switch(tn,
          Rodaja_Panorama_ICON = "Panorama ICON",
          Rodaja_Backfill_BBDD = "backfill BBDD",
          Rodaja_Backfill_DGA_Dire = "direccion viento DGA",
          Rodaja_Backfill_DGA_Rafaga = "rafaga horaria DGA",
          Rodaja_Docker_Warmup = "ensure docker",
          Rodaja_Generar_SEO = "SEO / IA",
          Rodaja_Bias_Historico_Chained = "descarga historica Open-Meteo",
          Rodaja_Pronostico_Precision_Chained = "recalculo bias",
          "Reportes PDF"
        )
        emit_row_span(tn, lab_row, row, outcome_row, rid, trigger, task_name = tn)
      }
      next
    }
    if (identical(tn, "Rodaja_Actualizar_Diario") || identical(mode, "full")) {
      emit_row_span("orquestador", "Orquestador", row, outcome_row, rid, trigger,
                    task_name = if (nzchar(tn)) tn else "Rodaja_Actualizar_Diario")
    }
    if (identical(tn, "Rodaja_Actualizar_Resume") || mode %in% c("watchdog", "resume")) {
      n_vig <- 0L
      if (length(pasos) > 0L) {
        for (p in pasos) {
          if (isTRUE(emit_paso(if (nzchar(tn)) tn else "Rodaja_Actualizar_Resume",
                               p, rid, trigger, id_override = "vigilancia"))) {
            n_vig <- n_vig + 1L
          }
        }
      }
      if (n_vig == 0L) {
        emit_row_span("vigilancia", "Vigilancia", row, outcome_row, rid, trigger,
                      task_name = if (nzchar(tn)) tn else "Rodaja_Actualizar_Resume")
      }
      next
    }

    is_pipe <- length(pasos) > 0L && any(vapply(pasos, function(p) {
      as.character(p$id %||% "") %in% known_steps
    }, logical(1)))
    is_allowed_task <- (nzchar(tn) && tn %in% names(task_labels)) || (mode %in% allow_modes)
    if (!is_pipe && !is_allowed_task) next

    if (length(pasos) > 0L) {
      for (p in pasos) {
        sid <- as.character(p$id %||% "")
        if (!nzchar(sid) || !(sid %in% known_steps)) next
        if (isTRUE(p$skipped)) next
        ec <- suppressWarnings(as.integer(p$exit_code %||% 0L))
        if (is.na(ec)) ec <- 0L
        outcome <- if (ec == 0L) "ok" else "error"
        lab <- unname(step_labels[[sid]])
        at <- as.character(p$finished_at %||% p$started_at %||% "")
        dur <- suppressWarnings(as.numeric(p$duration_sec %||% NA_real_)[1])
        if (length(dur) != 1L || is.na(dur)) dur <- NULL
        push_ev(sid, lab, at, outcome, "step", rid, trigger,
                started_chr = p$started_at, finished_chr = p$finished_at %||% p$started_at,
                duration_sec = dur, task_name = tn)
      }
    } else if (is_allowed_task) {
      lab <- if (nzchar(tn) && tn %in% names(task_labels)) {
        unname(task_labels[[tn]])
      } else if (identical(mode, "watchdog") || identical(mode, "resume")) {
        "Vigilancia"
      } else if (identical(mode, "warmup")) {
        "Precalentar"
      } else {
        next
      }
      eid <- if (nzchar(tn)) tn else (mode %||% "run")
      emit_row_span(eid, lab, row, outcome_row, rid, trigger,
                    task_name = if (nzchar(tn)) tn else NULL)
    }
  }

  admin_timeline_merge_open(events, hoy_chr)
}

admin_backfill_bbdd_stats <- function() {
  logs <- ingest_logs_dir()
  st <- read_json_file_safe(file.path(logs, "backfill_bbdd.status.json"))
  ckpt <- read_ckpt_kv(file.path(logs, "backfill_u_rafaga.ckpt"))
  lock <- read_json_file_safe(file.path(logs, "backfill_bbdd.lock"))
  running <- FALSE
  if (!is.null(lock)) {
    running <- TRUE
    age <- NULL
    if (!is.null(lock$ts)) age <- age_min_from_ts(parse_live_ts(lock$ts))
    # Lock solo durante el scrape (max 6 h); 8 h deja margen al WaitIdle corto.
    if (!is.null(age) && is.finite(age) && age > 480) running <- FALSE
  }
  # La cuota horaria usa el mismo lock. El nodo de u_rafaga se queda en listo.
  lock_phase <- if (!is.null(lock)) as.character(lock$phase %||% "")[1] else ""
  if (identical(lock_phase, "u_rafaga_h")) running <- FALSE
  hoy_df <- tryCatch(
    db_query("SELECT (CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date AS d"),
    error = function(e) data.frame()
  )
  hoy <- if (nrow(hoy_df) > 0L) as.character(hoy_df$d[1]) else as.character(Sys.Date())
  ini <- "2012-01-01"
  fin <- hoy
  if (!is.null(st$fecha_ini) && nzchar(as.character(st$fecha_ini)[1])) {
    ini <- as.character(st$fecha_ini)[1]
  }
  if (!is.null(st$fecha_fin) && nzchar(as.character(st$fecha_fin)[1])) {
    fin <- as.character(st$fecha_fin)[1]
  }
  nxt <- NULL
  if (!is.null(st$fecha_next) && nzchar(as.character(st$fecha_next)[1])) {
    nxt <- as.character(st$fecha_next)[1]
  } else if (nzchar(as.character(ckpt[["fecha_next"]] %||% ""))) {
    nxt <- as.character(ckpt[["fecha_next"]])[1]
  }
  pct_fn <- function(nxt, ini, fin) {
    d_ini <- tryCatch(as.Date(ini), error = function(e) NA)
    d_fin <- tryCatch(as.Date(fin), error = function(e) NA)
    d_nxt <- tryCatch(as.Date(nxt), error = function(e) NA)
    if (length(d_ini) != 1L || is.na(d_ini) || length(d_fin) != 1L || is.na(d_fin) || d_ini > d_fin) {
      return(0L)
    }
    total <- as.integer(d_fin - d_ini) + 1L
    if (total < 1L) return(0L)
    if (length(d_nxt) != 1L || is.na(d_nxt) || d_nxt <= d_ini) return(0L)
    if (d_nxt > d_fin) return(100L)
    as.integer(max(0L, min(100L, round(100 * as.integer(d_nxt - d_ini) / total))))
  }
  max_nxt <- as.character(ckpt[["fecha_next"]] %||% "")
  max_done <- FALSE
  if (nzchar(max_nxt)) {
    d_max <- tryCatch(as.Date(max_nxt), error = function(e) NA)
    d_hoy <- tryCatch(as.Date(hoy), error = function(e) NA)
    max_done <- length(d_max) == 1L && !is.na(d_max) &&
      length(d_hoy) == 1L && !is.na(d_hoy) && d_max > d_hoy
  }
  # u_rafaga todavía no cerró: la tarjeta sigue siendo esa fase.
  if (!isTRUE(max_done)) {
    pct <- suppressWarnings(as.integer(st$pct %||% NA)[1])
    if (length(pct) != 1L || is.na(pct)) pct <- pct_fn(nxt, ini, fin)
    phase <- as.character(st$phase %||% "")[1]
    complete <- identical(phase, "complete") ||
      (!is.null(nxt) && !is.na(tryCatch(as.Date(nxt), error = function(e) NA)) &&
         tryCatch(as.Date(nxt), error = function(e) as.Date("1900-01-01")) > tryCatch(as.Date(fin), error = function(e) as.Date("9999-01-01")))
    state <- "pendiente"
    if (isTRUE(running) || identical(phase, "running")) {
      state <- "en_curso"
    } else if (isTRUE(complete) || identical(pct, 100L)) {
      state <- "ok"
    } else if (!is.null(nxt) && nzchar(nxt) && !identical(nxt, ini)) {
      state <- "parcial"
    }
    line <- if (identical(state, "ok") || isTRUE(complete)) {
      paste0("listo · u_rafaga hasta ", fin)
    } else if (!is.null(nxt) && nzchar(nxt)) {
      paste0(nxt, " · ", pct, "%")
    } else {
      "sin arranque"
    }
    if (identical(state, "en_curso")) line <- paste0(line, " · cuota")
    return(list(
      state = state,
      line = line,
      pct = as.integer(pct),
      fecha_next = nxt,
      fecha_ini = ini,
      fecha_fin = fin,
      phase = if (nzchar(phase)) phase else NULL,
      running = isTRUE(running),
      n_inserted = st$n_inserted %||% NULL,
      stop_reason = st$stop_reason %||% NULL,
      updated_at = st$updated_at %||% ckpt[["updated_at"]] %||% NULL,
      variable = "u_rafaga",
      u10max_complete = FALSE
    ))
  }

  # Predominante: % por año (1915–2011). year_next>2012 es el cursor viejo 2012–hoy.
  kt <- read_ckpt_kv(file.path(logs, "backfill_u_10_kt.ckpt"))
  raw_y <- suppressWarnings(as.integer(kt[["year_next"]] %||% NA)[1])
  kt_done <- length(raw_y) == 1L && !is.na(raw_y) && raw_y == 2012L
  yn <- if (length(raw_y) != 1L || is.na(raw_y) || raw_y > 2012L) 1915L else raw_y
  y0 <- 1915L
  y1 <- 2011L
  pct <- if (yn <= y0) 0L else if (yn > y1) 100L else as.integer(round(100 * (yn - y0) / (y1 - y0 + 1L)))
  state <- if (isTRUE(running)) "en_curso" else if (isTRUE(kt_done)) "ok" else "parcial"
  line <- if (isTRUE(kt_done)) {
    "listo · predominante hasta 2011"
  } else {
    paste0("viento predominante · año ", yn, " · ", pct, "%")
  }
  if (identical(state, "en_curso")) line <- paste0(line, " · cuota")
  list(
    state = state,
    line = line,
    pct = as.integer(pct),
    fecha_next = as.character(yn),
    fecha_ini = "1915-01-01",
    fecha_fin = "2011-12-31",
    phase = if (isTRUE(kt_done)) "complete" else "parcial",
    running = isTRUE(running),
    n_inserted = st$n_inserted %||% NULL,
    stop_reason = st$stop_reason %||% NULL,
    updated_at = kt[["updated_at"]] %||% st$updated_at %||% NULL,
    variable = "u_10_kt",
    u10max_complete = TRUE
  )
}

# Progreso historico u_rafaga_h (2012→hoy). Mismo schtask, status propio.
admin_backfill_u10h_stats <- function() {
  logs <- ingest_logs_dir()
  st <- read_json_file_safe(file.path(logs, "backfill_u_rafaga_h.status.json"))
  ckpt <- read_ckpt_kv(file.path(logs, "backfill_u_rafaga_h.ckpt"))
  lock <- read_json_file_safe(file.path(logs, "backfill_bbdd.lock"))
  lock_phase <- if (!is.null(lock)) as.character(lock$phase %||% "")[1] else ""
  running <- FALSE
  if (!is.null(lock) && identical(lock_phase, "u_rafaga_h")) {
    running <- TRUE
    age <- NULL
    if (!is.null(lock$ts)) age <- age_min_from_ts(parse_live_ts(lock$ts))
    if (!is.null(age) && is.finite(age) && age > 480) running <- FALSE
  }
  hoy_df <- tryCatch(
    db_query("SELECT (CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date AS d"),
    error = function(e) data.frame()
  )
  hoy <- if (nrow(hoy_df) > 0L) as.character(hoy_df$d[1]) else as.character(Sys.Date())
  ini <- "2012-01-01"
  fin <- hoy
  if (!is.null(st$fecha_ini) && nzchar(as.character(st$fecha_ini)[1])) {
    ini <- as.character(st$fecha_ini)[1]
  }
  if (!is.null(st$fecha_fin) && nzchar(as.character(st$fecha_fin)[1])) {
    fin <- as.character(st$fecha_fin)[1]
  }
  nxt <- NULL
  if (!is.null(st$fecha_next) && nzchar(as.character(st$fecha_next)[1])) {
    nxt <- as.character(st$fecha_next)[1]
  } else if (nzchar(as.character(ckpt[["fecha_next"]] %||% ""))) {
    nxt <- as.character(ckpt[["fecha_next"]])[1]
  }
  d_ini <- tryCatch(as.Date(ini), error = function(e) NA)
  d_fin <- tryCatch(as.Date(fin), error = function(e) NA)
  d_nxt <- tryCatch(as.Date(nxt), error = function(e) NA)
  pct <- suppressWarnings(as.integer(st$pct %||% NA)[1])
  if (length(pct) != 1L || is.na(pct)) {
    pct <- 0L
    if (length(d_ini) == 1L && !is.na(d_ini) && length(d_fin) == 1L && !is.na(d_fin) &&
        d_ini <= d_fin && length(d_nxt) == 1L && !is.na(d_nxt)) {
      total <- as.integer(d_fin - d_ini) + 1L
      if (total >= 1L && d_nxt > d_ini) {
        pct <- if (d_nxt > d_fin) 100L else as.integer(round(100 * as.integer(d_nxt - d_ini) / total))
      }
    }
  }
  phase <- as.character(st$phase %||% "")[1]
  complete <- identical(phase, "complete") ||
    (length(d_nxt) == 1L && !is.na(d_nxt) && length(d_fin) == 1L && !is.na(d_fin) && d_nxt > d_fin)
  state <- "pendiente"
  if (isTRUE(running) || identical(phase, "running")) {
    state <- "en_curso"
  } else if (isTRUE(complete) || identical(pct, 100L)) {
    state <- "ok"
  } else if (!is.null(nxt) && nzchar(nxt) && !identical(nxt, ini)) {
    state <- "parcial"
  }
  line <- if (identical(state, "ok") || isTRUE(complete)) {
    paste0("listo · u_rafaga_h hasta ", fin)
  } else if (!is.null(nxt) && nzchar(nxt)) {
    paste0(nxt, " · ", pct, "%")
  } else {
    "sin arranque"
  }
  if (identical(state, "en_curso")) line <- paste0(line, " · cuota")
  list(
    state = state,
    line = line,
    pct = as.integer(max(0L, min(100L, pct))),
    fecha_next = nxt,
    fecha_ini = ini,
    fecha_fin = fin,
    phase = if (nzchar(phase)) phase else NULL,
    running = isTRUE(running),
    n_inserted = st$n_inserted %||% NULL,
    stop_reason = st$stop_reason %||% NULL,
    updated_at = st$updated_at %||% ckpt[["updated_at"]] %||% NULL,
    variable = "u_rafaga_h"
  )
}

# Progreso histórico u2_dire (2011→hoy). Independiente de backfill_bbdd.
admin_backfill_dga_stats <- function() {
  logs <- ingest_logs_dir()
  st <- read_json_file_safe(file.path(logs, "backfill_u2_dire.status.json"))
  ckpt <- read_ckpt_kv(file.path(logs, "backfill_u2_dire.ckpt"))
  lock <- read_json_file_safe(file.path(logs, "backfill_u2_dire.lock"))
  running <- FALSE
  if (!is.null(lock)) {
    running <- TRUE
    age <- NULL
    if (!is.null(lock$ts)) age <- age_min_from_ts(parse_live_ts(lock$ts))
    if (!is.null(age) && is.finite(age) && age > 480) running <- FALSE
  }
  hoy_df <- tryCatch(
    db_query("SELECT (CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date AS d"),
    error = function(e) data.frame()
  )
  hoy <- if (nrow(hoy_df) > 0L) as.character(hoy_df$d[1]) else as.character(Sys.Date())
  ini <- "2011-01-01"
  fin <- hoy
  if (!is.null(st$fecha_ini) && nzchar(as.character(st$fecha_ini)[1])) {
    ini <- as.character(st$fecha_ini)[1]
  }
  if (!is.null(st$fecha_fin) && nzchar(as.character(st$fecha_fin)[1])) {
    fin <- as.character(st$fecha_fin)[1]
  }
  yn <- suppressWarnings(as.integer(st$year_next %||% ckpt[["year_next"]] %||% NA)[1])
  y0 <- suppressWarnings(as.integer(substr(ini, 1L, 4L))[1])
  y1 <- suppressWarnings(as.integer(substr(fin, 1L, 4L))[1])
  if (length(y0) != 1L || is.na(y0)) y0 <- 2011L
  if (length(y1) != 1L || is.na(y1)) y1 <- as.integer(format(Sys.Date(), "%Y"))
  pct <- suppressWarnings(as.integer(st$pct %||% NA)[1])
  if (length(pct) != 1L || is.na(pct)) {
    if (length(yn) != 1L || is.na(yn) || y0 > y1) {
      pct <- 0L
    } else if (yn <= y0) {
      pct <- 0L
    } else if (yn > y1) {
      pct <- 100L
    } else {
      pct <- as.integer(round(100 * (yn - y0) / (y1 - y0 + 1L)))
    }
  }
  phase <- as.character(st$phase %||% "")[1]
  complete <- identical(phase, "complete") ||
    (length(yn) == 1L && !is.na(yn) && yn > y1)
  state <- "pendiente"
  if (isTRUE(running) || identical(phase, "running")) {
    state <- "en_curso"
  } else if (isTRUE(complete) || identical(pct, 100L)) {
    state <- "ok"
  } else if (length(yn) == 1L && !is.na(yn) && yn > y0) {
    state <- "parcial"
  }
  line <- if (identical(state, "ok") || isTRUE(complete)) {
    paste0("listo · u2_dire hasta ", fin)
  } else if (length(yn) == 1L && !is.na(yn)) {
    paste0("año ", yn, " · ", pct, "%")
  } else {
    "sin arranque"
  }
  if (identical(state, "en_curso")) line <- paste0(line, " · cuota")
  list(
    state = state,
    line = line,
    pct = as.integer(pct),
    year_next = if (length(yn) == 1L && !is.na(yn)) as.integer(yn) else NULL,
    fecha_ini = ini,
    fecha_fin = fin,
    phase = if (nzchar(phase)) phase else NULL,
    running = isTRUE(running),
    n_inserted = st$n_inserted %||% NULL,
    stop_reason = st$stop_reason %||% NULL,
    updated_at = st$updated_at %||% ckpt[["updated_at"]] %||% NULL,
    variable = "u2_dire"
  )
}

# Progreso rafaga horaria DGA (_76_, 2011→hoy). Lock y status propios.
admin_backfill_dga_rafaga_stats <- function() {
  logs <- ingest_logs_dir()
  st <- read_json_file_safe(file.path(logs, "backfill_u_rafaga_h_dga.status.json"))
  ckpt <- read_ckpt_kv(file.path(logs, "backfill_u_rafaga_h_dga.ckpt"))
  lock <- read_json_file_safe(file.path(logs, "backfill_dga_rafaga.lock"))
  running <- FALSE
  if (!is.null(lock)) {
    running <- TRUE
    age <- NULL
    if (!is.null(lock$ts)) age <- age_min_from_ts(parse_live_ts(lock$ts))
    if (!is.null(age) && is.finite(age) && age > 480) running <- FALSE
  }
  hoy_df <- tryCatch(
    db_query("SELECT (CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date AS d"),
    error = function(e) data.frame()
  )
  hoy <- if (nrow(hoy_df) > 0L) as.character(hoy_df$d[1]) else as.character(Sys.Date())
  ini <- "2011-01-01"
  fin <- hoy
  if (!is.null(st$fecha_ini) && nzchar(as.character(st$fecha_ini)[1])) {
    ini <- as.character(st$fecha_ini)[1]
  }
  if (!is.null(st$fecha_fin) && nzchar(as.character(st$fecha_fin)[1])) {
    fin <- as.character(st$fecha_fin)[1]
  }
  nxt <- NULL
  if (!is.null(st$fecha_next) && nzchar(as.character(st$fecha_next)[1])) {
    nxt <- as.character(st$fecha_next)[1]
  } else if (nzchar(as.character(ckpt[["fecha_next"]] %||% ""))) {
    nxt <- as.character(ckpt[["fecha_next"]])[1]
  }
  codigo <- as.character(st$codigo %||% ckpt[["codigo"]] %||% "")[1]
  pct <- suppressWarnings(as.integer(st$pct %||% NA)[1])
  if (length(pct) != 1L || is.na(pct)) pct <- 0L
  phase <- as.character(st$phase %||% "")[1]
  complete <- identical(phase, "complete") || identical(pct, 100L)
  state <- "pendiente"
  if (isTRUE(running) || identical(phase, "running")) {
    state <- "en_curso"
  } else if (isTRUE(complete)) {
    state <- "ok"
  } else if (!is.null(nxt) && nzchar(nxt) && !identical(nxt, ini)) {
    state <- "parcial"
  } else if (!is.null(st$estacion_idx) && suppressWarnings(as.integer(st$estacion_idx)[1]) > 1L) {
    state <- "parcial"
  }
  line <- if (identical(state, "ok") || isTRUE(complete)) {
    paste0("listo · u_rafaga_h DGA hasta ", fin)
  } else if (!is.null(nxt) && nzchar(nxt)) {
    paste0(if (nzchar(codigo)) paste0(codigo, " · ") else "", nxt, " · ", pct, "%")
  } else {
    "sin arranque"
  }
  if (identical(state, "en_curso")) line <- paste0(line, " · cuota")
  list(
    state = state,
    line = line,
    pct = as.integer(max(0L, min(100L, pct))),
    fecha_next = nxt,
    fecha_ini = ini,
    fecha_fin = fin,
    codigo = if (nzchar(codigo)) codigo else NULL,
    phase = if (nzchar(phase)) phase else NULL,
    running = isTRUE(running),
    n_inserted = st$n_inserted %||% NULL,
    stop_reason = st$stop_reason %||% NULL,
    updated_at = st$updated_at %||% ckpt[["updated_at"]] %||% NULL,
    variable = "u_rafaga_h"
  )
}

admin_pipelines_resumen <- function() {
  if (!admin_tabla_existe("parte_de_guardia")) {
    return(list(disponible = FALSE))
  }
  by_task <- lapply(PIPELINE_TASK_CATALOG, function(t) {
    tn <- t$task_name
    df <- db_query("
      SELECT id, run_id, started_at, finished_at, exit_code, skipped,
             pasos_detalle, errores, job_bat, metrics, task_name, mode, phase,
             estaciones_dga, estaciones_dmc, filas_afectadas_total,
             filas_afectadas_dga, filas_afectadas_dmc
      FROM parte_de_guardia
      WHERE task_name = $1
      ORDER BY started_at DESC
      LIMIT 1", params = list(tn))
    # Fallback legacy: same job_bat without task_name
    if (nrow(df) == 0L && identical(t$mode, "full")) {
      df <- db_query("
        SELECT id, run_id, started_at, finished_at, exit_code, skipped,
               pasos_detalle, errores, job_bat, metrics, task_name, mode, phase,
               estaciones_dga, estaciones_dmc, filas_afectadas_total,
               filas_afectadas_dga, filas_afectadas_dmc
        FROM parte_de_guardia
        WHERE job_bat = $1 AND (task_name IS NULL OR task_name = '')
        ORDER BY started_at DESC
        LIMIT 1", params = list(t$job_bat))
    }
    if (nrow(df) == 0L && t$mode %in% c("bias", "precision", "reportes", "warmup", "panorama", "backfill")) {
      df <- db_query("
        SELECT id, run_id, started_at, finished_at, exit_code, skipped,
               pasos_detalle, errores, job_bat, metrics, task_name, mode, phase,
               estaciones_dga, estaciones_dmc, filas_afectadas_total,
               filas_afectadas_dga, filas_afectadas_dmc
        FROM parte_de_guardia
        WHERE job_bat = $1
        ORDER BY started_at DESC
        LIMIT 1", params = list(t$job_bat))
    }
    # Skip de 2ª instancia y fail vacío (stderr Docker) no tapan la corrida real.
    if (identical(t$mode, "panorama") && nrow(df) > 0L) {
      skip_or_empty_fail <- isTRUE(df$skipped[1])
      if (!skip_or_empty_fail) {
        ec <- suppressWarnings(as.integer(df$exit_code[1] %||% 0L))[1]
        if (!is.na(ec) && ec != 0L) {
          det <- jsonb_as_list(df$pasos_detalle[[1]] %||% df$pasos_detalle[1])
          ids <- unique(as.character(vapply(det, function(p) {
            if (is.list(p)) as.character(p$id %||% "") else ""
          }, character(1))))
          ids <- ids[nzchar(ids)]
          if (length(ids) == 0L || (length(ids) == 1L && identical(ids[1], "docker"))) {
            skip_or_empty_fail <- TRUE
          }
        }
      }
      if (isTRUE(skip_or_empty_fail)) {
        df_ok <- db_query("
          SELECT id, run_id, started_at, finished_at, exit_code, skipped,
                 pasos_detalle, errores, job_bat, metrics, task_name, mode, phase,
                 estaciones_dga, estaciones_dmc, filas_afectadas_total,
                 filas_afectadas_dga, filas_afectadas_dmc
          FROM parte_de_guardia
          WHERE task_name = $1 AND COALESCE(skipped, false) IS NOT TRUE
            AND COALESCE(exit_code, 0) = 0
          ORDER BY started_at DESC
          LIMIT 1", params = list(tn))
        if (nrow(df_ok) > 0L) df <- df_ok
      }
    }
    ok15 <- db_query("
      SELECT COUNT(*)::int AS n FROM (
        SELECT exit_code, skipped FROM parte_de_guardia
        WHERE task_name = $1
        ORDER BY started_at DESC LIMIT 15
      ) s WHERE exit_code = 0 AND COALESCE(skipped, false) IS NOT TRUE
    ", params = list(tn))$n[1]

    list(
      task_name = tn,
      label = t$label,
      mode = t$mode,
      job_bat = t$job_bat,
      schedule = t$schedule,
      chained = isTRUE(t$chained),
      ok_recientes = as.integer(ok15 %||% 0L),
      ultimo = if (nrow(df) == 0L) NULL else actualizacion_row_list(df[1, , drop = FALSE])
    )
  })

  # Compat: ultimo del scrape o full diario
  scrape <- Filter(function(x) identical(x$mode, "scrape"), by_task)
  full <- Filter(function(x) identical(x$mode, "full"), by_task)
  ultimo <- NULL
  if (length(full) > 0L && !is.null(full[[1]]$ultimo)) ultimo <- full[[1]]$ultimo
  else if (length(scrape) > 0L && !is.null(scrape[[1]]$ultimo)) ultimo <- scrape[[1]]$ultimo
  else if (length(full) > 0L) ultimo <- full[[1]]$ultimo

  list(
    disponible = TRUE,
    ultimo = ultimo,
    ok_recientes = if (!is.null(ultimo) && identical(ultimo$estado, "exitoso")) 1L else 0L,
    by_task = by_task,
    stage_stats = admin_pipeline_stage_stats()
  )
}

# Conteos live de ficha_del_mirador. No mezclar con stage_stats del Diario
# (HUB_STAGE_STATS_WATCHDOG_ROW).
admin_estacion_reporte_snapshot <- function() {
  empty <- list(
    disponible = FALSE,
    activo = 0L, error = 0L, pendiente = 0L, congelado = 0L,
    total = 0L, by_estado = list()
  )
  if (!admin_tabla_existe("ficha_del_mirador")) return(empty)
  df <- tryCatch(
    db_query("SELECT estado, COUNT(*)::int AS n FROM ficha_del_mirador GROUP BY 1"),
    error = function(e) data.frame()
  )
  counts <- list(activo = 0L, error = 0L, pendiente = 0L, congelado = 0L)
  by <- list()
  total <- 0L
  if (nrow(df) > 0L) {
    for (i in seq_len(nrow(df))) {
      st <- as.character(df$estado[i])
      n <- as.integer(df$n[i])
      if (!nzchar(st) || is.na(n)) next
      by[[st]] <- n
      total <- total + n
      if (st %in% names(counts)) counts[[st]] <- n
    }
  }
  list(
    disponible = TRUE,
    activo = counts$activo,
    error = counts$error,
    pendiente = counts$pendiente,
    congelado = counts$congelado,
    total = total,
    by_estado = by
  )
}

parse_live_ts <- function(x) {
  na <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  if (is.null(x) || length(x) == 0L) return(na)
  if (inherits(x, "POSIXct")) {
    if (length(x) < 1L || is.na(x[1])) return(na)
    return(x[1])
  }
  s <- as.character(x)[1]
  if (!nzchar(s) || is.na(s)) return(na)
  # as.POSIXct() sin format se queda en la fecha (medianoche) y no es NA.
  # PowerShell ToString('o'): 2026-10-09T11:24:02.2046297-03:00
  s <- sub("\\.\\d+", "", s)
  s <- sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", s)
  s <- sub("Z$", "+0000", s, ignore.case = TRUE)
  ts <- suppressWarnings(as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"))
  if (length(ts) != 1L || is.na(ts)) {
    ts <- suppressWarnings(as.POSIXct(s, format = "%Y-%m-%d %H:%M:%S%z", tz = "UTC"))
  }
  if (length(ts) != 1L || is.na(ts)) {
    ts <- suppressWarnings(as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  }
  if (length(ts) != 1L || is.na(ts)) return(na)
  ts
}

age_min_from_ts <- function(ts) {
  if (is.null(ts) || length(ts) == 0L || is.na(ts[1])) return(NULL)
  as.numeric(difftime(Sys.time(), ts[1], units = "mins"))
}

# Últimas N líneas no vacías leyendo cola del archivo (máx ~48KB desde el final).
read_log_last_lines <- function(path, n = 5L, max_bytes = 49152L) {
  n <- max(1L, as.integer(n))
  if (!file.exists(path)) return(character(0))
  sz <- tryCatch(file.info(path)$size, error = function(e) NA_real_)
  if (is.na(sz) || sz <= 0) return(character(0))
  con <- tryCatch(file(path, "rb"), error = function(e) NULL)
  if (is.null(con)) return(character(0))
  on.exit(close(con), add = TRUE)
  start <- max(0, as.integer(sz) - as.integer(max_bytes))
  tryCatch(seek(con, start, origin = "start"), error = function(e) NULL)
  raw <- tryCatch(readBin(con, "raw", n = as.integer(max_bytes) + 1L), error = function(e) raw(0))
  if (length(raw) == 0L) return(character(0))
  txt <- rawToChar(raw, multiple = FALSE)
  if (start > 0L) {
    nl <- regexpr("\n", txt, fixed = TRUE)[1]
    if (!is.na(nl) && nl > 0L) txt <- substring(txt, nl + 1L)
  }
  lines <- strsplit(txt, "\r?\n", perl = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L) return(character(0))
  tail(lines, n)
}

# Última línea no vacía leyendo cola del archivo (máx ~32KB desde el final).
read_log_last_line <- function(path, max_bytes = 32768L) {
  lines <- read_log_last_lines(path, n = 1L, max_bytes = max_bytes)
  if (length(lines) == 0L) return(NULL)
  lines[[1]]
}

pick_useful_log_lines <- function(lines, n = 5L) {
  lines <- trimws(as.character(lines))
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L) return(character(0))
  prefer <- grepl("estacion|\\[dga|\\[dmc|PASO |ingest|alertas|pronostico|Open-Meteo|bias-|coord |verif-hc",
                  lines, ignore.case = TRUE, perl = TRUE)
  src <- if (any(prefer)) lines[prefer] else lines
  tail(src, max(1L, as.integer(n)))
}

pick_useful_log_line <- function(lines) {
  out <- pick_useful_log_lines(lines, 1L)
  if (length(out) == 0L) return(NULL)
  out[[1]]
}

# Log con mtime más reciente del paso en curso. En histórico OM incluye las
# terminales internas docker_bounded_* (download/calc/ráfaga/precisión): el
# redirect del orquestador va ~1 min atrás.
freshest_step_log <- function(logs_dir, step, max_age_min = 20) {
  step <- as.character(step %||% "")
  if (!nzchar(step) || step %in% c("ingest", "adopt_ingest", "ingest_done")) return(NULL)
  day <- format(Sys.Date(), "%Y%m%d")
  paths <- c(
    file.path(logs_dir, sprintf("docker_%s_%s.err.log", step, day)),
    file.path(logs_dir, sprintf("docker_%s_%s.out.log", step, day)),
    file.path(logs_dir, sprintf("%s_%s.log", step, day)),
    file.path(logs_dir, sprintf("actualizar_diario_%s.log", day))
  )
  if (identical(step, "bias_historico")) {
    bounded <- list.files(
      logs_dir,
      pattern = paste0("^docker_bounded_.+_", day, "\\.(err|out)\\.log$"),
      full.names = TRUE
    )
    paths <- c(
      paths,
      bounded,
      file.path(logs_dir, sprintf("pronostico_precision_%s.log", day))
    )
  }
  paths <- unique(paths[nzchar(paths)])
  hits <- list()
  for (p in paths) {
    if (!file.exists(p)) next
    mt <- file.info(p)$mtime
    if (length(mt) != 1L || is.na(mt)) next
    age <- as.numeric(difftime(Sys.time(), mt, units = "mins"))
    if (!is.finite(age) || age > max_age_min) next
    lines <- pick_useful_log_lines(read_log_last_lines(p, n = 40L), 5L)
    if (!length(lines)) next
    hits[[length(hits) + 1L]] <- list(
      lines = lines,
      mtime = mt,
      source = basename(p),
      step = step,
      bounded = grepl("^docker_bounded_", basename(p))
    )
  }
  if (!length(hits)) return(NULL)
  newest <- max(vapply(hits, function(h) as.numeric(h$mtime), numeric(1)))
  # El orquestador copia la terminal ~1 min después. Si la terminal interna
  # sigue fresca, esa es la que está corriendo.
  bounded <- Filter(function(h) {
    isTRUE(h$bounded) && (newest - as.numeric(h$mtime)) <= 180
  }, hits)
  pool <- if (length(bounded)) bounded else hits
  best <- NULL
  best_m <- -Inf
  for (h in pool) {
    if (as.numeric(h$mtime) <= best_m) next
    best_m <- as.numeric(h$mtime)
    best <- h
  }
  best
}

resolve_live_log_tail <- function(logs_dir, running, ckpt = list()) {
  if (!isTRUE(running)) return(NULL)
  ckpt_path <- file.path(logs_dir, "actualizar_diario.checkpoint")
  ckpt_mtime <- if (file.exists(ckpt_path)) file.info(ckpt_path)$mtime else NA

  pack_tail <- function(lines, ts = NULL, source = NULL, container_id = NULL, step = NULL) {
    lines <- as.character(lines)
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    if (length(lines) == 0L) return(NULL)
    if (length(lines) > 5L) lines <- tail(lines, 5L)
    list(
      line = lines[[length(lines)]],
      # character vector (no as.list): evita [["x"]] con Plumber/jsonlite
      lines = unname(lines),
      ts = ts,
      source = source,
      container_id = container_id,
      step = step
    )
  }

  synthesize_ckpt <- function() {
    fu <- as.character(ckpt[["fuente"]] %||% "")
    va <- as.character(ckpt[["variable"]] %||% "")
    if (!(nzchar(fu) || nzchar(va))) return(NULL)
    line <- sprintf(
      "[%s/%s] estacion %s %s",
      fu, va,
      as.character(ckpt[["estacion_idx"]] %||% ""),
      as.character(ckpt[["estacion_codigo"]] %||% "")
    )
    pack_tail(
      trimws(line),
      ts = if (!is.na(ckpt_mtime)) format(ckpt_mtime, "%Y-%m-%dT%H:%M:%S%z") else NULL,
      source = "checkpoint"
    )
  }

  read_from_logs <- function() {
    day <- format(Sys.Date(), "%Y%m%d")
    candidates <- c(
      file.path(logs_dir, sprintf("docker_ingest_%s.err.log", day)),
      file.path(logs_dir, sprintf("docker_ingest_%s.out.log", day)),
      file.path(logs_dir, sprintf("actualizar_diario_%s.log", day))
    )
    infos <- lapply(candidates, function(p) {
      if (!file.exists(p)) return(NULL)
      list(path = p, mtime = file.info(p)$mtime)
    })
    infos <- Filter(Negate(is.null), infos)
    if (length(infos) == 0L) return(NULL)
    infos <- infos[order(vapply(infos, function(x) as.numeric(x$mtime), numeric(1)), decreasing = TRUE)]
    for (inf in infos) {
      age_m <- as.numeric(difftime(Sys.time(), inf$mtime, units = "mins"))
      # Host err/out dejan de actualizarse si murió el orquestador; no usar basura vieja.
      if (is.finite(age_m) && age_m > 15) next
      # Checkpoint más fresco que el archivo → el log no refleja el scrape vivo.
      if (!is.na(ckpt_mtime) && inf$mtime < ckpt_mtime) {
        lag_m <- as.numeric(difftime(ckpt_mtime, inf$mtime, units = "mins"))
        if (is.finite(lag_m) && lag_m > 2) next
      }
      lines <- read_log_last_lines(inf$path, n = 40L)
      lines <- pick_useful_log_lines(lines, 5L)
      if (length(lines)) {
        return(pack_tail(
          lines,
          ts = format(inf$mtime, "%Y-%m-%dT%H:%M:%S%z"),
          source = basename(inf$path)
        ))
      }
    }
    NULL
  }

  # Logs frescos ganan; si solo hay log viejo / orch muerto → checkpoint.
  prefer_logs_if_richer <- function(fallback) {
    from_logs <- read_from_logs()
    if (is.null(from_logs)) return(fallback)
    if (!is.null(fallback) && !is.na(ckpt_mtime)) {
      log_ts <- parse_live_ts(from_logs$ts)
      if (is.na(log_ts) || ckpt_mtime > log_ts) return(fallback)
    }
    n_log <- length(from_logs$lines)
    n_fb <- if (is.null(fallback)) 0L else length(fallback$lines)
    if (n_log >= 2L || n_fb == 0L) return(from_logs)
    fallback
  }

  step <- as.character(ckpt[["pipeline_step"]] %||% "")
  ingestish <- !nzchar(step) || step %in% c("ingest", "adopt_ingest")

  json_packed <- NULL
  tail_path <- file.path(logs_dir, "pipeline_live_tail.json")
  raw <- read_json_file_safe(tail_path)
  if (!is.null(raw)) {
    raw_lines <- raw$lines
    if (is.null(raw_lines) || !length(raw_lines)) {
      one <- as.character(raw$line %||% "")
      raw_lines <- if (nzchar(one)) one else character(0)
    }
    lines_chr <- as.character(unlist(raw_lines, use.names = FALSE))
    lines_chr <- trimws(lines_chr)
    lines_chr <- lines_chr[nzchar(lines_chr)]
    if (length(lines_chr) && !identical(as.character(raw$source %||% ""), "cleared")) {
      json_packed <- pack_tail(
        lines_chr,
        ts = raw$ts %||% NULL,
        source = raw$source %||% "pipeline_live_tail",
        container_id = raw$container_id %||% NULL,
        step = raw$step %||% NULL
      )
    }
  }

  # Fuera del scrape: la terminal que se escribió al último, no la estación del checkpoint.
  if (!ingestish) {
    fresh <- freshest_step_log(logs_dir, step)
    if (!is.null(fresh)) {
      return(pack_tail(
        fresh$lines,
        ts = format(fresh$mtime, "%Y-%m-%dT%H:%M:%S%z"),
        source = fresh$source,
        step = fresh$step
      ))
    }
    if (!is.null(json_packed)) return(json_packed)
    return(NULL)
  }

  if (!is.null(json_packed)) {
    json_ts <- parse_live_ts(json_packed$ts)
    if (!is.na(ckpt_mtime) && (is.na(json_ts) || ckpt_mtime > json_ts)) {
      # checkpoint más fresco: enriquecer desde logs (o 1 línea de ckpt)
      return(prefer_logs_if_richer(synthesize_ckpt()))
    }
    if (length(json_packed$lines) < 2L) {
      return(prefer_logs_if_richer(json_packed))
    }
    return(json_packed)
  }
  prefer_logs_if_richer(synthesize_ckpt())
}

# Quién está realmente en curso: lock/ckpt > status (status puede ser un skip reciente).
resolve_live_active <- function(status, lock, ckpt) {
  status_skipped <- isTRUE(status$skipped) ||
    identical(as.character(status$mode %||% ""), "skip")
  lock_task <- if (!is.null(lock)) as.character(lock$task_name %||% "") else ""
  lock_mode <- if (!is.null(lock)) as.character(lock$mode %||% "") else ""
  st_task <- if (!is.null(status)) as.character(status$task_name %||% "") else ""
  st_mode <- if (!is.null(status)) as.character(status$mode %||% "") else ""
  step <- as.character(ckpt[["pipeline_step"]] %||% "")
  phase_lock <- if (!is.null(lock)) as.character(lock$phase %||% "") else ""
  phase_st <- if (!is.null(status)) as.character(status$phase %||% "") else ""

  task_name <- NULL
  mode <- NULL
  source <- "unknown"

  if (nzchar(lock_task)) {
    task_name <- lock_task
    mode <- if (nzchar(lock_mode)) lock_mode else NULL
    source <- "lock"
  } else if (nzchar(lock_mode)) {
    mode <- lock_mode
    source <- "lock_mode"
  } else if (identical(step, "ingest") || identical(phase_lock, "adopt_ingest") ||
             nzchar(as.character(ckpt[["fuente"]] %||% ""))) {
    # Ingest en curso: atribuir al pipeline diario (full), no al split Scrape
    task_name <- if (nzchar(st_task)) st_task else "Rodaja_Actualizar_Diario"
    mode <- if (nzchar(st_mode)) st_mode else "full"
    source <- "checkpoint"
  } else if (!status_skipped && nzchar(st_task)) {
    task_name <- st_task
    mode <- if (nzchar(st_mode)) st_mode else NULL
    source <- "status"
  } else if (!status_skipped && nzchar(st_mode) && st_mode != "skip") {
    mode <- st_mode
    source <- "status_mode"
  }

  if (is.null(task_name) && !is.null(mode)) {
    task_name <- switch(
      mode,
      scrape = "Rodaja_Ingest_Scrape",
      post = "Rodaja_Ingest_Post",
      resume = "Rodaja_Actualizar_Resume",
      watchdog = "Rodaja_Actualizar_Resume",
      full = "Rodaja_Actualizar_Diario",
      panorama = "Rodaja_Panorama_ICON",
      backfill = "Rodaja_Backfill_BBDD",
      seo = "Rodaja_Generar_SEO",
      NULL
    )
  }
  if (is.null(mode) && !is.null(task_name)) {
    mode <- switch(
      task_name,
      Rodaja_Ingest_Scrape = "scrape",
      Rodaja_Ingest_Post = "post",
      Rodaja_Actualizar_Resume = "watchdog",
      Rodaja_Actualizar_Diario = "full",
      Rodaja_Panorama_ICON = "panorama",
      Rodaja_Backfill_BBDD = "backfill",
      Rodaja_Generar_SEO = "seo",
      NULL
    )
  }

  label <- NULL
  for (t in PIPELINE_TASK_CATALOG) {
    if (identical(t$task_name, task_name)) {
      label <- t$label
      break
    }
  }
  if (is.null(label) && !is.null(task_name)) label <- task_name

  phase <- if (nzchar(phase_lock)) phase_lock else if (!status_skipped && nzchar(phase_st)) phase_st else step
  if (!nzchar(as.character(phase %||% ""))) phase <- NULL

  list(
    task_name = task_name,
    mode = mode,
    label = label,
    phase = phase,
    source = source,
    status_skipped = status_skipped
  )
}

admin_pipelines_live <- function() {
  logs <- ingest_logs_dir()
  status_path <- file.path(logs, "actualizar_diario.status.json")
  lock_path <- file.path(logs, "actualizar_diario.lock")
  ckpt_path <- file.path(logs, "actualizar_diario.checkpoint")
  status <- read_json_file_safe(status_path)
  lock <- read_json_file_safe(lock_path)
  ckpt <- read_ckpt_kv(ckpt_path)
  bias_st <- read_json_file_safe(file.path(logs, "bias_historico.status.json"))
  prec_st <- read_json_file_safe(file.path(logs, "pronostico_precision.status.json"))

  running <- FALSE
  stale <- FALSE
  heartbeat_age_min <- NULL
  progress <- NULL

  status_phase <- as.character(status$phase %||% "")
  status_skipped <- isTRUE(status$skipped) || identical(as.character(status$mode %||% ""), "skip")
  has_lock <- !is.null(lock)
  status_running <- identical(status_phase, "running") && !status_skipped
  if (has_lock || status_running) {
    running <- TRUE
  }

  consider_age <- function(age) {
    if (is.null(age) || !is.finite(age)) return()
    if (is.null(heartbeat_age_min) || age < heartbeat_age_min) heartbeat_age_min <<- age
  }

  if (has_lock && !is.null(lock$ts)) {
    consider_age(age_min_from_ts(parse_live_ts(lock$ts)))
  }
  lp <- ckpt[["last_progress_at"]] %||% ""
  if (nzchar(lp)) {
    consider_age(age_min_from_ts(parse_live_ts(lp)))
  }
  # mtime del checkpoint = señal de avance aunque falte last_progress_at
  if (file.exists(ckpt_path)) {
    consider_age(age_min_from_ts(file.info(ckpt_path)$mtime))
  }
  # live_tail / log diario frescos cuentan como heartbeat (pasos post-ingest)
  tail_path <- file.path(logs, "pipeline_live_tail.json")
  if (file.exists(tail_path)) {
    consider_age(age_min_from_ts(file.info(tail_path)$mtime))
  }
  day_log <- file.path(logs, sprintf("actualizar_diario_%s.log", format(Sys.Date(), "%Y%m%d")))
  if (file.exists(day_log)) {
    consider_age(age_min_from_ts(file.info(day_log)$mtime))
  }

  step_ckpt <- as.character(ckpt[["pipeline_step"]] %||% "")
  # Sin lock y status ya done → no hay corrida viva (mtime ckpt no debe reabrir banner)
  if (identical(status_phase, "done") && !has_lock) {
    running <- FALSE
  }
  # Lock/status huérfanos: heartbeat viejo, o R terminó ingest y el orquestador no cerró
  if (!is.null(heartbeat_age_min) && is.finite(heartbeat_age_min)) {
    if (heartbeat_age_min > 60) stale <- TRUE
    if (heartbeat_age_min > 90) running <- FALSE
    if (identical(step_ckpt, "ingest_done") && heartbeat_age_min > 20) {
      stale <- TRUE
      running <- FALSE
    }
  }

  pipeline_done <- character(0)
  raw_done <- as.character(ckpt[["pipeline_done"]] %||% "")
  if (nzchar(raw_done)) {
    pipeline_done <- trimws(strsplit(raw_done, ",", fixed = TRUE)[[1]])
    pipeline_done <- pipeline_done[nzchar(pipeline_done)]
  }

  if (length(ckpt) > 0L) {
    # La estación del checkpoint es del scrape. En pasos posteriores el banner
    # se queda en dga/t_min #720 aunque ya corra histórico OM.
    scrape_live <- step_ckpt %in% c("ingest", "adopt_ingest") ||
      grepl("^gapfill", step_ckpt, ignore.case = TRUE)
    progress <- list(
      fuente = if (scrape_live) ckpt[["fuente"]] %||% NULL else NULL,
      variable = if (scrape_live) ckpt[["variable"]] %||% NULL else NULL,
      estacion_idx = if (scrape_live) ckpt[["estacion_idx"]] %||% NULL else NULL,
      estacion_codigo = if (scrape_live) ckpt[["estacion_codigo"]] %||% NULL else NULL,
      completed_vars = ckpt[["completed_vars"]] %||% NULL,
      fecha_fin = ckpt[["fecha_fin"]] %||% NULL,
      last_progress_at = ckpt[["last_progress_at"]] %||% NULL,
      pipeline_step = ckpt[["pipeline_step"]] %||% NULL,
      pipeline_done = if (length(pipeline_done)) pipeline_done else NULL
    )
  }

  active <- if (isTRUE(running)) resolve_live_active(status, lock, ckpt) else NULL
  log_tail <- resolve_live_log_tail(logs, running, ckpt)

  # Paso en curso para el hub (lock.phase preferido; excluir ya hechos)
  current_step <- NULL
  if (isTRUE(running)) {
    ph <- if (!is.null(active)) as.character(active$phase %||% "") else ""
    if (identical(ph, "adopt_ingest")) {
      current_step <- "ingest"
    } else if (nzchar(ph) && !(ph %in% c("orchestrator", "running", "done")) &&
               !(ph %in% pipeline_done)) {
      current_step <- ph
    } else if (nzchar(step_ckpt) && !identical(step_ckpt, "ingest_done") &&
               !(step_ckpt %in% pipeline_done)) {
      current_step <- step_ckpt
    } else if (!is.null(log_tail) && nzchar(as.character(log_tail$step %||% "")) &&
               !(as.character(log_tail$step) %in% pipeline_done)) {
      current_step <- as.character(log_tail$step)
    }
  }

  # Snapshot gapfill para barras del Hub (poll live, cache corto)
  gapfill_live <- NULL
  tryCatch({
    day_tag <- format(Sys.Date(), "%Y%m%d")
    dh <- read_json_file_safe(file.path(logs, sprintf("pipeline_day_health_%s.json", day_tag)))
    inc <- character(0)
    if (!is.null(dh$ingest$incomplete_vars)) {
      inc <- as.character(unlist(dh$ingest$incomplete_vars))
    }
    raw_cv <- as.character(ckpt[["completed_vars"]] %||% "")
    if (nzchar(raw_cv)) {
      done_v <- trimws(strsplit(raw_cv, ",", fixed = TRUE)[[1]])
      expected <- ADMIN_EXPECTED_INGEST_VARS
      if (!is.null(dh$ingest$expected_vars)) {
        expected <- as.character(unlist(dh$ingest$expected_vars))
      }
      expected <- expected[nzchar(expected)]
      if (length(done_v) > 0L) inc <- setdiff(expected, done_v)
    }
    inc <- inc[nzchar(inc)]
    gap_done <- isTRUE(dh$ingest$gapfill_completed) || length(inc) == 0L
    fecha_fin <- ckpt[["fecha_fin"]] %||% format(Sys.Date(), "%Y-%m-%d")
    meta <- admin_gapfill_pares_meta(
      incomplete_vars = inc,
      fecha_fin = fecha_fin,
      day_tag = day_tag,
      gapfill_completed = gap_done,
      ttl_sec = if (isTRUE(running)) 25 else 60
    )
    idx <- suppressWarnings(as.integer(ckpt[["estacion_idx"]] %||% NA))[1]
    if (length(idx) != 1L || is.na(idx) || idx < 0L) idx <- NA_integer_
    n_tot <- suppressWarnings(as.integer(meta$n_pares_ingest %||% 0L))[1]
    if (length(n_tot) != 1L || is.na(n_tot) || n_tot < 0L) n_tot <- 0L
    # Pasada actual (estaciones scrapadas) vs huecos ya cerrados en BD
    pass_pct <- if (!is.na(idx) && n_tot > 0L) {
      as.integer(max(0L, min(100L, round(100 * min(idx, n_tot) / n_tot))))
    } else {
      NULL
    }
    closed_pct <- suppressWarnings(as.integer(meta$pct %||% NA))[1]
    if (length(closed_pct) != 1L || is.na(closed_pct)) closed_pct <- 0L
    show_pct <- if (!is.null(pass_pct) && isTRUE(running)) {
      max(closed_pct, pass_pct)
    } else {
      closed_pct
    }
    lbl <- as.character(meta$label %||% "gapfill")[1]
    if (!is.na(idx) && n_tot > 0L && isTRUE(running)) {
      lbl <- sprintf("est %d/%d · %s", idx, n_tot, lbl)
    }
    # Descargar % (pares totales) ≠ gapfill % (solo huecos)
    expected_live <- ADMIN_EXPECTED_INGEST_VARS
    if (!is.null(dh$ingest$expected_vars)) {
      expected_live <- as.character(unlist(dh$ingest$expected_vars))
    }
    expected_live <- expected_live[nzchar(expected_live)]
    file_huecos_live <- admin_read_gapfill_huecos_file(day_tag, fecha_fin)
    est_dga_live <- suppressWarnings(as.integer(dh$ingest$estaciones_dga %||% NA)[1])
    est_dmc_live <- suppressWarnings(as.integer(dh$ingest$estaciones_dmc %||% NA)[1])
    if (length(est_dga_live) != 1L || is.na(est_dga_live)) est_dga_live <- NULL
    if (length(est_dmc_live) != 1L || is.na(est_dmc_live)) est_dmc_live <- NULL
    n_pares_total_live <- admin_ingest_pares_total(
      expected_live, file_huecos_live, est_dga_live, est_dmc_live
    )
    n_rem_live <- if (isTRUE(gap_done) || length(inc) == 0L) {
      0L
    } else {
      as.integer(meta$n_pares_gapfill %||% 0L)
    }
    ingest_pct_live <- admin_ingest_pct_pares(n_pares_total_live, n_rem_live)
    if (is.null(ingest_pct_live)) {
      # Fallback: base var-ratio + blend con avance gapfill (no resetear a 0)
      done_v <- character(0)
      raw_cv2 <- as.character(ckpt[["completed_vars"]] %||% "")
      if (nzchar(raw_cv2)) done_v <- trimws(strsplit(raw_cv2, ",", fixed = TRUE)[[1]])
      if (!is.null(dh$ingest$completed_vars)) {
        cv_dh <- as.character(unlist(dh$ingest$completed_vars))
        if (length(cv_dh) > length(done_v)) done_v <- cv_dh
      }
      done_v <- done_v[nzchar(done_v)]
      base_pct <- if (length(expected_live) > 0L) {
        as.integer(round(100 * length(intersect(done_v, expected_live)) / length(expected_live)))
      } else {
        0L
      }
      if (isTRUE(gap_done)) {
        ingest_pct_live <- 100L
      } else if (n_tot > 0L) {
        ingest_pct_live <- as.integer(max(
          base_pct,
          min(100L, round(base_pct + (100 - base_pct) * show_pct / 100))
        ))
      } else {
        ingest_pct_live <- base_pct
      }
    }
    gapfill_live <- list(
      n_pares_ingest = as.integer(meta$n_pares_ingest %||% 0L),
      n_pares_gapfill = as.integer(meta$n_pares_gapfill %||% 0L),
      n_pares_total = if (n_pares_total_live > 0L) as.integer(n_pares_total_live) else NULL,
      n_done = as.integer(meta$n_done %||% 0L),
      pct = as.integer(show_pct),
      pct_closed = as.integer(closed_pct),
      pass_pct = pass_pct,
      ingest_pct = as.integer(ingest_pct_live),
      label = lbl,
      pending_vars = as.character(inc),
      completed = isTRUE(gap_done),
      fuente = ckpt[["fuente"]] %||% NULL,
      variable = ckpt[["variable"]] %||% NULL,
      estacion_idx = if (!is.na(idx)) as.integer(idx) else NULL,
      estacion_total = if (n_tot > 0L) as.integer(n_tot) else NULL,
      estacion_codigo = ckpt[["estacion_codigo"]] %||% NULL
    )
  }, error = function(e) NULL)

  # Panorama ICON: lock propio. No pisa el live del Diario si ese ya corre.
  panorama_running <- FALSE
  pn_lock <- read_json_file_safe(file.path(logs, "panorama_icon.lock"))
  if (!is.null(pn_lock)) {
    panorama_running <- TRUE
    pn_age <- NULL
    if (!is.null(pn_lock$ts)) pn_age <- age_min_from_ts(parse_live_ts(pn_lock$ts))
    if (!is.null(pn_age) && is.finite(pn_age) && pn_age > 90) panorama_running <- FALSE
    if (isTRUE(panorama_running) && !isTRUE(running)) {
      running <- TRUE
      consider_age(pn_age)
      active <- list(
        task_name = "Rodaja_Panorama_ICON",
        mode = "panorama",
        label = "Panorama ICON",
        phase = "running",
        source = "panorama_lock",
        status_skipped = FALSE
      )
      current_step <- "download"
      pn_log <- file.path(logs, sprintf("panorama_icon_%s.log", format(Sys.Date(), "%Y%m%d")))
      if (file.exists(pn_log)) {
        lines <- read_log_last_lines(pn_log, 5L)
        if (length(lines) > 0L) {
          log_tail <- list(source = basename(pn_log), lines = as.list(lines), step = "download")
        }
      }
    }
  }

  bf_stats <- tryCatch(admin_backfill_bbdd_stats(), error = function(e) NULL)
  dga_stats <- tryCatch(admin_backfill_dga_stats(), error = function(e) NULL)
  u10h_stats <- tryCatch(admin_backfill_u10h_stats(), error = function(e) NULL)
  dga_raf_stats <- tryCatch(admin_backfill_dga_rafaga_stats(), error = function(e) NULL)
  backfill_running <- isTRUE(bf_stats$running)
  backfill_dga_running <- isTRUE(dga_stats$running)
  backfill_dga_rafaga_running <- isTRUE(dga_raf_stats$running)
  if (isTRUE(backfill_running) && !isTRUE(running)) {
    running <- TRUE
    consider_age(age_min_from_ts(parse_live_ts(bf_stats$updated_at)))
    active <- list(
      task_name = "Rodaja_Backfill_BBDD",
      mode = "backfill",
      label = "backfill BBDD",
      phase = "backfill",
      source = "backfill_lock",
      status_skipped = FALSE
    )
    current_step <- "backfill"
    bf_log <- file.path(logs, sprintf("backfill_bbdd_%s.log", format(Sys.Date(), "%Y%m%d")))
    if (file.exists(bf_log)) {
      lines <- read_log_last_lines(bf_log, 5L)
      if (length(lines) > 0L) {
        log_tail <- list(source = basename(bf_log), lines = as.list(lines), step = "backfill")
      }
    }
  }

  if (isTRUE(backfill_dga_running) && !isTRUE(running)) {
    running <- TRUE
    consider_age(age_min_from_ts(parse_live_ts(dga_stats$updated_at)))
    active <- list(
      task_name = "Rodaja_Backfill_DGA_Dire",
      mode = "backfill_dga",
      label = "direccion viento DGA",
      phase = "backfill",
      source = "backfill_dga_lock",
      status_skipped = FALSE
    )
    current_step <- "backfill"
    dga_log <- file.path(logs, sprintf("backfill_u2_dire_%s.log", format(Sys.Date(), "%Y%m%d")))
    if (file.exists(dga_log)) {
      lines <- read_log_last_lines(dga_log, 5L)
      if (length(lines) > 0L) {
        log_tail <- list(source = basename(dga_log), lines = as.list(lines), step = "backfill")
      }
    }
  }

  if (isTRUE(backfill_dga_rafaga_running) && !isTRUE(running)) {
    running <- TRUE
    consider_age(age_min_from_ts(parse_live_ts(dga_raf_stats$updated_at)))
    active <- list(
      task_name = "Rodaja_Backfill_DGA_Rafaga",
      mode = "backfill_dga_rafaga",
      label = "rafaga horaria DGA",
      phase = "backfill",
      source = "backfill_dga_rafaga_lock",
      status_skipped = FALSE
    )
    current_step <- "backfill"
    raf_log <- file.path(logs, sprintf("backfill_dga_rafaga_%s.log", format(Sys.Date(), "%Y%m%d")))
    if (file.exists(raf_log)) {
      lines <- read_log_last_lines(raf_log, 5L)
      if (length(lines) > 0L) {
        log_tail <- list(source = basename(raf_log), lines = as.list(lines), step = "backfill")
      }
    }
  }

  seo_lock <- read_json_file_safe(file.path(logs, "generar_seo.lock"))
  seo_running <- FALSE
  if (!is.null(seo_lock)) {
    seo_running <- TRUE
    seo_age <- NULL
    if (!is.null(seo_lock$ts)) seo_age <- age_min_from_ts(parse_live_ts(seo_lock$ts))
    if (!is.null(seo_age) && is.finite(seo_age) && seo_age > 90) seo_running <- FALSE
    if (isTRUE(seo_running) && !isTRUE(running)) {
      running <- TRUE
      consider_age(seo_age)
      active <- list(
        task_name = "Rodaja_Generar_SEO",
        mode = "seo",
        label = "SEO / IA",
        phase = "running",
        source = "seo_lock",
        status_skipped = FALSE
      )
      current_step <- "generar_seo"
      seo_log <- file.path(logs, sprintf("generar_seo_%s.log", format(Sys.Date(), "%Y%m%d")))
      if (file.exists(seo_log)) {
        lines <- read_log_last_lines(seo_log, 5L)
        if (length(lines) > 0L) {
          log_tail <- list(source = basename(seo_log), lines = as.list(lines), step = "generar_seo")
        }
      }
    }
  }

  # stale se calculo antes de que la tarea viva achicara la edad. Un log del
  # Diario quieto dejaba "sin avance >60 min" con heartbeat de 1 min.
  if (isTRUE(running) && !is.null(heartbeat_age_min) && is.finite(heartbeat_age_min)) {
    stale <- heartbeat_age_min > 60
  }

  list(
    ok = TRUE,
    running = isTRUE(running),
    stale = isTRUE(stale),
    heartbeat_age_min = if (is.null(heartbeat_age_min)) NULL else round(heartbeat_age_min, 1),
    active = active,
    current_step = current_step,
    pipeline_done = pipeline_done,
    log_tail = log_tail,
    status = status,
    lock = lock,
    progress = progress,
    gapfill = gapfill_live,
    bias_status = bias_st,
    precision_status = prec_st,
    panorama_running = isTRUE(panorama_running),
    backfill_running = isTRUE(backfill_running),
    backfill_dga_running = isTRUE(backfill_dga_running),
    backfill_dga_rafaga_running = isTRUE(backfill_dga_rafaga_running),
    seo_running = isTRUE(seo_running),
    backfill_bbdd = bf_stats,
    backfill_dga = dga_stats,
    backfill_dga_rafaga = dga_raf_stats,
    backfill_u10h = u10h_stats,
    logs_dir = logs
  )
}

admin_schtasks_health <- function() {
  path <- file.path(ingest_logs_dir(), "schtasks_health.json")
  raw <- read_json_file_safe(path)
  if (is.null(raw)) {
    return(list(
      ok = TRUE,
      disponible = FALSE,
      mensaje = "Sin schtasks_health.json — ejecute ingest/scripts/export_schtasks_health.ps1"
    ))
  }
  list(ok = TRUE, disponible = TRUE, path = path, data = raw)
}

#* Precision pronostico Open-Meteo vs observado (bias correction, mes=0 global)
#* @get /admin/pronostico/precision
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  if (!admin_tabla_existe("correccion_de_sesgo")) {
    return(list(ok = TRUE, disponible = FALSE,
                mensaje = "Tabla correccion_de_sesgo no existe"))
  }

  resumen <- admin_pronostico_resumen()
  apply_status <- admin_bias_apply_status()
  monitor <- admin_bias_monitor_rolling()
  por_lead <- admin_bias_por_lead()

  por_var <- db_query("
    SELECT v.nombre_abr, v.nombre,
      SUM(b.mae_antes * b.n_muestra) / NULLIF(SUM(b.n_muestra), 0) AS mae_antes,
      SUM(b.mae_despues * b.n_muestra) / NULLIF(SUM(b.n_muestra), 0) AS mae_despues,
      AVG(b.bias_antes) AS bias_medio,
      AVG(b.csi_1mm) AS csi_1mm,
      AVG(b.csi_10mm) AS csi_10mm,
      SUM(b.n_muestra)::int AS n_muestra,
      SUM(CASE WHEN b.confiable THEN 1 ELSE 0 END)::int AS n_confiable
    FROM correccion_de_sesgo b
    JOIN magnitud v ON v.id = b.id_variable
    WHERE b.mes = 0 AND COALESCE(b.lead_bin, 0) = 0 AND b.mae_antes IS NOT NULL
    GROUP BY v.id, v.nombre_abr, v.nombre
    ORDER BY v.nombre_abr")

  peores <- db_query("
    SELECT e.id AS id_estacion, e.nombre AS estacion_nombre, e.cod_reg,
           v.nombre_abr, b.mae_antes, b.mae_despues, b.bias_antes, b.n_muestra, b.confiable
    FROM correccion_de_sesgo b
    JOIN mirador e ON e.id = b.id_estacion
    JOIN magnitud v ON v.id = b.id_variable
    WHERE b.mes = 0 AND COALESCE(b.lead_bin, 0) = 0
      AND b.mae_despues IS NOT NULL AND b.confiable = TRUE
    ORDER BY b.mae_despues DESC
    LIMIT 15")

  mejores <- db_query("
    SELECT e.id AS id_estacion, e.nombre AS estacion_nombre, e.cod_reg,
           v.nombre_abr, b.mae_antes, b.mae_despues, b.bias_antes, b.n_muestra, b.confiable
    FROM correccion_de_sesgo b
    JOIN mirador e ON e.id = b.id_estacion
    JOIN magnitud v ON v.id = b.id_variable
    WHERE b.mes = 0 AND COALESCE(b.lead_bin, 0) = 0
      AND b.mae_despues IS NOT NULL AND b.confiable = TRUE AND b.n_muestra >= 30
    ORDER BY b.mae_despues ASC
    LIMIT 15")

  # MAE ponderado por comuna (mes=0); gid alineado con SVG /alertas/geo/comunas.svg
  por_com_df <- db_query("
    SELECT TRIM(e.nom_com) AS nom_com,
      SUM(b.mae_antes * b.n_muestra) / NULLIF(SUM(b.n_muestra), 0) AS mae_antes,
      SUM(b.mae_despues * b.n_muestra) / NULLIF(SUM(b.n_muestra), 0) AS mae_despues,
      SUM(b.n_muestra)::int AS n_muestra,
      COUNT(DISTINCT b.id_estacion)::int AS n_estaciones
    FROM correccion_de_sesgo b
    JOIN mirador e ON e.id = b.id_estacion
    WHERE b.mes = 0 AND COALESCE(b.lead_bin, 0) = 0 AND b.mae_antes IS NOT NULL
      AND e.nom_com IS NOT NULL AND TRIM(e.nom_com) <> ''
    GROUP BY TRIM(e.nom_com)
    ORDER BY mae_antes DESC")

  por_comuna <- list()
  if (nrow(por_com_df) > 0) {
    por_comuna <- lapply(seq_len(nrow(por_com_df)), function(i) {
      nom <- as.character(por_com_df$nom_com[i])
      list(
        id = paste0("com:", alerta_digest_normalizar_reg(nom)),
        etiqueta = nom,
        mae = round(as.numeric(por_com_df$mae_antes[i]), 3),
        mae_antes = round(as.numeric(por_com_df$mae_antes[i]), 3),
        mae_despues = if (is.na(por_com_df$mae_despues[i])) NULL else round(as.numeric(por_com_df$mae_despues[i]), 3),
        n_muestra = as.integer(por_com_df$n_muestra[i]),
        n_estaciones = as.integer(por_com_df$n_estaciones[i])
      )
    })
  }

  list(
    ok = TRUE,
    disponible = TRUE,
    resumen = resumen,
    apply_status = apply_status,
    monitor_rolling = monitor,
    por_lead = por_lead,
    por_variable = admin_df_rows(por_var),
    peores = admin_df_rows(peores),
    mejores = admin_df_rows(mejores),
    por_comuna = por_comuna,
    comunas_cobertura = digest_comunas_cobertura(),
    nota = paste0(
      "Coeficientes mes=0 lead_bin=0 del backfill hist\u00f3rico Open-Meteo vs jornada_cerrada. ",
      "Apply write-time: valor + valor_crudo. Train siempre sobre staging crudo. ",
      "TP de alertas = predijo entrada al top-10 y SI entro (no exige puesto exacto). ",
      "Impacto raw vs corr: GET /admin/pronostico/precision/alert-impact"
    )
  )
}

#* Impacto bias en alertas top-10: TP/FP crudo vs corregido (hindcast)
#* @get /admin/pronostico/precision/alert-impact
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  impact <- tryCatch(
    admin_bias_alert_impact(6L),
    error = function(e) list(disponible = FALSE, mensaje = conditionMessage(e))
  )
  list(ok = TRUE, alert_impact = impact)
}

#* Serie diaria precision/MAE Open-Meteo crudo vs corregido
#* @get /admin/pronostico/precision/serie-diaria
#* @param metric precision|mae|both (default both)
#* @param variable Abr (pp|t_max|t_min). Default pp
#* @param region cod_reg opcional
#* @param comuna nom_com exacto opcional
#* @param desde YYYY-MM-DD opcional (default hoy-60)
#* @param hasta YYYY-MM-DD opcional (default ayer)
function(req, res, metric = "both", variable = "pp", region = "", comuna = "",
         desde = "", hasta = "") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  tryCatch(
    admin_precision_serie_diaria(metric, variable, region, comuna, desde, hasta),
    error = function(e) list(
      ok = TRUE, disponible = FALSE, mensaje = conditionMessage(e), serie = list()
    )
  )
}

ingest_profiles_dir <- function() {
  file.path(ingest_logs_dir(), "profiles")
}

admin_profiler_read_summary <- function(run_id) {
  path <- file.path(ingest_profiles_dir(), sprintf("profile_%s.summary.json", run_id))
  read_json_file_safe(path)
}

# wall_ms del summary puede quedar en ~0 si el padre hizo fork (DGA∥DMC) y no reescribe summary.
admin_profiler_jsonl_max_end_ms <- function(run_id) {
  path <- file.path(ingest_profiles_dir(), sprintf("profile_%s.jsonl", run_id))
  if (!file.exists(path)) return(NULL)
  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  if (length(lines) == 0L) return(NULL)
  max_end <- 0
  for (ln in lines) {
    if (!nzchar(trimws(ln))) next
    ts_m <- regmatches(ln, regexpr('"ts"\\s*:\\s*[0-9.]+', ln, perl = TRUE))
    dur_m <- regmatches(ln, regexpr('"dur_ms"\\s*:\\s*[0-9.]+', ln, perl = TRUE))
    if (length(ts_m) == 0L) next
    ts <- suppressWarnings(as.numeric(sub(".*:", "", ts_m[[1]])))
    dur <- if (length(dur_m)) suppressWarnings(as.numeric(sub(".*:", "", dur_m[[1]]))) else 0
    if (!is.finite(ts)) next
    if (!is.finite(dur)) dur <- 0
    end <- ts + dur
    if (end > max_end) max_end <- end
  }
  if (max_end <= 0) return(NULL)
  round(max_end, 3)
}

admin_profiler_enrich_summary_wall <- function(raw, run_id = NULL) {
  if (is.null(raw)) return(NULL)
  rid <- as.character(raw$run_id %||% run_id %||% "")
  st <- as.character(raw$status %||% "")
  wall <- suppressWarnings(as.numeric(raw$wall_ms %||% 0))
  if (!is.finite(wall)) wall <- 0
  # Enrich si running o wall ridículamente chico vs corrida larga
  if (identical(st, "running") || identical(st, "en_curso") || wall < 60000) {
    mx <- tryCatch(admin_profiler_jsonl_max_end_ms(rid), error = function(e) NULL)
    if (!is.null(mx) && is.finite(mx) && mx > wall) {
      raw$wall_ms <- mx
      raw$wall_from_spans <- TRUE
    }
  }
  raw
}

# Reconciliar status del summary con convención Hub (ok/parcial/en_curso/error).
# Corridas viejas con status=done + Vigilancia incompleta → parcial vía PG metrics.
admin_profiler_normalize_outcome <- function(raw) {
  if (is.null(raw) || !is.list(raw)) return(raw)
  rid <- as.character(raw$run_id %||% "")[1]
  mode <- as.character(raw$mode %||% "")[1]
  tn <- as.character(raw$task_name %||% "")[1]
  st0 <- as.character(raw$status %||% "")[1]
  norm <- pipeline_outcome_normalize(st0)

  is_vig <- identical(mode, "watchdog") || identical(mode, "resume") ||
    grepl("Resume|Vigilancia|watchdog", tn, ignore.case = TRUE)

  if (is_vig && !identical(norm, "en_curso") && !identical(norm, "error") &&
      nzchar(rid) && admin_tabla_existe("parte_de_guardia")) {
    df <- tryCatch(db_query("
      SELECT exit_code, metrics, job_bat
      FROM parte_de_guardia
      WHERE run_id = $1
      ORDER BY started_at DESC
      LIMIT 1
    ", params = list(rid)), error = function(e) data.frame())
    if (nrow(df) > 0L) {
      m <- jsonb_as_list(df$metrics[[1]] %||% df$metrics[1])
      out <- pipeline_outcome_from_watchdog_metrics(m, df$exit_code[1])
      raw$status <- out
      raw$outcome <- out
      return(raw)
    }
  }

  if (is.na(norm)) {
    raw$status <- if (nzchar(st0)) st0 else "ok"
  } else {
    raw$status <- norm
  }
  raw$outcome <- raw$status
  raw
}

admin_profiler_list_runs <- function(limit = 30L) {
  d <- ingest_profiles_dir()
  if (!dir.exists(d)) {
    return(list(ok = TRUE, runs = list(), profiles_dir = d))
  }
  files <- list.files(d, pattern = "^profile_.*\\.summary\\.json$", full.names = TRUE)
  if (length(files) == 0L) {
    return(list(ok = TRUE, runs = list(), profiles_dir = d))
  }
  infos <- file.info(files)
  ord <- order(infos$mtime, decreasing = TRUE)
  files <- files[ord]
  limit <- max(1L, min(as.integer(limit), 100L))
  files <- head(files, limit)
  runs <- lapply(files, function(f) {
    raw <- read_json_file_safe(f)
    rid <- sub("^profile_", "", sub("\\.summary\\.json$", "", basename(f)))
    if (is.null(raw)) {
      return(list(run_id = rid, available = FALSE))
    }
    raw <- admin_profiler_normalize_outcome(raw)
    list(
      run_id = raw$run_id %||% rid,
      task_name = raw$task_name %||% NA_character_,
      mode = raw$mode %||% NA_character_,
      trigger_source = raw$trigger_source %||% "manual",
      status = raw$status %||% NA_character_,
      outcome = raw$outcome %||% raw$status %||% NA_character_,
      started_at = raw$started_at %||% NA_character_,
      finished_at = raw$finished_at,
      wall_ms = raw$wall_ms %||% 0,
      http_ms = raw$http_ms %||% 0,
      sleep_ms = raw$sleep_ms %||% 0,
      compute_ms = raw$compute_ms %||% 0,
      n_http = raw$n_http %||% 0,
      n_timeout = raw$n_timeout %||% 0
    )
  })
  list(ok = TRUE, runs = runs, profiles_dir = d)
}

admin_profiler_get_run <- function(run_id) {
  run_id <- as.character(run_id)
  raw <- admin_profiler_read_summary(run_id)
  if (is.null(raw)) {
    return(list(ok = TRUE, available = FALSE, run_id = run_id))
  }
  summ <- admin_profiler_normalize_outcome(
    admin_profiler_enrich_summary_wall(raw, run_id)
  )
  list(ok = TRUE, available = TRUE, summary = summ)
}

admin_profiler_get_spans <- function(run_id, parent = NULL, cat = NULL, limit = 500L) {
  run_id <- as.character(run_id)
  path <- file.path(ingest_profiles_dir(), sprintf("profile_%s.jsonl", run_id))
  if (!file.exists(path)) {
    return(list(ok = TRUE, available = FALSE, run_id = run_id, spans = list(), total = 0L))
  }
  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  if (length(lines) == 0L) {
    return(list(ok = TRUE, available = TRUE, run_id = run_id, spans = list(), total = 0L))
  }
  parent_f <- if (is.null(parent) || !nzchar(as.character(parent))) NULL else as.character(parent)
  cat_f <- if (is.null(cat) || !nzchar(as.character(cat))) NULL else as.character(cat)
  limit <- max(1L, min(as.integer(limit %||% 500L), 50000L))
  spans <- list()
  total_match <- 0L
  max_end_ms <- 0
  for (ln in lines) {
    if (!nzchar(trimws(ln))) next
    sp <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(sp)) next
    if (!is.null(parent_f)) {
      # parent filter: exact parent OR top-level when parent=__root__
      if (identical(parent_f, "__root__")) {
        if (!is.null(sp$parent) && !identical(sp$parent, NULL) && nzchar(as.character(sp$parent %||% ""))) next
      } else if (!identical(as.character(sp$parent %||% ""), parent_f)) {
        next
      }
    }
    if (!is.null(cat_f)) {
      cats <- strsplit(cat_f, ",", fixed = TRUE)[[1]]
      cats <- trimws(cats)
      if (!(as.character(sp$cat %||% "") %in% cats)) next
    }
    total_match <- total_match + 1L
    ts <- suppressWarnings(as.numeric(sp$ts %||% 0))
    dur <- suppressWarnings(as.numeric(sp$dur_ms %||% 0))
    if (is.finite(ts) && is.finite(dur)) {
      end <- ts + dur
      if (end > max_end_ms) max_end_ms <- end
    }
    if (length(spans) < limit) {
      spans[[length(spans) + 1L]] <- sp
    }
  }
  # Si hay demasiados spans, quedarse con los ÚLTIMOS (corrida larga) en vez de cortar al minuto 100.
  if (total_match > length(spans) && length(spans) > 0L) {
    # re-scan taking tail — más simple: índice desde el final
    need <- limit
    tail_spans <- list()
    for (i in seq.int(length(lines), 1L, by = -1L)) {
      ln <- lines[[i]]
      if (!nzchar(trimws(ln))) next
      sp <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE), error = function(e) NULL)
      if (is.null(sp)) next
      if (!is.null(parent_f)) {
        if (identical(parent_f, "__root__")) {
          if (!is.null(sp$parent) && !identical(sp$parent, NULL) && nzchar(as.character(sp$parent %||% ""))) next
        } else if (!identical(as.character(sp$parent %||% ""), parent_f)) {
          next
        }
      }
      if (!is.null(cat_f)) {
        cats <- strsplit(cat_f, ",", fixed = TRUE)[[1]]
        cats <- trimws(cats)
        if (!(as.character(sp$cat %||% "") %in% cats)) next
      }
      tail_spans[[length(tail_spans) + 1L]] <- sp
      if (length(tail_spans) >= need) break
    }
    spans <- rev(tail_spans)
  }
  list(
    ok = TRUE,
    available = TRUE,
    run_id = run_id,
    spans = spans,
    total = total_match,
    truncated = total_match > length(spans),
    max_end_ms = if (max_end_ms > 0) round(max_end_ms, 3) else NULL
  )
}

admin_profiler_live <- function() {
  listed <- admin_profiler_list_runs(20L)
  runs <- listed$runs %||% list()
  live <- NULL
  for (r in runs) {
    st <- as.character(r$status %||% "")
    if (st %in% c("running", "en_curso") || is.null(r$finished_at) || identical(r$finished_at, NULL)) {
      live <- r
      break
    }
  }
  if (is.null(live) && length(runs) > 0L) {
    live <- runs[[1]]
  }
  if (is.null(live)) {
    return(list(ok = TRUE, available = FALSE, running = FALSE))
  }
  rid <- as.character(live$run_id)
  summary <- admin_profiler_normalize_outcome(
    admin_profiler_enrich_summary_wall(admin_profiler_read_summary(rid), rid)
  )
  spans <- admin_profiler_get_spans(rid, parent = "__root__", cat = "step,fuente,watchdog", limit = 80L)
  st_live <- as.character(summary$status %||% live$status %||% "")
  list(
    ok = TRUE,
    available = TRUE,
    running = st_live %in% c("running", "en_curso"),
    run = live,
    summary = summary,
    spans = spans$spans %||% list()
  )
}

admin_profiler_history_path <- function() {
  file.path(ingest_profiles_dir(), "history.jsonl")
}

admin_profiler_read_history_raw <- function() {
  path <- admin_profiler_history_path()
  if (!file.exists(path)) return(list())
  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  out <- list()
  for (ln in lines) {
    if (!nzchar(trimws(ln))) next
    sp <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(sp) && !is.null(sp$run_id)) out[[length(out) + 1L]] <- sp
  }
  out
}

admin_profiler_summary_to_history <- function(raw, rid_fallback = NULL) {
  if (is.null(raw)) return(NULL)
  flatten_sums <- function(bucket) {
    out <- list()
    if (is.null(bucket) || !length(bucket)) return(out)
    nms <- names(bucket)
    if (is.null(nms)) return(out)
    for (nm in nms) {
      rec <- bucket[[nm]]
      if (is.list(rec) && !is.null(rec$sum_ms)) {
        out[[nm]] <- as.numeric(rec$sum_ms)
      }
    }
    out
  }
  steps_ms <- list()
  steps <- raw$steps %||% list()
  if (length(steps)) {
    for (i in seq_along(steps)) {
      st <- steps[[i]]
      sid <- as.character(st$id %||% st$name %||% paste0("step", i))
      if (!is.null(st$duration_sec)) {
        steps_ms[[sid]] <- as.numeric(st$duration_sec) * 1000
      } else if (!is.null(st$dur_ms)) {
        steps_ms[[sid]] <- as.numeric(st$dur_ms)
      }
    }
  }
  list(
    run_id = as.character(raw$run_id %||% rid_fallback %||% ""),
    task_name = as.character(raw$task_name %||% ""),
    mode = as.character(raw$mode %||% ""),
    trigger_source = as.character(raw$trigger_source %||% "manual"),
    started_at = raw$started_at,
    finished_at = raw$finished_at,
    status = as.character(raw$status %||% ""),
    wall_ms = as.numeric(raw$wall_ms %||% 0),
    http_ms = as.numeric(raw$http_ms %||% 0),
    sleep_ms = as.numeric(raw$sleep_ms %||% 0),
    compute_ms = as.numeric(raw$compute_ms %||% 0),
    n_http = as.integer(raw$n_http %||% 0L),
    n_timeout = as.integer(raw$n_timeout %||% 0L),
    fuentes = flatten_sums(raw$by_fuente),
    vars = flatten_sums(raw$by_var),
    steps = steps_ms
  )
}

admin_profiler_history_from_summaries <- function() {
  d <- ingest_profiles_dir()
  if (!dir.exists(d)) return(list())
  files <- list.files(d, pattern = "^profile_.*\\.summary\\.json$", full.names = TRUE)
  out <- list()
  for (f in files) {
    rid <- sub("^profile_", "", sub("\\.summary\\.json$", "", basename(f)))
    raw <- read_json_file_safe(f)
    snap <- admin_profiler_summary_to_history(raw, rid)
    if (is.null(snap) || !nzchar(snap$run_id)) next
    if (identical(snap$status, "running")) next
    out[[length(out) + 1L]] <- snap
  }
  out
}

admin_profiler_dedupe_history <- function(rows) {
  if (!length(rows)) return(list())
  by_id <- list()
  for (r in rows) {
    rid <- as.character(r$run_id %||% "")
    if (!nzchar(rid)) next
    by_id[[rid]] <- r
  }
  ids <- names(by_id)
  ord <- order(vapply(ids, function(id) {
    r <- by_id[[id]]
    as.character(r$started_at %||% r$finished_at %||% id)
  }, character(1)))
  lapply(ids[ord], function(id) by_id[[id]])
}

admin_profiler_mean <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  round(mean(x), 1)
}

admin_profiler_parse_day <- function(s) {
  s <- trimws(as.character(s %||% ""))
  if (!nzchar(s)) return(NULL)
  # YYYY-MM-DD
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", s)) return(NULL)
  as.Date(s)
}

admin_profiler_row_day <- function(r) {
  ts <- as.character(r$started_at %||% r$finished_at %||% "")
  if (!nzchar(ts)) return(NA)
  # Accept ISO with/without timezone; take date part in local-ish string
  day_s <- substr(gsub("T", " ", ts), 1, 10)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", day_s)) return(NA)
  as.Date(day_s)
}

admin_profiler_is_success <- function(status) {
  st <- tolower(trimws(as.character(status %||% "")))
  st %in% c("done", "ok", "success", "")
}

admin_profiler_parse_ts <- function(ts) {
  ts <- as.character(ts %||% "")
  if (!nzchar(ts)) return(NULL)
  # 2026-07-19T10:14:34.7152847-04:00 -> drop fractional seconds
  ts2 <- sub("\\.(\\d+)([Zz]|[+-]\\d{2}:?\\d{2})$", "\\2", ts)
  ts2 <- sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", ts2)
  parsed <- suppressWarnings(tryCatch(
    as.POSIXct(ts2, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    error = function(e) NULL
  ))
  if (is.null(parsed) || length(parsed) == 0L || is.na(parsed[[1]])) {
    daypart <- substr(gsub("T", " ", ts), 1, 19)
    parsed <- suppressWarnings(tryCatch(
      as.POSIXct(daypart, format = "%Y-%m-%d %H:%M:%S", tz = "America/Santiago"),
      error = function(e) NULL
    ))
  }
  if (is.null(parsed) || length(parsed) == 0L || is.na(parsed[[1]])) return(NULL)
  parsed[[1]]
}

admin_profiler_trends <- function(task = NULL, mode = NULL, limit = 60L,
                                  desde = NULL, hasta = NULL, trigger = NULL) {
  hist <- admin_profiler_read_history_raw()
  if (length(hist) < 2L) {
    hist <- c(hist, admin_profiler_history_from_summaries())
  }
  rows <- admin_profiler_dedupe_history(hist)
  if (!is.null(task) && nzchar(as.character(task))) {
    tpat <- as.character(task)
    rows <- Filter(function(r) grepl(tpat, as.character(r$task_name %||% ""), ignore.case = TRUE), rows)
  }
  if (!is.null(mode) && nzchar(as.character(mode))) {
    m <- as.character(mode)
    rows <- Filter(function(r) identical(as.character(r$mode %||% ""), m), rows)
  }
  if (!is.null(trigger) && nzchar(as.character(trigger))) {
    tr <- tolower(trimws(as.character(trigger)))
    if (identical(tr, "scheduled")) tr <- "scheduler"
    if (identical(tr, "auto")) tr <- "scheduler"
    rows <- Filter(function(r) {
      rt <- tolower(as.character(r$trigger_source %||% "manual"))
      if (identical(tr, "scheduler")) {
        return(rt %in% c("scheduler", "chained"))
      }
      identical(rt, tr)
    }, rows)
  }
  d0 <- admin_profiler_parse_day(desde)
  d1 <- admin_profiler_parse_day(hasta)
  if (!is.null(d0) || !is.null(d1)) {
    rows <- Filter(function(r) {
      day <- admin_profiler_row_day(r)
      if (is.na(day)) return(FALSE)
      if (!is.null(d0) && day < d0) return(FALSE)
      if (!is.null(d1) && day > d1) return(FALSE)
      TRUE
    }, rows)
  }
  limit <- max(1L, min(as.integer(limit %||% 60L), 200L))
  if (length(rows) > limit) rows <- tail(rows, limit)

  empty_agg <- list(
    n_runs = 0L,
    wall_ms_mean = NA_real_,
    http_ms_mean = NA_real_,
    sleep_ms_mean = NA_real_,
    compute_ms_mean = NA_real_,
    n_http_mean = NA_real_,
    n_timeout_mean = NA_real_,
    last_wall_ms = NA_real_,
    delta_wall_vs_mean_ms = NA_real_,
    prev_wall_ms = NA_real_,
    delta_wall_vs_prev_ms = NA_real_,
    n_success = 0L,
    n_failed = 0L,
    success_rate = NA_real_,
    slowest_fuente = NULL,
    last_started_at = NULL,
    last_run_ago_sec = NA_real_,
    by_fuente_mean = list(),
    by_var_mean = list(),
    by_step_mean = list()
  )

  if (!length(rows)) {
    return(list(
      ok = TRUE,
      history_path = admin_profiler_history_path(),
      filter_task = task %||% "",
      filter_mode = mode %||% "",
      filter_desde = desde %||% "",
      filter_hasta = hasta %||% "",
      filter_trigger = trigger %||% "",
      aggregates = empty_agg,
      series = list()
    ))
  }

  series <- lapply(rows, function(r) {
    list(
      run_id = r$run_id,
      task_name = r$task_name,
      mode = r$mode,
      trigger_source = r$trigger_source %||% "manual",
      started_at = r$started_at,
      finished_at = r$finished_at,
      status = r$status,
      wall_ms = as.numeric(r$wall_ms %||% 0),
      http_ms = as.numeric(r$http_ms %||% 0),
      sleep_ms = as.numeric(r$sleep_ms %||% 0),
      compute_ms = as.numeric(r$compute_ms %||% 0),
      n_http = as.integer(r$n_http %||% 0L),
      n_timeout = as.integer(r$n_timeout %||% 0L),
      fuentes = r$fuentes %||% list(),
      vars = r$vars %||% list(),
      steps = r$steps %||% list()
    )
  })

  mean_of <- function(field) {
    admin_profiler_mean(vapply(series, function(r) as.numeric(r[[field]] %||% 0), numeric(1)))
  }

  keys_union <- function(getter) {
    ks <- character(0)
    for (r in series) ks <- union(ks, names(getter(r) %||% list()))
    ks
  }
  section_means <- function(getter) {
    out <- list()
    for (k in keys_union(getter)) {
      vals <- vapply(series, function(r) {
        b <- getter(r)
        if (is.null(b) || is.null(b[[k]])) return(NA_real_)
        as.numeric(b[[k]])
      }, numeric(1))
      last_v <- vals[length(vals)]
      out[[k]] <- list(
        mean_ms = admin_profiler_mean(vals),
        n = sum(is.finite(vals)),
        last_ms = if (length(last_v) && is.finite(last_v)) round(last_v, 1) else NA_real_
      )
    }
    out
  }

  by_fuente <- section_means(function(r) r$fuentes)
  by_var <- section_means(function(r) r$vars)
  by_step <- section_means(function(r) r$steps)

  last <- series[[length(series)]]
  prev <- if (length(series) >= 2L) series[[length(series) - 1L]] else NULL
  wall_mean <- mean_of("wall_ms")

  n_success <- sum(vapply(series, function(r) admin_profiler_is_success(r$status), logical(1)))
  n_failed <- length(series) - n_success
  success_rate <- if (length(series)) round(100 * n_success / length(series), 1) else NA_real_

  slowest <- NULL
  if (length(by_fuente)) {
    best_k <- NULL
    best_ms <- -Inf
    for (nm in names(by_fuente)) {
      ms <- as.numeric(by_fuente[[nm]]$mean_ms %||% NA)
      if (is.finite(ms) && ms > best_ms) {
        best_ms <- ms
        best_k <- nm
      }
    }
    if (!is.null(best_k)) {
      slowest <- list(name = best_k, mean_ms = round(best_ms, 1))
    }
  }

  last_started <- as.character(last$started_at %||% last$finished_at %||% "")
  last_ago <- NA_real_
  if (nzchar(last_started)) {
    parsed <- admin_profiler_parse_ts(last_started)
    if (!is.null(parsed)) {
      last_ago <- as.numeric(difftime(Sys.time(), parsed, units = "secs"))
      if (is.finite(last_ago) && last_ago < 0) last_ago <- 0
    }
  }

  aggregates <- list(
    n_runs = length(series),
    wall_ms_mean = wall_mean,
    http_ms_mean = mean_of("http_ms"),
    sleep_ms_mean = mean_of("sleep_ms"),
    compute_ms_mean = mean_of("compute_ms"),
    n_http_mean = mean_of("n_http"),
    n_timeout_mean = mean_of("n_timeout"),
    last_wall_ms = last$wall_ms,
    delta_wall_vs_mean_ms = if (is.finite(wall_mean)) round(last$wall_ms - wall_mean, 1) else NA_real_,
    prev_wall_ms = if (!is.null(prev)) prev$wall_ms else NA_real_,
    delta_wall_vs_prev_ms = if (!is.null(prev)) round(last$wall_ms - prev$wall_ms, 1) else NA_real_,
    n_success = as.integer(n_success),
    n_failed = as.integer(n_failed),
    success_rate = success_rate,
    slowest_fuente = slowest,
    last_started_at = if (nzchar(last_started)) last_started else NULL,
    last_run_ago_sec = if (is.finite(last_ago)) round(last_ago, 0) else NA_real_,
    by_fuente_mean = by_fuente,
    by_var_mean = by_var,
    by_step_mean = by_step
  )

  list(
    ok = TRUE,
    history_path = admin_profiler_history_path(),
    filter_task = task %||% "",
    filter_mode = mode %||% "",
    filter_desde = desde %||% "",
    filter_hasta = hasta %||% "",
    filter_trigger = trigger %||% "",
    aggregates = aggregates,
    series = series
  )
}

#* Lista perfiles de performance (summaries)
#* @get /admin/profiler/runs
#* @param limit Max corridas (default 30)
function(req, res, limit = 30) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  admin_profiler_list_runs(limit)
}

#* Summary de un perfil
#* @get /admin/profiler/runs/<run_id>
function(req, res, run_id) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  admin_profiler_get_run(run_id)
}

#* Spans de un perfil (filtrable)
#* @get /admin/profiler/runs/<run_id>/spans
#* @param parent Parent span id o __root__
#* @param cat Categorias separadas por coma (http,station,var,fuente,step,sleep,watchdog)
#* @param limit Max spans
function(req, res, run_id, parent = "", cat = "", limit = 500) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  admin_profiler_get_spans(run_id, parent = parent, cat = cat, limit = limit)
}

#* Perfil en vivo / mas reciente
#* @get /admin/profiler/live
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  admin_profiler_live()
}

#* Tendencias agregadas entre corridas (history.jsonl + medias)
#* @get /admin/profiler/trends
#* @param task Filtro substring de task_name (ej Diario, Vigilancia)
#* @param mode full|watchdog
#* @param limit Max puntos en serie (default 60)
#* @param desde Fecha inicio YYYY-MM-DD (inclusive)
#* @param hasta Fecha fin YYYY-MM-DD (inclusive)
#* @param trigger manual|scheduler|chained (scheduler incluye chained)
function(req, res, task = "", mode = "", limit = 60, desde = "", hasta = "", trigger = "") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  admin_profiler_trends(
    task = task, mode = mode, limit = limit,
    desde = desde, hasta = hasta, trigger = trigger
  )
}


# --- App profiler (visitas app.rodaja.cl) ---

#* Estado + toggle del app profiler
#* @get /admin/app-profiler/status
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  app_profile_status()
}

#* Encender / apagar grabación
#* @post /admin/app-profiler/enabled
#* @param enabled true|false (query o body JSON)
function(req, res, enabled = "") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  en <- NULL
  body <- req$body
  if (is.null(body) || !length(body)) {
    raw <- req$postBody
    if (!is.null(raw) && nzchar(as.character(raw))) {
      body <- tryCatch(jsonlite::fromJSON(as.character(raw), simplifyVector = FALSE), error = function(e) NULL)
    }
  }
  if (is.list(body) && !is.null(body$enabled)) en <- body$enabled
  if (is.null(en) || (is.character(en) && !nzchar(as.character(en)))) {
    en <- enabled
  }
  if (is.null(en) || (is.character(en) && !nzchar(as.character(en)))) {
    # raw query fallback
    qs <- req$QUERY_STRING %||% ""
    if (grepl("enabled=", qs, fixed = TRUE)) {
      en <- sub(".*(?:^|&)enabled=([^&]*).*", "\\1", qs)
    }
  }
  if (is.character(en)) {
    en <- tolower(trimws(URLdecode(as.character(en)))) %in% c("1", "true", "yes", "on")
  }
  app_profile_set_enabled(isTRUE(en))
}

#* Lista sesiones profileadas
#* @get /admin/app-profiler/sessions
#* @param limit Max sesiones
function(req, res, limit = 40) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  app_profile_list_sessions(limit)
}

#* Summary de una sesión
#* @get /admin/app-profiler/sessions/<run_id>
function(req, res, run_id) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  app_profile_get_session(run_id)
}

#* Spans de una sesión
#* @get /admin/app-profiler/sessions/<run_id>/spans
#* @param cat Categorias separadas por coma
#* @param limit Max spans
function(req, res, run_id, cat = "", limit = 2000) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  app_profile_get_spans(run_id, cat = cat, limit = limit)
}

#* Sesión en vivo / más reciente
#* @get /admin/app-profiler/live
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  app_profile_live()
}


# Calendario de resultados del Diario (America/Santiago).
# month vacío → resumen por mes del año; con month → un día = última corrida full.
# Vars canónicas del scrape Diario (mismo set que stage_stats / day_health).
ADMIN_EXPECTED_INGEST_VARS <- c(
  "dga/pp", "dga/t_min", "dga/u_max_diario",
  "dmc/pp", "dmc/t_max", "dmc/t_min", "dmc/t_mean", "dmc/hr_mean", "dmc/rs_mean",
  "dmc/u_10_kt", "dmc/u_rafaga"
)

admin_santiago_today <- function() {
  hoy_df <- tryCatch(
    db_query("SELECT (CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date AS d"),
    error = function(e) data.frame()
  )
  if (nrow(hoy_df) > 0L) as.Date(hoy_df$d[1]) else {
    as.Date(format(Sys.time(), tz = "America/Santiago"))
  }
}

admin_day_tag <- function(day_chr) {
  gsub("-", "", as.character(day_chr)[1])
}

# Estado final post-Vigilancia/gapfill para un día (day_health; ckpt solo si es hoy).
admin_day_final_ingest_status <- function(day_chr) {
  day_chr <- as.character(day_chr)[1]
  if (!nzchar(day_chr %||% "")) {
    return(list(
      available = FALSE, final = NULL, gapfill_completed = FALSE,
      vig_ran = FALSE, incomplete_vars = character(0),
      completed_vars = character(0), expected_vars = ADMIN_EXPECTED_INGEST_VARS
    ))
  }
  day_tag <- admin_day_tag(day_chr)
  dh <- read_json_file_safe(file.path(
    ingest_logs_dir(), sprintf("pipeline_day_health_%s.json", day_tag)
  ))

  expected <- ADMIN_EXPECTED_INGEST_VARS
  if (!is.null(dh$ingest$expected_vars)) {
    ev <- as.character(unlist(dh$ingest$expected_vars))
    ev <- ev[nzchar(ev)]
    if (length(ev)) expected <- ev
  }
  completed <- character(0)
  if (!is.null(dh$ingest$completed_vars)) {
    completed <- as.character(unlist(dh$ingest$completed_vars))
    completed <- completed[nzchar(completed)]
  }
  incomplete <- character(0)
  if (!is.null(dh$ingest$incomplete_vars)) {
    incomplete <- as.character(unlist(dh$ingest$incomplete_vars))
    incomplete <- incomplete[nzchar(incomplete)]
  }
  gapfill_completed <- isTRUE(dh$ingest$gapfill_completed)
  vig_ran <- !is.null(dh$watchdog_ran_at) && nzchar(as.character(dh$watchdog_ran_at)[1])

  # Hoy: checkpoint gapfill puede ir delante de day_health (HUB_GAPFILL_STALE_DAY_HEALTH).
  hoy <- tryCatch(admin_santiago_today(), error = function(e) Sys.Date())
  if (identical(as.character(hoy), day_chr)) {
    ckpt <- read_ckpt_kv(file.path(ingest_logs_dir(), "actualizar_diario.checkpoint"))
    raw_cv <- as.character(ckpt[["completed_vars"]] %||% "")
    if (nzchar(raw_cv)) {
      ckpt_completed <- trimws(strsplit(raw_cv, ",", fixed = TRUE)[[1]])
      ckpt_completed <- ckpt_completed[nzchar(ckpt_completed)]
      if (length(ckpt_completed) > length(completed)) completed <- ckpt_completed
      if (length(ckpt_completed) > 0L) {
        incomplete <- setdiff(expected, completed)
      }
    }
  }

  # Completo: gapfill explícito, o sin huecos con vars esperadas (o vigilancia sin incomplete).
  fully_ok <- isTRUE(gapfill_completed) || (
    length(incomplete) == 0L &&
      (length(completed) >= length(expected) || isTRUE(vig_ran))
  )
  holes_remain <- length(incomplete) > 0L && !isTRUE(fully_ok)

  final <- NULL
  if (fully_ok) {
    final <- "ok"
  } else if (holes_remain) {
    final <- "parcial"
  }

  list(
    available = !is.null(dh),
    final = final,
    gapfill_completed = isTRUE(fully_ok),
    vig_ran = vig_ran,
    incomplete_vars = incomplete,
    completed_vars = completed,
    expected_vars = expected,
    watchdog_ran_at = if (vig_ran) as.character(dh$watchdog_ran_at)[1] else NULL,
    day_health = dh
  )
}

# Calendario: outcome de la fila Diario + estado final post-gapfill.
# Si Vigilancia rellenó 100% → ok; si quedan huecos → parcial.
admin_calendar_reconcile_outcome <- function(base_outcome, day_chr) {
  base <- as.character(base_outcome %||% "none")[1]
  if (!nzchar(base)) base <- "none"
  if (base %in% c("none", "omitido")) return(base)

  st <- admin_day_final_ingest_status(day_chr)
  if (identical(st$final, "ok")) return("ok")
  if (identical(st$final, "parcial")) return("parcial")
  base
}

# Estaciones con dato en bitacora_oficial por fuente/variable.
# Mira fecha_fin y hasta 2 días atrás; elige el día con más estaciones
# (DMC publica con atraso; un par de filas "de hoy" no debe ganar).
admin_stations_scraped_by_var <- function(day_chr, fecha_fin = NULL, fecha_ini = NULL) {
  day_chr <- as.character(day_chr)[1]
  if (!nzchar(day_chr %||% "") || !admin_tabla_existe("bitacora_oficial")) {
    return(list())
  }
  fin <- tryCatch(as.Date(fecha_fin %||% day_chr), error = function(e) as.Date(day_chr))
  if (is.na(fin)) fin <- as.Date(day_chr)
  # Ventana fija 3 dias (fin..fin-2): atraso DMC tipico. fecha_ini se ignora a proposito
  # (antes as.Date(NULL) -> Date(0) rompia el if del drawer con 500).
  ini <- fin - 2L
  # ponytail: fecha_ini reserved for callers; widening the window made the UI query too slow.

  df <- tryCatch(db_query("
    SELECT lower(f.nombre) AS fuente,
           lower(v.nombre_abr) AS variable,
           o.fecha::text AS fecha_dato,
           COUNT(DISTINCT o.id_estacion)::int AS n_estaciones
    FROM bitacora_oficial o
    JOIN mirador e ON e.id = o.id_estacion
    JOIN cantera f ON f.id = e.id_fuente
    JOIN magnitud v ON v.id = o.id_variable
    WHERE o.fecha BETWEEN $1::date AND $2::date
      AND lower(f.nombre) IN ('dga', 'dmc')
    GROUP BY lower(f.nombre), lower(v.nombre_abr), o.fecha
  ", params = list(as.character(ini), as.character(fin))), error = function(e) data.frame())

  if (nrow(df) == 0L) return(list())
  out <- list()
  keys <- unique(paste0(df$fuente, "/", df$variable))
  for (key in keys) {
    rows <- df[paste0(df$fuente, "/", df$variable) == key, , drop = FALSE]
    n_vec <- as.integer(rows$n_estaciones)
    max_n <- max(n_vec, na.rm = TRUE)
    # Día más reciente con cobertura razonable (evita 2 filas "de hoy" vs 230 de ayer).
    thresh <- if (!is.finite(max_n) || max_n < 50L) {
      max_n
    } else {
      max(50L, as.integer(ceiling(0.5 * max_n)))
    }
    ok <- rows[n_vec >= thresh, , drop = FALSE]
    if (nrow(ok) == 0L) ok <- rows
    ord <- order(-as.numeric(as.Date(ok$fecha_dato)), -as.integer(ok$n_estaciones))
    best <- ok[ord[1], , drop = FALSE]
    out[[key]] <- list(
      fuente = as.character(best$fuente[1]),
      variable = as.character(best$variable[1]),
      n_estaciones = as.integer(best$n_estaciones[1]),
      fecha_dato = as.character(best$fecha_dato[1])
    )
  }
  out
}

# Deep dive de un día del calendario Diario (drawer Hub).
admin_diario_day_detail <- function(day = NULL) {
  hoy <- admin_santiago_today()
  day_chr <- if (is.null(day) || !nzchar(as.character(day)[1])) {
    as.character(hoy)
  } else {
    d <- tryCatch(as.Date(gsub("/", "-", as.character(day)[1])), error = function(e) NA)
    if (is.na(d)) {
      return(list(ok = FALSE, disponible = FALSE, mensaje = "Fecha inválida"))
    }
    as.character(d)
  }
  day_tag <- admin_day_tag(day_chr)
  final_st <- admin_day_final_ingest_status(day_chr)
  dh <- final_st$day_health

  df <- tryCatch(db_query("
    SELECT id, run_id, started_at, finished_at, exit_code, skipped,
           pasos_detalle, errores, job_bat, metrics, task_name, mode, phase,
           estaciones_dga, estaciones_dmc, filas_afectadas_total,
           filas_afectadas_dga, filas_afectadas_dmc, duracion_segundos
    FROM parte_de_guardia
    WHERE (started_at AT TIME ZONE 'America/Santiago')::date = $1::date
      AND COALESCE(skipped, false) IS NOT TRUE
      AND (
        task_name = 'Rodaja_Actualizar_Diario'
        OR (
          COALESCE(task_name, '') = ''
          AND job_bat = 'actualizar_diario.bat'
          AND COALESCE(mode, 'full') IN ('full', '')
        )
      )
      AND COALESCE(mode, 'full') NOT IN
        ('watchdog','resume','warmup','scrape','post','reportes','bias','precision','skip')
    ORDER BY started_at DESC
    LIMIT 1
  ", params = list(day_chr)), error = function(e) data.frame())

  ran <- nrow(df) > 0L
  info <- if (ran) actualizacion_row_list(df[1, , drop = FALSE], include_full_errores = TRUE) else NULL
  base_outcome <- if (!is.null(info)) {
    est <- as.character(info$estado %||% "fallo")
    if (identical(est, "exitoso")) "ok"
    else if (identical(est, "parcial")) "parcial"
    else if (identical(est, "omitido")) "omitido"
    else "fail"
  } else {
    "none"
  }
  outcome <- admin_calendar_reconcile_outcome(base_outcome, day_chr)

  m <- if (ran) jsonb_as_list(df$metrics[[1]] %||% df$metrics[1]) else list()
  response_ms <- extract_response_ms(m, dh)
  pasos <- if (ran) {
    jsonb_as_list(df$pasos_detalle[[1]] %||% df$pasos_detalle[1])
  } else {
    list()
  }
  # Enriquecer pasos desde day_health si PG incompleto
  if (!is.null(dh$pasos) && is.list(dh$pasos)) {
    have <- vapply(pasos, function(p) as.character(p$id %||% ""), character(1))
    nms <- names(dh$pasos)
    if (!is.null(nms)) {
      for (nm in nms) {
        if (nm %in% have) next
        info_p <- dh$pasos[[nm]]
        if (is.null(info_p)) next
        pasos[[length(pasos) + 1L]] <- list(
          id = nm,
          exit_code = info_p$exit_code %||% 0L,
          skipped = isTRUE(info_p$skipped),
          duration_sec = info_p$duration_sec %||% NULL,
          finished_at = info_p$finished_at %||% NULL,
          started_at = info_p$started_at %||% NULL
        )
      }
    }
  }

  est_dga <- if (ran) pg_scalar_int(df$estaciones_dga[1]) else NULL
  est_dmc <- if (ran) pg_scalar_int(df$estaciones_dmc[1]) else NULL
  if ((is.null(est_dga) || identical(est_dga, 0L)) && !is.null(m$estaciones_dga)) {
    est_dga <- suppressWarnings(as.integer(unlist(m$estaciones_dga)[1]))
  }
  if ((is.null(est_dmc) || identical(est_dmc, 0L)) && !is.null(m$estaciones_dmc)) {
    est_dmc <- suppressWarnings(as.integer(unlist(m$estaciones_dmc)[1]))
  }
  if ((is.null(est_dga) || identical(est_dga, 0L)) && !is.null(dh$ingest$estaciones_dga)) {
    est_dga <- suppressWarnings(as.integer(unlist(dh$ingest$estaciones_dga)[1]))
  }
  if ((is.null(est_dmc) || identical(est_dmc, 0L)) && !is.null(dh$ingest$estaciones_dmc)) {
    est_dmc <- suppressWarnings(as.integer(unlist(dh$ingest$estaciones_dmc)[1]))
  }
  filas <- if (ran) pg_scalar_int(df$filas_afectadas_total[1]) else NULL
  if ((is.null(filas) || identical(filas, 0L)) && !is.null(m$filas_afectadas_total)) {
    filas <- suppressWarnings(as.integer(unlist(m$filas_afectadas_total)[1]))
  }

  # Estado por variable: completed vs incomplete + estaciones con dato
  # (última fecha en ventana de scrape; DMC suele publicar con 1 día de atraso)
  fecha_fin_run <- day_chr
  fecha_ini_run <- NULL
  if (identical(as.character(admin_santiago_today()), day_chr)) {
    ckpt <- read_ckpt_kv(file.path(ingest_logs_dir(), "actualizar_diario.checkpoint"))
    if (nzchar(as.character(ckpt[["fecha_fin"]] %||% "")[1])) {
      fecha_fin_run <- as.character(ckpt[["fecha_fin"]])[1]
    }
    if (nzchar(as.character(ckpt[["fecha_ini"]] %||% "")[1])) {
      fecha_ini_run <- as.character(ckpt[["fecha_ini"]])[1]
    }
  }
  if (!is.null(m$fecha_fin) && nzchar(as.character(unlist(m$fecha_fin)[1]))) {
    fecha_fin_run <- as.character(unlist(m$fecha_fin)[1])
  }
  if (!is.null(m$fecha_ini) && nzchar(as.character(unlist(m$fecha_ini)[1]))) {
    fecha_ini_run <- as.character(unlist(m$fecha_ini)[1])
  }
  if (!is.null(dh$ingest$fecha_fin) && nzchar(as.character(unlist(dh$ingest$fecha_fin)[1]))) {
    fecha_fin_run <- as.character(unlist(dh$ingest$fecha_fin)[1])
  }
  if (!is.null(dh$ingest$fecha_ini) && nzchar(as.character(unlist(dh$ingest$fecha_ini)[1]))) {
    fecha_ini_run <- as.character(unlist(dh$ingest$fecha_ini)[1])
  }

  stations_by_var <- admin_stations_scraped_by_var(
    day_chr,
    fecha_fin = fecha_fin_run,
    fecha_ini = fecha_ini_run
  )
  vars_detail <- lapply(final_st$expected_vars, function(vk) {
    parts <- strsplit(vk, "/", fixed = TRUE)[[1]]
    fuente <- if (length(parts) >= 1L) parts[1] else ""
    variable <- if (length(parts) >= 2L) parts[2] else ""
    # DGA temp: scrape t_min pero BD/gapfill usan temp
    db_key <- vk
    if (identical(vk, "dga/t_min") && is.null(stations_by_var[[vk]]) &&
        !is.null(stations_by_var[["dga/temp"]])) {
      db_key <- "dga/temp"
    }
    if (identical(vk, "dga/u_max_diario") && is.null(stations_by_var[[vk]]) &&
        !is.null(stations_by_var[["dga/viento"]])) {
      db_key <- "dga/viento"
    }
    rec <- stations_by_var[[db_key]] %||% stations_by_var[[vk]]
    n_est <- rec$n_estaciones %||% NULL
    fecha_dato <- rec$fecha_dato %||% NULL
    status <- if (vk %in% final_st$completed_vars || isTRUE(final_st$gapfill_completed)) {
      "ok"
    } else if (vk %in% final_st$incomplete_vars) {
      "parcial"
    } else if (length(final_st$completed_vars) == 0L && length(final_st$incomplete_vars) == 0L) {
      "desconocido"
    } else {
      "ok"
    }
    list(
      key = vk,
      fuente = fuente,
      variable = variable,
      status = status,
      n_estaciones = n_est,
      fecha_dato = fecha_dato
    )
  })

  # Totales fuente: max estaciones vistas por var (si PG no las guardó)
  if (is.null(est_dga) || identical(est_dga, 0L)) {
    ns <- vapply(vars_detail, function(v) {
      if (!identical(v$fuente, "dga")) return(0L)
      as.integer(v$n_estaciones %||% 0L)
    }, integer(1))
    if (any(ns > 0L)) est_dga <- max(ns)
  }
  if (is.null(est_dmc) || identical(est_dmc, 0L)) {
    ns <- vapply(vars_detail, function(v) {
      if (!identical(v$fuente, "dmc")) return(0L)
      as.integer(v$n_estaciones %||% 0L)
    }, integer(1))
    if (any(ns > 0L)) est_dmc <- max(ns)
  }

  gap_pares <- admin_gapfill_pares_meta(
    incomplete_vars = final_st$incomplete_vars,
    fecha_fin = as.Date(day_chr),
    day_tag = day_tag,
    gapfill_completed = isTRUE(final_st$gapfill_completed),
    ttl_sec = 60
  )

  err_rep <- admin_pipeline_error_report(day_tag)
  err_compact <- list()
  if (isTRUE(err_rep$available)) {
    for (e in err_rep$compact %||% list()) {
      err_compact[[length(err_compact) + 1L]] <- list(
        code = as.character(e$code %||% ""),
        count = e$count %||% 1L,
        step = as.character(e$step %||% ""),
        fuente = as.character(e$fuente %||% "")
      )
      if (length(err_compact) >= 12L) break
    }
  }

  step_labels <- c(
    preflight = "Precalentar",
    ingest = "Descargar",
    alertas_top10 = "Alertas confirmadas",
    refresh_pronostico = "Pronóstico",
    alertas_pronostico = "Alertas+",
    verificar_concordancia = "QC",
    evaluar_precision_alertas = "Precisión alertas",
    actualizar_skill_alertas = "Skill alertas",
    bias_historico = "Histórico OM"
  )
  pasos_out <- lapply(pasos, function(p) {
    id <- as.character(p$id %||% "")
    lab <- if (nzchar(id) && id %in% names(step_labels)) unname(step_labels[[id]]) else id
    list(
      id = id,
      label = lab,
      exit_code = suppressWarnings(as.integer(unlist(p$exit_code)[1])) %||% 0L,
      skipped = isTRUE(p$skipped),
      duration_sec = suppressWarnings(as.numeric(unlist(p$duration_sec)[1])),
      finished_at = as.character(p$finished_at %||% "")[1],
      started_at = as.character(p$started_at %||% "")[1]
    )
  })

  list(
    ok = TRUE,
    disponible = ran || isTRUE(final_st$available),
    day = day_chr,
    day_label = format(as.Date(day_chr), "%d/%m/%Y"),
    outcome = outcome,
    outcome_base = base_outcome,
    outcome_note = if (identical(outcome, "ok") && identical(base_outcome, "parcial") &&
                       isTRUE(final_st$gapfill_completed)) {
      "completado por gapfill (Vigilancia)"
    } else if (identical(outcome, "parcial") && length(final_st$incomplete_vars) > 0L) {
      paste0("huecos pendientes: ", paste(final_st$incomplete_vars, collapse = ", "))
    } else {
      NULL
    },
    run = if (ran) list(
      id = info$id,
      run_id = info$run_id,
      started_at = info$started_at,
      finished_at = info$finished_at,
      exit_code = info$exit_code,
      estado = info$estado,
      duracion_segundos = pg_scalar_int(df$duracion_segundos[1]) %||%
        {
          dm <- suppressWarnings(as.numeric(unlist(m$duracion_ingesta_minutos)[1]))
          if (length(dm) == 1L && is.finite(dm)) as.integer(round(dm * 60)) else NULL
        }
    ) else NULL,
    # as.list: jsonlite no debe colapsar length-1 character a string JSON
    scrape = list(
      estaciones_dga = est_dga,
      estaciones_dmc = est_dmc,
      filas = filas,
      response_ms = response_ms,
      vars = vars_detail,
      completed_vars = as.list(final_st$completed_vars),
      incomplete_vars = as.list(final_st$incomplete_vars),
      expected_vars = as.list(final_st$expected_vars)
    ),
    gapfill = list(
      completed = isTRUE(final_st$gapfill_completed),
      vig_ran = isTRUE(final_st$vig_ran),
      watchdog_ran_at = final_st$watchdog_ran_at,
      n_pares_ingest = gap_pares$n_pares_ingest,
      n_pares_gapfill = gap_pares$n_pares_gapfill,
      n_done = gap_pares$n_done,
      pct = gap_pares$pct,
      by_var = gap_pares$by_var,
      label = gap_pares$label,
      pending_vars = as.list(final_st$incomplete_vars)
    ),
    pasos = pasos_out,
    errors = err_compact,
    day_events = admin_day_timeline_events(day_chr)
  )
}

admin_diario_calendar <- function(year = NULL, month = NULL) {
  if (!admin_tabla_existe("parte_de_guardia")) {
    return(list(ok = FALSE, disponible = FALSE, mensaje = "Sin tabla parte_de_guardia"))
  }
  bounds <- tryCatch(db_query("
    SELECT
      EXTRACT(YEAR FROM MIN(started_at AT TIME ZONE 'America/Santiago'))::int AS y_min,
      EXTRACT(YEAR FROM MAX(started_at AT TIME ZONE 'America/Santiago'))::int AS y_max
    FROM parte_de_guardia
    WHERE COALESCE(skipped, false) IS NOT TRUE
      AND (
        task_name = 'Rodaja_Actualizar_Diario'
        OR (
          COALESCE(task_name, '') = ''
          AND job_bat = 'actualizar_diario.bat'
          AND COALESCE(mode, 'full') IN ('full', '')
        )
      )
      AND COALESCE(mode, 'full') NOT IN
        ('watchdog','resume','warmup','scrape','post','reportes','bias','precision')
  "), error = function(e) data.frame())
  y_min <- if (nrow(bounds) > 0L) pg_scalar_int(bounds$y_min[1]) else NULL
  y_max <- if (nrow(bounds) > 0L) pg_scalar_int(bounds$y_max[1]) else NULL

  hoy <- tryCatch({
    d <- db_query("SELECT (CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date AS d")
    as.Date(d$d[1])
  }, error = function(e) Sys.Date())

  yr <- suppressWarnings(as.integer(year))
  if (is.null(year) || !nzchar(as.character(year)) || is.na(yr)) {
    yr <- as.integer(format(hoy, "%Y"))
  }
  mo <- suppressWarnings(as.integer(month))
  if (!is.null(month) && nzchar(as.character(month)) && !is.na(mo) && mo >= 1L && mo <= 12L) {
    # --- vista mes ---
    d0 <- as.Date(sprintf("%04d-%02d-01", yr, mo))
    d1 <- seq(d0, by = "month", length.out = 2L)[2]
    df <- tryCatch(db_query("
      SELECT id, run_id, started_at, finished_at, exit_code, skipped,
             pasos_detalle, errores, job_bat, metrics, task_name, mode
      FROM parte_de_guardia
      WHERE (started_at AT TIME ZONE 'America/Santiago')::date >= $1::date
        AND (started_at AT TIME ZONE 'America/Santiago')::date < $2::date
        AND COALESCE(skipped, false) IS NOT TRUE
        AND (
          task_name = 'Rodaja_Actualizar_Diario'
          OR (
            COALESCE(task_name, '') = ''
            AND job_bat = 'actualizar_diario.bat'
            AND COALESCE(mode, 'full') IN ('full', '')
          )
        )
        AND COALESCE(mode, 'full') NOT IN
          ('watchdog','resume','warmup','scrape','post','reportes','bias','precision')
      ORDER BY started_at ASC
    ", params = list(as.character(d0), as.character(d1))), error = function(e) data.frame())

    by_day <- list()
    if (nrow(df) > 0L) {
      for (i in seq_len(nrow(df))) {
        row <- df[i, , drop = FALSE]
        day_chr <- as.character(as.Date(
          format(as.POSIXct(row$started_at[1], tz = "UTC"), tz = "America/Santiago")
        ))
        info <- actualizacion_row_list(row)
        est <- as.character(info$estado %||% "fallo")
        base_outcome <- if (identical(est, "exitoso")) "ok"
          else if (identical(est, "parcial")) "parcial"
          else if (identical(est, "omitido")) "omitido"
          else "fail"
        outcome <- admin_calendar_reconcile_outcome(base_outcome, day_chr)
        by_day[[day_chr]] <- list(
          day = day_chr,
          outcome = outcome,
          outcome_base = base_outcome,
          estado = est,
          exit_code = info$exit_code,
          run_id = info$run_id,
          started_at = info$started_at,
          finished_at = info$finished_at,
          id = info$id
        )
      }
    }

    n_days <- as.integer(d1 - d0)
    days <- lapply(seq_len(n_days) - 1L, function(off) {
      dchr <- as.character(d0 + off)
      if (!is.null(by_day[[dchr]])) return(by_day[[dchr]])
      list(day = dchr, outcome = "none", estado = NULL, exit_code = NULL,
           run_id = NULL, started_at = NULL, finished_at = NULL, id = NULL)
    })

    return(list(
      ok = TRUE, disponible = TRUE, view = "month",
      year = yr, month = mo,
      month_label = format(d0, "%B %Y"),
      today = as.character(hoy),
      y_min = y_min, y_max = y_max,
      days = days
    ))
  }

  # --- vista año ---
  d0 <- as.Date(sprintf("%04d-01-01", yr))
  d1 <- as.Date(sprintf("%04d-01-01", yr + 1L))
  df <- tryCatch(db_query("
    SELECT
      (started_at AT TIME ZONE 'America/Santiago')::date AS day,
      exit_code, skipped, pasos_detalle, errores, job_bat, metrics, mode
    FROM parte_de_guardia
    WHERE (started_at AT TIME ZONE 'America/Santiago')::date >= $1::date
      AND (started_at AT TIME ZONE 'America/Santiago')::date < $2::date
      AND COALESCE(skipped, false) IS NOT TRUE
      AND (
        task_name = 'Rodaja_Actualizar_Diario'
        OR (
          COALESCE(task_name, '') = ''
          AND job_bat = 'actualizar_diario.bat'
          AND COALESCE(mode, 'full') IN ('full', '')
        )
      )
      AND COALESCE(mode, 'full') NOT IN
        ('watchdog','resume','warmup','scrape','post','reportes','bias','precision')
    ORDER BY started_at ASC
  ", params = list(as.character(d0), as.character(d1))), error = function(e) data.frame())

  # última corrida por día → agrega por mes
  day_out <- list()
  if (nrow(df) > 0L) {
    for (i in seq_len(nrow(df))) {
      day_chr <- as.character(df$day[i])
      est <- derive_estado(
        df$exit_code[i], df$skipped[i],
        jsonb_as_list(df$pasos_detalle[[i]] %||% df$pasos_detalle[i]),
        df$errores[[i]] %||% df$errores[i],
        df$job_bat[i],
        jsonb_as_list(df$metrics[[i]] %||% df$metrics[i])
      )
      base_outcome <- if (identical(est, "exitoso")) "ok"
        else if (identical(est, "parcial")) "parcial"
        else if (identical(est, "omitido")) "omitido"
        else "fail"
      day_out[[day_chr]] <- admin_calendar_reconcile_outcome(base_outcome, day_chr)
    }
  }
  months <- lapply(1:12, function(m) {
    ok <- 0L; fail <- 0L; parcial <- 0L; omitido <- 0L; none <- 0L
    d_start <- as.Date(sprintf("%04d-%02d-01", yr, m))
    d_end <- seq(d_start, by = "month", length.out = 2L)[2]
    nd <- as.integer(d_end - d_start)
    for (off in seq_len(nd) - 1L) {
      dchr <- as.character(d_start + off)
      past_or_today <- (d_start + off) <= hoy
      o <- day_out[[dchr]]
      if (is.null(o)) {
        if (past_or_today) none <- none + 1L
      } else if (identical(o, "ok")) ok <- ok + 1L
      else if (identical(o, "parcial")) parcial <- parcial + 1L
      else if (identical(o, "omitido")) omitido <- omitido + 1L
      else fail <- fail + 1L
    }
    list(
      month = m,
      label = format(d_start, "%b"),
      ok = ok, fail = fail, parcial = parcial, omitido = omitido, none = none,
      has_data = (ok + fail + parcial + omitido) > 0L
    )
  })

  list(
    ok = TRUE, disponible = TRUE, view = "year",
    year = yr, month = NULL,
    today = as.character(hoy),
    y_min = y_min, y_max = y_max,
    months = months
  )
}

#* Estado en vivo (status.json / lock / checkpoint)
#* @get /admin/pipelines/live
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  admin_pipelines_live()
}

#* Salud Task Scheduler (JSON exportado desde host)
#* @get /admin/pipelines/schtasks
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  admin_schtasks_health()
}


#* Calendario ok/fail del Actualizar Diario
#* @get /admin/pipelines/diario-calendar
#* @param year Año (default: actual Santiago)
#* @param month Mes 1-12 (omitir = vista año)
function(req, res, year = "", month = "") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  y <- if (is.null(year) || !nzchar(as.character(year))) NULL else as.character(year)
  m <- if (is.null(month) || !nzchar(as.character(month))) NULL else as.character(month)
  admin_diario_calendar(y, m)
}

#* Deep dive de un día del pipeline Diario (calendario Hub)
#* @get /admin/pipelines/diario-day
#* @param day Fecha yyyy-mm-dd (default: hoy Santiago)
function(req, res, day = "") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  d <- if (is.null(day) || !nzchar(as.character(day))) NULL else as.character(day)
  admin_diario_day_detail(d)
}

#* Catalogo de tareas programadas
#* @get /admin/pipelines/catalog
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  list(ok = TRUE, items = PIPELINE_TASK_CATALOG)
}

#* Reporte de errores del pipeline del día (JSON Vigilancia/Diario)
#* @get /admin/pipelines/error-report
#* @param day Fecha yyyymmdd o yyyy-mm-dd (default: hoy)
function(req, res, day = "") {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)
  d <- if (is.null(day) || !nzchar(as.character(day))) NULL else as.character(day)
  admin_pipeline_error_report(d)
}

#* KPIs agregados para el hub de administracion
#* @get /admin/overview
function(req, res) {
  err <- require_admin_key(req, res)
  if (!is.null(err)) return(err)

  list(
    ok = TRUE,
    pipelines = admin_pipelines_resumen(),
    live = admin_pipelines_live(),
    schtasks = admin_schtasks_health(),
    alertas = admin_alertas_resumen(),
    pronostico = admin_pronostico_resumen(),
    error_report = admin_pipeline_error_report(),
    backfill_bbdd = admin_backfill_bbdd_stats(),
    backfill_dga = admin_backfill_dga_stats(),
    backfill_dga_rafaga = admin_backfill_dga_rafaga_stats(),
    backfill_u10h = admin_backfill_u10h_stats(),
    reportes_pdf = admin_estacion_reporte_snapshot()
  )
}
