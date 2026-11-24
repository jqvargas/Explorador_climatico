# Orquestacion MeteoVision: scraping + persist.R + post_ingesta (sin dbWriteTable legacy).

`%||%` <- function(x, y) if (is.null(x)) y else x

ingest_fechas_default <- function() {
  yr <- as.integer(format(Sys.Date(), "%Y"))
  list(
    ini = Sys.getenv("INGEST_FECHA_INI", paste0(yr, "-01-01")),
    fin = Sys.getenv("INGEST_FECHA_FIN", format(Sys.Date(), "%Y-%m-%d"))
  )
}

ingest_flag_true <- function(name) {
  tolower(Sys.getenv(name, "")) %in% c("1", "true", "yes", "si")
}

meteovision_valor_column <- function(data) {
  cols <- setdiff(
    names(data),
    c("year", "month", "day", "codigo_nacional", "status_code", "fecha")
  )
  if (length(cols) == 0L) return(NA_character_)
  cols[1]
}

meteovision_prepare_obs <- function(conn, data, id_variable, fuente, fecha_ini, fecha_fin) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) {
    return(data.frame(
      id_estacion = integer(), id_variable = integer(),
      fecha = as.Date(character()), valor = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  vcol <- meteovision_valor_column(data)
  if (is.na(vcol)) return(data.frame())

  if (vcol %in% names(data)) {
    ok <- !vapply(data[[vcol]], is.logical, logical(1))
    data <- data[ok, , drop = FALSE]
  }
  data <- data[stats::complete.cases(data[, c("year", "month", "day", "codigo_nacional", vcol), drop = FALSE]), , drop = FALSE]
  if (nrow(data) == 0L) return(data.frame())

  long <- normalize_long_to_observaciones(data, valor_col = vcol)
  id_fuente <- catalog_get_id_fuente(conn, fuente)
  est_map <- catalog_cargar_estaciones(conn, id_fuente)
  obs <- catalog_aplicar_a_filas(long, est_map, as.integer(id_variable))
  if (nrow(obs) == 0L) return(obs)

  f_ini <- as.Date(fecha_ini)
  f_fin <- as.Date(fecha_fin)
  obs[obs$fecha >= f_ini & obs$fecha <= f_fin, , drop = FALSE]
}

meteovision_persist_scrape_data <- function(
  conn, data, id_variable, fuente, fecha_ini, fecha_fin,
  dry_run = FALSE
) {
  obs <- meteovision_prepare_obs(conn, data, id_variable, fuente, fecha_ini, fecha_fin)
  if (nrow(obs) == 0L) return(0L)
  if (dry_run) {
    message("[dry-run] ", nrow(obs), " filas listas (fuente=", fuente, " id_var=", id_variable, ")")
    return(nrow(obs))
  }
  persist_observacion_final(conn, obs)
}

meteovision_dga_tabla_fechas <- function(fecha_ini, fecha_fin) {
  if (!exists("ingest_tier1_url_dates_enabled", mode = "function") ||
      !ingest_tier1_url_dates_enabled()) {
    return(list(fecha_tabla_ini = NULL, fecha_tabla_fin = NULL))
  }
  list(
    fecha_tabla_ini = as.Date(fecha_ini),
    fecha_tabla_fin = as.Date(fecha_fin)
  )
}

meteovision_persist_batch_size <- function() {
  raw <- Sys.getenv("INGEST_PERSIST_BATCH_STATIONS", "10")
  n <- suppressWarnings(as.integer(raw))
  if (is.na(n) || n < 1L) 10L else n
}

# Environment, no list: en R `list$value <-` dentro de un closure no toca al caller.
# Eso dejo n_inserted=0 con persist real (logs: "Filas afectadas: 48" luego "filas afectadas: 0").
meteovision_new_contador <- function() {
  e <- new.env(parent = emptyenv())
  e$value <- 0L
  e
}

# El UPSERT no corrige la fila que no llega. El dia abierto (pp_day NA) hay
# que borrarlo de bitacora_oficial y de jornada_cerrada; si no, el 0
# anterior sigue en el reporte y tapa el pronostico.
dga_pp_delete_chunk <- function(conn, tabla, id_est, vid, fecha) {
  if (!tabla %in% c("bitacora_oficial", "jornada_cerrada")) stop("tabla no permitida")
  values <- sprintf(
    "(%d, %d, '%s'::date)",
    as.integer(id_est), as.integer(vid), format(as.Date(fecha), "%Y-%m-%d")
  )
  sql <- paste0(
    "DELETE FROM ", tabla, " AS f USING (VALUES ",
    paste(values, collapse = ", "),
    ") AS d(id_estacion, id_variable, fecha) ",
    "WHERE f.id_estacion = d.id_estacion AND f.id_variable = d.id_variable ",
    "AND f.fecha = d.fecha"
  )
  tryCatch(as.integer(DBI::dbExecute(conn, sql)), error = function(e) {
    message("[dga/pp] delete ", tabla, ": ", conditionMessage(e))
    0L
  })
}

dga_pp_borrar_abiertas <- function(conn, abiertas) {
  if (is.null(abiertas) || !is.data.frame(abiertas) || nrow(abiertas) == 0L) {
    return(invisible(0L))
  }
  need <- c("codigo_nacional", "fecha", "id_variable")
  if (!all(need %in% names(abiertas))) return(invisible(0L))
  id_fuente <- catalog_get_id_fuente(conn, "dga")
  est_map <- catalog_cargar_estaciones(conn, id_fuente)
  id_est <- est_map[as.character(abiertas$codigo_nacional)]
  fecha <- as.Date(abiertas$fecha)
  vid <- as.integer(abiertas$id_variable)
  ok <- !is.na(id_est) & !is.na(fecha) & !is.na(vid)
  if (!any(ok)) return(invisible(0L))
  id_est <- as.integer(id_est[ok])
  fecha <- fecha[ok]
  vid <- vid[ok]
  keep <- !duplicated(paste(id_est, vid, format(fecha)))
  id_est <- id_est[keep]
  vid <- vid[keep]
  fecha <- fecha[keep]
  n <- 0L
  step <- 400L
  for (start in seq(1L, length(id_est), by = step)) {
    end <- min(length(id_est), start + step - 1L)
    n <- n + dga_pp_delete_chunk(conn, "bitacora_oficial", id_est[start:end], vid[start:end], fecha[start:end])
    n <- n + dga_pp_delete_chunk(conn, "jornada_cerrada", id_est[start:end], vid[start:end], fecha[start:end])
  }
  if (n > 0L) message("[dga/pp] dia abierto borrado: ", n, " fila(s)")
  invisible(n)
}

# drop_na antes de persist borra el NA del dia abierto y el UPSERT no puede
# corregir el 0 que ya esta guardado. En pp se conserva esa fila.
meteovision_drop_na_salvo_pp <- function(dat) {
  if (!is.data.frame(dat) || nrow(dat) == 0L) return(dat)
  if ("pp_day" %in% names(dat)) return(dat)
  tidyr::drop_na(dat)
}

meteovision_make_persist_buffer <- function(conn, fuente, fecha_ini, fecha_fin, dry_run, contador_ref) {
  buf <- list()
  abiertas <- list()
  anotar_abiertas <- function(data, vid) {
    if (!identical(fuente, "dga")) return(invisible(NULL))
    if (!is.data.frame(data) || !"pp_day" %in% names(data)) return(invisible(NULL))
    if (!exists("dga_pp_fechas_abiertas", mode = "function")) return(invisible(NULL))
    fa <- dga_pp_fechas_abiertas(data)
    if (is.null(fa) || nrow(fa) == 0L) return(invisible(NULL))
    fa$id_variable <- as.integer(vid)
    abiertas[[length(abiertas) + 1L]] <<- fa
    invisible(NULL)
  }
  flush <- function() {
    n <- 0L
    if (length(buf) > 0L) {
      combined <- dplyr::bind_rows(buf)
      buf <<- list()
      if (nrow(combined) > 0L) {
        if (dry_run) {
          contador_ref$value <- contador_ref$value + nrow(combined)
          n <- nrow(combined)
        } else {
          n <- persist_observacion_final(conn, combined)
          contador_ref$value <- contador_ref$value + as.integer(n)
        }
      }
    }
    if (!dry_run && length(abiertas) > 0L) {
      dga_pp_borrar_abiertas(conn, dplyr::bind_rows(abiertas))
    }
    abiertas <<- list()
    invisible(n)
  }
  persist_one <- function(data, vid) {
    anotar_abiertas(data, vid)
    obs <- meteovision_prepare_obs(conn, data, vid, fuente, fecha_ini, fecha_fin)
    if (nrow(obs) == 0L) return(invisible(0L))
    buf[[length(buf) + 1L]] <<- obs
    invisible(nrow(obs))
  }
  list(persist_one = persist_one, flush = flush, every_n = meteovision_persist_batch_size())
}

meteovision_filter_estaciones <- function(estaciones, modo, conn, fuente, id_variable, codigo = NULL) {
  if (!is.null(codigo) && nzchar(codigo)) {
    estaciones <- estaciones[estaciones$codigo_nacional == codigo, , drop = FALSE]
  } else if (identical(modo, "test") || ingest_flag_true("INGEST_WRITE_TEST")) {
    estaciones <- estaciones[1, , drop = FALSE]
  } else if (identical(modo, "completo")) {
    id_fuente <- obtener_id_fuente(conn, fuente)
    activas <- obtener_estaciones_con_datos_variable(conn, id_fuente, id_variable)
    estaciones <- estaciones[estaciones$codigo_nacional %in% activas, , drop = FALSE]
  }
  estaciones
}

# Universo viento DMC: 134 codigos con producto (el resto da 200 + 0 celdas).
dmc_viento_codigos_csv <- function() {
  ingest_dir <- get0("INGEST_DIR", envir = .GlobalEnv, ifnotfound = NULL)
  p <- if (!is.null(ingest_dir) && nzchar(as.character(ingest_dir)[1])) {
    file.path(ingest_dir, "docs", "dmc_viento_estaciones.csv")
  } else {
    file.path("ingest", "docs", "dmc_viento_estaciones.csv")
  }
  if (!file.exists(p)) stop("Falta lista viento DMC: ", p)
  raw <- utils::read.csv(p, stringsAsFactors = FALSE)
  unique(trimws(as.character(raw$codigo_nacional)))
}

dmc_filter_estaciones_viento <- function(estaciones) {
  if (nrow(estaciones) == 0L) return(estaciones)
  want <- dmc_viento_codigos_csv()
  estaciones[as.character(estaciones$codigo_nacional) %in% want, , drop = FALSE]
}

# Estaciones DGA que ya tienen velocidad (u_max_diario). La direccion no recorre
# todas las meteorologicas: la mayoria responde vacio.
dga_viento_codigos_con_u2 <- function(conn) {
  cached <- get0(".dga_viento_codigos_u2", envir = .GlobalEnv, ifnotfound = NULL)
  if (is.character(cached)) return(cached)
  id_u2 <- obtener_id_variable(conn, "u_max_diario")
  id_f <- obtener_id_fuente(conn, "dga")
  rows <- DBI::dbGetQuery(conn, "
    SELECT e.codigo_nacional
    FROM mirador e
    WHERE e.id_fuente = $1
      AND e.id IN (
        SELECT DISTINCT id_estacion FROM bitacora_oficial WHERE id_variable = $2
      )
  ", params = list(as.integer(id_f), as.integer(id_u2)))
  cod <- unique(trimws(as.character(rows$codigo_nacional)))
  assign(".dga_viento_codigos_u2", cod, envir = .GlobalEnv)
  cod
}

dga_filter_estaciones_viento <- function(estaciones, conn) {
  if (nrow(estaciones) == 0L) return(estaciones)
  want <- dga_viento_codigos_con_u2(conn)
  estaciones[as.character(estaciones$codigo_nacional) %in% want, , drop = FALSE]
}

# Gapfill: quedarse solo con estaciones del set actual que aun no tienen fecha_fin.
# Pure filter (sin_fecha) usable en selfcheck sin BD.
meteovision_filter_gapfill_codigos <- function(estaciones, sin_fecha_codigos) {
  if (nrow(estaciones) == 0L) return(estaciones)
  estaciones[estaciones$codigo_nacional %in% sin_fecha_codigos, , drop = FALSE]
}

meteovision_filter_gapfill_fecha_fin <- function(estaciones, conn, fuente, id_variable, fecha_fin) {
  if (nrow(estaciones) == 0L) return(estaciones)
  id_fuente <- obtener_id_fuente(conn, fuente)
  sin <- obtener_codigos_sin_fecha(conn, id_fuente, id_variable, fecha_fin)
  meteovision_filter_gapfill_codigos(estaciones, sin)
}

# Hub: snapshot de huecos gapfill (pares est/var) para el arco Vigilancia→Descargar.
# Solo escribe n_initial la primera vez por var; n_remaining se actualiza.
gapfill_huecos_json_path <- function(fecha_fin) {
  root <- get0("INGEST_DIR", envir = .GlobalEnv, ifnotfound = NULL)
  if (is.null(root) || !nzchar(as.character(root)[1])) return(NULL)
  day <- format(as.Date(fecha_fin), "%Y%m%d")
  file.path(root, "logs", sprintf("pipeline_gapfill_huecos_%s.json", day))
}

gapfill_huecos_record <- function(label, n_huecos, n0, fecha_fin) {
  path <- gapfill_huecos_json_path(fecha_fin)
  if (is.null(path)) return(invisible(NULL))
  label <- as.character(label)[1]
  n_huecos <- as.integer(n_huecos)[1]
  n0 <- as.integer(n0)[1]
  if (!nzchar(label) || is.na(n_huecos) || n_huecos < 0L) return(invisible(NULL))
  if (is.na(n0) || n0 < 0L) n0 <- n_huecos
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  raw <- tryCatch(
    if (file.exists(path)) jsonlite::fromJSON(path, simplifyVector = FALSE) else NULL,
    error = function(e) NULL
  )
  if (is.null(raw) || !is.list(raw)) {
    raw <- list(
      day = format(as.Date(fecha_fin), "%Y-%m-%d"),
      fecha_fin = format(as.Date(fecha_fin), "%Y-%m-%d"),
      vars = list()
    )
  }
  if (is.null(raw$vars) || !is.list(raw$vars)) raw$vars <- list()
  prev <- raw$vars[[label]]
  n_initial <- if (is.list(prev) && !is.null(prev$n_initial)) {
    suppressWarnings(as.integer(prev$n_initial)[1])
  } else {
    n_huecos
  }
  if (length(n_initial) != 1L || is.na(n_initial)) n_initial <- n_huecos
  raw$vars[[label]] <- list(
    n_initial = as.integer(n_initial),
    n0 = as.integer(n0),
    n_remaining = as.integer(n_huecos),
    recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  raw$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  tryCatch(
    jsonlite::write_json(raw, path, auto_unbox = TRUE, pretty = TRUE),
    error = function(e) {
      message("[gapfill] no pude escribir huecos json: ", e$message)
    }
  )
  invisible(NULL)
}

# jobs: list(codigo, row, params=list(list(kind,i))). Max 3 estaciones y 6 parametros.
dga_pack_jobs <- function(jobs, fecha_ini, fecha_fin, ano_actual, mes_ultimo) {
  items <- list()
  buf <- list()
  n_par <- 0L
  flush <- function() {
    if (!length(buf)) return(invisible(NULL))
    slots <- list()
    for (job in buf) {
      for (p in job$params) {
        slots[[length(slots) + 1L]] <- list(
          codigo = job$codigo, kind = p$kind, i = p$i, row = job$row
        )
      }
    }
    items[[length(items) + 1L]] <<- list(
      batch = TRUE,
      slots = slots,
      attempt = 1L,
      cod = paste(vapply(buf, function(j) as.character(j$codigo), character(1)), collapse = ","),
      fecha_ini = fecha_ini,
      fecha_fin = fecha_fin,
      ano_actual = ano_actual,
      mes_ultimo = mes_ultimo
    )
    buf <<- list()
    n_par <<- 0L
  }
  for (job in jobs) {
    need <- length(job$params)
    if (!need) next
    if (length(buf) && (length(buf) >= 3L || n_par + need > 6L)) flush()
    buf[[length(buf) + 1L]] <- job
    n_par <- n_par + need
  }
  flush()
  items
}

dga_batch_singles <- function(slots) {
  lapply(slots, function(s) {
    list(
      batch = FALSE,
      i = s$i,
      cod = as.character(s$codigo),
      row = s$row,
      kind = s$kind,
      attempt = 1L
    )
  })
}

# Un GET de tanda. Si la forma no calza, split_retry con las estaciones sueltas.
dga_run_batch_item <- function(item) {
  if (exists("dga_reset_handle", mode = "function")) dga_reset_handle()
  slots <- item$slots
  parsed <- dga_fetch_batch(slots, item$fecha_ini, item$fecha_fin)
  base <- list(
    batch = TRUE,
    cod = item$cod,
    timeout = isTRUE(parsed$timeout),
    http_code = parsed$http_code,
    dead_mark = FALSE,
    status_429 = if (identical(as.character(parsed$reason), "429")) 1L else 0L,
    status_403 = if (identical(as.character(parsed$reason), "403")) 1L else 0L,
    reason = parsed$reason
  )
  if (!isTRUE(parsed$ok)) {
    base$ok <- FALSE
    base$split_retry <- TRUE
    base$singles <- dga_batch_singles(slots)
    base$data <- NULL
    return(base)
  }
  parts <- vector("list", length(parsed$parts))
  for (i in seq_along(parsed$parts)) {
    kind <- slots[[i]]$kind
    parts[[i]] <- list(
      kind = kind,
      codigo = as.character(slots[[i]]$codigo),
      i = slots[[i]]$i,
      data = dga_finalize_part(parsed$parts[[i]], kind, item$ano_actual, item$mes_ultimo)
    )
  }
  base$ok <- TRUE
  base$batch_ok <- TRUE
  base$split_retry <- FALSE
  base$parts <- parts
  base$data <- NULL
  base
}

meteovision_loop_estaciones <- function(
  estaciones, fuente, variable, label, dry_run, handler = NULL,
  every_n_flush = NULL, flush_fn = NULL,
  scrape_fn = NULL, persist_fn = NULL,
  conn = NULL, id_variable = NULL, fecha_fin = NULL,
  fecha_ini = NULL, ano_fin = NULL, mes_ultimo = NULL
) {
  # post-tipo, pre-scrape: solo huecos reales cuando Vigilancia pide gapfill
  if (ingest_flag_true("INGEST_GAPFILL") &&
      !is.null(conn) && !is.null(id_variable) && !is.null(fecha_fin)) {
    n0 <- nrow(estaciones)
    estaciones <- meteovision_filter_gapfill_fecha_fin(
      estaciones, conn, fuente, id_variable, fecha_fin
    )
    message(
      "[gapfill] ", label, " huecos fecha_fin=",
      format(as.Date(fecha_fin), "%Y-%m-%d"), ": ", nrow(estaciones), "/", n0
    )
    gapfill_huecos_record(label, nrow(estaciones), n0, fecha_fin)
  }
  if (nrow(estaciones) == 0L) return(character(0))
  visited <- character(0)
  Sys.setenv(INGEST_SCRAPE_FUENTE = fuente)
  ckpt_begin_variable(fuente, variable)
  aborted_circuit <- FALSE

  conc <- 1L
  ramp_sec <- 45
  max_retries <- 3L
  if (identical(fuente, "dga") && exists("dga_concurrency", mode = "function")) {
    conc <- dga_concurrency()
  }
  if (identical(fuente, "dga") && exists("dga_ramp_sec", mode = "function")) {
    ramp_sec <- dga_ramp_sec(45)
  }
  if (identical(fuente, "dga") && exists("dga_max_retries", mode = "function")) {
    max_retries <- dga_max_retries(3L)
  }
  if (identical(fuente, "dga") && isTRUE(dga_batch_enabled()) &&
      !is.null(dga_batch_kind(variable))) {
    conc <- min(as.integer(conc), dga_batch_inflight())
  }
  use_parallel <- conc > 1L && is.function(scrape_fn) && is.function(persist_fn)

  # --- path paralelo DGA: pool diferido (ramp), persist solo en padre ---
  if (isTRUE(use_parallel)) {
    if (!requireNamespace("parallel", quietly = TRUE) ||
        !exists("dga_deferred_pool_run", mode = "function")) {
      message("[", label, "] pool diferido no disponible; fallback secuencial")
      use_parallel <- FALSE
    }
  }

  if (isTRUE(use_parallel)) {
    message("[", label, "] concurrency=", conc, " ramp_sec=", ramp_sec,
            " max_retries=", max_retries, " (pool diferido || persist seq)")
    # Una sola lectura de ckpt: N× readLines bajo DGA∥DMC + bind mount
    # Windows → "cannot open the connection" y aborta toda la variable.
    resume_idx <- ckpt_resume_estacion_idx(fuente, variable)
    work <- list()
    for (i in seq_len(nrow(estaciones))) {
      cod <- as.character(estaciones$codigo_nacional[i])
      if (ckpt_should_skip_station(fuente, variable, i, resume_idx = resume_idx)) {
        message("[ckpt] skip ", fuente, "/", variable, " estacion ", i, "/",
                nrow(estaciones), " ", cod)
        visited <- c(visited, cod)
        next
      }
      if (exists("scrape_dead_is_blocked", mode = "function") &&
          scrape_dead_is_blocked(fuente, cod)) {
        message("[", label, "] skip dead ", cod)
        visited <- c(visited, cod)
        if (!dry_run) ckpt_mark_station_done(fuente, variable, i, cod, timeout = FALSE)
        next
      }
      work[[length(work) + 1L]] <- list(
        i = i, cod = cod, row = estaciones[i, , drop = FALSE], attempt = 1L, batch = FALSE
      )
    }
    kind <- if (identical(fuente, "dga") && isTRUE(dga_batch_enabled())) dga_batch_kind(variable) else NULL
    if (!is.null(kind) && length(work) > 0L &&
        !is.null(fecha_ini) && !is.null(ano_fin) && !is.null(mes_ultimo)) {
      jobs <- lapply(work, function(w) {
        list(codigo = w$cod, row = w$row, params = list(list(kind = kind, i = w$i)))
      })
      work <- dga_pack_jobs(jobs, fecha_ini, fecha_fin, ano_fin, mes_ultimo)
      message("[", label, "] batch kind=", kind, " tandas=", length(work), " inflight=", conc)
    }

    n_done_batch <- 0L
    worker_fn <- function(w) {
      if (isTRUE(w$batch)) return(dga_run_batch_item(w))
      if (exists("dga_reset_handle", mode = "function")) dga_reset_handle()
      Sys.setenv(
        INGEST_SCRAPE_FUENTE = fuente,
        INGEST_SCRAPE_VARIABLE = variable,
        INGEST_SCRAPE_WORKER = as.character(w$worker %||% 1L),
        INGEST_SCRAPE_ATTEMPT = as.character(as.integer(w$attempt %||% 1L))
      )
      assign(".scrape_last_was_timeout", FALSE, envir = .GlobalEnv)
      if (exists("dga_scrape_clear_mark", mode = "function")) dga_scrape_clear_mark()
      if (exists("scrape_http_stats_reset", mode = "function")) {
        scrape_http_stats_reset(fuente)
      }
      err <- NULL
      data <- tryCatch(scrape_fn(w$row), error = function(e) {
        err <<- conditionMessage(e)
        NULL
      })
      st <- if (exists("scrape_http_stats_get", mode = "function")) {
        scrape_http_stats_get(fuente)
      } else {
        list()
      }
      http_code <- suppressWarnings(as.integer(
        get0(".scrape_last_http_code", envir = .GlobalEnv, ifnotfound = NA_integer_)
      ))[1]
      empty_reason <- as.character(
        get0(".scrape_last_empty_reason", envir = .GlobalEnv, ifnotfound = NA_character_)
      )[1]
      list(
        i = w$i, cod = w$cod, worker = as.integer(w$worker %||% 1L),
        data = data, err = err,
        timeout = isTRUE(get0(".scrape_last_was_timeout", envir = .GlobalEnv, ifnotfound = FALSE)),
        status_429 = as.integer(st$status_429 %||% 0L),
        status_403 = as.integer(st$status_403 %||% 0L),
        http_code = http_code,
        empty_reason = empty_reason
      )
    }

    scraped <- dga_deferred_pool_run(
      items = work,
      worker_fn = worker_fn,
      max_inflight = conc,
      ramp_sec = ramp_sec,
      max_retries = max_retries,
      label = label,
      should_abort = function() {
        exists("scrape_should_skip_rest", mode = "function") &&
          isTRUE(scrape_should_skip_rest(fuente))
      },
      on_result = function(s) {
        if (inherits(s, "try-error")) return(invisible(NULL))
        if (isTRUE(s$split_retry)) {
          message(
            "[dga-batch] fallback ", s$cod, " reason=",
            if (is.null(s$reason) || length(s$reason) != 1L || is.na(s$reason)) "?" else s$reason
          )
          if (exists("profile_record_pool_http", mode = "function")) {
            profile_record_pool_http(
              fuente = fuente, variable = variable, codigo = s$cod,
              worker = s$worker, timeout = isTRUE(s$timeout), ok = FALSE,
              elapsed_sec = s$pool_elapsed_sec,
              parent = if (exists("profile_current_parent", mode = "function")) {
                profile_current_parent(c("var", "fuente"))
              } else NULL,
              concurrency = conc, attempt = s$attempt,
              http_code = s$http_code, empty_reason = s$reason
            )
          }
          return(invisible(NULL))
        }
        if (isTRUE(s$batch_ok)) {
          if (exists("profile_record_pool_http", mode = "function")) {
            profile_record_pool_http(
              fuente = fuente, variable = variable, codigo = s$cod,
              worker = s$worker, timeout = FALSE, ok = TRUE,
              elapsed_sec = s$pool_elapsed_sec,
              parent = if (exists("profile_current_parent", mode = "function")) {
                profile_current_parent(c("var", "fuente"))
              } else NULL,
              concurrency = conc, attempt = s$attempt,
              http_code = s$http_code
            )
          }
          for (p in s$parts) {
            dat <- p$data
            if (is.data.frame(dat) && nrow(dat) > 0L) dat <- meteovision_drop_na_salvo_pp(dat)
            if (is.data.frame(dat) && nrow(dat) > 0L) persist_fn(dat)
            visited <<- c(visited, p$codigo)
            if (!dry_run) {
              ckpt_mark_station_done(fuente, variable, p$i, p$codigo, timeout = FALSE)
            }
          }
          n_done_batch <<- n_done_batch + length(s$parts %||% list())
          if (!is.null(every_n_flush) && !is.null(flush_fn) &&
              n_done_batch %% every_n_flush == 0L) {
            flush_fn()
          }
          return(invisible(NULL))
        }
        if (exists("profile_record_pool_http", mode = "function")) {
          profile_record_pool_http(
            fuente = fuente,
            variable = variable,
            codigo = s$cod,
            worker = s$worker,
            timeout = isTRUE(s$timeout),
            ok = !is.null(s$data) && is.data.frame(s$data) && nrow(s$data) > 0L,
            elapsed_sec = s$pool_elapsed_sec,
            parent = if (exists("profile_current_parent", mode = "function")) {
              profile_current_parent(c("var", "fuente"))
            } else {
              NULL
            },
            concurrency = conc,
            attempt = s$attempt,
            status_429 = as.integer(s$status_429 %||% 0L),
            status_403 = as.integer(s$status_403 %||% 0L),
            http_code = s$http_code,
            empty_reason = s$empty_reason
          )
        }
        # Documentar latencia / success rate DGA/DMC por corrida (pool no pasa por GET sync)
        if (exists("scrape_http_stats_bump", mode = "function")) {
          el_ms <- suppressWarnings(as.numeric(s$pool_elapsed_sec)[1]) * 1000
          ok_st <- !is.null(s$data) && is.data.frame(s$data) && nrow(s$data) > 0L
          scrape_http_stats_bump(
            status_code = if (isTRUE(s$timeout)) NA_integer_ else if (ok_st) 200L else NA_integer_,
            is_error = isTRUE(s$timeout) || !ok_st,
            fuente = fuente,
            is_timeout = isTRUE(s$timeout),
            elapsed_ms = if (is.finite(el_ms)) el_ms else NULL
          )
        }
        # Kill-switch solo tras racha de 403/429 (ban SNIA). Vacio/timeout/500 reinician racha.
        denied <- as.integer(s$status_429 %||% 0L) > 0L ||
          as.integer(s$status_403 %||% 0L) > 0L
        ok_st <- !is.null(s$data) && is.data.frame(s$data) && nrow(s$data) > 0L
        if (!is.null(s$data) && is.data.frame(s$data) && nrow(s$data) > 0L) {
          persist_fn(s$data)
        }
        visited <<- c(visited, s$cod)
        if (!dry_run) {
          ckpt_mark_station_done(fuente, variable, s$i, s$cod, timeout = isTRUE(s$timeout))
        }
        n_done_batch <<- n_done_batch + 1L
        if (!is.null(every_n_flush) && !is.null(flush_fn) &&
            n_done_batch %% every_n_flush == 0L) {
          flush_fn()
        }
        if (exists("scrape_register_snia_denial", mode = "function")) {
          if (isTRUE(ok_st) || isTRUE(s$timeout) || !isTRUE(denied)) {
            scrape_register_snia_denial(fuente, denied = FALSE)
          } else if (isTRUE(denied)) {
            if (isTRUE(scrape_register_snia_denial(fuente, denied = TRUE))) {
              aborted_circuit <<- TRUE
            }
          }
        }
        if (exists("scrape_register_outcome", mode = "function")) {
          if (isTRUE(s$timeout) || !isTRUE(ok_st)) {
            # Vacio/timeout/deny: no usar circuit legado de "20 soft-fails"
            scrape_register_outcome(FALSE, fuente = fuente, count_toward_circuit = FALSE)
          } else {
            scrape_register_outcome(TRUE, fuente = fuente)
          }
        }
        invisible(NULL)
      }
    )

    # Resultados ya procesados en on_result; scraped solo para conteo/debug
    invisible(scraped)
    if (exists("scrape_should_skip_rest", mode = "function") &&
        isTRUE(scrape_should_skip_rest(fuente))) {
      aborted_circuit <- TRUE
      message("[", label, "] circuit OPEN — abortando resto de estaciones")
    }
    if (!is.null(flush_fn)) flush_fn()
    if (!dry_run && !aborted_circuit) ckpt_mark_variable_done(fuente, variable)
    if (aborted_circuit) {
      message("[WARN] ", label, " incompleta por circuit-breaker; no se marca completed_vars")
    }
    return(unique(visited))
  }

  # --- path secuencial (default) ---
  if (!is.function(handler)) {
    if (is.function(scrape_fn) && is.function(persist_fn)) {
      handler <- function(i, row) {
        data <- scrape_fn(row)
        if (!is.null(data) && is.data.frame(data) && nrow(data) > 0L) persist_fn(data)
      }
    } else {
      stop("handler o scrape_fn+persist_fn requeridos")
    }
  }
  resume_idx_seq <- ckpt_resume_estacion_idx(fuente, variable)
  for (i in seq_len(nrow(estaciones))) {
    if (exists("scrape_should_skip_rest", mode = "function") &&
        scrape_should_skip_rest(fuente)) {
      message("[", label, "] circuit OPEN — abortando resto de estaciones")
      aborted_circuit <- TRUE
      break
    }
    cod <- as.character(estaciones$codigo_nacional[i])
    if (ckpt_should_skip_station(fuente, variable, i, resume_idx = resume_idx_seq)) {
      message("[ckpt] skip ", fuente, "/", variable, " estacion ", i, "/",
              nrow(estaciones), " ", cod)
      visited <- c(visited, cod)
      next
    }
    if (exists("scrape_dead_is_blocked", mode = "function") &&
        scrape_dead_is_blocked(fuente, cod)) {
      message("[", label, "] skip dead ", cod)
      visited <- c(visited, cod)
      if (!dry_run) ckpt_mark_station_done(fuente, variable, i, cod, timeout = FALSE)
      next
    }
    message("[", label, "] estacion ", i, "/", nrow(estaciones), " ", cod)
    assign(".scrape_last_was_timeout", FALSE, envir = .GlobalEnv)
    st_parent <- if (exists("profile_current_parent", mode = "function")) {
      profile_current_parent(c("var", "fuente"))
    } else {
      NULL
    }
    sid <- if (exists("profile_span_begin", mode = "function")) {
      profile_span_begin(
        paste0("station.", cod), "station", parent = st_parent,
        attrs = list(fuente = fuente, variable = variable, codigo = cod),
        fine = TRUE
      )
    } else {
      NULL
    }
    handler(i, estaciones[i, , drop = FALSE])
    if (!is.null(sid) && exists("profile_span_end", mode = "function")) {
      was_to <- isTRUE(get0(".scrape_last_was_timeout", envir = .GlobalEnv, ifnotfound = FALSE))
      profile_span_end(sid, attrs_extra = list(timeout = was_to))
    }
    visited <- c(visited, cod)
    if (!is.null(every_n_flush) && !is.null(flush_fn) &&
        i %% every_n_flush == 0L) {
      flush_fn()
    }
    was_timeout <- isTRUE(get0(".scrape_last_was_timeout", envir = .GlobalEnv, ifnotfound = FALSE))
    if (!dry_run) ckpt_mark_station_done(fuente, variable, i, cod, timeout = was_timeout)
  }
  if (!is.null(flush_fn)) flush_fn()
  if (!dry_run && !aborted_circuit) ckpt_mark_variable_done(fuente, variable)
  if (aborted_circuit) {
    message("[WARN] ", label, " incompleta por circuit-breaker; no se marca completed_vars")
  }
  unique(visited)
}

meteovision_actualizar_variable <- function(
  conn,
  fuente,
  variable,
  fecha_ini,
  fecha_fin,
  modo = "completo",
  dry_run = FALSE,
  skip_post_ingesta = FALSE,
  codigo_estacion = NULL
) {
  meteovision_init()
  if (!dry_run && !persist_unique_index_existe(conn)) {
    stop("Falta el indice unico (UPSERT) en bitacora_oficial")
  }

  estaciones <- obtener_estaciones_fuente(conn, fuente)
  id_variable <- obtener_id_variable(conn, variable)
  is_u10max <- identical(fuente, "dmc") && identical(variable, "u_rafaga")
  # u_10_kt / u_rafaga parten en 0 filas: modo=completo via activas vaciaría el set.
  # u2_dire parte en 0 filas: modo=completo via activas vaciaria el set.
  skip_activas <- (
      (identical(fuente, "dmc") && variable %in% c("u_10_kt", "u_rafaga")) ||
      (identical(fuente, "dga") && identical(variable, "u2_dire"))
    ) &&
    identical(modo, "completo") &&
    (is.null(codigo_estacion) || !nzchar(codigo_estacion)) &&
    !ingest_flag_true("INGEST_WRITE_TEST")
  filtro_modo <- if (isTRUE(skip_activas)) "diario" else modo
  estaciones <- meteovision_filter_estaciones(
    estaciones, filtro_modo, conn, fuente, id_variable, codigo_estacion
  )
  if (identical(fuente, "dmc") && identical(variable, "u_10_kt") &&
      (is.null(codigo_estacion) || !nzchar(codigo_estacion))) {
    estaciones <- dmc_filter_estaciones_viento(estaciones)
  }
  if (!isTRUE(is_u10max) && nrow(estaciones) == 0L) {
    message("[ingest] Sin estaciones para ", fuente, "/", variable)
    return(list(ok = TRUE, n_inserted = 0L, estaciones_codigos = character(0)))
  }

  ano_ini <- as.numeric(format(as.Date(fecha_ini), "%Y"))
  ano_fin <- as.numeric(format(as.Date(fecha_fin), "%Y"))
  mes_ultimo <- as.numeric(format(as.Date(fecha_fin), "%m"))

  contador_ref <- meteovision_new_contador()
  pb <- meteovision_make_persist_buffer(conn, fuente, fecha_ini, fecha_fin, dry_run, contador_ref)
  persist_one <- pb$persist_one
  flush_obs <- pb$flush
  loop_args <- list(
    every_n_flush = pb$every_n,
    flush_fn = flush_obs,
    conn = conn,
    id_variable = id_variable,
    fecha_fin = fecha_fin,
    fecha_ini = fecha_ini,
    ano_fin = ano_fin,
    mes_ultimo = mes_ultimo
  )
  dga_tabla <- meteovision_dga_tabla_fechas(fecha_ini, fecha_fin)
  dga_scrape_pp <- function(row) {
    do.call(
      descargar_pp_DGA,
      c(list(ano_ini, ano_fin, mes_ultimo, row), dga_tabla)
    )
  }
  dga_scrape_temp <- function(row) {
    do.call(
      descargar_temp_DGA,
      c(list(ano_ini, ano_fin, mes_ultimo, row), dga_tabla)
    )
  }
  dga_scrape_viento <- function(row) {
    do.call(
      descargar_viento_DGA,
      c(list(ano_ini, ano_fin, mes_ultimo, row), dga_tabla)
    )
  }
  dga_scrape_viento_dire <- function(row) {
    do.call(
      descargar_viento_dire_DGA,
      c(list(ano_ini, ano_fin, mes_ultimo, row), dga_tabla)
    )
  }

  visited <- character(0)

  if (fuente == "dga" && variable == "pp") {
    estaciones <- estaciones[grepl("METEOROLOGICA", estaciones$tipo), , drop = FALSE]
    visited <- c(visited, do.call(meteovision_loop_estaciones, c(list(
      estaciones, fuente, variable, "dga/pp", dry_run,
      handler = function(i, row) {
        data <- dga_scrape_pp(row)
        if (is.data.frame(data) && nrow(data) > 0L) persist_one(data, id_variable)
      },
      scrape_fn = dga_scrape_pp,
      persist_fn = function(data) persist_one(data, id_variable)
    ), loop_args)))
  } else if (fuente == "dga" && variable == "q") {
    estaciones <- estaciones[grepl("FLUVIOMETRICA", estaciones$tipo), , drop = FALSE]
    visited <- c(visited, do.call(meteovision_loop_estaciones, c(list(
      estaciones, fuente, variable, "dga/q", dry_run,
      function(i, row) {
        data <- tidyr::drop_na(descargar_caudal_DGA(ano_ini, ano_fin, mes_ultimo, row))
        if (nrow(data) > 0L) persist_one(data, id_variable)
      }
    ), loop_args)))
  } else if (fuente == "dga" && variable %in% c("t_max", "t_min")) {
    estaciones <- estaciones[grepl("METEOROLOGICA", estaciones$tipo), , drop = FALSE]
    id_t_mean <- obtener_id_variable(conn, "t_mean")
    id_t_min <- obtener_id_variable(conn, "t_min")
    id_t_max <- obtener_id_variable(conn, "t_max")
    persist_temp <- function(temp_data) {
      if (is.null(temp_data) || nrow(temp_data) == 0L) return(invisible(NULL))
      persist_one(dplyr::select(temp_data, -dplyr::any_of(c("temp_min", "temp_mean"))), id_t_max)
      persist_one(dplyr::select(temp_data, -dplyr::any_of(c("temp_mean", "temp_max"))), id_t_min)
      persist_one(dplyr::select(temp_data, -dplyr::any_of(c("temp_min", "temp_max"))), id_t_mean)
    }
    visited <- c(visited, do.call(meteovision_loop_estaciones, c(list(
      estaciones, fuente, variable, "dga/temp", dry_run,
      scrape_fn = dga_scrape_temp,
      persist_fn = persist_temp
    ), loop_args)))
  } else if (fuente == "dga" && variable %in% c("u_max_diario", "u2_min", "u2_mean")) {
    estaciones <- estaciones[grepl("METEOROLOGICA", estaciones$tipo), , drop = FALSE]
    id_u2_mean <- obtener_id_variable(conn, "u2_mean")
    id_u2_min <- obtener_id_variable(conn, "u2_min")
    id_u_max_diario <- obtener_id_variable(conn, "u_max_diario")
    persist_viento <- function(viento_data) {
      if (is.null(viento_data) || nrow(viento_data) == 0L) return(invisible(NULL))
      persist_one(dplyr::select(viento_data, -dplyr::any_of(c("u2_min", "u2_mean"))), id_u_max_diario)
      persist_one(dplyr::select(viento_data, -dplyr::any_of(c("u2_mean", "u_max_diario"))), id_u2_min)
      persist_one(dplyr::select(viento_data, -dplyr::any_of(c("u2_min", "u_max_diario"))), id_u2_mean)
    }
    visited <- c(visited, do.call(meteovision_loop_estaciones, c(list(
      estaciones, fuente, variable, "dga/viento", dry_run,
      scrape_fn = dga_scrape_viento,
      persist_fn = persist_viento
    ), loop_args)))
  } else if (fuente == "dga" && identical(variable, "u2_dire")) {
    estaciones <- dga_filter_estaciones_viento(estaciones, conn)
    persist_dire <- function(dire_data) {
      if (is.null(dire_data) || nrow(dire_data) == 0L) return(invisible(NULL))
      persist_one(dire_data, id_variable)
    }
    visited <- c(visited, do.call(meteovision_loop_estaciones, c(list(
      estaciones, fuente, variable, "dga/u2_dire", dry_run,
      scrape_fn = dga_scrape_viento_dire,
      persist_fn = persist_dire
    ), loop_args)))
  } else if (fuente == "dmc" && variable == "pp") {
    visited <- c(visited, do.call(meteovision_loop_estaciones, c(list(
      estaciones, fuente, variable, "dmc/pp", dry_run,
      function(i, row) {
        data <- tidyr::drop_na(descargar_pp_DMC(ano_ini, ano_fin, mes_ultimo, row))
        if (nrow(data) > 0L) persist_one(data, id_variable)
      }
    ), loop_args)))
  } else if (fuente == "dmc" && variable == "t_max") {
    visited <- c(visited, do.call(meteovision_loop_estaciones, c(list(
      estaciones, fuente, variable, "dmc/t_max", dry_run,
      function(i, row) {
        data <- tidyr::drop_na(descargar_temp_max_DMC(ano_ini, ano_fin, mes_ultimo, row))
        if (nrow(data) > 0L) persist_one(data, id_variable)
      }
    ), loop_args)))
  } else if (fuente == "dmc" && variable == "t_min") {
    visited <- c(visited, do.call(meteovision_loop_estaciones, c(list(
      estaciones, fuente, variable, "dmc/t_min", dry_run,
      function(i, row) {
        data <- tidyr::drop_na(descargar_temp_min_DMC(ano_ini, ano_fin, mes_ultimo, row))
        if (nrow(data) > 0L) persist_one(data, id_variable)
      }
    ), loop_args)))
  } else if (fuente == "dmc" && variable == "t_mean") {
    visited <- c(visited, do.call(meteovision_loop_estaciones, c(list(
      estaciones, fuente, variable, "dmc/t_mean", dry_run,
      function(i, row) {
        data <- tidyr::drop_na(descargar_temp_media_DMC(ano_ini, ano_fin, mes_ultimo, row))
        if (nrow(data) > 0L) persist_one(data, id_variable)
      }
    ), loop_args)))
  } else if (fuente == "dmc" && variable == "hr_mean") {
    visited <- c(visited, do.call(meteovision_loop_estaciones, c(list(
      estaciones, fuente, variable, "dmc/hr_mean", dry_run,
      function(i, row) {
        data <- tidyr::drop_na(descargar_humedad_DMC(ano_ini, ano_fin, mes_ultimo, row))
        if (nrow(data) > 0L) persist_one(data, id_variable)
      }
    ), loop_args)))
  } else if (fuente == "dmc" && variable == "rs_mean") {
    visited <- c(visited, do.call(meteovision_loop_estaciones, c(list(
      estaciones, fuente, variable, "dmc/rs_mean", dry_run,
      function(i, row) {
        data <- tidyr::drop_na(descargar_radiacion_DMC(ano_ini, ano_fin, mes_ultimo, row))
        if (nrow(data) > 0L) persist_one(data, id_variable)
      }
    ), loop_args)))
  } else if (fuente == "dmc" && variable == "u_10_kt") {
    id_u_10_kt <- obtener_id_variable(conn, "u_10_kt")
    id_u_10_dire <- obtener_id_variable(conn, "u_10_dire")
    persist_viento_dmc <- function(viento_data) {
      if (is.null(viento_data) || nrow(viento_data) == 0L) return(invisible(NULL))
      persist_one(dplyr::select(viento_data, -dplyr::any_of("u_10_dire")), id_u_10_kt)
      persist_one(dplyr::select(viento_data, -dplyr::any_of("u_10_kt")), id_u_10_dire)
    }
    visited <- c(visited, do.call(meteovision_loop_estaciones, c(list(
      estaciones, fuente, variable, "dmc/u_10_kt", dry_run,
      scrape_fn = function(row) {
        descargar_viento_DMC(ano_ini, ano_fin, mes_ultimo, row)
      },
      persist_fn = persist_viento_dmc
    ), loop_args)))
  } else if (isTRUE(is_u10max)) {
    ven <- dmc_viento_max_ventana(fecha_ini, fecha_fin)
    if (as.Date(ven$ini) != as.Date(fecha_ini)) {
      message(
        "[dmc/u_rafaga] ventana corta ", ven$ini, " -> ", ven$fin,
        " (YTD/historico es INGEST_U10MAX_BACKFILL=1)"
      )
    }
    data <- descargar_viento_max_DMC(ven$ini, ven$fin)
    horario <- attr(data, "horario", exact = TRUE)
    if (!is.null(data) && nrow(data) > 0L) persist_one(data, id_variable)
    flush_obs()
    # Mismo HTML: las 24 horas no van a bitacora_oficial (grano DATE).
    if (!is.null(horario) && is.data.frame(horario) && nrow(horario) > 0L) {
      n_h <- tryCatch(
        persist_observacion_horaria_from_scrape(conn, horario, dry_run = dry_run),
        error = function(e) {
          message("[dmc/u_rafaga_h] WARN: ", conditionMessage(e))
          0L
        }
      )
      contador_ref$value <- contador_ref$value + as.integer(n_h)
    }
    cod_d <- if (!is.null(data) && "codigo_nacional" %in% names(data)) {
      unique(as.character(data$codigo_nacional[!is.na(data$codigo_nacional)]))
    } else {
      character(0)
    }
    cod_h <- if (!is.null(horario) && is.data.frame(horario) &&
                 "codigo_nacional" %in% names(horario)) {
      unique(as.character(horario$codigo_nacional[!is.na(horario$codigo_nacional)]))
    } else {
      character(0)
    }
    visited <- unique(c(cod_d, cod_h))
    if (!dry_run) ckpt_mark_variable_done(fuente, variable)
  } else {
    stop("Fuente o variable no valida: ", fuente, "/", variable)
  }

  message("[ingest] ", fuente, "/", variable, " filas afectadas: ", contador_ref$value)
  if (!dry_run && !skip_post_ingesta && !ingest_flag_true("INGEST_SKIP_POST_INGESTA") && contador_ref$value > 0L) {
    post_ingesta_observaciones(conn, contador_ref$value)
  }
  list(ok = TRUE, n_inserted = contador_ref$value, estaciones_codigos = unique(visited))
}

meteovision_scrape_only <- function(fuente, variable, fecha_ini, fecha_fin, codigo_estacion = NULL) {
  meteovision_init()
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  f <- ingest_fechas_default()
  if (missing(fecha_ini)) fecha_ini <- f$ini
  if (missing(fecha_fin)) fecha_fin <- f$fin

  estaciones <- obtener_estaciones_fuente(conn, fuente)
  id_variable <- obtener_id_variable(conn, variable)
  estaciones <- meteovision_filter_estaciones(
    estaciones, "test", conn, fuente, id_variable, codigo_estacion
  )
  if (nrow(estaciones) == 0L) stop("Sin estaciones para smoke scrape")

  ano_ini <- as.numeric(format(as.Date(fecha_ini), "%Y"))
  ano_fin <- as.numeric(format(as.Date(fecha_fin), "%Y"))
  mes_ultimo <- as.numeric(format(as.Date(fecha_fin), "%m"))
  row <- estaciones[1, , drop = FALSE]

  data <- if (fuente == "dga" && variable == "pp") {
    descargar_pp_DGA(ano_ini, ano_fin, mes_ultimo, row)
  } else if (fuente == "dmc" && variable == "t_min") {
    descargar_temp_min_DMC(ano_ini, ano_fin, mes_ultimo, row)
  } else if (fuente == "dmc" && variable == "pp") {
    descargar_pp_DMC(ano_ini, ano_fin, mes_ultimo, row)
  } else {
    stop("smoke_scrape no configurado para ", fuente, "/", variable)
  }
  tidyr::drop_na(data)
}

# Conteo de GETs DMC restantes (est x anios x vars no completed). Gapfill: solo huecos.
# u_rafaga es region x dia (16 x n_dias), no estacion x año.
dmc_budget_count_urls <- function(conn, variables, fecha_ini, fecha_fin, modo) {
  ano_ini <- as.numeric(format(as.Date(fecha_ini), "%Y"))
  ano_fin <- as.numeric(format(as.Date(fecha_fin), "%Y"))
  n_years <- max(1L, as.integer(ano_fin - ano_ini) + 1L)
  n <- 0L
  gapfill <- ingest_flag_true("INGEST_GAPFILL")
  fecha_fin_d <- as.Date(fecha_fin)
  for (v in variables) {
    if (ckpt_variable_done("dmc", v)) next
    if (identical(v, "u_rafaga")) {
      ven <- if (exists("dmc_viento_max_ventana", mode = "function")) {
        dmc_viento_max_ventana(fecha_ini, fecha_fin)
      } else {
        list(ini = as.Date(fecha_ini), fin = as.Date(fecha_fin))
      }
      n_days <- max(1L, as.integer(as.Date(ven$fin) - as.Date(ven$ini)) + 1L)
      n_reg <- if (exists("dmc_viento_max_regiones", mode = "function")) {
        length(dmc_viento_max_regiones())
      } else {
        16L
      }
      n <- n + as.integer(n_reg) * n_days
      next
    }
    id_variable <- tryCatch(obtener_id_variable(conn, v), error = function(e) NA_integer_)
    if (length(id_variable) != 1L || is.na(id_variable)) next
    estaciones <- obtener_estaciones_fuente(conn, "dmc")
    estaciones <- meteovision_filter_estaciones(
      estaciones, modo, conn, "dmc", id_variable, NULL
    )
    if (isTRUE(gapfill)) {
      estaciones <- meteovision_filter_gapfill_fecha_fin(
        estaciones, conn, "dmc", id_variable, fecha_fin_d
      )
    }
    n <- n + as.integer(nrow(estaciones)) * n_years
  }
  as.integer(n)
}

# pos es un vector atomico con nombres. x[[cod]] tira subscript out of bounds
# si el codigo esta solo en la otra variable (no devuelve NULL, como una lista).
dga_pos_index <- function(pos, cod) {
  cod <- as.character(cod)[1]
  if (length(pos) == 0L || length(cod) != 1L || is.na(cod) || !nzchar(cod)) {
    return(NA_integer_)
  }
  hit <- match(cod, names(pos))
  if (length(hit) != 1L || is.na(hit)) return(NA_integer_)
  v <- suppressWarnings(as.integer(pos[[hit]]))
  if (length(v) != 1L || is.na(v)) NA_integer_ else v
}

# Pp + temperatura en la misma tanda (hasta 3 estaciones, 6 parametros).
# El viento no entra aca: es otra pasada, de a 3 y un solo parametro.
meteovision_dga_batch_pp_temp <- function(conn, fecha_ini, fecha_fin, modo, dry_run) {
  base <- obtener_estaciones_fuente(conn, "dga")
  id_pp <- obtener_id_variable(conn, "pp")
  id_t_min <- obtener_id_variable(conn, "t_min")
  id_t_max <- obtener_id_variable(conn, "t_max")
  id_t_mean <- obtener_id_variable(conn, "t_mean")
  pp_est <- meteovision_filter_estaciones(base, modo, conn, "dga", id_pp, NULL)
  t_est <- meteovision_filter_estaciones(base, modo, conn, "dga", id_t_min, NULL)
  pp_est <- pp_est[grepl("METEOROLOGICA", pp_est$tipo), , drop = FALSE]
  t_est <- t_est[grepl("METEOROLOGICA", t_est$tipo), , drop = FALSE]
  if (ingest_flag_true("INGEST_GAPFILL")) {
    pp_est <- meteovision_filter_gapfill_fecha_fin(pp_est, conn, "dga", id_pp, fecha_fin)
    t_est <- meteovision_filter_gapfill_fecha_fin(t_est, conn, "dga", id_t_min, fecha_fin)
  }
  pp_resume <- max(ckpt_batch_idx_get("dga", "pp"), ckpt_resume_estacion_idx("dga", "pp"), na.rm = TRUE)
  t_resume <- max(ckpt_batch_idx_get("dga", "t_min"), ckpt_resume_estacion_idx("dga", "t_min"), na.rm = TRUE)
  if (!is.finite(pp_resume)) pp_resume <- 0
  if (!is.finite(t_resume)) t_resume <- 0
  pp_pos <- setNames(seq_len(nrow(pp_est)), as.character(pp_est$codigo_nacional))
  t_pos <- setNames(seq_len(nrow(t_est)), as.character(t_est$codigo_nacional))
  codes <- unique(c(as.character(pp_est$codigo_nacional), as.character(t_est$codigo_nacional)))
  jobs <- list()
  for (cod in codes) {
    ip <- dga_pos_index(pp_pos, cod)
    it <- dga_pos_index(t_pos, cod)
    if (exists("scrape_dead_is_blocked", mode = "function") &&
        isTRUE(scrape_dead_is_blocked("dga", cod))) {
      next
    }
    params <- list()
    row <- data.frame(codigo_nacional = cod, stringsAsFactors = FALSE)
    if (!is.na(ip) && ip > pp_resume) {
      params[[length(params) + 1L]] <- list(kind = "pp", i = ip)
    }
    if (!is.na(it) && it > t_resume) {
      params[[length(params) + 1L]] <- list(kind = "temp", i = it)
    }
    if (!length(params)) next
    jobs[[length(jobs) + 1L]] <- list(codigo = cod, row = row, params = params)
  }
  ano_fin <- as.numeric(format(as.Date(fecha_fin), "%Y"))
  mes_ultimo <- as.numeric(format(as.Date(fecha_fin), "%m"))
  items <- dga_pack_jobs(jobs, fecha_ini, fecha_fin, ano_fin, mes_ultimo)
  message("[dga/pp+t] tandas=", length(items), " estaciones=", length(jobs),
          " inflight=", dga_batch_inflight())
  contador <- meteovision_new_contador()
  pb <- meteovision_make_persist_buffer(conn, "dga", fecha_ini, fecha_fin, dry_run, contador)
  persist_pp <- function(data) pb$persist_one(data, id_pp)
  persist_temp <- function(temp_data) {
    if (is.null(temp_data) || nrow(temp_data) == 0L) return(invisible(NULL))
    pb$persist_one(dplyr::select(temp_data, -dplyr::any_of(c("temp_min", "temp_mean"))), id_t_max)
    pb$persist_one(dplyr::select(temp_data, -dplyr::any_of(c("temp_mean", "temp_max"))), id_t_min)
    pb$persist_one(dplyr::select(temp_data, -dplyr::any_of(c("temp_min", "temp_max"))), id_t_mean)
  }
  n_done <- 0L
  aborted <- FALSE
  ano_ini <- as.numeric(format(as.Date(fecha_ini), "%Y"))
  fetch_one <- function(kind, row) {
    extra <- list(fecha_tabla_ini = as.Date(fecha_ini), fecha_tabla_fin = as.Date(fecha_fin))
    if (identical(kind, "pp")) {
      return(do.call(descargar_pp_DGA, c(list(ano_ini, ano_fin, mes_ultimo, row), extra)))
    }
    if (identical(kind, "temp")) {
      return(do.call(descargar_temp_DGA, c(list(ano_ini, ano_fin, mes_ultimo, row), extra)))
    }
    do.call(descargar_viento_DGA, c(list(ano_ini, ano_fin, mes_ultimo, row), extra))
  }
  if (length(items)) {
    dga_deferred_pool_run(
      items = items,
      worker_fn = function(w) {
        if (isTRUE(w$batch)) return(dga_run_batch_item(w))
        if (exists("dga_reset_handle", mode = "function")) dga_reset_handle()
        data <- tryCatch(fetch_one(w$kind, w$row), error = function(e) {
          message("[dga/pp+t] suelta ", w$cod, ": ", conditionMessage(e))
          NULL
        })
        list(
          ok = is.data.frame(data) && nrow(data) > 0L,
          batch = FALSE,
          batch_ok = FALSE,
          split_retry = FALSE,
          data = data,
          kind = w$kind,
          i = w$i,
          cod = w$cod,
          timeout = isTRUE(get0(".scrape_last_was_timeout", envir = .GlobalEnv, ifnotfound = FALSE)),
          http_code = suppressWarnings(as.integer(get0(
            ".scrape_last_http_code", envir = .GlobalEnv, ifnotfound = NA_integer_
          )))[1],
          status_429 = 0L,
          status_403 = 0L,
          dead_mark = FALSE
        )
      },
      max_inflight = dga_batch_inflight(),
      ramp_sec = if (exists("dga_ramp_sec", mode = "function")) dga_ramp_sec(45) else 45,
      max_retries = if (exists("dga_max_retries", mode = "function")) dga_max_retries(3L) else 3L,
      label = "dga/pp+t",
      should_abort = function() {
        exists("scrape_should_skip_rest", mode = "function") &&
          isTRUE(scrape_should_skip_rest("dga"))
      },
      on_result = function(s) {
        if (isTRUE(s$split_retry)) {
          message(
            "[dga-batch] fallback ", s$cod, " reason=",
            if (is.null(s$reason) || length(s$reason) != 1L || is.na(s$reason)) "?" else s$reason
          )
          if (exists("profile_record_pool_http", mode = "function")) {
            profile_record_pool_http(
              fuente = "dga", variable = "pp+t_min", codigo = s$cod,
              worker = s$worker, timeout = isTRUE(s$timeout), ok = FALSE,
              elapsed_sec = s$pool_elapsed_sec, concurrency = dga_batch_inflight(),
              attempt = s$attempt, http_code = s$http_code, empty_reason = s$reason
            )
          }
          return(invisible(NULL))
        }
        mark_one <- function(kind, idx, codigo) {
          if (dry_run) return(invisible(NULL))
          var <- if (identical(kind, "pp")) "pp" else "t_min"
          ckpt_batch_idx_set("dga", var, idx, codigo)
        }
        if (!isTRUE(s$batch_ok)) {
          dat <- s$data
          if (is.data.frame(dat) && nrow(dat) > 0L) dat <- meteovision_drop_na_salvo_pp(dat)
          if (is.data.frame(dat) && nrow(dat) > 0L) {
            if (identical(s$kind, "pp")) persist_pp(dat) else persist_temp(dat)
          }
          mark_one(s$kind, s$i, s$cod)
          return(invisible(NULL))
        }
        if (exists("profile_record_pool_http", mode = "function")) {
          profile_record_pool_http(
            fuente = "dga", variable = "pp+t_min", codigo = s$cod,
            worker = s$worker, timeout = FALSE, ok = TRUE,
            elapsed_sec = s$pool_elapsed_sec, concurrency = dga_batch_inflight(),
            attempt = s$attempt, http_code = s$http_code
          )
        }
        for (p in s$parts) {
          dat <- p$data
          if (is.data.frame(dat) && nrow(dat) > 0L) dat <- meteovision_drop_na_salvo_pp(dat)
          if (is.data.frame(dat) && nrow(dat) > 0L) {
            if (identical(p$kind, "pp")) persist_pp(dat) else persist_temp(dat)
          }
          mark_one(p$kind, p$i, p$codigo)
        }
        n_done <<- n_done + length(s$parts)
        if (n_done %% pb$every_n == 0L) pb$flush()
        invisible(NULL)
      }
    )
    aborted <- exists("scrape_should_skip_rest", mode = "function") &&
      isTRUE(scrape_should_skip_rest("dga"))
  }
  pb$flush()
  if (!dry_run && !aborted) {
    ckpt_mark_variable_done("dga", "pp")
    ckpt_mark_variable_done("dga", "t_min")
  }
  message("[dga/pp+t] filas afectadas: ", contador$value)
  invisible(contador$value)
}

meteovision_run_fuente <- function(conn, fuente, variables, fecha_ini, fecha_fin, modo = "completo", dry_run = FALSE) {
  meteovision_init()
  total <- 0L
  failed <- character(0)
  warnings_soft <- character(0)
  all_codes <- character(0)
  Sys.setenv(INGEST_SCRAPE_FUENTE = fuente)
  if (exists("scrape_http_stats_reset", mode = "function")) {
    scrape_http_stats_reset(fuente)
  }
  if (identical(fuente, "dmc") && exists("dmc_budget_init", mode = "function")) {
    n_urls <- tryCatch(
      dmc_budget_count_urls(conn, variables, fecha_ini, fecha_fin, modo),
      error = function(e) {
        message("[dmc] budget count fallo (", e$message, "); usando URLS_EST")
        NA_integer_
      }
    )
    dmc_budget_init(n_urls)
  }
  f_parent <- if (exists("profile_current_parent", mode = "function")) {
    profile_current_parent(c("step", "fuente"))
  } else {
    NULL
  }
  fid <- if (exists("profile_span_begin", mode = "function")) {
    profile_span_begin(paste0("fuente.", fuente), "fuente", parent = f_parent,
                       attrs = list(fuente = fuente))
  } else {
    NULL
  }
  if (identical(fuente, "dga") && isTRUE(dga_batch_enabled()) &&
      "pp" %in% variables && "t_min" %in% variables &&
      !(ckpt_variable_done(fuente, "pp") && ckpt_variable_done(fuente, "t_min"))) {
    meteovision_dga_batch_pp_temp(conn, fecha_ini, fecha_fin, modo, dry_run)
  }
  for (v in variables) {
    if (ckpt_variable_done(fuente, v)) {
      message("[ckpt] skip ", fuente, "/", v, " (completada)")
      next
    }
    if (exists("scrape_transport_hold", mode = "function") &&
        isTRUE(scrape_transport_hold(fuente))) {
      message("[ingest] skip ", fuente, "/", v, " (transport hold)")
      warnings_soft <- c(warnings_soft, paste0(fuente, "/", v, ":transport_hold"))
      next
    }
    was_open <- exists("scrape_should_skip_rest", mode = "function") &&
      scrape_should_skip_rest(fuente)
    if (exists("scrape_circuit_reset_for_next_var", mode = "function")) {
      scrape_circuit_reset_for_next_var(fuente)
      if (isTRUE(was_open)) {
        message("[ingest] circuit reset para variable siguiente ", fuente, "/", v)
      }
    } else if (isTRUE(was_open)) {
      message("[WARN] skip ", fuente, "/", v, " (circuit OPEN)")
      warnings_soft <- c(warnings_soft, paste0(fuente, "/", v, ":circuit"))
      next
    }
    message("[ingest] === ", fuente, " / ", v, " ===")
    Sys.setenv(INGEST_SCRAPE_VARIABLE = v)
    vid <- if (exists("profile_span_begin", mode = "function")) {
      profile_span_begin(
        paste0("var.", fuente, ".", v), "var", parent = fid,
        attrs = list(fuente = fuente, variable = v)
      )
    } else {
      NULL
    }
    res <- tryCatch(
      meteovision_actualizar_variable(
        conn, fuente, v, fecha_ini, fecha_fin,
        modo = modo, dry_run = dry_run, skip_post_ingesta = TRUE
      ),
      error = function(e) {
        message("[ingest] ERROR ", v, ": ", e$message)
        # soft-fail por variable: no tumba la fuente entera
        warnings_soft <<- c(warnings_soft, paste0(fuente, "/", v, ":", e$message))
        list(ok = FALSE, n_inserted = 0L, error = e$message, estaciones_codigos = character(0))
      }
    )
    if (!is.null(vid) && exists("profile_span_end", mode = "function")) {
      profile_span_end(vid, attrs_extra = list(ok = !isFALSE(res$ok), n_inserted = res$n_inserted %||% 0L))
    }
    total <- total + as.integer(res$n_inserted %||% 0L)
    all_codes <- c(all_codes, res$estaciones_codigos %||% character(0))
    if (isFALSE(res$ok)) failed <- c(failed, v)
  }
  if (!is.null(fid) && exists("profile_span_end", mode = "function")) {
    profile_span_end(fid, attrs_extra = list(n_inserted = total, n_estaciones = length(unique(all_codes))))
  }
  if (!dry_run && !ingest_flag_true("INGEST_SKIP_POST_INGESTA") && total > 0L) {
    post_ingesta_observaciones(conn, total)
  }
  http_st <- if (exists("scrape_http_stats_get", mode = "function")) {
    scrape_http_stats_get(fuente)
  } else {
    NULL
  }
  list(
    ok = length(failed) == 0L,
    n_inserted = total,
    failed = failed,
    warnings = warnings_soft,
    n_estaciones = length(unique(all_codes)),
    http_stats = http_st
  )
}
