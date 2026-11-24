# Orquestacion Fase 1 / 2 / render PDF por estacion.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

reporte_source_all <- function(api_root) {
  source(file.path("/app", "utils", "reporte_constants.R"), local = FALSE)
  source(file.path("/app", "utils", "reporte_storage.R"), local = FALSE)
  source(file.path("/app", "utils", "reporte_db.R"), local = FALSE)
  source(file.path("/app", "utils", "senales_curvas.R"), local = FALSE)
  source(file.path("/app", "utils", "reporte_metricas.R"), local = FALSE)
  source(file.path("/app", "utils", "reporte_charts.R"), local = FALSE)
  source(file.path("/app", "utils", "reporte_sections.R"), local = FALSE)
  source(file.path("/app", "utils", "reporte_interpretacion.R"), local = FALSE)
  source(file.path("/app", "utils", "md2pdf.R"), local = FALSE)
  reporte_bootstrap_api_utils(api_root)
}

reporte_fetch_calidad <- function(conn, eid) {
  mv <- if (exists("mv_estacion_calidad_nombre", mode = "function")) mv_estacion_calidad_nombre() else "estacion_calidad"
  df <- DBI::dbGetQuery(conn, paste0(
    "SELECT * FROM ", mv, " WHERE id_estacion = $1 LIMIT 1"
  ), params = list(as.integer(eid)))
  if (nrow(df) == 0) return(NULL)
  as.list(df[1, ])
}

reporte_build_full_state <- function(conn, eid, meta, variables_df) {
  oni <- reporte_load_oni()
  state <- reporte_state_init(eid, meta = list(
    nombre = meta$nombre, codigo = meta$codigo, operador = meta$operador
  ))
  state$variables <- list()
  if (nrow(variables_df) == 0) return(state)
  last_dates <- character(0)
  for (i in seq_len(nrow(variables_df))) {
    vid <- variables_df$id[i]
    df <- reporte_fetch_daily_series(conn, eid, vid)
    if (nrow(df) == 0) next
    var_meta <- as.list(variables_df[i, ])
    abr <- var_meta$nombre_abr %||% ""
    df <- df_display_valores(df, abr)
    vk <- as.character(vid)
    state$variables[[vk]] <- reporte_build_variable_state(df, var_meta, oni)
    last_dates <- c(last_dates, state$variables[[vk]]$last_obs_date)
  }
  state$last_obs_date <- if (length(last_dates)) max(last_dates, na.rm = TRUE) else NA_character_
  state
}

reporte_render_document <- function(eid, manifest = NULL) {
  reporte_assemble_document(eid)
  pdf_out <- reporte_pdf_path(eid)
  md2pdf_render(reporte_completo_md_path(eid), pdf_out, media_dir = reporte_media_dir(eid))
  manifest <- reporte_manifest_get(eid)
  manifest$merged_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  reporte_save_manifest(eid, manifest)
  pdf_out
}

reporte_pipeline_phase1 <- function(conn, eid, log_fn = message) {
  eid <- as.integer(eid)
  t0 <- Sys.time()
  if (!reporte_estacion_elegible(conn, eid)) {
    log_fn("[reporte] skip eid=", eid, " no excelente")
    return(list(ok = FALSE, reason = "not_eligible"))
  }
  meta <- reporte_fetch_estacion_meta(conn, eid)
  if (is.null(meta)) return(list(ok = FALSE, reason = "no_station"))
  variables_df <- reporte_fetch_variables(conn, eid)
  state <- reporte_build_full_state(conn, eid, meta, variables_df)
  state$variables_df <- NULL
  reporte_save_state(eid, state)
  calidad <- reporte_fetch_calidad(conn, eid)
  pron <- tryCatch(reporte_display_pronostico_df(reporte_fetch_pronostico(conn, eid)), error = function(e) NULL)
  manifest <- reporte_manifest_init(eid)
  reporte_save_manifest(eid, manifest)
  reporte_render_charts(eid, state, meta, conn, dirty_sections = NULL, log_fn = log_fn)
  reporte_write_all_sections(eid, meta, state, calidad, pron, variables_df = variables_df)
  reporte_render_document(eid)
  now <- Sys.time()
  reporte_db_upsert(conn, eid, list(
    estado = "activo", elegible = TRUE,
    ultima_actualizacion = format(now, "%Y-%m-%d %H:%M:%S"),
    ultima_actualizacion_pesada = format(now, "%Y-%m-%d %H:%M:%S"),
    error_msg = NA_character_
  ))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  log_fn("[reporte] Fase1 eid=", eid, " ", round(elapsed, 2), "s")
  list(ok = TRUE, seconds = elapsed)
}

reporte_pipeline_phase2 <- function(conn, eid, fecha_desde = NULL, log_fn = message) {
  eid <- as.integer(eid)
  t0 <- Sys.time()
  row <- reporte_db_get(conn, eid)
  if (is.null(row) || !identical(as.character(row$estado), "activo")) {
    return(list(ok = FALSE, reason = "not_active"))
  }
  if (!reporte_estacion_elegible(conn, eid)) {
    return(list(ok = FALSE, reason = "not_eligible"))
  }
  state <- reporte_load_state(eid)
  if (is.null(state)) return(list(ok = FALSE, reason = "no_state"))
  meta <- reporte_fetch_estacion_meta(conn, eid)
  calidad <- reporte_fetch_calidad(conn, eid)
  if (is.null(fecha_desde) || !nzchar(fecha_desde)) {
    fecha_desde <- state$last_obs_date
  }
  changed_ext <- FALSE
  for (vk in names(state$variables)) {
    vid <- state$variables[[vk]]$id_variable
    abr <- state$variables[[vk]]$nombre_abr %||% ""
    full_df <- df_display_valores(reporte_fetch_daily_series(conn, eid, vid), abr)
    new_rows <- full_df
    if (!is.null(fecha_desde) && nzchar(fecha_desde) && nrow(new_rows) > 0) {
      ld <- as.Date(fecha_desde)
      new_rows <- new_rows[new_rows$fecha > ld, , drop = FALSE]
    }
    if (nrow(new_rows) == 0) next
    old_ext <- length(state$variables[[vk]]$extremes$max %||% list())
    state$variables[[vk]] <- reporte_incremental_update_variable(
      state$variables[[vk]], new_rows, full_df = full_df
    )
    if (length(state$variables[[vk]]$extremes$max) != old_ext) changed_ext <- TRUE
  }
  if (!is.null(state$last_obs_date)) {
    state$last_obs_date <- max(c(state$last_obs_date, as.character(Sys.Date())), na.rm = TRUE)
  }
  reporte_save_state(eid, state)
  pron <- tryCatch(reporte_display_pronostico_df(reporte_fetch_pronostico(conn, eid)), error = function(e) NULL)
  variables_df <- reporte_fetch_variables(conn, eid)
  dirty <- c("00_portada", "01_resumen", "04_ciclo_estacional", "08_pronostico", "07_spi_spei", "05_tendencias")
  if (changed_ext) dirty <- c(dirty, "06_extremos")
  manifest <- reporte_manifest_get(eid)
  manifest <- reporte_manifest_mark_dirty(manifest, dirty)
  reporte_save_manifest(eid, manifest)
  reporte_render_charts(eid, state, meta, conn, dirty_sections = unique(dirty), log_fn = log_fn)
  reporte_write_all_sections(eid, meta, state, calidad, pron, dirty_ids = dirty, variables_df = variables_df)
  reporte_render_document(eid)
  now <- Sys.time()
  reporte_db_upsert(conn, eid, list(ultima_actualizacion = now, error_msg = NULL))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  log_fn("[reporte] Fase2 eid=", eid, " ", round(elapsed, 2), "s")
  list(ok = TRUE, seconds = elapsed)
}

reporte_pipeline_heavy <- function(conn, eid, log_fn = message) {
  eid <- as.integer(eid)
  t0 <- Sys.time()
  state <- reporte_load_state(eid)
  if (is.null(state)) return(list(ok = FALSE, reason = "no_state"))
  for (vk in names(state$variables)) {
    state$variables[[vk]] <- reporte_heavy_refresh_variable(conn, eid, state$variables[[vk]])
  }
  reporte_save_state(eid, state)
  meta <- reporte_fetch_estacion_meta(conn, eid)
  calidad <- reporte_fetch_calidad(conn, eid)
  pron <- tryCatch(reporte_display_pronostico_df(reporte_fetch_pronostico(conn, eid)), error = function(e) NULL)
  dirty <- c("00_portada", "02_ficha_tecnica", "03_calidad", "05_tendencias", "07_spi_spei",
             "09_enso", "01_resumen", "04_ciclo_estacional", "06_extremos", "08_pronostico",
             "10_metodologia")
  manifest <- reporte_manifest_get(eid)
  manifest <- reporte_manifest_mark_dirty(manifest, dirty)
  reporte_save_manifest(eid, manifest)
  reporte_render_charts(eid, state, meta, conn, dirty_sections = dirty, log_fn = log_fn)
  reporte_write_all_sections(eid, meta, state, calidad, pron, dirty_ids = dirty)
  reporte_render_document(eid)
  now <- Sys.time()
  reporte_db_upsert(conn, eid, list(
    ultima_actualizacion = now,
    ultima_actualizacion_pesada = now,
    error_msg = NA_character_
  ))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  log_fn("[reporte] Heavy eid=", eid, " ", round(elapsed, 2), "s")
  list(ok = TRUE, seconds = elapsed)
}

reporte_log_timing <- function(label, eid, seconds, path = NULL) {
  if (is.null(path)) {
    repo <- Sys.getenv("REPO_ROOT", "/app/..")
    logs <- file.path(repo, "ingest", "logs")
    if (!dir.exists(logs)) logs <- file.path("/app", "..", "ingest", "logs")
    if (!dir.exists(logs)) logs <- tempdir()
    path <- file.path(logs, paste0("reporte_", label, "_", format(Sys.Date(), "%Y%m%d"), ".log"))
  }
  line <- sprintf("[%s] eid=%s seconds=%.3f\n",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"), eid, seconds)
  cat(line, file = path, append = TRUE)
  invisible(path)
}
