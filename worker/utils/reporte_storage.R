# Almacenamiento local: state.json, manifest.json y rutas por estacion.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

REPORTE_SCHEMA_VERSION <- 1L
REPORTE_EXTREMOS_TOP_N <- 10L
REPORTE_HIST_BINS <- 256L

REPORTE_SECTION_ORDER <- c(
  "00_portada", "01_resumen", "02_ficha_tecnica", "03_calidad",
  "04_ciclo_estacional", "05_tendencias", "06_extremos", "07_spi_spei",
  "08_pronostico", "09_enso", "10_metodologia"
)

REPORTE_SECTION_FILES <- setNames(
  paste0(REPORTE_SECTION_ORDER, ".md"),
  REPORTE_SECTION_ORDER
)

reporte_dir_base <- function() {
  d <- Sys.getenv("REPORTES_DIR", "/data/reportes")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

reporte_station_dir <- function(eid) {
  p <- file.path(reporte_dir_base(), as.character(as.integer(eid)))
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

reporte_sections_dir <- function(eid) {
  p <- file.path(reporte_station_dir(eid), "sections")
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

reporte_sections_pdf_dir <- function(eid) {
  p <- file.path(reporte_station_dir(eid), "sections_pdf")
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

reporte_media_dir <- function(eid) {
  p <- file.path(reporte_sections_dir(eid), "media")
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

reporte_state_path <- function(eid) file.path(reporte_station_dir(eid), "state.json")
reporte_manifest_path <- function(eid) file.path(reporte_station_dir(eid), "manifest.json")
reporte_pdf_path <- function(eid) file.path(reporte_station_dir(eid), "reporte.pdf")
reporte_completo_md_path <- function(eid) file.path(reporte_station_dir(eid), "reporte_completo.md")

reporte_read_json <- function(path) {
  if (!file.exists(path)) return(NULL)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

reporte_write_json <- function(obj, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(obj, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}

reporte_manifest_init <- function(eid) {
  secs <- stats::setNames(
    lapply(REPORTE_SECTION_ORDER, function(id) {
      list(dirty = TRUE, hash = NA_character_, updated_at = NA_character_)
    }),
    REPORTE_SECTION_ORDER
  )
  list(
    schema_version = REPORTE_SCHEMA_VERSION,
    id_estacion = as.integer(eid),
    frozen = FALSE,
    sections = secs,
    merged_at = NA_character_
  )
}

reporte_manifest_get <- function(eid) {
  m <- reporte_read_json(reporte_manifest_path(eid))
  if (is.null(m)) return(reporte_manifest_init(eid))
  m
}

reporte_manifest_mark_dirty <- function(manifest, section_ids, all = FALSE) {
  if (isTRUE(all)) {
    for (id in REPORTE_SECTION_ORDER) manifest$sections[[id]]$dirty <- TRUE
    return(manifest)
  }
  for (id in section_ids) {
    if (id %in% names(manifest$sections)) manifest$sections[[id]]$dirty <- TRUE
  }
  manifest
}

reporte_file_hash <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  as.character(tools::md5sum(path))
}

reporte_state_init <- function(eid, meta = list()) {
  list(
    schema_version = REPORTE_SCHEMA_VERSION,
    id_estacion = as.integer(eid),
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    last_obs_date = NA_character_,
    meta = meta,
    variables = list()
  )
}

reporte_save_state <- function(eid, state) {
  reporte_write_json(state, reporte_state_path(eid))
}

reporte_load_state <- function(eid) {
  reporte_read_json(reporte_state_path(eid))
}

reporte_save_manifest <- function(eid, manifest) {
  reporte_write_json(manifest, reporte_manifest_path(eid))
}

reporte_section_md_path <- function(eid, section_id) {
  file.path(reporte_sections_dir(eid), REPORTE_SECTION_FILES[[section_id]])
}

reporte_section_pdf_path <- function(eid, section_id) {
  file.path(reporte_sections_pdf_dir(eid), paste0(section_id, ".pdf"))
}
