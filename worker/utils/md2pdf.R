# Markdown → PDF via pandoc + typst (port de .cursor/md2pdf/md2pdf.ps1)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

md2pdf_template_dir <- function() {
  p <- file.path("/app", "templates")
  if (dir.exists(p)) return(p)
  p2 <- file.path(getwd(), "templates")
  if (dir.exists(p2)) return(p2)
  stop("No se encuentra worker/templates")
}

md2pdf_cmd_exists <- function(cmd) {
  nzchar(Sys.which(cmd))
}

md2pdf_parse_yaml <- function(raw) {
  meta <- list()
  body <- raw
  m <- regexpr("(?s)\\A---\\s*\\r?\\n(.+?)\\r?\\n---\\s*\\r?\\n(.*)", raw, perl = TRUE)
  if (m[1] > 0) {
    yaml_block <- sub("(?s)\\A---\\s*\\r?\\n(.+?)\\r?\\n---\\s*\\r?\\n.*", "\\1", raw, perl = TRUE)
    body <- sub("(?s)\\A---\\s*\\r?\\n(.+?)\\r?\\n---\\s*\\r?\\n", "", raw, perl = TRUE)
    for (line in strsplit(yaml_block, "\n", fixed = TRUE)[[1]]) {
      if (grepl("^\\s*(\\w[\\w-]*)\\s*:\\s*(.+)$", line, perl = TRUE)) {
        key <- sub("^\\s*(\\w[\\w-]*)\\s*:\\s*(.+)$", "\\1", line, perl = TRUE)
        val <- sub("^\\s*(\\w[\\w-]*)\\s*:\\s*(.+)$", "\\2", line, perl = TRUE)
        val <- gsub("^[\"']|[\"']$", "", trimws(val))
        meta[[key]] <- val
      }
    }
  }
  list(meta = meta, body = body)
}

md2pdf_meta <- function(meta, key, default = "") {
  v <- meta[[key]]
  if (is.null(v) || !nzchar(v)) default else v
}

md2pdf_esc_typst <- function(s) {
  s <- as.character(s)
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub('"', '\\\\"', s)
  s <- gsub("\\+", "\\\\+", s)
  s
}

md2pdf_render <- function(md_path, out_pdf, color = "#DF9531",
                          template_name = "reporte_estacion.typ", media_dir = NULL) {
  if (!file.exists(md_path)) stop("Markdown no encontrado: ", md_path)
  if (!md2pdf_cmd_exists("pandoc")) stop("pandoc no instalado")
  if (!md2pdf_cmd_exists("typst")) stop("typst no instalado")

  raw <- paste(readLines(md_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  parsed <- md2pdf_parse_yaml(raw)
  meta <- parsed$meta
  body <- parsed$body

  title <- md2pdf_meta(meta, "title", "Reporte estación")
  subtitle <- md2pdf_meta(meta, "subtitle")
  author <- md2pdf_meta(meta, "author")
  institution <- md2pdf_meta(meta, "institution", "rodaja.cl")
  department <- md2pdf_meta(meta, "department", "Explorador Climático")
  date <- md2pdf_meta(meta, "date", format(Sys.Date(), "%Y-%m-%d"))
  version <- md2pdf_meta(meta, "version")
  if (!is.null(meta[["color"]]) && nzchar(meta[["color"]])) color <- meta[["color"]]
  tags <- character(0)
  tag_raw <- md2pdf_meta(meta, "tags")
  if (nzchar(tag_raw)) {
    tag_raw <- gsub("[\\[\\]]", "", tag_raw, perl = TRUE)
    tags <- trimws(strsplit(tag_raw, ",")[[1]])
    tags <- tags[nzchar(tags)]
  }

  tmp <- tempfile("md2pdf_")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  md_file <- file.path(tmp, "body.md")
  writeLines(body, md_file, useBytes = TRUE)

  src_media <- media_dir
  if (is.null(src_media) || !dir.exists(src_media)) {
    src_media <- file.path(dirname(md_path), "media")
  }
  if (!dir.exists(src_media)) {
    src_media <- file.path(dirname(md_path), "sections", "media")
  }
  if (dir.exists(src_media)) {
    dir.create(file.path(tmp, "media"), showWarnings = FALSE, recursive = TRUE)
    file.copy(list.files(src_media, full.names = TRUE),
              file.path(tmp, "media"), recursive = FALSE)
  }

  body_file <- file.path(tmp, "_body.typ")
  status <- system2("pandoc", c(md_file, "--from=markdown", "--to=typst",
                                "--wrap=none", "-o", body_file),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L) && !identical(status, 0)) stop("pandoc fallo")

  body_raw <- paste(readLines(body_file, warn = FALSE), collapse = "\n")
  writeLines(c("#let horizontalrule = line(length: 100%)", "", body_raw),
             body_file, useBytes = TRUE)

  if (length(tags) == 0) {
    t_arr <- "()"
  } else if (length(tags) == 1) {
    t_arr <- paste0('("', md2pdf_esc_typst(tags[1]), '",)')
  } else {
    t_arr <- paste0("(", paste0('"', vapply(tags, md2pdf_esc_typst, ""), '"', collapse = ", "), ")")
  }

  cfg <- paste0(
    '#let doc_title = "', md2pdf_esc_typst(title), '"', "\n",
    '#let doc_subtitle = "', md2pdf_esc_typst(subtitle), '"', "\n",
    '#let doc_author = "', md2pdf_esc_typst(author), '"', "\n",
    '#let doc_institution = "', md2pdf_esc_typst(institution), '"', "\n",
    '#let doc_department = "', md2pdf_esc_typst(department), '"', "\n",
    '#let doc_date = "', md2pdf_esc_typst(date), '"', "\n",
    '#let doc_version = "', md2pdf_esc_typst(version), '"', "\n",
    '#let doc_color = "', md2pdf_esc_typst(color), '"', "\n",
    '#let doc_tags = ', t_arr, "\n"
  )
  skip_keys <- c("title", "subtitle", "author", "institution", "department",
                "date", "version", "color", "tags")
  defaults <- c(
    station_code = "", station_lat = "", station_lon = "",
    station_region = "", station_comuna = "", station_operador = "",
    station_calidad = "", station_periodo = "",
    kpi1_label = "", kpi1_value = "", kpi1_tone = "neutral",
    kpi2_label = "", kpi2_value = "", kpi2_tone = "neutral",
    kpi3_label = "", kpi3_value = "", kpi3_tone = "neutral",
    kpi4_label = "", kpi4_value = "", kpi4_tone = "neutral",
    has_map = "false", map_pin = "false", map_fx = "50%", map_fy = "50%"
  )
  for (k in names(defaults)) {
    if (is.null(meta[[k]]) || !nzchar(as.character(meta[[k]]))) meta[[k]] <- defaults[[k]]
  }
  for (k in names(defaults)) {
    val <- meta[[k]] %||% defaults[[k]]
    if (k %in% c("map_fx", "map_fy")) {
      pct <- gsub("[^0-9.]", "", as.character(val))
      if (!nzchar(pct)) pct <- "50"
      cfg <- paste0(cfg, "#let doc_", k, " = ", pct, "%\n")
    } else {
      cfg <- paste0(cfg, '#let doc_', k, ' = "', md2pdf_esc_typst(val), '"', "\n")
    }
  }
  for (k in names(meta)) {
    if (k %in% skip_keys || k %in% names(defaults)) next
    cfg <- paste0(cfg, '#let doc_', k, ' = "',
                  md2pdf_esc_typst(meta[[k]]), '"', "\n")
  }
  writeLines(cfg, file.path(tmp, "_config.typ"), useBytes = TRUE)

  tpl_src <- file.path(md2pdf_template_dir(), template_name)
  if (!file.exists(tpl_src)) stop("Template no encontrado: ", tpl_src)
  file.copy(tpl_src, file.path(tmp, "document.typ"), overwrite = TRUE)

  pdf_tmp <- file.path(tmp, "document.pdf")
  status <- system2("typst", c("compile", file.path(tmp, "document.typ"), pdf_tmp),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L) && !identical(status, 0)) stop("typst compilation failed")

  out_dir <- dirname(out_pdf)
  if (nzchar(out_dir) && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  file.copy(pdf_tmp, out_pdf, overwrite = TRUE)
  invisible(out_pdf)
}

md2pdf_merge_sections <- function(section_pdfs, out_pdf) {
  section_pdfs <- section_pdfs[file.exists(section_pdfs)]
  if (length(section_pdfs) == 0) stop("No hay secciones PDF para merge")
  if (length(section_pdfs) == 1) {
    file.copy(section_pdfs[1], out_pdf, overwrite = TRUE)
    return(invisible(out_pdf))
  }
  if (!md2pdf_cmd_exists("qpdf")) stop("qpdf no instalado")
  args <- c("--empty", "--pages", paste(section_pdfs, collapse = " "), "--", out_pdf)
  status <- system2("qpdf", args, stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L) && !identical(status, 0)) stop("qpdf merge failed")
  invisible(out_pdf)
}
