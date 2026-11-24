# Exportacion: un CSV con todas las estaciones (codigo_nacional) y variables pedidas.

`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

if (!exists("display_valor", mode = "function")) {
  for (ud_path in c(
    file.path(Sys.getenv("API_ROOT", "/api"), "utils", "unidades_display.R"),
    "/api/utils/unidades_display.R"
  )) {
    if (file.exists(ud_path)) {
      source(ud_path, local = FALSE)
      break
    }
  }
}

detect_var_type <- function(nombre) {
  n <- tolower(trimws(nombre %||% ""))
  if (!nzchar(n)) return("other")
  if (grepl("precip|lluvia|pp", n) && !grepl("viento", n)) return("precip")
  if (grepl("u_max_diario|u_rafaga|viento.*max", n)) return("wind_max")
  if (grepl("u2_min|viento.*min", n)) return("wind_min")
  if (grepl("u_10_kt|predomin", n)) return("wind_kt")
  if (grepl("u2_mean|viento", n)) return("wind_mean")
  "other"
}

sql_agg_expr <- function(vtype, nombre_abr = "") {
  abr <- tolower(trimws(as.character(nombre_abr %||% "")))
  if (identical(vtype, "precip") || identical(abr, "pp")) return("SUM(o.valor)")
  if (identical(vtype, "wind_max") || abr %in% c("u_max_diario", "u_rafaga")) return("MAX(o.valor)")
  if (identical(vtype, "wind_min") || identical(abr, "u2_min")) return("MIN(o.valor)")
  "AVG(o.valor)"
}

safe_slug <- function(x, max_len = 40L) {
  s <- tolower(trimws(as.character(x %||% "dato")))
  s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  if (!nzchar(s)) s <- "dato"
  if (nchar(s) > max_len) s <- substr(s, 1L, max_len)
  s
}

normalize_granularidad <- function(granularidad) {
  g <- tolower(trimws(as.character(granularidad %||% "anual")))
  typos <- c(
    diaio = "diario", dfiario = "diario", diaro = "diario", diari = "diario",
    aual = "anual", anual = "anual",
    mensual = "mensual", mensul = "mensual"
  )
  if (g %in% names(typos)) return(unname(typos[g]))
  if (g %in% c("horaria", "hora")) return("horaria")
  if (g %in% c("diario", "dia", "d")) return("diario")
  if (g %in% c("mensual", "mes", "m")) return("mensual")
  if (g %in% c("anual", "an", "a")) return("anual")
  if (grepl("^di", g)) return("diario")
  if (grepl("^men", g)) return("mensual")
  if (grepl("^anu", g)) return("anual")
  "anual"
}

fecha_rango_slug <- function(fecha_inicio, fecha_fin) {
  f_ini <- as.Date(fecha_inicio)
  f_fin <- as.Date(fecha_fin)
  if (is.na(f_ini) || is.na(f_fin)) {
    return(paste0(gsub("-", "", fecha_inicio), "_", gsub("-", "", fecha_fin)))
  }
  paste0(format(f_ini, "%Y"), "_", format(f_fin, "%Y"))
}

fetch_variable_meta <- function(conn, vid) {
  var <- DBI::dbGetQuery(
    conn,
    "SELECT id, COALESCE(nombre_abr, '') AS nombre_abr, COALESCE(nombre, nombre_abr, '') AS nombre FROM magnitud WHERE id = $1",
    params = list(vid)
  )
  if (nrow(var) == 0) {
    return(list(id = vid, nombre_abr = "", nombre = paste0("var_", vid)))
  }
  list(
    id = var$id[1],
    nombre_abr = var$nombre_abr[1],
    nombre = var$nombre[1]
  )
}

var_col_name <- function(nombre_abr, nombre = "", vid = NA) {
  abr <- tolower(trimws(as.character(nombre_abr %||% "")))
  abr <- gsub("[^a-z0-9_]+", "_", abr)
  abr <- gsub("_+", "_", abr)
  abr <- gsub("^_|_$", "", abr)

  alias <- c(
    t_mi = "t_min", tmin = "t_min",
    t_ma = "t_max", tmax = "t_max",
    t_me = "t_mean", tme = "t_mean"
  )
  if (nzchar(abr) && abr %in% names(alias)) return(unname(alias[abr]))
  if (identical(abr, "u_rafaga_h")) return("u_rafaga_h")
  if (identical(abr, "u_rafaga")) return("u_rafaga")
  if (identical(abr, "u2_dire")) return("u2_dire")

  n <- tolower(trimws(as.character(nombre %||% "")))
  if (grepl("temperatura.*min|temp.*min|m[ií]nima", n)) return("t_min")
  if (grepl("temperatura.*max|temp.*max|m[aá]xima", n)) return("t_max")
  if (grepl("temperatura.*(media|mean)|temp.*mean", n)) return("t_mean")
  if (grepl("precip|lluvia", n)) return("pp")
  if (grepl("humedad", n)) return("hr_mean")
  if (grepl("presi[oó]n", n)) return("pa")
  if (grepl("viento.*predomin|predominante|u_10_kt|viento_velocidad", n)) return("u_10_kt")
  if (grepl("viento.*dire|direcci[o\u00f3]n.*viento|u_10_dire", n)) return("u_10_dire")
  if (grepl("u_rafaga_h|viento_rafaga_horaria|rafaga horaria|r[aá]faga horaria", n)) return("u_rafaga_h")
  if (grepl("u_rafaga|viento_maximo_dmc|maximo dmc|m[aá]ximo dmc", n)) return("u_rafaga")
  if (grepl("viento.*max", n)) return("u_max_diario")
  if (grepl("viento.*min", n)) return("u2_min")
  if (grepl("viento", n)) return("u2_mean")
  if (grepl("caudal", n)) return("q")
  if (grepl("radiaci", n)) return("rs_mean")

  if (nzchar(abr)) return(abr)
  n_slug <- safe_slug(nombre, max_len = 20L)
  if (nzchar(n_slug) && !identical(n_slug, "dato")) return(n_slug)
  paste0("var_", as.integer(vid))
}

export_bundle_name <- function(formato, estacion_ids, variable_ids, fecha_inicio, fecha_fin, granularidad) {
  gran <- normalize_granularidad(granularidad)
  paste0(
    "rodaja_", formato, "_", gran, "_",
    fecha_rango_slug(fecha_inicio, fecha_fin), "_",
    length(estacion_ids), "est_", length(variable_ids), "var"
  )
}

consolidated_file_name <- function(formato, estacion_ids, variable_ids, fecha_inicio, fecha_fin, granularidad) {
  paste0(export_bundle_name(formato, estacion_ids, variable_ids, fecha_inicio, fecha_fin, granularidad), ".csv")
}

fecha_col_for_gran <- function(granularidad) {
  gran <- normalize_granularidad(granularidad)
  if (identical(gran, "mensual")) {
    "to_char(make_date(o.year, o.month, 1), 'YYYY-MM-DD')"
  } else if (identical(gran, "anual")) {
    "to_char(make_date(o.year, 1, 1), 'YYYY-MM-DD')"
  } else {
    "to_char(o.fecha, 'YYYY-MM-DD')"
  }
}

export_vacio <- function() {
  data.frame(fecha = character(), valor = numeric(), stringsAsFactors = FALSE)
}

es_rafaga_horaria <- function(nombre_abr) {
  identical(tolower(trimws(as.character(nombre_abr %||% ""))), "u_rafaga_h")
}

fetch_combo_df <- function(conn, eid, vid, fecha_inicio, fecha_fin, granularidad) {
  var_meta <- fetch_variable_meta(conn, vid)
  gran <- normalize_granularidad(granularidad)
  # Ráfaga horaria no se aplasta al máximo del día. Solo sale con granularidad horaria.
  if (es_rafaga_horaria(var_meta$nombre_abr)) {
    if (!identical(gran, "horaria")) return(export_vacio())
    q <- paste0(
      "SELECT to_char(o.ts AT TIME ZONE 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') AS fecha, o.valor ",
      "FROM reloj_de_horas o ",
      "WHERE o.id_estacion = ", as.integer(eid), " AND o.id_variable = ", as.integer(vid), " ",
      "AND o.ts >= ($1::date::timestamp AT TIME ZONE 'UTC') ",
      "AND o.ts < (($2::date + 1)::timestamp AT TIME ZONE 'UTC') ",
      "ORDER BY o.ts"
    )
    df <- DBI::dbGetQuery(conn, q, params = list(fecha_inicio, fecha_fin))
    if (nrow(df) == 0) return(export_vacio())
    df$fecha <- as.character(df$fecha)
    df$valor <- display_valor(df$valor, var_meta$nombre_abr)
    attr(df, "var_col") <- var_col_name(var_meta$nombre_abr, var_meta$nombre, vid)
    return(df)
  }
  if (identical(gran, "horaria")) return(export_vacio())

  vtype <- detect_var_type(paste(var_meta$nombre, var_meta$nombre_abr))
  agg <- sql_agg_expr(vtype, var_meta$nombre_abr)
  fecha_sql <- fecha_col_for_gran(gran)

  if (identical(gran, "diario")) {
    q <- paste0(
      "SELECT ", fecha_sql, " AS fecha, o.valor ",
      "FROM bitacora_oficial o ",
      "WHERE o.id_estacion = ", eid, " AND o.id_variable = ", vid,
      " AND o.fecha >= $1 AND o.fecha <= $2 ",
      "ORDER BY o.fecha"
    )
    df <- DBI::dbGetQuery(conn, q, params = list(fecha_inicio, fecha_fin))
  } else if (identical(gran, "mensual")) {
    q <- paste0(
      "SELECT ", fecha_sql, " AS fecha, ", agg, " AS valor ",
      "FROM bitacora_oficial o ",
      "WHERE o.id_estacion = ", eid, " AND o.id_variable = ", vid,
      " AND o.fecha >= $1 AND o.fecha <= $2 ",
      "GROUP BY o.year, o.month ORDER BY o.year, o.month"
    )
    df <- DBI::dbGetQuery(conn, q, params = list(fecha_inicio, fecha_fin))
  } else {
    q <- paste0(
      "SELECT ", fecha_sql, " AS fecha, ", agg, " AS valor ",
      "FROM bitacora_oficial o ",
      "WHERE o.id_estacion = ", eid, " AND o.id_variable = ", vid,
      " AND o.fecha >= $1 AND o.fecha <= $2 ",
      "GROUP BY o.year ORDER BY o.year"
    )
    df <- DBI::dbGetQuery(conn, q, params = list(fecha_inicio, fecha_fin))
  }

  if (nrow(df) == 0) return(df)
  df$fecha <- as.character(df$fecha)
  df$valor <- display_valor(df$valor, var_meta$nombre_abr)
  attr(df, "var_col") <- var_col_name(var_meta$nombre_abr, var_meta$nombre, vid)
  df
}

write_csv_df <- function(df, path, append = FALSE, col_names = TRUE) {
  write.table(df, path, sep = ",", row.names = FALSE, col.names = col_names, append = append, na = "")
}

fetch_estacion_meta <- function(conn, estacion_ids) {
  ids <- paste(as.integer(estacion_ids), collapse = ",")
  DBI::dbGetQuery(conn, paste0(
    "SELECT e.codigo_nacional, e.id AS estacion_id, e.nombre, e.lat, e.lon, ",
    "e.nom_reg, e.nom_com, e.macrozona, e.altura, e.id_fuente, ",
    "e.nom_cuen AS cuenca, e.nom_subc AS subcuenca ",
    "FROM mirador e WHERE e.id IN (", ids, ") ORDER BY e.id"
  ))
}

fetch_variables_meta <- function(conn, variable_ids) {
  ids <- paste(as.integer(variable_ids), collapse = ",")
  df <- DBI::dbGetQuery(conn, paste0(
    "SELECT v.id AS variable_id, COALESCE(v.nombre, v.nombre_abr, '') AS nombre, ",
    "COALESCE(v.nombre_abr, '') AS nombre_abr, COALESCE(v.unidad, '') AS unidad ",
    "FROM magnitud v WHERE v.id IN (", ids, ") ORDER BY v.id"
  ))
  if (nrow(df) > 0 && exists("display_unidad", mode = "function")) {
    df$unidad <- mapply(display_unidad, df$nombre_abr, df$unidad, USE.NAMES = FALSE)
  }
  df
}

bundle_files <- function(base_path, files) {
  files <- files[file.exists(files)]
  if (length(files) == 0) stop("No hay archivos para empaquetar")
  zip_path <- paste0(base_path, ".zip")
  args <- c("-j", shQuote(normalizePath(zip_path, mustWork = FALSE)), shQuote(normalizePath(files, mustWork = TRUE)))
  status <- suppressWarnings(system2("zip", args, stdout = FALSE, stderr = FALSE))
  if (identical(status, 0L) || identical(status, 0)) return(zip_path)
  tar_path <- paste0(base_path, ".tar.gz")
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(dirname(files[1]))
  utils::tar(tar_path, files = basename(files), compression = "gzip")
  tar_path
}

store_export <- function(source_path, job_id, out_dir, archivo_nombre, as_zip = FALSE) {
  ext <- if (isTRUE(as_zip)) ".zip" else ".csv"
  dest <- file.path(out_dir, paste0(job_id, ext))
  file.copy(source_path, dest, overwrite = TRUE)
  list(path = dest, archivo_nombre = archivo_nombre)
}

is_lote_child_export <- function(params) {
  lid <- params$lote_id %||% NULL
  !is.null(lid) && nzchar(as.character(lid))
}

store_export_folder <- function(file_paths, job_id, out_dir, folder_label) {
  dest_dir <- file.path(out_dir, as.character(job_id))
  if (dir.exists(dest_dir)) unlink(dest_dir, recursive = TRUE)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  file_paths <- unique(file_paths[file.exists(file_paths)])
  if (length(file_paths) == 0L) stop("No hay archivos para exportar en carpeta de lote")
  for (fp in file_paths) {
    file.copy(fp, file.path(dest_dir, basename(fp)), overwrite = TRUE)
  }
  for (fp in file_paths) {
    if (file.exists(fp) && normalizePath(dirname(fp)) == normalizePath(out_dir)) {
      tryCatch(unlink(fp), error = function(e) invisible(NULL))
    }
  }
  list(path = dest_dir, archivo_nombre = folder_label %||% as.character(job_id), is_dir = TRUE)
}

export_total_bytes <- function(path) {
  if (dir.exists(path)) {
    files <- list.files(path, recursive = TRUE, full.names = TRUE)
    if (length(files) == 0L) return(0L)
    as.integer(sum(file.info(files)$size, na.rm = TRUE))
  } else {
    as.integer(file.info(path)$size)
  }
}

fetch_codigos_nacionales <- function(conn, estacion_ids) {
  ids <- unique(as.integer(estacion_ids))
  ids <- ids[!is.na(ids)]
  if (length(ids) == 0L) return(setNames(character(0), character(0)))
  df <- DBI::dbGetQuery(conn, paste0(
    "SELECT e.id AS estacion_id, e.codigo_nacional ",
    "FROM mirador e WHERE e.id IN (", paste(ids, collapse = ","), ")"
  ))
  stats::setNames(as.character(df$codigo_nacional), as.character(df$estacion_id))
}

codigo_nacional_de <- function(codigos, eid) {
  cod <- unname(codigos[as.character(as.integer(eid))])
  if (length(cod) == 0L || is.na(cod) || !nzchar(cod)) return(as.character(eid))
  cod
}

collect_combo_parts <- function(conn, estacion_ids, variable_ids, fecha_inicio, fecha_fin, granularidad) {
  codigos <- fetch_codigos_nacionales(conn, estacion_ids)
  parts <- list()
  for (eid in estacion_ids) {
    codigo <- codigo_nacional_de(codigos, eid)
    for (vid in variable_ids) {
      df <- fetch_combo_df(conn, eid, vid, fecha_inicio, fecha_fin, granularidad)
      if (nrow(df) == 0) next
      parts[[length(parts) + 1L]] <- list(
        df = df,
        eid = as.integer(eid),
        codigo_nacional = codigo,
        vid = as.integer(vid),
        var_col = attr(df, "var_col")
      )
    }
  }
  parts
}

to_consolidated_long <- function(parts, include_variable_id = FALSE) {
  if (length(parts) == 0) return(data.frame())
  rows <- lapply(parts, function(p) {
    row <- data.frame(
      fecha = p$df$fecha,
      codigo_nacional = p$codigo_nacional,
      variable = p$var_col,
      valor = p$df$valor,
      stringsAsFactors = FALSE
    )
    if (isTRUE(include_variable_id)) {
      row <- data.frame(
        fecha = row$fecha,
        codigo_nacional = row$codigo_nacional,
        variable = row$variable,
        variable_id = p$vid,
        valor = row$valor,
        stringsAsFactors = FALSE
      )
    }
    row
  })
  do.call(rbind, rows)
}

generar_liviano <- function(conn, params, out_dir, job_id) {
  estacion_ids <- as.integer(params$estacion_ids)
  variable_ids <- as.integer(params$variable_ids)
  fecha_inicio <- params$fecha_inicio
  fecha_fin <- params$fecha_fin
  granularidad <- normalize_granularidad(params$granularidad %||% "anual")

  parts <- collect_combo_parts(conn, estacion_ids, variable_ids, fecha_inicio, fecha_fin, granularidad)
  if (length(parts) == 0) stop("Sin datos para exportar")

  fname <- consolidated_file_name("liviano", estacion_ids, variable_ids, fecha_inicio, fecha_fin, granularidad)
  fpath <- file.path(out_dir, fname)
  write_csv_df(to_consolidated_long(parts), fpath)

  if (is_lote_child_export(params)) {
    return(store_export_folder(fpath, job_id, out_dir, paste0(granularidad, "_datos")))
  }
  store_export(fpath, job_id, out_dir, fname, as_zip = FALSE)
}

generar_completo <- function(conn, params, out_dir, job_id) {
  estacion_ids <- as.integer(params$estacion_ids)
  variable_ids <- as.integer(params$variable_ids)
  fecha_inicio <- params$fecha_inicio
  fecha_fin <- params$fecha_fin
  granularidad <- normalize_granularidad(params$granularidad %||% "anual")

  parts <- collect_combo_parts(conn, estacion_ids, variable_ids, fecha_inicio, fecha_fin, granularidad)
  if (length(parts) == 0) stop("Sin datos para exportar")

  meta_est_path <- file.path(out_dir, "metadatos_estaciones.csv")
  meta_var_path <- file.path(out_dir, "metadatos_variables.csv")
  write_csv_df(fetch_estacion_meta(conn, estacion_ids), meta_est_path)
  write_csv_df(fetch_variables_meta(conn, variable_ids), meta_var_path)

  datos_path <- file.path(out_dir, "datos.csv")
  write_csv_df(to_consolidated_long(parts, include_variable_id = TRUE), datos_path)
  files <- c(datos_path, meta_est_path, meta_var_path)

  if (is_lote_child_export(params)) {
    return(store_export_folder(files, job_id, out_dir, paste0(granularidad, "_completo")))
  }
  bundle_base <- file.path(
    out_dir,
    export_bundle_name("completo", estacion_ids, variable_ids, fecha_inicio, fecha_fin, granularidad)
  )
  zip_path <- bundle_files(bundle_base, files)
  store_export(zip_path, job_id, out_dir, paste0(basename(bundle_base), ".zip"), as_zip = TRUE)
}

#' Genera archivo de descarga (CSV liviano o ZIP completo)
generar_archivo <- function(params, archivo_tmp, formato = "csv") {
  conn <- get_db_connection()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  fmt <- tolower(params$formato_descarga %||% "liviano")
  job_id <- params$job_id %||% tools::file_path_sans_ext(basename(archivo_tmp))
  out_dir <- dirname(archivo_tmp)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  path <- if (identical(fmt, "completo")) {
    generar_completo(conn, params, out_dir, job_id)
  } else {
    generar_liviano(conn, params, out_dir, job_id)
  }
  if (is.character(path)) path <- list(path = path)

  bytes_reales <- export_total_bytes(path$path)
  if (is.na(bytes_reales) || bytes_reales < 1L) stop("Archivo exportado vacio")

  invisible(list(
    path = path$path,
    bytes_reales = bytes_reales,
    registros_reales = NA_integer_,
    archivo_nombre = path$archivo_nombre %||% basename(path$path)
  ))
}

# ponytail: u_rafaga_h no puede etiquetarse como la ráfaga diaria
stopifnot(identical(var_col_name("u_rafaga_h", "viento_rafaga_horaria", 21L), "u_rafaga_h"))
stopifnot(identical(var_col_name("u_rafaga", "viento_rafaga", 19L), "u_rafaga"))
stopifnot(identical(normalize_granularidad("horaria"), "horaria"))
stopifnot(!identical(normalize_granularidad("horaria"), "anual"))
