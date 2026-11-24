# Empaquetado de lotes bulk de descarga

`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  if (is.atomic(x) && length(x) == 1L && is.na(x)) return(y)
  x
}

lote_descargas_dir <- function() {
  d <- Sys.getenv("DESCARGAS_DIR", "/tmp/descargas")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

lote_child_path <- function(job_id) {
  out_dir <- lote_descargas_dir()
  path_dir <- file.path(out_dir, as.character(job_id))
  path_zip <- file.path(out_dir, paste0(job_id, ".zip"))
  path_csv <- file.path(out_dir, paste0(job_id, ".csv"))
  if (dir.exists(path_dir)) return(path_dir)
  if (file.exists(path_zip)) path_zip else if (file.exists(path_csv)) path_csv else NULL
}

lote_username_slug <- function(conn, user_id) {
  if (is.null(user_id) || is.na(user_id) || !nzchar(as.character(user_id))) return("usuario")
  row <- DBI::dbGetQuery(conn,
    "SELECT email FROM tripulacion WHERE id = $1::uuid LIMIT 1",
    params = list(as.character(user_id))
  )
  if (nrow(row) == 0 || is.na(row$email[1]) || !nzchar(row$email[1])) return("usuario")
  local <- sub("@.*$", "", tolower(trimws(as.character(row$email[1]))))
  slug <- gsub("[^a-zA-Z0-9_-]+", "_", local)
  slug <- gsub("_+", "_", slug)
  slug <- gsub("^_|_$", "", slug)
  if (!nzchar(slug)) return("usuario")
  if (nchar(slug) > 40L) slug <- substr(slug, 1L, 40L)
  slug
}

lote_archivo_nombre <- function(conn, user_id) {
  paste0("rodaja_", lote_username_slug(conn, user_id), "_", format(Sys.Date(), "%Y%m%d"), ".zip")
}

lote_copy_child_files <- function(src, gran_dir, gran, h, archivos) {
  append_archivo <- function(dest_name, bytes) {
    archivos[[length(archivos) + 1L]] <- list(
      path = paste0(gran, "/", dest_name),
      job_id = h$id,
      granularidad = gran,
      bytes = as.integer(bytes)
    )
    archivos
  }

  if (dir.exists(src)) {
    child_files <- list.files(src, full.names = TRUE, recursive = FALSE)
    for (cf in child_files) {
      dest_name <- basename(cf)
      dest <- file.path(gran_dir, dest_name)
      file.copy(cf, dest, overwrite = TRUE)
      archivos <- append_archivo(dest_name, file.info(dest)$size)
    }
    return(archivos)
  }

  if (grepl("\\.zip$", src, ignore.case = TRUE)) {
    tmp <- tempfile(pattern = "lote_unzip_")
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(src, exdir = tmp)
    extracted <- list.files(tmp, full.names = TRUE, recursive = TRUE)
    extracted <- extracted[!dir.exists(extracted)]
    for (cf in extracted) {
      dest_name <- basename(cf)
      dest <- file.path(gran_dir, dest_name)
      file.copy(cf, dest, overwrite = TRUE)
      archivos <- append_archivo(dest_name, file.info(dest)$size)
    }
    unlink(tmp, recursive = TRUE)
    return(archivos)
  }

  dest_name <- h$archivo_nombre %||% basename(src)
  dest_name <- gsub("[/\\\\]", "_", as.character(dest_name))
  dest <- file.path(gran_dir, dest_name)
  file.copy(src, dest, overwrite = TRUE)
  append_archivo(dest_name, h$bytes_reales %||% file.info(dest)$size)
}

lote_maybe_finalize <- function(conn, lote_id) {
  if (is.null(lote_id) || !nzchar(as.character(lote_id))) return(invisible(FALSE))

  lote <- DBI::dbGetQuery(conn,
    "SELECT id, user_id, nombre, estado, params, bytes_estimados FROM fardo_de_datos WHERE id = $1",
    params = list(lote_id)
  )
  if (nrow(lote) == 0) return(invisible(FALSE))
  if (lote$estado[1] %in% c("listo", "parcial", "error")) return(invisible(TRUE))

  hijos <- DBI::dbGetQuery(conn,
    "SELECT id, estado, granularidad, bytes_reales, archivo_nombre, params
     FROM encargo_de_archivo WHERE lote_id = $1 ORDER BY creado_en",
    params = list(lote_id)
  )
  if (nrow(hijos) == 0) return(invisible(FALSE))

  pendientes <- sum(!hijos$estado %in% c("listo", "error", "fallido"), na.rm = TRUE)
  if (pendientes > 0) return(invisible(FALSE))

  out_dir <- lote_descargas_dir()
  staging <- file.path(out_dir, paste0("lote_staging_", lote_id))
  if (dir.exists(staging)) unlink(staging, recursive = TRUE)
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)

  lote_params <- tryCatch(jsonlite::fromJSON(lote$params[1]), error = function(e) list())
  archivos <- list()
  excluidos <- lote_params$excluidos
  if (is.null(excluidos)) excluidos <- list()
  errores <- list()

  for (i in seq_len(nrow(hijos))) {
    h <- hijos[i, ]
    gran <- h$granularidad %||% "anual"
    gran_dir <- file.path(staging, gran)
    dir.create(gran_dir, recursive = TRUE, showWarnings = FALSE)

    if (!identical(h$estado, "listo")) {
      errores[[length(errores) + 1L]] <- list(
        job_id = h$id,
        granularidad = gran,
        motivo = as.character(h$estado)
      )
      next
    }

    src <- lote_child_path(h$id)
    if (is.null(src) || (!dir.exists(src) && !file.exists(src))) {
      errores[[length(errores) + 1L]] <- list(job_id = h$id, granularidad = gran, motivo = "archivo_no_encontrado")
      next
    }

    archivos <- lote_copy_child_files(src, gran_dir, gran, h, archivos)
  }

  manifest <- list(
    lote_id = lote_id,
    nombre = lote$nombre[1],
    estaciones_solicitadas = as.list(as.integer(unlist(lote_params$estacion_ids))),
    estaciones_exportadas = as.list(unique(as.integer(unlist(lote_params$estacion_ids)))),
    variables = as.list(as.integer(unlist(lote_params$variable_ids))),
    granularidades = as.character(unlist(lote_params$granularidades)),
    periodo = list(inicio = lote_params$fecha_inicio, fin = lote_params$fecha_fin),
    formato = lote_params$formato_descarga %||% "liviano",
    modo_cobertura = lote_params$modo_cobertura %||% "solo_con_datos",
    excluidos = excluidos,
    errores = errores,
    archivos = archivos
  )

  readme_lines <- c(
    "rodaja.cl — Paquete de descarga bulk",
    paste0("Lote: ", lote_id),
    paste0("Nombre: ", lote$nombre[1]),
    paste0("Estaciones: ", length(manifest$estaciones_exportadas)),
    paste0("Variables: ", length(manifest$variables)),
    paste0("Granularidades: ", paste(manifest$granularidades, collapse = ", ")),
    paste0("Periodo: ", manifest$periodo$inicio, " a ", manifest$periodo$fin),
    paste0("Formato: ", manifest$formato),
  if (length(excluidos) > 0L) paste0("Pares excluidos (sin datos): ", length(excluidos)) else "Sin exclusiones por falta de datos.",
    "",
    "Estructura:",
    "  MANIFEST.json — detalle tecnico de la exportacion",
    "  README.txt — este archivo",
    "  {granularidad}/ — archivos CSV por granularidad solicitada"
  )

  writeLines(readme_lines, file.path(staging, "README.txt"), useBytes = TRUE)
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE), file.path(staging, "MANIFEST.json"))

  zip_path <- file.path(out_dir, paste0("lote_", lote_id, ".zip"))
  if (file.exists(zip_path)) unlink(zip_path)
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(staging)
  utils::zip(zip_path, files = list.files(recursive = TRUE), flags = "-r9Xq")
  unlink(staging, recursive = TRUE)

  real_bytes <- as.integer(file.info(zip_path)$size)
  n_ok <- sum(hijos$estado == "listo", na.rm = TRUE)
  estado_final <- if (n_ok == 0) "error" else if (n_ok < nrow(hijos)) "parcial" else "listo"
  fname <- lote_archivo_nombre(conn, lote$user_id[1])

  DBI::dbExecute(conn,
    "UPDATE fardo_de_datos SET estado = $2, bytes_reales = $3, archivo_nombre = $4,
      link_descarga = $5, completado_en = NOW() WHERE id = $1",
    params = list(
      lote_id, estado_final, real_bytes, fname,
      paste0("/descarga/lote/", lote_id, "/archivo")
    )
  )

  user_id <- lote$user_id[1]
  if (!is.null(user_id) && !is.na(user_id) && nzchar(as.character(user_id))) {
    tier <- worker_resolve_tier(conn, user_id)
    estimado <- as.integer(lote$bytes_estimados[1])
    overage_est <- as.integer(lote_params$bytes_overage_reservado %||% 0L)
    if (is.na(overage_est)) overage_est <- 0L
    extra_est <- as.integer(lote_params$bytes_extra_reservado %||% 0L)
    if (is.na(extra_est)) extra_est <- 0L
    if (!is.na(estimado) && estimado > 0L) {
      limite <- worker_get_limite_bytes(tier)
      periodo <- worker_periodo_inicio(conn, user_id, tier)
      df_u <- DBI::dbGetQuery(conn,
        "SELECT bytes_consumidos FROM cupo_del_periodo WHERE user_id = $1::uuid AND periodo_inicio = $2",
        params = list(user_id, periodo)
      )
      usado <- if (nrow(df_u) > 0) as.numeric(df_u$bytes_consumidos[1]) else 0
      u0 <- usado - real_bytes
      overage_real <- if (extra_est > 0L) NULL else as.integer(worker_on_demand_overage_increment(u0, limite, real_bytes))
      worker_ajustar_cuota(conn, user_id, tier, estimado, real_bytes, overage_est, overage_real, extra_est)
    }
  }

  invisible(TRUE)
}
