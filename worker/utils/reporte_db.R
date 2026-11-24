# Operaciones DB ficha_del_mirador + elegibilidad excelente.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

reporte_bootstrap_api_utils <- function(api_root) {
  if (!nzchar(api_root) || !dir.exists(api_root)) return(invisible(FALSE))
  for (f in c("db.R", "calidad.R", "alerta_temprana.R", "alertas_catalog.R",
              "alertas_skill.R", "unidades_display.R", "reporte_pdf_offer.R")) {
    p <- file.path(api_root, "utils", f)
    if (file.exists(p)) source(p, local = FALSE)
  }
  if (!exists("db_query", mode = "function")) {
    assign("db_query", function(q, params = NULL) {
      conn <- get_db_connection()
      on.exit(tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL))
      if (is.null(params) || length(params) == 0) DBI::dbGetQuery(conn, q) else DBI::dbGetQuery(conn, q, params = params)
    }, envir = .GlobalEnv)
  }
  invisible(TRUE)
}

reporte_tabla_existe <- function(conn) {
  df <- DBI::dbGetQuery(conn,
    "SELECT 1 AS ok FROM information_schema.tables WHERE table_name = 'ficha_del_mirador' LIMIT 1")
  nrow(df) > 0
}

reporte_estacion_elegible <- function(conn, eid) {
  if (exists("alerta_temprana_estacion_elegible", mode = "function")) {
    return(alerta_temprana_estacion_elegible(eid))
  }
  mv <- if (exists("mv_estacion_calidad_nombre", mode = "function")) mv_estacion_calidad_nombre() else "estacion_calidad"
  df <- DBI::dbGetQuery(conn, paste0(
    "SELECT 1 FROM ", mv, " cal WHERE cal.id_estacion = $1",
    " AND COALESCE(cal.nivel, 'limitada') = 'excelente' LIMIT 1"
  ), params = list(as.integer(eid)))
  nrow(df) > 0
}

reporte_eids_excelente <- function(conn) {
  mv <- if (exists("mv_estacion_calidad_nombre", mode = "function")) mv_estacion_calidad_nombre() else "estacion_calidad"
  df <- DBI::dbGetQuery(conn, paste0(
    "SELECT cal.id_estacion FROM ", mv, " cal",
    " WHERE COALESCE(cal.nivel, 'limitada') = 'excelente'",
    " ORDER BY cal.id_estacion"
  ))
  if (nrow(df) == 0) return(integer(0))
  as.integer(df$id_estacion)
}

reporte_db_get <- function(conn, eid) {
  df <- DBI::dbGetQuery(conn,
    "SELECT * FROM ficha_del_mirador WHERE id_estacion = $1",
    params = list(as.integer(eid)))
  if (nrow(df) == 0) return(NULL)
  df[1, ]
}

reporte_db_upsert <- function(conn, eid, fields) {
  eid <- as.integer(eid)
  row <- reporte_db_get(conn, eid)
  estado <- fields$estado %||% row$estado %||% "pendiente"
  elegible <- isTRUE(fields$elegible %||% row$elegible %||% FALSE)
  congelado <- fields$congelado_desde %||% row$congelado_desde
  ult <- fields$ultima_actualizacion %||% row$ultima_actualizacion
  ult_p <- fields$ultima_actualizacion_pesada %||% row$ultima_actualizacion_pesada
  ver <- as.integer(fields$version_schema %||% row$version_schema %||% 1L)
  err <- fields$error_msg %||% row$error_msg
  if (length(congelado) == 0) congelado <- NA
  if (length(ult) == 0) ult <- NA
  if (length(ult_p) == 0) ult_p <- NA
  if (length(err) == 0 || (length(err) == 1 && is.na(err))) err <- NA_character_

  if (is.null(row)) {
    DBI::dbExecute(conn,
      "INSERT INTO ficha_del_mirador (id_estacion, estado, elegible, congelado_desde,
        ultima_actualizacion, ultima_actualizacion_pesada, version_schema, error_msg)
       VALUES ($1, $2, $3, $4, $5, $6, $7, $8)",
      params = list(eid, estado, elegible, congelado, ult, ult_p, ver, err))
  } else {
    DBI::dbExecute(conn,
      "UPDATE ficha_del_mirador SET estado = $2, elegible = $3, congelado_desde = $4,
        ultima_actualizacion = $5, ultima_actualizacion_pesada = $6,
        version_schema = $7, error_msg = $8, actualizado_en = NOW()
       WHERE id_estacion = $1",
      params = list(eid, estado, elegible, congelado, ult, ult_p, ver, err))
  }
  invisible(TRUE)
}

reporte_db_list_activos <- function(conn) {
  df <- DBI::dbGetQuery(conn,
    "SELECT id_estacion FROM ficha_del_mirador WHERE estado = 'activo' AND elegible = TRUE")
  if (nrow(df) == 0) return(integer(0))
  as.integer(df$id_estacion)
}

reporte_db_list_pendientes <- function(conn) {
  df <- DBI::dbGetQuery(conn,
    "SELECT id_estacion FROM ficha_del_mirador WHERE estado = 'pendiente'")
  if (nrow(df) == 0) return(integer(0))
  as.integer(df$id_estacion)
}

reporte_db_list_errores <- function(conn) {
  df <- DBI::dbGetQuery(conn,
    "SELECT id_estacion FROM ficha_del_mirador WHERE estado = 'error'")
  if (nrow(df) == 0) return(integer(0))
  as.integer(df$id_estacion)
}

# Mismo predicado que GET/status: excelente y skill si hay masa critica.
reporte_eids_pdf_elegibles <- function(conn) {
  excelente <- reporte_eids_excelente(conn)
  if (length(excelente) == 0L) return(integer(0))
  n_eleg <- tryCatch(
    as.integer(DBI::dbGetQuery(conn,
      "SELECT COUNT(*)::int AS n FROM oficio_del_mirador WHERE elegible = TRUE")$n[1]),
    error = function(e) 0L
  )
  min_e <- if (exists("alerta_skill_min_elegibles_enforce", mode = "function")) {
    alerta_skill_min_elegibles_enforce()
  } else {
    30L
  }
  if (is.na(n_eleg) || n_eleg < min_e) return(excelente)
  sk <- tryCatch(
    DBI::dbGetQuery(conn, "SELECT id_estacion FROM oficio_del_mirador WHERE elegible = TRUE"),
    error = function(e) data.frame(id_estacion = integer(0))
  )
  if (nrow(sk) == 0) return(integer(0))
  intersect(excelente, as.integer(sk$id_estacion))
}

# Sync ficha_del_mirador con oferta PDF (excelente + skill), no solo nivel.
reporte_sync_elegibilidad <- function(conn, log_fn = message) {
  excelente <- reporte_eids_excelente(conn)
  eligible <- reporte_eids_pdf_elegibles(conn)
  eligible_set <- as.character(eligible)
  all_rows <- DBI::dbGetQuery(conn, "SELECT id_estacion, estado, elegible FROM ficha_del_mirador")
  known <- if (nrow(all_rows) > 0) as.character(all_rows$id_estacion) else character(0)
  n_new <- 0L
  n_cong <- 0L
  n_react <- 0L

  for (eid in eligible) {
    eid_s <- as.character(eid)
    if (!eid_s %in% known) {
      reporte_db_upsert(conn, eid, list(estado = "pendiente", elegible = TRUE, error_msg = NULL))
      log_fn("[reporte-sync] nueva elegible eid=", eid, " → pendiente")
      n_new <- n_new + 1L
    }
  }

  if (nrow(all_rows) > 0) {
    for (i in seq_len(nrow(all_rows))) {
      eid <- all_rows$id_estacion[i]
      eid_s <- as.character(eid)
      is_ok <- eid_s %in% eligible_set
      st <- as.character(all_rows$estado[i])

      if (is_ok) {
        if (!isTRUE(all_rows$elegible[i]) || st == "congelado") {
          reporte_db_upsert(conn, eid, list(elegible = TRUE, congelado_desde = NULL))
        }
        if (st == "congelado") {
          reporte_db_upsert(conn, eid, list(estado = "pendiente"))
          log_fn("[reporte-sync] recongelada→pendiente eid=", eid)
          n_react <- n_react + 1L
        }
      } else {
        if (st %in% c("activo", "pendiente", "error")) {
          manifest <- tryCatch(reporte_manifest_get(eid), error = function(e) NULL)
          if (!is.null(manifest)) {
            manifest$frozen <- TRUE
            reporte_save_manifest(eid, manifest)
          }
          reporte_db_upsert(conn, eid, list(
            estado = "congelado", elegible = FALSE,
            congelado_desde = Sys.time()
          ))
          log_fn("[reporte-sync] eid=", eid, " → congelado")
          n_cong <- n_cong + 1L
        } else if (isTRUE(all_rows$elegible[i])) {
          reporte_db_upsert(conn, eid, list(elegible = FALSE))
        }
      }
    }
  }

  list(
    excelente = length(excelente),
    elegibles = length(eligible),
    nuevas = n_new,
    congeladas = n_cong,
    reactivadas = n_react
  )
}

reporte_fetch_estacion_meta <- function(conn, eid) {
  df <- DBI::dbGetQuery(conn,
    "SELECT e.id, e.nombre, e.lat, e.lon, e.altura, e.nom_cuen AS cuenca,
            e.nom_reg, e.nom_com, f.nombre AS operador, e.codigo_nacional AS codigo
     FROM mirador e
     LEFT JOIN cantera f ON e.id_fuente = f.id
     WHERE e.id = $1",
    params = list(as.integer(eid)))
  if (nrow(df) == 0) return(NULL)
  as.list(df[1, ])
}

reporte_fetch_variables <- function(conn, eid) {
  df <- DBI::dbGetQuery(conn,
    "SELECT v.id, v.nombre, v.nombre_abr, MIN(od.fecha) AS fecha_min, MAX(od.fecha) AS fecha_max,
            COUNT(*) AS n_registros
     FROM jornada_cerrada od
     JOIN magnitud v ON v.id = od.id_variable
     WHERE od.id_estacion = $1
     GROUP BY v.id, v.nombre, v.nombre_abr
     ORDER BY v.nombre",
    params = list(as.integer(eid)))
  if (nrow(df) == 0) return(df)
  if (!"nombre_abr" %in% names(df)) df$nombre_abr <- NA_character_
  df
}

reporte_fetch_daily_series <- function(conn, eid, vid, fecha_min = NULL) {
  q <- "SELECT fecha, valor FROM jornada_cerrada
        WHERE id_estacion = $1 AND id_variable = $2"
  params <- list(as.integer(eid), as.integer(vid))
  if (!is.null(fecha_min) && nzchar(fecha_min)) {
    q <- paste0(q, " AND fecha >= $3")
    params <- c(params, list(as.character(fecha_min)))
  }
  q <- paste0(q, " ORDER BY fecha")
  df <- DBI::dbGetQuery(conn, q, params = params)
  if (nrow(df) == 0) return(df)
  df$fecha <- as.Date(df$fecha)
  df
}

reporte_fetch_pronostico <- function(conn, eid) {
  DBI::dbGetQuery(conn,
    "SELECT pe.fecha_objetivo, pe.valor, v.nombre AS variable, v.nombre_abr, v.unidad
     FROM carta_del_mirador pe
     JOIN magnitud v ON v.id = pe.id_variable
     WHERE pe.id_estacion = $1 AND pe.fecha_objetivo >= CURRENT_DATE
     ORDER BY pe.fecha_objetivo, v.nombre",
    params = list(as.integer(eid)))
}

reporte_display_pronostico_df <- function(pron) {
  if (is.null(pron) || nrow(pron) == 0 || !"nombre_abr" %in% names(pron)) return(pron)
  wind <- vapply(pron$nombre_abr, is_wind_abr, logical(1))
  if (!any(wind)) return(pron)
  for (i in which(wind)) {
    pron$valor[i] <- display_valor(pron$valor[i], pron$nombre_abr[i])
    if ("unidad" %in% names(pron)) {
      pron$unidad[i] <- display_unidad(pron$nombre_abr[i], pron$unidad[i])
    }
  }
  pron
}
