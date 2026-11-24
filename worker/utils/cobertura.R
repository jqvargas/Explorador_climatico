# Resumen de calidad del mapa (vista materializada estacion_calidad v2).
# Score v2 + gates definidos en la vista materializada.
# El REFRESH post-ingesta recalcula vigencia, continuidad y niveles automaticamente.
#
# Llamar post_ingesta_observaciones() tras ingesta (con fecha_min/fecha_max) o cuando
# jornada_cerrada quede desalineada. No usar tras exportaciones CSV (solo lectura).

mv_estacion_calidad_existe <- function(conn) {
  row <- DBI::dbGetQuery(conn, "SELECT to_regclass('public.estacion_calidad') AS t")
  !is.null(row) && nrow(row) > 0 && !is.na(row$t[1])
}

mv_estacion_cobertura_existe <- function(conn) {
  row <- DBI::dbGetQuery(conn, "SELECT to_regclass('public.estacion_cobertura') AS t")
  !is.null(row) && nrow(row) > 0 && !is.na(row$t[1])
}

mv_estacion_variable_anio_existe <- function(conn) {
  row <- DBI::dbGetQuery(conn, "SELECT to_regclass('public.estacion_variable_anio') AS t")
  !is.null(row) && nrow(row) > 0 && !is.na(row$t[1])
}

refresh_estacion_variable_anio <- function(conn) {
  if (!mv_estacion_variable_anio_existe(conn)) {
    message("[calidad] estacion_variable_anio no existe; omitiendo refresh.")
    return(invisible(FALSE))
  }
  t0 <- Sys.time()
  DBI::dbExecute(conn, "REFRESH MATERIALIZED VIEW CONCURRENTLY estacion_variable_anio")
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  message("[calidad] estacion_variable_anio refrescada en ", elapsed, " s")
  invisible(TRUE)
}

refresh_estacion_calidad <- function(conn) {
  ok <- FALSE
  if (mv_estacion_calidad_existe(conn)) {
    t0 <- Sys.time()
    DBI::dbExecute(conn, "REFRESH MATERIALIZED VIEW CONCURRENTLY estacion_calidad")
    elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
    message("[calidad] estacion_calidad refrescada en ", elapsed, " s")
    ok <- TRUE
  } else if (mv_estacion_cobertura_existe(conn)) {
    t0 <- Sys.time()
    DBI::dbExecute(conn, "REFRESH MATERIALIZED VIEW CONCURRENTLY estacion_cobertura")
    elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
    message("[calidad] estacion_cobertura refrescada en ", elapsed, " s (legacy)")
    ok <- TRUE
  } else {
    message("[calidad] Vista estacion_calidad no existe; omitiendo refresh.")
  }
  refresh_estacion_variable_anio(conn)
  invisible(ok)
}

refresh_estacion_cobertura <- refresh_estacion_calidad

invalidate_estaciones_cache <- function() {
  if (!requireNamespace("redux", quietly = TRUE)) return(invisible(FALSE))
  host <- Sys.getenv("REDIS_HOST", "redis")
  port <- as.integer(Sys.getenv("REDIS_PORT", "6379"))
  r <- tryCatch(redux::hiredis(host = host, port = port), error = function(e) NULL)
  if (is.null(r)) return(invisible(FALSE))
  n <- 0L
  tryCatch({
    cursor <- "0"
    repeat {
      out <- r$SCAN(cursor, "MATCH", "estaciones:*", "COUNT", 200)
      cursor <- out[[1]]
      keys <- out[[2]]
      if (length(keys) > 0) {
        do.call(r$DEL, as.list(keys))
        n <- n + length(keys)
      }
      if (cursor == "0") break
    }
  }, error = function(e) NULL)
  tryCatch(r$DEL("descarga:pares-variable"), error = function(e) NULL)
  if (n > 0L) message("[calidad] Cache Redis invalidada (", n, " claves estaciones:*)")
  invisible(n > 0L)
}

post_ingesta_observaciones <- function(conn, n_rows_inserted = 0L, scope_estaciones = NULL,
                                       fecha_min = NULL, fecha_max = NULL) {
  n <- as.integer(if (is.null(n_rows_inserted) || length(n_rows_inserted) == 0L) 0L else n_rows_inserted[1])
  if (is.na(n)) n <- 0L

  fmin <- tryCatch(as.Date(fecha_min), error = function(e) NA)
  fmax <- tryCatch(as.Date(fecha_max), error = function(e) NA)
  tiene_rango <- !is.na(fmin) && !is.na(fmax)
  atrasada <- tiene_rango && exists("observacion_diaria_atrasada", mode = "function") &&
    observacion_diaria_atrasada(conn, fmax)
  rev <- if (exists("QC_REV")) QC_REV else 1L
  pendiente <- tiene_rango && exists("qc_mv_ventana_pendiente", mode = "function") &&
    tryCatch(qc_mv_ventana_pendiente(conn, fmin, fmax, rev), error = function(e) FALSE)
  debe_qc <- tiene_rango && (n > 0L || isTRUE(pendiente))
  debe_sync <- (n > 0L || atrasada || debe_qc) && tiene_rango

  if (!debe_sync) {
    message("[calidad] jornada_cerrada al dia; omitiendo sync/refresh.")
    return(invisible(FALSE))
  }

  if (debe_qc) {
    motivo <- if (n > 0L) paste0(n, " fila(s) ingestadas") else "qc_rev pendiente en la ventana"
    message("[calidad] ", motivo, "; ejecutando QC multinivel...")
    # Mismo punto de entrada que usa el backfill historico: la cascada completa
    # (rango duro, consistencia logica, estacional, salto temporal, arbitraje con
    # vecinas/reanalisis) mas el sello qc_rev. Las consistencias se invocan desde
    # dentro, no aparte, para que no haya dos caminos que puedan divergir.
    if (exists("qc_multinivel_procesar", mode = "function")) {
      tryCatch(
        qc_multinivel_procesar(conn, fmin, fmax, scope_estaciones = scope_estaciones),
        error = function(e) message("[calidad] QC multinivel omitido: ", conditionMessage(e))
      )
    } else {
      message("[calidad] qc_multinivel no cargado; usando solo consistencias.")
      tryCatch({
        qc_consistencia_temperatura(conn, tol = 0.5, scope_estaciones = scope_estaciones)
        qc_consistencia_viento(conn, tol = 0.1, scope_estaciones = scope_estaciones)
      }, error = function(e) message("[calidad] QC consistencia omitido: ", conditionMessage(e)))
    }
  } else {
    message("[calidad] 0 filas nuevas, ventana sellada; sync diaria sin QC.")
  }
  if (exists("observacion_diaria_upsert_rango", mode = "function") && debe_sync) {
    observacion_diaria_upsert_rango(conn, fmin, fmax, scope_estaciones)
  }
  message("[calidad] Refrescando resumen del mapa...")
  refresh_estacion_calidad(conn)
  invalidate_estaciones_cache()
  refrescar_clima_ref_si_vieja(conn)
  invisible(TRUE)
}

#' Refresca clima_de_referencia si lleva mas de `dias` sin actualizarse.
#'
#' Va al final y no al principio para no meter ~7 min en el camino critico de la
#' ingesta diaria; la climatologia de 117 anios no se mueve por un dia de datos.
#' Se auto-dispara en vez de depender de una tarea programada aparte, que es una
#' pieza mas que se puede olvidar de correr.
refrescar_clima_ref_si_vieja <- function(conn, dias = 30L) {
  if (!exists("qc_clima_ref_refresh", mode = "function")) return(invisible(FALSE))
  edad <- tryCatch(DBI::dbGetQuery(conn, "
    SELECT COALESCE(EXTRACT(DAY FROM NOW() - MAX(updated_at)), 9999)::int AS dias
    FROM clima_de_referencia"), error = function(e) NULL)
  if (is.null(edad) || nrow(edad) == 0L) return(invisible(FALSE))
  if (as.integer(edad$dias[1]) < as.integer(dias)) return(invisible(FALSE))
  message("[calidad] clima_de_referencia tiene ", edad$dias[1], " dias; refrescando...")
  tryCatch(qc_clima_ref_refresh(conn),
           error = function(e) message("[calidad] refresh clima_ref fallo: ", conditionMessage(e)))
  invisible(TRUE)
}
