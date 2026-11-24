# Persistencia idempotente en bitacora_oficial (UPSERT por estacion+variable+fecha).

persist_observacion_final <- function(conn, rows, chunk_size = 5000L) {
  req <- c("id_estacion", "id_variable", "fecha", "valor")
  if (!all(req %in% names(rows))) {
    stop("Filas invalidas: se requieren ", paste(req, collapse = ", "))
  }

  rows <- rows[, req, drop = FALSE]
  rows$id_estacion <- as.integer(rows$id_estacion)
  rows$id_variable <- as.integer(rows$id_variable)
  rows$fecha <- as.Date(rows$fecha)
  rows$valor <- suppressWarnings(as.numeric(rows$valor))
  rows <- rows[!is.na(rows$id_estacion) & !is.na(rows$id_variable) &
                 !is.na(rows$fecha) & !is.na(rows$valor), , drop = FALSE]

  if (nrow(rows) == 0L) {
    message("[persist] Sin filas validas para insertar.")
    return(0L)
  }

  rules <- qc_cargar_reglas(conn)
  rows$qc_flag <- as.integer(qc_evaluar_vector(rows$id_variable, rows$valor, rules))
  rows$year <- as.integer(format(rows$fecha, "%Y"))
  rows$month <- as.integer(format(rows$fecha, "%m"))
  rows$day <- as.integer(format(rows$fecha, "%d"))
  rows$fecha_unix <- as.integer(as.POSIXct(rows$fecha, tz = "UTC"))

  n_total <- 0L
  n_chunks <- ceiling(nrow(rows) / chunk_size)

  for (i in seq_len(n_chunks)) {
    start <- (i - 1L) * chunk_size + 1L
    end <- min(i * chunk_size, nrow(rows))
    chunk <- rows[start:end, , drop = FALSE]
    n_total <- n_total + persist_chunk_upsert(conn, chunk)
    if (n_chunks > 1L) {
      message("[persist] Lote ", i, "/", n_chunks, " (", nrow(chunk), " filas)")
    }
  }

  message("[persist] Filas afectadas (insert/update): ", n_total)
  n_total
}

persist_chunk_upsert <- function(conn, chunk) {
  if (nrow(chunk) == 0L) return(0L)

  values_sql <- vapply(seq_len(nrow(chunk)), function(j) {
    r <- chunk[j, , drop = FALSE]
    sprintf(
      "(%d, %d, '%s'::date, %s, %d, %d, %d, %d, %d)",
      r$id_estacion, r$id_variable, format(r$fecha, "%Y-%m-%d"),
      format(r$valor, scientific = FALSE, trim = TRUE),
      r$qc_flag, r$year, r$month, r$day, r$fecha_unix
    )
  }, character(1))

  # qc_flag aqui es solo el nivel 1 (rango duro). qc_rev vuelve a NULL para que
  # toda fila nueva o corregida reentre a la cola del QC multinivel. Solo si
  # cambio el valor: el flag de persist (0/1) es distinto del multinivel (2/3),
  # y disparar por qc_flag pisa el sello en cada re-scrape del mismo numero.
  sql <- paste0(
    "INSERT INTO bitacora_oficial ",
    "(id_estacion, id_variable, fecha, valor, qc_flag, year, month, day, fecha_unix) ",
    "VALUES ", paste(values_sql, collapse = ", "), " ",
    "ON CONFLICT (id_estacion, id_variable, fecha) DO UPDATE SET ",
    "valor = EXCLUDED.valor, ",
    "qc_flag = EXCLUDED.qc_flag, ",
    "qc_rev = NULL, ",
    "year = EXCLUDED.year, ",
    "month = EXCLUDED.month, ",
    "day = EXCLUDED.day, ",
    "fecha_unix = EXCLUDED.fecha_unix ",
    "WHERE bitacora_oficial.valor IS DISTINCT FROM EXCLUDED.valor"
  )

  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(conn, sql)
  })
}

# RE5036 horario. qc_flag queda 0: no correr qc_multinivel (pisaria flags de T/PP
# si el rango tocara bitacora_oficial). El tope anti-parseo ya corrio en el scraper.
persist_observacion_horaria_from_scrape <- function(conn, data, dry_run = FALSE) {
  need <- c("year", "month", "day", "hour", "codigo_nacional", "u_rafaga_h")
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) return(0L)
  if (!all(need %in% names(data))) {
    stop("horario: se requieren ", paste(need, collapse = ", "))
  }

  id_var <- catalog_get_id_variable(conn, "u_rafaga_h")
  id_fuente <- catalog_get_id_fuente(conn, "dmc")
  est_map <- catalog_cargar_estaciones(conn, id_fuente)

  codigo <- as.character(data$codigo_nacional)
  id_est <- est_map[codigo]
  hour <- suppressWarnings(as.integer(data$hour))
  valor <- suppressWarnings(as.numeric(data$u_rafaga_h))
  year <- suppressWarnings(as.integer(data$year))
  month <- suppressWarnings(as.integer(data$month))
  day <- suppressWarnings(as.integer(data$day))
  ok <- !is.na(id_est) & !is.na(hour) & hour >= 0L & hour <= 23L &
    !is.na(valor) & !is.na(year) & !is.na(month) & !is.na(day)
  if (any(!ok & is.na(id_est) & !is.na(codigo) & nzchar(codigo))) {
    unknown <- unique(codigo[is.na(id_est)])
    message(
      "[catalog] Omitiendo ", sum(is.na(id_est)),
      " hora(s) con estacion desconocida (ej. ",
      paste(head(unknown, 3), collapse = ", "), ")."
    )
  }
  if (!any(ok)) return(0L)

  ts <- as.POSIXct(
    sprintf("%04d-%02d-%02d %02d:00:00", year[ok], month[ok], day[ok], hour[ok]),
    tz = "UTC", format = "%Y-%m-%d %H:%M:%S"
  )
  rows <- data.frame(
    id_estacion = as.integer(id_est[ok]),
    id_variable = as.integer(id_var),
    ts = ts,
    valor = valor[ok],
    stringsAsFactors = FALSE
  )
  rows <- rows[!is.na(rows$ts), , drop = FALSE]
  if (nrow(rows) == 0L) return(0L)
  rows <- rows[order(rows$id_estacion, rows$ts), , drop = FALSE]
  key <- paste(rows$id_estacion, format(rows$ts, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  rows <- rows[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  if (isTRUE(dry_run)) return(nrow(rows))

  chunk_size <- 5000L
  n_total <- 0L
  n_chunks <- ceiling(nrow(rows) / chunk_size)
  for (i in seq_len(n_chunks)) {
    start <- (i - 1L) * chunk_size + 1L
    end <- min(i * chunk_size, nrow(rows))
    n_total <- n_total + persist_observacion_horaria_chunk(conn, rows[start:end, , drop = FALSE])
  }
  message("[persist] reloj_de_horas filas upsert: ", n_total)
  n_total
}

persist_observacion_horaria_chunk <- function(conn, chunk) {
  if (nrow(chunk) == 0L) return(0L)
  values_sql <- vapply(seq_len(nrow(chunk)), function(j) {
    r <- chunk[j, , drop = FALSE]
    sprintf(
      "(%d, %d, '%s+00'::timestamptz, %s, 0)",
      r$id_estacion, r$id_variable,
      format(r$ts, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      format(r$valor, scientific = FALSE, trim = TRUE)
    )
  }, character(1))
  sql <- paste0(
    "INSERT INTO reloj_de_horas ",
    "(id_estacion, id_variable, ts, valor, qc_flag) VALUES ",
    paste(values_sql, collapse = ", "),
    " ON CONFLICT (id_estacion, id_variable, ts) DO UPDATE SET ",
    "valor = EXCLUDED.valor ",
    "WHERE reloj_de_horas.valor IS DISTINCT FROM EXCLUDED.valor"
  )
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(conn, sql)
  })
  nrow(chunk)
}

# Rafaga horaria DGA (_76_). ts ya viene en America/Santiago; se guarda el
# instante (minuto incluido). No usa el camino DMC, que aplasta a hora UTC.
persist_rafaga_h_dga <- function(conn, data, dry_run = FALSE) {
  need <- c("codigo_nacional", "ts", "u_rafaga_h")
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) return(0L)
  if (!all(need %in% names(data))) {
    stop("rafaga DGA: se requieren ", paste(need, collapse = ", "))
  }
  id_var <- catalog_get_id_variable(conn, "u_rafaga_h")
  id_fuente <- catalog_get_id_fuente(conn, "dga")
  est_map <- catalog_cargar_estaciones(conn, id_fuente)
  codigo <- as.character(data$codigo_nacional)
  id_est <- est_map[codigo]
  valor <- suppressWarnings(as.numeric(data$u_rafaga_h))
  ts <- data$ts
  ok <- !is.na(id_est) & !is.na(ts) & is.finite(valor)
  if (!any(ok)) return(0L)
  rows <- data.frame(
    id_estacion = as.integer(id_est[ok]),
    id_variable = as.integer(id_var),
    ts = ts[ok],
    valor = valor[ok],
    stringsAsFactors = FALSE
  )
  key <- paste(rows$id_estacion, format(rows$ts, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  rows <- rows[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  if (isTRUE(dry_run)) return(nrow(rows))
  persist_observacion_horaria_chunk(conn, rows)
}

# Maximo del dia civil ya cerrado dentro de la ventana. No baja un maximo
# previo. No toca qc_rev (no reabre el QC multinivel).
persist_rafaga_dga_dia <- function(conn, data, fecha_ini, fecha_fin, dry_run = FALSE) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) return(0L)
  if (!all(c("codigo_nacional", "ts", "u_rafaga_h") %in% names(data))) return(0L)
  hoy <- as.Date(format(Sys.time(), tz = "America/Santiago", "%Y-%m-%d"))
  last_ok <- min(as.Date(fecha_fin), hoy - 1L)
  first <- as.Date(fecha_ini)
  if (is.na(last_ok) || is.na(first) || last_ok < first) return(0L)
  dia <- as.Date(data$ts, tz = "America/Santiago")
  keep <- !is.na(dia) & dia >= first & dia <= last_ok & is.finite(data$u_rafaga_h)
  if (!any(keep)) return(0L)
  tmp <- data.frame(
    codigo_nacional = as.character(data$codigo_nacional[keep]),
    fecha = dia[keep],
    valor = as.numeric(data$u_rafaga_h[keep]),
    stringsAsFactors = FALSE
  )
  partes <- split(tmp$valor, paste(tmp$codigo_nacional, tmp$fecha))
  cods <- vapply(strsplit(names(partes), " ", fixed = TRUE), `[`, character(1), 1L)
  fechas <- as.Date(vapply(strsplit(names(partes), " ", fixed = TRUE), `[`, character(1), 2L))
  id_var <- catalog_get_id_variable(conn, "u_rafaga")
  id_fuente <- catalog_get_id_fuente(conn, "dga")
  est_map <- catalog_cargar_estaciones(conn, id_fuente)
  id_est <- est_map[cods]
  valor <- vapply(partes, function(v) max(v, na.rm = TRUE), numeric(1))
  ok <- !is.na(id_est) & is.finite(valor)
  if (!any(ok)) return(0L)
  if (isTRUE(dry_run)) return(sum(ok))
  values_sql <- vapply(which(ok), function(j) {
    sprintf(
      "(%d, %d, '%s'::date, %s, 0, %d, %d, %d, %d)",
      as.integer(id_est[j]), as.integer(id_var), format(fechas[j], "%Y-%m-%d"),
      format(valor[j], scientific = FALSE, trim = TRUE),
      as.integer(format(fechas[j], "%Y")),
      as.integer(format(fechas[j], "%m")),
      as.integer(format(fechas[j], "%d")),
      as.integer(as.POSIXct(fechas[j], tz = "UTC"))
    )
  }, character(1))
  sql <- paste0(
    "INSERT INTO bitacora_oficial ",
    "(id_estacion, id_variable, fecha, valor, qc_flag, year, month, day, fecha_unix) ",
    "VALUES ", paste(values_sql, collapse = ", "), " ",
    "ON CONFLICT (id_estacion, id_variable, fecha) DO UPDATE SET ",
    "valor = GREATEST(bitacora_oficial.valor, EXCLUDED.valor) ",
    "WHERE bitacora_oficial.valor < EXCLUDED.valor"
  )
  DBI::dbWithTransaction(conn, DBI::dbExecute(conn, sql))
  length(values_sql)
}

persist_unique_index_existe <- function(conn) {
  row <- DBI::dbGetQuery(conn, "
    SELECT 1 AS ok
    FROM pg_indexes
    WHERE schemaname = 'public'
      AND tablename = 'bitacora_oficial'
      AND indexname = 'idx_bitacora_oficial_unique_key'
    LIMIT 1
  ")
  nrow(row) > 0L
}
