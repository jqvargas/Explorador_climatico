# Orquestacion comun: adapter -> catalog -> persist -> post_ingesta.

ingest_run_pipeline <- function(
  fuente_nombre,
  variable_abr,
  fetch_fn,
  ...,
  conn = NULL
) {
  own_conn <- is.null(conn)
  if (own_conn) {
    conn <- get_db_connection()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  if (!persist_unique_index_existe(conn)) {
    stop(
      "Falta indice unico en bitacora_oficial."
    )
  }

  id_fuente <- catalog_get_id_fuente(conn, fuente_nombre)
  id_variable <- catalog_get_id_variable(conn, variable_abr)
  message("[ingest] Fuente=", fuente_nombre, " (id=", id_fuente, ") variable=", variable_abr,
          " (id=", id_variable, ")")

  raw <- fetch_fn(...)
  if (is.null(raw) || (is.data.frame(raw) && nrow(raw) == 0L)) {
    message("[ingest] Sin datos descargados.")
    post_ingesta_observaciones(conn, 0L)
    return(list(ok = TRUE, n_inserted = 0L))
  }

  if (all(c("codigo_nacional", "year", "month", "day") %in% names(raw))) {
    valor_col <- setdiff(names(raw), c("codigo_nacional", "year", "month", "day"))[1]
    long <- normalize_long_to_observaciones(raw, valor_col = valor_col)
  } else if (all(c("id_estacion", "id_variable", "fecha", "valor") %in% names(raw))) {
    long <- raw
    long$id_estacion <- as.integer(long$id_estacion)
    long$id_variable <- as.integer(long$id_variable)
    long$fecha <- as.Date(long$fecha)
    long$valor <- as.numeric(long$valor)
  } else {
    stop("Formato de salida del adapter no reconocido.")
  }

  if ("id_estacion" %in% names(long)) {
    obs <- long[, c("id_estacion", "id_variable", "fecha", "valor"), drop = FALSE]
  } else {
    est_map <- catalog_cargar_estaciones(conn, id_fuente)
    obs <- catalog_aplicar_a_filas(long, est_map, id_variable)
  }

  n <- persist_observacion_final(conn, obs)
  post_ingesta_observaciones(
    conn,
    n_rows_inserted = n,
    fecha_min = min(obs$fecha),
    fecha_max = max(obs$fecha),
    scope_estaciones = unique(obs$id_estacion)
  )

  list(ok = TRUE, n_inserted = n)
}

ingest_run_all_variables <- function(fuente_nombre, adapters, ...) {
  results <- list()
  for (nm in names(adapters)) {
    message("[ingest] === ", fuente_nombre, " / ", nm, " ===")
    results[[nm]] <- tryCatch(
      ingest_run_pipeline(
        fuente_nombre = fuente_nombre,
        variable_abr = nm,
        fetch_fn = adapters[[nm]],
        ...
      ),
      error = function(e) {
        message("[ingest] ERROR ", nm, ": ", e$message)
        list(ok = FALSE, error = e$message)
      }
    )
  }
  invisible(results)
}
