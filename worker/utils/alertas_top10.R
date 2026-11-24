# Evaluacion diaria alertas top 10 (digest unico por usuario)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

# Cache por proceso: valores invariantes durante una corrida (disponibilidad de
# jornada_cerrada, ids de variables permitidas). Evita repetir queries caras
# dentro del bucle reglas x estaciones x variables. Se limpia al inicio de cada
# evaluacion. ponytail: si el runner llegara a evaluar dos fechas en un mismo
# proceso, llamar alertas_reset_cache() entre corridas.
.alertas_cache <- new.env(parent = emptyenv())

alertas_reset_cache <- function() {
  .alertas_cache$diaria_disp <- NULL
  .alertas_cache$allowed_vids <- NULL
  invisible(NULL)
}

ALERTAS_TIER_SCOPES <- list(
  gratuito = c("favorita"),
  pro = c("favorita", "grupo", "estacion", "comuna", "region", "cuenca"),
  enterprise = c("favorita", "grupo", "estacion", "comuna", "region", "cuenca")
)

alertas_normalizar_tier <- function(tier) {
  tier <- tolower(trimws(as.character(tier %||% "gratuito")))
  if (tier %in% c("pro", "enterprise")) tier else "gratuito"
}

alertas_scopes_for_tier <- function(tier) {
  ALERTAS_TIER_SCOPES[[alertas_normalizar_tier(tier)]] %||% ALERTAS_TIER_SCOPES$gratuito
}

alertas_filtrar_reglas_por_tier <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (!"tier" %in% names(df)) return(df)
  keep <- vapply(seq_len(nrow(df)), function(i) {
    df$scope_type[i] %in% alertas_scopes_for_tier(df$tier[i])
  }, logical(1))
  df[keep, , drop = FALSE]
}


pg_text_array_param <- function(vals) {
  if (is.null(vals) || length(vals) == 0) return(NA_character_)
  vals <- as.character(unlist(vals))
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) return(NA_character_)
  paste0("{", paste(vals, collapse = ","), "}")
}

# RPostgres/DBI rechaza NULL con length 0 en un bind param; usar NA tipado (= NULL en SQL).
alertas_sql_scalar <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0L) return(default)
  x[[1L]]
}

alertas_sql_date_param <- function(x) {
  d <- suppressWarnings(as.Date(alertas_sql_scalar(x), optional = TRUE))
  if (length(d) == 0L || is.na(d)) return(NA_character_)
  format(d, "%Y-%m-%d")
}

alertas_sql_text_param <- function(x) {
  v <- alertas_sql_scalar(x, NA_character_)
  if (length(v) == 0L || is.na(v)) return(NA_character_)
  as.character(v)
}

pg_text_param <- function(x) alertas_sql_text_param(x)

alertas_allowed_variable_ids <- function(conn) {
  cached <- .alertas_cache$allowed_vids
  if (!is.null(cached)) return(cached)
  abrs <- if (exists("alertas_catalogo_abrs", mode = "function")) {
    alertas_catalogo_abrs()
  } else {
    c("pp", "t_max", "t_min", "u_max_diario")
  }
  df <- alertas_db_query(conn,
    "SELECT id FROM magnitud WHERE nombre_abr = ANY($1::text[]) ORDER BY array_position($1::text[], nombre_abr)",
    params = list(pg_text_array_param(abrs))
  )
  res <- if (nrow(df) == 0) integer(0) else as.integer(df$id)
  .alertas_cache$allowed_vids <- res
  res
}

alertas_db_query <- function(conn, q, params = NULL) {
  if (is.null(params) || length(params) == 0) {
    DBI::dbGetQuery(conn, q)
  } else {
    DBI::dbGetQuery(conn, q, params = params)
  }
}

alertas_db_execute <- function(conn, q, params = NULL) {
  if (is.null(params) || length(params) == 0) {
    DBI::dbExecute(conn, q)
  } else {
    DBI::dbExecute(conn, q, params = params)
  }
}

alertas_umbral_boot <- function() {
  if (exists("alertas_umbral_evaluar", mode = "function")) return(invisible(TRUE))
  for (p in c(
    "/app/utils/alertas_umbral.R",
    file.path(getwd(), "utils", "alertas_umbral.R"),
    file.path(getwd(), "worker", "utils", "alertas_umbral.R")
  )) {
    if (file.exists(p)) {
      source(p, local = FALSE)
      return(invisible(exists("alertas_umbral_evaluar", mode = "function")))
    }
  }
  invisible(FALSE)
}

alertas_umbral_correr <- function(conn, dry_run = FALSE) {
  alertas_umbral_boot()
  if (!exists("alertas_umbral_evaluar", mode = "function")) return(invisible(0L))
  tryCatch(
    alertas_umbral_evaluar(conn, dry_run = dry_run),
    error = function(e) {
      message("[alertas] umbral ERROR: ", conditionMessage(e))
      0L
    }
  )
}

alertas_tabla_existe_conn <- function(conn, name) {
  df <- alertas_db_query(conn,
    "SELECT 1 AS ok FROM information_schema.tables WHERE table_name = $1 LIMIT 1",
    params = list(name)
  )
  nrow(df) > 0
}

alertas_bootstrap_reporte <- function(project_root) {
  roots <- if (exists("worker_resolve_data_roots", mode = "function")) {
    worker_resolve_data_roots(project_root)
  } else {
    list(shiny = "", api = "")
  }
  shiny_root <- roots$shiny
  if (!nzchar(shiny_root) || !dir.exists(shiny_root)) {
    shiny_root <- file.path(project_root, "shiny")
  }
  if (!dir.exists(shiny_root)) stop("No se encuentra shiny en ", shiny_root)
  Sys.setenv(SHINY_ROOT = shiny_root)
  api_root <- roots$api
  if (!nzchar(api_root) || !dir.exists(api_root)) {
    api_root <- file.path(project_root, "api")
  }
  for (od_path in c(
    file.path(project_root, "worker", "utils", "observacion_diaria.R"),
    "/app/utils/observacion_diaria.R"
  )) {
    if (file.exists(od_path)) {
      source(od_path, local = FALSE)
      break
    }
  }
  assign("db_query", function(q, params = NULL) {
    conn <- get_db_connection()
    on.exit(tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL))
    alertas_db_query(conn, q, params)
  }, envir = .GlobalEnv)
  assign("db_execute", function(q, params = NULL) {
    conn <- get_db_connection()
    on.exit(tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL))
    alertas_db_execute(conn, q, params)
  }, envir = .GlobalEnv)
  api_payload <- file.path(api_root, "utils", "reporte_payload.R")
  if (!file.exists(api_payload)) stop("No se encuentra ", api_payload)
  source(api_payload, local = FALSE)
  calidad_path <- file.path(api_root, "utils", "calidad.R")
  if (file.exists(calidad_path)) source(calidad_path, local = FALSE)
  catalog_path <- file.path(api_root, "utils", "alertas_catalog.R")
  if (file.exists(catalog_path)) source(catalog_path, local = FALSE)
  alerta_path <- file.path(api_root, "utils", "alerta_temprana.R")
  if (file.exists(alerta_path)) source(alerta_path, local = FALSE)
  skill_path <- file.path(api_root, "utils", "alertas_skill.R")
  if (file.exists(skill_path)) source(skill_path, local = FALSE)
  for (tardia_path in c(
    file.path(project_root, "worker", "utils", "alertas_tardia_sql.R"),
    "/app/utils/alertas_tardia_sql.R"
  )) {
    if (file.exists(tardia_path)) {
      source(tardia_path, local = FALSE)
      break
    }
  }
  reporte_source_shiny()
  invisible(TRUE)
}

alertas_cargar_preferencias <- function(conn, user_id) {
  if (!alertas_tabla_existe_conn(conn, "gusto_de_aviso")) {
    return(list(variable_ids = NULL, alerta_max = TRUE, alerta_min = TRUE,
                alerta_tardia = TRUE, email_modo = "cuenta", email_alternativo = NULL))
  }
  # ponytail: si falta la columna alerta_tardia, fallback sin la col.
  df <- tryCatch(
    alertas_db_query(conn,
      "SELECT email_modo, email_alternativo, variable_ids, alerta_max, alerta_min, alerta_tardia
       FROM gusto_de_aviso WHERE user_id = $1::uuid",
      params = list(user_id)
    ),
    error = function(e) alertas_db_query(conn,
      "SELECT email_modo, email_alternativo, variable_ids, alerta_max, alerta_min
       FROM gusto_de_aviso WHERE user_id = $1::uuid",
      params = list(user_id)
    )
  )
  if (nrow(df) == 0) {
    return(list(variable_ids = NULL, alerta_max = TRUE, alerta_min = TRUE,
                alerta_tardia = TRUE, email_modo = "cuenta", email_alternativo = NULL))
  }
  row <- df[1, ]
  # variable_ids NULL en SQL llega como NA (len 1) o lista envuelta; NA/vacio = "todas".
  vids <- row$variable_ids
  if (is.list(vids)) vids <- vids[[1]]
  if (!is.null(vids)) {
    vids <- suppressWarnings(as.integer(vids))
    vids <- vids[!is.na(vids)]
    if (length(vids) == 0) vids <- NULL
  }
  list(
    variable_ids = vids,
    alerta_max = isTRUE(row$alerta_max),
    alerta_min = isTRUE(row$alerta_min),
    alerta_tardia = if ("alerta_tardia" %in% names(row)) isTRUE(row$alerta_tardia) else TRUE,
    email_modo = row$email_modo %||% "cuenta",
    email_alternativo = row$email_alternativo
  )
}

alertas_resolver_email <- function(cuenta_email, prefs) {
  cuenta_email <- trimws(tolower(cuenta_email %||% ""))
  if (identical(prefs$email_modo, "alternativo")) {
    alt <- trimws(tolower(prefs$email_alternativo %||% ""))
    if (nzchar(alt) && !grepl("@pending\\.rodaja\\.cl$", alt)) return(alt)
  }
  cuenta_email
}

alertas_resolver_scope_eids <- function(conn, user_id, scope_type, scope_value = list()) {
  scope_type <- tolower(trimws(scope_type %||% ""))
  scope_value <- scope_value %||% list()
  if (scope_type == "favorita") {
    df <- alertas_db_query(conn,
      "SELECT eid FROM miradores_pinchados WHERE user_id = $1::uuid AND es_primario = TRUE LIMIT 1",
      params = list(user_id)
    )
    if (nrow(df) == 0) return(integer(0))
    return(as.integer(df$eid[1]))
  }
  if (scope_type == "grupo") {
    df <- alertas_db_query(conn,
      "SELECT payload FROM cuadrilla WHERE user_id = $1::uuid",
      params = list(user_id)
    )
    if (nrow(df) == 0) return(integer(0))
    payload <- tryCatch(jsonlite::fromJSON(df$payload[1], simplifyVector = FALSE), error = function(e) list())
    ids <- unique(as.integer(unlist(payload$ids %||% list())))
    ids <- ids[!is.na(ids) & ids > 0L]
    return(ids)
  }
  if (scope_type == "estacion") {
    eid <- as.integer(scope_value$eid %||% NA)
    if (is.na(eid) || eid < 1L) return(integer(0))
    return(eid)
  }
  if (scope_type == "comuna") {
    com <- trimws(as.character(scope_value$nom_com %||% ""))
    if (!nzchar(com)) return(integer(0))
    df <- alertas_db_query(conn,
      "SELECT id FROM mirador WHERE TRIM(COALESCE(nom_com, '')) = $1
       AND lat BETWEEN -56 AND -17 AND lon BETWEEN -76 AND -66",
      params = list(com)
    )
    if (nrow(df) == 0) return(integer(0))
    return(as.integer(df$id))
  }
  if (scope_type == "region") {
    rid <- suppressWarnings(as.integer(scope_value$cod_reg %||% NA))
    if (is.na(rid)) return(integer(0))
    df <- alertas_db_query(conn,
      "SELECT id FROM mirador WHERE cod_reg = $1
       AND lat BETWEEN -56 AND -17 AND lon BETWEEN -76 AND -66",
      params = list(rid)
    )
    if (nrow(df) == 0) return(integer(0))
    return(as.integer(df$id))
  }
  if (scope_type == "cuenca") {
    cu <- trimws(as.character(scope_value$nom_cuen %||% scope_value$cuenca %||% ""))
    if (!nzchar(cu)) return(integer(0))
    df <- alertas_db_query(conn,
      "SELECT id FROM mirador WHERE TRIM(COALESCE(nom_cuen, '')) = $1
       AND lat BETWEEN -56 AND -17 AND lon BETWEEN -76 AND -66",
      params = list(cu)
    )
    if (nrow(df) == 0) return(integer(0))
    return(as.integer(df$id))
  }
  integer(0)
}

alertas_variables_con_datos <- function(conn, eid, variable_ids = NULL) {
  allowed <- alertas_allowed_variable_ids(conn)
  if (length(allowed) == 0) return(integer(0))
  if (alertas_top10_diaria_disponible(conn)) {
    df <- alertas_db_query(conn,
      "SELECT DISTINCT id_variable FROM jornada_cerrada
       WHERE id_estacion = $1 AND id_variable = ANY($2::int[])
       ORDER BY id_variable",
      params = list(as.integer(eid), paste0("{", paste(allowed, collapse = ","), "}"))
    )
    if (nrow(df) > 0) {
      vids <- as.integer(df$id_variable)
      if (!is.null(variable_ids) && length(variable_ids) > 0) {
        vids <- intersect(vids, as.integer(variable_ids))
      }
      return(vids)
    }
  }
  df <- alertas_db_query(conn,
    "SELECT DISTINCT id_variable FROM bitacora_oficial WHERE id_estacion = $1 ORDER BY id_variable",
    params = list(eid)
  )
  if (nrow(df) == 0) return(integer(0))
  vids <- intersect(as.integer(df$id_variable), allowed)
  if (!is.null(variable_ids) && length(variable_ids) > 0) {
    vids <- intersect(vids, as.integer(variable_ids))
  }
  vids
}

alertas_top10_diaria_disponible <- function(conn) {
  # Invariante durante la corrida: memoizar evita repetir esta comprobacion
  # cientos/miles de veces dentro del bucle reglas x estaciones x variables.
  cached <- .alertas_cache$diaria_disp
  if (!is.null(cached)) return(cached)
  disp <- FALSE
  if (exists("observacion_diaria_tabla_existe", mode = "function") &&
      observacion_diaria_tabla_existe(conn)) {
    # EXISTS corta en la primera fila; COUNT(*) escaneaba toda la hypertable (~42M filas).
    row <- alertas_db_query(conn, "SELECT EXISTS(SELECT 1 FROM jornada_cerrada) AS ok")
    disp <- isTRUE(row$ok[1])
  }
  .alertas_cache$diaria_disp <- disp
  disp
}

alertas_top10_min_anios <- function() {
  if (exists("alerta_temprana_min_anios", mode = "function")) {
    alerta_temprana_min_anios()
  } else {
    20L
  }
}

alertas_top10_meta_par <- function(conn, eid, variable_id) {
  df <- alertas_db_query(conn,
    "SELECT e.nombre_publico AS estacion_nombre, v.nombre AS variable_nombre, v.nombre_abr,
            COALESCE(v.unidad, '') AS unidad
     FROM mirador e, magnitud v
     WHERE e.id = $1 AND v.id = $2",
    params = list(as.integer(eid), as.integer(variable_id))
  )
  if (nrow(df) == 0) {
    return(list(
      estacion_nombre = paste0("estacion ", eid),
      variable_nombre = paste0("variable ", variable_id),
      nombre_abr = "",
      unidad = ""
    ))
  }
  list(
    estacion_nombre = df$estacion_nombre[1] %||% paste0("estacion ", eid),
    variable_nombre = df$variable_nombre[1] %||% paste0("variable ", variable_id),
    nombre_abr = df$nombre_abr[1] %||% "",
    unidad = df$unidad[1] %||% ""
  )
}

alertas_top10_ultimo_diario <- function(conn, eid, variable_id) {
  df <- alertas_db_query(conn,
    "SELECT fecha, valor FROM jornada_cerrada
     WHERE id_estacion = $1 AND id_variable = $2 AND valor IS NOT NULL
     ORDER BY fecha DESC LIMIT 1",
    params = list(as.integer(eid), as.integer(variable_id))
  )
  if (nrow(df) == 0) return(NULL)
  list(
    fecha = as.Date(df$fecha[1]),
    valor = as.numeric(df$valor[1])
  )
}

alertas_top10_rank_diaria_sql <- function(conn, eid, variable_id, valor, decreasing = TRUE) {
  # Estricto (> / <): el valor observado ya esta en jornada_cerrada, asi que
  # >= se contaria a si mismo (off-by-one). Con estricto el record queda rank 1,
  # igual que reporte_rank_position en el reporte por estacion.
  cmp <- if (isTRUE(decreasing)) ">" else "<"
  min_anios <- alertas_top10_min_anios()
  min_dias <- if (exists("alerta_temprana_min_dias", mode = "function")) {
    alerta_temprana_min_dias()
  } else {
    365L
  }
  df <- alertas_db_query(conn, paste0(
    "SELECT COUNT(*)::int AS n_days,",
    " COUNT(DISTINCT EXTRACT(YEAR FROM fecha))::int AS anios_con_datos,",
    " 1 + COUNT(*) FILTER (WHERE valor ", cmp, " $3)::int AS rank",
    " FROM jornada_cerrada",
    " WHERE id_estacion = $1 AND id_variable = $2 AND valor IS NOT NULL"
  ), params = list(as.integer(eid), as.integer(variable_id), as.numeric(valor)))
  if (nrow(df) == 0 || is.na(df$n_days[1]) || df$n_days[1] == 0L) return(NULL)
  anios_con_datos <- as.integer(df$anios_con_datos[1])
  historia_ok <- if (exists("alerta_par_rank_historia_ok", mode = "function")) {
    alerta_par_rank_historia_ok(df$n_days[1], anios_con_datos, min_anios, min_dias)
  } else {
    !is.na(anios_con_datos) && anios_con_datos >= min_anios &&
      !is.na(df$n_days[1]) && df$n_days[1] >= min_dias
  }
  if (!isTRUE(historia_ok)) return(NULL)
  list(
    rank = as.integer(df$rank[1]),
    n_days = as.integer(df$n_days[1]),
    anios_con_datos = anios_con_datos
  )
}

alertas_top10_registrar_eventos <- function(conn, user_id, regla_id, eid, variable_id,
                                            last_date, prefs, meta, widgets, valor_row,
                                            dry_run = FALSE, rank_fn) {
  found <- list()
  for (spec in widgets) {
    direccion <- if (identical(spec$id, "maximos")) "max" else "min"
    if (identical(spec$id, "minimos") && isTRUE(spec$skipDaily)) next
    if (direccion == "max" && !isTRUE(prefs$alerta_max)) next
    if (direccion == "min" && !isTRUE(prefs$alerta_min)) next

    ranked <- rank_fn(spec$decreasing)
    if (is.null(ranked)) next
    rank <- ranked$rank
    if (is.na(rank) || rank > 10L) next
    if (!isTRUE(dry_run) && alertas_evento_existe(conn, user_id, eid, variable_id, last_date, direccion, tipo = "tardia")) next

    dir_label <- if (direccion == "max") "maximos" else "minimos"
    fecha_larga <- reporte_fmt_fecha_larga(last_date)
    ev <- list(
      user_id = user_id,
      regla_id = regla_id,
      eid = eid,
      variable_id = variable_id,
      fecha = last_date,
      direccion = direccion,
      ranking = rank,
      valor = valor_row,
      unidad = meta$unidad,
      variable_nombre = meta$variable_nombre,
      estacion_nombre = meta$estacion_nombre,
      fecha_larga = fecha_larga,
      direccion_label = dir_label,
      n_dias = ranked$n_days
    )
    if (isTRUE(dry_run)) {
      message("[alertas:dry] user=", user_id, " eid=", eid, " vid=", variable_id,
              " ", direccion, " rank=", rank)
      found[[length(found) + 1L]] <- ev
      next
    }
    alertas_registrar_evento_pendiente(
      conn, user_id, regla_id, eid, variable_id, last_date, direccion, rank, valor_row
    )
    found[[length(found) + 1L]] <- ev
  }
  found
}

alertas_evaluar_par_diaria <- function(conn, user_id, regla_id, eid, variable_id, prefs, dry_run = FALSE) {
  if (isTRUE(prefs$alerta_max) == FALSE && isTRUE(prefs$alerta_min) == FALSE) return(list())
  ultimo <- alertas_top10_ultimo_diario(conn, eid, variable_id)
  if (is.null(ultimo) || is.na(ultimo$fecha) || is.na(ultimo$valor)) return(list())

  meta <- alertas_top10_meta_par(conn, eid, variable_id)
  vtype <- reporte_detect_var_type(meta$variable_nombre)
  nombre_abr <- meta$nombre_abr %||% ""
  widgets <- if (exists("alertas_specs_to_widgets", mode = "function")) {
    alertas_specs_to_widgets(nombre_abr, vtype, meta$variable_nombre)
  } else {
    reporte_widget_defs(vtype)
  }
  if (length(widgets) == 0L) return(list())
  last_date <- ultimo$fecha
  valor_row <- ultimo$valor

  rank_fn <- function(decreasing) {
    alertas_top10_rank_diaria_sql(conn, eid, variable_id, valor_row, decreasing = decreasing)
  }
  alertas_top10_registrar_eventos(
    conn, user_id, regla_id, eid, variable_id, last_date, prefs, meta, widgets,
    valor_row, dry_run = dry_run, rank_fn = rank_fn
  )
}

alertas_evaluar_par_legacy <- function(conn, user_id, regla_id, eid, variable_id, prefs, dry_run = FALSE) {
  found <- list()
  if (isTRUE(prefs$alerta_max) == FALSE && isTRUE(prefs$alerta_min) == FALSE) return(found)

  day <- NULL
  meta_src <- NULL
  if (exists("reporte_fetch_daily_agg_db", mode = "function")) {
    rd_daily <- reporte_fetch_daily_agg_db(eid, variable_id)
    if (!is.null(rd_daily) && !is.null(rd_daily$df) && nrow(rd_daily$df) > 0) {
      day <- rd_daily$df
      names(day) <- c("fecha", "valor")
      meta_src <- rd_daily$meta
    }
  }
  if (is.null(day)) {
    rd <- reporte_fetch_series_db(eid, variable_id)
    if (is.null(rd) || is.null(rd$df) || nrow(rd$df) == 0) return(found)
    df <- rd$df
    if (!"qc_flag" %in% names(df)) df$qc_flag <- 0L
    df$fecha <- tryCatch(as.Date(df$fecha), error = function(e) as.Date(as.character(df$fecha)))
    df_ok <- df[(is.na(df$qc_flag) | df$qc_flag == 0) & !is.na(df$valor), , drop = FALSE]
    if (nrow(df_ok) == 0) return(found)
    meta_src <- rd$meta
    vtype <- reporte_detect_var_type(meta_src$variable_nombre %||% "")
    day <- reporte_aggregate_valor(df_ok, valor ~ fecha, vtype, max(df_ok$fecha), min_coverage = 0)
    names(day) <- c("fecha", "valor")
  }

  variable_nombre <- meta_src$variable_nombre %||% paste0("variable ", variable_id)
  estacion_nombre <- meta_src$estacion_nombre %||% paste0("estacion ", eid)
  unidad <- meta_src$unidad %||% ""

  vtype <- reporte_detect_var_type(variable_nombre)
  last_date <- max(day$fecha, na.rm = TRUE)
  nombre_abr <- if (!is.null(meta_src$nombre_abr)) meta_src$nombre_abr else ""
  widgets <- if (exists("alertas_specs_to_widgets", mode = "function") && nzchar(nombre_abr)) {
    alertas_specs_to_widgets(nombre_abr, vtype, variable_nombre)
  } else if (exists("alertas_specs_to_widgets", mode = "function")) {
    abr_guess <- if (grepl("precip|lluvia", tolower(variable_nombre))) "pp"
                 else if (grepl("temp.*max|tmax", tolower(variable_nombre))) "t_max"
                 else if (grepl("temp.*min|tmin", tolower(variable_nombre))) "t_min"
                 else if (grepl("viento", tolower(variable_nombre))) "u_max_diario"
                 else ""
    if (nzchar(abr_guess)) alertas_specs_to_widgets(abr_guess, vtype, variable_nombre) else reporte_widget_defs(vtype)
  } else {
    reporte_widget_defs(vtype)
  }
  if (length(widgets) == 0L) return(found)

  day$label <- as.character(day$fecha)
  n_days <- sum(!is.na(day$valor))
  min_anios <- alertas_top10_min_anios()
  span_ok <- if (exists("alerta_par_historia_suficiente", mode = "function")) {
    alerta_par_historia_suficiente(day$fecha, min_anios)
  } else {
    alerta_anios_span <- as.integer(format(max(day$fecha, na.rm = TRUE), "%Y")) -
      as.integer(format(min(day$fecha, na.rm = TRUE), "%Y")) + 1L
    !is.na(alerta_anios_span) && alerta_anios_span >= min_anios
  }
  if (!isTRUE(span_ok)) return(found)

  target_label <- as.character(last_date)
  if (!(target_label %in% day$label)) return(found)

  meta <- list(
    estacion_nombre = estacion_nombre,
    variable_nombre = variable_nombre,
    nombre_abr = nombre_abr,
    unidad = unidad
  )
  valor_row <- day$valor[day$label == target_label][1]
  rank_fn <- function(decreasing) {
    rank <- reporte_rank_position(day$valor, day$label, target_label, decreasing = decreasing)
    if (is.na(rank)) return(NULL)
    list(rank = rank, n_days = n_days)
  }
  alertas_top10_registrar_eventos(
    conn, user_id, regla_id, eid, variable_id, last_date, prefs, meta, widgets,
    valor_row, dry_run = dry_run, rank_fn = rank_fn
  )
}

alertas_evento_existe <- function(conn, user_id, eid, variable_id, fecha, direccion, tipo = NULL) {
  tipo_sql <- ""
  params <- list(user_id, eid, variable_id, as.character(fecha), direccion)
  if (!is.null(tipo) && alertas_tipo_columna_existe(conn)) {
    tipo_sql <- " AND tipo = $6"
    params <- c(params, list(tipo))
  }
  df <- alertas_db_query(conn,
    paste0(
      "SELECT 1 AS ok FROM campanada",
      " WHERE user_id = $1::uuid AND eid = $2 AND variable_id = $3 AND fecha = $4::date AND direccion = $5",
      tipo_sql,
      " LIMIT 1"
    ),
    params = params
  )
  nrow(df) > 0
}

alertas_valor_pronostico_columna_existe <- function(conn) {
  df <- alertas_db_query(conn,
    "SELECT column_name FROM information_schema.columns
     WHERE table_name = 'campanada' AND column_name = 'valor_pronostico' LIMIT 1"
  )
  nrow(df) > 0
}

alertas_tipo_columna_existe <- function(conn) {
  df <- alertas_db_query(conn,
    "SELECT column_name FROM information_schema.columns
     WHERE table_name = 'campanada' AND column_name = 'tipo' LIMIT 1"
  )
  nrow(df) > 0
}

alertas_registrar_evento_pendiente <- function(conn, user_id, regla_id, eid, variable_id, fecha, direccion, ranking, valor, tipo = "tardia") {
  # ponytail: NULL en R tiene length 0; RPostgres exige length 1 por parametro ($2).
  regla_param <- if (is.null(regla_id) || length(regla_id) == 0L) NA_character_ else as.character(regla_id)[1L]
  # suscripcion_id = NULL: el sistema usa norma_propia; regla_id ($2) puede no
  # tener una fila espejo en pedido_de_aviso (reglas nuevas), asi que meter
  # regla_id en suscripcion_id violaba el FK a pedido_de_aviso.
  if (alertas_tipo_columna_existe(conn) && alertas_valor_pronostico_columna_existe(conn)) {
    # La prevista guarda el pronostico aparte: valor sigue siendo lo emitido.
    vp <- if (identical(tipo, "temprana")) as.numeric(valor) else NA_real_
    alertas_db_execute(conn,
      "INSERT INTO campanada (user_id, suscripcion_id, regla_id, eid, variable_id, fecha, direccion, ranking, valor, exito, pendiente_digest, tipo, valor_pronostico)
       VALUES ($1::uuid, NULL, $2::uuid, $3, $4, $5::date, $6, $7, $8, FALSE, TRUE, $9, $10)
       ON CONFLICT (user_id, eid, variable_id, fecha, direccion, tipo) DO NOTHING",
      params = list(user_id, regla_param, eid, variable_id, as.character(fecha), direccion, ranking, valor, tipo, vp)
    )
  } else if (alertas_tipo_columna_existe(conn)) {
    alertas_db_execute(conn,
      "INSERT INTO campanada (user_id, suscripcion_id, regla_id, eid, variable_id, fecha, direccion, ranking, valor, exito, pendiente_digest, tipo)
       VALUES ($1::uuid, NULL, $2::uuid, $3, $4, $5::date, $6, $7, $8, FALSE, TRUE, $9)
       ON CONFLICT (user_id, eid, variable_id, fecha, direccion) DO NOTHING",
      params = list(user_id, regla_param, eid, variable_id, as.character(fecha), direccion, ranking, valor, tipo)
    )
  } else {
    alertas_db_execute(conn,
      "INSERT INTO campanada (user_id, suscripcion_id, regla_id, eid, variable_id, fecha, direccion, ranking, valor, exito, pendiente_digest)
       VALUES ($1::uuid, NULL, $2::uuid, $3, $4, $5::date, $6, $7, $8, FALSE, TRUE)
       ON CONFLICT (user_id, eid, variable_id, fecha, direccion) DO NOTHING",
      params = list(user_id, regla_param, eid, variable_id, as.character(fecha), direccion, ranking, valor)
    )
  }
}

alertas_registrar_evento_temprana_pendiente <- function(conn, user_id, eid, variable_id, fecha, direccion, ranking, valor) {
  alertas_registrar_evento_pendiente(
    conn, user_id, NULL, eid, variable_id, fecha, direccion, ranking, valor, tipo = "temprana"
  )
}

alertas_evaluar_par <- function(conn, user_id, regla_id, eid, variable_id, prefs, dry_run = FALSE) {
  if (exists("alerta_temprana_estacion_elegible", mode = "function") &&
      !alerta_temprana_estacion_elegible(eid)) {
    return(list())
  }
  if (alertas_top10_diaria_disponible(conn)) {
    return(alertas_evaluar_par_diaria(
      conn, user_id, regla_id, eid, variable_id, prefs, dry_run = dry_run
    ))
  }
  alertas_evaluar_par_legacy(
    conn, user_id, regla_id, eid, variable_id, prefs, dry_run = dry_run
  )
}

alertas_cargar_reglas_activas <- function(conn) {
  if (alertas_tabla_existe_conn(conn, "norma_propia")) {
    df <- alertas_db_query(conn,
      "SELECT r.id AS regla_id, r.user_id, r.scope_type, r.scope_value, u.email, u.tier
       FROM norma_propia r
       JOIN tripulacion u ON u.id = r.user_id
       WHERE r.activa = TRUE AND u.activo = TRUE"
    )
    return(alertas_filtrar_reglas_por_tier(df))
  }
  df <- alertas_db_query(conn,
    "SELECT s.id AS regla_id, s.user_id, s.scope_type, '{}'::text AS scope_value, u.email, u.tier
     FROM pedido_de_aviso s
     JOIN tripulacion u ON u.id = s.user_id
     WHERE s.activa = TRUE AND u.activo = TRUE"
  )
  alertas_filtrar_reglas_por_tier(df)
}

alertas_digest_ya_enviado <- function(conn, user_id, fecha_eval) {
  if (!alertas_tabla_existe_conn(conn, "sobre_del_resumen")) return(FALSE)
  df <- alertas_db_query(conn,
    "SELECT 1 AS ok FROM sobre_del_resumen WHERE user_id = $1::uuid AND fecha_eval = $2::date LIMIT 1",
    params = list(user_id, as.character(fecha_eval))
  )
  nrow(df) > 0
}

alertas_digest_max_eventos <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("ALERTAS_DIGEST_MAX_EVENTOS", "80")))
  if (is.na(n) || n < 10L) 80L else as.integer(n)
}

# Horizonte correo: tempranas hoy..hoy+(N-1); tardias solo ayer (fecha_eval - 1).
alertas_temprana_horizonte_dias <- function() {
  # EMIT: lead 0-1 => 2 dias calendario (hoy y mañana).
  n <- suppressWarnings(as.integer(Sys.getenv("ALERTAS_TEMPRANA_HORIZONTE_DIAS", "2")))
  if (is.na(n) || n < 1L) 2L else min(as.integer(n), 2L)
}

alertas_digest_filtrar_ventana_correo <- function(eventos, fecha_eval) {
  if (length(eventos) == 0) return(eventos)
  fe <- as.Date(fecha_eval)
  ayer <- fe - 1L
  horiz <- alertas_temprana_horizonte_dias()
  hasta_t <- fe + (horiz - 1L)
  rank_max <- if (exists("alerta_digest_rank_max", mode = "function")) {
    alerta_digest_rank_max()
  } else {
    3L
  }
  Filter(function(ev) {
    f <- suppressWarnings(as.Date(ev$fecha %||% NA))
    if (is.na(f)) return(FALSE)
    tipo <- ev$tipo %||% "tardia"
    if (identical(tipo, "temprana")) {
      rk <- suppressWarnings(as.integer(ev$ranking %||% 99L))
      if (is.na(rk) || rk > rank_max) return(FALSE)
      f >= fe && f <= hasta_t
    } else {
      identical(f, ayer)
    }
  }, eventos)
}

alertas_push_ensure <- function() {
  if (exists("alertas_push_digest_usuario", mode = "function")) return(invisible(TRUE))
  src <- "/app/utils/alertas_push.R"
  if (!file.exists(src)) return(invisible(FALSE))
  source(src, local = FALSE)
  invisible(TRUE)
}

extremos_emision_boot <- function() {
  if (exists("extremos_emision_usuarios", mode = "function")) return(invisible(TRUE))
  for (p in c("/app/utils/alertas_extremo_emision.R",
              file.path(getwd(), "utils", "alertas_extremo_emision.R"))) {
    if (file.exists(p)) {
      source(p, local = FALSE)
      return(invisible(TRUE))
    }
  }
  invisible(FALSE)
}

# El arranque vive aca, no dentro del archivo que carga. Si no, el digest
# llama extremos_anticipada_boot() antes de que esa funcion exista.
extremos_anticipada_boot <- function() {
  if (exists("extremos_anticipada_usuarios", mode = "function")) return(invisible(TRUE))
  for (p in c("/app/utils/alertas_extremo_anticipada_salida.R",
              file.path(getwd(), "utils", "alertas_extremo_anticipada_salida.R"))) {
    if (file.exists(p)) {
      source(p, local = FALSE)
      return(invisible(TRUE))
    }
  }
  invisible(FALSE)
}

alertas_enviar_digests <- function(conn, fecha_eval, app_url, dry_run = FALSE) {
  tryCatch({
    if (!exists("correo_rebote_procesar_buzon", mode = "function")) {
      for (p_reb in c(
        file.path(Sys.getenv("API_ROOT", "/api"), "utils", "correo_rebote.R"),
        "/api/utils/correo_rebote.R"
      )) {
        if (file.exists(p_reb)) {
          source(p_reb, local = FALSE)
          break
        }
      }
    }
    if (exists("correo_rebote_procesar_buzon", mode = "function")) {
      correo_rebote_procesar_buzon(conn = conn)
    }
  }, error = function(e) {
    message("[carta_devuelta] procesar: ", conditionMessage(e))
  })
  alertas_push_ensure()
  if (!exists("alertas_tipo_usar", mode = "function")) {
    p <- "/api/utils/alertas_tipo_control.R"
    if (file.exists(p)) tryCatch(source(p, local = FALSE), error = function(e) NULL)
  }
  if (exists("alertas_tipo_usar", mode = "function")) {
    tryCatch(alertas_tipo_usar(conn), error = function(e) NULL)
  }
  defer_raw <- tolower(trimws(Sys.getenv("ALERTAS_DIGEST_DEFER", "0")))
  if (defer_raw %in% c("1", "true", "yes", "on")) {
    # Soft-continue con ingest incompleto: eventos quedan pendiente_digest; flush en FromGapfill.
    alertas_log("[alertas] DIGEST_DEFER=1; encolados sin enviar (flush post-gapfill o 12:00)")
    return(0L)
  }
  if (!alertas_tabla_existe_conn(conn, "campanada")) return(0L)
  tiene_pendiente <- alertas_db_query(conn,
    "SELECT column_name FROM information_schema.columns
     WHERE table_name = 'campanada' AND column_name = 'pendiente_digest' LIMIT 1"
  )
  if (nrow(tiene_pendiente) == 0) return(0L)

  alertas_umbral_boot()
  user_sql <- "SELECT DISTINCT user_id::text AS user_id FROM campanada WHERE pendiente_digest = TRUE"
  if (exists("alertas_umbral_sql_usuarios", mode = "function")) {
    extra_users <- alertas_umbral_sql_usuarios(conn)
    if (nzchar(extra_users %||% "")) user_sql <- paste0(user_sql, " UNION ", extra_users)
  }
  df_users <- alertas_db_query(conn, user_sql)
  if (!exists("extremos_emision_usuarios", mode = "function")) extremos_emision_boot()
  if (exists("extremos_emision_usuarios", mode = "function")) {
    eu <- tryCatch(extremos_emision_usuarios(conn, fecha_eval), error = function(e) character())
    if (length(eu)) {
      df_users <- rbind(df_users, data.frame(user_id = eu, stringsAsFactors = FALSE))
      df_users <- df_users[!duplicated(df_users$user_id), , drop = FALSE]
    }
  }
  if (!exists("extremos_anticipada_usuarios", mode = "function")) extremos_anticipada_boot()
  if (exists("extremos_anticipada_usuarios", mode = "function")) {
    au <- tryCatch(extremos_anticipada_usuarios(conn, fecha_eval), error = function(e) character())
    if (length(au)) {
      df_users <- rbind(df_users, data.frame(user_id = au, stringsAsFactors = FALSE))
      df_users <- df_users[!duplicated(df_users$user_id), , drop = FALSE]
    }
  }
  if (nrow(df_users) == 0) return(0L)

  sent <- 0L
  max_body <- alertas_digest_max_eventos()
  for (i in seq_len(nrow(df_users))) {
    uid <- df_users$user_id[i]
    email_ya <- !isTRUE(dry_run) && alertas_digest_ya_enviado(conn, uid, fecha_eval)

    tipo_sel <- if (alertas_tipo_columna_existe(conn)) {
      ", COALESCE(e.tipo, 'tardia') AS tipo"
    } else {
      ", 'tardia' AS tipo"
    }
    ev_df <- alertas_db_query(conn,
      paste0(
        "SELECT e.id, e.eid, e.variable_id, e.fecha, e.direccion, e.ranking, e.valor",
        tipo_sel, ",
              est.nombre_publico AS estacion_nombre, est.nom_com,
              v.nombre AS variable_nombre, lower(v.nombre_abr) AS nombre_abr,
              COALESCE(v.unidad, '') AS unidad
       FROM campanada e
       LEFT JOIN mirador est ON est.id = e.eid
       LEFT JOIN magnitud v ON v.id = e.variable_id
       WHERE e.user_id = $1::uuid AND e.pendiente_digest = TRUE
       ORDER BY e.ranking ASC, e.fecha DESC"
      ),
      params = list(uid)
    )
    u_df <- alertas_db_query(conn,
      "SELECT email FROM tripulacion WHERE id = $1::uuid",
      params = list(uid)
    )
    if (nrow(u_df) == 0) next
    prefs <- alertas_cargar_preferencias(conn, uid)
    email <- alertas_resolver_email(u_df$email[1], prefs)
    if (!nzchar(email) || grepl("@pending\\.rodaja\\.cl$", email)) next
    email_bloqueado <- exists("correo_rebote_esta_bloqueado", mode = "function") &&
      isTRUE(correo_rebote_esta_bloqueado(email, conn = conn))
    if (isTRUE(email_bloqueado)) {
      message("[alertas] digest omitido (correo bloqueado por rebotes) ", email)
    }

    eventos <- lapply(seq_len(nrow(ev_df)), function(j) {
      row <- ev_df[j, ]
      list(
        eid = as.integer(row$eid),
        variable_id = as.integer(row$variable_id),
        fecha = as.character(row$fecha),
        fecha_larga = reporte_fmt_fecha_larga(row$fecha),
        direccion = row$direccion,
        ranking = as.integer(row$ranking),
        valor = as.numeric(row$valor),
        unidad = "",
        estacion_nombre = row$estacion_nombre %||% paste0("Estacion ", row$eid),
        nom_com = if ("nom_com" %in% names(row)) row$nom_com %||% "" else "",
        nombre_abr = if ("nombre_abr" %in% names(row)) row$nombre_abr %||% "" else "",
        unidad = if ("unidad" %in% names(row)) row$unidad %||% "" else "",
        variable_nombre = row$variable_nombre %||% paste0("Var ", row$variable_id),
        tipo = if ("tipo" %in% names(row) && nzchar(row$tipo %||% "")) row$tipo else "tardia"
      )
    })

    n_cola <- length(eventos)
    eventos <- alertas_digest_filtrar_ventana_correo(eventos, fecha_eval)
    if (n_cola > length(eventos)) {
      message("[alertas] digest ventana ", email, ": cola=", n_cola,
              " correo=", length(eventos),
              " (temprana<=", as.character(as.Date(fecha_eval) + alertas_temprana_horizonte_dias() - 1L),
              " tardia=", as.character(as.Date(fecha_eval) - 1L), ")")
    }

    split_u <- list(mail = list(), push = list())
    if (exists("alertas_umbral_eventos_digest", mode = "function")) {
      split_u <- tryCatch(
        alertas_umbral_eventos_digest(conn, uid, fecha_eval),
        error = function(e) {
          message("[alertas] umbral digest: ", conditionMessage(e))
          list(mail = list(), push = list())
        }
      )
    }
    umbral_on <- !exists("alertas_tipo_emite_id", mode = "function") ||
      isTRUE(alertas_tipo_emite_id("umbral_usuario", TRUE))
    if (umbral_on) {
      eventos_push <- c(eventos, split_u$push %||% list())
      eventos <- c(eventos, split_u$mail %||% list())
    } else {
      eventos_push <- eventos
    }
    if (exists("extremos_digest_sin_tardia_temp", mode = "function")) {
      eventos <- extremos_digest_sin_tardia_temp(eventos)
      eventos_push <- extremos_digest_sin_tardia_temp(eventos_push)
    }
    if (!exists("extremos_emision_para_digest", mode = "function")) extremos_emision_boot()
    if (exists("extremos_emision_para_digest", mode = "function")) {
      extra_ex <- tryCatch(
        extremos_emision_para_digest(conn, uid, fecha_eval, eventos),
        error = function(e) {
          message("[alertas] extremo digest: ", conditionMessage(e))
          list()
        }
      )
      eventos <- c(eventos, extra_ex)
      eventos_push <- c(eventos_push, extra_ex)
    }
    if (!exists("extremos_anticipada_para_digest", mode = "function")) extremos_anticipada_boot()
    if (exists("extremos_anticipada_sin_clasica", mode = "function")) {
      eventos <- extremos_anticipada_sin_clasica(eventos, fecha_eval)
      eventos_push <- extremos_anticipada_sin_clasica(eventos_push, fecha_eval)
    }
    extra_ant <- list()
    if (exists("extremos_anticipada_para_digest", mode = "function")) {
      extra_ant <- tryCatch(
        extremos_anticipada_para_digest(conn, uid, fecha_eval),
        error = function(e) {
          message("[alertas] anticipada digest: ", conditionMessage(e))
          list()
        }
      )
      eventos <- c(eventos, extra_ant)
      if (exists("extremos_anticipada_push_activo", mode = "function") &&
          isTRUE(extremos_anticipada_push_activo())) {
        eventos_push <- c(eventos_push, extra_ant)
      }
    }
    if (exists("extremos_digest_ambito_amplio", mode = "function") &&
        exists("extremos_digest_por_comuna", mode = "function") &&
        isTRUE(extremos_digest_ambito_amplio(conn, uid))) {
      eventos <- extremos_digest_por_comuna(eventos)
      eventos_push <- extremos_digest_por_comuna(eventos_push)
    }

    if (isTRUE(dry_run)) {
      message("[alertas:dry] digest ", email, " n=", length(eventos))
      if (exists("alertas_push_digest_usuario", mode = "function")) {
        alertas_push_digest_usuario(conn, uid, fecha_eval, eventos_push, dry_run = TRUE)
      }
      sent <- sent + 1L
      next
    }

    if (length(eventos) == 0L && length(eventos_push) == 0L) {
      if (nrow(ev_df) > 0) {
        if (exists("alertas_push_digest_usuario", mode = "function")) {
          alertas_push_digest_usuario(conn, uid, fecha_eval, eventos_push, dry_run = FALSE)
        }
        alertas_db_execute(conn,
          "UPDATE campanada SET pendiente_digest = FALSE, exito = FALSE
           WHERE user_id = $1::uuid AND pendiente_digest = TRUE",
          params = list(uid)
        )
        message("[alertas] digest omitido (sin eventos en ventana) ", email)
      }
      next
    }

    if (length(eventos) == 0L) {
      push_res <- list(cerrado = TRUE)
      if (exists("alertas_push_digest_usuario", mode = "function")) {
        push_res <- alertas_push_digest_usuario(conn, uid, fecha_eval, eventos_push, dry_run = FALSE)
      }
      if (nrow(ev_df) > 0) {
        alertas_db_execute(conn,
          "UPDATE campanada SET pendiente_digest = FALSE, exito = FALSE
           WHERE user_id = $1::uuid AND pendiente_digest = TRUE",
          params = list(uid)
        )
      }
      if (umbral_on && isTRUE(push_res$cerrado) && exists("alertas_umbral_cerrar_pendientes", mode = "function")) {
        tryCatch(alertas_umbral_cerrar_pendientes(conn, uid, fecha_eval), error = function(e) NULL)
      }
      next
    }

    email_ok <- email_ya
    digest_id <- NULL
    if (isTRUE(email_bloqueado)) {
      email_ok <- FALSE
    } else if (!email_ya) {
      eventos_body <- if (length(eventos) > max_body) eventos[seq_len(max_body)] else eventos
      if (length(eventos) > max_body) {
        message("[alertas] digest truncado ", email, ": body=", length(eventos_body),
                " total=", length(eventos))
      }
      res <- enviar_alerta_digest(
        email, eventos_body, app_url, fecha_eval, eventos_resumen = eventos
      )
      if (alertas_tabla_existe_conn(conn, "sobre_del_resumen")) {
        ins <- alertas_db_query(conn,
          "INSERT INTO sobre_del_resumen (user_id, fecha_eval, n_eventos, exito, error_msg)
           VALUES ($1::uuid, $2::date, $3, $4, $5)
           ON CONFLICT (user_id, fecha_eval) DO NOTHING
           RETURNING id",
          params = list(
            uid, as.character(fecha_eval), length(eventos), isTRUE(res$ok),
            alertas_sql_text_param(res$error)
          )
        )
        if (nrow(ins) > 0) digest_id <- ins$id[1]
      }
      email_ok <- isTRUE(res$ok)
      if (email_ok && exists("extremos_emision_marcar", mode = "function")) {
        tryCatch(extremos_emision_marcar(conn, uid, eventos), error = function(e) {
          message("[alertas] extremo envio: ", conditionMessage(e))
        })
      }
      if (email_ok && exists("extremos_anticipada_marcar", mode = "function")) {
        tryCatch(extremos_anticipada_marcar(conn, uid, eventos), error = function(e) {
          message("[alertas] anticipada envio: ", conditionMessage(e))
        })
      }
      if (email_ok) {
        sent <- sent + 1L
        message("[alertas] digest enviado ", email, " eventos=", length(eventos),
                if (length(eventos_body) < length(eventos))
                  paste0(" (body=", length(eventos_body), ")") else "")
      } else {
        message("[alertas] fallo digest ", email, ": ", res$error %||% "desconocido")
      }
    }

    push_cerrado <- TRUE
    if (exists("alertas_push_digest_usuario", mode = "function")) {
      push_res <- alertas_push_digest_usuario(conn, uid, fecha_eval, eventos_push, dry_run = FALSE)
      push_cerrado <- isTRUE(push_res$cerrado)
    }
    # Correo ya intentado (esta pasada o una fila previa). El push en horario
    # silencioso no cierra la cola: la critica sale y el resto espera a la manana.
    if (isTRUE(email_bloqueado)) {
      # Deja pendiente_digest para reintentar el dia que Reactiven el correo.
      next
    }
    if (isTRUE(push_cerrado)) {
      if (!email_ya && isTRUE(email_ok) && !is.null(digest_id)) {
        alertas_db_execute(conn,
          "UPDATE campanada SET pendiente_digest = FALSE, exito = TRUE, digest_id = $2::uuid, enviado_en = NOW()
           WHERE user_id = $1::uuid AND pendiente_digest = TRUE",
          params = list(uid, digest_id)
        )
      } else if (!email_ya && isTRUE(email_ok)) {
        alertas_db_execute(conn,
          "UPDATE campanada SET pendiente_digest = FALSE, exito = TRUE, enviado_en = NOW()
           WHERE user_id = $1::uuid AND pendiente_digest = TRUE",
          params = list(uid)
        )
      } else {
        alertas_db_execute(conn,
          "UPDATE campanada SET pendiente_digest = FALSE
           WHERE user_id = $1::uuid AND pendiente_digest = TRUE",
          params = list(uid)
        )
      }
      if (umbral_on && exists("alertas_umbral_cerrar_pendientes", mode = "function")) {
        tryCatch(alertas_umbral_cerrar_pendientes(conn, uid, fecha_eval), error = function(e) NULL)
      }
    } else if (!email_ya && isTRUE(email_ok) && !is.null(digest_id)) {
      alertas_db_execute(conn,
        "UPDATE campanada SET exito = TRUE, digest_id = $2::uuid, enviado_en = NOW()
         WHERE user_id = $1::uuid AND pendiente_digest = TRUE",
        params = list(uid, digest_id)
      )
    }
  }
  sent
}

# Fan-out barato: candidatos globales (df) x reglas activas. El calculo caro
# (ranking) ya se hizo una vez; aqui solo se intersecan eids y se aplican prefs.
alertas_tardia_fanout <- function(conn, cand, subs, dry_run = FALSE) {
  if (!exists("alertas_tipo_usar", mode = "function")) {
    p <- "/api/utils/alertas_tipo_control.R"
    if (file.exists(p)) tryCatch(source(p, local = FALSE), error = function(e) NULL)
  }
  if (exists("alertas_tipo_usar", mode = "function")) {
    tryCatch(alertas_tipo_usar(conn), error = function(e) NULL)
  }
  if (exists("alertas_tipo_emite_id", mode = "function") &&
      !isTRUE(alertas_tipo_emite_id("tardia_clasica", FALSE))) {
    return(0L)
  }
  if (is.null(cand) || nrow(cand) == 0 || nrow(subs) == 0) return(0L)
  queued <- 0L
  prefs_cache <- list()
  for (i in seq_len(nrow(subs))) {
    row <- subs[i, ]
    uid <- row$user_id
    if (is.null(prefs_cache[[uid]])) {
      prefs_cache[[uid]] <- alertas_cargar_preferencias(conn, uid)
    }
    prefs <- prefs_cache[[uid]]
    if (!isTRUE(prefs$alerta_tardia)) next

    sv <- tryCatch(jsonlite::fromJSON(row$scope_value %||% "{}", simplifyVector = FALSE),
                   error = function(e) list())
    eids <- alertas_resolver_scope_eids(conn, uid, row$scope_type, sv)
    if (length(eids) == 0) next

    sel <- cand[cand$id_estacion %in% as.integer(eids), , drop = FALSE]
    if (nrow(sel) == 0) next
    if (!is.null(prefs$variable_ids) && length(prefs$variable_ids) > 0) {
      sel <- sel[sel$id_variable %in% as.integer(prefs$variable_ids), , drop = FALSE]
    }
    if (nrow(sel) == 0) next

    for (j in seq_len(nrow(sel))) {
      ev <- sel[j, ]
      direccion <- as.character(ev$direccion)
      if (direccion == "max" && !isTRUE(prefs$alerta_max)) next
      if (direccion == "min" && !isTRUE(prefs$alerta_min)) next
      if (isTRUE(dry_run)) {
        message("[alertas:tardia:dry] user=", uid, " eid=", ev$id_estacion,
                " vid=", ev$id_variable, " ", direccion, " rank=", ev$ranking)
        queued <- queued + 1L
        next
      }
      alertas_registrar_evento_pendiente(
        conn, uid, row$regla_id, as.integer(ev$id_estacion), as.integer(ev$id_variable),
        as.character(ev$fecha), direccion, as.integer(ev$ranking), as.numeric(ev$valor)
      )
      queued <- queued + 1L
    }
  }
  queued
}

# Fallback legacy (sin jornada_cerrada): loop par-a-par con COUNT sobre serie.
alertas_tardia_eval_legacy <- function(conn, subs, dry_run = FALSE) {
  prefs_cache <- list()
  for (i in seq_len(nrow(subs))) {
    row <- subs[i, ]
    uid <- row$user_id
    if (is.null(prefs_cache[[uid]])) {
      prefs_cache[[uid]] <- alertas_cargar_preferencias(conn, uid)
    }
    prefs <- prefs_cache[[uid]]
    if (!isTRUE(prefs$alerta_tardia)) next
    sv <- tryCatch(jsonlite::fromJSON(row$scope_value %||% "{}", simplifyVector = FALSE),
                   error = function(e) list())
    eids <- alertas_resolver_scope_eids(conn, uid, row$scope_type, sv)
    if (length(eids) == 0) next
    for (eid in eids) {
      vids <- alertas_variables_con_datos(conn, eid, prefs$variable_ids)
      for (vid in vids) {
        alertas_evaluar_par(conn, uid, row$regla_id, eid, vid, prefs, dry_run = dry_run)
      }
    }
  }
  invisible(NULL)
}

evaluar_alertas_top10 <- function(project_root = NULL, dry_run = FALSE) {
  if (is.null(project_root)) {
    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    run_dir <- normalizePath(dirname(sub("^--file=", "", fa[1])), winslash = "/", mustWork = FALSE)
    project_root <- normalizePath(file.path(run_dir, "..", ".."), winslash = "/", mustWork = FALSE)
  }
  alertas_bootstrap_reporte(project_root)
  alertas_reset_cache()

  conn <- get_db_connection()
  on.exit(tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL))

  sp_load <- NULL
  if (exists("profile_span_begin", mode = "function")) {
    sp_load <- tryCatch(
      profile_span_begin("phase.load_reglas", "phase", attrs = list(step = "alertas_top10")),
      error = function(e) NULL
    )
  }
  subs <- alertas_cargar_reglas_activas(conn)
  if (exists("profile_span_end", mode = "function") && !is.null(sp_load)) {
    tryCatch(profile_span_end(sp_load, list(n_reglas = nrow(subs))), error = function(e) NULL)
  }
  if (nrow(subs) == 0) {
    message("[alertas] sin reglas activas")
    alertas_umbral_correr(conn, dry_run)
    return(invisible(list(digests = 0L, dry_run = dry_run)))
  }

  if (isTRUE(dry_run)) message("[alertas] modo simulacion")

  fecha_eval <- Sys.Date()
  app_url <- trimws(Sys.getenv("APP_PUBLIC_URL", "https://app.rodaja.cl"))

  sp_eval <- NULL
  if (exists("profile_span_begin", mode = "function")) {
    sp_eval <- tryCatch(
      profile_span_begin("phase.eval_tardias", "phase", attrs = list(step = "alertas_top10")),
      error = function(e) NULL
    )
  }
  queued <- 0L
  mode <- "batch"
  if (alertas_top10_diaria_disponible(conn)) {
    # Correo: solo tardias del dia de ayer (dato observado en fecha_eval - 1).
    cand <- alertas_tardia_candidatos_batch(conn, hasta = fecha_eval - 1L)
    queued <- alertas_tardia_fanout(conn, cand, subs, dry_run = dry_run)
    message("[alertas] tardias encoladas: ", queued, " (fecha=", as.character(fecha_eval - 1L), ")")
  } else {
    mode <- "legacy"
    alertas_tardia_eval_legacy(conn, subs, dry_run = dry_run)
  }
  if (exists("profile_span_end", mode = "function") && !is.null(sp_eval)) {
    tryCatch(profile_span_end(sp_eval, list(mode = mode, queued = queued, n_reglas = nrow(subs))),
             error = function(e) NULL)
  }

  alertas_umbral_correr(conn, dry_run)
  # ponytail: digest unico al final de evaluar_alertas_pronostico (pipeline completo)
  message("[alertas] evaluacion completa (digest diferido a pronostico)")
  invisible(list(dry_run = dry_run))
}
