# Helpers: estimacion de filas/bytes y formatos de exportacion.

BYTES_POR_FILA_LIVIANO <- 30L
BYTES_POR_FILA_COMPLETO <- 55L
BYTES_FIJO_LIVIANO <- 20L
BYTES_FIJO_COMPLETO <- 800L

descarga_detect_var_type <- function(nombre) {
  n <- tolower(trimws(nombre %||% ""))
  if (!nzchar(n)) return("other")
  if (grepl("precip|lluvia|pp", n) && !grepl("viento", n)) return("precip")
  if (grepl("u_max_diario|u_rafaga|u_rafaga_h|rafaga|viento.*max", n)) return("wind_max")
  if (grepl("u2_min|viento.*min", n)) return("wind_min")
  if (grepl("u_10_kt|predomin", n)) return("wind_kt")
  if (grepl("u2_mean|viento", n)) return("wind_mean")
  if (grepl("max|m[aá]x", n)) return("temp_max")
  if (grepl("min|m[ií]n", n)) return("temp_min")
  if (grepl("temp|temperatura", n)) return("temp_mean")
  "other"
}

descarga_sql_agg_expr <- function(vtype, nombre_abr = "") {
  abr <- tolower(trimws(as.character(nombre_abr %||% "")))
  if (identical(vtype, "precip") || identical(abr, "pp")) return("SUM(o.valor)")
  if (identical(vtype, "wind_max") || abr %in% c("u_max_diario", "u_rafaga", "u_rafaga_h")) return("MAX(o.valor)")
  if (identical(vtype, "wind_min") || identical(abr, "u2_min")) return("MIN(o.valor)")
  "AVG(o.valor)"
}

descarga_estimar_registros <- function(estacion_ids, variable_ids, fecha_inicio, fecha_fin, granularidad) {
  estacion_ids <- as.integer(estacion_ids)
  variable_ids <- as.integer(variable_ids)
  estacion_ids <- estacion_ids[!is.na(estacion_ids) & estacion_ids > 0L]
  variable_ids <- variable_ids[!is.na(variable_ids) & variable_ids > 0L]
  if (length(estacion_ids) == 0L || length(variable_ids) == 0L) return(0L)
  granularidad <- tolower(trimws(granularidad %||% "diario"))
  if (!granularidad %in% GRANULARIDADES_VALIDAS) return(0L)

  f_ini <- as.Date(fecha_inicio)
  f_fin <- as.Date(fecha_fin)
  if (is.na(f_ini) || is.na(f_fin) || f_fin < f_ini) return(0L)

  ids_est <- paste(estacion_ids, collapse = ",")
  ids_var <- paste(variable_ids, collapse = ",")

  if (identical(granularidad, "horaria")) {
    vids_hora <- descarga_ids_rafaga_h(variable_ids)
    if (length(vids_hora) == 0L) return(0L)
    horas <- descarga_horas_por_par(estacion_ids, vids_hora, f_ini, f_fin)
    if (nrow(horas) == 0L) return(0L)
    n <- sum(horas$n)
    if (is.na(n) || n < 1) return(0L)
    if (n > 2147483647) return(2147483647L)
    return(as.integer(n))
  }

  if (identical(granularidad, "diario")) {
    q <- paste0(
      "SELECT COUNT(*)::bigint AS n FROM bitacora_oficial o ",
      "WHERE o.id_estacion IN (", ids_est, ") AND o.id_variable IN (", ids_var, ") ",
      "AND o.fecha >= $1 AND o.fecha <= $2"
    )
  } else if (identical(granularidad, "mensual")) {
    q <- paste0(
      "SELECT COUNT(*)::bigint AS n FROM (",
      "SELECT DISTINCT o.id_estacion, o.id_variable, o.year, o.month ",
      "FROM bitacora_oficial o ",
      "WHERE o.id_estacion IN (", ids_est, ") AND o.id_variable IN (", ids_var, ") ",
      "AND o.fecha >= $1 AND o.fecha <= $2",
      ") sub"
    )
  } else {
    q <- paste0(
      "SELECT COUNT(*)::bigint AS n FROM (",
      "SELECT DISTINCT o.id_estacion, o.id_variable, o.year ",
      "FROM bitacora_oficial o ",
      "WHERE o.id_estacion IN (", ids_est, ") AND o.id_variable IN (", ids_var, ") ",
      "AND o.fecha >= $1 AND o.fecha <= $2",
      ") sub"
    )
  }

  df <- db_query(q, params = list(f_ini, f_fin))
  if (is.null(df) || nrow(df) == 0L) return(0L)
  as.integer(df$n[1])
}

descarga_estimar_registros_cliente <- function(fecha_inicio, fecha_fin, granularidad,
                                                n_estaciones = 1L, n_variables = 1L) {
  f_ini <- as.Date(fecha_inicio)
  f_fin <- as.Date(fecha_fin)
  if (is.na(f_ini) || is.na(f_fin) || f_fin < f_ini) return(0L)
  granularidad <- tolower(trimws(granularidad %||% "diario"))
  dias <- as.integer(difftime(f_fin, f_ini, units = "days")) + 1L
  n_estaciones <- max(1L, as.integer(n_estaciones))
  n_variables <- max(1L, as.integer(n_variables))
  base <- if (identical(granularidad, "horaria")) {
    dias * 24L
  } else if (identical(granularidad, "diario")) {
    dias
  } else if (identical(granularidad, "mensual")) {
    (as.integer(format(f_fin, "%Y")) - as.integer(format(f_ini, "%Y"))) * 12L +
      as.integer(format(f_fin, "%m")) - as.integer(format(f_ini, "%m")) + 1L
  } else {
    as.integer(format(f_fin, "%Y")) - as.integer(format(f_ini, "%Y")) + 1L
  }
  as.numeric(base) * n_estaciones * n_variables
}

format_bytes <- function(bytes) {
  bytes <- as.numeric(bytes)
  if (is.na(bytes) || bytes < 0) bytes <- 0
  units <- c("B", "KB", "MB", "GB")
  i <- 1L
  v <- bytes
  while (v >= 1024 && i < length(units)) {
    v <- v / 1024
    i <- i + 1L
  }
  decimals <- if (i > 1L && v < 10) 1L else 0L
  label <- paste0(format(round(v, decimals), nsmall = decimals, trim = TRUE), " ", units[i])
  list(
    bytes = round(bytes),
    value = as.numeric(round(v, decimals)),
    unit = units[i],
    label = label
  )
}

# Entero de R se desborda en 2 GB (plan institucional = 3 GB).
descarga_bytes_num <- function(x) {
  x <- suppressWarnings(as.numeric(unlist(x))[1])
  if (is.na(x) || x < 0) return(0)
  if (x > 9e15) 9e15 else x
}

descarga_estimar_bytes <- function(registros, formato_descarga, n_estaciones = 1L, n_variables = 1L) {
  registros <- descarga_bytes_num(registros)
  if (registros < 1) return(0)
  fmt <- tolower(trimws(formato_descarga %||% "liviano"))
  if (identical(fmt, "completo")) {
    descarga_bytes_num(BYTES_FIJO_COMPLETO + registros * BYTES_POR_FILA_COMPLETO)
  } else {
    descarga_bytes_num(BYTES_FIJO_LIVIANO + registros * BYTES_POR_FILA_LIVIANO)
  }
}

descarga_on_demand_disponible <- function(user_id, tier, bytes_est) {
  bytes_est <- descarga_bytes_num(bytes_est)
  if (bytes_est < 1) return(FALSE)
  settings <- get_on_demand_settings(user_id, tier)
  if (!isTRUE(settings$can_enable) && !isTRUE(settings$enabled)) return(FALSE)
  if (isTRUE(settings$enabled)) {
    cuota <- get_cuota_usuario(user_id, tier)
    overage <- on_demand_overage_increment(cuota$bytes_usados, cuota$limite_bytes, bytes_est)
    if (overage <= 0) return(TRUE)
    clp <- on_demand_clp_for_bytes(overage)
    mes <- get_on_demand_mes(user_id)
    if (is.null(settings$limit_clp)) return(TRUE)
    return((mes$clp_acumulado + clp) <= settings$limit_clp)
  }
  isTRUE(settings$can_enable)
}

descarga_respuesta_cuota_excedida <- function(user_id, tier, bytes_est, code = "cuota_excedida") {
  cuota <- get_cuota_usuario(user_id, tier)
  bytes_est <- descarga_bytes_num(bytes_est)
  od <- on_demand_payload(user_id, tier)
  overage <- on_demand_overage_increment(cuota$bytes_usados, cuota$limite_bytes, bytes_est)
  costo <- if (overage > 0) on_demand_clp_for_bytes(overage) else 0L
  od_disp <- descarga_on_demand_disponible(user_id, tier, bytes_est)
  msg <- paste0(
    "Cuota de descarga excedida (", cuota$usado_label, " / ", cuota$limite_label, "). ",
    "Disponible: ", cuota$disponible_label, "."
  )
  if (od_disp && !isTRUE(od$enabled)) {
    msg <- paste0(msg, " Activa pago por uso en /cuenta/.")
  }
  list(
    error = msg,
    code = code,
    cuota = cuota,
    bytes_solicitados = bytes_est,
    bytes_solicitados_label = format_bytes(bytes_est)$label,
    on_demand_disponible = od_disp,
    on_demand = od,
    costo_overage_clp = costo,
    cuenta_url = "/cuenta/"
  )
}

descarga_excede_cuota <- function(user_id, tier, bytes_est) {
  bytes_est <- descarga_bytes_num(bytes_est)
  cuota <- get_cuota_usuario(user_id, tier)
  excede <- bytes_est >= 1 && bytes_est > cuota$bytes_disponibles
  od <- on_demand_payload(user_id, tier)
  overage <- if (excede) on_demand_overage_increment(cuota$bytes_usados, cuota$limite_bytes, bytes_est) else 0
  costo <- if (overage > 0) on_demand_clp_for_bytes(overage) else 0L
  permitido <- excede && isTRUE(od$enabled) && descarga_on_demand_disponible(user_id, tier, bytes_est)
  list(
    excede = excede && !permitido,
    cuota = cuota,
    bytes_est = bytes_est,
    on_demand = od,
    on_demand_disponible = descarga_on_demand_disponible(user_id, tier, bytes_est),
    costo_overage_clp = costo,
    dentro_cuota = !excede || permitido
  )
}

descarga_estimar_registros_rapido <- function(fecha_inicio, fecha_fin, granularidad,
                                              n_estaciones = 1L, n_variables = 1L) {
  descarga_estimar_registros_cliente(fecha_inicio, fecha_fin, granularidad, n_estaciones, n_variables)
}

descarga_normalize_granularidad <- function(granularidad) {
  g <- tolower(trimws(as.character(granularidad %||% "anual")))
  typos <- c(
    diaio = "diario", dfiario = "diario", diaro = "diario", diari = "diario",
    aual = "anual", anual = "anual",
    mensual = "mensual", mensul = "mensual"
  )
  if (g %in% names(typos)) return(unname(typos[g]))
  if (g %in% c("horaria", "hora", "h")) return("horaria")
  if (g %in% c("diario", "dia", "d")) return("diario")
  if (g %in% c("mensual", "mes", "m")) return("mensual")
  if (g %in% c("anual", "an", "a")) return("anual")
  if (grepl("^di", g)) return("diario")
  if (grepl("^men", g)) return("mensual")
  if (grepl("^anu", g)) return("anual")
  "anual"
}

descarga_fecha_rango_slug <- function(fecha_inicio, fecha_fin) {
  f_ini <- as.Date(fecha_inicio)
  f_fin <- as.Date(fecha_fin)
  if (is.na(f_ini) || is.na(f_fin)) {
    return(paste0(gsub("-", "", fecha_inicio), "_", gsub("-", "", fecha_fin)))
  }
  paste0(format(f_ini, "%Y"), "_", format(f_fin, "%Y"))
}

descarga_export_bundle_name <- function(formato, estacion_ids, variable_ids, fecha_inicio, fecha_fin, granularidad) {
  gran <- descarga_normalize_granularidad(granularidad)
  paste0(
    "rodaja_", formato, "_", gran, "_",
    descarga_fecha_rango_slug(fecha_inicio, fecha_fin), "_",
    length(estacion_ids), "est_", length(variable_ids), "var"
  )
}

descarga_resolver_nombre_archivo <- function(params, formato_descarga = NULL, registros_estimados = NULL) {
  if (!is.null(params$archivo_nombre) && nzchar(as.character(params$archivo_nombre))) {
    return(as.character(params$archivo_nombre))
  }

  estacion_ids <- unique(as.integer(unlist(params$estacion_ids)))
  variable_ids <- unique(as.integer(unlist(params$variable_ids)))
  estacion_ids <- estacion_ids[!is.na(estacion_ids) & estacion_ids > 0L]
  variable_ids <- variable_ids[!is.na(variable_ids) & variable_ids > 0L]
  if (length(estacion_ids) == 0L || length(variable_ids) == 0L) return(NULL)

  fmt <- tolower(formato_descarga %||% params$formato_descarga %||% "liviano")
  gran <- descarga_normalize_granularidad(params$granularidad %||% "anual")
  bundle <- descarga_export_bundle_name(fmt, estacion_ids, variable_ids, params$fecha_inicio, params$fecha_fin, gran)
  if (identical(fmt, "liviano")) return(paste0(bundle, ".csv"))
  paste0(bundle, ".zip")
}

descarga_nombre_parece_corrupto <- function(name) {
  if (is.null(name) || !nzchar(as.character(name))) return(TRUE)
  n <- tolower(as.character(name))
  grepl("_t_mi[_\\.]|_t_ma[_\\.]|_diaio[_\\.]|_dfiario[_\\.]|_aual[_\\.]|_diari[_\\.]", n)
}

descarga_nombre_desde_trabajo <- function(row) {
  params <- tryCatch(jsonlite::fromJSON(row$params[1]), error = function(e) list())
  resolved <- descarga_resolver_nombre_archivo(
    params,
    formato_descarga = row$formato_descarga %||% NULL,
    registros_estimados = row$registros_estimados %||% NULL
  )
  stored <- NULL
  if ("archivo_nombre" %in% names(row) && !is.na(row$archivo_nombre) && nzchar(as.character(row$archivo_nombre))) {
    stored <- as.character(row$archivo_nombre)
  } else if (!is.null(params$archivo_nombre) && nzchar(as.character(params$archivo_nombre))) {
    stored <- as.character(params$archivo_nombre)
  }
  if (!is.null(stored) && !descarga_nombre_parece_corrupto(stored)) return(stored)
  if (!is.null(resolved) && nzchar(resolved)) return(resolved)
  stored
}

descarga_parse_id_list <- function(ids_param, id_scalar = "") {
  out <- c(as.integer(unlist(ids_param)), suppressWarnings(as.integer(id_scalar)))
  unique(out[!is.na(out) & out > 0L])
}

descarga_parse_granularidades <- function(granularidades = list(), granularidad = "anual") {
  grans <- unique(tolower(trimws(as.character(unlist(granularidades)))))
  if (length(grans) == 0L && nzchar(trimws(granularidad %||% ""))) {
    grans <- tolower(trimws(granularidad))
  }
  grans <- grans[grans %in% GRANULARIDADES_VALIDAS]
  if (length(grans) == 0L) grans <- "anual"
  unique(grans)
}

.MV_VAR_ANIO <- list(set = FALSE, val = FALSE)

descarga_mv_variable_anio_existe <- function() {
  if (isTRUE(.MV_VAR_ANIO$set)) return(isTRUE(.MV_VAR_ANIO$val))
  chk <- tryCatch(
    db_query("SELECT to_regclass('public.estacion_variable_anio') AS t"),
    error = function(e) NULL
  )
  ok <- !is.null(chk) && nrow(chk) > 0L && !is.na(chk$t[1])
  .MV_VAR_ANIO <<- list(set = TRUE, val = ok)
  ok
}

descarga_anios_rango <- function(fecha_inicio, fecha_fin) {
  f_ini <- as.Date(fecha_inicio)
  f_fin <- as.Date(fecha_fin)
  if (is.na(f_ini) || is.na(f_fin) || f_fin < f_ini) return(NULL)
  c(as.integer(format(f_ini, "%Y")), as.integer(format(f_fin, "%Y")))
}

descarga_ids_ok <- function(ids) {
  ids <- unique(as.integer(unlist(ids)))
  ids[!is.na(ids) & ids > 0L]
}

# u_rafaga_h no está en bitacora_oficial. Diario/mensual/anual no la agregan.
descarga_ids_rafaga_h <- function(vids) {
  vids <- descarga_ids_ok(vids)
  if (length(vids) == 0L) return(integer(0))
  df <- tryCatch(
    db_query(
      "SELECT id FROM magnitud WHERE id = ANY($1::int[]) AND lower(nombre_abr) = 'u_rafaga_h'",
      params = list(pg_int_array_param(vids))
    ),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0L) integer(0) else descarga_ids_ok(df$id)
}

# Una fila por estación y variable horaria. El rango es el día civil UTC.
# ponytail: DGA guarda el instante local; cerca de medianoche puede caer en el día UTC vecino.
descarga_horas_por_par <- function(estacion_ids, vids_hora, fecha_inicio, fecha_fin) {
  eids <- descarga_ids_ok(estacion_ids)
  vids_hora <- descarga_ids_ok(vids_hora)
  f_ini <- as.Date(fecha_inicio)
  f_fin <- as.Date(fecha_fin)
  empty <- data.frame(id_variable = integer(), id_estacion = integer(), n = numeric())
  if (length(eids) == 0L || length(vids_hora) == 0L || is.na(f_ini) || is.na(f_fin)) return(empty)
  df <- tryCatch(
    db_query(
      paste0(
        "SELECT id_variable, id_estacion, COUNT(*)::float8 AS n ",
        "FROM reloj_de_horas WHERE ",
        "id_estacion = ANY($1::int[]) AND id_variable = ANY($2::int[]) ",
        "AND ts >= ($3::date::timestamp AT TIME ZONE 'UTC') ",
        "AND ts < (($4::date + 1)::timestamp AT TIME ZONE 'UTC') ",
        "GROUP BY id_variable, id_estacion"
      ),
      params = list(pg_int_array_param(eids), pg_int_array_param(vids_hora), f_ini, f_fin)
    ),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0L) return(empty)
  df$id_variable <- as.integer(df$id_variable)
  df$id_estacion <- as.integer(df$id_estacion)
  df$n <- as.numeric(df$n)
  df
}

# Una lectura de la vista. NULL si no existe: el llamador usa el calendario.
descarga_leer_variable_anio <- function(estacion_ids, variable_ids, fecha_inicio, fecha_fin) {
  if (!descarga_mv_variable_anio_existe()) return(NULL)
  eids <- descarga_ids_ok(estacion_ids)
  vids <- descarga_ids_ok(variable_ids)
  anios <- descarga_anios_rango(fecha_inicio, fecha_fin)
  if (length(eids) == 0L || length(vids) == 0L || is.null(anios)) return(NULL)
  params <- list(pg_int_array_param(eids), pg_int_array_param(vids), anios[1], anios[2])
  where <- paste(
    "id_estacion = ANY($1::int[]) AND id_variable = ANY($2::int[])",
    "AND anio >= $3 AND anio <= $4"
  )
  suma <- db_query(
    paste0(
      "SELECT COALESCE(SUM(n_dias), 0)::float8 AS n_dias,",
      " COALESCE(SUM(n_meses), 0)::float8 AS n_meses,",
      " COUNT(*)::float8 AS n_anios,",
      " COUNT(DISTINCT id_estacion)::float8 AS n_estaciones",
      " FROM estacion_variable_anio WHERE ", where
    ),
    params = params
  )
  ids <- db_query(
    paste0("SELECT DISTINCT id_estacion FROM estacion_variable_anio WHERE ", where),
    params = params
  )
  est_ids <- if (is.null(ids) || nrow(ids) == 0L) integer(0) else as.integer(ids$id_estacion)
  list(
    n_dias = if (is.null(suma) || nrow(suma) == 0L) 0 else as.numeric(suma$n_dias[1]),
    n_meses = if (is.null(suma) || nrow(suma) == 0L) 0 else as.numeric(suma$n_meses[1]),
    n_anios = if (is.null(suma) || nrow(suma) == 0L) 0 else as.numeric(suma$n_anios[1]),
    n_estaciones = if (is.null(suma) || nrow(suma) == 0L) 0 else as.numeric(suma$n_estaciones[1]),
    estacion_ids = est_ids
  )
}

descarga_regs_gran <- function(suma, granularidad, fecha_inicio, fecha_fin, n_estaciones, n_variables) {
  g <- tolower(trimws(granularidad %||% "anual"))
  if (!is.null(suma)) {
    if (identical(g, "horaria")) return(suma$n_horas %||% 0)
    if (identical(g, "diario")) return(suma$n_dias)
    if (identical(g, "mensual")) return(suma$n_meses)
    return(suma$n_anios)
  }
  descarga_estimar_registros_cliente(fecha_inicio, fecha_fin, g, n_estaciones, n_variables)
}

descarga_nombres_variable <- function(vids) {
  if (length(vids) == 0L) return(setNames(character(0), character(0)))
  var_df <- db_query(
    "SELECT id, COALESCE(nombre, nombre_abr, '') AS nombre FROM magnitud WHERE id = ANY($1::int[])",
    params = list(pg_int_array_param(vids))
  )
  if (is.null(var_df) || nrow(var_df) == 0L) return(setNames(character(0), character(0)))
  setNames(as.character(var_df$nombre), as.character(var_df$id))
}

descarga_cobertura_resumen <- function(estacion_ids, variable_ids, fecha_inicio, fecha_fin,
                                       incluir_ids = FALSE) {
  eids <- descarga_ids_ok(estacion_ids)
  vids <- descarga_ids_ok(variable_ids)
  var_nombres <- descarga_nombres_variable(vids)
  anios <- descarga_anios_rango(fecha_inicio, fecha_fin)
  con_por <- setNames(integer(length(vids)), as.character(vids))
  ids_completas <- eids
  aproximado <- !descarga_mv_variable_anio_existe()
  if (!aproximado && !is.null(anios) && length(eids) > 0L && length(vids) > 0L) {
    params <- list(pg_int_array_param(eids), pg_int_array_param(vids), anios[1], anios[2])
    where <- paste(
      "id_estacion = ANY($1::int[]) AND id_variable = ANY($2::int[])",
      "AND anio >= $3 AND anio <= $4"
    )
    df <- db_query(
      paste0(
        "SELECT id_variable, COUNT(DISTINCT id_estacion)::int AS con_datos",
        " FROM estacion_variable_anio WHERE ", where,
        " GROUP BY id_variable"
      ),
      params = params
    )
    if (!is.null(df) && nrow(df) > 0L) {
      con_por[as.character(df$id_variable)] <- as.integer(df$con_datos)
    }
    vids_hora <- descarga_ids_rafaga_h(vids)
    vids_dia <- vids[!vids %in% vids_hora]
    horas <- if (length(vids_hora) > 0L) descarga_horas_por_par(eids, vids_hora, fecha_inicio, fecha_fin) else NULL
    if (!is.null(horas) && nrow(horas) > 0L) {
      for (vid in vids_hora) {
        con_por[[as.character(vid)]] <- length(unique(horas$id_estacion[horas$id_variable == vid]))
      }
    }
    if (isTRUE(incluir_ids)) {
      ids_completas <- eids
      if (length(vids_dia) > 0L) {
        params_d <- list(pg_int_array_param(eids), pg_int_array_param(vids_dia), anios[1], anios[2])
        where_d <- paste(
          "id_estacion = ANY($1::int[]) AND id_variable = ANY($2::int[])",
          "AND anio >= $3 AND anio <= $4"
        )
        ids_df <- db_query(
          paste0(
            "SELECT id_estacion FROM estacion_variable_anio WHERE ", where_d,
            " GROUP BY id_estacion HAVING COUNT(DISTINCT id_variable) >= $5"
          ),
          params = c(params_d, list(length(vids_dia)))
        )
        ids_completas <- if (is.null(ids_df) || nrow(ids_df) == 0L) integer(0) else as.integer(ids_df$id_estacion)
      }
      if (length(vids_hora) > 0L) {
        for (vid in vids_hora) {
          tiene <- if (is.null(horas) || nrow(horas) == 0L) integer(0) else unique(horas$id_estacion[horas$id_variable == vid])
          ids_completas <- intersect(ids_completas, tiene)
        }
      }
    }
  }

  variables <- lapply(vids, function(vid) {
    con <- as.integer(con_por[[as.character(vid)]] %||% 0L)
    if (is.na(con)) con <- 0L
    if (aproximado) con <- length(eids)
    sin <- max(0L, length(eids) - con)
    list(
      variable_id = vid,
      nombre = var_nombres[[as.character(vid)]] %||% paste0("Variable ", vid),
      con_datos = con,
      sin_datos = sin,
      cobertura = paste0(con, "/", length(eids))
    )
  })

  list(
    variables = variables,
    total_estaciones = length(eids),
    total_variables = length(vids),
    pares_con_datos = sum(vapply(variables, function(v) as.integer(v$con_datos), integer(1))),
    aproximado = aproximado,
    estacion_ids = if (isTRUE(incluir_ids)) as.integer(ids_completas) else NULL
  )
}

descarga_pares_por_variable <- function() {
  if (!descarga_mv_variable_anio_existe()) return(list(disponible = FALSE, por_variable = list()))
  # array_agg llega roto a RPostgres; dos enteros por fila y se agrupa aqui.
  df <- db_query("SELECT DISTINCT id_variable, id_estacion FROM estacion_variable_anio")
  df_h <- tryCatch(
    db_query(
      "SELECT DISTINCT id_variable, id_estacion FROM reloj_de_horas WHERE id_variable = (SELECT id FROM magnitud WHERE lower(nombre_abr) = 'u_rafaga_h' LIMIT 1)"
    ),
    error = function(e) NULL
  )
  if (!is.null(df_h) && nrow(df_h) > 0L) {
    df <- if (is.null(df) || nrow(df) == 0L) df_h else rbind(df, df_h)
  }
  por <- list()
  if (!is.null(df) && nrow(df) > 0L) {
    df$id_variable <- as.integer(df$id_variable)
    df$id_estacion <- as.integer(df$id_estacion)
    for (vid in unique(df$id_variable)) {
      por[[as.character(vid)]] <- df$id_estacion[df$id_variable == vid]
    }
  }
  list(disponible = TRUE, por_variable = por)
}

descarga_estimar_bulk <- function(estacion_ids, variable_ids, granularidades,
                                  fecha_inicio, fecha_fin, formato_descarga,
                                  modo_cobertura = "solo_con_datos") {
  eids <- descarga_ids_ok(estacion_ids)
  vids <- descarga_ids_ok(variable_ids)
  grans <- descarga_parse_granularidades(granularidades)
  fmt <- tolower(trimws(formato_descarga %||% "liviano"))
  modo <- tolower(trimws(modo_cobertura %||% "solo_con_datos"))
  vids_hora <- descarga_ids_rafaga_h(vids)
  vids_dia <- vids[!vids %in% vids_hora]
  horas <- if (length(vids_hora) > 0L && "horaria" %in% grans) {
    tryCatch(descarga_horas_por_par(eids, vids_hora, fecha_inicio, fecha_fin), error = function(e) NULL)
  } else {
    NULL
  }
  n_horas <- if (is.null(horas) || nrow(horas) == 0L) 0 else sum(horas$n)
  est_hora <- if (is.null(horas) || nrow(horas) == 0L) integer(0) else unique(as.integer(horas$id_estacion))
  suma <- if (length(vids_dia) > 0L) {
    tryCatch(descarga_leer_variable_anio(eids, vids_dia, fecha_inicio, fecha_fin), error = function(e) NULL)
  } else {
    list(n_dias = 0, n_meses = 0, n_anios = 0, n_estaciones = 0, estacion_ids = integer(0))
  }
  aproximado <- is.null(suma)
  if (!is.null(suma)) suma$n_horas <- n_horas
  n_var_dia <- max(1L, length(vids_dia))

  desglose <- lapply(grans, function(gran) {
    regs <- if (identical(gran, "horaria")) {
      if (length(vids_hora) == 0L) 0 else n_horas
    } else if (length(vids_dia) == 0L) {
      0
    } else if (!aproximado && length(suma$estacion_ids) < 1L) {
      0
    } else {
      descarga_regs_gran(
        suma, gran, fecha_inicio, fecha_fin,
        if (aproximado) length(eids) else length(suma$estacion_ids),
        n_var_dia
      )
    }
    list(
      granularidad = gran,
      registros = descarga_bytes_num(regs),
      bytes = descarga_estimar_bytes(regs, fmt)
    )
  })

  registros_total <- sum(vapply(desglose, function(x) as.numeric(x$registros), numeric(1)))
  bytes_total <- sum(vapply(desglose, function(x) as.numeric(x$bytes), numeric(1)))
  est_out <- if (aproximado) eids else unique(c(as.integer(suma$estacion_ids), est_hora))

  list(
    registros_total = registros_total,
    bytes_estimados = bytes_total,
    desglose = desglose,
    estacion_ids = as.integer(est_out),
    variable_ids = vids,
    excluidos = list(),
    modo_cobertura = modo,
    aproximado = aproximado,
    estaciones_con_datos = length(est_out),
    combinaciones = list(
      estaciones = length(est_out),
      variables = length(vids),
      granularidades = length(grans),
      pares_con_datos = if (aproximado) length(eids) * length(vids) else as.integer(min(suma$n_anios, 2147483647))
    )
  )
}

descarga_validar_solicitud <- function(tier, estacion_ids, variable_ids, formato_descarga) {
  lim <- get_limite_tier(tier)
  eids <- unique(as.integer(estacion_ids))
  vids <- unique(as.integer(variable_ids))
  eids <- eids[!is.na(eids) & eids > 0L]
  vids <- vids[!is.na(vids) & vids > 0L]
  fmt <- resolve_formato_descarga(tier, formato_descarga)

  max_est <- lim$max_estaciones
  if (!is.null(max_est) && length(eids) > max_est) {
    return(list(ok = FALSE, error = paste0("M\u00e1ximo ", max_est, " estaci\u00f3n(es) permitida(s)"), code = "max_estaciones"))
  }
  if (!fmt %in% (lim$formatos %||% c("liviano"))) {
    return(list(ok = FALSE, error = paste0("Formato '", fmt, "' no disponible en plan ", tier), code = "formato_no_permitido"))
  }
  list(ok = TRUE, formato_descarga = fmt, estacion_ids = eids, variable_ids = vids)
}

descarga_estado_label <- function(estado) {
  switch(as.character(estado %||% ""),
    listo = "Listo",
    procesando = "Procesando",
    pendiente = "En cola",
    error = "Error",
    fallido = "Fallido",
    as.character(estado %||% "Desconocido")
  )
}

serie_bytes_descargas_usuario <- function(user_id, dias = 30L) {
  dias <- as.integer(dias)
  if (is.na(dias) || dias < 1L) dias <- 30L
  if (dias > 90L) dias <- 90L
  df <- db_query(
    "SELECT periodo_inicio AS dia, bytes_consumidos AS bytes
     FROM cupo_del_periodo
     WHERE user_id = $1::uuid AND periodo_inicio >= CURRENT_DATE - $2::integer
     ORDER BY periodo_inicio",
    params = list(user_id, dias)
  )
  if (nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) {
    list(
      dia = as.character(df$dia[i]),
      bytes = as.numeric(df$bytes[i])
    )
  })
}

listar_descargas_recientes_usuario <- function(user_id, limit = 15L) {
  limit <- as.integer(limit)
  if (is.na(limit) || limit < 1L) limit <- 15L
  if (limit > 50L) limit <- 50L
  df <- db_query(
    "SELECT id, estado, bytes_reales, bytes_estimados, formato_descarga,
            granularidad, archivo_nombre, creado_en, expira_en, lote_id
     FROM encargo_de_archivo
     WHERE user_id = $1::uuid
     ORDER BY creado_en DESC
     LIMIT $2",
    params = list(user_id, limit)
  )
  if (nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    bytes <- suppressWarnings(as.numeric(row$bytes_reales[1]))
    if (is.na(bytes) || bytes < 0) {
      bytes <- suppressWarnings(as.numeric(row$bytes_estimados[1]))
    }
    if (is.na(bytes) || bytes < 0) bytes <- 0
    est <- as.character(row$estado[1] %||% "")
    expira <- row$expira_en[1]
    expirado <- identical(est, "listo") && !is.na(expira) &&
      !is.null(expira) && as.POSIXct(expira, tz = "UTC") < Sys.time()
    fmt_b <- format_bytes(bytes)
    list(
      id = as.character(row$id[1]),
      estado = est,
      estado_label = descarga_estado_label(est),
      bytes = bytes,
      bytes_label = fmt_b$label,
      formato = as.character(row$formato_descarga[1] %||% ""),
      granularidad = as.character(row$granularidad[1] %||% ""),
      archivo_nombre = as.character(row$archivo_nombre[1] %||% ""),
      creado_en = as.character(row$creado_en[1]),
      expira_en = as.character(expira),
      expirado = isTRUE(expirado),
      lote_id = as.character(row$lote_id[1] %||% "")
    )
  })
}

descarga_sanitize_filename <- function(fname) {
  gsub('["\r\n]', "", as.character(fname %||% "descarga.zip"))
}

descarga_content_disposition <- function(fname) {
  fname <- descarga_sanitize_filename(fname)
  fname_ascii <- gsub("[^a-zA-Z0-9_.-]+", "_", fname)
  fname_enc <- utils::URLencode(fname, reserved = TRUE)
  paste0('attachment; filename="', fname_ascii, '"; filename*=UTF-8\'\'', fname_enc)
}

# ponytail: PDF vista se genera en el cliente; tarifa plana por exportacion
reporte_vista_pdf_bytes_estimar <- function() {
  raw <- Sys.getenv("REPORTE_VISTA_BYTES_EST", "3145728")
  n <- suppressWarnings(as.integer(raw))
  if (is.na(n) || n < 50000L) 3145728L else n
}

# Tope del CSV de la tabla anual (31 x 12 celdas). Por encima no es esa tabla.
REPORTE_TABLA_CSV_MAX_BYTES <- 2L * 1024L * 1024L

# Mide el CSV que el cliente ya armo y fija el nombre. El archivo que sale es este raw.
reporte_tabla_csv_payload <- function(csv, anio = NULL) {
  if (is.null(csv) || length(csv) < 1L) {
    return(list(ok = FALSE, error = "CSV vac\u00edo"))
  }
  txt <- enc2utf8(paste(as.character(csv), collapse = "\n"))
  if (!nzchar(txt)) return(list(ok = FALSE, error = "CSV vac\u00edo"))
  raw <- charToRaw(txt)
  n <- length(raw)
  if (n < 8L) return(list(ok = FALSE, error = "CSV vac\u00edo"))
  if (n > REPORTE_TABLA_CSV_MAX_BYTES) {
    return(list(ok = FALSE, error = "El CSV de la tabla supera el m\u00e1ximo permitido"))
  }
  anio_n <- suppressWarnings(as.integer(anio))
  fname <- if (!is.na(anio_n) && anio_n >= 1900L && anio_n <= 2100L) {
    sprintf("registros-diarios-%d.csv", anio_n)
  } else {
    "registros-diarios.csv"
  }
  list(ok = TRUE, bytes = n, raw = raw, filename = fname)
}
