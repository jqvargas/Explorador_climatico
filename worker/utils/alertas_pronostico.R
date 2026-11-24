# Evaluacion diaria alertas tempranas (catalogo global + email por suscripcion)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

alertas_pronostico_var_abrs <- function() {
  if (exists("alerta_temprana_var_abrs", mode = "function")) return(alerta_temprana_var_abrs())
  if (exists("alertas_catalogo_abrs", mode = "function")) {
    abrs <- alertas_catalogo_abrs()
    # u_max_diario no alcanza gate de 20 años: catalogo muerto, excluir.
    return(setdiff(abrs, "u_max_diario"))
  }
  c("pp", "t_max", "t_min")
}

alertas_log <- function(...) {
  message(...)
  flush.console()
}

alertas_pronostico_bootstrap <- function(project_root) {
  roots <- if (exists("worker_resolve_data_roots", mode = "function")) {
    worker_resolve_data_roots(project_root)
  } else {
    list(shiny = Sys.getenv("SHINY_ROOT", file.path(project_root, "shiny")),
         api = Sys.getenv("API_ROOT", file.path(project_root, "api")))
  }
  shiny_root <- roots$shiny
  api_root <- roots$api
  if (!nzchar(shiny_root) || !dir.exists(shiny_root)) {
    shiny_root <- file.path(project_root, "shiny")
  }
  if (!nzchar(api_root) || !dir.exists(api_root)) {
    api_root <- file.path(project_root, "api")
  }
  if (!dir.exists(shiny_root)) stop("No se encuentra shiny en ", shiny_root)
  Sys.setenv(SHINY_ROOT = shiny_root)
  if (!exists(".ALERTAS_PRON_DB", envir = .GlobalEnv)) {
    assign(".ALERTAS_PRON_DB", get_db_connection(), envir = .GlobalEnv)
  }
  assign("db_query", function(q, params = NULL) {
    conn <- get(".ALERTAS_PRON_DB", envir = .GlobalEnv)
    if (is.null(params) || length(params) == 0) DBI::dbGetQuery(conn, q)
    else DBI::dbGetQuery(conn, q, params = params)
  }, envir = .GlobalEnv)
  calidad_path <- file.path(api_root, "utils", "calidad.R")
  if (file.exists(calidad_path)) source(calidad_path, local = FALSE)
  catalog_path <- file.path(api_root, "utils", "alertas_catalog.R")
  if (file.exists(catalog_path)) source(catalog_path, local = FALSE)
  alerta_path <- file.path(api_root, "utils", "alerta_temprana.R")
  ud_path <- file.path(api_root, "utils", "unidades_display.R")
  if (file.exists(ud_path)) source(ud_path, local = FALSE)
  if (file.exists(alerta_path)) source(alerta_path, local = FALSE)
  for (od_path in c(
    file.path(project_root, "worker", "utils", "observacion_diaria.R"),
    "/app/utils/observacion_diaria.R"
  )) {
    if (file.exists(od_path)) {
      source(od_path, local = FALSE)
      break
    }
  }
  api_payload <- file.path(api_root, "utils", "reporte_payload.R")
  if (!file.exists(api_payload)) stop("No se encuentra ", api_payload)
  source(api_payload, local = FALSE)
  reporte_source_shiny()
  invisible(TRUE)
}

alertas_pronostico_db_close <- function() {
  if (exists(".ALERTAS_PRON_DB", envir = .GlobalEnv)) {
    conn <- get(".ALERTAS_PRON_DB", envir = .GlobalEnv)
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL)
    rm(".ALERTAS_PRON_DB", envir = .GlobalEnv)
  }
}

alertas_pronostico_pg_text_array <- function(vals) {
  if (is.null(vals) || length(vals) == 0) return(NA_character_)
  vals <- as.character(unlist(vals))
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) return(NA_character_)
  paste0("{", paste(vals, collapse = ","), "}")
}

alertas_pronostico_tabla_existe <- function(conn, name) {
  df <- DBI::dbGetQuery(conn,
    "SELECT 1 AS ok FROM information_schema.tables WHERE table_name = $1 LIMIT 1",
    params = list(name)
  )
  nrow(df) > 0
}

alertas_temprana_qc_where <- function() {
  if (exists("observacion_tiene_qc_flag", mode = "function") && observacion_tiene_qc_flag()) {
    " AND (o.qc_flag IS NULL OR o.qc_flag = 0)"
  } else {
    ""
  }
}

alertas_temprana_hist_key <- function(eid, var_id) paste0(eid, ":", var_id)

alertas_temprana_hist_load <- function(conn, eid, var_id, hist_cache, nombre_abr, nombre_var = NULL) {
  key <- alertas_temprana_hist_key(eid, var_id)
  if (!is.null(hist_cache[[key]])) return(hist_cache[[key]])
  vtype <- reporte_detect_var_type(nombre_var %||% nombre_abr %||% "")
  df <- NULL
  if (exists("observacion_diaria_fetch", mode = "function") &&
      exists("observacion_diaria_tabla_existe", mode = "function") &&
      observacion_diaria_tabla_existe(conn)) {
    df <- observacion_diaria_fetch(conn, eid, var_id)
  }
  if (is.null(df) || nrow(df) == 0) {
    abr <- tolower(trimws(nombre_abr %||% ""))
    agg <- if (identical(vtype, "precip")) {
      "SUM(o.valor)"
    } else if (abr %in% c("u_max_diario", "u_10_kt")) {
      "MAX(o.valor)"
    } else {
      "AVG(o.valor)"
    }
    q <- paste0(
      "SELECT o.fecha, ", agg, " AS valor FROM bitacora_oficial o",
      " WHERE o.id_estacion = $1 AND o.id_variable = $2 AND o.valor IS NOT NULL",
      alertas_temprana_qc_where(),
      " GROUP BY o.fecha ORDER BY o.fecha"
    )
    df <- DBI::dbGetQuery(conn, q, params = list(as.integer(eid), as.integer(var_id)))
  }
  if (nrow(df) == 0) { hist_cache[[key]] <- NULL; return(NULL) }
  min_anios <- if (exists("alerta_temprana_min_anios", mode = "function")) {
    alerta_temprana_min_anios()
  } else {
    20L
  }
  span_ok <- if (exists("alerta_par_historia_suficiente", mode = "function")) {
    alerta_par_historia_suficiente(df$fecha, min_anios)
  } else {
    FALSE
  }
  if (!isTRUE(span_ok)) { hist_cache[[key]] <- NULL; return(NULL) }
  hist <- suppressWarnings(as.numeric(df$valor))
  hist <- hist[!is.na(hist)]
  if (length(hist) == 0) { hist_cache[[key]] <- NULL; return(NULL) }
  entry <- list(hist = hist, n_days = length(hist), vtype = vtype, nombre_abr = nombre_abr)
  hist_cache[[key]] <- entry
  entry
}

alertas_temprana_rank_forecast <- function(conn, eid, vid, forecast_val, nombre_abr, nombre_var = NULL, direccion = NULL) {
  vtype <- if (exists("reporte_detect_var_type", mode = "function")) {
    reporte_detect_var_type(nombre_var %||% nombre_abr %||% "")
  } else {
    "generic"
  }
  dir <- direccion
  if (is.null(dir) || !nzchar(as.character(dir))) {
    specs <- if (exists("alertas_specs_por_abr", mode = "function")) {
      alertas_specs_por_abr(nombre_abr)
    } else {
      list()
    }
    dir <- if (length(specs) == 1L) specs[[1L]]$direccion else {
      if (tolower(nombre_abr %||% "") == "t_min") "min" else "max"
    }
  }
  spec <- reporte_alerta_temprana_widget(vtype, nombre_abr, direccion = dir)
  decreasing <- isTRUE(spec$decreasing)
  agg <- if (identical(vtype, "precip")) "SUM(o.valor)" else "AVG(o.valor)"
  cmp <- if (decreasing) ">=" else "<="
  min_anios <- if (exists("alerta_temprana_min_anios", mode = "function")) {
    alerta_temprana_min_anios()
  } else {
    20L
  }
  min_dias <- if (exists("alerta_temprana_min_dias", mode = "function")) {
    alerta_temprana_min_dias()
  } else {
    365L
  }
  q <- paste0(
    "WITH daily AS (",
    " SELECT o.fecha, ", agg, " AS v FROM bitacora_oficial o",
    " WHERE o.id_estacion = $1 AND o.id_variable = $2 AND o.valor IS NOT NULL",
    alertas_temprana_qc_where(),
    " GROUP BY o.fecha",
    ") SELECT COUNT(*)::int AS n_days,",
    " COUNT(DISTINCT EXTRACT(YEAR FROM fecha))::int AS anios_con_datos,",
    " 1 + COUNT(*) FILTER (WHERE v ", cmp, " $3)::int AS rank",
    " FROM daily WHERE v IS NOT NULL"
  )
  df <- DBI::dbGetQuery(conn, q, params = list(as.integer(eid), as.integer(vid), forecast_val))
  if (nrow(df) == 0 || is.na(df$n_days[1]) || df$n_days[1] == 0L) return(NULL)
  historia_ok <- if (exists("alerta_par_rank_historia_ok", mode = "function")) {
    alerta_par_rank_historia_ok(df$n_days[1], df$anios_con_datos[1], min_anios, min_dias)
  } else {
    !is.na(df$anios_con_datos[1]) && as.integer(df$anios_con_datos[1]) >= min_anios &&
      !is.na(df$n_days[1]) && df$n_days[1] >= min_dias
  }
  if (!isTRUE(historia_ok)) return(NULL)
  list(
    rank = as.integer(df$rank[1]),
    n_days = as.integer(df$n_days[1]),
    direccion = if (identical(spec$id, "maximos")) "max" else "min",
    vtype = vtype
  )
}

alertas_temprana_sync_legacy <- function(conn, dry_run = FALSE, limit = NULL) {
  if (!alertas_pronostico_tabla_existe(conn, "aviso_de_madrugada")) {
    alertas_log("[alertas:temprana] Tabla aviso_de_madrugada no existe; omitiendo sync")
    return(invisible(list(upserts = 0L, deletes = 0L)))
  }
  if (!alertas_pronostico_tabla_existe(conn, "carta_del_mirador")) {
    alertas_log("[alertas:temprana] Sin carta_del_mirador; omitiendo sync")
    return(invisible(list(upserts = 0L, deletes = 0L)))
  }

  min_anios_sql <- if (exists("alerta_temprana_min_anios", mode = "function")) {
    alerta_temprana_min_anios()
  } else {
    20L
  }
  rows <- DBI::dbGetQuery(conn, paste0(
    "SELECT p.id_estacion, p.fecha_objetivo, p.id_variable, p.valor,",
    " v.nombre_abr, v.nombre, COALESCE(v.unidad, '') AS unidad,",
    " e.nombre_publico AS estacion_nombre",
    " FROM carta_del_mirador p",
    " JOIN mirador e ON e.id = p.id_estacion",
    " JOIN magnitud v ON v.id = p.id_variable",
    " LEFT JOIN estacion_calidad ec ON ec.id_estacion = e.id",
    " WHERE p.fecha_objetivo >= CURRENT_DATE",
    " AND p.fecha_objetivo <= CURRENT_DATE + 2",
    " AND COALESCE(ec.nivel, 'limitada') = 'excelente'",
    " AND COALESCE(ec.anios_span, 0) >= ", as.integer(min_anios_sql),
    " AND v.nombre_abr = ANY($1::text[])",
    " ORDER BY p.id_estacion, p.fecha_objetivo, v.nombre_abr"
  ),
    params = list(alertas_pronostico_pg_text_array(alertas_pronostico_var_abrs()))
  )
  if (nrow(rows) == 0) {
    if (!isTRUE(dry_run)) {
      DBI::dbExecute(conn, "DELETE FROM aviso_de_madrugada WHERE fecha_objetivo < CURRENT_DATE")
    }
    alertas_log("[alertas:temprana] Sin filas de pronostico elegibles")
    return(invisible(list(upserts = 0L, deletes = 0L)))
  }

  n_total <- nrow(rows)
  n_series <- length(unique(paste(rows$id_estacion, rows$id_variable, sep = ":")))
  if (!is.null(limit) && !is.na(limit) && limit > 0L && limit < n_total) {
    rows <- rows[seq_len(as.integer(limit)), , drop = FALSE]
    n_series <- length(unique(paste(rows$id_estacion, rows$id_variable, sep = ":")))
    alertas_log("[alertas:temprana] Limitado a ", nrow(rows), " de ", n_total,
                " pronosticos (", n_series, " series)")
  } else {
    alertas_log("[alertas:temprana] Evaluando ", n_total, " pronosticos (", n_series,
                " series; puede tardar varios minutos)")
  }

  corrida_id <- DBI::dbGetQuery(conn, "SELECT gen_random_uuid()::text AS id")$id[1]

  hist_cache <- list()
  upserts <- 0L
  deletes <- 0L
  n_rows <- nrow(rows)
  t0 <- Sys.time()
  

  for (i in seq_len(n_rows)) {
    row <- rows[i, ]
    eid <- as.integer(row$id_estacion)
    vid <- as.integer(row$id_variable)
    forecast_val <- as.numeric(row$valor)
    fecha_obj <- row$fecha_objetivo
    

    if (i == 1L || i %% 100L == 0L || i == n_rows) {
      elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
      alertas_log("[alertas:temprana] progreso ", i, "/", n_rows,
                  " series=", length(hist_cache), " elapsed=", elapsed, "s")
    }

    loaded <- alertas_temprana_hist_load(conn, eid, vid, hist_cache, row$nombre_abr, row$nombre)
    cat_specs <- if (exists("alertas_specs_por_abr", mode = "function")) {
      alertas_specs_por_abr(row$nombre_abr)
    } else {
      list(list(direccion = if (tolower(row$nombre_abr) == "t_min") "min" else "max"))
    }
    if (length(cat_specs) == 0L) {
      if (!isTRUE(dry_run)) {
        n <- DBI::dbExecute(conn,
          "DELETE FROM aviso_de_madrugada WHERE id_estacion = $1 AND id_variable = $2 AND fecha_objetivo = $3::date",
          params = list(eid, vid, as.character(fecha_obj))
        )
        deletes <- deletes + as.integer(n)
      }
      next
    }

    for (cs in cat_specs) {
      direccion <- cs$direccion
      widget_spec <- if (exists("alertas_widget_spec", mode = "function")) {
        alertas_widget_spec(row$nombre_abr, direccion, vtype = loaded$vtype %||% NULL, nombre_var = row$nombre)
      } else {
        reporte_alerta_temprana_widget(loaded$vtype %||% "generic", row$nombre_abr, direccion = direccion)
      }
      rank <- NA_integer_
      n_days <- 0L
      confianza <- "alta"
      if (!is.null(loaded) && !is.null(widget_spec)) {
        rank <- reporte_rank_forecast_position(loaded$hist, forecast_val, decreasing = widget_spec$decreasing)
        n_days <- loaded$n_days
        confianza <- if (n_days < 365L) "baja" else "alta"
      }

      min_dias <- if (exists("alerta_temprana_min_dias", mode = "function")) {
        alerta_temprana_min_dias()
      } else {
        365L
      }
      if (!is.na(rank) && rank <= 10L && n_days >= min_dias) {
        if (!isTRUE(dry_run)) {
          DBI::dbExecute(conn,
            "INSERT INTO aviso_de_madrugada
             (id_estacion, id_variable, fecha_objetivo, direccion, ranking,
              valor_pronostico, n_dias_historico, confianza, generado_en, corrida_id)
             VALUES ($1, $2, $3::date, $4, $5, $6, $7, $8, NOW(), $9::uuid)
             ON CONFLICT (id_estacion, id_variable, fecha_objetivo, direccion)
             DO UPDATE SET ranking = EXCLUDED.ranking,
                           valor_pronostico = EXCLUDED.valor_pronostico,
                           n_dias_historico = EXCLUDED.n_dias_historico,
                           confianza = EXCLUDED.confianza,
                           generado_en = NOW(),
                           corrida_id = EXCLUDED.corrida_id",
            params = list(
              eid, vid, as.character(fecha_obj), direccion, rank,
              forecast_val, n_days, confianza, corrida_id
            )
          )
        }
        upserts <- upserts + 1L
        if (isTRUE(dry_run)) {
          alertas_log("[alertas:temprana:dry] upsert eid=", eid, " vid=", vid,
                      " ", direccion, " rank=", rank, " fecha=", fecha_obj)
        }
      } else if (!isTRUE(dry_run)) {
        n <- DBI::dbExecute(conn,
          "DELETE FROM aviso_de_madrugada
           WHERE id_estacion = $1 AND id_variable = $2
             AND fecha_objetivo = $3::date AND direccion = $4",
          params = list(eid, vid, as.character(fecha_obj), direccion)
        )
        deletes <- deletes + as.integer(n)
      }
    }
  }

  if (!isTRUE(dry_run)) {
    pruned <- DBI::dbExecute(conn, "DELETE FROM aviso_de_madrugada WHERE fecha_objetivo < CURRENT_DATE")
    deletes <- deletes + as.integer(pruned)
    n_obsolete <- DBI::dbExecute(conn,
      "DELETE FROM aviso_de_madrugada a
       USING variable v
       WHERE a.id_variable = v.id
         AND lower(v.nombre_abr) IN ('t_mean', 'u2_mean', 'u_10_kt')"
    )
    deletes <- deletes + as.integer(n_obsolete)
  }

  alertas_log("[alertas:temprana] sync upserts=", upserts, " deletes=", deletes)
  invisible(list(upserts = upserts, deletes = deletes, corrida_id = corrida_id))
}

alertas_pronostico_usuario_eids <- function(conn, user_id) {
  eids <- integer(0)
  tier_df <- DBI::dbGetQuery(conn, "SELECT tier FROM tripulacion WHERE id = $1::uuid", params = list(user_id))
  tier <- if (nrow(tier_df) > 0) tier_df$tier[1] else "gratuito"
  allowed <- alertas_scopes_for_tier(tier)
  if (alertas_pronostico_tabla_existe(conn, "norma_propia")) {
    reglas <- DBI::dbGetQuery(conn,
      "SELECT scope_type, scope_value FROM norma_propia
       WHERE user_id = $1::uuid AND activa = TRUE",
      params = list(user_id)
    )
    if (nrow(reglas) > 0) {
      for (i in seq_len(nrow(reglas))) {
        r <- reglas[i, ]
        if (!r$scope_type %in% allowed) next
        sv <- tryCatch(jsonlite::fromJSON(r$scope_value %||% "{}", simplifyVector = FALSE), error = function(e) list())
        got <- alertas_resolver_scope_eids(conn, user_id, r$scope_type, sv)
        if (length(got) > 0) eids <- c(eids, got)
      }
      return(unique(eids[!is.na(eids) & eids > 0L]))
    }
  }
  if (alertas_pronostico_tabla_existe(conn, "pedido_de_aviso")) {
    subs <- DBI::dbGetQuery(conn,
      "SELECT scope_type FROM pedido_de_aviso WHERE user_id = $1::uuid AND activa = TRUE",
      params = list(user_id)
    )
    for (i in seq_len(nrow(subs))) {
      if (!subs$scope_type[i] %in% allowed) next
      got <- alertas_resolver_scope_eids(conn, user_id, subs$scope_type[i], list())
      if (length(got) > 0) eids <- c(eids, got)
    }
  }
  unique(eids[!is.na(eids) & eids > 0L])
}

alertas_pronostico_evento_ya_enviado <- function(conn, user_id, eid, variable_id, fecha_obj, direccion) {
  df <- DBI::dbGetQuery(conn,
    "SELECT 1 AS ok FROM campanada_temprana
     WHERE user_id = $1::uuid AND eid = $2 AND variable_id = $3
       AND fecha_objetivo = $4::date AND direccion = $5 LIMIT 1",
    params = list(
      alertas_sql_text_param(user_id),
      as.integer(alertas_sql_scalar(eid)),
      as.integer(alertas_sql_scalar(variable_id)),
      alertas_sql_date_param(fecha_obj),
      alertas_sql_text_param(direccion)
    )
  )
  nrow(df) > 0
}

alertas_pronostico_registrar_evento <- function(conn, user_id, ev) {
  DBI::dbExecute(conn,
    "INSERT INTO campanada_temprana
     (user_id, eid, variable_id, fecha_objetivo, direccion, ranking, valor_pronostico, exito)
     VALUES ($1::uuid, $2, $3, $4::date, $5, $6, $7, TRUE)
     ON CONFLICT (user_id, eid, variable_id, fecha_objetivo, direccion) DO NOTHING",
    params = list(
      user_id, ev$eid, ev$variable_id, as.character(ev$fecha_objetivo),
      ev$direccion, ev$ranking, ev$valor_pronostico
    )
  )
}

alertas_pronostico_enviar_email <- function(to, ev, app_url) {
  fecha_larga <- reporte_fmt_fecha_larga(ev$fecha_objetivo)
  payload <- build_alerta_pronostico_email(
    ev$estacion_nombre, ev$variable_nombre, fecha_larga,
    ev$valor_pronostico, ev$unidad, ev$ranking, ev$n_dias,
    ev$direccion_label, ev$eid, ev$variable_id, app_url
  )
  smtp <- smtp_client()
  if (is.null(smtp)) return(list(ok = FALSE, error = "SMTP no configurado"))
  msg <- emayili::envelope(
    to = to,
    from = Sys.getenv("SMTP_FROM", "admin@rodaja.cl"),
    subject = payload$subject,
    text = payload$text,
    html = payload$html
  )
  tryCatch({
    smtp(msg)
    list(ok = TRUE)
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
}

evaluar_alertas_pronostico <- function(project_root, dry_run = FALSE, limit = NULL) {
  alertas_pronostico_bootstrap(project_root)
  on.exit(alertas_pronostico_db_close(), add = TRUE)
  conn <- get(".ALERTAS_PRON_DB", envir = .GlobalEnv)

  app_url <- Sys.getenv("APP_PUBLIC_URL", Sys.getenv("APP_URL", "https://app.rodaja.cl"))
  fecha_eval <- Sys.Date()
  queued <- 0L
  sync <- list(upserts = 0L)

  sp_sync <- NULL
  if (exists("profile_span_begin", mode = "function")) {
    sp_sync <- tryCatch(
      profile_span_begin("phase.sync_temprana", "phase", attrs = list(step = "alertas_pronostico")),
      error = function(e) NULL
    )
  }
  if (alertas_pronostico_tabla_existe(conn, "aviso_de_madrugada")) {
    sync <- alertas_temprana_sync_global(conn, dry_run = dry_run, limit = limit)
  } else {
    alertas_log("[alertas:pronostico] Sin catalogo aviso_de_madrugada; solo digest tardias pendientes")
  }
  if (exists("profile_span_end", mode = "function") && !is.null(sp_sync)) {
    tryCatch(profile_span_end(sp_sync, list(upserts = sync$upserts %||% 0L)), error = function(e) NULL)
  }

  if (alertas_pronostico_tabla_existe(conn, "pedido_temprano")) {
    subs <- DBI::dbGetQuery(conn,
      "SELECT s.user_id, u.email
       FROM pedido_temprano s
       JOIN tripulacion u ON u.id = s.user_id
       WHERE s.activa = TRUE AND u.activo = TRUE"
    )
  } else {
    subs <- data.frame(user_id = character(0), email = character(0))
    alertas_log("[alertas:pronostico] Tabla suscripcion no existe; solo digest tardias pendientes")
  }

  if (nrow(subs) == 0) {
    alertas_log("[alertas:pronostico] Sin abono tempranas activas")
  } else if (!alertas_pronostico_tabla_existe(conn, "aviso_de_madrugada")) {
    alertas_log("[alertas:pronostico] Sin catalogo para evaluar tempranas")
  }

  sp_fan <- NULL
  if (exists("profile_span_begin", mode = "function")) {
    sp_fan <- tryCatch(
      profile_span_begin("phase.fanout_subs", "phase",
                         attrs = list(step = "alertas_pronostico", n_subs = nrow(subs))),
      error = function(e) NULL
    )
  }
  for (i in seq_len(nrow(subs))) {
    if (!alertas_pronostico_tabla_existe(conn, "aviso_de_madrugada")) break
    row <- subs[i, ]
    uid <- row$user_id
    eids <- alertas_pronostico_usuario_eids(conn, uid)
    if (length(eids) == 0) next

    prefs <- alertas_cargar_preferencias(conn, uid)
    if (isTRUE(prefs$alerta_max) == FALSE && isTRUE(prefs$alerta_min) == FALSE) next

    hist_clause <- if (exists("alerta_temprana_historia_clause", mode = "function")) {
      alerta_temprana_historia_clause("a.id_estacion")
    } else {
      "TRUE"
    }
    val_clause <- if (exists("alerta_temprana_alerta_valida_sql", mode = "function")) {
      alerta_temprana_alerta_valida_sql("a")
    } else {
      "TRUE"
    }
    # Correo: solo tempranas en horizonte emit (hoy .. hoy+1) y ranking <= 3.
    horiz <- if (exists("alertas_temprana_horizonte_dias", mode = "function")) {
      alertas_temprana_horizonte_dias()
    } else {
      2L
    }
    rank_max <- if (exists("alerta_digest_rank_max", mode = "function")) {
      alerta_digest_rank_max()
    } else {
      3L
    }
    alertas_df <- DBI::dbGetQuery(conn, paste0(
      "SELECT a.id_estacion, a.id_variable, a.fecha_objetivo, a.direccion,",
      " a.ranking, a.valor_pronostico, a.n_dias_historico,",
      " e.nombre_publico AS estacion_nombre, v.nombre AS variable_nombre,",
      " COALESCE(v.nombre_abr, '') AS nombre_abr, COALESCE(v.unidad, '') AS unidad",
      " FROM aviso_de_madrugada a",
      " JOIN mirador e ON e.id = a.id_estacion",
      " JOIN magnitud v ON v.id = a.id_variable",
      " WHERE a.id_estacion = ANY($1::int[])",
      " AND a.fecha_objetivo >= CURRENT_DATE",
      " AND a.fecha_objetivo < CURRENT_DATE + $2::int",
      " AND a.ranking <= $3::int",
      " AND ", hist_clause,
      " AND ", val_clause,
      " ORDER BY a.ranking ASC, a.fecha_objetivo ASC"
    ),
      params = list(paste0("{", paste(eids, collapse = ","), "}"), as.integer(horiz), as.integer(rank_max))
    )
    if (nrow(alertas_df) == 0) next

    for (j in seq_len(nrow(alertas_df))) {
      a <- alertas_df[j, , drop = FALSE]
      eid <- as.integer(a$id_estacion[[1L]])
      vid <- as.integer(a$id_variable[[1L]])
      fecha_obj <- a$fecha_objetivo[[1L]]
      direccion <- a$direccion[[1L]]
      ranking <- as.integer(a$ranking[[1L]])
      if (!is.null(prefs$variable_ids) && length(prefs$variable_ids) > 0) {
        if (!(vid %in% as.integer(prefs$variable_ids))) next
      }
      if (identical(direccion, "max") && !isTRUE(prefs$alerta_max)) next
      if (identical(direccion, "min") && !isTRUE(prefs$alerta_min)) next
      if (alertas_pronostico_evento_ya_enviado(conn, uid, eid, vid, fecha_obj, direccion)) next

      val_canon <- as.numeric(a$valor_pronostico[[1L]])
      if (isTRUE(dry_run)) {
        alertas_log("[alertas:pronostico:dry] user=", uid, " eid=", eid, " rank=", ranking)
        queued <- queued + 1L
        next
      }
      alertas_registrar_evento_temprana_pendiente(
        conn, uid, eid, vid, fecha_obj, direccion, ranking, val_canon
      )
      queued <- queued + 1L
    }
  }
  if (exists("profile_span_end", mode = "function") && !is.null(sp_fan)) {
    tryCatch(profile_span_end(sp_fan, list(queued = queued, n_subs = nrow(subs))),
             error = function(e) NULL)
  }

  sp_dig <- NULL
  if (exists("profile_span_begin", mode = "function")) {
    sp_dig <- tryCatch(
      profile_span_begin("phase.digest", "phase", attrs = list(step = "alertas_pronostico")),
      error = function(e) NULL
    )
  }
  digests <- alertas_enviar_digests(conn, fecha_eval, app_url, dry_run = dry_run)
  briefings <- 0L
  if (exists("alertas_enviar_briefings", mode = "function")) {
    briefings <- tryCatch(
      alertas_enviar_briefings(conn, fecha_eval, dry_run = dry_run),
      error = function(e) {
        alertas_log("[alertas:push] briefing ERROR: ", conditionMessage(e))
        0L
      }
    )
  }
  if (exists("profile_span_end", mode = "function") && !is.null(sp_dig)) {
    tryCatch(profile_span_end(sp_dig, list(digests = digests, briefings = briefings)), error = function(e) NULL)
  }
  alertas_log("[alertas:pronostico] Encoladas tempranas: ", queued,
              " digests=", digests, " briefings=", briefings,
              " (sync upserts=", sync$upserts %||% 0L, ")")
  invisible(list(queued = queued, digests = digests, briefings = briefings))
}
