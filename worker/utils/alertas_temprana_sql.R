# Sync batch aviso_de_madrugada (SQL set-based sobre jornada_cerrada).

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

alertas_temprana_obs_diaria_est_rows <- function(conn) {
  row <- DBI::dbGetQuery(conn,
    "SELECT COALESCE(c.reltuples::bigint, 0) AS n
     FROM pg_class c
     JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE c.relname = 'jornada_cerrada' AND n.nspname = 'public'"
  )
  if (nrow(row) == 0L) return(0L)
  as.integer(row$n[1])
}

alertas_temprana_pron_cte_sql <- function(limit_sql = "", min_anios_sql = 20L,
                                          shadow_lead_max = 2L) {
  shadow_lead_max <- as.integer(shadow_lead_max %||% 2L)
  paste0(
    "WITH catalog AS (",
    " SELECT * FROM (VALUES",
    "   ('pp', 'max'),",
    "   ('t_max', 'max'),",
    "   ('t_max', 'min'),",
    "   ('t_min', 'min'),",
    "   ('t_min', 'max')",
    " ) AS c(abr, direccion)",
    "), pron AS (",
    " SELECT p.id_estacion, p.id_variable, p.fecha_objetivo, p.valor, lower(v.nombre_abr) AS abr",
    " FROM carta_del_mirador p",
    " JOIN magnitud v ON v.id = p.id_variable",
    " JOIN mirador e ON e.id = p.id_estacion",
    " JOIN estacion_calidad ec ON ec.id_estacion = e.id",
    " WHERE p.fecha_objetivo >= CURRENT_DATE",
    " AND p.fecha_objetivo <= CURRENT_DATE + ", shadow_lead_max,
    " AND ec.nivel = 'excelente'",
    " AND ec.anios_span >= ", as.integer(min_anios_sql),
    " AND v.nombre_abr = ANY($1::text[])",
    " ORDER BY p.id_estacion, p.fecha_objetivo, v.nombre_abr",
    limit_sql,
    "), pron_dir AS (",
    " SELECT pr.id_estacion, pr.id_variable, pr.fecha_objetivo, pr.valor, c.direccion",
    " FROM pron pr",
    " INNER JOIN catalog c ON c.abr = pr.abr",
    ") "
  )
}

alertas_temprana_materialize_pron_dir <- function(conn, var_arr, limit_sql = "", min_anios_sql = 20L) {
  shadow_max <- if (exists("alerta_skill_shadow_lead_max", mode = "function")) {
    alerta_skill_shadow_lead_max()
  } else {
    2L
  }
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS alertas_pron_dir")
  t_mat <- Sys.time()
  DBI::dbExecute(conn, paste0(
    "CREATE TEMP TABLE alertas_pron_dir AS ",
    alertas_temprana_pron_cte_sql(limit_sql, min_anios_sql, shadow_lead_max = shadow_max),
    " SELECT id_estacion, id_variable, fecha_objetivo, valor, direccion FROM pron_dir"
  ), params = list(var_arr))
  DBI::dbExecute(conn,
    "CREATE INDEX alertas_pron_dir_eid_vid ON alertas_pron_dir (id_estacion, id_variable)"
  )
  elapsed <- round(as.numeric(difftime(Sys.time(), t_mat, units = "secs")), 1)
  alertas_log("[alertas:temprana] materialized pron_dir elapsed=", elapsed, "s shadow_lead_max=", shadow_max)
}

# ponytail: el catalogo es top-10 (CHECK ranking BETWEEN 1 AND 10), asi que basta
# con los 10 valores mas extremos por serie/direccion. Comparar el pronostico
# contra esos 10 da el ranking exacto cuando el resultado es <= 10. Un near-miss
# (margen negativo) puede dar 11: el INSERT lo topa a 10, no ensancha el CHECK.
# Indices idx_obs_diaria_est_var_valor_{desc,asc} => ~10 filas/serie, no las ~42M.
# Estricto (> / <), igual que alertas_tardia: >= cuenta el empate con el 10o
# extremo como 11 y aviso_de_madrugada_ranking_check aborta el INSERT entero.
alertas_temprana_ranking_expr_sql <- function() {
  paste0(
    "(1 + COUNT(*) FILTER (",
    "WHERE (p.direccion = 'max' AND u.valor > p.valor)",
    " OR (p.direccion = 'min' AND u.valor < p.valor)))::int"
  )
}

# Tope del CHECK. Near-miss (ranking 11 con solo 10 umbrales) se guarda como 10.
alertas_temprana_ranking_stored_sql <- function(ranking_sql = "ranking") {
  paste0("LEAST(GREATEST(", ranking_sql, ", 1), 10)")
}
alertas_temprana_umbrales_select_sql <- function() {
  paste0(
    "SELECT s.id_estacion, s.id_variable, 'max'::text AS direccion, u.valor",
    " FROM (SELECT DISTINCT id_estacion, id_variable FROM alertas_pron_dir",
    "       WHERE direccion = 'max') s",
    " CROSS JOIN LATERAL (",
    "   SELECT d.valor FROM jornada_cerrada d",
    "   WHERE d.id_estacion = s.id_estacion AND d.id_variable = s.id_variable",
    "   ORDER BY d.valor DESC LIMIT 10",
    " ) u",
    " UNION ALL ",
    "SELECT s.id_estacion, s.id_variable, 'min'::text AS direccion, u.valor",
    " FROM (SELECT DISTINCT id_estacion, id_variable FROM alertas_pron_dir",
    "       WHERE direccion = 'min') s",
    " CROSS JOIN LATERAL (",
    "   SELECT d.valor FROM jornada_cerrada d",
    "   WHERE d.id_estacion = s.id_estacion AND d.id_variable = s.id_variable",
    "   ORDER BY d.valor ASC LIMIT 10",
    " ) u"
  )
}

alertas_temprana_sync_sql <- function(conn, dry_run = FALSE, limit = NULL) {
  if (!exists("observacion_diaria_tabla_existe", mode = "function")) {
    od <- file.path(Sys.getenv("API_ROOT", "/api"), "utils", "observacion_diaria.R")
    if (!file.exists(od)) od <- file.path(dirname(getwd()), "worker", "utils", "observacion_diaria.R")
    worker_od <- "/app/utils/observacion_diaria.R"
    if (file.exists(worker_od)) source(worker_od, local = FALSE)
    else if (file.exists(od)) source(od, local = FALSE)
  }
  if (!exists("observacion_diaria_tabla_existe", mode = "function") ||
      !observacion_diaria_tabla_existe(conn)) {
    alertas_log("[alertas:temprana] jornada_cerrada no disponible; sync legacy")
    return(alertas_temprana_sync_legacy(conn, dry_run = dry_run, limit = limit))
  }
  has_rows <- DBI::dbGetQuery(conn, "SELECT EXISTS (SELECT 1 FROM jornada_cerrada LIMIT 1) AS ok")$ok[1]
  if (!isTRUE(has_rows)) {
    alertas_log("[alertas:temprana] jornada_cerrada vacia; sync legacy")
    return(alertas_temprana_sync_legacy(conn, dry_run = dry_run, limit = limit))
  }

  t0 <- Sys.time()
  n_diaria <- alertas_temprana_obs_diaria_est_rows(conn)
  corrida_id <- DBI::dbGetQuery(conn, "SELECT gen_random_uuid()::text AS id")$id[1]
  var_abrs <- if (exists("alertas_pronostico_var_abrs", mode = "function")) {
    alertas_pronostico_var_abrs()
  } else {
    c("pp", "t_max", "t_min")
  }
  var_arr <- alertas_pronostico_pg_text_array(var_abrs)
  min_anios_sql <- if (exists("alerta_temprana_min_anios", mode = "function")) {
    alerta_temprana_min_anios()
  } else {
    20L
  }
  min_dias_sql <- if (exists("alerta_temprana_min_dias", mode = "function")) {
    alerta_temprana_min_dias()
  } else {
    365L
  }
  limit_sql <- if (!is.null(limit) && !is.na(limit) && limit > 0L) {
    paste0(" LIMIT ", as.integer(limit))
  } else ""

  alertas_temprana_materialize_pron_dir(conn, var_arr, limit_sql, min_anios_sql)
  n_pron_dir <- DBI::dbGetQuery(conn, "SELECT COUNT(*)::int AS n FROM alertas_pron_dir")$n[1]
  n_series <- DBI::dbGetQuery(conn,
    "SELECT COUNT(DISTINCT (id_estacion, id_variable))::int AS n FROM alertas_pron_dir"
  )$n[1]
  alertas_log("[alertas:temprana] pron_dir=", n_pron_dir, " series=", n_series,
              " jornada_cerrada~", n_diaria, " filas (est)")

  # work_mem acotado: el ranking ya no expande la historia (usa umbrales top-10);
  # el unico agregado grande es stats por serie (hash de ~n_series grupos).
  DBI::dbExecute(conn, "SET work_mem TO '64MB'")

  # Umbrales: 10 valores mas extremos por serie/direccion (index scan, ~10 filas/serie).
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS alertas_umbrales")
  DBI::dbExecute(conn, paste0(
    "CREATE TEMP TABLE alertas_umbrales AS ",
    alertas_temprana_umbrales_select_sql()
  ))
  DBI::dbExecute(conn,
    "CREATE INDEX alertas_umbrales_key ON alertas_umbrales (id_estacion, id_variable, direccion)"
  )

  # Stats por serie en un solo scan agrupado. Solo referencia (id_estacion,
  # id_variable, fecha) => index-only scan sobre la PK. valor es NOT NULL, asi
  # que COUNT(*) == COUNT(valor). Filtra elegibilidad (min dias/anios) aqui.
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS alertas_serie_stats")
  DBI::dbExecute(conn, paste0(
    "CREATE TEMP TABLE alertas_serie_stats AS ",
    "SELECT d.id_estacion, d.id_variable, COUNT(*)::int AS n_dias,",
    " COUNT(DISTINCT EXTRACT(YEAR FROM d.fecha))::int AS n_anios",
    " FROM jornada_cerrada d",
    " JOIN (SELECT DISTINCT id_estacion, id_variable FROM alertas_pron_dir) s",
    "   ON s.id_estacion = d.id_estacion AND s.id_variable = d.id_variable",
    " GROUP BY d.id_estacion, d.id_variable",
    " HAVING COUNT(*) >= ", min_dias_sql,
    " AND COUNT(DISTINCT EXTRACT(YEAR FROM d.fecha)) >= ", min_anios_sql
  ))
  DBI::dbExecute(conn,
    "CREATE INDEX alertas_serie_stats_key ON alertas_serie_stats (id_estacion, id_variable)"
  )

  # Ranking set-based + near-miss (margen negativo): candidatos con valor
  # >= umbral_10 + margen (margen puede ser < 0). Ranking > 10 entra al temp
  # y al persistir se topa a 10 (CHECK 1..10). Capa sigue el lead.
  has_margen_tbl <- tryCatch(
    nrow(DBI::dbGetQuery(conn,
      "SELECT 1 FROM information_schema.tables WHERE table_name='margen_del_tope' LIMIT 1")) > 0,
    error = function(e) FALSE
  )
  margen_enabled <- FALSE
  if (isTRUE(has_margen_tbl)) {
    margen_enabled <- tryCatch({
      st <- DBI::dbGetQuery(conn,
        "SELECT enabled FROM punteria_en_ronda WHERE id = 1")
      nrow(st) > 0 && isTRUE(as.logical(st$enabled[1]))
    }, error = function(e) FALSE)
  }

  margen_join <- if (isTRUE(margen_enabled)) {
    paste0(
      " LEFT JOIN margen_del_tope mg ",
      "   ON mg.id_estacion = p.id_estacion AND mg.id_variable = p.id_variable ",
      "  AND mg.direccion = p.direccion AND mg.activo = TRUE "
    )
  } else {
    ""
  }
  # Entrada al catalogo: rank<=10 OR near-miss (margen < 0 y cruza umbral+margen).
  # Filtro de emit: margen finito y valor supera umbral_10 + margen (negativo incluido).
  # margen >= 1e8 => serie suprimida.
  entry_where <- if (isTRUE(margen_enabled)) {
    paste0(
      " WHERE (",
      "   rk.ranking <= 10",
      "   OR (",
      "     mg.margen IS NOT NULL AND mg.margen < 0 AND mg.margen > -1e8 AND (",
      "       (p.direccion = 'max' AND p.valor >= (",
      "         (SELECT MIN(u2.valor) FROM alertas_umbrales u2 ",
      "          WHERE u2.id_estacion = p.id_estacion AND u2.id_variable = p.id_variable ",
      "            AND u2.direccion = p.direccion) + mg.margen))",
      "       OR (p.direccion = 'min' AND p.valor <= (",
      "         (SELECT MAX(u2.valor) FROM alertas_umbrales u2 ",
      "          WHERE u2.id_estacion = p.id_estacion AND u2.id_variable = p.id_variable ",
      "            AND u2.direccion = p.direccion) - mg.margen))",
      "     )",
      "   )",
      " ) AND (",
      "   mg.margen IS NULL OR (",
      "     mg.margen < 1e8 AND (",
      "       (p.direccion = 'max' AND p.valor >= (",
      "         (SELECT MIN(u2.valor) FROM alertas_umbrales u2 ",
      "          WHERE u2.id_estacion = p.id_estacion AND u2.id_variable = p.id_variable ",
      "            AND u2.direccion = p.direccion) + COALESCE(mg.margen,0))) ",
      "       OR (p.direccion = 'min' AND p.valor <= (",
      "         (SELECT MAX(u2.valor) FROM alertas_umbrales u2 ",
      "          WHERE u2.id_estacion = p.id_estacion AND u2.id_variable = p.id_variable ",
      "            AND u2.direccion = p.direccion) - COALESCE(mg.margen,0)))",
      "     )",
      "   )",
      " )"
    )
  } else {
    " WHERE rk.ranking <= 10"
  }

  DBI::dbExecute(conn, "DROP TABLE IF EXISTS alertas_temprana_nuevas")
  DBI::dbExecute(conn, paste0(
    "CREATE TEMP TABLE alertas_temprana_nuevas AS ",
    "SELECT p.id_estacion, p.id_variable, p.fecha_objetivo, p.direccion,",
    " rk.ranking, p.valor AS valor_pronostico, st.n_dias AS n_dias_historico",
    " FROM alertas_pron_dir p",
    " JOIN alertas_serie_stats st",
    "   ON st.id_estacion = p.id_estacion AND st.id_variable = p.id_variable",
    margen_join,
    " CROSS JOIN LATERAL (",
    "   SELECT ", alertas_temprana_ranking_expr_sql(), " AS ranking",
    "   FROM alertas_umbrales u",
    "   WHERE u.id_estacion = p.id_estacion AND u.id_variable = p.id_variable",
    "     AND u.direccion = p.direccion",
    " ) rk",
    entry_where
  ))
  if (isTRUE(margen_enabled)) {
    alertas_log("[alertas:temprana] filtro margen v2 (negativos/near-miss) activo")
  }
  n_series_stats <- DBI::dbGetQuery(conn,
    "SELECT COUNT(*)::int AS n FROM alertas_serie_stats")$n[1]

  if (isTRUE(dry_run)) {
    upserts <- DBI::dbGetQuery(conn,
      "SELECT COUNT(*)::int AS n FROM alertas_temprana_nuevas")$n[1]
    sample <- DBI::dbGetQuery(conn,
      "SELECT id_estacion, id_variable, fecha_objetivo, direccion, ranking
       FROM alertas_temprana_nuevas ORDER BY ranking, fecha_objetivo LIMIT 20")
    for (j in seq_len(nrow(sample))) {
      s <- sample[j, ]
      alertas_log("[alertas:temprana:dry] upsert eid=", s$id_estacion, " vid=", s$id_variable,
                  " ", s$direccion, " rank=", s$ranking, " fecha=", s$fecha_objetivo)
    }
    deletes <- 0L
    rank_fuera <- 0L
  } else {
    rank_fuera <- DBI::dbGetQuery(conn,
      "SELECT COUNT(*)::int AS n FROM alertas_temprana_nuevas WHERE ranking < 1 OR ranking > 10"
    )$n[1]
    if (is.na(rank_fuera)) rank_fuera <- 0L
    if (rank_fuera > 0L) {
      alertas_log("[alertas:temprana] ranking fuera de 1..10: ", rank_fuera,
                  " filas; se guardan como ", alertas_temprana_ranking_stored_sql(),
                  " (CHECK aviso_de_madrugada_ranking_check)")
    }
    emit_max <- if (exists("alerta_skill_emit_lead_max", mode = "function")) {
      alerta_skill_emit_lead_max()
    } else {
      1L
    }
    # DELETE + INSERT en una transaccion: si el CHECK vuelve a fallar, el
    # catalogo de hoy no queda vacio (2026-10-20 el DELETE ya habia hecho commit).
    wrote <- DBI::dbWithTransaction(conn, {
      DBI::dbExecute(conn, "DELETE FROM aviso_de_madrugada WHERE fecha_objetivo >= CURRENT_DATE")
      ups <- DBI::dbExecute(conn, paste0(
        "INSERT INTO aviso_de_madrugada",
        " (id_estacion, id_variable, fecha_objetivo, direccion, ranking,",
        "  valor_pronostico, n_dias_historico, confianza, generado_en, corrida_id, capa)",
        " SELECT id_estacion, id_variable, fecha_objetivo, direccion, ",
        alertas_temprana_ranking_stored_sql(), ",",
        " valor_pronostico, n_dias_historico,",
        " 'alta',",
        " NOW(), $1::uuid,",
        " CASE WHEN (fecha_objetivo - CURRENT_DATE) <= ", as.integer(emit_max),
        "      THEN 'emit' ELSE 'shadow' END",
        " FROM alertas_temprana_nuevas"
      ), params = list(corrida_id))
      del <- DBI::dbExecute(conn, "DELETE FROM aviso_de_madrugada WHERE fecha_objetivo < CURRENT_DATE")
      del <- del + DBI::dbExecute(conn,
        "DELETE FROM aviso_de_madrugada a
         USING variable v
         WHERE a.id_variable = v.id
           AND lower(v.nombre_abr) IN ('t_mean', 'u2_mean', 'u_10_kt')"
      )
      list(upserts = ups, deletes = del)
    })
    upserts <- wrote$upserts
    deletes <- wrote$deletes
  }
  alertas_log("[alertas:temprana] series_elegibles=", n_series_stats,
              " upserts=", upserts)

  DBI::dbExecute(conn, "DROP TABLE IF EXISTS alertas_umbrales")
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS alertas_serie_stats")
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS alertas_temprana_nuevas")
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS alertas_pron_dir")

  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  alertas_log("[alertas:temprana] sync SQL upserts=", upserts, " deletes=", deletes,
              " rank_fuera=", if (exists("rank_fuera")) rank_fuera else 0L,
              " elapsed=", elapsed, "s (jornada_cerrada~", n_diaria, " filas)")
  invisible(list(upserts = upserts, deletes = deletes, corrida_id = corrida_id,
                 pron_dir = n_pron_dir, series = n_series, elapsed = elapsed))
}

alertas_temprana_sync_global <- function(conn, dry_run = FALSE, limit = NULL) {
  alertas_temprana_sync_sql(conn, dry_run = dry_run, limit = limit)
}
