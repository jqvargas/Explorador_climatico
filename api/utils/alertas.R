# Helpers alertas top 10 (reglas, preferencias, digest)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

ALERTAS_SCOPE_TYPES <- c("favorita", "grupo", "estacion", "comuna", "region", "cuenca")
ALERTAS_SCOPE_LEGACY <- c("favorita", "grupo")
ALERTAS_SCOPE_GEO <- c("comuna", "region", "cuenca")

alertas_variable_abrs <- function() {
  if (!exists("alertas_catalogo_abrs", mode = "function")) {
    stop("alertas_catalog.R no cargado")
  }
  alertas_catalogo_abrs()
}

alertas_prueba_tabla_existe <- function() {
  chk <- tryCatch(
    db_query("SELECT 1 FROM information_schema.tables WHERE table_name = 'ensayo_del_dia' LIMIT 1"),
    error = function(e) NULL
  )
  !is.null(chk) && nrow(chk) > 0
}

alertas_variable_ids_allowed <- function() {
  df <- db_query(
    "SELECT id FROM magnitud WHERE nombre_abr = ANY($1::text[]) ORDER BY array_position($1::text[], nombre_abr)",
    params = list(pg_text_array_param(alertas_variable_abrs()))
  )
  if (nrow(df) == 0) return(integer(0))
  as.integer(df$id)
}

alertas_etiqueta_variable <- function(abr, nombre = "") {
  lab <- switch(tolower(trimws(as.character(abr %||% ""))),
    pp = "Precipitaci\u00f3n",
    t_max = "Temperatura m\u00e1xima",
    t_min = "Temperatura m\u00ednima",
    u_rafaga = "R\u00e1faga",
    NULL
  )
  if (!is.null(lab)) return(lab)
  as.character(nombre %||% "")
}

listar_variables_alertas <- function() {
  df <- db_query(
    "SELECT id, nombre, nombre_abr, unidad
     FROM magnitud
     WHERE nombre_abr = ANY($1::text[])
     ORDER BY array_position($1::text[], nombre_abr)",
    params = list(pg_text_array_param(alertas_variable_abrs()))
  )
  if (nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    abr <- row$nombre_abr %||% ""
    list(
      id = as.integer(row$id),
      nombre = alertas_etiqueta_variable(abr, row$nombre),
      nombre_abr = abr,
      unidad = row$unidad %||% ""
    )
  })
}

filtrar_variable_ids_alertas <- function(vids) {
  allowed <- alertas_variable_ids_allowed()
  if (length(allowed) == 0) return(integer(0))
  if (is.null(vids) || length(vids) == 0) return(allowed)
  intersect(as.integer(vids), allowed)
}

validar_preferencias_variables <- function(vids, tier) {
  allowed <- alertas_variable_ids_allowed()
  if (length(allowed) == 0) {
    return(list(ok = FALSE, error = "Variables de alerta no disponibles"))
  }
  lim <- get_limite_tier(tier %||% "gratuito")
  max_vars <- lim$max_alertas_variables
  if (!is.null(vids) && length(vids) > 0) {
    bad <- setdiff(as.integer(vids), allowed)
    if (length(bad) > 0) {
      return(list(ok = FALSE, error = "Solo se permiten precipitaci\u00f3n y temperatura max/min"))
    }
  }
  if (!is.null(max_vars) && max_vars == 1L) {
    if (is.null(vids) || length(vids) == 0) {
      return(list(ok = FALSE, error = "En plan gratuito debes elegir una variable"))
    }
    if (length(vids) > 1L) {
      return(list(ok = FALSE, error = "En plan gratuito solo puedes elegir una variable"))
    }
    return(list(ok = TRUE, variable_ids = as.integer(vids[1])))
  }
  if (is.null(vids) || length(vids) == 0) {
    return(list(ok = TRUE, variable_ids = NULL))
  }
  list(ok = TRUE, variable_ids = unique(as.integer(vids)))
}

prueba_alerta_enviada_hoy <- function(user_id) {
  if (!alertas_prueba_tabla_existe()) return(FALSE)
  df <- db_query(
    "SELECT 1 AS ok FROM ensayo_del_dia WHERE user_id = $1::uuid AND fecha = CURRENT_DATE LIMIT 1",
    params = list(user_id)
  )
  nrow(df) > 0
}

enviar_prueba_alerta_usuario <- function(user_id, cuenta_email) {
  if (prueba_alerta_enviada_hoy(user_id)) {
    return(list(ok = FALSE, error = "Ya enviaste una prueba hoy. Intenta ma\u00f1ana."))
  }
  email <- resolver_email_destino(user_id, cuenta_email)
  if (!nzchar(trimws(email %||% ""))) {
    return(list(ok = FALSE, error = "No hay correo configurado para alertas"))
  }
  if (grepl("@pending\\.rodaja\\.cl$", trimws(tolower(email)))) {
    return(list(ok = FALSE, error = "Configura un correo v\u00e1lido antes de probar"))
  }
  if (exists("correo_rebote_esta_bloqueado", mode = "function") &&
      isTRUE(correo_rebote_esta_bloqueado(email))) {
    err <- if (exists("correo_rebote_error_bloqueado", mode = "function")) correo_rebote_error_bloqueado() else "correo bloqueado por rebotes"
    return(list(ok = FALSE, error = err))
  }
  app_url <- Sys.getenv("APP_URL", "https://app.rodaja.cl")
  out <- enviar_alerta_prueba(email, app_url)
  if (!isTRUE(out$ok)) return(out)
  if (alertas_prueba_tabla_existe()) {
    db_execute(
      "INSERT INTO ensayo_del_dia (user_id, fecha, enviado_en)
       VALUES ($1::uuid, CURRENT_DATE, NOW())
       ON CONFLICT (user_id, fecha) DO NOTHING",
      params = list(user_id)
    )
  }
  list(ok = TRUE, email = email)
}

alertas_regla_tabla_existe <- function() {
  chk <- tryCatch(
    db_query("SELECT 1 FROM information_schema.tables WHERE table_name = 'norma_propia' LIMIT 1"),
    error = function(e) NULL
  )
  !is.null(chk) && nrow(chk) > 0
}

alertas_tabla_existe <- function() {
  alertas_regla_tabla_existe() || {
    chk <- tryCatch(
      db_query("SELECT 1 FROM information_schema.tables WHERE table_name = 'pedido_de_aviso' LIMIT 1"),
      error = function(e) NULL
    )
    !is.null(chk) && nrow(chk) > 0
  }
}

alertas_preferencias_tabla_existe <- function() {
  chk <- tryCatch(
    db_query("SELECT 1 FROM information_schema.tables WHERE table_name = 'gusto_de_aviso' LIMIT 1"),
    error = function(e) NULL
  )
  !is.null(chk) && nrow(chk) > 0
}

alertas_digest_tabla_existe <- function() {
  chk <- tryCatch(
    db_query("SELECT 1 FROM information_schema.tables WHERE table_name = 'sobre_del_resumen' LIMIT 1"),
    error = function(e) NULL
  )
  !is.null(chk) && nrow(chk) > 0
}

validar_email_alerta <- function(email) {
  email <- trimws(tolower(email %||% ""))
  if (!nzchar(email)) return(list(ok = FALSE, error = "Email vac\u00edo"))
  if (grepl("@pending\\.rodaja\\.cl$", email)) {
    return(list(ok = FALSE, error = "Email de cuenta pendiente no v\u00e1lido para alertas"))
  }
  if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", email)) {
    return(list(ok = FALSE, error = "Formato de email inv\u00e1lido"))
  }
  list(ok = TRUE, email = email)
}

resolver_email_destino <- function(user_id, cuenta_email) {
  cuenta_email <- trimws(tolower(cuenta_email %||% ""))
  if (!alertas_preferencias_tabla_existe()) return(cuenta_email)
  df <- db_query(
    "SELECT email_modo, email_alternativo FROM gusto_de_aviso WHERE user_id = $1::uuid",
    params = list(user_id)
  )
  if (nrow(df) == 0) return(cuenta_email)
  modo <- df$email_modo[1]
  if (identical(modo, "alternativo")) {
    alt <- trimws(tolower(df$email_alternativo[1] %||% ""))
    chk <- validar_email_alerta(alt)
    if (isTRUE(chk$ok)) return(chk$email)
  }
  cuenta_email
}

#' SQL: correo efectivo de alertas (cuenta o alternativo valido).
alertas_email_efectivo_sql <- function(alias_u = "u", alias_ap = "ap") {
  paste0(
    "lower(trim(CASE WHEN ", alias_ap, ".email_modo = 'alternativo'",
    " AND NULLIF(trim(", alias_ap, ".email_alternativo), '') IS NOT NULL",
    " THEN trim(", alias_ap, ".email_alternativo) ELSE ", alias_u, ".email END))"
  )
}

get_preferencias_usuario <- function(user_id, cuenta_email) {
  defaults <- list(
    email_modo = "cuenta",
    email_alternativo = NULL,
    email_efectivo = cuenta_email %||% "",
    variable_ids = NULL,
    alerta_max = TRUE,
    alerta_min = TRUE,
    push_alertas = FALSE,
    push_briefing = FALSE,
    briefing_hora = 450L,
    push_vapid_public = if (exists("alertas_push_vapid_public", mode = "function")) alertas_push_vapid_public() else ""
  )
  if (!alertas_preferencias_tabla_existe()) return(defaults)
  df <- tryCatch(db_query(
    "SELECT email_modo, email_alternativo, variable_ids, alerta_max, alerta_min,
            push_alertas, push_briefing, briefing_hora
     FROM gusto_de_aviso WHERE user_id = $1::uuid",
    params = list(user_id)
  ), error = function(e) db_query(
    "SELECT email_modo, email_alternativo, variable_ids, alerta_max, alerta_min
     FROM gusto_de_aviso WHERE user_id = $1::uuid",
    params = list(user_id)
  ))
  if (nrow(df) == 0) {
    defaults$email_efectivo <- resolver_email_destino(user_id, cuenta_email)
    return(defaults)
  }
  row <- df[1, ]
  vids <- row$variable_ids
  if (!is.null(vids) && length(vids) == 0) vids <- NULL
  if (!is.null(vids)) {
    vids <- as.integer(vids)
    vids <- vids[!is.na(vids) & vids > 0L]
    if (length(vids) == 0) vids <- NULL
  }
  list(
    email_modo = row$email_modo %||% "cuenta",
    email_alternativo = row$email_alternativo,
    email_efectivo = resolver_email_destino(user_id, cuenta_email),
    variable_ids = vids,
    alerta_max = isTRUE(row$alerta_max),
    alerta_min = isTRUE(row$alerta_min),
    push_alertas = if ("push_alertas" %in% names(row)) isTRUE(row$push_alertas) else FALSE,
    push_briefing = if ("push_briefing" %in% names(row)) isTRUE(row$push_briefing) else FALSE,
    briefing_hora = if ("briefing_hora" %in% names(row)) {
      h <- suppressWarnings(as.integer(row$briefing_hora))
      if (length(h) != 1L || is.na(h)) 450L else h
    } else 450L,
    push_vapid_public = if (exists("alertas_push_vapid_public", mode = "function")) alertas_push_vapid_public() else ""
  )
}

upsert_preferencias_usuario <- function(user_id, body, tier = "gratuito") {
  solo_push <- is.null(body$email_modo) && is.null(body$variable_ids) && is.null(body$alerta_max) &&
    is.null(body$alerta_min) &&
    (!is.null(body$push_alertas) || !is.null(body$push_briefing) || !is.null(body$briefing_hora))
  if (isTRUE(solo_push)) {
    if (!exists("alertas_push_guardar_prefs", mode = "function")) {
      return(list(ok = FALSE, error = "Avisos del celular no disponibles"))
    }
    return(alertas_push_guardar_prefs(user_id, body))
  }
  email_modo <- tolower(trimws(body$email_modo %||% "cuenta"))
  if (!email_modo %in% c("cuenta", "alternativo")) {
    return(list(ok = FALSE, error = "email_modo debe ser cuenta o alternativo"))
  }
  alt <- body$email_alternativo
  if (identical(email_modo, "alternativo")) {
    chk <- validar_email_alerta(alt)
    if (!isTRUE(chk$ok)) return(chk)
    alt <- chk$email
  } else {
    alt <- NULL
  }
  vids <- body$variable_ids
  if (!is.null(vids) && length(vids) == 0) vids <- NULL
  if (!is.null(vids)) {
    vids <- unique(as.integer(unlist(vids)))
    vids <- vids[!is.na(vids) & vids > 0L]
    if (length(vids) == 0) vids <- NULL
  }
  chk_vars <- validar_preferencias_variables(vids, tier)
  if (!isTRUE(chk_vars$ok)) return(chk_vars)
  vids <- chk_vars$variable_ids
  alerta_max <- if (is.null(body$alerta_max)) TRUE else parse_bool(body$alerta_max)
  alerta_min <- if (is.null(body$alerta_min)) TRUE else parse_bool(body$alerta_min)
  if (!alerta_max && !alerta_min) {
    return(list(ok = FALSE, error = "Debes activar al menos un tipo de alerta (max o min)"))
  }
  if (!alertas_preferencias_tabla_existe()) {
    return(list(ok = FALSE, error = "Preferencias de alertas no disponibles"))
  }
  db_execute(
    "INSERT INTO gusto_de_aviso (user_id, email_modo, email_alternativo, variable_ids, alerta_max, alerta_min, updated_at)
     VALUES ($1::uuid, $2, $3, $4::integer[], $5, $6, NOW())
     ON CONFLICT (user_id) DO UPDATE SET
       email_modo = EXCLUDED.email_modo,
       email_alternativo = EXCLUDED.email_alternativo,
       variable_ids = EXCLUDED.variable_ids,
       alerta_max = EXCLUDED.alerta_max,
       alerta_min = EXCLUDED.alerta_min,
       updated_at = NOW()",
    params = list(user_id, email_modo, pg_text_param(alt), pg_int_array_param(vids), alerta_max, alerta_min)
  )
  list(ok = TRUE)
}

scopes_permitidos_tier <- function(tier) {
  lim <- get_limite_tier(tier %||% "gratuito")
  scopes <- lim$alertas_scopes %||% ALERTAS_SCOPE_LEGACY
  as.character(scopes)
}

resolver_scope_eids <- function(user_id, scope_type, scope_value = list()) {
  scope_type <- tolower(trimws(scope_type %||% ""))
  scope_value <- scope_value %||% list()
  if (scope_type == "favorita") {
    df <- db_query(
      "SELECT eid FROM miradores_pinchados WHERE user_id = $1::uuid AND es_primario = TRUE LIMIT 1",
      params = list(user_id)
    )
    if (nrow(df) == 0) return(integer(0))
    return(as.integer(df$eid[1]))
  }
  if (scope_type == "grupo") {
    df <- db_query(
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
    eid <- as.integer(scope_value$eid %||% scope_value$eid)
    if (is.na(eid) || eid < 1L) return(integer(0))
    return(eid)
  }
  if (scope_type == "comuna") {
    com <- trimws(as.character(scope_value$nom_com %||% ""))
    if (!nzchar(com)) return(integer(0))
    df <- db_query(
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
    df <- db_query(
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
    df <- db_query(
      "SELECT id FROM mirador WHERE TRIM(COALESCE(nom_cuen, '')) = $1
       AND lat BETWEEN -56 AND -17 AND lon BETWEEN -76 AND -66",
      params = list(cu)
    )
    if (nrow(df) == 0) return(integer(0))
    return(as.integer(df$id))
  }
  integer(0)
}

scope_configurado <- function(user_id, scope_type, scope_value = list()) {
  length(resolver_scope_eids(user_id, scope_type, scope_value)) > 0L
}

count_reglas_activas <- function(user_id, exclude_regla_id = NULL, tier = NULL) {
  if (!alertas_regla_tabla_existe()) return(count_suscripciones_activas_legacy(user_id))
  if (!is.null(tier)) {
    allowed <- scopes_permitidos_tier(tier)
    df <- db_query(
      "SELECT id, scope_type FROM norma_propia WHERE user_id = $1::uuid AND activa = TRUE",
      params = list(user_id)
    )
    if (nrow(df) == 0) return(0L)
    df <- df[df$scope_type %in% allowed, , drop = FALSE]
    if (!is.null(exclude_regla_id) && nzchar(as.character(exclude_regla_id))) {
      df <- df[as.character(df$id) != as.character(exclude_regla_id), , drop = FALSE]
    }
    return(as.integer(nrow(df)))
  }
  if (is.null(exclude_regla_id) || !nzchar(as.character(exclude_regla_id))) {
    df <- db_query(
      "SELECT COUNT(*)::integer AS n FROM norma_propia WHERE user_id = $1::uuid AND activa = TRUE",
      params = list(user_id)
    )
  } else {
    df <- db_query(
      "SELECT COUNT(*)::integer AS n FROM norma_propia
       WHERE user_id = $1::uuid AND activa = TRUE AND id <> $2::uuid",
      params = list(user_id, exclude_regla_id)
    )
  }
  if (nrow(df) == 0) return(0L)
  as.integer(df$n[1])
}

count_suscripciones_activas_legacy <- function(user_id, exclude_scope = NULL) {
  chk <- tryCatch(
    db_query("SELECT 1 FROM information_schema.tables WHERE table_name = 'pedido_de_aviso' LIMIT 1"),
    error = function(e) NULL
  )
  if (is.null(chk) || nrow(chk) == 0) return(0L)
  if (is.null(exclude_scope) || !nzchar(exclude_scope)) {
    df <- db_query(
      "SELECT COUNT(*)::integer AS n FROM pedido_de_aviso WHERE user_id = $1::uuid AND activa = TRUE",
      params = list(user_id)
    )
  } else {
    df <- db_query(
      "SELECT COUNT(*)::integer AS n FROM pedido_de_aviso
       WHERE user_id = $1::uuid AND activa = TRUE AND scope_type <> $2",
      params = list(user_id, exclude_scope)
    )
  }
  if (nrow(df) == 0) return(0L)
  as.integer(df$n[1])
}

count_suscripciones_activas <- function(user_id, exclude_scope = NULL) {
  count_reglas_activas(user_id, exclude_regla_id = NULL)
}

puede_activar_regla <- function(user_id, tier, scope_type, scope_value = list(), activa = TRUE, regla_id = NULL) {
  scope_type <- tolower(trimws(scope_type %||% ""))
  if (!scope_type %in% ALERTAS_SCOPE_TYPES) {
    return(list(ok = FALSE, error = "scope_type inv\u00e1lido"))
  }
  allowed <- scopes_permitidos_tier(tier)
  if (!scope_type %in% allowed) {
    return(list(ok = FALSE, error = paste0("Tu plan no permite alertas por ", scope_type)))
  }
  if (!isTRUE(activa)) return(list(ok = TRUE))
  if (!scope_configurado(user_id, scope_type, scope_value)) {
    msg <- switch(scope_type,
      favorita = "Debes fijar una estaci\u00f3n favorita antes de activar alertas",
      grupo = "Debes guardar un grupo de estaciones antes de activar alertas",
      estacion = "Debes seleccionar una estaci\u00f3n",
      comuna = "Debes seleccionar una comuna con estaciones",
      region = "Debes seleccionar una regi\u00f3n con estaciones",
      cuenca = "Debes seleccionar una cuenca con estaciones",
      "Alcance no configurado"
    )
    return(list(ok = FALSE, error = msg))
  }
  lim <- get_limite_tier(tier %||% "gratuito")
  max_subs <- lim$max_alertas_suscripcion
  if (!is.null(max_subs) && max_subs >= 0L) {
    otras <- count_reglas_activas(user_id, exclude_regla_id = regla_id, tier = tier)
    if (otras >= max_subs) {
      return(list(
        ok = FALSE,
        error = "En plan gratuito solo puedes tener 1 alerta activa"
      ))
    }
  }
  list(ok = TRUE)
}

puede_activar_suscripcion <- function(user_id, tier, scope_type, activa = TRUE) {
  puede_activar_regla(user_id, tier, scope_type, list(), activa)
}

regla_to_list <- function(row, user_id) {
  sv <- tryCatch(jsonlite::fromJSON(row$scope_value %||% "{}", simplifyVector = FALSE), error = function(e) list())
  list(
    id = as.character(row$id),
    nombre = row$nombre %||% "",
    scope_type = row$scope_type,
    scope_value = sv,
    activa = isTRUE(row$activa),
    scope_configurado = scope_configurado(user_id, row$scope_type, sv),
    n_estaciones = length(resolver_scope_eids(user_id, row$scope_type, sv))
  )
}

listar_reglas_usuario <- function(user_id) {
  if (!alertas_regla_tabla_existe()) return(list())
  df <- db_query(
    "SELECT id, nombre, scope_type, scope_value, activa FROM norma_propia
     WHERE user_id = $1::uuid ORDER BY created_at ASC",
    params = list(user_id)
  )
  if (nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) regla_to_list(df[i, ], user_id))
}

get_suscripciones_usuario <- function(user_id) {
  if (alertas_regla_tabla_existe()) {
    reglas <- listar_reglas_usuario(user_id)
    out <- list(
      favorita = list(activa = FALSE, scope_configurado = scope_configurado(user_id, "favorita")),
      grupo = list(activa = FALSE, scope_configurado = scope_configurado(user_id, "grupo"))
    )
    for (r in reglas) {
      st <- r$scope_type
      if (st %in% names(out)) {
        out[[st]]$activa <- isTRUE(r$activa)
        out[[st]]$regla_id <- r$id
      }
    }
    return(out)
  }
  if (!alertas_tabla_existe()) {
    return(list(
      favorita = list(activa = FALSE, scope_configurado = scope_configurado(user_id, "favorita")),
      grupo = list(activa = FALSE, scope_configurado = scope_configurado(user_id, "grupo"))
    ))
  }
  df <- db_query(
    "SELECT scope_type, activa FROM pedido_de_aviso WHERE user_id = $1::uuid",
    params = list(user_id)
  )
  out <- list(
    favorita = list(activa = FALSE, scope_configurado = scope_configurado(user_id, "favorita")),
    grupo = list(activa = FALSE, scope_configurado = scope_configurado(user_id, "grupo"))
  )
  if (nrow(df) > 0) {
    for (i in seq_len(nrow(df))) {
      st <- df$scope_type[i]
      if (st %in% names(out)) out[[st]]$activa <- isTRUE(df$activa[i])
    }
  }
  out
}

upsert_regla_legacy <- function(user_id, scope_type, activa) {
  if (alertas_regla_tabla_existe()) {
    df <- db_query(
      "SELECT id FROM norma_propia WHERE user_id = $1::uuid AND scope_type = $2 LIMIT 1",
      params = list(user_id, scope_type)
    )
    nombre <- if (scope_type == "favorita") "Estaci\u00f3n favorita" else "Mi grupo"
    if (nrow(df) > 0) {
      db_execute(
        "UPDATE norma_propia SET activa = $2, updated_at = NOW() WHERE id = $1::uuid",
        params = list(df$id[1], isTRUE(activa))
      )
    } else {
      db_execute(
        "INSERT INTO norma_propia (user_id, nombre, scope_type, scope_value, activa)
         VALUES ($1::uuid, $2, $3, '{}'::jsonb, $4)",
        params = list(user_id, nombre, scope_type, isTRUE(activa))
      )
    }
    return(invisible(TRUE))
  }
  db_execute(
    "INSERT INTO pedido_de_aviso (user_id, scope_type, activa, updated_at)
     VALUES ($1::uuid, $2, $3, NOW())
     ON CONFLICT (user_id, scope_type) DO UPDATE
       SET activa = EXCLUDED.activa, updated_at = NOW()",
    params = list(user_id, scope_type, isTRUE(activa))
  )
}

upsert_suscripcion <- function(user_id, scope_type, activa) {
  upsert_regla_legacy(user_id, scope_type, activa)
}

# Pausa un alcance sin crear la fila si no existía.
pausar_scope_legacy <- function(user_id, scope_type) {
  if (alertas_regla_tabla_existe()) {
    db_execute(
      "UPDATE norma_propia SET activa = FALSE, updated_at = NOW()
       WHERE user_id = $1::uuid AND scope_type = $2",
      params = list(user_id, scope_type)
    )
    return(invisible(TRUE))
  }
  if (!alertas_tabla_existe()) return(invisible(FALSE))
  db_execute(
    "UPDATE pedido_de_aviso SET activa = FALSE, updated_at = NOW()
     WHERE user_id = $1::uuid AND scope_type = $2",
    params = list(user_id, scope_type)
  )
  invisible(TRUE)
}

# Las reglas de región/comuna se pausan, no se borran: al volver a avanzada se restauran.
set_reglas_geo_activas <- function(user_id, activa) {
  if (!alertas_regla_tabla_existe()) return(invisible(FALSE))
  db_execute(
    "UPDATE norma_propia SET activa = $2, updated_at = NOW()
     WHERE user_id = $1::uuid AND scope_type IN ('region', 'comuna')",
    params = list(user_id, isTRUE(activa))
  )
  invisible(TRUE)
}

# Un solo modo entre favorita, grupo y avanzada.
aplicar_alcance_exclusivo <- function(user_id, tier, scope_type, activa) {
  scope_type <- tolower(trimws(scope_type %||% ""))
  if (!scope_type %in% c("favorita", "grupo", "avanzada")) {
    return(list(ok = FALSE, error = "scope_type debe ser favorita, grupo o avanzada"))
  }
  if (!isTRUE(activa)) {
    if (identical(scope_type, "avanzada")) {
      set_reglas_geo_activas(user_id, FALSE)
    } else {
      upsert_suscripcion(user_id, scope_type, FALSE)
    }
    return(list(ok = TRUE))
  }
  if (identical(scope_type, "avanzada")) {
    allowed <- scopes_permitidos_tier(tier)
    if (!any(c("region", "comuna") %in% allowed)) {
      return(list(ok = FALSE, error = "Tu plan no permite seleccionar por regi\u00f3n o comuna"))
    }
    pausar_scope_legacy(user_id, "favorita")
    pausar_scope_legacy(user_id, "grupo")
    set_reglas_geo_activas(user_id, TRUE)
    return(list(ok = TRUE))
  }
  chk <- puede_activar_suscripcion(user_id, tier, scope_type, TRUE)
  if (!isTRUE(chk$ok)) return(chk)
  otro <- if (identical(scope_type, "favorita")) "grupo" else "favorita"
  pausar_scope_legacy(user_id, otro)
  set_reglas_geo_activas(user_id, FALSE)
  upsert_suscripcion(user_id, scope_type, TRUE)
  list(ok = TRUE)
}

crear_regla <- function(user_id, tier, body) {
  scope_type <- tolower(trimws(body$scope_type %||% ""))
  scope_value <- body$scope_value %||% list()
  activa <- if (is.null(body$activa)) TRUE else isTRUE(body$activa)
  nombre <- trimws(as.character(body$nombre %||% ""))
  chk <- puede_activar_regla(user_id, tier, scope_type, scope_value, activa)
  if (!isTRUE(chk$ok)) return(chk)
  if (scope_type %in% c("region", "comuna") && isTRUE(activa)) {
    pausar_scope_legacy(user_id, "favorita")
    pausar_scope_legacy(user_id, "grupo")
  }
  if (!nzchar(nombre)) {
    nombre <- switch(scope_type,
      favorita = "Estaci\u00f3n favorita",
      grupo = "Mi grupo",
      estacion = paste("Estaci\u00f3n", scope_value$eid %||% ""),
      comuna = paste("Comuna", scope_value$nom_com %||% ""),
      region = paste("Regi\u00f3n", scope_value$cod_reg %||% ""),
      cuenca = paste("Cuenca", scope_value$nom_cuen %||% ""),
      "Alerta"
    )
  }
  sv_json <- jsonlite::toJSON(scope_value, auto_unbox = TRUE, null = "null")
  df <- db_query(
    "INSERT INTO norma_propia (user_id, nombre, scope_type, scope_value, activa)
     VALUES ($1::uuid, $2, $3, $4::jsonb, $5)
     RETURNING id, nombre, scope_type, scope_value, activa",
    params = list(user_id, nombre, scope_type, sv_json, activa)
  )
  list(ok = TRUE, regla = regla_to_list(df[1, ], user_id))
}

actualizar_regla <- function(user_id, tier, regla_id, body) {
  df <- db_query(
    "SELECT id, scope_type, scope_value, activa FROM norma_propia
     WHERE id = $1::uuid AND user_id = $2::uuid",
    params = list(regla_id, user_id)
  )
  if (nrow(df) == 0) return(list(ok = FALSE, error = "Regla no encontrada"))
  row <- df[1, ]
  scope_type <- tolower(trimws(body$scope_type %||% row$scope_type))
  sv <- tryCatch(jsonlite::fromJSON(row$scope_value, simplifyVector = FALSE), error = function(e) list())
  if (!is.null(body$scope_value)) sv <- body$scope_value
  activa <- if (is.null(body$activa)) isTRUE(row$activa) else isTRUE(body$activa)
  nombre <- if (!is.null(body$nombre)) trimws(as.character(body$nombre)) else NULL
  chk <- puede_activar_regla(user_id, tier, scope_type, sv, activa, regla_id = regla_id)
  if (!isTRUE(chk$ok)) return(chk)
  sv_json <- jsonlite::toJSON(sv, auto_unbox = TRUE, null = "null")
  params <- list(regla_id, user_id, scope_type, sv_json, activa)
  q <- "UPDATE norma_propia SET scope_type = $3, scope_value = $4::jsonb, activa = $5, updated_at = NOW()"
  if (!is.null(nombre) && nzchar(nombre)) {
    q <- paste0(q, ", nombre = $6")
    params <- c(params, list(nombre))
  }
  q <- paste0(q, " WHERE id = $1::uuid AND user_id = $2::uuid RETURNING id, nombre, scope_type, scope_value, activa")
  out <- db_query(q, params = params)
  list(ok = TRUE, regla = regla_to_list(out[1, ], user_id))
}

eliminar_regla <- function(user_id, regla_id) {
  n <- db_execute(
    "DELETE FROM norma_propia WHERE id = $1::uuid AND user_id = $2::uuid",
    params = list(regla_id, user_id)
  )
  list(ok = n > 0L)
}

variables_con_datos <- function(eid) {
  eid <- as.integer(eid)
  if (is.na(eid) || eid < 1L) return(integer(0))
  df <- db_query(
    "SELECT DISTINCT id_variable FROM bitacora_oficial WHERE id_estacion = $1 ORDER BY id_variable",
    params = list(eid)
  )
  if (nrow(df) == 0) return(integer(0))
  filtrar_variable_ids_alertas(as.integer(df$id_variable))
}

listar_eventos_usuario <- function(user_id, limit = 50L, offset = 0L) {
  if (!alertas_tabla_existe()) return(list())
  limit <- as.integer(limit)
  if (is.na(limit) || limit < 1L) limit <- 50L
  if (limit > 200L) limit <- 200L
  offset <- as.integer(offset)
  if (is.na(offset) || offset < 0L) offset <- 0L
  if (offset > 100000L) offset <- 100000L
  has_tipo <- nrow(db_query(
    "SELECT column_name FROM information_schema.columns
     WHERE table_name = 'campanada' AND column_name = 'tipo' LIMIT 1"
  )) > 0
  tipo_sel <- if (has_tipo) ", COALESCE(e.tipo, 'tardia') AS tipo" else ", 'tardia' AS tipo"
  df <- db_query(
    paste0(
      "SELECT e.eid, e.variable_id, e.fecha, e.direccion, e.ranking, e.valor",
      tipo_sel, ",
            e.enviado_en, e.exito, est.nombre AS estacion_nombre, v.nombre AS variable_nombre,
            COALESCE(v.nombre_abr, '') AS nombre_abr
     FROM campanada e
     LEFT JOIN mirador est ON est.id = e.eid
     LEFT JOIN magnitud v ON v.id = e.variable_id
     WHERE e.user_id = $1::uuid
     ORDER BY e.enviado_en DESC
     LIMIT $2 OFFSET $3"
    ),
    params = list(user_id, limit, offset)
  )
  if (nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    abr <- row$nombre_abr %||% ""
    tipo <- if ("tipo" %in% names(row) && nzchar(row$tipo %||% "")) row$tipo else "tardia"
    tipo_etiqueta <- if (exists("alerta_digest_tipo_label", mode = "function")) {
      alerta_digest_tipo_label(tipo)
    } else if (identical(tipo, "temprana")) {
      "Temprana"
    } else {
      "Tard\u00eda"
    }
    list(
      eid = as.integer(row$eid),
      variable_id = as.integer(row$variable_id),
      fecha = as.character(row$fecha),
      direccion = row$direccion,
      ranking = as.integer(row$ranking),
      valor = display_valor(as.numeric(row$valor), abr),
      unidad = display_unidad(abr, ""),
      enviado_en = as.character(row$enviado_en),
      exito = isTRUE(row$exito),
      estacion_nombre = row$estacion_nombre %||% "",
      variable_nombre = row$variable_nombre %||% "",
      tipo = tipo,
      tipo_etiqueta = tipo_etiqueta
    )
  })
}

contar_eventos_usuario <- function(user_id) {
  if (!alertas_tabla_existe()) return(0L)
  df <- db_query(
    "SELECT COUNT(*)::integer AS n FROM campanada WHERE user_id = $1::uuid",
    params = list(user_id)
  )
  if (nrow(df) == 0) return(0L)
  as.integer(df$n[1])
}

alertas_historial_csv_estimar_bytes <- function(n_eventos) {
  n <- as.integer(n_eventos)
  if (is.na(n) || n < 1L) return(500L)
  as.integer(max(500L, 80L + n * 110L))
}

alertas_tipo_etiqueta_csv <- function(direccion) {
  if (identical(tolower(trimws(as.character(direccion %||% ""))), "min")) {
    return("m\u00edn")
  }
  "m\u00e1x"
}

alertas_eventos_df_usuario <- function(user_id) {
  if (!alertas_tabla_existe()) return(data.frame())
  db_query(
    "SELECT e.ranking, est.nombre AS estacion_nombre, v.nombre AS variable_nombre,
            e.fecha, e.direccion, e.valor, COALESCE(v.nombre_abr, '') AS nombre_abr
     FROM campanada e
     LEFT JOIN mirador est ON est.id = e.eid
     LEFT JOIN magnitud v ON v.id = e.variable_id
     WHERE e.user_id = $1::uuid
     ORDER BY e.enviado_en DESC",
    params = list(user_id)
  )
}

# CSV del historial: api/utils/alertas_resumen_query.R (columnas segun la vista).

estadisticas_alertas_usuario <- function(user_id) {
  total <- 0L
  este_mes <- 0L
  por_mes <- list()
  por_dia <- list()
  ultimo_digest <- NULL
  if (!alertas_tabla_existe()) {
    return(list(total = total, este_mes = este_mes, por_mes = por_mes, por_dia = por_dia, ultimo_digest = ultimo_digest))
  }
  df_total <- db_query(
    "SELECT COUNT(*)::integer AS n FROM campanada WHERE user_id = $1::uuid AND exito = TRUE",
    params = list(user_id)
  )
  if (nrow(df_total) > 0) total <- as.integer(df_total$n[1])
  df_mes <- db_query(
    "SELECT COUNT(*)::integer AS n FROM campanada
     WHERE user_id = $1::uuid AND exito = TRUE
       AND enviado_en >= date_trunc('month', CURRENT_DATE)",
    params = list(user_id)
  )
  if (nrow(df_mes) > 0) este_mes <- as.integer(df_mes$n[1])
  df_series <- db_query(
    "SELECT to_char(date_trunc('month', enviado_en), 'YYYY-MM') AS mes,
            COUNT(*)::integer AS n
     FROM campanada
     WHERE user_id = $1::uuid AND exito = TRUE
       AND enviado_en >= (CURRENT_DATE - INTERVAL '12 months')
     GROUP BY 1 ORDER BY 1",
    params = list(user_id)
  )
  if (nrow(df_series) > 0) {
    por_mes <- lapply(seq_len(nrow(df_series)), function(i) {
      list(mes = df_series$mes[i], n = as.integer(df_series$n[i]))
    })
  }
  df_dias <- db_query(
    "SELECT (enviado_en AT TIME ZONE 'America/Santiago')::date AS dia,
            COUNT(*)::integer AS n
     FROM campanada
     WHERE user_id = $1::uuid AND exito = TRUE
       AND enviado_en >= ((CURRENT_TIMESTAMP AT TIME ZONE 'America/Santiago')::date - INTERVAL '364 days')
     GROUP BY 1 ORDER BY 1",
    params = list(user_id)
  )
  if (nrow(df_dias) > 0) {
    por_dia <- lapply(seq_len(nrow(df_dias)), function(i) {
      list(dia = as.character(df_dias$dia[i]), n = as.integer(df_dias$n[i]))
    })
  }
  if (alertas_digest_tabla_existe()) {
    u_row <- db_query("SELECT email FROM tripulacion WHERE id = $1::uuid", params = list(user_id))
    email_eff <- if (nrow(u_row) > 0) resolver_email_destino(user_id, u_row$email[1]) else ""
    email_eff <- trimws(tolower(email_eff %||% ""))
    join_ap <- if (alertas_preferencias_tabla_existe()) {
      "LEFT JOIN gusto_de_aviso ap ON ap.user_id = u.id"
    } else {
      ""
    }
    eff_sql <- if (alertas_preferencias_tabla_existe()) {
      alertas_email_efectivo_sql("u", "ap")
    } else {
      "lower(trim(u.email))"
    }
    df_d <- if (nzchar(email_eff)) db_query(
      paste0(
        "SELECT d.fecha_eval, d.n_eventos, d.enviado_en, d.exito
         FROM sobre_del_resumen d
         JOIN tripulacion u ON u.id = d.user_id
         ", join_ap, "
         WHERE d.exito = TRUE AND ", eff_sql, " = $1
         ORDER BY d.fecha_eval DESC, d.enviado_en DESC NULLS LAST
         LIMIT 1"
      ),
      params = list(email_eff)
    ) else data.frame()
    if (nrow(df_d) > 0) {
      ultimo_digest <- list(
        fecha_eval = as.character(df_d$fecha_eval[1]),
        n_eventos = as.integer(df_d$n_eventos[1]),
        enviado_en = as.character(df_d$enviado_en[1]),
        exito = isTRUE(df_d$exito[1])
      )
    }
  }
  list(total = total, este_mes = este_mes, por_mes = por_mes, por_dia = por_dia, ultimo_digest = ultimo_digest)
}


upsert_alerta_tardia_usuario <- function(user_id, activa) {
  if (!alertas_preferencias_tabla_existe()) {
    return(list(ok = FALSE, error = "Preferencias de alertas no disponibles"))
  }
  activa <- isTRUE(activa)
  tryCatch({
    db_execute(
      "INSERT INTO gusto_de_aviso (user_id, alerta_tardia, updated_at)
       VALUES ($1::uuid, $2, NOW())
       ON CONFLICT (user_id) DO UPDATE SET alerta_tardia = EXCLUDED.alerta_tardia, updated_at = NOW()",
      params = list(user_id, activa)
    )
    list(ok = TRUE, alerta_tardia = activa)
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
}