#* @apiTitle Usuario autenticado
#* @get /me
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  u <- req$user
  list(
    id = u$id,
    email = u$email,
    tier = u$tier,
    tipo_usuario = get_tipo_usuario(req),
    is_admin = usuario_es_admin(u)
  )
}

#* Cuota de descarga del periodo actual
#* @get /me/cuota
function(req, res, email = "") {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  if (nzchar(trimws(email %||% "")) && !is.null(req$user)) {
    req$user <- sync_usuario_email(req$user, email)
  }
  tier <- get_tipo_usuario(req)
  uid <- get_user_id(req)
  cuota <- get_cuota_usuario(uid, tier)
  lim <- get_limite_tier(tier)
  bytes_incluidos_usados <- min(cuota$bytes_usados, cuota$limite_bytes)
  bytes_overage_hoy <- max(0, cuota$bytes_usados - cuota$limite_bytes)
  od <- get_on_demand_settings(uid, tier)
  c(cuota, list(
    api_access = isTRUE(lim$api_access),
    tier_label = switch(tier,
      enterprise = "Institucional",
      pro = "Pro",
      gratuito = "Gratuito",
      tier
    ),
    bytes_incluidos_usados = bytes_incluidos_usados,
    bytes_overage_hoy = bytes_overage_hoy,
    on_demand_enabled = isTRUE(od$enabled)
  ))
}

tier_label_usuario <- function(tier) {
  switch(tier %||% "gratuito",
    enterprise = "Institucional",
    pro = "Pro",
    gratuito = "Gratuito",
    as.character(tier %||% "Gratuito")
  )
}

#* Estado de suscripcion (sin secretos de pago)
#* @get /me/billing
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  uid <- get_user_id(req)
  tier <- get_tipo_usuario(req)
  df <- db_query(
    "SELECT tier, billing_provider, subscription_status, subscription_period_end, stripe_subscription_id
     FROM tripulacion WHERE id = $1::uuid AND activo = TRUE",
    params = list(uid)
  )
  sub_id <- ""
  sub_status <- NULL
  period_end <- NULL
  provider <- "flow"
  if (nrow(df) > 0) {
    db_tier <- as.character(df$tier[1] %||% "")
    if (nzchar(db_tier)) tier <- db_tier
    sub_id <- trimws(as.character(df$stripe_subscription_id[1] %||% ""))
    sub_status <- as.character(df$subscription_status[1] %||% "")
    if (!nzchar(sub_status)) sub_status <- NULL
    pe <- df$subscription_period_end[1]
    if (!is.na(pe) && !is.null(pe)) {
      period_end <- format(as.POSIXct(pe, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
    bp <- trimws(as.character(df$billing_provider[1] %||% ""))
    if (nzchar(bp)) provider <- bp
  }
  checkout_ok <- exists("flow_billing_configured", mode = "function") && isTRUE(flow_billing_configured())
  is_pro <- identical(tier, "pro")
  is_gratuito <- identical(tier, "gratuito")
  list(
    tier = tier,
    tier_label = tier_label_usuario(tier),
    provider = provider,
    subscription_status = sub_status,
    subscription_period_end = period_end,
    has_active_subscription = is_pro,
    checkout_enabled = checkout_ok,
    can_upgrade = is_gratuito,
    can_cancel = is_pro && nzchar(sub_id)
  )
}

#* Historial y uso de descargas del usuario
#* @get /me/descargas
function(req, res, limit = "15", dias = "30") {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  uid <- get_user_id(req)
  tier <- get_tipo_usuario(req)
  n <- suppressWarnings(as.integer(limit))
  if (is.na(n) || n < 1L) n <- 15L
  if (n > 50L) n <- 50L
  d <- suppressWarnings(as.integer(dias))
  if (is.na(d) || d < 1L) d <- 30L
  if (d > 90L) d <- 90L
  list(
    cuota = get_cuota_usuario(uid, tier),
    serie_diaria = serie_bytes_descargas_usuario(uid, d),
    recientes = listar_descargas_recientes_usuario(uid, n)
  )
}

#* @get /me/favoritos
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  uid <- get_user_id(req)
  df <- db_query(
    "SELECT eid, variable_id, fuente_id, alias, es_primario, lat, lon
     FROM miradores_pinchados WHERE user_id = $1::uuid ORDER BY es_primario DESC, updated_at DESC",
    params = list(uid)
  )
  primary <- NULL
  recents <- list()
  if (nrow(df) > 0) {
    for (i in seq_len(nrow(df))) {
      row <- df[i, ]
      fav <- list(
        eid = as.integer(row$eid),
        variableId = if (is.na(row$variable_id)) NULL else as.integer(row$variable_id),
        fuenteId = if (is.na(row$fuente_id)) NULL else as.integer(row$fuente_id),
        alias = row$alias %||% "",
        lat = if (is.na(row$lat)) NULL else as.numeric(row$lat),
        lon = if (is.na(row$lon)) NULL else as.numeric(row$lon)
      )
      if (isTRUE(row$es_primario)) primary <- fav
      recents[[length(recents) + 1]] <- fav
    }
  }
  list(primary = primary, recents = recents)
}

#* @put /me/favoritos
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }

  uid <- get_user_id(req)
  primary <- body$primary %||% NULL
  recents <- body$recents %||% list()

  normalize_fav <- function(f) {
    if (is.null(f) || !is.list(f)) return(NULL)
    eid <- as.integer(f$eid %||% f[["eid"]] %||% NA)
    if (is.na(eid) || eid < 1) return(NULL)
    list(
      eid = eid,
      variable_id = suppressWarnings(as.integer(f$variableId %||% f$variable_id %||% NA)),
      fuente_id = suppressWarnings(as.integer(f$fuenteId %||% f$fuente_id %||% NA)),
      alias = as.character(f$alias %||% ""),
      lat = suppressWarnings(as.numeric(f$lat %||% NA)),
      lon = suppressWarnings(as.numeric(f$lon %||% NA))
    )
  }

  p <- normalize_fav(primary)
  rec_norm <- list()
  seen <- integer(0)
  if (!is.null(p)) seen <- c(seen, p$eid)
  if (length(recents) > 0) {
    for (r in recents) {
      n <- normalize_fav(r)
      if (is.null(n)) next
      if (n$eid %in% seen) next
      seen <- c(seen, n$eid)
      rec_norm[[length(rec_norm) + 1]] <- n
      if (length(rec_norm) >= 5) break
    }
  }
  if (!is.null(p) && !(p$eid %in% vapply(rec_norm, function(x) x$eid, integer(1)))) {
    rec_norm <- c(list(p), rec_norm)
    rec_norm <- rec_norm[seq_len(min(5, length(rec_norm)))]
  }

  db_execute("DELETE FROM miradores_pinchados WHERE user_id = $1::uuid", params = list(uid))

  insert_fav <- function(f, es_primario) {
    db_execute(
      "INSERT INTO miradores_pinchados (user_id, eid, variable_id, fuente_id, alias, es_primario, lat, lon)
       VALUES ($1::uuid, $2, $3, $4, $5, $6, $7, $8)",
      params = list(
        uid, f$eid,
        pg_na_int(f$variable_id),
        pg_na_int(f$fuente_id),
        f$alias,
        es_primario,
        pg_na_dbl(f$lat),
        pg_na_dbl(f$lon)
      )
    )
  }

  if (!is.null(p)) insert_fav(p, TRUE)
  for (f in rec_norm) {
    if (!is.null(p) && f$eid == p$eid) next
    insert_fav(f, FALSE)
  }

  list(ok = TRUE, primary = p, recents = rec_norm)
}

#* Grupo de estaciones del usuario (sync liviano)
#* @get /me/grupo-estaciones
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  uid <- get_user_id(req)
  df <- db_query(
    "SELECT payload, updated_at FROM cuadrilla WHERE user_id = $1::uuid",
    params = list(uid)
  )
  if (nrow(df) == 0) {
    return(list(nombre = "Mi grupo", ids = list(), updated_at = NULL))
  }
  payload <- tryCatch(jsonlite::fromJSON(df$payload[1], simplifyVector = FALSE), error = function(e) list())
  list(
    nombre = payload$nombre %||% "Mi grupo",
    ids = as.list(as.integer(unlist(payload$ids %||% list()))),
    updated_at = as.character(df$updated_at[1])
  )
}

#* @put /me/grupo-estaciones
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) { res$status <- 400; return(list(error = "JSON inv\u00e1lido")) }

  uid <- get_user_id(req)
  nombre <- trimws(as.character(body$nombre %||% "Mi grupo"))
  ids_raw <- body$ids %||% list()
  ids <- unique(as.integer(unlist(ids_raw)))
  ids <- ids[!is.na(ids) & ids > 0L]
  updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  payload <- jsonlite::toJSON(list(nombre = nombre, ids = ids, updated_at = updated_at), auto_unbox = TRUE)

  db_execute(
    "INSERT INTO cuadrilla (user_id, payload, updated_at)
     VALUES ($1::uuid, $2::jsonb, NOW())
     ON CONFLICT (user_id) DO UPDATE SET payload = $2::jsonb, updated_at = NOW()",
    params = list(uid, payload)
  )

  list(ok = TRUE, nombre = nombre, ids = as.list(ids), updated_at = updated_at)
}

# --- Atribucion de registros (first-touch). Tabla: puerta_de_entrada ---
origen_txt <- function(x, n = 255L) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x[[1]])
  if (is.na(x)) return(NA_character_)
  x <- gsub("[[:cntrl:]]", "", trimws(x))
  if (!nzchar(x)) return(NA_character_)
  substr(x, 1L, n)
}

origen_referrer <- function(x) {
  x <- origen_txt(x, 2048L)
  if (is.na(x)) return(list(url = NA_character_, host = NA_character_))
  m <- regmatches(x, regexec("^(https?)://([^/?#@]+)([^?#]*)", x, ignore.case = TRUE))[[1]]
  if (length(m) < 4) return(list(url = NA_character_, host = NA_character_))
  host <- tolower(sub(":[0-9]+$", "", m[3]))
  path <- m[4]
  if (!nzchar(path)) path <- "/"
  list(url = substr(paste0(tolower(m[2]), "://", host, path), 1L, 255L), host = substr(host, 1L, 255L))
}

origen_path <- function(x) {
  x <- origen_txt(x, 2048L)
  if (is.na(x)) return(NA_character_)
  x <- sub("[?#].*$", "", x)
  if (!startsWith(x, "/")) return(NA_character_)
  substr(x, 1L, 255L)
}

#* Guarda de donde llego el usuario (primera visita). Solo la primera vez por cuenta.
#* @post /me/origen
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  body <- tryCatch(jsonlite::fromJSON(req$postBody %||% "{}", simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body) || !is.list(body)) body <- list()
  uid <- get_user_id(req)
  ref <- origen_referrer(body$referrer)
  ts <- suppressWarnings(as.numeric(body$first_touch_at %||% NA))
  ts_txt <- if (is.na(ts) || ts <= 0) NA_character_ else
    format(as.POSIXct(ts / 1000, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  n <- tryCatch(db_execute(
    "INSERT INTO puerta_de_entrada (user_id, referrer, referrer_host, utm_source, utm_medium, utm_campaign,
                                 landing_path, first_touch_at, es_registro)
     SELECT u.id, $2, $3, $4, $5, $6, $7, $8::timestamptz, (u.created_at > NOW() - INTERVAL '2 days')
     FROM tripulacion u WHERE u.id = $1::uuid
     ON CONFLICT (user_id) DO NOTHING",
    params = list(
      uid, ref$url, ref$host,
      origen_txt(body$utm_source), origen_txt(body$utm_medium), origen_txt(body$utm_campaign),
      origen_path(body$landing_path), ts_txt
    )
  ), error = function(e) { message("[origen] ", conditionMessage(e)); -1 })
  if (identical(n, -1)) { res$status <- 500; return(list(ok = FALSE)) }
  list(ok = TRUE, guardado = isTRUE(n > 0))
}
