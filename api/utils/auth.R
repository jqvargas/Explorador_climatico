# Autenticacion Clerk: JWT en Authorization Bearer + usuarios en PostgreSQL.

.JWKS_CACHE <- NULL
.JWKS_CACHE_AT <- NULL
.JWKS_TTL_SEC <- 3600

jwt_base64url_decode <- function(x) {
  x <- chartr("-_", "+/", x)
  pad <- (4 - nchar(x) %% 4) %% 4
  if (pad > 0) x <- paste0(x, strrep("=", pad))
  base64enc::base64decode(x)
}

get_jwks <- function() {
  url <- trimws(Sys.getenv("CLERK_JWKS_URL", ""))
  if (!nzchar(url)) return(NULL)
  now <- as.numeric(Sys.time())
  if (!is.null(.JWKS_CACHE) && !is.null(.JWKS_CACHE_AT) && (now - .JWKS_CACHE_AT) < .JWKS_TTL_SEC) {
    return(.JWKS_CACHE)
  }
  if (!requireNamespace("httr", quietly = TRUE)) return(NULL)
  resp <- tryCatch(httr::GET(url, httr::timeout(8)), error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  body <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(body) || !nzchar(body)) return(NULL)
  jwks <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(jwks) || is.null(jwks$keys)) return(NULL)
  .JWKS_CACHE <<- jwks
  .JWKS_CACHE_AT <<- now
  jwks
}

jwk_to_pubkey <- function(jwk) {
  if (is.null(jwk) || is.null(jwk$kty)) return(NULL)
  if (!requireNamespace("jose", quietly = TRUE)) return(NULL)
  tryCatch(jose::jwk_read(jwk), error = function(e) NULL)
}

verify_clerk_jwt <- function(token) {
  token <- trimws(token %||% "")
  if (!nzchar(token)) return(NULL)
  if (!requireNamespace("jose", quietly = TRUE)) return(NULL)

  parts <- tryCatch(jose::jwt_split(token), error = function(e) NULL)
  if (is.null(parts) || length(parts) < 2) return(NULL)

  header <- parts$header
  if (is.null(header) || is.null(header$kid)) {
    header <- tryCatch(
      jsonlite::fromJSON(rawToChar(jwt_base64url_decode(strsplit(token, ".", fixed = TRUE)[[1]][[1]]))),
      error = function(e) NULL
    )
  }
  if (is.null(header)) return(NULL)

  jwks <- get_jwks()
  if (is.null(jwks)) return(NULL)

  kid <- header$kid %||% ""
  jwk <- NULL
  for (k in jwks$keys) {
    if (identical(k$kid %||% "", kid)) {
      jwk <- k
      break
    }
  }
  if (is.null(jwk)) return(NULL)

  pubkey <- jwk_to_pubkey(jwk)
  if (is.null(pubkey)) return(NULL)

  payload <- tryCatch(jose::jwt_decode_sig(token, pubkey), error = function(e) NULL)
  if (is.null(payload)) return(NULL)

  if (!is.null(payload$exp) && as.numeric(payload$exp) < as.numeric(Sys.time())) return(NULL)

  issuer <- trimws(Sys.getenv("CLERK_ISSUER", ""))
  if (nzchar(issuer) && !identical(payload$iss, issuer)) return(NULL)

  payload
}

extract_email_from_claims <- function(claims) {
  if (is.null(claims)) return(NULL)
  for (field in c("email", "primary_email_address", "email_address")) {
    val <- claims[[field]]
    if (!is.null(val) && nzchar(as.character(val))) {
      return(tolower(trimws(as.character(val))))
    }
  }
  NULL
}

sync_usuario_email <- function(user, email) {
  if (is.null(user) || is.null(user$clerk_id)) return(user)
  email <- tolower(trimws(email %||% ""))
  if (!nzchar(email)) return(user)
  if (identical(tolower(user$email %||% ""), email)) {
    return(apply_pro_grant_email(user, email, user$clerk_id))
  }
  upsert_usuario(user$clerk_id, email)
}

get_usuario_by_clerk_id <- function(clerk_id) {
  df <- db_query(
    "SELECT id, clerk_id, email, tier, activo, is_admin FROM tripulacion WHERE clerk_id = $1 AND activo = TRUE",
    params = list(clerk_id)
  )
  if (nrow(df) == 0) return(NULL)
  as.list(df[1, ])
}

upsert_usuario <- function(clerk_id, email) {
  clerk_id <- trimws(clerk_id %||% "")
  email <- trimws(tolower(email %||% ""))
  if (!nzchar(clerk_id)) return(NULL)
  if (!nzchar(email)) email <- paste0(clerk_id, "@pending.rodaja.cl")

  existing <- get_usuario_by_clerk_id(clerk_id)
  if (!is.null(existing)) {
    if (existing$email != email && !grepl("@pending\\.rodaja\\.cl$", email)) {
      db_execute(
        "UPDATE tripulacion SET email = $2, updated_at = NOW() WHERE clerk_id = $1",
        params = list(clerk_id, email)
      )
      existing$email <- email
    }
    existing <- apply_pro_grant_email(existing, email, clerk_id)
    existing <- apply_admin_grant_email(existing, email, clerk_id)
    return(existing)
  }

  db_execute(
    "INSERT INTO tripulacion (clerk_id, email) VALUES ($1, $2)
     ON CONFLICT (clerk_id) DO UPDATE SET email = EXCLUDED.email, updated_at = NOW(), activo = TRUE",
    params = list(clerk_id, email)
  )
  u <- get_usuario_by_clerk_id(clerk_id)
  u <- apply_pro_grant_email(u, email, clerk_id)
  apply_admin_grant_email(u, email, clerk_id)
}

usuario_es_admin <- function(user) {
  if (is.null(user)) return(FALSE)
  x <- user$is_admin
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(FALSE)
  if (is.logical(x[1])) return(isTRUE(x[1]))
  tolower(as.character(x[1])) %in% c("true", "t", "1")
}

apply_admin_grant_email <- function(user, email, clerk_id) {
  if (is.null(user)) return(NULL)
  grants <- trimws(strsplit(Sys.getenv("GRANT_ADMIN_EMAILS", "jqvargas1000@gmail.com"), ",", fixed = TRUE)[[1]])
  grants <- tolower(grants[nzchar(grants)])
  if (length(grants) && tolower(trimws(email %||% "")) %in% grants && !usuario_es_admin(user)) {
    db_execute(
      "UPDATE tripulacion SET is_admin = TRUE, updated_at = NOW() WHERE clerk_id = $1",
      params = list(clerk_id)
    )
    user$is_admin <- TRUE
  }
  user
}

apply_pro_grant_email <- function(user, email, clerk_id) {
  if (is.null(user)) return(NULL)
  grants <- trimws(strsplit(Sys.getenv("GRANT_PRO_EMAILS", "jqvargas1000@gmail.com"), ",", fixed = TRUE)[[1]])
  grants <- tolower(grants[nzchar(grants)])
  if (length(grants) && tolower(trimws(email %||% "")) %in% grants) {
    db_execute(
      "UPDATE tripulacion SET tier = 'pro', updated_at = NOW() WHERE clerk_id = $1",
      params = list(clerk_id)
    )
    user$tier <- "pro"
  }
  user
}

resolve_usuario_from_claims <- function(claims) {
  clerk_id <- claims$sub %||% ""
  if (!nzchar(clerk_id)) return(NULL)
  email <- extract_email_from_claims(claims)
  upsert_usuario(clerk_id, email)
}

parse_bearer_token <- function(req) {
  auth <- req$HTTP_AUTHORIZATION %||% req$headers[["authorization"]] %||% ""
  if (!nzchar(auth)) return("")
  sub("^Bearer\\s+", "", auth, ignore.case = TRUE)
}

auth_filter <- function(req, res) {
  token <- parse_bearer_token(req)
  if (nzchar(token)) {
    claims <- verify_clerk_jwt(token)
    if (!is.null(claims)) {
      user <- resolve_usuario_from_claims(claims)
      if (!is.null(user)) {
        req$user <- user
        req$clerk_claims <- claims
      }
    }
  }
  if (is.null(req$user)) {
    api_key <- req$HTTP_X_USER_API_KEY %||% req$headers[["x-user-api-key"]] %||% ""
    if (nzchar(api_key)) {
      user <- verify_user_api_key(api_key)
      if (!is.null(user)) req$user <- user
    }
  }
  plumber::forward()
}

require_auth <- function(req, res) {
  if (is.null(req$user)) {
    res$status <- 401
    return(list(error = "Autenticaci\u00f3n requerida"))
  }
  invisible(NULL)
}

require_admin_user <- function(req, res) {
  err <- require_auth(req, res)
  if (!is.null(err)) return(err)
  if (!usuario_es_admin(req$user)) {
    res$status <- 403
    return(list(error = "Se requiere rol de administrador"))
  }
  invisible(NULL)
}

get_tipo_usuario <- function(req = NULL) {
  if (is.null(req) || is.null(req$user)) return("anonimo")
  tier <- req$user$tier %||% "gratuito"
  if (identical(tier, "enterprise")) return("enterprise")
  if (identical(tier, "pro")) return("pro")
  "gratuito"
}

cuota_ancla_usuario <- function(user_id, tier) {
  df <- db_query(cuota_ancla_sql, params = list(user_id, tier %||% "gratuito"))
  if (nrow(df) == 0 || is.na(df$ancla[1])) return(Sys.time())
  df$ancla[1]
}

# Sin user_id no hay ancla: queda el corte de calendario (nadie debería cobrar así).
get_periodo_inicio <- function(tier, user_id = NULL) {
  lim <- get_limite_tier(tier)
  if (!is.null(user_id) && length(user_id) > 0 && !is.na(user_id[1]) && nzchar(as.character(user_id[1]))) {
    return(cuota_periodo_de(cuota_ancla_usuario(user_id, tier), lim$reset)$inicio)
  }
  today <- Sys.Date()
  if (identical(lim$reset, "diario")) {
    today
  } else if (identical(lim$reset, "semanal")) {
    wday <- as.integer(format(today, "%u"))
    today - (wday - 1L)
  } else {
    as.Date(format(today, "%Y-%m-01"))
  }
}

get_periodo_fin <- function(tier, user_id = NULL) {
  lim <- get_limite_tier(tier)
  if (!is.null(user_id) && length(user_id) > 0 && !is.na(user_id[1]) && nzchar(as.character(user_id[1]))) {
    return(cuota_periodo_de(cuota_ancla_usuario(user_id, tier), lim$reset)$fin)
  }
  inicio <- get_periodo_inicio(tier)
  if (identical(lim$reset, "diario")) {
    inicio + 1L
  } else if (identical(lim$reset, "semanal")) {
    inicio + 7L
  } else {
    as.Date(format(inicio + 32, "%Y-%m-01"))
  }
}

get_cuota_usuario <- function(user_id, tier) {
  lim <- get_limite_tier(tier)
  periodo <- get_periodo_inicio(tier, user_id)
  usado <- 0
  if (!is.null(user_id) && nzchar(user_id)) {
    df <- db_query(
      "SELECT bytes_consumidos FROM cupo_del_periodo WHERE user_id = $1::uuid AND periodo_inicio = $2",
      params = list(user_id, periodo)
    )
    if (nrow(df) > 0) usado <- as.numeric(df$bytes_consumidos[1])
  }
  saldo <- if (exists("mb_pack_saldo", mode = "function")) mb_pack_saldo(user_id) else 0
  disp <- max(0, lim$max_bytes - usado) + saldo
  fmt_usado <- format_bytes(usado)
  fmt_limite <- format_bytes(lim$max_bytes)
  fmt_disp <- format_bytes(disp)
  list(
    tier = tier,
    limite_bytes = as.numeric(lim$max_bytes),
    bytes_usados = as.numeric(usado),
    bytes_disponibles = as.numeric(disp),
    bytes_extra_saldo = as.numeric(saldo),
    limite_label = fmt_limite$label,
    usado_label = fmt_usado$label,
    disponible_label = fmt_disp$label,
    periodo_inicio = as.character(periodo),
    reset_en = as.character(get_periodo_fin(tier, user_id)),
    reset_tipo = lim$reset,
    granularidades = lim$granularidades,
    formatos = lim$formatos,
    max_estaciones = lim$max_estaciones
  )
}

reservar_bytes_cuota <- function(user_id, tier, n_bytes) {
  r <- reservar_bytes_descarga(user_id, tier, n_bytes)
  isTRUE(r$ok)
}

liberar_bytes_cuota <- function(user_id, tier, n_bytes, overage_bytes = 0L, extra_bytes = 0L) {
  if (is.null(user_id) || !nzchar(user_id)) return(invisible(NULL))
  n_bytes <- as.integer(n_bytes)
  if (is.na(n_bytes) || n_bytes < 1L) return(invisible(NULL))
  extra_bytes <- as.integer(extra_bytes)
  if (is.na(extra_bytes) || extra_bytes < 0L) extra_bytes <- 0L
  period_release <- max(0L, n_bytes - extra_bytes)
  if (period_release > 0L) {
    periodo <- get_periodo_inicio(tier, user_id)
    db_execute(
      "UPDATE cupo_del_periodo
       SET bytes_consumidos = GREATEST(0, bytes_consumidos - $3)
       WHERE user_id = $1::uuid AND periodo_inicio = $2",
      params = list(user_id, periodo, period_release)
    )
  }
  overage_bytes <- as.integer(overage_bytes)
  if (!is.na(overage_bytes) && overage_bytes > 0L) {
    liberar_bytes_overage(user_id, overage_bytes)
  }
  if (extra_bytes > 0L && exists("mb_pack_credit", mode = "function")) {
    mb_pack_credit(user_id, extra_bytes)
  }
  invisible(NULL)
}

ajustar_cuota_post_job <- function(user_id, tier, estimado, real, overage_est = 0L, overage_real = NULL, extra_est = 0L) {
  if (is.null(user_id) || !nzchar(user_id)) return(invisible(NULL))
  estimado <- as.integer(estimado)
  real <- as.integer(real)
  if (is.na(estimado)) estimado <- 0L
  if (is.na(real)) real <- 0L
  extra_est <- as.integer(extra_est)
  if (is.na(extra_est) || extra_est < 0L) extra_est <- 0L
  if (extra_est > 0L && exists("mb_pack_realize", mode = "function")) {
    overage_est <- as.integer(overage_est)
    if (is.na(overage_est) || overage_est < 0L) overage_est <- 0L
    incluido_est <- max(0L, estimado - extra_est - overage_est)
    got <- mb_pack_realize(incluido_est, extra_est, overage_est, real)
    diff <- as.integer(round(got$period_delta))
    overage_real <- as.integer(round(got$overage))
    extra_delta <- as.integer(round(got$extra_delta))
    if (extra_delta > 0L) mb_pack_debit(user_id, extra_delta)
    if (extra_delta < 0L) mb_pack_credit(user_id, abs(extra_delta))
  } else {
    diff <- real - estimado
  }
  if (diff != 0L) {
    periodo <- get_periodo_inicio(tier, user_id)
    if (diff > 0L) {
      db_execute(
        "INSERT INTO cupo_del_periodo (user_id, periodo_inicio, bytes_consumidos, registros_consumidos)
         VALUES ($1::uuid, $2, $3, 0)
         ON CONFLICT (user_id, periodo_inicio)
         DO UPDATE SET bytes_consumidos = cupo_del_periodo.bytes_consumidos + $3",
        params = list(user_id, periodo, diff)
      )
    } else {
      db_execute(
        "UPDATE cupo_del_periodo
         SET bytes_consumidos = GREATEST(0, bytes_consumidos + $3)
         WHERE user_id = $1::uuid AND periodo_inicio = $2",
        params = list(user_id, periodo, diff)
      )
    }
  }

  overage_est <- as.integer(overage_est)
  if (is.na(overage_est)) overage_est <- 0L
  if (is.null(overage_real)) {
    if (diff == 0L) return(invisible(NULL))
    lim <- get_limite_tier(tier)
    usado <- 0
    periodo <- get_periodo_inicio(tier, user_id)
    df_u <- db_query(
      "SELECT bytes_consumidos FROM cupo_del_periodo WHERE user_id = $1::uuid AND periodo_inicio = $2",
      params = list(user_id, periodo)
    )
    if (nrow(df_u) > 0) usado <- as.numeric(df_u$bytes_consumidos[1])
    u0 <- usado - real
    overage_real <- as.integer(on_demand_overage_increment(u0, lim$max_bytes, real))
  } else {
    overage_real <- as.integer(overage_real)
  }
  if (diff == 0L && overage_est == overage_real) return(invisible(NULL))
  ajustar_overage_post_job(user_id, tier, overage_est, overage_real)
  try_charge_on_demand(user_id)
  invisible(NULL)
}

# Compatibilidad temporal (deprecated)
reservar_registros_cuota <- function(user_id, tier, n) reservar_bytes_cuota(user_id, tier, n)
liberar_registros_cuota <- function(user_id, tier, n) liberar_bytes_cuota(user_id, tier, n)

resolve_user_tier_by_id <- function(user_id) {
  df <- db_query("SELECT tier FROM tripulacion WHERE id = $1::uuid AND activo = TRUE", params = list(user_id))
  if (nrow(df) == 0) return("gratuito")
  tier <- df$tier[1]
  if (identical(tier, "enterprise")) return("enterprise")
  if (identical(tier, "pro")) return("pro")
  "gratuito"
}

get_user_id <- function(req) {
  if (is.null(req) || is.null(req$user)) return(NULL)
  req$user$id %||% NULL
}

verify_svix_webhook <- function(body_raw, headers, secret) {
  secret <- trimws(secret %||% "")
  if (!nzchar(secret) || !nzchar(body_raw)) return(FALSE)

  svix_id <- headers[["svix-id"]] %||% headers[["Svix-Id"]] %||% ""
  svix_ts <- headers[["svix-timestamp"]] %||% headers[["Svix-Timestamp"]] %||% ""
  svix_sig <- headers[["svix-signature"]] %||% headers[["Svix-Signature"]] %||% ""
  if (!nzchar(svix_id) || !nzchar(svix_ts) || !nzchar(svix_sig)) return(FALSE)

  ts_num <- suppressWarnings(as.numeric(svix_ts))
  if (is.na(ts_num) || abs(as.numeric(Sys.time()) - ts_num) > 300) return(FALSE)

  key_b64 <- sub("^whsec_", "", secret)
  key_raw <- tryCatch(base64enc::base64decode(key_b64), error = function(e) NULL)
  if (is.null(key_raw)) return(FALSE)

  signed <- paste(svix_id, svix_ts, body_raw, sep = ".")
  expected <- digest::hmac(key_raw, signed, algo = "sha256", raw = TRUE)

  parts <- strsplit(svix_sig, "\\s+")[[1]]
  for (part in parts) {
    kv <- strsplit(part, ",", fixed = TRUE)[[1]]
    if (length(kv) != 2 || kv[[1]] != "v1") next
    actual <- tryCatch(base64enc::base64decode(kv[[2]]), error = function(e) NULL)
    if (!is.null(actual) && identical(as.raw(expected), as.raw(actual))) return(TRUE)
  }
  FALSE
}

handle_clerk_webhook_event <- function(event_type, data) {
  obj <- data$object %||% data
  clerk_id <- obj$id %||% ""
  if (!nzchar(clerk_id)) return(invisible(NULL))

  if (event_type == "user.deleted") {
    db_execute("UPDATE tripulacion SET activo = FALSE, updated_at = NOW() WHERE clerk_id = $1", params = list(clerk_id))
    return(invisible(NULL))
  }

  emails <- obj$email_addresses %||% list()
  email <- NULL
  if (length(emails) > 0) {
    primary_id <- obj$primary_email_address_id %||% ""
    for (e in emails) {
      if (nzchar(primary_id) && identical(e$id, primary_id)) {
        email <- e$email_address
        break
      }
    }
    if (is.null(email) && !is.null(emails[[1]]$email_address)) {
      email <- emails[[1]]$email_address
    }
  }
  if (is.null(email) && !is.null(obj$email_addresses) && length(obj$email_addresses) > 0) {
    email <- obj$email_addresses[[1]]
  }

  upsert_usuario(clerk_id, email %||% paste0(clerk_id, "@pending.rodaja.cl"))
  invisible(NULL)
}

verify_user_api_key <- function(api_key) {
  key <- trimws(api_key %||% "")
  if (!nzchar(key)) return(NULL)
  key_hash <- digest::digest(key, algo = "sha256")
  df <- db_query(
    "SELECT u.id, u.email, u.tier, u.clerk_id, u.activo
     FROM llave_de_puerta k
     JOIN tripulacion u ON u.id = k.user_id
     WHERE k.key_hash = $1 AND k.activo = TRUE AND u.activo = TRUE AND u.tier = 'enterprise'",
    params = list(key_hash)
  )
  if (nrow(df) == 0) return(NULL)
  db_execute("UPDATE llave_de_puerta SET last_used_at = NOW() WHERE key_hash = $1", params = list(key_hash))
  as.list(df[1, ])
}
