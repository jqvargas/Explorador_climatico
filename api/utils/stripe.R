# Stripe REST helpers (httr + env vars). Secret key solo en backend.

stripe_secret_key <- function() {
  trimws(Sys.getenv("STRIPE_SECRET_KEY", ""))
}

stripe_webhook_secret <- function() {
  trimws(Sys.getenv("STRIPE_WEBHOOK_SECRET", ""))
}

stripe_app_url <- function() {
  url <- trimws(Sys.getenv("APP_PUBLIC_URL", "https://app.rodaja.cl"))
  sub("/+$", "", url)
}

stripe_price_id_for_plan <- function(plan) {
  plan <- tolower(trimws(plan %||% ""))
  switch(plan,
    pro = trimws(Sys.getenv("STRIPE_PRICE_PRO_MONTHLY", "")),
    ""
  )
}

stripe_tier_for_price_id <- function(price_id) {
  price_id <- trimws(price_id %||% "")
  if (!nzchar(price_id)) return(NULL)
  candidates <- list(
    pro = c(
      trimws(Sys.getenv("STRIPE_PRICE_PRO_MONTHLY", "")),
      trimws(Sys.getenv("STRIPE_PRICE_PRO_YEARLY", ""))
    ),
    enterprise = c(
      trimws(Sys.getenv("STRIPE_PRICE_ENTERPRISE_MONTHLY", "")),
      trimws(Sys.getenv("STRIPE_PRICE_ENTERPRISE_YEARLY", ""))
    )
  )
  for (tier in names(candidates)) {
    ids <- candidates[[tier]]
    ids <- ids[nzchar(ids)]
    if (price_id %in% ids) return(tier)
  }
  NULL
}

stripe_api_request <- function(method, path, params = list()) {
  key <- stripe_secret_key()
  if (!nzchar(key)) {
    stop("STRIPE_SECRET_KEY no configurada", call. = FALSE)
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Paquete httr no disponible", call. = FALSE)
  }
  url <- paste0("https://api.stripe.com/v1", path)
  method <- toupper(method)
  resp <- if (method == "GET") {
    httr::GET(url, httr::authenticate(key, ""), query = params, httr::timeout(20))
  } else {
    httr::VERB(method, url, httr::authenticate(key, ""), body = params, encode = "form", httr::timeout(20))
  }
  body <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
  parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
  if (httr::status_code(resp) >= 400) {
    msg <- if (!is.null(parsed$error$message)) parsed$error$message else body
    stop(msg %||% paste("Stripe HTTP", httr::status_code(resp)), call. = FALSE)
  }
  parsed
}

stripe_create_checkout_session <- function(user_id, email, price_id, tier = "pro") {
  app <- stripe_app_url()
  stripe_api_request("POST", "/checkout/sessions", list(
    mode = "subscription",
    "line_items[0][price]" = price_id,
    "line_items[0][quantity]" = 1L,
    success_url = paste0(app, "/precios/exito.html?session_id={CHECKOUT_SESSION_ID}"),
    cancel_url = paste0(app, "/precios/?checkout=cancel"),
    client_reference_id = user_id,
    customer_email = email,
    "metadata[user_id]" = user_id,
    "metadata[tier]" = tier,
    "subscription_data[metadata][user_id]" = user_id,
    "subscription_data[metadata][tier]" = tier,
    allow_promotion_codes = "true"
  ))
}

verify_stripe_webhook <- function(body_raw, sig_header, secret) {
  secret <- trimws(secret %||% "")
  body_raw <- body_raw %||% ""
  sig_header <- sig_header %||% ""
  if (!nzchar(secret) || !nzchar(body_raw) || !nzchar(sig_header)) return(FALSE)

  parts <- strsplit(sig_header, ",", fixed = FALSE)[[1]]
  timestamp <- NULL
  signatures <- character(0)
  for (part in parts) {
    kv <- strsplit(part, "=", fixed = TRUE)[[1]]
    if (length(kv) != 2) next
    if (kv[[1]] == "t") timestamp <- kv[[2]]
    if (kv[[1]] == "v1") signatures <- c(signatures, kv[[2]])
  }
  if (is.null(timestamp) || !length(signatures)) return(FALSE)
  ts_num <- suppressWarnings(as.numeric(timestamp))
  if (is.na(ts_num) || abs(as.numeric(Sys.time()) - ts_num) > 300) return(FALSE)

  key_b64 <- sub("^whsec_", "", secret)
  key_raw <- tryCatch(base64enc::base64decode(key_b64), error = function(e) key_b64)
  signed <- paste0(timestamp, ".", body_raw)
  expected <- digest::hmac(key_raw, signed, algo = "sha256", raw = FALSE)

  for (sig in signatures) {
    if (identical(expected, sig)) return(TRUE)
  }
  FALSE
}

stripe_set_user_tier <- function(user_id, tier, stripe_customer_id = NULL, stripe_subscription_id = NULL, subscription_status = NULL, period_end = NULL) {
  user_id <- trimws(user_id %||% "")
  if (!nzchar(user_id)) return(invisible(NULL))
  tier <- tolower(trimws(tier %||% "gratuito"))
  if (!tier %in% c("gratuito", "pro", "enterprise")) tier <- "gratuito"

  has_stripe_cols <- nrow(db_query(
    "SELECT 1 FROM information_schema.columns WHERE table_name = 'tripulacion' AND column_name = 'stripe_customer_id' LIMIT 1"
  )) > 0L

  if (has_stripe_cols) {
    db_execute(
      "UPDATE tripulacion SET
         tier = $2,
         stripe_customer_id = COALESCE($3, stripe_customer_id),
         stripe_subscription_id = COALESCE($4, stripe_subscription_id),
         subscription_status = COALESCE($5, subscription_status),
         subscription_period_end = COALESCE($6::timestamptz, subscription_period_end),
         updated_at = NOW()
       WHERE id = $1::uuid",
      params = list(
        user_id,
        tier,
        stripe_customer_id,
        stripe_subscription_id,
        subscription_status,
        period_end
      )
    )
  } else {
    db_execute(
      "UPDATE tripulacion SET tier = $2, updated_at = NOW() WHERE id = $1::uuid",
      params = list(user_id, tier)
    )
  }
  invisible(NULL)
}

stripe_user_id_from_metadata <- function(meta) {
  if (is.null(meta)) return(NULL)
  uid <- meta$user_id %||% meta$userId %||% ""
  uid <- trimws(as.character(uid))
  if (nzchar(uid)) return(uid)
  NULL
}

stripe_resolve_user_id <- function(client_reference_id = NULL, customer_id = NULL, metadata = NULL) {
  uid <- stripe_user_id_from_metadata(metadata)
  if (nzchar(uid %||% "")) return(uid)
  uid <- trimws(client_reference_id %||% "")
  if (nzchar(uid)) return(uid)
  cid <- trimws(customer_id %||% "")
  if (!nzchar(cid)) return(NULL)
  has_stripe_cols <- nrow(db_query(
    "SELECT 1 FROM information_schema.columns WHERE table_name = 'tripulacion' AND column_name = 'stripe_customer_id' LIMIT 1"
  )) > 0L
  if (!has_stripe_cols) return(NULL)
  df <- db_query(
    "SELECT id FROM tripulacion WHERE stripe_customer_id = $1 AND activo = TRUE LIMIT 1",
    params = list(cid)
  )
  if (nrow(df) == 0) return(NULL)
  as.character(df$id[1])
}

stripe_tier_from_subscription <- function(subscription) {
  if (is.null(subscription)) return(NULL)
  meta_tier <- subscription$metadata$tier %||% NULL
  if (!is.null(meta_tier) && nzchar(as.character(meta_tier))) {
    tier <- tolower(trimws(as.character(meta_tier)))
    if (tier %in% c("pro", "enterprise")) return(tier)
  }
  items <- subscription$items$data %||% list()
  if (length(items) > 0) {
    price_id <- items[[1]]$price$id %||% items[[1]]$price$ID %||% ""
    mapped <- stripe_tier_for_price_id(price_id)
    if (!is.null(mapped)) return(mapped)
  }
  NULL
}

stripe_handle_webhook_event <- function(event) {
  event_type <- event$type %||% ""
  obj <- event$data$object %||% list()

  if (event_type == "checkout.session.completed") {
    user_id <- stripe_resolve_user_id(
      client_reference_id = obj$client_reference_id,
      customer_id = obj$customer,
      metadata = obj$metadata
    )
    tier <- obj$metadata$tier %||% "pro"
    tier <- tolower(trimws(as.character(tier)))
    if (!tier %in% c("pro", "enterprise")) tier <- "pro"
    if (nzchar(user_id %||% "")) {
      stripe_set_user_tier(
        user_id,
        tier,
        stripe_customer_id = obj$customer,
        stripe_subscription_id = obj$subscription,
        subscription_status = "active"
      )
    }
    return(invisible(NULL))
  }

  if (event_type %in% c("customer.subscription.created", "customer.subscription.updated")) {
    status <- obj$status %||% ""
    user_id <- stripe_resolve_user_id(
      customer_id = obj$customer,
      metadata = obj$metadata
    )
    if (!nzchar(user_id %||% "")) return(invisible(NULL))
    tier <- stripe_tier_from_subscription(obj)
    if (status %in% c("active", "trialing")) {
      stripe_set_user_tier(
        user_id,
        tier %||% "pro",
        stripe_customer_id = obj$customer,
        stripe_subscription_id = obj$id,
        subscription_status = status,
        period_end = if (!is.null(obj$current_period_end)) {
          format(as.POSIXct(as.numeric(obj$current_period_end), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        } else NULL
      )
    } else if (status %in% c("canceled", "unpaid", "incomplete_expired")) {
      stripe_set_user_tier(
        user_id,
        "gratuito",
        stripe_customer_id = obj$customer,
        stripe_subscription_id = obj$id,
        subscription_status = status
      )
    }
    return(invisible(NULL))
  }

  if (event_type == "customer.subscription.deleted") {
    user_id <- stripe_resolve_user_id(customer_id = obj$customer, metadata = obj$metadata)
    if (nzchar(user_id %||% "")) {
      stripe_set_user_tier(
        user_id,
        "gratuito",
        stripe_customer_id = obj$customer,
        stripe_subscription_id = obj$id,
        subscription_status = "canceled"
      )
    }
  }
  invisible(NULL)
}
