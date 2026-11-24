#* @post /webhooks/clerk
function(req, res) {
  secret <- Sys.getenv("CLERK_WEBHOOK_SECRET", "")
  if (!nzchar(secret)) {
    res$status <- 503
    return(list(error = "Webhook no configurado"))
  }

  body_raw <- req$postBody %||% ""
  if (!verify_svix_webhook(body_raw, req$headers, secret)) {
    res$status <- 401
    return(list(error = "Firma webhook inv\u00e1lida"))
  }

  payload <- tryCatch(jsonlite::fromJSON(body_raw, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(payload)) {
    res$status <- 400
    return(list(error = "JSON inv\u00e1lido"))
  }

  event_type <- payload$type %||% ""
  data <- payload$data %||% list()
  tryCatch(handle_clerk_webhook_event(event_type, data), error = function(e) {
    message("[webhook clerk] error: ", conditionMessage(e))
  })

  list(received = TRUE)
}

#* @post /webhooks/stripe
function(req, res) {
  secret <- stripe_webhook_secret()
  if (!nzchar(secret)) {
    res$status <- 503
    return(list(error = "Stripe webhook no configurado", code = "not_configured"))
  }

  body_raw <- req$postBody %||% ""
  sig <- req$HTTP_STRIPE_SIGNATURE %||% req$headers[["stripe-signature"]] %||% ""
  if (!verify_stripe_webhook(body_raw, sig, secret)) {
    res$status <- 400
    return(list(error = "Firma webhook inv\u00e1lida"))
  }

  event <- tryCatch(jsonlite::fromJSON(body_raw, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(event) || is.null(event$id)) {
    res$status <- 400
    return(list(error = "JSON inv\u00e1lido"))
  }

  # Idempotencia basica (la tabla de eventos es opcional)
  has_events_table <- nrow(db_query(
    "SELECT 1 FROM information_schema.tables WHERE table_name = 'golpe_de_stripe' LIMIT 1"
  )) > 0L
  if (has_events_table) {
    seen <- db_query("SELECT 1 FROM golpe_de_stripe WHERE event_id = $1", params = list(event$id))
    if (nrow(seen) > 0) return(list(received = TRUE, duplicate = TRUE))
  }

  tryCatch(stripe_handle_webhook_event(event), error = function(e) {
    message("[webhook stripe] error: ", conditionMessage(e))
  })

  if (has_events_table) {
    tryCatch(
      db_execute(
        "INSERT INTO golpe_de_stripe (event_id, event_type) VALUES ($1, $2) ON CONFLICT DO NOTHING",
        params = list(event$id, event$type %||% "")
      ),
      error = function(e) NULL
    )
  }

  list(received = TRUE)
}

#* @post /webhooks/flow
function(req, res) {
  if (!flow_billing_configured()) {
    res$status <- 503
    return(list(error = "Flow webhook no configurado", code = "not_configured"))
  }

  body_raw <- req$postBody %||% ""
  token <- flow_parse_form_token(body_raw)
  if (!nzchar(token %||% "")) {
    res$status <- 400
    return(list(error = "token requerido"))
  }

  if (flow_webhook_seen(token)) {
    return(list(received = TRUE, duplicate = TRUE))
  }

  tryCatch(flow_handle_payment_token(token), error = function(e) {
    message("[webhook flow] error: ", conditionMessage(e))
  })

  list(received = TRUE)
}

#* @post /webhooks/flow/register
function(req, res) {
  if (!flow_billing_configured()) {
    res$status <- 503
    return(list(error = "Flow webhook no configurado", code = "not_configured"))
  }

  body_raw <- req$postBody %||% ""
  token <- flow_parse_form_token(body_raw)
  if (!nzchar(token %||% "")) {
    res$status <- 400
    return(list(error = "token requerido"))
  }

  result <- tryCatch(
    flow_process_register_callback(token),
    error = function(e) {
      message("[webhook flow/register] error: ", conditionMessage(e))
      list(ok = FALSE, redirect_url = flow_exito_url())
    }
  )
  flow_redirect_response(res, result$redirect_url %||% flow_exito_url())
  ""
}
