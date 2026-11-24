#* @apiTitle Billing (Flow Chile)

#* Crear checkout Flow para suscripcion Pro
#* @post /billing/checkout
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)

  if (!flow_billing_configured()) {
    res$status <- 503
    return(list(
      error = "Flow no configurado. Agrega FLOW_API_KEY, FLOW_SECRET_KEY y FLOW_PLAN_PRO_MONTHLY en .env",
      code = "flow_not_configured"
    ))
  }

  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  plan <- tolower(trimws(body$plan %||% "pro"))
  if (!identical(plan, "pro")) {
    res$status <- 400
    return(list(error = "Solo el plan Pro tiene checkout autom\u00e1tico. Institucional: contactar ventas.", code = "invalid_plan"))
  }

  plan_id <- flow_plan_id_for_plan(plan)
  if (!nzchar(plan_id)) {
    res$status <- 503
    return(list(
      error = "Falta FLOW_PLAN_PRO_MONTHLY en .env. Crea el plan rodaja_pro_mensual en Flow sandbox primero.",
      code = "price_not_configured"
    ))
  }

  user_id <- get_user_id(req)
  email <- trimws(req$user$email %||% "")
  if (!nzchar(email) || grepl("@pending\\.rodaja\\.cl$", email)) {
    res$status <- 400
    return(list(error = "Tu cuenta necesita un email v\u00e1lido antes de suscribirte.", code = "email_required"))
  }

  name <- sub("@.*$", "", email)
  if (!nzchar(name)) name <- "Usuario rodaja"

  # GRANT_PRO_EMAILS: Pro de cortesía. No crear cliente ni cobro en Flow.
  grants <- tolower(trimws(strsplit(Sys.getenv("GRANT_PRO_EMAILS", ""), ",", fixed = TRUE)[[1]]))
  grants <- grants[nzchar(grants)]
  if (length(grants) && tolower(email) %in% grants) {
    return(list(url = flow_exito_url(), step = "already_active"))
  }

  checkout <- tryCatch(
    flow_checkout_for_user(user_id, email, name),
    error = function(e) {
      message("[billing] checkout error: ", conditionMessage(e))
      list(error = conditionMessage(e))
    }
  )

  if (!is.null(checkout$error)) {
    res$status <- if (grepl("suscripci\u00f3n Pro activa", checkout$error, ignore.case = TRUE)) 409 else 502
    return(list(error = checkout$error, code = "flow_error"))
  }
  if (is.null(checkout$url) || !nzchar(checkout$url)) {
    res$status <- 502
    return(list(error = "No se pudo crear la sesi\u00f3n de pago en Flow", code = "flow_error"))
  }

  list(
    url = checkout$url,
    step = checkout$step %||% NULL,
    subscription_id = checkout$subscription_id %||% NULL
  )
}

#* Estado de configuracion Flow (sin secretos)
#* @get /billing/config
function(req, res) {
  list(
    provider = "flow",
    checkout_enabled = flow_billing_configured(),
    env = flow_env(),
    plans = list(
      pro = list(
        tier = "pro",
        label = "Pro",
        checkout = TRUE,
        price_configured = nzchar(flow_plan_id_for_plan("pro")),
        plan_id = flow_plan_id_for_plan("pro")
      ),
      institucional = list(
        tier = "enterprise",
        label = "Institucional",
        checkout = FALSE
      )
    ),
    webhook_required = TRUE,
    note = "El tier Pro se activa cuando Flow confirma el pago via webhook /webhooks/flow"
  )
}

#* Cancelar suscripcion Pro (Flow)
#* @post /billing/cancel
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)

  if (!flow_billing_configured()) {
    res$status <- 503
    return(list(error = "Flow no configurado", code = "flow_not_configured"))
  }

  user_id <- get_user_id(req)
  row <- flow_get_user_billing_row(user_id)
  if (is.null(row)) {
    res$status <- 404
    return(list(error = "Usuario no encontrado"))
  }

  sub_id <- trimws(row$stripe_subscription_id %||% "")
  if (!nzchar(sub_id)) {
    res$status <- 400
    return(list(error = "No tienes una suscripci\u00f3n Flow activa para cancelar", code = "no_subscription"))
  }

  tryCatch(
    flow_cancel_subscription(sub_id),
    error = function(e) {
      message("[billing] cancel error: ", conditionMessage(e))
    }
  )

  flow_set_user_tier(
    user_id,
    "gratuito",
    customer_id = row$stripe_customer_id,
    subscription_id = sub_id,
    subscription_status = "canceled"
  )

  list(ok = TRUE, canceled = TRUE, subscription_id = sub_id)
}

#* Return URL tras inscripcion de tarjeta Flow (POST del browser con token)
#* @post /billing/flow/register-return
function(req, res) {
  if (!flow_billing_configured()) {
    res$status <- 503
    flow_redirect_response(res, flow_exito_url())
    return("")
  }

  body_raw <- req$postBody %||% ""
  token <- flow_parse_form_token(body_raw)
  if (!nzchar(token %||% "")) {
    res$status <- 400
    flow_redirect_response(res, flow_exito_url())
    return("")
  }

  result <- tryCatch(
    flow_process_register_callback(token),
    error = function(e) {
      message("[billing] register-return error: ", conditionMessage(e))
      list(ok = FALSE, redirect_url = flow_exito_url())
    }
  )
  flow_redirect_response(res, result$redirect_url %||% flow_exito_url())
  ""
}

#* Medio de pago (tarjeta) del usuario en Flow
#* @get /me/payment-method
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)

  configured <- flow_billing_configured()
  uid <- get_user_id(req)
  row <- flow_get_user_billing_row(uid)
  od <- if (exists("on_demand_get_user_billing", mode = "function")) on_demand_get_user_billing(uid) else NULL

  tier <- if (!is.null(row)) tolower(trimws(row$tier %||% "gratuito")) else "gratuito"
  sub_id <- if (!is.null(row)) trimws(row$stripe_subscription_id %||% "") else ""
  subscription_active <- identical(tier, "pro") && nzchar(sub_id)
  on_demand_enabled <- !is.null(od) && isTRUE(od$on_demand_enabled)

  cid <- if (!is.null(row)) trimws(row$stripe_customer_id %||% "") else ""
  card <- list(has_card = FALSE, brand = "", last4 = "")
  if (configured && flow_customer_id_looks_valid(cid)) {
    customer <- tryCatch(flow_get_customer(cid), error = function(e) NULL)
    card <- flow_customer_card_info(customer)
  }

  list(
    configured = configured,
    provider = "flow",
    has_customer = nzchar(cid),
    has_card = isTRUE(card$has_card),
    brand = card$brand,
    last4 = card$last4,
    subscription_active = subscription_active,
    on_demand_enabled = on_demand_enabled,
    can_delete = isTRUE(card$has_card) && !subscription_active && !on_demand_enabled
  )
}

#* Registrar/cambiar tarjeta (redireccion a Flow)
#* @post /me/payment-method/register
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)

  if (!flow_billing_configured()) {
    res$status <- 503
    return(list(error = "Flow no configurado", code = "flow_not_configured"))
  }

  uid <- get_user_id(req)
  email <- trimws(req$user$email %||% "")
  if (!nzchar(email) || grepl("@pending\\.rodaja\\.cl$", email)) {
    res$status <- 400
    return(list(error = "Tu cuenta necesita un email v\u00e1lido antes de registrar una tarjeta.", code = "email_required"))
  }
  name <- sub("@.*$", "", email)
  if (!nzchar(name)) name <- "Usuario rodaja"

  out <- tryCatch({
    ensured <- flow_ensure_customer(uid, email, name)
    flow_register_customer(ensured$customer_id, flow_card_return_url())
  }, error = function(e) {
    message("[billing] payment-method/register error: ", conditionMessage(e))
    list(error = conditionMessage(e))
  })

  if (!is.null(out$error)) {
    res$status <- 502
    return(list(error = out$error, code = "flow_error"))
  }
  if (is.null(out$url) || !nzchar(out$url)) {
    res$status <- 502
    return(list(error = "No se pudo generar el registro de tarjeta en Flow", code = "flow_error"))
  }
  list(url = out$url)
}

#* Eliminar tarjeta registrada en Flow
#* @post /me/payment-method/remove
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)

  if (!flow_billing_configured()) {
    res$status <- 503
    return(list(error = "Flow no configurado", code = "flow_not_configured"))
  }

  uid <- get_user_id(req)
  row <- flow_get_user_billing_row(uid)
  od <- if (exists("on_demand_get_user_billing", mode = "function")) on_demand_get_user_billing(uid) else NULL

  tier <- if (!is.null(row)) tolower(trimws(row$tier %||% "gratuito")) else "gratuito"
  sub_id <- if (!is.null(row)) trimws(row$stripe_subscription_id %||% "") else ""
  if (identical(tier, "pro") && nzchar(sub_id)) {
    res$status <- 409
    return(list(error = "Cancela tu suscripci\u00f3n Pro antes de eliminar la tarjeta.", code = "subscription_active"))
  }
  if (!is.null(od) && isTRUE(od$on_demand_enabled)) {
    res$status <- 409
    return(list(error = "Desactiva pago por uso antes de eliminar la tarjeta.", code = "on_demand_active"))
  }

  cid <- if (!is.null(row)) trimws(row$stripe_customer_id %||% "") else ""
  if (!flow_customer_id_looks_valid(cid)) {
    res$status <- 404
    return(list(error = "No tienes una tarjeta registrada.", code = "no_card"))
  }

  ok <- tryCatch({
    flow_unregister_customer_card(cid)
    TRUE
  }, error = function(e) {
    message("[billing] payment-method/remove error: ", conditionMessage(e))
    conditionMessage(e)
  })

  if (!isTRUE(ok)) {
    res$status <- 502
    return(list(error = if (is.character(ok)) ok else "No se pudo eliminar la tarjeta", code = "flow_error"))
  }
  list(ok = TRUE, removed = TRUE)
}

#* Packs de MB de compra única (no se renuevan)
#* @get /me/mb-packs
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  mb_pack_payload(get_user_id(req))
}

#* Checkout Flow de un pack de MB
#* @post /billing/mb-pack
function(req, res) {
  auth_err <- require_auth(req, res)
  if (!is.null(auth_err)) return(auth_err)
  if (!flow_billing_configured()) {
    res$status <- 503
    return(list(error = "Flow no configurado", code = "flow_not_configured"))
  }
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  pack_id <- trimws(body$pack_id %||% "")
  if (is.null(mb_pack_get(pack_id))) {
    res$status <- 400
    return(list(error = "Elige un pack de MB válido", code = "invalid_pack"))
  }
  email <- trimws(req$user$email %||% "")
  if (!nzchar(email) || grepl("@pending\\.rodaja\\.cl$", email)) {
    res$status <- 400
    return(list(error = "Tu cuenta necesita un email válido antes de comprar.", code = "email_required"))
  }
  out <- tryCatch(
    mb_pack_checkout(get_user_id(req), email, pack_id),
    error = function(e) {
      message("[billing] mb-pack error: ", conditionMessage(e))
      list(error = conditionMessage(e))
    }
  )
  if (!is.null(out$error)) {
    res$status <- 502
    return(list(error = out$error, code = "flow_error"))
  }
  list(url = out$url, pack_id = out$pack_id, clp = out$clp)
}

#* Vuelta del navegador tras pagar un pack (Flow hace POST con token)
#* @post /billing/flow/mb-return
function(req, res) {
  ok_url <- paste0(flow_app_url(), "/cuenta/?mb=ok")
  bad_url <- paste0(flow_app_url(), "/cuenta/?mb=error")
  if (!flow_billing_configured()) {
    flow_redirect_response(res, bad_url)
    return("")
  }
  token <- flow_parse_form_token(req$postBody %||% "")
  if (!nzchar(token %||% "")) {
    flow_redirect_response(res, bad_url)
    return("")
  }
  payment <- tryCatch(flow_payment_get_status(token), error = function(e) NULL)
  commerce <- if (!is.null(payment)) payment$commerceOrder %||% "" else ""
  if (!is.null(payment) && flow_payment_is_paid(payment)) {
    mb_pack_mark_paid(commerce, token)
    flow_redirect_response(res, ok_url)
  } else {
    flow_redirect_response(res, bad_url)
  }
  ""
}

#* Return URL tras registrar/cambiar tarjeta (POST del browser con token)
#* @post /billing/flow/card-return
function(req, res) {
  card_url <- paste0(flow_app_url(), "/cuenta/?card=updated")
  if (!flow_billing_configured()) {
    flow_redirect_response(res, card_url)
    return("")
  }
  token <- flow_parse_form_token(req$postBody %||% "")
  if (nzchar(token %||% "")) {
    reg <- tryCatch(flow_get_register_status(token), error = function(e) NULL)
    if (is.null(reg) || as.character(reg$status %||% "") != "1") {
      card_url <- paste0(flow_app_url(), "/cuenta/?card=error")
    }
  } else {
    card_url <- paste0(flow_app_url(), "/cuenta/?card=error")
  }
  flow_redirect_response(res, card_url)
  ""
}
