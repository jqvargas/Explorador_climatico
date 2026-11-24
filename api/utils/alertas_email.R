# Envio SMTP para prueba de alertas (API). Requiere emayili + SMTP_* en .env.

alertas_smtp_client <- function() {
  host <- Sys.getenv("SMTP_HOST")
  if (is.na(host) || !nzchar(trimws(host))) return(NULL)
  if (!requireNamespace("emayili", quietly = TRUE)) return(NULL)
  port <- as.integer(Sys.getenv("SMTP_PORT", "587"))
  args <- list(
    host = host,
    port = port,
    username = Sys.getenv("SMTP_USER"),
    password = Sys.getenv("SMTP_PASS"),
    reuse = TRUE
  )
  if (port == 465L) args$protocol <- "smtps"
  do.call(emayili::server, args)
}

build_alerta_prueba_email <- function(email_destino, app_url = "https://app.rodaja.cl") {
  base <- sub("/$", "", trimws(app_url %||% "https://app.rodaja.cl"))
  panel_url <- sprintf("%s/alertas/", base)
  fecha_txt <- format(Sys.Date(), "%d %b %Y")

  subject <- "Prueba de alerta \u2014 rodaja.cl"
  ff <- "font-family:DM Sans,Segoe UI,sans-serif;"

  text <- paste0(
    "PRUEBA DE ALERTA \u2014 rodaja.cl\n\n",
    "Este es un correo de prueba enviado a ", email_destino, ".\n",
    "Si recibes este mensaje, la configuraci\u00f3n de alertas y el env\u00edo por correo funcionan.\n\n",
    "[Confirmada] Lluvia extrema en Estaci\u00f3n de ejemplo\n",
    "   24,5 mm \u2014 la mayor lluvia registrada en esta estaci\u00f3n\n",
    "   ", fecha_txt, "\n\n",
    "Las alertas reales llegan en un solo correo al d\u00eda: previstas (pron\u00f3stico) y confirmadas (lo ya observado).\n\n",
    "Panel de alertas: ", panel_url, "\n"
  )

  html <- paste0(
    '<!DOCTYPE html><html><head><meta charset="utf-8"></head>',
    '<body style="margin:0;padding:0;background:#F5F6F8;">',
    '<table role="presentation" width="100%" cellspacing="0" cellpadding="0" style="background:#F5F6F8;padding:24px 12px;">',
    '<tr><td align="center">',
    '<table role="presentation" width="100%" cellspacing="0" cellpadding="0" style="max-width:640px;background:#ffffff;border:1px solid #E6E8EC;border-radius:16px;">',
    '<tr><td style="padding:16px 20px;border-bottom:1px solid #E6E8EC;">',
    '<span style="', ff, 'font-size:13px;color:#5B6474;">rodaja.cl</span>',
    '<span style="', ff, 'font-size:13px;color:#9A5F10;font-weight:600;"> \u00b7 prueba de alerta</span>',
    '</td></tr>',
    '<tr><td style="padding:22px 20px 8px;">',
    '<div style="', ff, 'font-size:26px;font-weight:700;color:#232A36;letter-spacing:-0.02em;">Correo de prueba</div>',
    '<div style="', ff, 'font-size:14.5px;color:#5B6474;margin-top:4px;">', email_destino, '</div>',
    '<p style="', ff, 'font-size:14.5px;line-height:1.5;color:#232A36;margin:14px 0 0;">',
    'Si ves este mensaje, tu configuraci\u00f3n de alertas y el env\u00edo por correo est\u00e1n operativos. El ejemplo es simulado.</p>',
    '</td></tr>',
    '<tr><td style="padding:8px 16px 10px;">',
    '<table role="presentation" width="100%" cellspacing="0" cellpadding="0" style="background:#ffffff;border:1px solid #E6E8EC;border-radius:14px;"><tr>',
    '<td width="44" valign="top" style="padding:16px 0 16px 16px;">',
    '<div style="width:44px;height:44px;line-height:44px;text-align:center;background:#E6F0F8;border-radius:12px;font-size:20px;color:#2E6FA7;">&#9730;</div></td>',
    '<td valign="top" style="padding:16px 12px;">',
    '<div style="', ff, 'font-size:15.5px;font-weight:700;color:#232A36;">Lluvia extrema en Estaci\u00f3n de ejemplo ',
    '<span style="display:inline-block;font-size:11px;font-weight:700;letter-spacing:0.03em;text-transform:uppercase;padding:2px 9px;border-radius:99px;background:#E7F4EC;color:#2E8B57;vertical-align:middle;">Confirmada</span></div>',
    '<div style="', ff, 'font-size:13.5px;color:#232A36;margin-top:8px;">24,5 mm \u2014 la mayor lluvia registrada en esta estaci\u00f3n</div>',
    '</td>',
    '<td valign="top" align="right" style="padding:16px 16px 16px 0;white-space:nowrap;">',
    '<div style="', ff, 'font-size:12.5px;color:#5B6474;">', fecha_txt, '</div>',
    '</td></tr></table></td></tr>',
    '<tr><td style="padding:8px 24px 20px;text-align:center;">',
    '<a href="', panel_url, '" style="display:inline-block;background:#9A5F10;color:#ffffff;', ff,
    'font-size:14px;font-weight:700;text-decoration:none;padding:12px 20px;border-radius:10px;">Panel de alertas \u2192</a>',
    '</td></tr>',
    '<tr><td style="padding:0 24px 20px;text-align:center;">',
    '<p style="', ff, 'font-size:12px;color:#5B6474;margin:0;">Solo puedes enviar una prueba por d\u00eda. Las alertas reales llegan en un solo correo al d\u00eda.</p>',
    '</td></tr>',
    '</table></td></tr></table></body></html>'
  )

  list(subject = subject, text = text, html = html, panel_url = panel_url)
}

enviar_alerta_prueba <- function(email_destino, app_url = "https://app.rodaja.cl") {
  email_destino <- trimws(email_destino %||% "")
  if (!nzchar(email_destino)) return(list(ok = FALSE, error = "email vac\u00edo"))
  if (exists("correo_rebote_esta_bloqueado", mode = "function") &&
      isTRUE(correo_rebote_esta_bloqueado(email_destino))) {
    message("[alertas:prueba] omitido (correo bloqueado por rebotes) to=", email_destino)
    err <- if (exists("correo_rebote_error_bloqueado", mode = "function")) correo_rebote_error_bloqueado() else "correo bloqueado por rebotes"
    return(list(ok = FALSE, error = err))
  }
  host <- Sys.getenv("SMTP_HOST")
  if (!nzchar(trimws(host))) {
    return(list(ok = FALSE, error = "SMTP no configurado o emayili no disponible"))
  }
  payload <- build_alerta_prueba_email(email_destino, app_url)

  # ponytail: el envio SMTP (curl/emayili) hace segfault dentro del loop de
  # plumber; se aisla en un proceso hijo Rscript. Ceiling: bloquea el request
  # ~5-7s (aceptable para un boton manual). Upgrade path: cola Redis al worker.
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(jsonlite::toJSON(
    list(to = email_destino, subject = payload$subject,
         text = payload$text, html = payload$html),
    auto_unbox = TRUE
  ), tmp)

  script <- file.path(Sys.getenv("API_ROOT", "/app"), "scripts", "send_email_child.R")
  out <- suppressWarnings(system2("Rscript", c(script, tmp), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L

  if (identical(as.integer(status), 0L) && any(grepl("^OK", out))) {
    message("[alertas:prueba] SMTP ok (hijo) to=", email_destino)
    return(list(ok = TRUE, error = NULL))
  }
  err_lines <- out[grepl("ERROR", out)]
  err <- if (length(err_lines)) sub("^ERROR:\\s*", "", paste(err_lines, collapse = "; ")) else ""
  if (!nzchar(err)) err <- paste0("fallo envio de correo (status ", status, ")")
  list(ok = FALSE, error = err)
}
