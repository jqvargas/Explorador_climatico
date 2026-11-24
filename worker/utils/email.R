#' Cliente SMTP compartido (emayili). Puerto 465 = SMTPS; 587 = STARTTLS.

correo_rebote_boot <- function() {
  if (exists("correo_rebote_esta_bloqueado", mode = "function")) return(invisible(TRUE))
  for (p in c(
    file.path(Sys.getenv("API_ROOT", "/api"), "utils", "correo_rebote.R"),
    "/api/utils/correo_rebote.R"
  )) {
    if (file.exists(p)) {
      source(p, local = FALSE)
      return(invisible(TRUE))
    }
  }
  invisible(FALSE)
}

smtp_client <- function() {
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

#' Envia link de descarga por email usando emayili
#' Si SMTP_HOST esta vacio, no envia (prototipo)
enviar_link_descarga <- function(email_destino, link, job_id) {
  correo_rebote_boot()
  if (exists("correo_rebote_esta_bloqueado", mode = "function") &&
      isTRUE(correo_rebote_esta_bloqueado(email_destino))) {
    message("[descarga] omitido (correo bloqueado por rebotes)")
    return(invisible(NULL))
  }
  smtp <- smtp_client()
  if (is.null(smtp)) return(invisible(NULL))

  msg <- emayili::envelope(
    to = email_destino,
    from = Sys.getenv("SMTP_FROM", "noreply@clima.local"),
    subject = "Tu descarga est\u00e1 lista",
    text = glue::glue("Tu descarga est\u00e1 lista. Enlace: {link}")
  )
  smtp(msg)
  invisible(NULL)
}

#' Formatea valor numerico para alertas (sin depender de reporte.R)
alerta_fmt_valor <- function(x) {
  if (length(x) == 0 || is.na(x)) return("\u2014")
  if (abs(x) >= 1000) {
    formatC(round(x, 0), format = "f", digits = 0, big.mark = ".")
  } else {
    formatC(round(x, 1), format = "f", digits = 1, decimal.mark = ",")
  }
}

alerta_rank_emoji <- function(ranking) {
  r <- as.integer(ranking)
  switch(as.character(r),
    "1" = "\U0001F947",
    "2" = "\U0001F948",
    "3" = "\U0001F949",
    "\U0001F3C5"
  )
}

alerta_variable_emoji <- function(variable_nombre) {
  n <- tolower(as.character(variable_nombre))
  if (grepl("precip|lluvia|\\bpp\\b", n)) return("\U0001F327\U000FE0F")
  if (grepl("temp|term|\\bt_max\\b|\\bt_min\\b", n)) return("\U0001F321\U000FE0F")
  if (grepl("caudal|flow|\\bq\\b|nivel", n)) return("\U0001F4A7")
  if (grepl("viento|wind", n)) return("\U0001F32C\U000FE0F")
  if (grepl("radiacion|rad", n)) return("\u2600\uFE0F")
  "\U0001F4CA"
}

alerta_evento_frase <- function(direccion_label, variable_nombre) {
  n <- tolower(as.character(variable_nombre))
  minimo <- identical(direccion_label, "minimos")
  if (minimo && grepl("precip|lluvia|\\bpp\\b", n)) return("Entre los 10 d\u00edas m\u00e1s secos de la historia")
  if (minimo && grepl("temp|term", n)) return("Entre los 10 d\u00edas m\u00e1s fr\u00edos de la historia")
  if (minimo) return("Entre los 10 m\u00ednimos hist\u00f3ricos diarios")
  if (grepl("precip|lluvia|\\bpp\\b", n)) return("Entre los 10 d\u00edas m\u00e1s lluviosos de la historia")
  if (grepl("temp|term", n)) return("Entre los 10 d\u00edas m\u00e1s c\u00e1lidos de la historia")
  if (grepl("caudal|flow|\\bq\\b", n)) return("Entre los 10 d\u00edas de mayor caudal registrados")
  if (grepl("nivel", n)) return("Entre los 10 d\u00edas de mayor nivel registrados")
  "Entre los 10 m\u00e1ximos hist\u00f3ricos diarios"
}

#' Arma subject + cuerpo texto/HTML de alerta top 10
build_alerta_top10_email <- function(estacion_nombre, variable_nombre,
                                     fecha_larga, valor, unidad, ranking, n_dias,
                                     direccion_label, eid, variable_id, app_url) {
  base <- sub("/$", "", trimws(app_url %||% "https://app.rodaja.cl"))
  reporte_url <- sprintf("%s/mi-reporte/?e=%s&v=%s", base, eid, variable_id)
  cuenta_url <- sprintf("%s/alertas/", base)
  valor_fmt <- alerta_fmt_valor(valor)
  unidad_txt <- if (nzchar(unidad %||% "")) paste0(" ", unidad) else ""
  rank_emoji <- alerta_rank_emoji(ranking)
  var_emoji <- alerta_variable_emoji(variable_nombre)
  evento_frase <- alerta_evento_frase(direccion_label, variable_nombre)
  rank_color <- if (ranking <= 3L) "#DF9531" else "#378ADD"
  rank_bg <- if (ranking <= 3L) "#FFF3E0" else "#EEF4FC"

  subject <- glue::glue("{rank_emoji} Puesto {ranking} \u2014 {variable_nombre} \u2014 {fecha_larga}")

  text <- glue::glue(
    "{rank_emoji} ALERTA: entre los d\u00edas m\u00e1s extremos \u2014 rodaja.cl\n\n",
    "{evento_frase}\n\n",
    "Puesto #{ranking} de {n_dias} d\u00edas con dato\n",
    "{var_emoji} {variable_nombre}\n",
    "Fecha: {fecha_larga}\n",
    "Valor: {valor_fmt}{unidad_txt}\n",
    "Estaci\u00f3n: {estacion_nombre}\n\n",
    "Ver reporte: {reporte_url}\n",
    "Gestionar alertas: {cuenta_url}\n"
  )

  html <- glue::glue(
    '<!DOCTYPE html><html><head><meta charset="utf-8"></head><body style="margin:0;padding:0;background:#F4F6F8;">',
    '<table role="presentation" width="100%" cellspacing="0" cellpadding="0" style="background:#F4F6F8;padding:24px 12px;">',
    '<tr><td align="center">',
    '<table role="presentation" width="100%" cellspacing="0" cellpadding="0" style="max-width:520px;background:#ffffff;border-radius:16px;overflow:hidden;box-shadow:0 2px 12px rgba(57,69,83,0.08);">',

    '<tr><td style="padding:14px 20px;border-bottom:1px solid #E8ECF0;">',
    '<span style="font-family:DM Sans,Segoe UI,sans-serif;font-size:13px;color:#8C9091;">rodaja.cl</span>',
    '<span style="font-family:DM Sans,Segoe UI,sans-serif;font-size:13px;color:#DF9531;font-weight:600;"> \u00b7 alerta</span>',
    '</td></tr>',

    '<tr><td style="padding:28px 24px 8px;background:{rank_bg};text-align:center;">',
    '<div style="font-size:52px;line-height:1;margin-bottom:8px;">{rank_emoji}</div>',
    '<div style="font-family:DM Sans,Segoe UI,sans-serif;font-size:48px;font-weight:700;color:{rank_color};line-height:1;">#{ranking}</div>',
    '<div style="font-family:DM Sans,Segoe UI,sans-serif;font-size:12px;font-weight:600;color:#8C9091;letter-spacing:0.08em;text-transform:uppercase;margin-top:8px;">Entre los d\u00edas m\u00e1s extremos</div>',
    '</td></tr>',

    '<tr><td style="padding:20px 24px 8px;text-align:center;background:{rank_bg};">',
    '<div style="font-family:DM Sans,Segoe UI,sans-serif;font-size:22px;font-weight:600;color:#394553;line-height:1.3;">',
    '{var_emoji} {variable_nombre}',
    '</div>',
    '<div style="font-family:DM Sans,Segoe UI,sans-serif;font-size:18px;color:#394553;margin-top:10px;">{fecha_larga}</div>',
    '<div style="font-family:DM Sans,Segoe UI,sans-serif;font-size:32px;font-weight:700;color:#394553;margin-top:14px;">',
    '{valor_fmt}<span style="font-size:18px;font-weight:500;color:#8C9091;">{unidad_txt}</span>',
    '</div>',
    '</td></tr>',

    '<tr><td style="padding:8px 24px 24px;text-align:center;background:{rank_bg};">',
    '<p style="font-family:DM Sans,Segoe UI,sans-serif;font-size:15px;line-height:1.5;color:#394553;margin:0;">',
    '{evento_frase}',
    '</p>',
    '<p style="font-family:DM Sans,Segoe UI,sans-serif;font-size:14px;color:#8C9091;margin:12px 0 0;">',
    'Estaci\u00f3n <strong style="color:#394553;">{estacion_nombre}</strong>',
    ' \u00b7 {n_dias} d\u00edas en el registro',
    '</p>',
    '</td></tr>',

    '<tr><td style="padding:24px;text-align:center;">',
    '<a href="{reporte_url}" style="display:inline-block;background:#DF9531;color:#ffffff;font-family:DM Sans,Segoe UI,sans-serif;font-size:15px;font-weight:600;text-decoration:none;padding:14px 28px;border-radius:10px;margin:0 6px 10px;">Ver reporte \u2192</a>',
    '<br>',
    '<a href="{cuenta_url}" style="font-family:DM Sans,Segoe UI,sans-serif;font-size:14px;color:#378ADD;text-decoration:none;">Gestionar alertas</a>',
    '</td></tr>',

    '<tr><td style="padding:0 24px 20px;text-align:center;">',
    '<p style="font-family:DM Sans,Segoe UI,sans-serif;font-size:11px;color:#B0B5BA;margin:0;">Recibes este correo porque activaste alertas en rodaja.cl</p>',
    '</td></tr>',

    '</table></td></tr></table></body></html>'
  )

  list(subject = subject, text = text, html = html, reporte_url = reporte_url, cuenta_url = cuenta_url)
}

#' Email alerta temprana por pronostico (top 10 estimado)
build_alerta_pronostico_email <- function(estacion_nombre, variable_nombre,
                                        fecha_larga, valor, unidad, ranking, n_dias,
                                        direccion_label, eid, variable_id, app_url) {
  base <- sub("/$", "", trimws(app_url %||% "https://app.rodaja.cl"))
  reporte_url <- sprintf("%s/mi-reporte/?e=%s&v=%s&view=pronostico", base, eid, variable_id)
  cuenta_url <- sprintf("%s/cuenta/", base)
  payload <- build_alerta_top10_email(
    estacion_nombre, variable_nombre, fecha_larga, valor, unidad, ranking, n_dias,
    direccion_label, eid, variable_id, app_url
  )
  payload$subject <- sub("Puesto ", "Pron\u00f3stico, puesto ", payload$subject)
  payload$text <- paste0(
    "PRON\u00d3STICO\n\n",
    payload$text,
    "\nVer pron\u00f3stico: ", reporte_url, "\n"
  )
  payload$html <- sub(
    "alerta",
    "pron\u00f3stico",
    payload$html,
    fixed = TRUE
  )
  payload$reporte_url <- reporte_url
  payload$cuenta_url <- cuenta_url
  payload
}

alerta_digest_evento_linea <- function(ev) {
  if (exists("alerta_mensaje_linea", mode = "function")) {
    return(alerta_mensaje_linea(
      ev$estacion_nombre, ev$fecha, ev$variable_nombre, ev$nombre_abr %||% "",
      ev$valor, ev$unidad, ev$ranking, ev$direccion, ev$tipo %||% "tardia"
    ))
  }
  paste0(ev$estacion_nombre, " \u00b7 #", ev$ranking, " ", ev$variable_nombre)
}

alerta_digest_reporte_url <- function(ev, base) {
  if (identical(ev$tipo, "temprana")) {
    return(sprintf("%s/mi-reporte/?e=%s&v=%s&view=pronostico", base, ev$eid, ev$variable_id))
  }
  sprintf("%s/mi-reporte/?e=%s&v=%s", base, ev$eid, ev$variable_id)
}

alerta_digest_tipo_label <- function(tipo) {
  if (identical(tipo, "temprana")) return("Prevista")
  if (identical(tipo, "umbral")) return("Tu umbral")
  "Confirmada"
}

alerta_digest_ensure_helpers <- function() {
  if (exists("alerta_digest_resumen", mode = "function")) return(invisible(NULL))
  for (p in c("/api/utils/alertas_digest.R",
              file.path(Sys.getenv("API_ROOT", ""), "utils", "alertas_digest.R"))) {
    if (nzchar(p) && file.exists(p)) {
      source(p, local = FALSE)
      return(invisible(NULL))
    }
  }
  invisible(NULL)
}

# Misma voz que el panel (digest_view.js): Prevista / Confirmada, sin critica ni tardia.
.alerta_mail_ff <- "font-family:DM Sans,Segoe UI,sans-serif;"

alerta_digest_html_esc <- function(x) {
  x <- as.character(x %||% "")
  if (length(x) == 0 || is.na(x[[1]])) x <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

alerta_digest_palabra <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return("")
  paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}

alerta_digest_estacion_limpia <- function(nombre) {
  s <- trimws(as.character(nombre %||% ""))
  s <- gsub("_(DMC|DGA|INIA)$", "", s, ignore.case = TRUE)
  s <- gsub("_", " ", s, fixed = TRUE)
  s <- trimws(s)
  if (!nzchar(s)) "Estaci\u00f3n" else s
}

alerta_digest_lugar <- function(ev) {
  com <- alerta_digest_palabra(ev$nom_com)
  reg <- alerta_digest_palabra(ev$nom_reg)
  if (nzchar(com) && nzchar(reg)) paste0(com, ", ", reg) else if (nzchar(com)) com else reg
}

alerta_digest_fecha_corta <- function(fecha) {
  d <- suppressWarnings(tryCatch(as.Date(fecha), error = function(e) NA))
  if (length(d) == 0 || is.na(d)) {
    x <- trimws(as.character(fecha %||% ""))
    if (grepl("^\\d{4}-\\d{2}-\\d{2}", x)) d <- as.Date(substr(x, 1, 10))
  }
  if (length(d) == 0 || is.na(d)) return(as.character(fecha %||% ""))
  meses <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
  sprintf("%d %s %d", as.integer(format(d, "%d")), meses[as.integer(format(d, "%m"))], as.integer(format(d, "%Y")))
}

alerta_digest_fenomeno <- function(abr, dir) {
  abr <- tolower(as.character(abr %||% ""))
  dir <- if (identical(as.character(dir %||% "max"), "min")) "min" else "max"
  if (startsWith(abr, "u")) return("wind")
  if (identical(abr, "pp")) return("rain")
  if (abr %in% c("t_min", "t_max")) return(if (identical(dir, "min")) "cold" else "heat")
  "rain"
}

alerta_digest_titulo <- function(abr, dir, ranking) {
  abr <- tolower(as.character(abr %||% ""))
  dir <- if (identical(as.character(dir %||% "max"), "min")) "min" else "max"
  rank <- suppressWarnings(as.integer(ranking))
  soft <- length(rank) == 0 || is.na(rank) || rank > 3L
  if (identical(abr, "pp")) {
    if (identical(dir, "min")) return(if (soft) "Poca lluvia" else "Sequ\u00eda")
    return(if (soft) "Lluvia poco habitual" else "Lluvia extrema")
  }
  if (identical(abr, "t_min") && identical(dir, "min")) return(if (soft) "Fr\u00edo poco habitual" else "Fr\u00edo extremo")
  if (identical(abr, "t_min")) return(if (soft) "Calor poco habitual" else "M\u00ednima muy alta")
  if (identical(abr, "t_max") && identical(dir, "min")) return(if (soft) "M\u00e1xima baja" else "M\u00e1xima muy baja")
  if (identical(abr, "t_max")) return(if (soft) "Calor poco habitual" else "Calor extremo")
  if (startsWith(abr, "u")) return(if (soft) "Viento poco habitual" else "Viento extremo")
  if (soft) "Valor poco habitual" else "Valor extremo"
}

alerta_digest_frase_rank <- function(abr, dir, ranking) {
  rank <- suppressWarnings(as.integer(ranking))
  if (length(rank) == 0 || is.na(rank) || rank < 1L) return("")
  abr <- tolower(as.character(abr %||% ""))
  dir <- if (identical(as.character(dir %||% "max"), "min")) "min" else "max"
  if (rank == 1L && identical(abr, "pp") && identical(dir, "max")) return("la mayor lluvia registrada en esta estaci\u00f3n")
  if (rank == 1L && identical(abr, "t_min") && identical(dir, "min")) return("el d\u00eda m\u00e1s fr\u00edo desde que hay registro")
  if (rank == 1L && identical(abr, "t_max") && identical(dir, "max")) return("el d\u00eda m\u00e1s c\u00e1lido desde que hay registro")
  if (rank == 1L) return("el m\u00e1s extremo desde que hay registro")
  if (identical(abr, "pp") && identical(dir, "max")) return(paste0(rank, "\u00b0 d\u00eda m\u00e1s lluvioso en esta estaci\u00f3n"))
  if ((identical(abr, "t_min") && identical(dir, "min")) || (identical(abr, "t_max") && identical(dir, "min"))) {
    return(paste0("entre los ", rank, " d\u00edas m\u00e1s fr\u00edos desde que hay registro"))
  }
  if (abr %in% c("t_min", "t_max")) return(paste0(rank, "\u00b0 d\u00eda m\u00e1s c\u00e1lido en esta estaci\u00f3n"))
  paste0("puesto ", rank, " entre los d\u00edas m\u00e1s extremos de esta estaci\u00f3n")
}

alerta_digest_icono_pal <- function(kind) {
  if (identical(kind, "heat")) return(list(bg = "#FDECE3", fg = "#C24A1F", ch = "&#9728;"))
  if (identical(kind, "cold")) return(list(bg = "#E6F0F8", fg = "#2E6FA7", ch = "&#10052;"))
  if (identical(kind, "wind")) return(list(bg = "#E6F0F8", fg = "#2E6FA7", ch = "&#8776;"))
  list(bg = "#E6F0F8", fg = "#2E6FA7", ch = "&#9730;")
}

alerta_digest_tarjeta <- function(ev, base) {
  copy <- trimws(as.character(ev$copy %||% ""))
  if (nzchar(copy)) {
    est <- alerta_digest_estacion_limpia(ev$estacion_nombre %||% "")
    pie <- trimws(as.character(ev$pie %||% ""))
    return(list(
      titulo = paste0(copy, if (nzchar(est)) paste0(" En ", est, ".") else ""),
      rec = if (nzchar(pie)) pie else "",
      lugar = alerta_digest_lugar(ev),
      fecha = alerta_digest_fecha_corta(ev$fecha),
      badge = if (identical(ev$tipo, "anticipada")) "Prevista" else "Confirmada",
      prevista = identical(ev$tipo, "anticipada"),
      kind = alerta_digest_fenomeno(ev$nombre_abr %||% "", ev$direccion %||% "max"),
      url = alerta_digest_reporte_url(ev, base)
    ))
  }
  if (identical(as.character(ev$tipo %||% ""), "umbral")) {
    alerta_digest_ensure_helpers()
    est <- alerta_digest_estacion_limpia(ev$estacion_nombre %||% "")
    nom <- trimws(as.character(ev$umbral_nombre %||% "Tu umbral"))
    frase <- trimws(as.character(ev$umbral_frase %||% ""))
    valor <- if (exists("alerta_mensaje_valor_txt", mode = "function")) {
      alerta_mensaje_valor_txt(ev$valor, ev$unidad %||% "", ev$nombre_abr %||% "")
    } else {
      paste(ev$valor %||% "", ev$unidad %||% "")
    }
    abr <- ev$nombre_abr %||% ""
    dir <- if (identical(abr, "t_min")) "min" else "max"
    return(list(
      titulo = paste0(nom, " en ", est),
      rec = if (nzchar(frase)) paste0(valor, " \u2014 ", frase) else valor,
      lugar = alerta_digest_lugar(ev),
      fecha = alerta_digest_fecha_corta(ev$fecha),
      badge = "Tu umbral",
      prevista = FALSE,
      kind = alerta_digest_fenomeno(abr, dir),
      url = alerta_digest_reporte_url(ev, base)
    ))
  }
  alerta_digest_ensure_helpers()
  enriched <- if (exists("alerta_digest_evento_enriquecer", mode = "function")) {
    alerta_digest_evento_enriquecer(ev, base)
  } else {
    ev
  }
  abr <- ev$nombre_abr %||% ""
  dir <- ev$direccion %||% "max"
  rank <- ev$ranking
  est <- alerta_digest_estacion_limpia(ev$estacion_nombre %||% enriched$estacion)
  frase <- alerta_digest_frase_rank(abr, dir, rank)
  valor <- as.character(enriched$valor %||% "")
  rec <- if (nzchar(frase) && nzchar(valor)) paste0(valor, " \u2014 ", frase) else paste0(valor, frase)
  tipo <- enriched$tipo %||% ev$tipo %||% "tardia"
  prevista <- identical(as.character(tipo), "temprana")
  list(
    titulo = paste0(alerta_digest_titulo(abr, dir, rank), " en ", est),
    rec = rec,
    lugar = alerta_digest_lugar(ev),
    fecha = alerta_digest_fecha_corta(ev$fecha %||% enriched$fecha_iso),
    badge = if (prevista) "Prevista" else "Confirmada",
    prevista = prevista,
    kind = alerta_digest_fenomeno(abr, dir),
    url = enriched$reporte_url %||% alerta_digest_reporte_url(ev, base)
  )
}

alerta_digest_kpi_card_html <- function(valor, detalle) {
  paste0(
    '<td width="33%" valign="top" style="padding:4px;">',
    '<div style="background:#ffffff;border:1px solid #E6E8EC;border-radius:14px;padding:14px 12px;">',
    '<div style="', .alerta_mail_ff, 'font-size:28px;font-weight:700;color:#232A36;line-height:1;letter-spacing:-0.01em;">', valor, '</div>',
    '<div style="', .alerta_mail_ff, 'font-size:12.5px;color:#5B6474;margin-top:6px;line-height:1.35;">', detalle, '</div>',
    '</div></td>'
  )
}

alerta_digest_fila_html <- function(ev, base) {
  t <- alerta_digest_tarjeta(ev, base)
  esc <- alerta_digest_html_esc
  pal <- alerta_digest_icono_pal(t$kind)
  badge_bg <- if (identical(t$badge, "Tu umbral")) "#FBF0DD" else if (isTRUE(t$prevista)) "#E6F0F8" else "#E7F4EC"
  badge_fg <- if (identical(t$badge, "Tu umbral")) "#9A5F10" else if (isTRUE(t$prevista)) "#2E6FA7" else "#2E8B57"
  lugar <- if (nzchar(t$lugar)) {
    paste0('<div style="', .alerta_mail_ff, 'font-size:13.5px;color:#5B6474;margin-top:3px;">', esc(t$lugar), '</div>')
  } else {
    ""
  }
  paste0(
    '<tr><td style="padding:0 16px 10px;">',
    '<table role="presentation" width="100%" cellspacing="0" cellpadding="0" style="background:#ffffff;border:1px solid #E6E8EC;border-radius:14px;">',
    '<tr>',
    '<td width="44" valign="top" style="padding:16px 0 16px 16px;">',
    '<div style="width:44px;height:44px;line-height:44px;text-align:center;background:', pal$bg,
    ';border-radius:12px;font-size:20px;color:', pal$fg, ';">', pal$ch, '</div></td>',
    '<td valign="top" style="padding:16px 12px;">',
    '<div style="', .alerta_mail_ff, 'font-size:15.5px;font-weight:700;color:#232A36;letter-spacing:-0.005em;line-height:1.35;">',
    esc(t$titulo),
    ' <span style="display:inline-block;font-size:11px;font-weight:700;letter-spacing:0.03em;text-transform:uppercase;padding:2px 9px;border-radius:99px;background:',
    badge_bg, ';color:', badge_fg, ';vertical-align:middle;">', esc(t$badge), '</span></div>',
    lugar,
    '<div style="', .alerta_mail_ff, 'font-size:13.5px;color:#232A36;margin-top:8px;line-height:1.4;">', esc(t$rec), '</div>',
    '</td>',
    '<td valign="top" align="right" style="padding:16px 16px 16px 0;white-space:nowrap;">',
    '<div style="', .alerta_mail_ff, 'font-size:12.5px;color:#5B6474;">', esc(t$fecha), '</div>',
    '<a href="', esc(t$url), '" style="', .alerta_mail_ff, 'display:inline-block;margin-top:8px;font-size:13px;font-weight:600;color:#9A5F10;text-decoration:none;">Ver estaci\u00f3n \u2192</a>',
    '</td></tr></table></td></tr>'
  )
}

#' Arma digest diario (confirmadas observadas + previstas de pronostico).
# ponytail: HTML completo con cientos de filas (~2MB) lo rechaza/spam Gmail;
# el cuerpo lleva solo el tope, el KPI/asunto usan el total real.
# Copia = panel: Prevista / Confirmada. Sin "critica" ni "tardia".
build_alerta_digest_email <- function(eventos, app_url, fecha_eval = Sys.Date(),
                                      eventos_resumen = NULL) {
  alerta_digest_ensure_helpers()
  base <- sub("/$", "", trimws(app_url %||% "https://app.rodaja.cl"))
  panel_url <- sprintf("%s/alertas/", base)
  resumen_src <- if (length(eventos_resumen) > 0) eventos_resumen else eventos
  n <- length(resumen_src)
  n_body <- length(eventos)
  n_omitidos <- max(0L, as.integer(n) - as.integer(n_body))
  fecha_txt <- alerta_digest_fecha_corta(fecha_eval)
  subject <- if (n == 1L) {
    paste0("1 alerta \u2014 rodaja.cl (", fecha_txt, ")")
  } else {
    paste0(n, " alertas \u2014 rodaja.cl (", fecha_txt, ")")
  }

  grupos <- if (exists("alerta_eventos_agrupar", mode = "function")) {
    alerta_eventos_agrupar(eventos)
  } else {
    list(list(nom_reg = "", nom_com = "", eventos = eventos))
  }

  resumen <- if (exists("alerta_digest_resumen", mode = "function")) {
    alerta_digest_resumen(resumen_src)
  } else {
    list(total = as.integer(n), tardia = as.integer(n), temprana = 0L)
  }
  ff <- .alerta_mail_ff
  kpi_html <- paste0(
    '<tr><td style="padding:4px 12px 8px;"><table role="presentation" width="100%" cellspacing="0" cellpadding="0"><tr>',
    alerta_digest_kpi_card_html(resumen$total, "Alertas de hoy"),
    alerta_digest_kpi_card_html(resumen$temprana, "Previstas \u00b7 pron\u00f3stico de hoy y ma\u00f1ana"),
    alerta_digest_kpi_card_html(resumen$tardia, "Confirmadas \u00b7 observadas ayer"),
    "</tr></table></td></tr>"
  )

  trunc_txt <- if (n_omitidos > 0L) {
    paste0(
      "En el correo: las ", n_body, " m\u00e1s extremas. ",
      "Hay ", n_omitidos, " m\u00e1s en el panel de alertas."
    )
  } else {
    ""
  }

  text_lines <- c(
    paste0("RESUMEN DE ALERTAS \u2014 rodaja.cl (", fecha_txt, ")"),
    "",
    paste0("Total: ", resumen$total, " | Confirmadas: ", resumen$tardia, " | Previstas: ", resumen$temprana),
    "Previstas = pron\u00f3stico pr\u00f3ximo \u00b7 Confirmadas = ya ocurri\u00f3",
    if (nzchar(trunc_txt)) c(trunc_txt, "") else character(0),
    ""
  )
  rows_html <- character(0)
  if (n_omitidos > 0L) {
    rows_html <- c(rows_html, paste0(
      '<tr><td style="padding:4px 16px 12px;"><div style="', ff,
      'font-size:13px;color:#232A36;background:#FBF0DD;border-radius:10px;padding:10px 14px;">',
      alerta_digest_html_esc(trunc_txt), '</div></td></tr>'
    ))
  }

  for (g in grupos) {
    hdr <- alerta_digest_palabra(g$nom_reg %||% "")
    if (!nzchar(hdr)) hdr <- "Sin regi\u00f3n"
    com <- alerta_digest_palabra(g$nom_com %||% "")
    if (nzchar(com)) hdr <- paste0(hdr, " \u00b7 ", com)
    text_lines <- c(text_lines, paste0("=== ", hdr, " ==="), "")
    rows_html <- c(rows_html, paste0(
      '<tr><td style="padding:8px 16px 6px;"><div style="', ff,
      'font-size:11px;font-weight:700;color:#5B6474;text-transform:uppercase;letter-spacing:0.06em;">',
      alerta_digest_html_esc(hdr), '</div></td></tr>'
    ))
    for (ev in g$eventos) {
      tarjeta <- alerta_digest_tarjeta(ev, base)
      text_lines <- c(
        text_lines,
        paste0("[", tarjeta$badge, "] ", tarjeta$titulo),
        paste0("   ", tarjeta$rec),
        if (nzchar(tarjeta$lugar)) paste0("   ", tarjeta$lugar, " \u00b7 ", tarjeta$fecha) else paste0("   ", tarjeta$fecha),
        paste0("   ", tarjeta$url),
        ""
      )
      rows_html <- c(rows_html, alerta_digest_fila_html(ev, base))
    }
  }

  text_lines <- c(text_lines, paste0("Panel de alertas: ", panel_url), "")
  text <- paste(text_lines, collapse = "\n")
  ayuda <- "Previstas = pron\u00f3stico pr\u00f3ximo \u00b7 Confirmadas = ya ocurri\u00f3"

  html <- paste0(
    '<!DOCTYPE html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"></head>',
    '<body style="margin:0;padding:0;background:#F5F6F8;">',
    '<table role="presentation" width="100%" cellspacing="0" cellpadding="0" style="background:#F5F6F8;padding:24px 12px;">',
    '<tr><td align="center">',
    '<table role="presentation" width="100%" cellspacing="0" cellpadding="0" style="max-width:640px;background:#ffffff;border:1px solid #E6E8EC;border-radius:16px;overflow:hidden;">',
    '<tr><td style="padding:16px 20px;border-bottom:1px solid #E6E8EC;">',
    '<span style="', ff, 'font-size:13px;color:#5B6474;">rodaja.cl</span>',
    '<span style="', ff, 'font-size:13px;color:#9A5F10;font-weight:600;"> \u00b7 resumen diario</span>',
    '</td></tr>',
    '<tr><td style="padding:22px 20px 6px;">',
    '<div style="', ff, 'font-size:26px;font-weight:700;color:#232A36;letter-spacing:-0.02em;">Resumen del d\u00eda</div>',
    '<div style="', ff, 'font-size:14.5px;color:#5B6474;margin-top:4px;">', alerta_digest_html_esc(fecha_txt), '</div>',
    '</td></tr>',
    '<tr><td style="padding:8px 16px 0;"><table role="presentation" width="100%" cellspacing="0" cellpadding="0">',
    kpi_html,
    '</table></td></tr>',
    '<tr><td style="padding:4px 20px 14px;"><div style="', ff, 'font-size:13px;color:#5B6474;">', ayuda, '</div></td></tr>',
    '<tr><td style="padding:0 0 8px;"><table role="presentation" width="100%" cellspacing="0" cellpadding="0">',
    paste(rows_html, collapse = ""),
    '</table></td></tr>',
    '<tr><td style="padding:8px 24px 20px;text-align:center;">',
    '<a href="', alerta_digest_html_esc(panel_url), '" style="display:inline-block;background:#9A5F10;color:#ffffff;', ff,
    'font-size:14px;font-weight:700;text-decoration:none;padding:12px 20px;border-radius:10px;">Panel de alertas \u2192</a>',
    '</td></tr>',
    '<tr><td style="padding:0 24px 20px;text-align:center;">',
    '<p style="', ff, 'font-size:12px;color:#5B6474;margin:0;">Un solo correo al d\u00eda con las alertas de los lugares que elegiste.</p>',
    '</td></tr></table></td></tr></table></body></html>'
  )
  list(subject = subject, text = text, html = html, panel_url = panel_url,
       n_total = as.integer(n), n_body = as.integer(n_body))
}

enviar_alerta_digest <- function(email_destino, eventos, app_url, fecha_eval = Sys.Date(),
                                 eventos_resumen = NULL) {
  if (length(eventos) == 0) return(list(ok = TRUE, error = NULL))
  payload <- build_alerta_digest_email(
    eventos, app_url, fecha_eval, eventos_resumen = eventos_resumen
  )
  hb <- nchar(payload$html %||% "", type = "bytes")
  message("[alertas] digest html_bytes=", hb,
          " n_body=", payload$n_body %||% length(eventos),
          " n_total=", payload$n_total %||% length(eventos))
  enviar_alerta_top10_payload(email_destino, payload)
}

#' Envia alerta usando payload prearmado
enviar_alerta_top10_payload <- function(email_destino, payload) {
  email_destino <- trimws(email_destino %||% "")
  if (!nzchar(email_destino)) return(list(ok = FALSE, error = "email vacio"))
  correo_rebote_boot()
  if (exists("correo_rebote_esta_bloqueado", mode = "function") &&
      isTRUE(correo_rebote_esta_bloqueado(email_destino))) {
    message("[alertas] omitido SMTP (correo bloqueado por rebotes) ", email_destino)
    err <- if (exists("correo_rebote_error_bloqueado", mode = "function")) correo_rebote_error_bloqueado() else "correo bloqueado por rebotes"
    return(list(ok = FALSE, error = err, bloqueado = TRUE))
  }
  tryCatch({
    smtp <- smtp_client()
    if (is.null(smtp)) return(list(ok = FALSE, error = "SMTP no configurado o emayili no disponible"))
    msg <- emayili::envelope(
      to = email_destino,
      from = Sys.getenv("SMTP_FROM", "alertas@rodaja.cl"),
      subject = payload$subject,
      text = payload$text,
      html = payload$html
    )
    smtp(msg)
    list(ok = TRUE, error = NULL)
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
}

#' Alerta top 10 diario por email
#' @return list(ok = logical, error = character or NULL)
enviar_alerta_top10 <- function(email_destino, estacion_nombre, variable_nombre,
                                fecha_larga, valor, unidad, ranking, n_dias,
                                direccion_label, eid, variable_id, app_url) {
  if (is.na(Sys.getenv("SMTP_HOST")) || !nzchar(trimws(Sys.getenv("SMTP_HOST", "")))) {
    return(list(ok = FALSE, error = "SMTP_HOST no configurado"))
  }
  if (!requireNamespace("emayili", quietly = TRUE)) {
    return(list(ok = FALSE, error = "paquete emayili no disponible"))
  }
  payload <- build_alerta_top10_email(
    estacion_nombre = estacion_nombre,
    variable_nombre = variable_nombre,
    fecha_larga = fecha_larga,
    valor = valor,
    unidad = unidad,
    ranking = ranking,
    n_dias = n_dias,
    direccion_label = direccion_label,
    eid = eid,
    variable_id = variable_id,
    app_url = app_url
  )
  enviar_alerta_top10_payload(email_destino, payload)
}
