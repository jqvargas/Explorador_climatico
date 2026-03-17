#' Envia link de descarga por email usando emayili
#' Si SMTP_HOST esta vacio, no envia (prototipo)
enviar_link_descarga <- function(email_destino, link, job_id) {
  host <- Sys.getenv("SMTP_HOST")
  if (is.na(host) || host == "") return(invisible(NULL))

  pkg <- "emayili"
  if (!requireNamespace(pkg, quietly = TRUE)) return(invisible(NULL))

  smtp <- emayili::server(
    host = host,
    port = as.integer(Sys.getenv("SMTP_PORT", "587")),
    username = Sys.getenv("SMTP_USER"),
    password = Sys.getenv("SMTP_PASS"),
    reuse = TRUE
  )
  msg <- emayili::envelope(
    to = email_destino,
    from = Sys.getenv("SMTP_FROM", "noreply@clima.local"),
    subject = glue::glue("Descarga lista - Job {job_id}"),
    body = glue::glue("Tu descarga esta lista. Link: {link}")
  )
  smtp(msg)
  invisible(NULL)
}
