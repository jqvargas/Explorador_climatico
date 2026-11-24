# Copie este archivo a ingest/sources/<fuente>/adapter.R y adapte.

ingest_<fuente>_not_implemented <- function(variable) {
  stop("Implemente fetch para variable: ", variable)
}

<fuente>_fetch_pp <- function(...) {
  ingest_<fuente>_not_implemented("pp")
}
