# Constantes compartidas del reporte PDF.

REPORTE_MIN_YEARS_MK <- 8L
REPORTE_MIN_MONTHS_SPI <- 12L
REPORTE_MIN_MONTHS_ENSO <- 24L
REPORTE_DOC_VERSION <- "1.0"

RODAJA_COL_BRAND <- "#DF9531"
RODAJA_COL_DARK <- "#394553"
RODAJA_COL_WATER <- "#378ADD"
RODAJA_COL_BG <- "#F4F6F8"
RODAJA_COL_GREEN <- "#1B7F4B"
RODAJA_COL_MUTED <- "#8A95A0"
RODAJA_PALETTE <- c(
  brand = RODAJA_COL_BRAND,
  dark = RODAJA_COL_DARK,
  water = RODAJA_COL_WATER,
  green = RODAJA_COL_GREEN,
  muted = RODAJA_COL_MUTED
)

reporte_doc_version_string <- function(date = NULL) {
  if (is.null(date) || !nzchar(date)) date <- format(Sys.Date(), "%Y-%m-%d")
  paste0("v", REPORTE_DOC_VERSION, " \u00b7 generado ", date)
}
