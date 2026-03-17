# API helpers
api_url <- function() sub("/$", "", Sys.getenv("API_URL", "http://api:8000"))
api_get <- function(path, params = NULL, timeout = 15) {
  url <- paste0(api_url(), path)
  if (!is.null(params) && length(params) > 0) {
    q <- paste(names(params), params, sep = "=", collapse = "&")
    url <- paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", q)
  }
  resp <- tryCatch(httr::GET(url, httr::timeout(timeout)), error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  tryCatch(httr::content(resp, as = "parsed", type = "application/json"), error = function(e) NULL)
}
json_to_df <- function(dat, cols) {
  if (is.null(dat)) return(setNames(as.data.frame(matrix(0,0,length(cols))), cols))
  if (is.data.frame(dat)) return(dat[, cols[cols %in% names(dat)], drop = FALSE])
  if (!is.list(dat) || length(dat) == 0) return(setNames(as.data.frame(matrix(0,0,length(cols))), cols))
  if (identical(names(dat), "data") && is.list(dat$data)) dat <- dat$data
  rows <- lapply(dat, function(x) {
    if (is.atomic(x)) return(NULL)
    setNames(as.data.frame(as.list(setNames(lapply(cols, function(c) x[[c]] %||% NA), cols)), stringsAsFactors = FALSE), cols)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(setNames(as.data.frame(matrix(0,0,length(cols))), cols))
  df <- do.call(rbind, rows)
  if ("id" %in% cols) df$id <- as.integer(df$id)
  setNames(df, cols)
}
