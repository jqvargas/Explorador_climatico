# Filtro por variable en listados de estaciones.

estaciones_expand_id_variable <- function(id_variable) {
  vid <- suppressWarnings(as.integer(id_variable))
  if (is.na(vid) || vid <= 0L) return(integer(0))
  vid
}

estaciones_exists_variable_sql <- function(var_ids, eid_sql = "e.id") {
  ids <- unique(as.integer(var_ids))
  ids <- ids[!is.na(ids) & ids > 0L]
  if (length(ids) == 0L) return("")
  in_clause <- if (length(ids) == 1L) {
    paste0("o.id_variable = ", ids[1L])
  } else {
    paste0("o.id_variable IN (", paste(ids, collapse = ","), ")")
  }
  paste0(
    " AND EXISTS (SELECT 1 FROM bitacora_oficial o WHERE o.id_estacion = ",
    eid_sql, " AND ", in_clause, ")"
  )
}

estaciones_distinct_estaciones_subq <- function(var_ids) {
  ids <- unique(as.integer(var_ids))
  ids <- ids[!is.na(ids) & ids > 0L]
  subq <- "SELECT DISTINCT id_estacion FROM bitacora_oficial"
  if (length(ids) == 0L) return(subq)
  if (length(ids) == 1L) {
    paste0(subq, " WHERE id_variable = ", ids[1L])
  } else {
    paste0(subq, " WHERE id_variable IN (", paste(ids, collapse = ","), ")")
  }
}
