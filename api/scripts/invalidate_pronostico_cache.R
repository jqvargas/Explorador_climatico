#!/usr/bin/env Rscript
# Invalida cache Redis de pronostico tras evaluar_alertas_pronostico.

base <- Sys.getenv("API_ROOT", "/app")
if (!dir.exists(base)) base <- file.path(getwd(), "..")
source(file.path(base, "utils", "cache.R"), local = FALSE)

ok <- isTRUE(cache_invalidate("pronostico:*"))
message("[cache] invalidate pronostico:* ", if (ok) "OK" else "skip/fail")
quit(status = 0)
