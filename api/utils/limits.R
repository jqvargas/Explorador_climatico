# Limites por tier: cuota en bytes (tamano de archivos exportados).

BYTES_MB <- 1024^2
BYTES_GB <- 1024^3

LIMITES <- list(
  gratuito = list(
    max_bytes = 50 * BYTES_MB,
    max_estaciones = NULL,
    max_alertas_suscripcion = 1L,
    max_alertas_variables = 1L,
    alertas_scopes = c("favorita"),
    formatos = c("liviano", "completo"),
    granularidades = c("anual", "mensual", "diario", "horaria"),
    reset = "mensual"
  ),
  pro = list(
    max_bytes = 1 * BYTES_GB,
    max_estaciones = NULL,
    max_alertas_suscripcion = NULL,
    max_alertas_variables = NULL,
    alertas_scopes = c("favorita", "grupo", "estacion", "comuna", "region", "cuenca"),
    formatos = c("liviano", "completo"),
    granularidades = c("anual", "mensual", "diario", "horaria"),
    reset = "mensual"
  ),
  enterprise = list(
    max_bytes = 3 * BYTES_GB,
    max_estaciones = NULL,
    max_alertas_suscripcion = NULL,
    max_alertas_variables = NULL,
    alertas_scopes = c("favorita", "grupo", "estacion", "comuna", "region", "cuenca"),
    formatos = c("liviano", "completo"),
    granularidades = c("anual", "mensual", "diario", "horaria"),
    reset = "mensual",
    api_access = TRUE
  )
)

GRANULARIDADES_VALIDAS <- c("anual", "mensual", "diario", "horaria")
FORMATOS_DESCARGA_VALIDOS <- c("liviano", "completo")

get_limite_tier <- function(tier) {
  lim <- LIMITES[[tier %||% "gratuito"]]
  if (is.null(lim)) LIMITES$gratuito else lim
}

# Ancla de la cuota: el alta de la cuenta (gratuito) o el alta de ese plan.
cuota_ancla_sql <- paste(
  "SELECT CASE WHEN $2::text = 'gratuito' THEN u.created_at",
  "ELSE COALESCE(",
  "(SELECT MIN(s.created_at) FROM abono s WHERE s.user_id = u.id AND s.tier = $2),",
  "(SELECT MIN(a.creado_en) FROM libro_de_novedades a",
  "  WHERE a.target_user_id = u.id AND a.accion = 'cambiar_plan'",
  "    AND a.detalle->>'despues' = $2),",
  "u.created_at) END AS ancla",
  "FROM tripulacion u WHERE u.id = $1::uuid"
)

cuota_as_utc <- function(x) {
  if (inherits(x, "POSIXct")) {
    attr(x, "tzone") <- "UTC"
    return(x)
  }
  if (inherits(x, "Date")) return(as.POSIXct(paste(as.character(x), "00:00:00"), tz = "UTC"))
  as.POSIXct(x, tz = "UTC")
}

# Suma meses de calendario y recorta el día (31 ene -> 28 feb).
cuota_add_months <- function(ts, n) {
  ts <- cuota_as_utc(ts)
  n <- as.integer(n)
  if (is.na(n)) n <- 0L
  lt <- as.POSIXlt(ts, tz = "UTC")
  y <- lt$year + 1900L
  m0 <- lt$mon + n
  y <- y + m0 %/% 12L
  m <- m0 %% 12L
  next_y <- y + (m == 11L)
  next_m <- if (m == 11L) 1L else m + 2L
  last <- as.integer(format(as.Date(sprintf("%04d-%02d-01", next_y, next_m)) - 1L, "%d"))
  day <- min(as.integer(lt$mday), last)
  as.POSIXct(sprintf(
    "%04d-%02d-%02d %02d:%02d:%06.3f",
    y, m + 1L, day, lt$hour, lt$min, lt$sec
  ), tz = "UTC")
}

# Ventana vigente. El corte se repite desde `ancla`, no a las 00:00 UTC ni el día 1.
cuota_periodo_de <- function(ancla, reset, ahora = Sys.time()) {
  ancla <- cuota_as_utc(ancla)
  ahora <- cuota_as_utc(ahora)
  if (is.na(ancla)) ancla <- ahora
  if (is.na(ahora)) ahora <- ancla
  if (ancla > ahora) ancla <- ahora
  if (identical(reset, "diario") || identical(reset, "semanal")) {
    paso <- if (identical(reset, "semanal")) 7 * 86400 else 86400
    n <- as.integer(floor(as.numeric(difftime(ahora, ancla, units = "secs")) / paso))
    if (is.na(n) || n < 0L) n <- 0L
    ini <- ancla + n * paso
    fin <- ini + paso
  } else {
    n <- 0L
    while (n < 600L && cuota_add_months(ancla, n + 1L) <= ahora) n <- n + 1L
    ini <- cuota_add_months(ancla, n)
    fin <- cuota_add_months(ancla, n + 1L)
  }
  list(
    inicio = as.Date(ini, tz = "UTC"),
    fin = as.Date(fin, tz = "UTC"),
    inicio_ts = ini,
    fin_ts = fin
  )
}

# `dia` es un corte real si coincide con el inicio de alguna ventana desde el ancla.
cuota_es_corte <- function(ancla, reset, dia) {
  ancla <- cuota_as_utc(ancla)
  dia <- as.Date(dia)
  if (is.na(ancla) || is.na(dia)) return(FALSE)
  if (identical(reset, "diario") || identical(reset, "semanal")) {
    n <- as.integer(dia - as.Date(ancla, tz = "UTC"))
    paso <- if (identical(reset, "semanal")) 7L else 1L
    if (is.na(n) || n < 0L || n %% paso != 0L) return(FALSE)
    return(identical(as.Date(ancla + n * 86400, tz = "UTC"), dia))
  }
  n <- 0L
  repeat {
    d <- as.Date(cuota_add_months(ancla, n), tz = "UTC")
    if (!is.na(d) && d == dia) return(TRUE)
    if (is.na(d) || d > dia || n > 600L) return(FALSE)
    n <- n + 1L
  }
}

resolve_formato_descarga <- function(tier, formato_solicitado) {
  fmt <- tolower(trimws(formato_solicitado %||% "liviano"))
  if (!fmt %in% FORMATOS_DESCARGA_VALIDOS) fmt <- "liviano"
  lim <- get_limite_tier(tier)
  allowed <- lim$formatos %||% c("liviano")
  if (!fmt %in% allowed) fmt <- allowed[1]
  fmt
}

if (sys.nframe() == 0L) {
  ancla <- as.POSIXct("2026-11-19 23:12:20", tz = "UTC")
  ahora <- as.POSIXct("2026-11-20 13:45:00", tz = "UTC")
  p <- cuota_periodo_de(ancla, "diario", ahora)
  stopifnot(identical(p$inicio, as.Date("2026-11-19")), identical(p$fin, as.Date("2026-11-20")))
  p2 <- cuota_periodo_de(as.POSIXct("2026-11-18 15:44:20", tz = "UTC"), "diario", ahora)
  stopifnot(identical(p2$inicio, as.Date("2026-11-19")), identical(p2$fin, as.Date("2026-11-20")))
  borde <- as.POSIXct("2026-11-20 23:12:20", tz = "UTC")
  stopifnot(identical(cuota_periodo_de(ancla, "diario", borde)$inicio, as.Date("2026-11-20")))
  stopifnot(identical(cuota_periodo_de(ancla, "diario", borde - 1)$inicio, as.Date("2026-11-19")))
  pro <- as.POSIXct("2026-06-18 19:36:28", tz = "UTC")
  pm <- cuota_periodo_de(pro, "mensual", ahora)
  stopifnot(identical(pm$inicio, as.Date("2026-11-18")), identical(pm$fin, as.Date("2026-12-18")))
  jf <- cuota_periodo_de(
    as.POSIXct("2026-01-31 08:00:00", tz = "UTC"), "mensual",
    as.POSIXct("2026-03-01 08:00:00", tz = "UTC")
  )
  stopifnot(identical(jf$inicio, as.Date("2026-02-28")))
  stopifnot(isTRUE(cuota_es_corte(ancla, "diario", as.Date("2026-11-20"))))
  stopifnot(isTRUE(cuota_es_corte(pro, "mensual", as.Date("2026-11-18"))))
  stopifnot(isTRUE(!cuota_es_corte(pro, "mensual", as.Date("2026-11-01"))))
  message("[ok] cuota_periodo")
}
