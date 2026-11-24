# Cuota de descarga (worker): ajuste en bytes post-job.

`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

api_limits <- file.path(Sys.getenv("API_ROOT", "/api"), "utils", "limits.R")
if (!file.exists(api_limits)) stop("no encuentro ", api_limits)
source(api_limits)

BYTES_MB <- 1024^2
BYTES_GB <- 1024^3

LIMITES_WORKER <- list(
  gratuito = list(max_bytes = 50 * BYTES_MB, reset = "mensual"),
  pro = list(max_bytes = 1 * BYTES_GB, reset = "mensual"),
  enterprise = list(max_bytes = 3 * BYTES_GB, reset = "mensual")
)

worker_ancla <- function(conn, user_id, tier) {
  df <- DBI::dbGetQuery(conn, cuota_ancla_sql, params = list(user_id, tier %||% "gratuito"))
  if (nrow(df) == 0 || is.na(df$ancla[1])) return(Sys.time())
  df$ancla[1]
}

worker_periodo_inicio <- function(conn, user_id, tier) {
  lim <- LIMITES_WORKER[[tier %||% "gratuito"]]
  if (is.null(lim)) lim <- LIMITES_WORKER$gratuito
  cuota_periodo_de(worker_ancla(conn, user_id, tier), lim$reset)$inicio
}

worker_resolve_tier <- function(conn, user_id) {
  if (is.null(user_id) || is.na(user_id) || !nzchar(as.character(user_id))) return("gratuito")
  df <- DBI::dbGetQuery(conn, "SELECT tier FROM tripulacion WHERE id = $1::uuid", params = list(user_id))
  if (nrow(df) == 0) return("gratuito")
  tier <- df$tier[1]
  if (identical(tier, "enterprise")) return("enterprise")
  if (identical(tier, "pro")) return("pro")
  "gratuito"
}

worker_liberar_cuota <- function(conn, user_id, tier, n_bytes, overage_bytes = 0L, extra_bytes = 0L) {
  if (is.null(user_id) || is.na(n_bytes) || n_bytes < 1L) return(invisible(NULL))
  extra_bytes <- as.integer(extra_bytes)
  if (is.na(extra_bytes) || extra_bytes < 0L) extra_bytes <- 0L
  period_release <- max(0L, as.integer(n_bytes) - extra_bytes)
  if (period_release > 0L) {
    periodo <- worker_periodo_inicio(conn, user_id, tier)
    DBI::dbExecute(conn,
      "UPDATE cupo_del_periodo SET bytes_consumidos = GREATEST(0, bytes_consumidos - $3)
       WHERE user_id = $1::uuid AND periodo_inicio = $2",
      params = list(user_id, periodo, period_release)
    )
  }
  overage_bytes <- as.integer(overage_bytes)
  if (!is.na(overage_bytes) && overage_bytes > 0L) {
    worker_liberar_overage(conn, user_id, overage_bytes)
  }
  if (extra_bytes > 0L) {
    DBI::dbExecute(conn,
      "UPDATE tripulacion SET bytes_extra_saldo = bytes_extra_saldo + $2, updated_at = NOW() WHERE id = $1::uuid",
      params = list(user_id, extra_bytes)
    )
  }
  invisible(NULL)
}

# Igual que api/utils/mb_pack.R mb_pack_realize. El exceso sobre el estimado no come más saldo.
worker_mb_realize <- function(incluido_est, extra_est, overage_est, real) {
  incluido_est <- as.numeric(incluido_est); if (is.na(incluido_est) || incluido_est < 0) incluido_est <- 0
  extra_est <- as.numeric(extra_est); if (is.na(extra_est) || extra_est < 0) extra_est <- 0
  overage_est <- as.numeric(overage_est); if (is.na(overage_est) || overage_est < 0) overage_est <- 0
  real <- as.numeric(real); if (is.na(real) || real < 0) real <- 0
  left <- real
  real_inc <- min(incluido_est, left); left <- left - real_inc
  real_extra <- min(extra_est, left); left <- left - real_extra
  list(
    period_delta = as.integer(round((real_inc + left) - (incluido_est + overage_est))),
    extra_delta = as.integer(round(real_extra - extra_est)),
    overage = as.integer(round(left))
  )
}

worker_ajustar_cuota <- function(conn, user_id, tier, estimado, real, overage_est = 0L, overage_real = NULL, extra_est = 0L) {
  if (is.null(user_id)) return(invisible(NULL))
  estimado <- as.integer(estimado); if (is.na(estimado)) estimado <- 0L
  real <- as.integer(real); if (is.na(real)) real <- 0L
  extra_est <- as.integer(extra_est); if (is.na(extra_est) || extra_est < 0L) extra_est <- 0L
  if (extra_est > 0L) {
    overage_est <- as.integer(overage_est); if (is.na(overage_est) || overage_est < 0L) overage_est <- 0L
    got <- worker_mb_realize(max(0L, estimado - extra_est - overage_est), extra_est, overage_est, real)
    diff <- got$period_delta
    overage_real <- got$overage
    if (got$extra_delta < 0L) {
      DBI::dbExecute(conn,
        "UPDATE tripulacion SET bytes_extra_saldo = bytes_extra_saldo + $2, updated_at = NOW() WHERE id = $1::uuid",
        params = list(user_id, abs(got$extra_delta))
      )
    }
  } else {
    diff <- real - estimado
  }
  if (diff != 0L) {
    periodo <- worker_periodo_inicio(conn, user_id, tier)
    if (diff > 0L) {
      DBI::dbExecute(conn,
        "INSERT INTO cupo_del_periodo (user_id, periodo_inicio, bytes_consumidos, registros_consumidos)
         VALUES ($1::uuid, $2, $3, 0)
         ON CONFLICT (user_id, periodo_inicio)
         DO UPDATE SET bytes_consumidos = cupo_del_periodo.bytes_consumidos + $3",
        params = list(user_id, periodo, diff)
      )
    } else {
      DBI::dbExecute(conn,
        "UPDATE cupo_del_periodo SET bytes_consumidos = GREATEST(0, bytes_consumidos + $3)
         WHERE user_id = $1::uuid AND periodo_inicio = $2",
        params = list(user_id, periodo, diff)
      )
    }
  }

  overage_est <- as.integer(overage_est); if (is.na(overage_est)) overage_est <- 0L
  if (is.null(overage_real)) {
    if (diff == 0L) return(invisible(NULL))
    periodo <- worker_periodo_inicio(conn, user_id, tier)
    df_u <- DBI::dbGetQuery(conn,
      "SELECT bytes_consumidos FROM cupo_del_periodo WHERE user_id = $1::uuid AND periodo_inicio = $2",
      params = list(user_id, periodo)
    )
    usado <- if (nrow(df_u) > 0) as.numeric(df_u$bytes_consumidos[1]) else 0
    u0 <- usado - real
    limite <- worker_get_limite_bytes(tier)
    overage_real <- as.integer(worker_on_demand_overage_increment(u0, limite, real))
  } else {
    overage_real <- as.integer(overage_real)
  }
  if (diff == 0L && overage_est == overage_real) return(invisible(NULL))
  worker_ajustar_overage(conn, user_id, tier, overage_est, overage_real)
  worker_try_charge_on_demand(conn, user_id)
  invisible(NULL)
}
