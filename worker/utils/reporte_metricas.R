# Metricas incrementales para reportes de estacion (Fase 1 / 2 / pesada).

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

if (!exists("REPORTE_MIN_YEARS_MK")) REPORTE_MIN_YEARS_MK <- 8L
if (!exists("REPORTE_MIN_MONTHS_SPI")) REPORTE_MIN_MONTHS_SPI <- 12L
if (!exists("REPORTE_MIN_MONTHS_ENSO")) REPORTE_MIN_MONTHS_ENSO <- 24L

reporte_var_agg <- function(nombre_abr, nombre) {
  n <- tolower(paste(nombre_abr, nombre))
  if (grepl("precip|lluvia|^pp$|^p$", n)) return("sum")
  "mean"
}

reporte_welford_init <- function() {
  list(n = 0L, mean = 0, M2 = 0, sum = 0)
}

reporte_welford_update <- function(acc, x) {
  if (is.null(acc)) acc <- reporte_welford_init()
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return(acc)
  acc$sum <- acc$sum + x
  acc$n <- acc$n + 1L
  delta <- x - acc$mean
  acc$mean <- acc$mean + delta / acc$n
  delta2 <- x - acc$mean
  acc$M2 <- acc$M2 + delta * delta2
  acc
}

reporte_welford_merge <- function(a, b) {
  if (is.null(a) || a$n == 0) return(b)
  if (is.null(b) || b$n == 0) return(a)
  n <- a$n + b$n
  delta <- b$mean - a$mean
  mean <- a$mean + delta * b$n / n
  M2 <- a$M2 + b$M2 + delta^2 * a$n * b$n / n
  list(n = n, mean = mean, M2 = M2, sum = a$sum + b$sum)
}

reporte_hist_init <- function(bins = REPORTE_HIST_BINS, lo = NULL, hi = NULL) {
  list(bins = as.integer(bins), lo = lo, hi = hi, counts = rep(0L, bins))
}

reporte_hist_update <- function(hist, x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return(hist)
  lo <- hist$lo; hi <- hist$hi
  if (is.null(lo) || is.null(hi)) {
    lo <- x; hi <- x
  } else {
    if (x < lo) lo <- x
    if (x > hi) hi <- x
  }
  bins <- hist$bins
  if (hi <= lo) {
    idx <- 1L
  } else {
    idx <- as.integer(floor((x - lo) / (hi - lo) * (bins - 1))) + 1L
    idx <- max(1L, min(bins, idx))
  }
  counts <- hist$counts
  counts[idx] <- counts[idx] + 1L
  list(bins = bins, lo = lo, hi = hi, counts = counts)
}

reporte_hist_percentile <- function(hist, p = 0.5) {
  if (is.null(hist) || sum(hist$counts) == 0) return(NA_real_)
  total <- sum(hist$counts)
  target <- p * total
  cum <- 0L
  lo <- hist$lo; hi <- hist$hi
  bins <- hist$bins
  for (i in seq_along(hist$counts)) {
    cum <- cum + hist$counts[i]
    if (cum >= target) {
      if (hi <= lo) return(lo)
      bin_lo <- lo + (i - 1) * (hi - lo) / bins
      bin_hi <- lo + i * (hi - lo) / bins
      return((bin_lo + bin_hi) / 2)
    }
  }
  hi
}

reporte_extremos_init <- function(top_n = REPORTE_EXTREMOS_TOP_N) {
  list(top_n = as.integer(top_n), max = list(), min = list())
}

reporte_extremos_update <- function(ext, fecha, valor, top_n = NULL) {
  if (is.null(ext)) ext <- reporte_extremos_init(top_n)
  n <- ext$top_n %||% REPORTE_EXTREMOS_TOP_N
  v <- suppressWarnings(as.numeric(valor))
  if (length(v) == 0 || is.na(v)) return(ext)
  fd <- as.character(as.Date(fecha))
  entry <- list(fecha = fd, valor = v)

  ext$max <- c(ext$max, list(entry))
  ext$max <- ext$max[order(vapply(ext$max, function(e) -e$valor, numeric(1)))]
  if (length(ext$max) > n) ext$max <- ext$max[seq_len(n)]

  ext$min <- c(ext$min, list(entry))
  ext$min <- ext$min[order(vapply(ext$min, function(e) e$valor, numeric(1)))]
  if (length(ext$min) > n) ext$min <- ext$min[seq_len(n)]
  ext
}

reporte_build_monthly_annual <- function(df, agg = "mean") {
  if (nrow(df) == 0) return(list(monthly = vector("list", 12), annual = list()))
  df$mes <- as.integer(format(df$fecha, "%m"))
  df$anio <- as.integer(format(df$fecha, "%Y"))
  monthly <- vector("list", 12)
  for (m in 1:12) {
    sub <- df[df$mes == m, , drop = FALSE]
    acc <- reporte_welford_init()
    for (v in sub$valor) acc <- reporte_welford_update(acc, v)
    monthly[[m]] <- acc
  }
  annual <- list()
  for (yr in sort(unique(df$anio))) {
    sub <- df[df$anio == yr, , drop = FALSE]
    if (agg == "sum") {
      annual[[as.character(yr)]] <- list(n = nrow(sub), sum = sum(sub$valor, na.rm = TRUE),
                                         mean = mean(sub$valor, na.rm = TRUE), M2 = 0)
    } else {
      acc <- reporte_welford_init()
      for (v in sub$valor) acc <- reporte_welford_update(acc, v)
      annual[[as.character(yr)]] <- acc
    }
  }
  list(monthly = monthly, annual = annual)
}

reporte_build_climatology_hists <- function(df) {
  hists <- vector("list", 12)
  for (i in 1:12) hists[[i]] <- reporte_hist_init()
  if (nrow(df) == 0) return(hists)
  df$mes <- as.integer(format(df$fecha, "%m"))
  for (i in seq_len(nrow(df))) {
    m <- df$mes[i]
    hists[[m]] <- reporte_hist_update(hists[[m]], df$valor[i])
  }
  hists
}

reporte_build_extremes_from_df <- function(df, top_n = REPORTE_EXTREMOS_TOP_N) {
  ext <- reporte_extremos_init(top_n)
  if (nrow(df) == 0) return(ext)
  for (i in seq_len(nrow(df))) {
    ext <- reporte_extremos_update(ext, df$fecha[i], df$valor[i], top_n)
  }
  ext
}

# Ultimo registro = dato sin validar (no entra a extremos ni IQR de referencia).
reporte_split_sin_validar <- function(df) {
  empty_sv <- list(fecha = NA_character_, valor = NA_real_)
  if (is.null(df) || nrow(df) == 0) {
    return(list(validado = df, sin_validar = empty_sv))
  }
  df <- df[order(df$fecha), , drop = FALSE]
  last <- df[nrow(df), , drop = FALSE]
  sin_validar <- list(
    fecha = as.character(as.Date(last$fecha[1])),
    valor = suppressWarnings(as.numeric(last$valor[1]))
  )
  validado <- if (nrow(df) > 1L) df[-nrow(df), , drop = FALSE] else {
    data.frame(fecha = as.Date(character(0)), valor = numeric(0))
  }
  list(validado = validado, sin_validar = sin_validar)
}

reporte_iqr_stats <- function(values) {
  v <- suppressWarnings(as.numeric(values))
  v <- v[!is.na(v)]
  if (length(v) < 4L) {
    return(list(
      q1 = NA_real_, q3 = NA_real_, iqr = NA_real_,
      fence_lo = NA_real_, fence_hi = NA_real_,
      n = length(v), available = FALSE
    ))
  }
  q <- stats::quantile(v, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE, type = 8)
  iqr <- q[2] - q[1]
  list(
    q1 = unname(q[1]), q3 = unname(q[2]), iqr = unname(iqr),
    fence_lo = unname(q[1] - 1.5 * iqr),
    fence_hi = unname(q[2] + 1.5 * iqr),
    n = length(v), available = TRUE
  )
}

reporte_apply_sin_validar <- function(vstate, df) {
  sp <- reporte_split_sin_validar(df)
  vstate$sin_validar <- sp$sin_validar
  vstate$last_obs_date <- sp$sin_validar$fecha
  vstate$iqr <- reporte_iqr_stats(sp$validado$valor)
  vstate$extremes <- reporte_build_extremes_from_df(sp$validado)
  if ((vstate$agg %||% "") == "sum" && nrow(sp$validado) > 0) {
    vstate$extremes$streaks <- reporte_compute_precip_streaks(sp$validado)
  }
  vstate$climatology_hists <- reporte_build_climatology_hists(sp$validado)
  vstate <- reporte_curvas_pp(vstate, sp$validado$fecha, sp$validado$valor)
  pct <- vapply(vstate$climatology_hists, function(h) {
    c(p10 = reporte_hist_percentile(h, 0.1),
      p50 = reporte_hist_percentile(h, 0.5),
      p90 = reporte_hist_percentile(h, 0.9))
  }, numeric(3))
  vstate$climatology_pct <- as.data.frame(t(pct))
  vstate
}

reporte_sen_slope <- function(years, values) {
  ok <- !is.na(values) & !is.na(years)
  x <- years[ok]; y <- values[ok]
  if (length(y) < 2L) return(NA_real_)
  if (requireNamespace("zyp", quietly = TRUE)) {
    sen <- tryCatch(zyp::sen(cbind(x, y)), error = function(e) NULL)
    if (!is.null(sen)) return(unname(sen[1, "sen.slope"]))
  }
  slopes <- numeric(0)
  n <- length(y)
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      dx <- x[j] - x[i]
      if (dx != 0) slopes <- c(slopes, (y[j] - y[i]) / dx)
    }
  }
  if (length(slopes) == 0) NA_real_ else stats::median(slopes, na.rm = TRUE)
}

reporte_mann_kendall <- function(annual_values, years) {
  ok <- !is.na(annual_values) & !is.na(years)
  y <- annual_values[ok]; x <- years[ok]
  n_years <- length(y)
  unavailable <- list(
    tau = NA_real_, p = NA_real_, sen_slope = NA_real_,
    n_years = n_years, available = FALSE,
    reason = paste0(
      "Serie insuficiente para test de tendencia (", n_years, " a\u00f1o",
      if (n_years != 1L) "s" else "", " de registro; se requieren m\u00ednimo ",
      REPORTE_MIN_YEARS_MK, " a\u00f1os)."
    )
  )
  if (n_years < REPORTE_MIN_YEARS_MK) return(unavailable)
  out <- list(tau = NA_real_, p = NA_real_, sen_slope = NA_real_,
              n_years = n_years, available = TRUE, reason = NA_character_)
  if (requireNamespace("Kendall", quietly = TRUE)) {
    mk <- tryCatch(Kendall::MannKendall(y), error = function(e) NULL)
    if (!is.null(mk)) {
      out$tau <- unname(mk$tau)
      out$p <- unname(mk$sl)
    }
  }
  out$sen_slope <- reporte_sen_slope(x, y)
  out
}

reporte_spi_monthly_ts <- function(monthly_totals) {
  keys <- sort(names(monthly_totals))
  if (length(keys) == 0) return(NULL)
  vals <- vapply(keys, function(k) as.numeric(monthly_totals[[k]]), numeric(1))
  parts <- strsplit(keys[1], "-", fixed = TRUE)[[1]]
  yr <- as.integer(parts[1])
  mo <- as.integer(parts[2])
  ts(vals, start = c(yr, mo), frequency = 12)
}

reporte_spi_fit <- function(monthly_totals) {
  ts_obj <- reporte_spi_monthly_ts(monthly_totals)
  n_months <- if (is.null(ts_obj)) 0L else length(ts_obj)
  if (n_months < REPORTE_MIN_MONTHS_SPI) {
    return(list(
      params = list(), last_fit = NA_character_, n_months = n_months,
      available = FALSE,
      reason = paste0(
        "SPI no disponible (", n_months, " meses en ajuste; se requieren m\u00ednimo ",
        REPORTE_MIN_MONTHS_SPI, ")."
      )
    ))
  }
  fit <- tryCatch({
    if (requireNamespace("SPEI", quietly = TRUE)) {
      fitd <- SPEI::spi(ts_obj, scale = 1, fit = "ub-pwm", na.rm = TRUE, distribution = "Gamma", verbose = FALSE)
      list(dist = "Gamma", coefficients = fitd$coefficients)
    } else NULL
  }, error = function(e) NULL)
  list(
    params = fit %||% list(),
    coefficients = fit$coefficients,
    last_fit = format(Sys.Date(), "%Y-%m-%d"),
    n_months = n_months,
    available = !is.null(fit$coefficients),
    reason = if (!is.null(fit$coefficients)) NA_character_ else "No fue posible ajustar la distribuci\u00f3n gamma."
  )
}

reporte_spi_current <- function(month_value, spi_obj, monthly_totals = NULL) {
  if (!isTRUE(spi_obj$available)) return(NA_real_)
  coef <- spi_obj$coefficients
  if (is.null(coef) && !is.null(spi_obj$params$coefficients)) coef <- spi_obj$params$coefficients
  if (is.null(coef)) return(NA_real_)
  tryCatch({
    if (!is.null(monthly_totals) && length(monthly_totals) > 0) {
      mt <- monthly_totals
      cm_ym <- paste0(format(Sys.Date(), "%Y"), "-", sprintf("%02d", as.integer(format(Sys.Date(), "%m"))))
      v <- suppressWarnings(as.numeric(month_value))
      if (!is.na(v)) mt[[cm_ym]] <- v
      ts_obj <- reporte_spi_monthly_ts(mt)
      fitd <- SPEI::spi(ts_obj, scale = 1, distribution = "Gamma", params = coef, na.rm = TRUE, verbose = FALSE)
      return(as.numeric(tail(fitd$fitted, 1)))
    }
    v <- suppressWarnings(as.numeric(month_value))
    if (length(v) == 0 || is.na(v)) return(NA_real_)
    cur_ts <- ts(v, start = c(as.integer(format(Sys.Date(), "%Y")),
                              as.integer(format(Sys.Date(), "%m"))), frequency = 12)
    spi <- SPEI::spi(cur_ts, scale = 1, distribution = "Gamma", params = coef, na.rm = TRUE, verbose = FALSE)
    as.numeric(spi$fitted)[1]
  }, error = function(e) NA_real_)
}

reporte_spi_current_value <- function(vstate) {
  cm <- vstate$current_month %||% list()
  val <- cm$value %||% cm$sum %||% NA_real_
  reporte_spi_current(val, vstate$spi %||% list(), vstate$monthly_totals)
}

reporte_build_monthly_totals <- function(df, agg = "sum") {
  if (nrow(df) == 0) {
    return(list(totals = list(), current_month = list(ym = NA_character_, value = NA_real_, partial = TRUE, n_days = 0L)))
  }
  df <- df[order(df$fecha), , drop = FALSE]
  df$ym <- paste0(format(df$fecha, "%Y"), "-", sprintf("%02d", as.integer(format(df$fecha, "%m"))))
  if (agg == "sum") {
    agg_df <- stats::aggregate(valor ~ ym, df, FUN = function(v) sum(v, na.rm = TRUE))
  } else {
    agg_df <- stats::aggregate(valor ~ ym, df, FUN = function(v) mean(v, na.rm = TRUE))
  }
  totals <- stats::setNames(as.list(agg_df$valor), agg_df$ym)
  cur_ym <- paste0(format(Sys.Date(), "%Y"), "-", sprintf("%02d", as.integer(format(Sys.Date(), "%m"))))
  cur_rows <- df[df$ym == cur_ym, , drop = FALSE]
  cur_val <- if (nrow(cur_rows) == 0) NA_real_
  else if (agg == "sum") sum(cur_rows$valor, na.rm = TRUE)
  else mean(cur_rows$valor, na.rm = TRUE)
  list(
    totals = totals,
    current_month = list(
      ym = cur_ym, value = cur_val, sum = cur_val,
      partial = TRUE, n_days = nrow(cur_rows)
    )
  )
}

reporte_compute_precip_streaks <- function(df, threshold = 0) {
  empty <- list(
    max_dry_streak = 0L, max_wet_streak = 0L,
    max_dry_start = NA_character_, max_dry_end = NA_character_,
    max_wet_start = NA_character_, max_wet_end = NA_character_
  )
  if (nrow(df) == 0) return(empty)
  df <- df[order(df$fecha), , drop = FALSE]
  vals <- suppressWarnings(as.numeric(df$valor))
  fechas <- as.character(as.Date(df$fecha))

  best_dry <- 0L; best_wet <- 0L
  cur_dry <- 0L; cur_wet <- 0L
  dry_start <- NA_character_; wet_start <- NA_character_
  best_dry_start <- NA_character_; best_dry_end <- NA_character_
  best_wet_start <- NA_character_; best_wet_end <- NA_character_

  for (i in seq_along(vals)) {
    v <- vals[i]
    if (is.na(v)) {
      cur_dry <- 0L; cur_wet <- 0L
      dry_start <- NA_character_; wet_start <- NA_character_
      next
    }
    is_dry <- v <= threshold
    if (is_dry) {
      if (cur_dry == 0L) dry_start <- fechas[i]
      cur_dry <- cur_dry + 1L
      if (cur_dry > best_dry) {
        best_dry <- cur_dry
        best_dry_start <- dry_start
        best_dry_end <- fechas[i]
      }
      cur_wet <- 0L; wet_start <- NA_character_
    } else {
      if (cur_wet == 0L) wet_start <- fechas[i]
      cur_wet <- cur_wet + 1L
      if (cur_wet > best_wet) {
        best_wet <- cur_wet
        best_wet_start <- wet_start
        best_wet_end <- fechas[i]
      }
      cur_dry <- 0L; dry_start <- NA_character_
    }
  }
  list(
    max_dry_streak = best_dry, max_wet_streak = best_wet,
    max_dry_start = best_dry_start, max_dry_end = best_dry_end,
    max_wet_start = best_wet_start, max_wet_end = best_wet_end
  )
}

reporte_load_oni <- function() {
  paths <- c(
    file.path("/app", "data", "oni.csv"),
    file.path(getwd(), "data", "oni.csv")
  )
  p <- paths[file.exists(paths)][1]
  if (is.na(p)) return(NULL)
  df <- utils::read.csv(p, stringsAsFactors = FALSE)
  if (!all(c("year", "month", "oni") %in% names(df))) return(NULL)
  df
}

reporte_enso_correlation <- function(df, oni_df) {
  if (is.null(oni_df) || nrow(df) == 0) {
    return(list(r = NA_real_, n = 0L, available = FALSE,
                reason = paste0("Correlaci\u00f3n ENSO no disponible (n = 0; se requieren m\u00ednimo ",
                                REPORTE_MIN_MONTHS_ENSO, " meses).")))
  }
  df$year <- as.integer(format(df$fecha, "%Y"))
  df$month <- as.integer(format(df$fecha, "%m"))
  m <- merge(df[, c("year", "month", "valor")], oni_df, by = c("year", "month"))
  n <- nrow(m)
  if (n < REPORTE_MIN_MONTHS_ENSO) {
    return(list(
      r = NA_real_, n = n, available = FALSE,
      reason = paste0("Correlaci\u00f3n ENSO no disponible (n = ", n,
                      " meses; se requieren m\u00ednimo ", REPORTE_MIN_MONTHS_ENSO, ").")
    ))
  }
  list(
    r = suppressWarnings(cor(m$valor, m$oni, use = "complete.obs")),
    n = n, available = TRUE, reason = NA_character_
  )
}

reporte_build_availability <- function(vstate) {
  mk <- vstate$mann_kendall %||% list()
  spi <- vstate$spi %||% list()
  en <- vstate$enso %||% list()
  list(
    mk = list(
      available = isTRUE(mk$available),
      n_years = mk$n_years %||% length(vstate$annual %||% list()),
      reason = mk$reason
    ),
    spi = list(
      available = isTRUE(spi$available),
      n_months = spi$n_months %||% 0L,
      reason = spi$reason
    ),
    enso = list(
      available = isTRUE(en$available),
      n = en$n %||% 0L,
      reason = en$reason
    )
  )
}

reporte_curvas_pp <- function(vstate, fechas, valores) {
  if (!identical(as.character(vstate$nombre_abr %||% ""), "pp")) return(vstate)
  if (!exists("senales_adjuntar_curvas", mode = "function")) {
    p <- "/app/utils/senales_curvas.R"
    if (file.exists(p)) source(p, local = FALSE)
  }
  if (!exists("senales_adjuntar_curvas", mode = "function")) return(vstate)
  senales_adjuntar_curvas(vstate, fechas, valores)
}

reporte_build_variable_state <- function(df, var_meta, oni_df = NULL) {
  agg <- reporte_var_agg(var_meta$nombre_abr, var_meta$nombre)
  sp <- reporte_split_sin_validar(df)
  df_val <- sp$validado
  ma <- reporte_build_monthly_annual(df, agg)
  hists <- reporte_build_climatology_hists(df_val)
  ext <- reporte_build_extremes_from_df(df_val)
  yrs <- sort(as.integer(names(ma$annual)))
  ann_vals <- vapply(yrs, function(y) {
    a <- ma$annual[[as.character(y)]]
    if (agg == "sum") a$sum else a$mean
  }, numeric(1))
  mk <- reporte_mann_kendall(ann_vals, yrs)
  mt <- reporte_build_monthly_totals(df, agg)
  spi <- if (agg == "sum") reporte_spi_fit(mt$totals) else list(
    params = list(), last_fit = NA_character_, n_months = 0L,
    available = FALSE, reason = NA_character_
  )
  enso <- reporte_enso_correlation(df, oni_df)
  streaks <- if (agg == "sum" && nrow(df_val) > 0) reporte_compute_precip_streaks(df_val) else NULL
  if (!is.null(streaks)) ext$streaks <- streaks
  pct <- vapply(hists, function(h) {
    c(p10 = reporte_hist_percentile(h, 0.1),
      p50 = reporte_hist_percentile(h, 0.5),
      p90 = reporte_hist_percentile(h, 0.9))
  }, numeric(3))
  vstate <- list(
    id_variable = as.integer(var_meta$id),
    nombre = as.character(var_meta$nombre),
    nombre_abr = as.character(var_meta$nombre_abr),
    agg = agg,
    monthly = ma$monthly,
    annual = ma$annual,
    monthly_totals = mt$totals,
    current_month = mt$current_month,
    climatology_hists = hists,
    climatology_pct = as.data.frame(t(pct)),
    extremes = ext,
    mann_kendall = mk,
    spi = spi,
    enso = enso,
    sin_validar = sp$sin_validar,
    iqr = reporte_iqr_stats(df_val$valor),
    last_obs_date = sp$sin_validar$fecha
  )
  vstate <- reporte_curvas_pp(vstate, df_val$fecha, df_val$valor)
  vstate$availability <- reporte_build_availability(vstate)
  vstate
}

reporte_incremental_update_variable <- function(vstate, new_rows, oni_df = NULL, full_df = NULL) {
  if (nrow(new_rows) == 0) return(vstate)
  agg <- vstate$agg %||% "mean"
  for (i in seq_len(nrow(new_rows))) {
    m <- as.integer(format(new_rows$fecha[i], "%m"))
    yr <- as.integer(format(new_rows$fecha[i], "%Y"))
    val <- new_rows$valor[i]
    vstate$monthly[[m]] <- reporte_welford_update(vstate$monthly[[m]], val)
    yk <- as.character(yr)
    if (is.null(vstate$annual[[yk]])) {
      vstate$annual[[yk]] <- reporte_welford_init()
    }
    vstate$annual[[yk]] <- reporte_welford_update(vstate$annual[[yk]], val)
    vstate$climatology_hists[[m]] <- reporte_hist_update(vstate$climatology_hists[[m]], val)
  }
  if (!is.null(full_df) && nrow(full_df) > 0) {
    vstate <- reporte_apply_sin_validar(vstate, full_df)
  } else if (nrow(new_rows) > 0) {
    vstate$sin_validar <- list(
      fecha = as.character(max(new_rows$fecha)),
      valor = suppressWarnings(as.numeric(new_rows$valor[which.max(new_rows$fecha)]))
    )
    vstate$last_obs_date <- vstate$sin_validar$fecha
  }
  pct <- vapply(vstate$climatology_hists, function(h) {
    c(p10 = reporte_hist_percentile(h, 0.1),
      p50 = reporte_hist_percentile(h, 0.5),
      p90 = reporte_hist_percentile(h, 0.9))
  }, numeric(3))
  vstate$climatology_pct <- as.data.frame(t(pct))

  if (agg == "sum") {
    cur_ym <- paste0(format(Sys.Date(), "%Y"), "-", sprintf("%02d", as.integer(format(Sys.Date(), "%m"))))
    cur_sum <- 0
    cur_days <- 0L
    for (i in seq_len(nrow(new_rows))) {
      ym <- paste0(format(new_rows$fecha[i], "%Y"), "-", sprintf("%02d", as.integer(format(new_rows$fecha[i], "%m"))))
      if (ym == cur_ym) {
        cur_sum <- cur_sum + suppressWarnings(as.numeric(new_rows$valor[i]))
        cur_days <- cur_days + 1L
      }
      old <- vstate$monthly_totals[[ym]] %||% 0
      if (ym == cur_ym && !is.null(vstate$current_month$ym) && ym == vstate$current_month$ym) {
        vstate$current_month$value <- (vstate$current_month$value %||% 0) + suppressWarnings(as.numeric(new_rows$valor[i]))
        vstate$current_month$sum <- vstate$current_month$value
        vstate$current_month$n_days <- (vstate$current_month$n_days %||% 0L) + 1L
      } else if (ym != cur_ym) {
        vstate$monthly_totals[[ym]] <- old + suppressWarnings(as.numeric(new_rows$valor[i]))
      }
    }
  }
  vstate$availability <- reporte_build_availability(vstate)
  vstate
}

reporte_heavy_refresh_variable <- function(conn, eid, vstate) {
  vid <- vstate$id_variable
  df <- reporte_fetch_daily_series(conn, eid, vid)
  if (nrow(df) == 0) return(vstate)
  var_meta <- list(id = vid, nombre = vstate$nombre, nombre_abr = vstate$nombre_abr)
  oni <- reporte_load_oni()
  reporte_build_variable_state(df, var_meta, oni)
}
