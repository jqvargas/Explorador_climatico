# Graficos ggplot2 para reporte PDF (PNG persistidos en sections/media/).

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

if (!exists("RODAJA_PALETTE")) {
  if (!exists("REPORTE_MIN_YEARS_MK")) REPORTE_MIN_YEARS_MK <- 8L
  RODAJA_PALETTE <- c(brand = "#DF9531", dark = "#394553", water = "#378ADD",
                      green = "#1B7F4B", muted = "#8A95A0")
}

reporte_chart_theme <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = RODAJA_PALETTE["dark"], face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(color = RODAJA_PALETTE["muted"], size = 9),
      axis.title = ggplot2::element_text(color = RODAJA_PALETTE["dark"], size = 9),
      axis.text = ggplot2::element_text(color = RODAJA_PALETTE["muted"], size = 8),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#E5E8EB", linewidth = 0.3),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
}

reporte_chart_save <- function(p, path, width = 7, height = 3.5) {
  if (is.null(p)) return(invisible(FALSE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, plot = p, width = width, height = height, dpi = 150, bg = "white")
  invisible(file.exists(path))
}

reporte_clim_pct_val <- function(pct, m, col) {
  if (is.null(pct)) return(NA_real_)
  if (is.data.frame(pct) || is.matrix(pct)) {
    if (col %in% colnames(pct)) return(suppressWarnings(as.numeric(pct[m, col])))
    return(suppressWarnings(as.numeric(pct[m, match(col, c("p10", "p50", "p90"))])))
  }
  if (is.list(pct)) {
    row <- pct[[m]] %||% pct[[as.character(m)]]
    if (is.null(row)) return(NA_real_)
    return(suppressWarnings(as.numeric(row[[col]] %||% row[col])))
  }
  NA_real_
}

reporte_chart_ciclo <- function(vstate) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  pct <- vstate$climatology_pct
  meses <- 1:12
  mes_lbl <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  p10 <- vapply(meses, function(m) reporte_clim_pct_val(pct, m, "p10"), numeric(1))
  p50 <- vapply(meses, function(m) reporte_clim_pct_val(pct, m, "p50"), numeric(1))
  p90 <- vapply(meses, function(m) reporte_clim_pct_val(pct, m, "p90"), numeric(1))
  if (all(is.na(c(p10, p50, p90)))) return(NULL)

  df <- data.frame(
    mes = factor(mes_lbl, levels = mes_lbl),
    p10 = p10, p50 = p50, p90 = p90,
    stringsAsFactors = FALSE
  )
  ggplot2::ggplot(df, ggplot2::aes(x = mes, group = 1)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p10, ymax = p90), fill = RODAJA_PALETTE["water"], alpha = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = p50), color = RODAJA_PALETTE["brand"], linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(y = p50), color = RODAJA_PALETTE["brand"], size = 2) +
    ggplot2::labs(
      title = paste0("Ciclo estacional — ", vstate$nombre),
      subtitle = "Banda P10–P90 (sombreado) y mediana P50",
      x = NULL, y = if (identical(vstate$agg, "sum")) "mm" else "Valor"
    ) +
    reporte_chart_theme()
}

reporte_chart_downsample <- function(df, max_n = 800L) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df <- df[order(df$fecha), , drop = FALSE]
  if (nrow(df) <= max_n) return(df)
  idx <- unique(round(seq(1, nrow(df), length.out = max_n)))
  idx <- sort(unique(c(1L, nrow(df), idx)))
  df[idx, , drop = FALSE]
}

reporte_chart_iqr_layers <- function(p, iqr, xmin = -Inf, xmax = Inf) {
  if (!isTRUE(iqr$available %||% FALSE)) return(p)
  p +
    ggplot2::annotate("rect", xmin = xmin, xmax = xmax,
                      ymin = iqr$q1, ymax = iqr$q3,
                      fill = RODAJA_PALETTE["green"], alpha = 0.12) +
    ggplot2::geom_hline(yintercept = c(iqr$fence_lo, iqr$fence_hi),
                        linetype = "dashed", color = RODAJA_PALETTE["green"],
                        linewidth = 0.45, alpha = 0.75)
}

reporte_chart_tendencia <- function(vstate) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  yrs <- sort(as.integer(names(vstate$annual %||% list())))
  if (length(yrs) < 2) return(NULL)
  agg <- vstate$agg %||% "mean"
  vals <- vapply(yrs, function(y) {
    a <- vstate$annual[[as.character(y)]]
    if (agg == "sum") a$sum else a$mean
  }, numeric(1))
  df <- data.frame(year = yrs, value = vals)
  mk <- vstate$mann_kendall %||% list()
  sv <- vstate$sin_validar %||% list()
  last_yr <- if (!is.na(sv$fecha %||% NA)) as.integer(format(as.Date(sv$fecha), "%Y")) else NA_integer_
  iqr_ref <- if (!is.na(last_yr) && last_yr %in% yrs) {
    reporte_iqr_stats(vals[yrs != last_yr])
  } else {
    vstate$iqr %||% reporte_iqr_stats(vals)
  }
  outlier_yrs <- integer(0)
  if (isTRUE(iqr_ref$available)) {
    outlier_yrs <- yrs[vals < iqr_ref$fence_lo | vals > iqr_ref$fence_hi]
    if (!is.na(last_yr)) outlier_yrs <- setdiff(outlier_yrs, last_yr)
  }
  sin_val_yr <- if (!is.na(last_yr) && last_yr %in% yrs) last_yr else NA_integer_

  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = value))
  p <- reporte_chart_iqr_layers(p, iqr_ref)
  p <- p +
    ggplot2::geom_line(color = RODAJA_PALETTE["water"], linewidth = 0.8) +
    ggplot2::geom_point(color = RODAJA_PALETTE["water"], size = 2) +
    ggplot2::labs(
      title = paste0("Tendencia anual — ", vstate$nombre),
      subtitle = paste0(
        "Banda verde: IQR (Q1\u2013Q3); punteado: 1,5\u00d7IQR; rojo: at\u00edpicos; ",
        "naranja \u25c6: a\u00f1o con \u00faltimo dato sin validar"
      ),
      x = "A\u00f1o", y = if (agg == "sum") "mm/a\u00f1o" else "Promedio anual"
    ) +
    reporte_chart_theme()

  if (length(outlier_yrs) > 0) {
    p <- p + ggplot2::geom_point(
      data = df[df$year %in% outlier_yrs, , drop = FALSE],
      color = "#DC2626", size = 2.8, inherit.aes = FALSE,
      ggplot2::aes(x = year, y = value)
    )
  }
  if (!is.na(sin_val_yr)) {
    p <- p + ggplot2::geom_point(
      data = df[df$year == sin_val_yr, , drop = FALSE],
      color = RODAJA_PALETTE["brand"], size = 3.2, shape = 18,
      inherit.aes = FALSE, ggplot2::aes(x = year, y = value)
    )
  }

  if (isTRUE(mk$available) && !is.na(mk$sen_slope)) {
    x0 <- min(yrs); x1 <- max(yrs)
    y0 <- mk$sen_slope * (x0 - yrs[1]) + vals[1]
    y1 <- mk$sen_slope * (x1 - yrs[1]) + vals[1]
    trend_df <- data.frame(x = c(x0, x1), y = c(y0, y1))
    p <- p + ggplot2::geom_line(data = trend_df, ggplot2::aes(x = x, y = y),
                                color = RODAJA_PALETTE["brand"], linewidth = 0.9, linetype = "dashed",
                                inherit.aes = FALSE)
  }
  p
}

reporte_chart_extremos <- function(vstate, df = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  ext <- vstate$extremes %||% list(max = list(), min = list())
  iqr <- vstate$iqr %||% list()
  sv <- vstate$sin_validar %||% list()
  sv_fecha <- if (!is.na(sv$fecha %||% NA)) as.Date(sv$fecha) else as.Date(NA)

  if (is.null(df) || nrow(df) == 0) {
    if (length(ext$max) == 0 && length(ext$min) == 0) return(NULL)
    df <- do.call(rbind, lapply(c(ext$max, ext$min), function(e) {
      data.frame(fecha = as.Date(e$fecha), valor = e$valor, stringsAsFactors = FALSE)
    }))
  }

  df <- df[order(df$fecha), , drop = FALSE]
  df_val <- if (!is.na(sv_fecha)) df[df$fecha != sv_fecha, , drop = FALSE] else df
  last_pt <- if (!is.na(sv_fecha)) df[df$fecha == sv_fecha, , drop = FALSE] else NULL
  if (nrow(df_val) == 0 && (is.null(last_pt) || nrow(last_pt) == 0)) return(NULL)

  df_line <- reporte_chart_downsample(df_val)
  if (!is.null(last_pt) && nrow(last_pt) > 0) {
    df_line <- rbind(df_line, last_pt[1, , drop = FALSE])
  }
  base <- data.frame(fecha = as.Date(df_line$fecha), valor = df_line$valor)

  outliers <- NULL
  if (isTRUE(iqr$available) && nrow(df_val) > 0) {
    ol <- df_val[df_val$valor < iqr$fence_lo | df_val$valor > iqr$fence_hi, , drop = FALSE]
    if (nrow(ol) > 0) {
      outliers <- data.frame(fecha = as.Date(ol$fecha), valor = ol$valor)
    }
  }

  top <- do.call(rbind, lapply(ext$max, function(e) {
    data.frame(fecha = as.Date(e$fecha), valor = e$valor, tipo = "maximo")
  }))
  if (is.null(top) || nrow(top) == 0) top <- NULL

  p <- ggplot2::ggplot(base, ggplot2::aes(x = fecha, y = valor))
  p <- reporte_chart_iqr_layers(p, iqr)
  p <- p +
    ggplot2::geom_line(color = RODAJA_PALETTE["muted"], alpha = 0.55, linewidth = 0.4) +
    ggplot2::labs(
      title = paste0("Serie diaria — ", vstate$nombre),
      subtitle = paste0(
        "Banda verde: IQR (Q1\u2013Q3); punteado: 1,5\u00d7IQR; rojo: at\u00edpicos; ",
        "naranja \u25c6: \u00faltimo dato sin validar"
      ),
      x = NULL, y = if (identical(vstate$agg, "sum")) "mm" else "Valor"
    ) +
    reporte_chart_theme()

  if (!is.null(outliers) && nrow(outliers) > 0) {
    p <- p + ggplot2::geom_point(
      data = outliers, color = "#DC2626", size = 1.8, alpha = 0.85,
      inherit.aes = FALSE, ggplot2::aes(x = fecha, y = valor)
    )
  }
  if (!is.null(top) && nrow(top) > 0) {
    p <- p + ggplot2::geom_point(
      data = top, color = RODAJA_PALETTE["dark"], size = 2.5,
      inherit.aes = FALSE, ggplot2::aes(x = fecha, y = valor)
    )
  }
  if (!is.null(last_pt) && nrow(last_pt) > 0) {
    p <- p + ggplot2::geom_point(
      data = data.frame(fecha = as.Date(last_pt$fecha[1]), valor = last_pt$valor[1]),
      color = RODAJA_PALETTE["brand"], size = 3.2, shape = 18,
      inherit.aes = FALSE, ggplot2::aes(x = fecha, y = valor)
    )
  }
  p
}

reporte_osm_tile_frac <- function(lat, lon, zoom = 10L) {
  n <- 2^zoom
  lat_rad <- lat * pi / 180
  xtile_f <- (lon + 180) / 360 * n
  ytile_f <- (1 - log(tan(lat_rad) + 1 / cos(lat_rad)) / pi) / 2 * n
  xtile <- floor(xtile_f)
  ytile <- floor(ytile_f)
  xtile <- max(0, min(n - 1, xtile))
  ytile <- max(0, min(n - 1, ytile))
  list(
    zoom = zoom, xtile = xtile, ytile = ytile,
    fx = xtile_f - xtile, fy = ytile_f - ytile
  )
}

reporte_chart_mapa <- function(meta, out_path) {
  lat <- suppressWarnings(as.numeric(meta$lat))
  lon <- suppressWarnings(as.numeric(meta$lon))
  if (is.na(lat) || is.na(lon)) return(invisible(FALSE))

  tile <- reporte_osm_tile_frac(lat, lon, 10L)
  url <- sprintf("https://tile.openstreetmap.org/%d/%d/%d.png", tile$zoom, tile$xtile, tile$ytile)
  ok <- FALSE
  osm <- FALSE
  if (requireNamespace("httr", quietly = TRUE)) {
    ok <- tryCatch({
      resp <- httr::GET(url, httr::user_agent("rodaja.cl reporte/1.0"), httr::timeout(10))
      if (httr::status_code(resp) == 200) {
        dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
        writeBin(httr::content(resp, "raw"), out_path)
        osm <- TRUE
        TRUE
      } else FALSE
    }, error = function(e) FALSE)
  }
  if (!ok && requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = lon, y = lat), color = RODAJA_PALETTE["brand"], size = 4) +
      ggplot2::labs(title = paste0("Ubicaci\u00f3n (", round(lat, 3), ", ", round(lon, 3), ")"),
                    x = "Lon", y = "Lat") +
      reporte_chart_theme()
    reporte_chart_save(p, out_path, width = 4, height = 3)
    ok <- file.exists(out_path)
  }
  if (isTRUE(ok)) {
    jsonlite::write_json(list(osm = osm, fx = tile$fx, fy = tile$fy),
                         file.path(dirname(out_path), "map_pin.json"),
                         auto_unbox = TRUE)
  }
  invisible(ok)
}

reporte_render_charts <- function(eid, state, meta = NULL, conn = NULL,
                                  dirty_sections = NULL, log_fn = message) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    log_fn("[reporte] ggplot2 no disponible, sin graficos")
    return(invisible(character(0)))
  }
  media <- reporte_media_dir(eid)
  written <- character(0)
  all_charts <- is.null(dirty_sections)
  need_map <- all_charts || "00_portada" %in% dirty_sections

  if (need_map && !is.null(meta)) {
    mp <- file.path(media, "mapa_estacion.png")
    if (reporte_chart_mapa(meta, mp)) written <- c(written, mp)
  }

  for (vk in names(state$variables %||% list())) {
    vs <- state$variables[[vk]]
    vid <- vs$id_variable
    need_ciclo <- all_charts || "04_ciclo_estacional" %in% dirty_sections
    need_trend <- all_charts || "05_tendencias" %in% dirty_sections
    need_ext <- all_charts || "06_extremos" %in% dirty_sections

    if (need_ciclo) {
      p <- reporte_chart_ciclo(vs)
      path <- file.path(media, paste0("ciclo_", vid, ".png"))
      if (reporte_chart_save(p, path)) written <- c(written, path)
    }
    if (need_trend) {
      p <- reporte_chart_tendencia(vs)
      path <- file.path(media, paste0("tendencia_", vid, ".png"))
      if (reporte_chart_save(p, path)) written <- c(written, path)
    }
    if (need_ext) {
      df <- NULL
      if (!is.null(conn)) {
        df <- tryCatch(reporte_fetch_daily_series(conn, eid, vid), error = function(e) NULL)
      }
      p <- reporte_chart_extremos(vs, df)
      path <- file.path(media, paste0("extremos_", vid, ".png"))
      if (reporte_chart_save(p, path)) written <- c(written, path)
    }
  }
  invisible(written)
}

reporte_chart_media_ref <- function(vid, kind) {
  paste0("media/", kind, "_", vid, ".png")
}
