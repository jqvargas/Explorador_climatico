# ============================================================
# Modulo: Reporte Climatico por Estacion (ranking widgets)
# ============================================================

MESES_CORTOS <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
MESES_LARGOS <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

REPORTE_NORMAL_YEAR_START <- 1980L
REPORTE_NORMAL_YEAR_END <- 2010L
REPORTE_NORMAL_MIN_YEARS <- 5L
ULTIMAS_OBS_RANK_TOP_N <- 100L

# Agregacion que distingue NA (sin dato) de 0 (precipitacion nula valida).
reporte_agg_fn <- function(vtype) {
  if (identical(vtype, "precip")) {
    function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0L) return(NA_real_)
      sum(x)
    }
  } else {
    function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0L) return(NA_real_)
      mean(x)
    }
  }
}

EC_COBERTURA_MINIMA <- 0.70
# 70% entra al listado. Por debajo de 95% se marca incompleto y no cuenta
# en el puesto del ano en curso.
EC_COBERTURA_COMPLETA <- 0.95

reporte_aggregate_valor <- function(df, formula, vtype, last_date = NULL, min_coverage = EC_COBERTURA_MINIMA) {
  if (nrow(df) == 0) {
    vars <- all.vars(formula)
    empty <- setNames(data.frame(matrix(nrow = 0, ncol = length(vars))), vars)
    return(empty)
  }
  stats::aggregate(formula, data = df, FUN = reporte_agg_fn(vtype))
}

reporte_fmt_valor <- function(x) {
  if (length(x) == 0 || is.na(x)) return("\u2014")
  if (abs(x) >= 1000) {
    formatC(round(x, 0), format = "f", digits = 0, big.mark = ".")
  } else {
    sub("\\.", ",", sprintf("%.1f", x))
  }
}

reporte_scalar_chr <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L) return(default)
  if (is.list(x)) {
    if (!is.null(x$main)) return(reporte_scalar_chr(x$main, default))
    if (!is.null(x$label)) return(reporte_scalar_chr(x$label, default))
    if (!is.null(x$year)) return(reporte_scalar_chr(x$year, default))
    flat <- unlist(x, use.names = FALSE)
    if (length(flat) > 0L) return(reporte_scalar_chr(flat, default))
    return(default)
  }
  out <- unname(as.character(x)[1L])
  if (!nzchar(out) || identical(out, "NULL") || identical(out, "NA")) return(default)
  out
}

reporte_rank_json <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  r <- suppressWarnings(as.integer(unname(x))[1L])
  if (length(r) == 0L || is.na(r)) return(NULL)
  r
}

reporte_jsonify_labels <- function(x) {
  scalar_fields <- c(
    "label", "labelSub", "labelMain", "labelYear",
    "sectionLabel", "futureMsg", "incompleteMsg", "lastRecord", "valueVerb"
  )
  if (!is.list(x) || is.null(names(x))) return(x)
  for (nm in names(x)) {
    if (nm %in% scalar_fields) {
      x[[nm]] <- reporte_scalar_chr(x[[nm]])
    } else if (is.list(x[[nm]])) {
      x[[nm]] <- reporte_jsonify_labels(x[[nm]])
    }
  }
  x
}

reporte_fmt_fecha_larga <- function(d) {
  if (is.null(d) || length(d) == 0 || (length(d) == 1L && is.na(d))) return("")
  orig <- as.character(d)
  d <- suppressWarnings(as.Date(d))
  if (is.na(d)) return(orig)
  m <- as.integer(format(d, "%m"))
  sprintf("%d %s %s", as.integer(format(d, "%d")), tolower(MESES_CORTOS[m]), format(d, "%Y"))
}

reporte_fmt_dia_main <- function(d) {
  m <- as.integer(format(d, "%m"))
  sprintf("%d %s", as.integer(format(d, "%d")), tolower(MESES_CORTOS[m]))
}

reporte_month_display <- function(year_month) {
  parts <- strsplit(as.character(year_month), "-", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(list(main = year_month, sub = ""))
  y <- parts[1]
  m <- as.integer(parts[2])
  if (is.na(m) || m < 1 || m > 12) return(list(main = year_month, sub = ""))
  list(
    main = sprintf("%s %s", MESES_CORTOS[m], y),
    sub  = MESES_LARGOS[m]
  )
}

reporte_rank_top5 <- function(vals, labels, decreasing = TRUE, exclude_labels = character(0)) {
  ok <- !is.na(vals) & !is.na(labels)
  if (length(exclude_labels) > 0) {
    ok <- ok & !(labels %in% exclude_labels)
  }
  if (!any(ok)) return(NULL)
  v <- vals[ok]
  l <- labels[ok]
  ord <- order(v, decreasing = decreasing)
  top <- head(ord, 5)
  data.frame(
    rank  = seq_along(top),
    label = l[top],
    value = v[top],
    stringsAsFactors = FALSE
  )
}

reporte_rank_position <- function(vals, labels, target_label, decreasing = TRUE) {
  if (is.null(target_label) || !nzchar(as.character(target_label))) return(NA_integer_)
  ok <- !is.na(vals) & !is.na(labels)
  if (!any(ok)) return(NA_integer_)
  v <- vals[ok]
  l <- labels[ok]
  if (!(target_label %in% l)) return(NA_integer_)
  ord <- order(v, decreasing = decreasing)
  ranked <- l[ord]
  idx <- which(ranked == target_label)
  if (length(idx) == 0) NA_integer_ else as.integer(idx[1])
}

reporte_rank_forecast_position <- function(hist_vals, forecast_val, decreasing = TRUE) {
  v <- hist_vals[!is.na(hist_vals)]
  if (length(v) == 0 || is.na(forecast_val)) return(NA_integer_)
  v_all <- c(v, forecast_val)
  ord <- order(v_all, decreasing = decreasing)
  as.integer(which(ord == length(v_all))[1])
}

# Puesto del ano en curso solo entre anos con cobertura >= EC_COBERTURA_COMPLETA.
# El ano en curso no entra en n: se ubica entre los que si estan completos.
reporte_rank_among_complete <- function(ann_df, cur_label, cur_val, decreasing) {
  min_pct <- as.integer(round(EC_COBERTURA_COMPLETA * 100))
  cur_label <- as.character(cur_label)
  if (is.null(ann_df) || !nrow(ann_df) || !"coveragePct" %in% names(ann_df)) {
    return(list(rank = NA_integer_, n = 0L))
  }
  ok <- !is.na(ann_df$coveragePct) & ann_df$coveragePct >= min_pct &
    as.character(ann_df$label) != cur_label
  n <- as.integer(sum(ok))
  if (length(cur_val) == 0L || is.na(cur_val[1L])) return(list(rank = NA_integer_, n = n))
  pos <- reporte_rank_position(
    c(ann_df$valor[ok], cur_val[1L]),
    c(as.character(ann_df$label[ok]), cur_label),
    cur_label,
    decreasing
  )
  list(rank = pos, n = n)
}

reporte_attach_top_coverage <- function(top, labels, pct) {
  if (is.null(top) || !nrow(top) || is.null(pct) || !length(pct)) return(top)
  top$coveragePct <- pct[match(as.character(top$label), as.character(labels))]
  top
}

ALERTA_TEMPRANA_VAR_MAP <- list(
  pp      = list(widget = "maximos", direccion = "max"),
  t_max   = list(widget = "maximos", direccion = "max"),
  t_min   = list(widget = "minimos", direccion = "min"),
  u_max_diario  = list(widget = "maximos", direccion = "max")
)

reporte_alerta_temprana_widget <- function(vtype, nombre_abr = NULL, direccion = NULL) {
  abr <- tolower(trimws(nombre_abr %||% ""))
  dir <- tolower(trimws(direccion %||% ""))
  if (exists("alertas_widget_spec", mode = "function") && nzchar(abr) && nzchar(dir)) {
    spec <- alertas_widget_spec(abr, dir, vtype = vtype)
    if (!is.null(spec)) return(spec)
  }
  if (exists("alertas_specs_por_abr", mode = "function") && nzchar(abr) && !nzchar(dir)) {
    specs <- alertas_specs_por_abr(abr)
    if (length(specs) == 1L) {
      spec <- alertas_widget_spec(abr, specs[[1L]]$direccion, vtype = vtype)
      if (!is.null(spec)) return(spec)
    }
  }
  if (nzchar(abr) && abr %in% names(ALERTA_TEMPRANA_VAR_MAP)) {
    wid <- ALERTA_TEMPRANA_VAR_MAP[[abr]]$widget
    defs <- reporte_widget_defs(vtype)
    spec <- defs[vapply(defs, function(d) identical(d$id, wid), logical(1))]
    if (length(spec) > 0) return(spec[[1]])
  }
  defs <- reporte_widget_defs(vtype)
  if (vtype == "temp" && grepl("min", abr)) {
    spec <- defs[vapply(defs, function(d) identical(d$id, "minimos"), logical(1))]
    if (length(spec) > 0) return(spec[[1]])
  }
  spec <- defs[vapply(defs, function(d) identical(d$id, "maximos"), logical(1))]
  if (length(spec) > 0) return(spec[[1]])
  defs[[1]]
}

reporte_df_to_rows <- function(df, recent_fn = NULL) {
  if (is.null(df) || nrow(df) == 0) return(list())
  completa <- as.integer(round(EC_COBERTURA_COMPLETA * 100))
  lapply(seq_len(nrow(df)), function(i) {
    lbl <- reporte_scalar_chr(df$label[i])
    sub <- if ("labelSub" %in% names(df)) reporte_scalar_chr(df$labelSub[i]) else ""
    row <- list(
      rank     = as.integer(df$rank[i]),
      label    = lbl,
      value    = df$value[i],
      valueFmt = reporte_fmt_valor(df$value[i]),
      recent   = if (!is.null(recent_fn)) isTRUE(recent_fn(lbl)) else FALSE,
      labelSub = if (nzchar(sub)) sub else NULL
    )
    if ("coveragePct" %in% names(df) && !is.na(df$coveragePct[i])) {
      pct <- as.integer(df$coveragePct[i])
      row$coveragePct <- pct
      if (pct < completa) row$incompleteHist <- TRUE
    }
    row
  })
}

reporte_agg_ytd_by_year <- function(df, agg_fn, idx) {
  sel <- idx$is_ytd
  if (!any(sel)) return(data.frame(year = integer(), valor = numeric()))
  vals <- tapply(df$valor[sel], idx$year[sel], function(v) agg_fn(v, na.rm = TRUE))
  data.frame(
    year  = as.integer(names(vals)),
    valor = as.numeric(vals),
    stringsAsFactors = FALSE
  )
}

reporte_annual_aggregates <- function(df, agg_fn, anio_actual, last_date, idx) {
  vals <- tapply(df$valor, idx$year, agg_fn)
  ann <- data.frame(
    year  = as.integer(names(vals)),
    valor = as.numeric(vals),
    stringsAsFactors = FALSE
  )
  ann$label <- as.character(ann$year)

  cur_val <- ann$valor[ann$year == anio_actual]
  if (length(cur_val) == 0) cur_val <- NA_real_

  ann_ytd <- reporte_agg_ytd_by_year(df, agg_fn, idx)
  if (nrow(ann_ytd) > 0) ann_ytd$label <- as.character(ann_ytd$year)

  list(
    ann     = reporte_filter_ann_by_coverage(ann, idx, last_date),
    ann_ytd = reporte_filter_ann_by_coverage(ann_ytd, idx, last_date, ytd_ref = last_date),
    cur_val = cur_val
  )
}

reporte_build_annual_block <- function(agg, decreasing, anio_actual, last_date, unidad,
                                       value_verb = "lleva") {
  ann       <- agg$ann
  ann_ytd   <- agg$ann_ytd
  cur_val   <- agg$cur_val
  cur_label <- reporte_scalar_chr(anio_actual)
  value_verb <- if (identical(value_verb, "promedia")) "promedia" else "lleva"

  build_mode <- function(ann_df, exclude_label) {
    empty <- list(
      label = reporte_scalar_chr(cur_label), value = cur_val,
      valueFmt = reporte_fmt_valor(cur_val), rank = NULL, n = 0L
    )
    if (is.null(ann_df) || nrow(ann_df) == 0) {
      return(list(top5 = list(), current = empty))
    }
    top <- reporte_rank_top5(
      ann_df$valor, ann_df$label, decreasing,
      exclude_labels = exclude_label
    )
    if ("coveragePct" %in% names(ann_df)) {
      top <- reporte_attach_top_coverage(top, ann_df$label, ann_df$coveragePct)
    }
    among <- reporte_rank_among_complete(ann_df, cur_label, cur_val, decreasing)
    list(
      top5    = reporte_df_to_rows(top),
      current = list(
        label    = reporte_scalar_chr(cur_label),
        value    = cur_val,
        valueFmt = reporte_fmt_valor(cur_val),
        rank     = if (is.null(among$rank) || is.na(among$rank)) NULL else among$rank,
        n        = as.integer(among$n)
      )
    )
  }

  incomplete <- anio_actual == as.integer(format(last_date, "%Y")) &&
    last_date < as.Date(paste0(anio_actual, "-12-31"))

  comp <- build_mode(ann, cur_label)
  hasta <- build_mode(ann_ytd, cur_label)

  insight_completo <- reporte_insight_annual(cur_label, comp$current$rank, "completo", decreasing)
  insight_hasta    <- reporte_insight_annual(cur_label, hasta$current$rank, "hasta_hoy", decreasing)

  list(
    colLabelMid   = "A\u00f1o",
    colLabelRight = "Total",
    top5          = comp$top5,
    current       = list(
      label         = reporte_scalar_chr(cur_label),
      value         = cur_val,
      valueFmt      = reporte_fmt_valor(cur_val),
      rankCompleto  = reporte_rank_json(comp$current$rank),
      rankHastaHoy  = reporte_rank_json(hasta$current$rank),
      nAniosCompleto = as.integer(comp$current$n %||% 0L),
      nAniosHastaHoy = as.integer(hasta$current$n %||% 0L),
      valueVerb     = value_verb,
      incomplete    = incomplete,
      lastRecord    = reporte_fmt_fecha_larga(last_date),
      sectionLabel  = "A\u00f1o en curso",
      incompleteMsg = "A\u00f1o incompleto \u2014 la comparaci\u00f3n puede ser dispareja."
    ),
    compare = list(
      completo  = comp,
      hasta_hoy = hasta
    ),
    insights = list(
      completo  = insight_completo,
      hasta_hoy = insight_hasta
    )
  )
}

reporte_insight_annual <- function(year_label, rank, mode, decreasing) {
  if (is.null(rank) || is.na(rank)) {
    return(sprintf(
      "<strong>%s</strong> se encuentra dentro del rango hist\u00f3rico para este indicador.",
      year_label
    ))
  }
  ref <- if (mode == "hasta_hoy") {
    " \u2014 comparando cada a\u00f1o hasta la misma fecha del \u00faltimo registro."
  } else {
    " \u2014 usando totales anuales completos como referencia."
  }
  adj <- if (decreasing) "altos" else "bajos"
  if (rank == 1L) {
    return(sprintf(
      "<strong>%s</strong> ocupa el <strong>#1</strong> entre los a\u00f1os m\u00e1s %s registrados%s",
      year_label, adj, ref
    ))
  }
  sprintf(
    "<strong>%s</strong> acumula <strong>#%d</strong> entre los a\u00f1os m\u00e1s %s%s",
    year_label, rank, adj, ref
  )
}

reporte_monthly_all_aggregates <- function(df, agg_fn, last_date, idx) {
  agg_by_ym <- function(sel) {
    if (!any(sel)) {
      return(data.frame(month_label = character(), valor = numeric(), stringsAsFactors = FALSE))
    }
    v <- tapply(df$valor[sel], idx$ym[sel], agg_fn)
    data.frame(
      month_label = reporte_ym_label(as.integer(names(v))),
      valor = as.numeric(v),
      stringsAsFactors = FALSE
    )
  }

  mon <- agg_by_ym(rep(TRUE, nrow(df)))

  cur_val <- mon$valor[mon$month_label == format(last_date, "%Y-%m")]
  if (length(cur_val) == 0) cur_val <- NA_real_

  list(
    mon     = reporte_filter_months_by_coverage(mon, idx, last_date),
    mon_ytd = reporte_filter_months_by_coverage(agg_by_ym(idx$is_ytd), idx, last_date,
                                                ytd_ref = last_date),
    cur_val = cur_val
  )
}

reporte_build_monthly_all_block <- function(agg, decreasing, last_date, unidad) {
  mon       <- agg$mon
  mon_ytd   <- agg$mon_ytd
  cur_val   <- agg$cur_val
  cur_label <- format(last_date, "%Y-%m")

  enrich_top <- function(top_df) {
    if (is.null(top_df)) return(NULL)
    top_df$labelSub <- vapply(top_df$label, function(ml) {
      reporte_month_display(ml)$sub
    }, character(1))
    top_df$label <- vapply(top_df$label, function(ml) {
      reporte_month_display(ml)$main
    }, character(1))
    top_df
  }

  build_mode <- function(mon_df, exclude_label) {
    top <- reporte_rank_top5(
      mon_df$valor, mon_df$month_label, decreasing,
      exclude_labels = exclude_label
    )
    if (!is.null(top) && "coveragePct" %in% names(mon_df)) {
      top <- reporte_attach_top_coverage(top, mon_df$month_label, mon_df$coveragePct)
    }
    top <- enrich_top(top)
    pos <- if (!is.na(cur_val)) {
      reporte_rank_position(mon_df$valor, mon_df$month_label, cur_label, decreasing)
    } else NA_integer_
    list(
      top5 = reporte_df_to_rows(top),
      current = list(
        label = reporte_scalar_chr(reporte_month_display(cur_label)$main),
        labelSub = reporte_scalar_chr(reporte_month_display(cur_label)$sub),
        value = cur_val,
        valueFmt = reporte_fmt_valor(cur_val),
        rank = if (is.na(pos)) NULL else pos
      )
    )
  }

  last_day <- as.integer(format(last_date, "%d"))
  incomplete <- format(last_date, "%Y-%m") == format(Sys.Date(), "%Y-%m")

  comp <- build_mode(mon, cur_label)
  hasta <- build_mode(mon_ytd, cur_label)

  disp <- reporte_month_display(cur_label)
  insight_completo <- sprintf(
    "<strong>%s</strong> se ubica <strong>#%s</strong> entre todos los meses registrados \u2014 usando totales mensuales completos como referencia.",
    disp$main,
    if (is.null(comp$current$rank)) "\u2014" else comp$current$rank
  )
  insight_hasta <- sprintf(
    "<strong>%s</strong> se ubica <strong>#%s</strong> entre todos los meses registrados \u2014 comparando hasta el mismo d\u00eda del \u00faltimo registro.",
    disp$main,
    if (is.null(hasta$current$rank)) "\u2014" else hasta$current$rank
  )

  list(
    colLabelMid   = "Mes \u00b7 A\u00f1o",
    colLabelRight = "Total",
    top5          = comp$top5,
    current       = list(
      label         = reporte_scalar_chr(disp$main),
      labelSub      = reporte_scalar_chr(disp$sub),
      value         = cur_val,
      valueFmt      = reporte_fmt_valor(cur_val),
      rankCompleto  = reporte_rank_json(comp$current$rank),
      rankHastaHoy  = reporte_rank_json(hasta$current$rank),
      incomplete    = incomplete,
      lastRecord    = reporte_fmt_fecha_larga(last_date),
      sectionLabel  = "Mes en curso",
      incompleteMsg = sprintf(
        "Mes en curso \u2014 solo hay registros hasta el <strong>d\u00eda %d</strong>.",
        last_day
      )
    ),
    compare = list(completo = comp, hasta_hoy = hasta),
    insights = list(completo = insight_completo, hasta_hoy = insight_hasta)
  )
}

reporte_by_month_aggregates <- function(df, agg_fn, last_date, idx) {
  cy <- as.integer(format(last_date, "%Y"))
  lapply(1:12, function(m) {
    in_m <- idx$month == m
    if (!any(in_m)) return(NULL)
    agg_years <- function(sel) {
      if (!any(sel)) return(data.frame(year = integer(), valor = numeric()))
      v <- tapply(df$valor[sel], idx$year[sel], agg_fn)
      data.frame(year = as.integer(names(v)), valor = as.numeric(v), stringsAsFactors = FALSE)
    }

    ann <- agg_years(in_m)
    ann$label <- sprintf("%s %s", MESES_CORTOS[m], ann$year)
    cur_val <- ann$valor[ann$year == cy]
    if (length(cur_val) == 0) cur_val <- NA_real_

    ann <- reporte_filter_years_by_coverage(ann, idx, last_date, month = m)

    ay <- agg_years(in_m & idx$is_ytd)
    ann_ytd <- reporte_filter_years_by_coverage(
      ay, idx, last_date, ytd_ref = last_date, month = m
    )

    list(ann = ann, ann_ytd = ann_ytd, cur_val = cur_val)
  })
}

reporte_build_monthly_by_month <- function(aggs, decreasing, last_date) {
  cy <- as.integer(format(last_date, "%Y"))
  cm <- as.integer(format(last_date, "%m"))
  last_day <- as.integer(format(last_date, "%d"))

  by_month <- lapply(1:12, function(m) {
    agg <- aggs[[m]]
    if (is.null(agg)) {
      return(list(
        month = m,
        monthShort = MESES_CORTOS[m],
        monthLong = MESES_LARGOS[m],
        colLabelMid = sprintf("%s \u00b7 A\u00f1o", MESES_LARGOS[m]),
        top5 = list(),
        current = NULL
      ))
    }
    ann <- agg$ann
    ann_ytd <- agg$ann_ytd
    cur_val <- agg$cur_val
    cur_label <- reporte_scalar_chr(sprintf("%s %s", MESES_CORTOS[m], cy))

    status <- if (m > cm) "future"
    else if (m == cm) "current"
    else "past"

    exclude <- if (status == "current") as.character(cy) else character(0)
    top <- reporte_rank_top5(ann$valor, as.character(ann$year), decreasing, exclude_labels = exclude)
    if (!is.null(top)) {
      if ("coveragePct" %in% names(ann)) {
        top <- reporte_attach_top_coverage(top, as.character(ann$year), ann$coveragePct)
      }
      top$label <- sprintf("%s %s", MESES_CORTOS[m], top$label)
    }
    top_h <- reporte_rank_top5(
      ann_ytd$valor, as.character(ann_ytd$year), decreasing,
      exclude_labels = as.character(cy)
    )
    if (!is.null(top_h) && "coveragePct" %in% names(ann_ytd)) {
      top_h <- reporte_attach_top_coverage(top_h, as.character(ann_ytd$year), ann_ytd$coveragePct)
    }

    pos_c <- if (!is.na(cur_val) && status != "future") {
      reporte_rank_position(ann$valor, as.character(ann$year), as.character(cy), decreasing)
    } else NA_integer_
    pos_h <- if (!is.na(cur_val) && status != "future" && nrow(ann_ytd) > 0) {
      reporte_rank_position(ann_ytd$valor, as.character(ann_ytd$year), as.character(cy), decreasing)
    } else NA_integer_

    current <- if (status == "future") {
      list(
        status = "future",
        label = reporte_scalar_chr(sprintf("%s %d", MESES_LARGOS[m], cy)),
        sectionLabel = sprintf("%s %d", MESES_LARGOS[m], cy),
        lastRecord = reporte_fmt_fecha_larga(last_date),
        futureMsg = sprintf(
          "%s %d a\u00fan no comenz\u00f3. El puesto se actualizar\u00e1 cuando lleguen los datos.",
          MESES_LARGOS[m], cy
        )
      )
    } else if (status == "current") {
      list(
        status = "current",
        label = cur_label,
        value = cur_val,
        valueFmt = reporte_fmt_valor(cur_val),
        rankCompleto = reporte_rank_json(pos_c),
        rankHastaHoy = reporte_rank_json(pos_h),
        incomplete = TRUE,
        lastRecord = reporte_fmt_fecha_larga(last_date),
        sectionLabel = sprintf("%s %d \u00b7 en curso", MESES_LARGOS[m], cy),
        incompleteMsg = sprintf(
          "Mes en curso \u2014 solo registros hasta el <strong>d\u00eda %d</strong>.",
          last_day
        )
      )
    } else {
      NULL
    }

    list(
      month = m,
      monthShort = MESES_CORTOS[m],
      monthLong = MESES_LARGOS[m],
      colLabelMid = sprintf("%s \u00b7 A\u00f1o", MESES_LARGOS[m]),
      top5 = reporte_df_to_rows(top),
      current = current,
      compare = if (status == "current" && !is.na(cur_val)) {
        list(
          completo = list(
            top5 = reporte_df_to_rows(top),
            current = list(rank = pos_c)
          ),
          hasta_hoy = list(
            top5 = reporte_df_to_rows(top_h),
            current = list(rank = pos_h)
          )
        )
      } else NULL,
      insights = list(
        completo = sprintf(
          "El <strong>%s m\u00e1s extremo</strong> registrado fue en el per\u00edodo hist\u00f3rico analizado.",
          tolower(MESES_LARGOS[m])
        )
      )
    )
  })
  names(by_month) <- as.character(1:12)
  by_month
}

reporte_daily_aggregates <- function(df, agg_fn) {
  day <- stats::aggregate(valor ~ fecha, data = df, FUN = agg_fn)
  names(day) <- c("fecha", "valor")
  day$label <- as.character(day$fecha)
  day
}

reporte_build_daily_block <- function(day, decreasing, last_date, unidad) {
  top <- reporte_rank_top5(day$valor, day$label, decreasing)
  if (!is.null(top)) {
    top$labelMain <- vapply(top$label, function(lbl) {
      reporte_fmt_dia_main(as.Date(lbl))
    }, character(1))
    top$labelYear <- vapply(top$label, function(lbl) {
      format(as.Date(lbl), "%Y")
    }, character(1))
  }

  day_recent <- day[order(day$fecha, decreasing = TRUE), , drop = FALSE]
  recent <- head(day_recent, 5L)
  last_row <- if (nrow(recent) > 0) recent[1, , drop = FALSE] else day[which.max(day$fecha), , drop = FALSE]
  rows <- if (is.null(top)) list() else lapply(seq_len(nrow(top)), function(i) {
    list(
      rank = as.integer(top$rank[i]),
      labelMain = reporte_scalar_chr(top$labelMain[i]),
      labelYear = reporte_scalar_chr(top$labelYear[i]),
      value = top$value[i],
      valueFmt = reporte_fmt_valor(top$value[i])
    )
  })
  recent_rows <- if (nrow(recent) == 0) list() else lapply(seq_len(nrow(recent)), function(i) {
    f <- recent$fecha[i]
    list(
      date = reporte_fmt_fecha_larga(f),
      labelMain = reporte_fmt_dia_main(f),
      labelYear = format(f, "%Y"),
      value = recent$valor[i],
      valueFmt = reporte_fmt_valor(recent$valor[i]),
      isLatest = i == 1L
    )
  })

  list(
    colLabelMid   = "Fecha",
    colLabelRight = "Registro",
    top5          = rows,
    recentRecords = recent_rows,
    lastRecord    = list(
      date    = reporte_fmt_fecha_larga(last_row$fecha[1]),
      value   = last_row$valor[1],
      valueFmt = reporte_fmt_valor(last_row$valor[1]),
      unit    = unidad
    ),
    insight = if (length(rows) > 0) {
      sprintf(
        "El <strong>d\u00eda m\u00e1s extremo</strong> registrado fue el <strong>%s</strong> con %s %s.",
        reporte_fmt_fecha_larga(as.Date(top$label[1])),
        reporte_fmt_valor(top$value[1]),
        unidad
      )
    } else ""
  )
}

reporte_widget_defs <- function(vtype) {
  max_t <- switch(vtype,
    precip    = list(a = "A\u00f1os m\u00e1s lluviosos",    m = "Meses m\u00e1s lluviosos",    d = "D\u00edas m\u00e1s lluviosos"),
    temp_max  = list(a = "A\u00f1os m\u00e1s c\u00e1lidos",  m = "Meses m\u00e1s c\u00e1lidos",  d = "D\u00edas m\u00e1s c\u00e1lidos"),
    temp_min  = list(a = "A\u00f1os m\u00e1s c\u00e1lidos",  m = "Meses m\u00e1s c\u00e1lidos",  d = "D\u00edas m\u00e1s c\u00e1lidos"),
    temp_mean = list(a = "A\u00f1os m\u00e1s c\u00e1lidos",  m = "Meses m\u00e1s c\u00e1lidos",  d = "D\u00edas m\u00e1s c\u00e1lidos"),
    wind_max  = list(a = "A\u00f1os m\u00e1s ventosos",     m = "Meses m\u00e1s ventosos",     d = "D\u00edas m\u00e1s ventosos"),
    wind_mean = list(a = "A\u00f1os con mayor viento medio", m = "Meses con mayor viento medio", d = "D\u00edas con mayor viento medio"),
    wind_min  = list(a = "A\u00f1os m\u00e1s calmos",       m = "Meses m\u00e1s calmos",       d = "D\u00edas m\u00e1s calmos"),
    wind_kt   = list(a = "A\u00f1os con predominante m\u00e1s intenso", m = "Meses con predominante m\u00e1s intenso", d = "D\u00edas con predominante m\u00e1s intenso"),
    flow      = list(a = "A\u00f1os con mayor caudal",      m = "Meses con mayor caudal",      d = "D\u00edas con mayor caudal"),
    level     = list(a = "A\u00f1os con mayor nivel",       m = "Meses con mayor nivel",       d = "D\u00edas con mayor nivel"),
               list(a = "M\u00e1ximos anuales",             m = "M\u00e1ximos mensuales",        d = "M\u00e1ximos diarios")
  )
  min_t <- switch(vtype,
    precip    = list(a = "A\u00f1os m\u00e1s secos",        m = "Meses m\u00e1s secos",         d = ""),
    temp_max  = list(a = "A\u00f1os m\u00e1s fr\u00edos",   m = "Meses m\u00e1s fr\u00edos",    d = "D\u00edas m\u00e1s fr\u00edos"),
    temp_min  = list(a = "A\u00f1os m\u00e1s fr\u00edos",   m = "Meses m\u00e1s fr\u00edos",    d = "D\u00edas m\u00e1s fr\u00edos"),
    temp_mean = list(a = "A\u00f1os m\u00e1s fr\u00edos",   m = "Meses m\u00e1s fr\u00edos",    d = "D\u00edas m\u00e1s fr\u00edos"),
    wind_max  = list(a = "A\u00f1os menos ventosos",       m = "Meses menos ventosos",       d = "D\u00edas menos ventosos"),
    wind_mean = list(a = "A\u00f1os con menor viento medio", m = "Meses con menor viento medio", d = "D\u00edas con menor viento medio"),
    wind_min  = list(a = "A\u00f1os con menor viento m\u00ednimo", m = "Meses con menor viento m\u00ednimo", d = "D\u00edas con menor viento m\u00ednimo"),
    wind_kt   = list(a = "A\u00f1os con predominante m\u00e1s d\u00e9bil", m = "Meses con predominante m\u00e1s d\u00e9bil", d = "D\u00edas con predominante m\u00e1s d\u00e9bil"),
    flow      = list(a = "A\u00f1os con menor caudal",      m = "Meses con menor caudal",      d = "D\u00edas con menor caudal"),
    level     = list(a = "A\u00f1os con menor nivel",       m = "Meses con menor nivel",       d = "D\u00edas con menor nivel"),
               list(a = "M\u00ednimos anuales",             m = "M\u00ednimos mensuales",        d = "M\u00ednimos diarios")
  )
  skip_daily_min <- (vtype == "precip")

  list(
    list(id = "maximos", decreasing = TRUE,  skipDaily = FALSE,
         titleAnnual = max_t$a, titleMonthly = max_t$m, titleDaily = max_t$d),
    list(id = "minimos", decreasing = FALSE, skipDaily = skip_daily_min,
         titleAnnual = min_t$a, titleMonthly = min_t$m, titleDaily = min_t$d)
  )
}

reporte_widget_aggregates <- function(df, agg_fn, anio_actual, last_date, idx) {
  list(
    annual = reporte_annual_aggregates(df, agg_fn, anio_actual, last_date, idx),
    monthlyAll = reporte_monthly_all_aggregates(df, agg_fn, last_date, idx),
    byMonth = reporte_by_month_aggregates(df, agg_fn, last_date, idx),
    daily = reporte_daily_aggregates(df, agg_fn)
  )
}

reporte_build_multi_widget <- function(spec, aggs, anio_actual, last_date, unidad,
                                       value_verb = "lleva") {
  dec <- spec$decreasing
  list(
    id = spec$id,
    titleAnnual = spec$titleAnnual,
    titleMonthly = spec$titleMonthly,
    titleDaily = spec$titleDaily,
    decreasing = dec,
    skipDaily = isTRUE(spec$skipDaily),
    unit = unidad,
    defaultMonth = as.integer(format(last_date, "%m")),
    lastRecordDate = as.character(last_date),
    annual = reporte_build_annual_block(
      aggs$annual, dec, anio_actual, last_date, unidad, value_verb
    ),
    monthlyAll = reporte_build_monthly_all_block(aggs$monthlyAll, dec, last_date, unidad),
    monthlyByMonth = reporte_build_monthly_by_month(aggs$byMonth, dec, last_date),
    daily = reporte_build_daily_block(aggs$daily, dec, last_date, unidad)
  )
}

reporte_build_report_payload <- function(df, meta, info, sv, active_vid, variable_nombre_db,
                                         eid = NULL, df_pronostico = NULL) {
  variable_display <- if (exists("ec_variable_label", mode = "function")) {
    ec_variable_label(variable_nombre_db)
  } else {
    as.character(variable_nombre_db)
  }
  abr <- if (!is.null(meta) && !is.null(meta$nombre_abr)) as.character(meta$nombre_abr) else ""
  vtype <- reporte_detect_var_type(paste(abr, variable_nombre_db))
  agg_fn <- if (vtype == "precip") sum else mean

  df$fecha <- tryCatch(as.Date(df$fecha), error = function(e) as.Date(as.character(df$fecha)))
  if ("qc_flag" %in% names(df)) {
    n_flagged <- sum(!is.na(df$qc_flag) & df$qc_flag != 0L, na.rm = TRUE)
    df_ok <- df[(is.na(df$qc_flag) | df$qc_flag == 0L) & !is.na(df$valor) & !is.na(df$fecha), , drop = FALSE]
  } else {
    n_flagged <- 0L
    df_ok <- df[!is.na(df$valor) & !is.na(df$fecha), , drop = FALSE]
  }
  if (nrow(df_ok) == 0) return(NULL)
  df <- df_ok

  anio_actual <- as.integer(format(Sys.Date(), "%Y"))
  last_date   <- max(df$fecha, na.rm = TRUE)
  unidad      <- if (!is.null(meta) && !is.null(meta$unidad)) as.character(meta$unidad) else ""

  est_nombre <- if (!is.null(meta) && !is.null(meta$estacion_nombre)) as.character(meta$estacion_nombre) else ""
  periodo_str <- paste0(
    as.integer(format(min(df$fecha), "%Y")),
    "\u2013",
    as.integer(format(max(df$fecha), "%Y"))
  )

  alt_str <- if (!is.null(info) && !is.null(info$altura) && !is.na(info$altura) && nzchar(as.character(info$altura))) {
    paste0(info$altura, " m.s.n.m.")
  } else ""
  cuenca_str <- if (!is.null(info) && !is.null(info$cuenca) && !is.na(info$cuenca) && nzchar(as.character(info$cuenca))) {
    as.character(info$cuenca)
  } else ""
  region_str <- if (!is.null(info) && !is.null(info$nom_reg) && !is.na(info$nom_reg) && nzchar(as.character(info$nom_reg))) {
    as.character(info$nom_reg)
  } else ""
  meta_geo <- paste(Filter(nzchar, c(alt_str, cuenca_str, region_str)), collapse = " \u00b7 ")

  var_chips <- list()
  if (!is.null(sv) && is.data.frame(sv) && nrow(sv) > 0 && "id" %in% names(sv)) {
    sv_ok <- sv[!is.na(sv$id), ]
    for (i in seq_len(min(6L, nrow(sv_ok)))) {
      var_chips[[i]] <- list(
        id = sv_ok$id[i],
        nombre = ec_variable_label(as.character(sv_ok$nombre[i]), id = sv_ok$id[i]),
        active = !is.null(active_vid) && !is.na(active_vid) && sv_ok$id[i] == active_vid
      )
    }
  }

  idx <- reporte_date_index(df, last_date)
  aggs <- reporte_widget_aggregates(df, agg_fn, anio_actual, last_date, idx)
  defs <- reporte_widget_defs(vtype)
  value_verb <- if (identical(vtype, "precip")) "lleva" else "promedia"
  widgets <- lapply(defs, function(spec) {
    reporte_build_multi_widget(spec, aggs, anio_actual, last_date, unidad, value_verb)
  })

  insight_global <- reporte_generate_global_insight(widgets, anio_actual)
  clim <- reporte_build_normal_climatology(df, vtype)

  list(
    station = list(
      name = est_nombre,
      period = periodo_str,
      metaGeo = meta_geo,
      altitude = alt_str,
      cuenca = cuenca_str,
      region = region_str,
      variable = variable_display,
      variables = var_chips
    ),
    insightGlobal = insight_global,
    widgets = widgets,
    normalClimatology = clim,
    chart = reporte_compute_chart_data(df, variable_nombre_db, last_date, clim = clim, unidad = unidad),
    cicloAnual = reporte_compute_ciclo_anual(df, vtype, unidad, variable_display, last_date, clim = clim),
    tablaAnio = reporte_compute_tabla_anio_actual(
      df, anio_actual, unidad, vtype, last_date,
      df_pronostico = df_pronostico, variable = variable_display
    ),
    ultimasObservaciones = reporte_build_ultimas_observaciones(
      df, last_date, unidad, variable_display, vtype
    ),
    nFlaggedExcluded = as.integer(n_flagged)
  )
}

reporte_detect_var_type <- function(nombre) {
  raw <- enc2utf8(as.character(nombre %||% ""))
  n <- tryCatch(iconv(raw, from = "UTF-8", to = "ASCII//TRANSLIT"), error = function(e) raw)
  if (length(n) != 1L || is.na(n)) n <- raw
  n <- tolower(n)
  if (grepl("precip|lluvia|^pp$|^p$", n))          "precip"
  else if (grepl("temp.*max|tmax|t_max", n))        "temp_max"
  else if (grepl("temp.*min|tmin|t_min", n))        "temp_min"
  else if (grepl("temp", n))                        "temp_mean"
  else if (grepl("humedad|humidity|hr", n))         "humidity"
  else if (grepl("caudal|flujo|q_|streamflow", n))  "flow"
  else if (grepl("nivel|level", n))                 "level"
  else if (grepl("u_rafaga|u_rafaga_h|rafaga|viento_maximo_dmc|viento.*max|u_max_diario", n)) "wind_max"
  else if (grepl("viento.*min|u2_min", n))          "wind_min"
  else if (grepl("u_10_kt|predomin|viento_velocidad", n)) "wind_kt"
  else if (grepl("u2_mean|viento", n))              "wind_mean"
  else                                              "generic"
}

reporte_is_wind_vtype <- function(vtype) {
  as.character(vtype %||% "") %in% c("wind_max", "wind_mean", "wind_min", "wind_kt")
}

reporte_generate_global_insight <- function(widgets, anio_actual) {
  if (length(widgets) == 0) {
    return(sprintf(
      "Las condiciones de %d se encuentran dentro del rango hist\u00f3rico normal para esta estaci\u00f3n.",
      anio_actual
    ))
  }
  w <- widgets[[1]]
  ann <- w$annual
  if (!is.null(ann$current$rankCompleto) && !is.na(ann$current$rankCompleto)) {
    r <- ann$current$rankCompleto
    if (r == 1L) {
      return(sprintf("%d ocupa el primer puesto en el indicador principal del per\u00edodo actual.", anio_actual))
    }
    if (r <= 5L) {
      return(sprintf("%d se ubica en el puesto #%d entre los registros hist\u00f3ricos del indicador principal.", anio_actual, r))
    }
    return(sprintf("%d va en el puesto #%d del a\u00f1o.", anio_actual, r))
  }
  sprintf(
    "Las condiciones de %d se encuentran dentro del rango hist\u00f3rico normal para esta estaci\u00f3n.",
    anio_actual
  )
}

reporte_dias_esperados_anio <- function(year, last_date_in_data) {
  y <- as.integer(year)
  if (is.na(y)) return(0L)
  start <- as.Date(paste0(y, "-01-01"))
  end_full <- as.Date(paste0(y, "-12-31"))
  anio_actual <- as.integer(format(Sys.Date(), "%Y"))
  if (y > anio_actual) return(0L)
  if (y == anio_actual) {
    end <- min(c(Sys.Date(), end_full, last_date_in_data), na.rm = TRUE)
  } else {
    end <- end_full
  }
  if (is.na(end) || end < start) return(0L)
  as.integer(end - start) + 1L
}

reporte_dias_calendario_mes <- function(year, month) {
  y <- as.integer(year)
  m <- as.integer(month)
  if (is.na(y) || is.na(m) || m < 1L || m > 12L) return(0L)
  start <- as.Date(paste0(y, "-", sprintf("%02d", m), "-01"))
  end <- as.Date(format(start + 32, "%Y-%m-01")) - 1L
  as.integer(end - start) + 1L
}

reporte_dias_esperados_mes <- function(year, month, last_date = NULL) {
  y <- as.integer(year)
  m <- as.integer(month)
  if (is.na(y) || is.na(m) || m < 1L || m > 12L) return(0L)
  start <- as.Date(paste0(y, "-", sprintf("%02d", m), "-01"))
  end_full <- as.Date(format(start + 32, "%Y-%m-01")) - 1L
  hoy <- Sys.Date()
  ay <- as.integer(format(hoy, "%Y"))
  am <- as.integer(format(hoy, "%m"))
  if (y > ay || (y == ay && m > am)) return(0L)
  end <- if (y == ay && m == am) {
    min(c(hoy, end_full, last_date), na.rm = TRUE)
  } else {
    end_full
  }
  if (is.na(end) || end < start) return(0L)
  as.integer(end - start) + 1L
}

# Indice de fechas O(n) construido una vez por payload. Antes cada chequeo de
# cobertura re-escaneaba y formateaba el df completo, una vez por periodo:
# ~3000 pasadas de format() sobre 22k filas = 15 s de los 45 s del reporte.
reporte_date_index <- function(df, last_date) {
  fechas <- df$fecha
  lt <- as.POSIXlt(fechas)
  year <- lt$year + 1900L
  month <- lt$mon + 1L
  years <- sort(unique(year))

  ref_md <- format(as.Date(last_date), "%m-%d")
  # NA cuando el corte es 29-feb y el anio no es bisiesto: ese anio queda fuera
  # del modo "hasta hoy", igual que el calculo original por anio.
  cut_by_year <- suppressWarnings(as.Date(paste0(years, "-", ref_md)))
  cut_row <- cut_by_year[match(year, years)]
  is_ytd <- !is.na(cut_row) & fechas <= cut_row

  # Cobertura se mide en dias distintos: la serie cruda puede traer varias
  # observaciones por fecha cuando no hay jornada_cerrada.
  first <- !duplicated(fechas)
  tally <- function(keys) {
    t <- table(keys)
    list(k = as.integer(names(t)), n = as.integer(t))
  }
  fy <- year[first]
  fym <- (year * 100L + month)[first]
  fytd <- is_ytd[first]

  list(
    year = year,
    month = month,
    ym = year * 100L + month,
    is_ytd = is_ytd,
    years = years,
    ref_month = as.integer(format(as.Date(last_date), "%m")),
    ref_day = as.integer(format(as.Date(last_date), "%d")),
    ytd_year_ok = !is.na(cut_by_year),
    ytd_expected_anio = as.integer(cut_by_year - as.Date(paste0(years, "-01-01"))) + 1L,
    dias_anio = tally(fy),
    dias_anio_ytd = tally(fy[fytd]),
    dias_mes = tally(fym),
    dias_mes_ytd = tally(fym[fytd])
  )
}

reporte_idx_count <- function(tab, key) {
  i <- match(as.integer(key), tab$k)
  if (is.na(i)) 0L else tab$n[i]
}

reporte_ym_label <- function(ym) sprintf("%04d-%02d", ym %/% 100L, ym %% 100L)

# Rankings: drop periods below EC_COBERTURA_MINIMA (same gate as climatology).
# NA si el periodo no tiene dias esperados.
reporte_year_coverage_ratio <- function(idx, year, last_date, ytd_ref = NULL) {
  y <- as.integer(year)
  if (is.na(y) || is.null(idx)) return(NA_real_)
  if (!is.null(ytd_ref)) {
    i <- match(y, idx$years)
    if (is.na(i) || !idx$ytd_year_ok[i]) return(NA_real_)
    expected <- idx$ytd_expected_anio[i]
    dias <- reporte_idx_count(idx$dias_anio_ytd, y)
  } else {
    expected <- reporte_dias_esperados_anio(y, last_date)
    dias <- reporte_idx_count(idx$dias_anio, y)
  }
  if (is.na(expected) || expected <= 0L) return(NA_real_)
  dias / expected
}

reporte_month_coverage_ratio <- function(idx, year, month, last_date = NULL, ytd_ref = NULL) {
  y <- as.integer(year)
  m <- as.integer(month)
  if (is.na(y) || is.na(m) || is.null(idx)) return(NA_real_)
  if (!is.null(ytd_ref)) {
    i <- match(y, idx$years)
    if (is.na(i) || !idx$ytd_year_ok[i]) return(NA_real_)
    # Meses posteriores al de referencia no existen en el modo "hasta hoy".
    if (m > idx$ref_month) return(NA_real_)
    expected <- if (m == idx$ref_month) idx$ref_day else reporte_dias_calendario_mes(y, m)
    dias <- reporte_idx_count(idx$dias_mes_ytd, y * 100L + m)
  } else {
    expected <- reporte_dias_esperados_mes(y, m, last_date)
    dias <- reporte_idx_count(idx$dias_mes, y * 100L + m)
  }
  if (is.na(expected) || expected <= 0L) return(NA_real_)
  dias / expected
}

reporte_year_meets_coverage <- function(idx, year, last_date, ytd_ref = NULL,
                                        min_coverage = EC_COBERTURA_MINIMA) {
  r <- reporte_year_coverage_ratio(idx, year, last_date, ytd_ref = ytd_ref)
  !is.na(r) && r >= min_coverage
}

reporte_month_meets_coverage <- function(idx, year, month, last_date = NULL, ytd_ref = NULL,
                                         min_coverage = EC_COBERTURA_MINIMA) {
  r <- reporte_month_coverage_ratio(idx, year, month, last_date = last_date, ytd_ref = ytd_ref)
  !is.na(r) && r >= min_coverage
}

reporte_filter_years_by_coverage <- function(ann, idx, last_date, ytd_ref = NULL, month = NULL,
                                             min_coverage = EC_COBERTURA_MINIMA) {
  if (is.null(ann) || nrow(ann) == 0) return(ann)
  ratios <- if (is.null(month)) {
    vapply(ann$year, function(y) {
      reporte_year_coverage_ratio(idx, y, last_date, ytd_ref = ytd_ref)
    }, numeric(1))
  } else {
    vapply(ann$year, function(y) {
      reporte_month_coverage_ratio(idx, y, month, last_date = last_date, ytd_ref = ytd_ref)
    }, numeric(1))
  }
  keep <- !is.na(ratios) & ratios >= min_coverage
  ann <- ann[keep, , drop = FALSE]
  if (nrow(ann) > 0) ann$coveragePct <- as.integer(round(ratios[keep] * 100))
  ann
}

reporte_filter_ann_by_coverage <- function(ann, idx, last_date, ytd_ref = NULL,
                                           min_coverage = EC_COBERTURA_MINIMA) {
  reporte_filter_years_by_coverage(
    ann, idx, last_date, ytd_ref = ytd_ref, min_coverage = min_coverage
  )
}

reporte_filter_months_by_coverage <- function(mon, idx, last_date, ytd_ref = NULL,
                                              min_coverage = EC_COBERTURA_MINIMA) {
  if (is.null(mon) || nrow(mon) == 0) return(mon)
  ratios <- vapply(strsplit(as.character(mon$month_label), "-", fixed = TRUE), function(parts) {
    if (length(parts) < 2L) return(NA_real_)
    reporte_month_coverage_ratio(
      idx, as.integer(parts[1]), as.integer(parts[2]),
      last_date = last_date, ytd_ref = ytd_ref
    )
  }, numeric(1))
  keep <- !is.na(ratios) & ratios >= min_coverage
  mon <- mon[keep, , drop = FALSE]
  if (nrow(mon) > 0) mon$coveragePct <- as.integer(round(ratios[keep] * 100))
  mon
}

reporte_nullify_meses_futuros <- function(vec, max_month) {
  vec <- as.numeric(vec)
  if (length(vec) != 12L) return(vec)
  max_m <- as.integer(max_month)
  if (is.na(max_m) || max_m < 1L) return(rep(NA_real_, 12L))
  if (max_m < 12L) vec[(max_m + 1L):12L] <- NA_real_
  vec
}

reporte_cumsum_mensual <- function(vec) {
  vec <- as.numeric(vec)
  out <- rep(NA_real_, length(vec))
  running <- 0
  for (i in seq_along(vec)) {
    if (is.na(vec[i])) {
      out[i] <- NA_real_
    } else {
      running <- running + vec[i]
      out[i] <- running
    }
  }
  out
}

reporte_normal_period_label <- function(year_start, year_end) {
  sprintf("%d\u2013%d", as.integer(year_start), as.integer(year_end))
}

reporte_normal_suffix <- function(has_normal, year_start = REPORTE_NORMAL_YEAR_START,
                                  year_end = REPORTE_NORMAL_YEAR_END, vtype = NULL) {
  if (isTRUE(has_normal)) {
    return(paste0(" \u00b7 Normal ", reporte_normal_period_label(year_start, year_end)))
  }
  if (reporte_is_wind_vtype(vtype)) {
    return(paste0(" \u00b7 sin normal ", reporte_normal_period_label(year_start, year_end)))
  }
  ""
}

reporte_normal_gap_msg <- function(df, vtype, has_normal,
                                   year_start = REPORTE_NORMAL_YEAR_START,
                                   year_end = REPORTE_NORMAL_YEAR_END) {
  if (isTRUE(has_normal) || !reporte_is_wind_vtype(vtype)) return(NULL)
  years <- if (!is.null(df) && nrow(df) > 0 && "fecha" %in% names(df)) {
    as.integer(format(df$fecha[!is.na(df$fecha)], "%Y"))
  } else integer(0)
  years <- years[!is.na(years)]
  desde <- if (length(years)) min(years) else NA_integer_
  if (is.na(desde)) {
    sprintf("No hay muestra %s.", reporte_normal_period_label(year_start, year_end))
  } else {
    sprintf(
      "Serie desde %d; no hay muestra %s.",
      desde, reporte_normal_period_label(year_start, year_end)
    )
  }
}

reporte_build_normal_climatology <- function(df, vtype) {
  empty <- list(
    vtype = as.character(vtype %||% "generic"),
    minYears = REPORTE_NORMAL_MIN_YEARS,
    coverageMin = EC_COBERTURA_MINIMA,
    defaultStart = REPORTE_NORMAL_YEAR_START,
    defaultEnd = REPORTE_NORMAL_YEAR_END,
    years = integer(0),
    byYear = list()
  )
  if (is.null(df) || nrow(df) == 0) return(empty)
  sub <- df[!is.na(df$fecha) & !is.na(df$valor), , drop = FALSE]
  if (nrow(sub) == 0) return(empty)
  sub$year <- as.integer(format(sub$fecha, "%Y"))
  sub$month_num <- as.integer(format(sub$fecha, "%m"))
  years <- sort(unique(sub$year))
  agg_fn <- reporte_agg_fn(vtype)
  by_year <- stats::setNames(
    lapply(years, function(y) {
      vapply(1:12, function(m) {
        sub_y <- sub[sub$year == y & sub$month_num == m, , drop = FALSE]
        if (nrow(sub_y) == 0L) return(NA_real_)
        expected <- reporte_dias_calendario_mes(y, m)
        dias <- length(unique(sub_y$fecha))
        if (expected > 0L && dias / expected < EC_COBERTURA_MINIMA) return(NA_real_)
        agg_fn(sub_y$valor)
      }, numeric(1))
    }),
    as.character(years)
  )
  empty$years <- years
  empty$byYear <- by_year
  empty
}

reporte_norm_m_from_climatology <- function(clim, year_start, year_end,
                                            year_filter = NULL,
                                            min_years = REPORTE_NORMAL_MIN_YEARS) {
  na12 <- rep(NA_real_, 12L)
  if (is.null(clim) || length(clim$byYear %||% list()) == 0L) return(na12)
  vapply(1:12, function(m) {
    monthly <- vapply(names(clim$byYear), function(yn) {
      y <- as.integer(yn)
      if (!is.null(year_filter)) {
        if (!isTRUE(year_filter(y))) return(NA_real_)
      } else if (y < year_start || y > year_end) {
        return(NA_real_)
      }
      clim$byYear[[yn]][m]
    }, numeric(1))
    monthly <- monthly[!is.na(monthly)]
    if (length(monthly) < min_years) NA_real_ else mean(monthly)
  }, numeric(1))
}

reporte_annual_normal_scalar <- function(clim, year_start, year_end) {
  if (is.null(clim) || length(clim$byYear %||% list()) == 0L) return(NA_real_)
  is_precip <- identical(clim$vtype, "precip")
  annual <- vapply(names(clim$byYear), function(yn) {
    y <- as.integer(yn)
    if (y < year_start || y > year_end) return(NA_real_)
    vals <- clim$byYear[[yn]]
    vals <- vals[!is.na(vals)]
    if (length(vals) < 12L) return(NA_real_)
    if (is_precip) sum(vals) else mean(vals)
  }, numeric(1))
  annual <- annual[!is.na(annual)]
  min_y <- clim$minYears %||% REPORTE_NORMAL_MIN_YEARS
  if (length(annual) < min_y) return(NA_real_)
  mean(annual)
}

reporte_compute_vectors_mensuales <- function(df, vtype, last_date, anio_actual, anio_prev, clim = NULL) {
  na12 <- rep(NA_real_, 12L)
  if (is.null(df) || nrow(df) == 0) {
    return(list(curr_m = na12, prev_m = na12, hist_m = na12, norm_m = na12))
  }
  if (is.null(clim)) clim <- reporte_build_normal_climatology(df, vtype)
  df$year <- as.integer(format(df$fecha, "%Y"))
  df$month_num <- as.integer(format(df$fecha, "%m"))
  curr_df <- df[df$year == anio_actual, , drop = FALSE]
  prev_df <- df[df$year == anio_prev, , drop = FALSE]
  agg_fn <- reporte_agg_fn(vtype)

  safe_agg <- function(sub_df, m, ref_year = NULL) {
    sub_m <- sub_df[sub_df$month_num == m, , drop = FALSE]
    vals <- sub_m$valor[!is.na(sub_m$valor)]
    if (length(vals) == 0L) return(NA_real_)
    v <- agg_fn(vals)
    if (!is.null(ref_year)) {
      expected <- reporte_dias_esperados_mes(ref_year, m, last_date)
      dias <- length(unique(sub_m$fecha[!is.na(sub_m$valor)]))
      if (expected > 0L && dias / expected < EC_COBERTURA_MINIMA) return(NA_real_)
    }
    v
  }

  list(
    curr_m = vapply(1:12, function(m) safe_agg(curr_df, m, anio_actual), numeric(1)),
    prev_m = vapply(1:12, function(m) safe_agg(prev_df, m, anio_prev), numeric(1)),
    hist_m = reporte_norm_m_from_climatology(clim, 0L, anio_actual - 1L,
                                             year_filter = function(y) y < anio_actual),
    norm_m = reporte_norm_m_from_climatology(clim, REPORTE_NORMAL_YEAR_START, REPORTE_NORMAL_YEAR_END)
  )
}

reporte_ytd_cumsum_at_date <- function(df, year, target_date) {
  if (is.null(df) || nrow(df) == 0 || is.na(target_date)) return(NA_real_)
  y <- as.integer(year)
  sub <- df[as.integer(format(df$fecha, "%Y")) == y &
              !is.na(df$fecha) & df$fecha <= target_date, , drop = FALSE]
  vals <- sub$valor[!is.na(sub$valor)]
  if (length(vals) == 0L) return(NA_real_)
  sum(vals)
}

reporte_ytd_mean_at_date <- function(df, year, target_date) {
  if (is.null(df) || nrow(df) == 0 || is.na(target_date)) return(NA_real_)
  y <- as.integer(year)
  sub <- df[as.integer(format(df$fecha, "%Y")) == y &
              !is.na(df$fecha) & df$fecha <= target_date, , drop = FALSE]
  vals <- sub$valor[!is.na(sub$valor)]
  if (length(vals) == 0L) return(NA_real_)
  mean(vals)
}

reporte_same_calendar_date <- function(year, ref_date) {
  y <- as.integer(year)
  if (is.na(y) || is.na(ref_date)) return(NA)
  mes <- format(ref_date, "%m")
  dia <- format(ref_date, "%d")
  d <- suppressWarnings(as.Date(paste0(y, "-", mes, "-", dia)))
  if (is.na(d)) {
    m <- as.integer(format(ref_date, "%m"))
    d <- as.Date(paste0(y, "-", sprintf("%02d", m), "-28"))
  }
  d
}

reporte_normal_ytd_precip_at_date <- function(norm_m, year, target_date) {
  norm_m <- as.numeric(norm_m)
  if (length(norm_m) != 12L || is.na(target_date)) return(NA_real_)
  mes <- as.integer(format(target_date, "%m"))
  dia <- as.integer(format(target_date, "%d"))
  if (is.na(mes) || is.na(dia)) return(NA_real_)
  cum <- 0
  has <- FALSE
  if (mes > 1L) {
    prev <- norm_m[1:(mes - 1L)]
    prev <- prev[!is.na(prev)]
    if (length(prev) > 0L) {
      cum <- sum(prev)
      has <- TRUE
    }
  }
  if (!is.na(norm_m[mes])) {
    dias_mes <- reporte_dias_calendario_mes(year, mes)
    if (dias_mes > 0L) {
      cum <- cum + norm_m[mes] * (dia / dias_mes)
      has <- TRUE
    }
  }
  if (has) cum else NA_real_
}

reporte_normal_ytd_mean_at_date <- function(norm_m, year, target_date) {
  norm_m <- as.numeric(norm_m)
  if (length(norm_m) != 12L || is.na(target_date)) return(NA_real_)
  mes <- as.integer(format(target_date, "%m"))
  dia <- as.integer(format(target_date, "%d"))
  if (is.na(mes) || is.na(dia)) return(NA_real_)
  weighted <- 0
  total <- 0L
  for (m in 1:(mes - 1L)) {
    if (!is.na(norm_m[m])) {
      d_m <- reporte_dias_calendario_mes(year, m)
      weighted <- weighted + norm_m[m] * d_m
      total <- total + d_m
    }
  }
  if (!is.na(norm_m[mes])) {
    weighted <- weighted + norm_m[mes] * dia
    total <- total + dia
  }
  if (total > 0L) weighted / total else NA_real_
}

reporte_compute_ciclo_y_bounds <- function(df, vtype, mes_vigente, anio_actual, visible) {
  vals <- as.numeric(unlist(visible))
  vals <- vals[!is.na(vals) & is.finite(vals)]
  if (length(vals) == 0) return(list(suggestedMax = NA_real_, suggestedMin = NA_real_))
  vmax <- max(vals)
  vmin <- min(vals)
  if (identical(vtype, "precip")) {
    return(list(suggestedMax = max(vmax * 1.12, 2000), suggestedMin = 0))
  }
  pad <- (vmax - vmin) * 0.1
  if (!is.finite(pad) || pad == 0) pad <- abs(vmax) * 0.1
  if (!is.finite(pad)) pad <- 1
  list(suggestedMax = vmax + pad, suggestedMin = vmin - pad)
}

reporte_ciclo_attach_y_bounds <- function(out, df, vtype, mes_vigente, anio_actual, visible) {
  bounds <- reporte_compute_ciclo_y_bounds(df, vtype, mes_vigente, anio_actual, visible)
  if (!is.na(bounds$suggestedMax)) out$ySuggestedMax <- bounds$suggestedMax
  if (!is.na(bounds$suggestedMin)) out$ySuggestedMin <- bounds$suggestedMin
  out
}

reporte_compute_ciclo_mes_actual <- function(df, vtype, unidad, variable_display, last_date,
                                             norm_m, norm_meta, normal_suffix,
                                             anio_actual, anio_prev, mes_vigente) {
  mes_nombre <- MESES_LARGOS[mes_vigente]
  if (is.null(df) || nrow(df) == 0) return(NULL)

  df <- df[!is.na(df$fecha), , drop = FALSE]
  start_mes <- as.Date(paste0(anio_actual, "-", sprintf("%02d", mes_vigente), "-01"))
  end_mes_full <- as.Date(format(start_mes + 32, "%Y-%m-01")) - 1L
  hoy <- Sys.Date()
  dias_full <- seq(start_mes, end_mes_full, by = "1 day")
  labels <- as.character(as.integer(format(dias_full, "%d")))

  cap_actual <- if (anio_actual == as.integer(format(hoy, "%Y")) &&
                       mes_vigente == as.integer(format(hoy, "%m"))) {
    min(hoy, end_mes_full)
  } else {
    end_mes_full
  }

  currentDay <- if (hoy >= start_mes && hoy <= end_mes_full) {
    as.integer(format(hoy, "%d"))
  } else {
    NA_integer_
  }

  has_normal <- any(!is.na(norm_m))

  if (identical(vtype, "precip")) {
    acum_act <- vapply(dias_full, function(d) {
      if (d > cap_actual) return(NA_real_)
      reporte_ytd_cumsum_at_date(df, anio_actual, d)
    }, numeric(1))
    acum_prev <- vapply(dias_full, function(d) {
      d_prev <- reporte_same_calendar_date(anio_prev, d)
      if (is.na(d_prev)) NA_real_ else reporte_ytd_cumsum_at_date(df, anio_prev, d_prev)
    }, numeric(1))
    acum_norm <- if (has_normal) {
      vapply(dias_full, function(d) reporte_normal_ytd_precip_at_date(norm_m, anio_actual, d), numeric(1))
    } else {
      NULL
    }

    has_actual <- any(!is.na(acum_act))
    has_prev <- any(!is.na(acum_prev))
    has_norm <- !is.null(acum_norm) && any(!is.na(acum_norm))
    if (!has_actual && !has_prev && !has_norm) return(NULL)

    out <- list(
      variant = "pp",
      labels = labels,
      currentDay = currentDay,
      hasData = has_actual || has_prev || has_norm,
      title = sprintf("Precipitaci\u00f3n \u2014 %s %d", mes_nombre, anio_actual),
      subtitle = paste0("Acumulada anual diaria", normal_suffix),
      unidad = "mm",
      acumuladoDiario = acum_act,
      anioPasado = acum_prev
    )
    if (!is.null(acum_norm)) out$anioNormal <- acum_norm
    visible <- c(acum_act, acum_prev, acum_norm)
  } else {
    media_act <- vapply(dias_full, function(d) {
      if (d > cap_actual) return(NA_real_)
      reporte_ytd_mean_at_date(df, anio_actual, d)
    }, numeric(1))
    media_prev <- vapply(dias_full, function(d) {
      d_prev <- reporte_same_calendar_date(anio_prev, d)
      if (is.na(d_prev)) NA_real_ else reporte_ytd_mean_at_date(df, anio_prev, d_prev)
    }, numeric(1))
    media_norm <- if (has_normal) {
      vapply(dias_full, function(d) reporte_normal_ytd_mean_at_date(norm_m, anio_actual, d), numeric(1))
    } else {
      NULL
    }

    has_actual <- any(!is.na(media_act))
    has_prev <- any(!is.na(media_prev))
    has_norm <- !is.null(media_norm) && any(!is.na(media_norm))
    if (!has_actual && !has_prev && !has_norm) return(NULL)

    out <- list(
      variant = "generic",
      labels = labels,
      currentDay = currentDay,
      hasData = has_actual || has_prev || has_norm,
      title = sprintf("%s \u2014 %s %d", variable_display, mes_nombre, anio_actual),
      subtitle = paste0("Media anual diaria", normal_suffix),
      unidad = if (nzchar(unidad)) unidad else "",
      mediaAnioActual = media_act,
      mediaAnioPasado = media_prev
    )
    if (!is.null(media_norm)) out$mediaNormal <- media_norm
    visible <- c(media_act, media_prev, media_norm)
  }

  bounds <- reporte_compute_ciclo_y_bounds(df, vtype, mes_vigente, anio_actual, visible)
  if (!is.na(bounds$suggestedMax)) out$ySuggestedMax <- bounds$suggestedMax
  if (!is.na(bounds$suggestedMin)) out$ySuggestedMin <- bounds$suggestedMin
  out
}

reporte_ciclo_attach_mes_actual <- function(out, df, vtype, unidad, variable_display, last_date,
                                          norm_m, norm_meta, normal_suffix,
                                          anio_actual, anio_prev, mes_vigente) {
  mes_actual <- reporte_compute_ciclo_mes_actual(
    df, vtype, unidad, variable_display, last_date,
    norm_m, norm_meta, normal_suffix,
    anio_actual, anio_prev, mes_vigente
  )
  out$mesActual <- mes_actual
  out$cicloSchemaVersion <- 4L
  out$hasMesActual <- !is.null(mes_actual) && isTRUE(mes_actual$hasData %||% FALSE)
  out
}

reporte_compute_ciclo_anual <- function(df, vtype, unidad, variable_display, last_date, clim = NULL) {
  anio_actual <- as.integer(format(Sys.Date(), "%Y"))
  anio_prev   <- anio_actual - 1L
  mes_vigente <- as.integer(format(Sys.Date(), "%m"))
  meses <- MESES_CORTOS
  if (is.null(clim)) clim <- reporte_build_normal_climatology(df, vtype)

  vecs <- reporte_compute_vectors_mensuales(df, vtype, last_date, anio_actual, anio_prev, clim = clim)
  curr_m <- reporte_nullify_meses_futuros(vecs$curr_m, mes_vigente)
  prev_m <- vecs$prev_m
  norm_m <- vecs$norm_m
  has_normal <- any(!is.na(norm_m))
  norm_start <- REPORTE_NORMAL_YEAR_START
  norm_end <- REPORTE_NORMAL_YEAR_END
  normal_suffix <- reporte_normal_suffix(has_normal, norm_start, norm_end, vtype = vtype)
  normal_gap <- reporte_normal_gap_msg(df, vtype, has_normal, norm_start, norm_end)
  norm_meta <- list()

  ld <- suppressWarnings(as.Date(if (length(last_date) == 0L) NA else last_date[[1]]))
  month_partial <- FALSE
  if (length(ld) == 1L && !is.na(ld) &&
      as.integer(format(ld, "%Y")) == anio_actual &&
      as.integer(format(ld, "%m")) == mes_vigente) {
    inicio <- as.Date(sprintf("%04d-%02d-01", anio_actual, mes_vigente))
    fin <- seq(inicio, by = "month", length.out = 2L)[2] - 1L
    month_partial <- ld < fin
  }

  base <- list(
    anio = anio_actual,
    meses = meses,
    unidad = if (nzchar(unidad)) unidad else "",
    currentMonth = mes_vigente,
    monthPartial = isTRUE(month_partial),
    corte = reporte_fmt_fecha_larga(last_date),
    normalPeriod = list(start = norm_start, end = norm_end),
    hasNormal = isTRUE(has_normal)
  )
  if (!is.null(normal_gap)) base$normalGap <- normal_gap

  if (identical(vtype, "precip")) {
    parcial <- curr_m
    acum <- reporte_cumsum_mensual(parcial)
    acum <- reporte_nullify_meses_futuros(acum, mes_vigente)
    anio_pasado <- reporte_cumsum_mensual(prev_m)
    base$unidad <- "mm"
    out <- c(base, list(
      variant = "pp",
      title = sprintf("Precipitaci\u00f3n A\u00f1o %d", anio_actual),
      subtitle = paste0("Parcial y Acumulada", normal_suffix),
      parcialMensual = parcial,
      acumuladoMensual = acum,
      anioPasado = anio_pasado
    ))
    anio_normal <- if (has_normal) reporte_cumsum_mensual(norm_m) else NULL
    if (!is.null(anio_normal)) out$anioNormal <- anio_normal
    visible <- c(parcial, acum, anio_pasado, anio_normal)
    out <- reporte_ciclo_attach_y_bounds(out, df, vtype, mes_vigente, anio_actual, visible)
    return(reporte_ciclo_attach_mes_actual(
      out, df, vtype, unidad, variable_display, last_date,
      norm_m, norm_meta, normal_suffix, anio_actual, anio_prev, mes_vigente
    ))
  }

  media_actual <- curr_m
  out <- c(base, list(
    variant = "generic",
    variable = variable_display,
    title = sprintf("%s \u2014 Ciclo Anual %d", variable_display, anio_actual),
    subtitle = paste0("Media Mensual", normal_suffix),
    mediaAnioActual = media_actual,
    mediaAnioPasado = prev_m
  ))
  if (has_normal) out$mediaNormal <- norm_m
  visible <- c(media_actual, prev_m, norm_m)
  out <- reporte_ciclo_attach_y_bounds(out, df, vtype, mes_vigente, anio_actual, visible)
  reporte_ciclo_attach_mes_actual(
    out, df, vtype, unidad, variable_display, last_date,
    norm_m, norm_meta, normal_suffix, anio_actual, anio_prev, mes_vigente
  )
}

reporte_compute_tabla_anio_actual <- function(df, anio_actual, unidad, vtype = "generic",
                                              last_date = NULL, df_pronostico = NULL,
                                              variable = "") {
  hoy <- as.Date(Sys.time(), tz = "America/Santiago")
  ref_date <- if (!is.null(last_date) && length(last_date) > 0L && !is.na(last_date[[1]])) {
    as.Date(last_date[[1]])
  } else {
    hoy
  }
  yr <- as.integer(anio_actual)
  is_current_yr <- !is.na(yr) && !is.na(hoy) && yr == as.integer(format(hoy, "%Y"))
  ref_in_yr <- is_current_yr && !is.na(ref_date) && as.integer(format(ref_date, "%Y")) == yr
  marker_label <- if (!ref_in_yr) {
    NULL
  } else if (ref_date == hoy) {
    "Hoy"
  } else {
    "\u00daltimo dato"
  }
  if (is.null(vtype) || !nzchar(paste(vtype, collapse = ""))) vtype <- "generic"
  vtype <- as.character(vtype)[1]
  variable <- if (is.null(variable)) "" else as.character(variable)[1]

  lookup <- list()
  if (!is.null(df) && nrow(df) > 0) {
    sub <- df[!is.na(df$fecha) & as.integer(format(df$fecha, "%Y")) == yr, , drop = FALSE]
    if (nrow(sub) > 0) {
      sub <- sub[order(sub$fecha), , drop = FALSE]
      for (i in seq_len(nrow(sub))) {
        if (!is.na(sub$valor[i])) {
          lookup[[format(sub$fecha[i], "%m-%d")]] <- sub$valor[i]
        }
      }
    }
  }

  forecast_lookup <- list()
  has_forecast <- FALSE
  if (!is.null(df_pronostico) && nrow(df_pronostico) > 0) {
    for (i in seq_len(nrow(df_pronostico))) {
      fd <- tryCatch(as.Date(df_pronostico$fecha_objetivo[i]), error = function(e) NA)
      if (is.na(fd) || is.na(hoy) || fd < hoy) next
      val <- suppressWarnings(as.numeric(df_pronostico$valor[i]))
      if (is.na(val)) next
      if (as.integer(format(fd, "%Y")) != yr) next
      forecast_lookup[[format(fd, "%m-%d")]] <- val
      has_forecast <- TRUE
    }
  }

  cells <- vector("list", 31L)
  for (dia in 1:31L) {
    row <- vector("list", 12L)
    for (mes in 1:12L) {
      if (dia > reporte_dias_calendario_mes(yr, mes)) {
        row[[mes]] <- list(kind = "invalid", invalid = TRUE)
      } else {
        key <- sprintf("%02d-%02d", mes, dia)
        val <- lookup[[key]]
        if (!is.null(val)) {
          row[[mes]] <- list(
            kind = "obs",
            v = round(as.numeric(val), 1),
            fmt = as.character(reporte_fmt_valor(val))
          )
        } else {
          fc <- forecast_lookup[[key]]
          if (!is.null(fc) && is_current_yr) {
            row[[mes]] <- list(
              kind = "fc",
              v = round(as.numeric(fc), 1),
              fmt = as.character(reporte_fmt_valor(fc)),
              forecast = TRUE
            )
          } else {
            # ponytail: nunca row[[i]] <- NULL — en R elimina el slot y desalinea meses.
            # Futuro y el tramo posterior al pronóstico se ven igual (celda vacía);
            # la nota al pie dice hasta qué día llega Open-Meteo.
            cell_date <- as.Date(sprintf("%04d-%02d-%02d", yr, mes, dia))
            if (!is.na(ref_date) && !is.na(cell_date) && cell_date <= ref_date) {
              row[[mes]] <- list(kind = "missing", empty = TRUE)
            } else {
              row[[mes]] <- list(kind = "future", empty = TRUE)
            }
          }
        }
      }
    }
    cells[[dia]] <- row
  }

  list(
    hasData = length(lookup) > 0L || has_forecast,
    hasForecast = has_forecast,
    anio = yr,
    unidad = unidad,
    vtype = vtype,
    variable = variable,
    meses = MESES_CORTOS,
    todayDay = if (ref_in_yr) as.integer(format(ref_date, "%d")) else NA_integer_,
    todayMonth = if (ref_in_yr) as.integer(format(ref_date, "%m")) else NA_integer_,
    markerLabel = marker_label,
    cells = cells
  )
}

reporte_ultima_obs_accent <- function(vtype) {
  switch(vtype,
    precip    = list(color = "#F4B860", icon = "fa-tint", weather = "lluvia"),
    temp_max  = list(color = "#E2734B", icon = "fa-sun-o", weather = "soleado"),
    temp_min  = list(color = "#C4821E", icon = "fa-moon-o", weather = "nublado"),
    temp_mean = list(color = "#E2734B", icon = "fa-thermometer-half", weather = "soleado"),
    wind_max  = list(color = "#394553", icon = "fa-flag", weather = "nublado"),
    wind_mean = list(color = "#6E7C8C", icon = "fa-long-arrow-right", weather = "nublado"),
    wind_min  = list(color = "#6E7C8C", icon = "fa-flag-o", weather = "nublado"),
    wind_kt   = list(color = "#475569", icon = "fa-flag", weather = "nublado"),
    humidity  = list(color = "#6E7C8C", icon = "fa-tint", weather = "nublado"),
    flow      = list(color = "#6E7C8C", icon = "fa-tint", weather = "lluvia"),
    level     = list(color = "#6E7C8C", icon = "fa-bar-chart", weather = "lluvia"),
    list(color = "#F4B860", icon = "fa-line-chart", weather = "parcial-soleado")
  )
}

reporte_weather_icon_html <- function(file) {
  sprintf(
    '<img class="ec-rp-wx-icon" src="/icons/weather/%s.svg" alt="" aria-hidden="true" width="22" height="22">',
    htmltools::htmlEscape(as.character(file %||% "parcial-soleado"))
  )
}

reporte_var_chip_meta <- function(nombre) {
  vtype <- reporte_detect_var_type(nombre)
  accent <- reporte_ultima_obs_accent(vtype)
  list(vtype = vtype, weather = accent$weather)
}

reporte_spark_attr <- function(v) {
  sp <- v$spark
  if (is.null(sp)) return("")
  nums <- suppressWarnings(as.numeric(unlist(sp, use.names = FALSE)))
  nums <- nums[is.finite(nums)]
  if (length(nums) < 2L) return("")
  sprintf(' data-spark="%s"', paste(nums, collapse = ","))
}

reporte_week_trend_html <- function(cur, prev, vtype) {
  cur <- suppressWarnings(as.numeric(cur))
  prev <- suppressWarnings(as.numeric(prev))
  if (length(cur) == 0L || is.na(cur) || length(prev) == 0L || is.na(prev)) return("")
  d <- cur - prev
  cls <- "ec-rp-week-trend"
  arrow <- "\u2192"
  if (d > 1e-6) {
    cls <- paste(cls, "is-up")
    arrow <- "\u2191"
  } else if (d < -1e-6) {
    cls <- paste(cls, "is-down")
    arrow <- "\u2193"
  } else {
    cls <- paste(cls, "is-flat")
  }
  txt <- if (identical(vtype, "precip")) {
    sprintf("%s%s mm", if (d > 0) "+" else "", sub("\\.", ",", sprintf("%.1f", d)))
  } else {
    if (abs(prev) < 1e-9) return("")
    pct <- round((d / abs(prev)) * 100)
    sprintf("%s%d%%", if (pct > 0) "+" else "", as.integer(pct))
  }
  sprintf('<span class="%s">%s %s</span>', cls, arrow, txt)
}

reporte_week_bar_html <- function(cur, week_max, vtype = "generic") {
  if (!identical(vtype, "precip")) return("")
  cur <- suppressWarnings(as.numeric(cur))
  week_max <- suppressWarnings(as.numeric(week_max))
  if (length(cur) == 0L || is.na(cur) || length(week_max) == 0L || is.na(week_max) || week_max <= 0) return("")
  pct <- max(0, min(100, (cur / week_max) * 100))
  tip <- sprintf("%d %% del d\u00eda m\u00e1s lluvioso de la semana", as.integer(round(pct)))
  sprintf(
    '<div class="ec-rp-week-bar" title="%s" aria-hidden="true"><span style="width:%d%%"></span></div>',
    htmltools::htmlEscape(tip), as.integer(round(pct))
  )
}

# ponytail: umbrales fijos (0.1 / 2 / 20 mm), no la climatologia de la estacion.
reporte_weather_file_for_obs <- function(vtype, valor) {
  if (identical(vtype, "precip")) {
    v <- suppressWarnings(as.numeric(valor))
    if (length(v) == 0L || is.na(v) || v < 0.1) return("parcial-soleado")
    if (v < 2) return("llovizna")
    if (v < 20) return("lluvia")
    return("lluvia-fuerte")
  }
  reporte_ultima_obs_accent(vtype %||% "generic")$weather
}

reporte_card_wave_html <- function() {
  paste0(
    '<span class="ec-rp-card-aura" aria-hidden="true"></span>',
    '<svg class="ec-rp-card-wave" viewBox="0 0 200 40" preserveAspectRatio="none" aria-hidden="true">',
    '<path fill="currentColor" d="M0 24 Q 40 8 80 22 T 160 18 T 200 24 V 40 H 0 Z"/></svg>'
  )
}

reporte_fmt_dia_semana <- function(d) {
  dias <- c("Lunes", "Martes", "Mi\u00e9rcoles", "Jueves", "Viernes", "S\u00e1bado", "Domingo")
  n <- as.integer(format(as.Date(d), "%u"))
  if (is.na(n) || n < 1L || n > 7L) return("")
  dias[n]
}

reporte_ultimas_obs_decreasing <- function(vtype) {
  !identical(vtype, "temp_min")
}

reporte_fmt_rank_total <- function(n) {
  if (is.null(n) || length(n) == 0L || is.na(n) || n <= 0L) return("")
  prettyNum(as.numeric(n)[1L], big.mark = ".", scientific = FALSE)
}

reporte_ultima_obs_rank_html <- function(reg) {
  rank <- reg$rankHist %||% NULL
  if (is.null(rank) || length(rank) == 0L || is.na(rank)) return(NULL)
  rank_num <- as.integer(rank)[1L]
  if (is.na(rank_num) || rank_num < 1L) return(NULL)
  if (rank_num > ULTIMAS_OBS_RANK_TOP_N) return(NULL)
  rank_cls <- "ec-rp-ultima-obs-rank-num"
  if (rank_num <= 3L) rank_cls <- paste0(rank_cls, " is-top-", rank_num)
  medal <- if (rank_num <= 3L) {
    shiny::tags$span(class = paste0("ec-medal ec-medal--", rank_num), rank_num)
  }
  rank_span <- if (!is.null(medal)) {
    shiny::tags$span(class = rank_cls, medal)
  } else {
    shiny::tags$span(class = rank_cls, sprintf("#%d", rank_num))
  }
  total <- reg$rankTotal %||% NULL
  hint <- if (!is.null(total) && length(total) > 0L && !is.na(total) && as.integer(total) > 0L) {
    shiny::tags$span(
      class = "ec-rp-ultima-obs-rank-hint",
      sprintf("de %s d\u00edas", reporte_fmt_rank_total(total))
    )
  }
  aria <- sprintf("Puesto %d de %s d\u00edas hist\u00f3ricos", rank_num, reporte_fmt_rank_total(total))
  shiny::tags$div(class = "ec-rp-ultima-obs-rank-row", `aria-label` = aria, rank_span, hint)
}

reporte_hoy_cl <- function() {
  as.Date(Sys.time(), tz = "America/Santiago")
}

reporte_ultima_obs_frescura <- function(last_date) {
  dias <- suppressWarnings(as.integer(reporte_hoy_cl() - as.Date(last_date)))
  if (is.na(dias) || dias < 0L) return(list(fresh = FALSE, label = ""))
  if (dias == 0L) return(list(fresh = TRUE, label = "Hoy"))
  if (dias == 1L) return(list(fresh = TRUE, label = "Ayer"))
  if (dias <= 7L) return(list(fresh = TRUE, label = sprintf("Hace %d d\u00edas", dias)))
  if (dias <= 30L) return(list(fresh = FALSE, label = sprintf("Hace %d d\u00edas", dias)))
  list(fresh = FALSE, label = "")
}

reporte_build_ultimas_observaciones <- function(df, last_date, unidad, variable_display, vtype) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  agg_fn <- if (vtype == "precip") sum else mean
  day <- stats::aggregate(valor ~ fecha, data = df, FUN = agg_fn)
  names(day) <- c("fecha", "valor")
  day <- day[order(day$fecha, decreasing = TRUE), , drop = FALSE]
  recent <- head(day, 5L)
  if (nrow(recent) == 0) return(NULL)

  decreasing <- reporte_ultimas_obs_decreasing(vtype)
  day$labels <- as.character(as.Date(day$fecha))
  rank_total <- sum(!is.na(day$valor) & !is.na(day$labels))

  accent <- reporte_ultima_obs_accent(vtype)
  registros <- Filter(Negate(is.null), lapply(seq_len(nrow(recent)), function(i) {
    f <- recent$fecha[i]
    val <- recent$valor[i]
    if (length(val) == 0 || is.na(val)) return(NULL)
    frescura <- reporte_ultima_obs_frescura(f)
    fecha_label <- as.character(as.Date(f))
    rank_hist <- reporte_rank_position(day$valor, day$labels, fecha_label, decreasing)
    rank_hist_json <- reporte_rank_json(rank_hist)
    list(
      fecha = reporte_fmt_fecha_larga(f),
      fechaIso = as.character(as.Date(f)),
      labelMain = reporte_fmt_dia_main(f),
      labelYear = format(as.Date(f), "%Y"),
      weekday = reporte_fmt_dia_semana(f),
      valor = as.numeric(val),
      valueFmt = reporte_fmt_valor(val),
      diasAtras = suppressWarnings(as.integer(reporte_hoy_cl() - as.Date(f))),
      fresh = isTRUE(frescura$fresh),
      freshLabel = frescura$label,
      rankHist = rank_hist_json,
      rankTotal = as.integer(rank_total),
      rankFmt = if (!is.null(rank_hist_json)) sprintf("#%d", rank_hist_json) else NULL,
      isLatest = i == 1L
    )
  }))
  if (length(registros) == 0) return(NULL)

  list(
    variable = as.character(variable_display %||% ""),
    unidad = as.character(unidad %||% ""),
    vtype = as.character(vtype %||% "generic"),
    accentColor = accent$color,
    accentIcon = accent$icon,
    accentWeather = accent$weather,
    registros = registros
  )
}

reporte_compute_completeness_annual <- function(df) {
  anio_actual <- as.integer(format(Sys.Date(), "%Y"))
  empty <- list(
    labels = character(),
    values = numeric(),
    current_year = anio_actual,
    unit = "%",
    subtitle = "Cobertura diaria: d\u00edas con registro / d\u00edas del a\u00f1o"
  )
  if (is.null(df) || nrow(df) == 0) return(empty)
  df <- df[!is.na(df$fecha) & !is.na(df$valor), ]
  if (nrow(df) == 0) return(empty)

  df$year <- as.integer(format(df$fecha, "%Y"))
  last_date <- max(df$fecha, na.rm = TRUE)
  years <- sort(unique(df$year))

  days_count <- vapply(years, function(y) {
    length(unique(df$fecha[df$year == y]))
  }, numeric(1))
  expected <- vapply(years, function(y) {
    reporte_dias_esperados_anio(y, last_date)
  }, numeric(1))
  pct <- ifelse(expected > 0, round(100 * days_count / expected, 1), NA_real_)

  list(
    labels = as.character(years),
    values = as.numeric(pct),
    current_year = anio_actual,
    unit = "%",
    subtitle = "Cobertura diaria: d\u00edas con registro / d\u00edas del a\u00f1o"
  )
}

reporte_chart_rep <- function(v, n) {
  if (length(v) != 1L || is.na(v)) NULL else rep(v, n)
}

reporte_compute_chart_data <- function(df, variable_nombre, last_date = NULL, clim = NULL, unidad = "") {
  vtype  <- reporte_detect_var_type(variable_nombre)
  agg_fn <- if (vtype == "precip") sum else mean
  is_precip <- identical(vtype, "precip")

  df <- df[!is.na(df$valor) & !is.na(df$fecha), , drop = FALSE]
  if (is.null(last_date) || length(last_date) == 0L || is.na(last_date)) {
    last_date <- if (nrow(df) > 0) max(df$fecha, na.rm = TRUE) else Sys.Date()
  }
  last_date <- as.Date(last_date)
  anio_actual <- as.integer(format(Sys.Date(), "%Y"))
  anio_prev   <- anio_actual - 1L
  if (is.null(clim)) clim <- reporte_build_normal_climatology(df, vtype)
  norm_start <- REPORTE_NORMAL_YEAR_START
  norm_end <- REPORTE_NORMAL_YEAR_END
  normal_period <- list(start = norm_start, end = norm_end)
  unit <- as.character(unidad %||% "")
  coverage_min <- EC_COBERTURA_MINIMA * 100

  idx <- reporte_date_index(df, last_date)

  year_pct <- function(years, ytd = FALSE) {
    vapply(years, function(y) {
      y <- as.integer(y)
      if (isTRUE(ytd)) {
        i <- match(y, idx$years)
        if (is.na(i) || !idx$ytd_year_ok[i]) return(NA_real_)
        expected <- idx$ytd_expected_anio[i]
        dias <- reporte_idx_count(idx$dias_anio_ytd, y)
      } else {
        expected <- reporte_dias_esperados_anio(y, last_date)
        dias <- reporte_idx_count(idx$dias_anio, y)
      }
      if (is.na(expected) || expected <= 0) return(NA_real_)
      round(100 * dias / expected, 1)
    }, numeric(1))
  }

  ann_vals <- tapply(df$valor, idx$year, agg_fn)
  ann <- data.frame(
    year = as.integer(names(ann_vals)),
    valor = as.numeric(ann_vals),
    stringsAsFactors = FALSE
  )
  ann <- ann[order(ann$year), ]
  ann$coverage <- year_pct(ann$year, FALSE)
  ytd_df <- reporte_agg_ytd_by_year(df, agg_fn, idx)
  ann$ytd <- ytd_df$valor[match(ann$year, ytd_df$year)]
  ann$coverage_ytd <- year_pct(ann$year, TRUE)

  closed <- ann$year < anio_actual
  ok <- !is.na(ann$coverage) & ann$coverage >= coverage_min
  ok_ytd <- !is.na(ann$coverage_ytd) & ann$coverage_ytd >= coverage_min & !is.na(ann$ytd)
  h_v <- ann$valor[closed & ok]
  h_ytd <- ann$ytd[closed & ok_ytd]
  hist_mean_v <- if (length(h_v) > 0) mean(h_v) else NA_real_
  hist_mean_ytd <- if (length(h_ytd) > 0) mean(h_ytd) else NA_real_

  mv <- reporte_compute_vectors_mensuales(df, vtype, last_date, anio_actual, anio_prev, clim = clim)
  norm_m <- mv$norm_m
  norm_mean_v <- reporte_annual_normal_scalar(clim, norm_start, norm_end)
  norm_ytd_v <- if (is_precip) {
    reporte_normal_ytd_precip_at_date(norm_m, anio_actual, last_date)
  } else {
    reporte_normal_ytd_mean_at_date(norm_m, anio_actual, last_date)
  }

  partial_year <- anio_actual == as.integer(format(last_date, "%Y")) &&
    last_date < as.Date(paste0(anio_actual, "-12-31"))
  start_m <- as.Date(format(last_date, "%Y-%m-01"))
  end_m <- as.Date(format(start_m + 32, "%Y-%m-01")) - 1L
  partial_month <- last_date < end_m
  n_ann <- nrow(ann)

  annual_data <- list(
    labels = as.character(ann$year),
    current = ann$valor,
    ytd = ann$ytd,
    coverage = ann$coverage,
    coverage_ytd = ann$coverage_ytd,
    hist_mean = reporte_chart_rep(hist_mean_v, n_ann),
    hist_mean_ytd = reporte_chart_rep(hist_mean_ytd, n_ann),
    normal_mean = reporte_chart_rep(norm_mean_v, n_ann),
    normal_mean_ytd = reporte_chart_rep(norm_ytd_v, n_ann),
    current_year = anio_actual,
    last_date = as.character(last_date),
    last_label = reporte_fmt_fecha_larga(last_date),
    partial_year = isTRUE(partial_year),
    partial_month = isTRUE(partial_month),
    last_month = as.integer(format(last_date, "%m")),
    coverage_min = coverage_min,
    unit = unit,
    normalPeriod = normal_period
  )

  mon_vals <- tapply(df$valor, idx$ym, agg_fn)
  mon <- data.frame(
    ym = as.integer(names(mon_vals)),
    valor = as.numeric(mon_vals),
    stringsAsFactors = FALSE
  )
  y0 <- min(mon$ym) %/% 100L
  m0 <- min(mon$ym) %% 100L
  y1 <- as.integer(format(last_date, "%Y"))
  m1 <- as.integer(format(last_date, "%m"))
  grid_year <- integer(0)
  grid_month <- integer(0)
  for (y in y0:y1) {
    ms <- 1:12
    if (y == y0) ms <- ms[ms >= m0]
    if (y == y1) ms <- ms[ms <= m1]
    grid_year <- c(grid_year, rep(y, length(ms)))
    grid_month <- c(grid_month, ms)
  }
  grid_ym <- grid_year * 100L + grid_month
  month_pct <- vapply(seq_along(grid_ym), function(i) {
    expected <- reporte_dias_esperados_mes(grid_year[i], grid_month[i], last_date)
    dias <- reporte_idx_count(idx$dias_mes, grid_ym[i])
    if (is.na(expected) || expected <= 0) return(NA_real_)
    round(100 * dias / expected, 1)
  }, numeric(1))

  list(
    annual = annual_data,
    monthly = list(
      labels = paste(MESES_CORTOS[grid_month], grid_year),
      current = mon$valor[match(grid_ym, mon$ym)],
      month = grid_month,
      year = grid_year,
      coverage = month_pct,
      hist_mean = mv$hist_m[grid_month],
      normal_mean = if (any(!is.na(norm_m))) norm_m[grid_month] else NULL,
      current_year = anio_actual,
      prev_year = anio_prev,
      last_date = as.character(last_date),
      last_label = reporte_fmt_fecha_larga(last_date),
      partial_month = isTRUE(partial_month),
      last_month = as.integer(format(last_date, "%m")),
      coverage_min = coverage_min,
      unit = unit,
      normalPeriod = normal_period
    ),
    normalPeriod = normal_period,
    completeness = reporte_compute_completeness_annual(df)
  )
}

# ---- UI helpers ----
reporte_station_art_html <- function() {
  shiny::tags$div(
    class = "ec-rp-station-art",
    shiny::tags$img(
      src = "img/station-dish.png",
      class = "ec-rp-station-art-img",
      alt = "",
      `aria-hidden` = "true",
      loading = "lazy",
      decoding = "async"
    )
  )
}

reporte_header_meta_badge <- function(icon_class, text) {
  if (is.null(text) || !nzchar(trimws(as.character(text)))) return(NULL)
  shiny::tags$span(class = "ec-rp-meta-badge",
    shiny::tags$span(class = "ec-rp-meta-icon",
      shiny::tags$i(class = paste("fa", icon_class), `aria-hidden` = "true")
    ),
    shiny::tags$span(class = "ec-rp-meta-text", as.character(text))
  )
}

# Misma redaccion que EC_CALIDAD_TIP / EC_QC_TIP en nginx/html/mi-reporte/app.js.
.REPORTE_CALIDAD_TIP <- paste(
  "Puntaje de calidad del dato de la estaci\u00f3n. Combina: datos al d\u00eda (30 %),",
  "d\u00edas con dato (25 %), a\u00f1os sin cortes (20 %), variables medidas (10 %),",
  "lecturas descartadas (10 %) y volumen de registros (5 %)."
)
.REPORTE_QC_TIP <- paste(
  "Lecturas que el control de calidad descart\u00f3 en todas las variables de la estaci\u00f3n",
  "(imposibles f\u00edsicamente o falsas seg\u00fan estaciones vecinas). No se usan en el",
  "reporte ni en las alertas. Bajo 1 % no afectan la calidad del dato."
)

reporte_q_html <- function(tip) {
  shiny::tags$span(class = "ec-rp-q", tabindex = "0", title = tip, `aria-label` = tip, "?")
}

reporte_qc_range_text <- function(n_imp, p_imp) {
  pct_part <- if (is.na(p_imp) || p_imp < 0.1) {
    "menos de 0,1 % del total"
  } else {
    paste0(sub("\\.", ",", sprintf("%.1f", p_imp)), " % del total")
  }
  noun <- if (n_imp == 1L) "valor fuera de rango" else "valores fuera de rango"
  paste0(n_imp, " ", noun, " \u00b7 ", pct_part)
}

reporte_header_html <- function(st, var_chips_html) {
  badges <- list(
    reporte_header_meta_badge("fa-calendar", st$period),
    reporte_header_meta_badge("fa-arrows-v", st$altitude),
    reporte_header_meta_badge("fa-sitemap", st$cuenca),
    reporte_header_meta_badge("fa-map-marker", st$region)
  )
  badges <- Filter(Negate(is.null), badges)
  if (length(badges) == 0L && nzchar(st$metaGeo %||% "")) {
    badges <- list(shiny::tags$span(class = "ec-rp-meta-badge", st$metaGeo))
  }

  calidad_colors <- c(excelente = "#1B7F4B", buena = "#6E7C8C", regular = "#EAB308", limitada = "#DC2626")
  calidad_badge <- NULL
  if (nzchar(st$calidadNivel %||% "")) {
    niv <- as.character(st$calidadNivel)
    col <- calidad_colors[[tolower(niv)]] %||% "#8A95A0"
    score <- st$calidadScore
    score_txt <- if (!is.null(score) && !is.na(suppressWarnings(as.numeric(score)))) {
      paste0(" ", score, "/100")
    } else ""
    calidad_badge <- shiny::tags$span(
      class = "ec-rp-calidad-badge",
      style = paste0("--ec-cal-color:", col, ";"),
      shiny::tags$span(class = "ec-rp-calidad-dot"),
      paste0(tools::toTitleCase(niv), score_txt),
      reporte_q_html(.REPORTE_CALIDAD_TIP)
    )
  }

  qc_alert <- NULL
  n_imp <- suppressWarnings(as.integer(st$nImposibles))
  p_imp <- suppressWarnings(as.numeric(st$pctImposibles))
  if (!is.na(n_imp) && n_imp > 0L) {
    # ponytail: naranja solo desde 1 %; bajo eso el descarte no mueve el puntaje
    warn <- !is.na(p_imp) && p_imp >= 1
    qc_alert <- shiny::tags$span(
      class = if (warn) "ec-rp-qc-alert is-warn" else "ec-rp-qc-alert",
      role = "status",
      shiny::tags$span(class = "ec-rp-qc-alert-body", reporte_qc_range_text(n_imp, p_imp)),
      reporte_q_html(.REPORTE_QC_TIP)
    )
  }

  src_tag <- if (nzchar(st$fuente %||% "")) {
    shiny::tags$span(class = "ec-rp-src-tag", as.character(st$fuente))
  }

  download_cta <- NULL
  eid <- suppressWarnings(as.integer(st$eid))
  vid <- suppressWarnings(as.integer(st$vid))
  if (!is.na(eid) && eid > 0L) {
    dl_qs <- if (!is.na(vid) && vid > 0L) sprintf("?e=%d&v=%d", eid, vid) else sprintf("?e=%d", eid)
    download_cta <- shiny::tags$div(class = "ec-rp-header-aside",
      shiny::tags$a(
        class = "ec-rp-download-btn",
        href = paste0("/descargar/index.html", dl_qs),
        target = "_blank",
        rel = "noopener",
        shiny::tags$i(class = "fa fa-download", `aria-hidden` = "true"),
        " Descargar"
      ),
      shiny::tags$button(
        type = "button",
        class = "ec-rp-download-btn ec-rp-vista-pdf-btn",
        onclick = "ecExportReportePdf({root: this.closest('.ec-reporte-contenido'), button: this}); return false;",
        shiny::tags$i(class = "fa fa-file-pdf-o", `aria-hidden` = "true"),
        " Exportar PDF"
      )
    )
  }

  active_var <- st$variable %||% ""
  vars_panel <- if (nzchar(var_chips_html)) {
    shiny::tags$div(class = "ec-rp-header-vars-panel",
      shiny::tags$div(class = "ec-rp-vars-heading",
        shiny::tags$span(class = "ec-rp-vars-label", "Variables disponibles"),
        if (nzchar(active_var)) {
          shiny::tags$span(class = "ec-rp-vars-active", active_var)
        }
      ),
      shiny::tags$div(class = "ec-rp-vars", shiny::HTML(var_chips_html))
    )
  }

  shiny::tags$div(class = "ec-rp-header",
    shiny::tags$div(class = "ec-rp-header-deco", `aria-hidden` = "true"),
    shiny::tags$div(class = "ec-rp-header-grid",
      shiny::tags$div(class = "ec-rp-header-main",
        reporte_station_art_html(),
        shiny::tags$div(class = "ec-rp-header-text",
          shiny::tags$div(class = "ec-rp-station-title-row",
            shiny::tags$h2(class = "ec-rp-station-name", st$name),
            src_tag,
            calidad_badge
          ),
          if (length(badges) > 0L || !is.null(qc_alert)) {
            interleaved <- list()
            for (i in seq_along(badges)) {
              if (i > 1L) {
                interleaved[[length(interleaved) + 1L]] <- shiny::tags$span(
                  class = "ec-rp-meta-sep", `aria-hidden` = "true", "\u00b7"
                )
              }
              interleaved[[length(interleaved) + 1L]] <- badges[[i]]
            }
            if (!is.null(qc_alert)) {
              if (length(interleaved) > 0L) {
                interleaved[[length(interleaved) + 1L]] <- shiny::tags$span(
                  class = "ec-rp-meta-sep", `aria-hidden` = "true", "\u00b7"
                )
              }
              interleaved[[length(interleaved) + 1L]] <- qc_alert
            }
            shiny::tags$div(class = "ec-rp-station-meta", interleaved)
          }
        ),
        download_cta
      ),
      vars_panel
    )
  )
}

reporte_timeline_chip_class <- function(label) {
  cls <- "ec-pronostico-today-chip ec-timeline-anchor-chip"
  if (!is.null(label) && length(label) == 1L && label %in% c("Hoy", "Ayer")) {
    paste0(cls, " ec-timeline-anchor-chip--accent")
  } else {
    cls
  }
}


reporte_ultima_obs_card_html <- function(reg, shared, prev_valor = NA_real_, week_max = NA_real_) {
  latest_cls <- if (isTRUE(reg$isLatest)) " is-latest is-hero" else ""
  dias_atras <- suppressWarnings(as.integer(reg$diasAtras))
  day_offset <- if (!is.na(dias_atras) && dias_atras >= 0L) min(6L, dias_atras) else 0L
  badge <- if (isTRUE(reg$isLatest) && nzchar(reg$freshLabel %||% "")) {
    shiny::tags$span(class = reporte_timeline_chip_class(reg$freshLabel), reg$freshLabel)
  }
  unit_span <- if (nzchar(shared$unidad %||% "")) {
    shiny::tags$span(class = "ec-rp-ultima-obs-unit", shared$unidad)
  }
  vtype <- shared$vtype %||% "generic"
  wx <- reporte_weather_file_for_obs(vtype, reg$valor)
  trend <- reporte_week_trend_html(reg$valor, prev_valor, vtype)
  bar <- reporte_week_bar_html(reg$valor, week_max, vtype)
  shiny::tags$div(
    class = paste0("swiper-slide ec-ultimas-obs-slide", latest_cls),
    `data-day-offset` = as.character(day_offset),
    shiny::tags$div(class = "ec-ultimas-obs-slide-inner",
      shiny::HTML(reporte_card_wave_html()),
      shiny::tags$header(class = "ec-ultimas-obs-slide-title",
        shiny::tags$div(class = "ec-ultimas-obs-slide-head",
          shiny::tags$span(class = "ec-ultimas-obs-weekday", reg$weekday %||% ""),
          badge
        ),
        shiny::tags$time(
          class = "ec-ultimas-obs-date",
          datetime = reg$fechaIso %||% "",
          reg$fecha %||% ""
        )
      ),
      shiny::tags$div(class = "ec-rp-ultima-obs-icon",
        shiny::HTML(reporte_weather_icon_html(wx))
      ),
      shiny::tags$span(class = "ec-rp-ultima-obs-variable", shared$variable %||% ""),
      shiny::tags$div(class = "ec-rp-ultima-obs-value-row",
        shiny::tags$span(class = "ec-rp-ultima-obs-value", reg$valueFmt),
        unit_span,
        if (nzchar(trend)) shiny::HTML(trend)
      ),
      if (nzchar(bar)) shiny::HTML(bar),
      reporte_ultima_obs_rank_html(reg)
    )
  )
}

reporte_ultimas_obs_html <- function(block) {
  regs <- block$registros %||% list()
  if (is.null(block) || length(regs) == 0) return(NULL)
  vals <- vapply(regs, function(r) suppressWarnings(as.numeric(r$valor %||% NA_real_)), numeric(1))
  week_max <- suppressWarnings(max(vals, na.rm = TRUE))
  if (!is.finite(week_max)) week_max <- NA_real_
  accent <- reporte_ultima_obs_accent(block$vtype %||% "generic")
  shared <- block
  shared$accentWeather <- accent$weather
  cards <- rev(lapply(seq_along(regs), function(i) {
    prev <- if (i < length(regs)) regs[[i + 1]]$valor else NA_real_
    reporte_ultima_obs_card_html(regs[[i]], shared, prev_valor = prev, week_max = week_max)
  }))
  shiny::tags$div(
    class = "ec-rp-ultimas-obs",
    `data-vtype` = block$vtype %||% "generic",
    style = paste0("--ec-obs-accent:", block$accentColor %||% accent$color, ";"),
    shiny::tags$div(class = "ec-rp-ultimas-obs-deco", `aria-hidden` = "true"),
    shiny::tags$details(class = "ec-rp-ultimas-obs-details ec-rp-ultimas-obs-inner",
      shiny::tags$summary(class = "ec-rp-ultimas-obs-head",
        shiny::tags$span(class = "widget-title ec-rp-ultimas-obs-kicker", "Semana pasada"),
        shiny::tags$span(class = "ec-rp-ultimas-obs-toggle",
          shiny::tags$span(class = "ec-rp-ultimas-obs-hint", "Ver registros"),
          shiny::tags$span(class = "ec-rp-ultimas-obs-caret", `aria-hidden` = "true")
        )
      ),
      shiny::tags$div(class = "ec-rp-ultimas-obs-mount",
        shiny::tags$div(class = "ec-rp-pronostico-swiper-wrap ec-rp-ultimas-obs-swiper-wrap",
          shiny::tags$div(class = "swiper ec-ultimas-obs-swiper",
            shiny::tags$div(class = "swiper-wrapper", cards)
          ),
          shiny::tags$button(type = "button", class = "ec-pronostico-nav ec-pronostico-nav-prev", `aria-label` = "Registro anterior",
            shiny::tags$i(class = "fa fa-chevron-left", `aria-hidden` = "true")
          ),
          shiny::tags$button(type = "button", class = "ec-pronostico-nav ec-pronostico-nav-next", `aria-label` = "Registro siguiente",
            shiny::tags$i(class = "fa fa-chevron-right", `aria-hidden` = "true")
          )
        )
      )
    )
  )
}

reporte_pronostico_shell_html <- function(section_id, mount_id, detail_href = "") {
  link <- if (nzchar(detail_href)) {
    shiny::tags$a(class = "ec-rp-pronostico-detail", href = detail_href, "Ver el pron\u00f3stico \u2192")
  }
  shiny::tags$div(class = "ec-rp-pronostico-section ec-rp-pronostico-loading", id = section_id,
    shiny::tags$div(class = "widget ec-rp-pronostico-card",
      shiny::tags$div(class = "widget-header ec-rp-pronostico-header",
        shiny::tags$div(class = "ec-rp-pronostico-titles",
          shiny::tags$span(class = "widget-title ec-rp-pronostico-kicker", "Pron\u00f3stico de la estaci\u00f3n"),
          shiny::tags$span(class = "ec-rp-pronostico-sub", title = "Resoluci\u00f3n ~10 km", "C\u00f3mo evoluciona el d\u00eda")
        ),
        link
      ),
      shiny::tags$div(class = "ec-rp-pronostico-mount", id = mount_id)
    )
  )
}

# Banner + header al instante (sin API). Shell y full llegan despues.
reporte_skeleton_payload <- function(eid, vid, name = NULL, variable = NULL, variables = list()) {
  list(
    skeleton = TRUE,
    shell = TRUE,
    station = list(
      name = if (nzchar(name %||% "")) name else paste0("Estaci\u00f3n ", eid),
      period = "",
      metaGeo = "",
      altitude = "",
      cuenca = "",
      region = "",
      variable = variable %||% "",
      variables = variables %||% list(),
      fuente = "",
      eid = as.integer(eid),
      vid = as.integer(vid)
    ),
    insightGlobal = "",
    widgets = list(),
    normalClimatology = NULL,
    chart = NULL,
    cicloAnual = NULL,
    tablaAnio = NULL,
    ultimasObservaciones = NULL,
    nFlaggedExcluded = 0L
  )
}

reporte_skel_block_html <- function(extra_class = "") {
  cls <- paste(c("ec-rp-skel-block", extra_class), collapse = " ")
  shiny::tags$div(class = cls, `aria-hidden` = "true")
}

# ---- UI ----
reporte_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "ec-reporte-contenido", shiny::uiOutput(ns("report_content")))
}

# ---- Server ----
reporte_server <- function(id, active_eid_rv, variable_id_rv, ver_datos_click_rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ponytail: env, not reactiveValues. Writing cache$payloads from
    # fetch_report_payload used to invalidate station_vars_data (same
    # reactiveValues object) → second /variables + second renderUI (~4 s extra).
    # Chip clicks still go through selected_vid_by_eid (reactiveValues).
    cache <- new.env(parent = emptyenv())
    cache$payloads <- list()
    cache$station_info <- list()
    cache$station_vars <- list()

    # Variable seleccionada por estacion (persiste al cambiar de pestana)
    selected_vid_by_eid <- shiny::reactiveValues()

    shiny::observeEvent(active_eid_rv(), {
      session$sendCustomMessage("ec_report_destroy", TRUE)
    }, ignoreInit = TRUE)

    # Input desde chips de variable dentro del modal
    shiny::observeEvent(input$selected_vid, {
      vid <- suppressWarnings(as.integer(input$selected_vid))
      eid <- active_eid_rv()
      if (is.na(vid) || is.null(eid)) return()
      selected_vid_by_eid[[as.character(eid)]] <- vid
    })

    station_info_data <- shiny::reactive({
      eid <- active_eid_rv()
      if (is.null(eid)) return(NULL)
      ckey <- as.character(eid)
      if (!is.null(cache$station_info[[ckey]])) return(cache$station_info[[ckey]])
      info <- api_get(paste0("/estaciones/", eid), timeout = 15)
      if (!is.null(info)) cache$station_info[[ckey]] <- info
      info
    })

    station_vars_data <- shiny::reactive({
      eid <- active_eid_rv()
      if (is.null(eid)) return(data.frame())
      ckey <- as.character(eid)
      if (!is.null(cache$station_vars[[ckey]])) return(ec_label_variables_df(cache$station_vars[[ckey]]))
      dat <- api_get(paste0("/estaciones/", eid, "/variables"), timeout = 20)
      if (is.null(dat)) return(data.frame())
      df <- if (is.data.frame(dat)) dat else {
        if (!is.list(dat) || length(dat) == 0) return(data.frame())
        tryCatch(
          do.call(rbind, lapply(dat, function(x) data.frame(
            id        = as.integer(x$id %||% NA),
            nombre    = as.character(x$nombre %||% ""),
            unidad    = as.character(x$unidad %||% ""),
            fecha_min = as.character(x$fecha_min %||% ""),
            fecha_max = as.character(x$fecha_max %||% ""),
            stringsAsFactors = FALSE
          ))),
          error = function(e) data.frame()
        )
      }
      cache$station_vars[[ckey]] <- df
      ec_label_variables_df(df)
    })

    active_vid <- shiny::reactive({
      eid <- active_eid_rv()
      if (is.null(eid)) return(NULL)
      ckey <- as.character(eid)
      mv <- selected_vid_by_eid[[ckey]]
      if (!is.null(mv) && !is.na(mv)) return(mv)
      # Variable del filtro lateral
      vid <- variable_id_rv()
      if (!is.null(vid) && !is.na(vid)) return(vid)
      # Primera variable disponible
      sv <- station_vars_data()
      if (!is.null(sv) && is.data.frame(sv) && nrow(sv) > 0 && "id" %in% names(sv)) return(sv$id[1])
      NULL
    })

    fetch_report_payload <- function(eid, vid, shell = FALSE) {
      ckey <- paste0(eid, "_", vid, if (isTRUE(shell)) "_shell" else "_full")
      if (!is.null(cache$payloads[[ckey]])) return(cache$payloads[[ckey]])

      path <- if (isTRUE(shell)) {
        paste0("/estaciones/", eid, "/reporte/shell")
      } else {
        paste0("/estaciones/", eid, "/reporte")
      }
      resp <- api_get(
        path,
        list(id_variable = vid),
        timeout = if (isTRUE(shell)) 30 else 60
      )
      if (is.null(resp) || !is.null(resp$error)) return(NULL)

      cache$payloads[[ckey]] <- resp
      resp
    }

    # skel (instantaneo) -> shell (API rapida) -> full (hydrate JS).
    # Cada fase en onFlushed distinto para que el usuario vea progreso.
    report_payload_rv <- shiny::reactiveVal(NULL)
    report_phase_key <- shiny::reactiveVal(NULL)

    shiny::observeEvent(
      {
        eid <- active_eid_rv()
        vid <- active_vid()
        tick <- if (is.function(ver_datos_click_rv)) ver_datos_click_rv() else 0L
        list(eid = eid, vid = vid, tick = tick)
      },
      {
        eid <- shiny::isolate(active_eid_rv())
        vid <- shiny::isolate(active_vid())
        tick <- if (is.function(ver_datos_click_rv)) shiny::isolate(ver_datos_click_rv()) else 0L
        if (is.null(eid) || is.null(vid)) {
          report_payload_rv(NULL)
          report_phase_key(NULL)
          return()
        }
        req_key <- paste(eid, vid, tick, sep = "_")
        if (identical(cache$inflight_key, req_key)) return()
        cache$inflight_key <- req_key

        # Solo cache local: no bloquear el banner con api_get.
        ckey <- as.character(eid)
        info <- cache$station_info[[ckey]]
        nom <- if (!is.null(info)) {
          as.character(info$nombre %||% info$name %||% "")
        } else ""
        sv <- cache$station_vars[[ckey]]
        var_nom <- ""
        vars_skel <- list()
        if (!is.null(sv) && is.data.frame(sv) && nrow(sv) > 0L) {
          row <- sv[sv$id == vid, , drop = FALSE]
          if (nrow(row) > 0L) {
            var_nom <- ec_variable_label(as.character(row$nombre[1L]))
          }
          # Chips basicos si ya estan en cache (sin spark).
          vars_skel <- lapply(seq_len(min(nrow(sv), 12L)), function(i) {
            list(
              id = as.integer(sv$id[i]),
              nombre = ec_variable_label(as.character(sv$nombre[i])),
              active = identical(as.integer(sv$id[i]), as.integer(vid))
            )
          })
        }

        report_payload_rv(reporte_skeleton_payload(
          eid, vid,
          name = nom,
          variable = var_nom,
          variables = vars_skel
        ))
        report_phase_key(paste(req_key, "skel", sep = "_"))

        session$onFlushed(function() {
          if (!identical(cache$inflight_key, req_key)) return()
          shiny::withProgress(message = "Cargando reporte...", value = 0.35, {
            shell <- fetch_report_payload(eid, vid, shell = TRUE)
            if (!identical(cache$inflight_key, req_key)) return()
            if (is.null(shell)) {
              report_payload_rv(NULL)
              report_phase_key(paste(req_key, "err", sep = "_"))
              return()
            }
            report_payload_rv(shell)
            report_phase_key(paste(req_key, "shell", sep = "_"))
            # Usable = shell en mano (no esperar historial).
            if (!identical(cache$done_key, req_key)) {
              shiny_app_profile_mark("shiny.reporte_html.done", list(
                eid = as.integer(eid),
                vid = as.integer(vid),
                shell = TRUE
              ))
              cache$done_key <- req_key
            }
          })

          session$onFlushed(function() {
            if (!identical(cache$inflight_key, req_key)) return()
            shiny::withProgress(message = "Cargando historial...", value = 0.7, {
              full <- fetch_report_payload(eid, vid, shell = FALSE)
              if (is.null(full) || !identical(cache$inflight_key, req_key)) return()
              # Hidratar por JS: evita 2o renderUI del modal entero.
              ids <- cache$dom_ids
              if (is.list(ids) && identical(ids$req_key, req_key)) {
                session$sendCustomMessage("ec_report_hydrate_full", list(
                  payload = full,
                  eid = as.integer(eid),
                  vid = as.integer(vid),
                  chartId = ids$chartId,
                  cicloId = ids$cicloId,
                  yearGridId = ids$yearGridId,
                  pronMountId = ids$pronMountId,
                  pronSectionId = ids$pronSectionId
                ))
                report_phase_key(paste(req_key, "full", sep = "_"))
              } else {
                report_payload_rv(full)
                report_phase_key(paste(req_key, "full", sep = "_"))
              }
            })
          }, once = TRUE)
        }, once = TRUE)
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    output$report_content <- shiny::renderUI({
      phase_key <- report_phase_key()
      payload <- report_payload_rv()
      eid <- shiny::isolate(active_eid_rv())

      if (is.null(eid)) {
        return(shiny::tags$div(class = "ec-rp-empty",
          shiny::tags$p("Selecciona una estaci\u00f3n y haz clic en \u00abVer datos\u00bb para ver el reporte clim\u00e1tico.")
        ))
      }

      if (is.null(payload)) {
        return(shiny::tags$div(class = "ec-rp-empty",
          shiny::tags$p("No se pudieron cargar los datos. Intenta nuevamente.")
        ))
      }

      # isolate: el HTML no debe re-depender de chips/vars.
      shiny::isolate({
      # done ya se marco al recibir shell; aqui solo fallback si solo hubo full.
      phase_chr <- as.character(phase_key %||% "")
      base_key <- sub("_(skel|shell|full|err)$", "", phase_chr)
      if (nzchar(base_key) && !identical(cache$done_key, base_key) && grepl("_full$", phase_chr)) {
        shiny_app_profile_mark("shiny.reporte_html.done", list(
          eid = as.integer(eid),
          vid = as.integer(active_vid() %||% NA_integer_),
          shell = FALSE
        ))
        cache$done_key <- base_key
      }

      variable_nombre <- payload$station$variable %||% ""
      if (!nzchar(variable_nombre)) {
        sv <- station_vars_data()
        vid <- active_vid()
        if (!is.null(sv) && is.data.frame(sv) && nrow(sv) > 0 && !is.null(vid)) {
          row <- sv[sv$id == vid, , drop = FALSE]
          if (nrow(row) > 0) variable_nombre <- ec_variable_label(as.character(row$nombre[1]))
        }
      }
      payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null", digits = 4)
      chart_id <- ns("ts_chart")
      ciclo_id <- ns("ciclo_anual_chart")
      year_grid_id <- ns("year_grid")
      pron_section_id <- ns("pronostico_section")
      pron_mount_id <- ns("pronostico_carousel")
      anio_act <- as.integer(format(Sys.Date(), "%Y"))

      st <- payload$station
      ciclo <- payload$cicloAnual %||% list()
      tabla <- payload$tablaAnio %||% list()
      has_tabla <- !is.null(tabla) && isTRUE(tabla$hasData)
      has_mes_actual <- isTRUE(ciclo$hasMesActual)
      ciclo_title <- ciclo$title %||% "Ciclo Anual"
      ciclo_subtitle <- ciclo$subtitle %||% ""

      var_chips_html <- ""
      # httr/jsonlite convierten el array JSON en data.frame; vapply sobre un
      # data.frame itera columnas y rompe los chips. Normalizar a lista de filas.
      vars_list <- st$variables
      if (is.data.frame(vars_list) && nrow(vars_list) > 0) {
        vars_list <- lapply(seq_len(nrow(vars_list)), function(i) as.list(vars_list[i, , drop = FALSE]))
      }
      if (is.list(vars_list) && length(vars_list) > 0) {
        ns_id <- ns("selected_vid")
        var_chips_html <- paste(vapply(vars_list, function(v) {
          vid_i <- suppressWarnings(as.integer(v$id %||% NA_integer_))
          if (is.na(vid_i)) return("")
          meta <- reporte_var_chip_meta(v$nombre %||% "")
          cls <- if (isTRUE(v$active)) "ec-rp-var-chip active" else "ec-rp-var-chip"
          sprintf(
            paste0(
              '<button type="button" class="%s" data-vid="%d" data-vtype="%s"%s ',
              "onclick=\"Shiny.setInputValue('%s',%d,{priority:'event'})\">",
              '<span class="ec-rp-var-chip-deco" aria-hidden="true"></span>',
              '<span class="ec-rp-var-chip-icon">%s</span>',
              '<span class="ec-rp-var-chip-body">',
              '<span class="ec-rp-var-chip-label">%s</span>',
              '<span class="ec-rp-spark" aria-hidden="true"></span>',
              "</span></button>"
            ),
            cls, vid_i, meta$vtype, reporte_spark_attr(v),
            ns_id, vid_i,
            reporte_weather_icon_html(meta$weather),
            htmltools::htmlEscape(as.character(v$nombre %||% ""))
          )
        }, character(1)), collapse = "")
      }

      widgets_list <- payload$widgets
      if (is.data.frame(widgets_list) && nrow(widgets_list) > 0) {
        widgets_list <- lapply(seq_len(nrow(widgets_list)), function(i) as.list(widgets_list[i, , drop = FALSE]))
      }
      widget_shells <- paste(vapply(widgets_list %||% list(), function(w) {
        sprintf('<div class="widget ec-rw-widget" data-widget-id="%s"></div>', htmltools::htmlEscape(w$id %||% ""))
      }, character(1)), collapse = "\n")
      if (!nzchar(widget_shells) && (isTRUE(payload$shell) || isTRUE(payload$skeleton))) {
        widget_shells <- paste(
          rep('<div class="widget ec-rw-widget ec-rp-skel-widget" aria-hidden="true"></div>', 3L),
          collapse = "\n"
        )
      }

      pron_detail_href <- if (!is.null(st$eid)) {
        qs <- paste0("/mi-reporte/?view=pronostico&e=", as.integer(st$eid))
        if (!is.null(st$vid)) paste0(qs, "&v=", as.integer(st$vid)) else qs
      } else ""

      ciclo_pills <- shiny::tags$div(
        class = "pill-group ec-ciclo-pills",
        role = "group",
        `aria-label` = "Per\u00edodo del ciclo",
        shiny::tags$button(
          type = "button", class = "pill active", `data-ciclo-mode` = "anual",
          `aria-pressed` = "true",
          onclick = sprintf("ecCicloSetMode(this,'%s','anual')", ciclo_id), "Anual"
        ),
        if (has_mes_actual) {
          shiny::tags$button(
            type = "button", class = "pill", `data-ciclo-mode` = "mesActual",
            `aria-pressed` = "false",
            onclick = sprintf("ecCicloSetMode(this,'%s','mesActual')", ciclo_id), "Mes Actual"
          )
        }
      )

      is_shell <- isTRUE(payload$shell) || isTRUE(payload$skeleton)
      is_skel <- isTRUE(payload$skeleton)
      banner_msg <- if (is_skel) {
        "Cargando datos de la estaci\u00f3n\u2026"
      } else if (is_shell) {
        "Cargando historial y rankings\u2026"
      } else {
        NULL
      }
      chart_json <- if (!is.null(payload$chart)) {
        jsonlite::toJSON(payload$chart, auto_unbox = TRUE, na = "null", digits = 2)
      } else {
        "null"
      }
      ciclo_json <- if (length(ciclo) > 0) {
        jsonlite::toJSON(ciclo, auto_unbox = TRUE, na = "null", digits = 2)
      } else {
        "null"
      }
      tabla_json <- if (has_tabla) {
        jsonlite::toJSON(tabla, auto_unbox = TRUE, na = "null", digits = 2)
      } else {
        "null"
      }
      pronostico_lite_json <- if (!is.null(payload$pronosticoLite)) {
        jsonlite::toJSON(payload$pronosticoLite, auto_unbox = TRUE, na = "null", digits = 2)
      } else {
        "null"
      }
      eid_js <- if (!is.null(st$eid)) as.integer(st$eid) else 0L
      vid_js <- if (!is.null(st$vid)) as.integer(st$vid) else {
        av <- active_vid()
        if (!is.null(av) && !is.na(av)) as.integer(av) else 0L
      }

      phase_chr_dom <- as.character(phase_key %||% "")
      base_key_dom <- sub("_(skel|shell|full|err)$", "", phase_chr_dom)
      cache$dom_ids <- list(
        req_key = base_key_dom,
        chartId = chart_id,
        cicloId = ciclo_id,
        yearGridId = year_grid_id,
        pronMountId = pron_mount_id,
        pronSectionId = pron_section_id
      )

      shiny::tagList(
        shiny::tags$script(
          class = "ec-report-data-json",
          type = "application/json",
          shiny::HTML(payload_json)
        ),

        reporte_header_html(st, var_chips_html),

        if (!is.null(banner_msg)) {
          shiny::tags$div(
            class = if (is_skel) "ec-rp-heavy-banner is-skel" else "ec-rp-heavy-banner",
            role = "status",
            banner_msg
          )
        },

        shiny::tags$div(class = "ec-rp-timeline-pair",
          reporte_pronostico_shell_html(pron_section_id, pron_mount_id, pron_detail_href),
          {
            ultimas_obs <- reporte_ultimas_obs_html(payload$ultimasObservaciones)
            if (!is.null(ultimas_obs)) {
              shiny::tagList(
                shiny::tags$div(class = "ec-rp-timeline-connector", `aria-hidden` = "true",
                  shiny::tags$span(class = "ec-rp-timeline-connector-line")
                ),
                ultimas_obs
              )
            } else if (is_skel) {
              shiny::tagList(
                shiny::tags$div(class = "ec-rp-timeline-connector", `aria-hidden` = "true",
                  shiny::tags$span(class = "ec-rp-timeline-connector-line")
                ),
                reporte_skel_block_html("ec-rp-skel-ultimas")
              )
            } else {
              NULL
            }
          }
        ),

        if (nzchar(payload$insightGlobal %||% "")) {
          shiny::tags$div(class = "ec-rp-insight-strip", payload$insightGlobal)
        },

        shiny::tags$div(class = if (is_shell) "ec-rp-body is-shell-pending" else "ec-rp-body",
          shiny::tags$div(class = "ec-rp-section",
            shiny::tags$h3(
              class = "ec-rp-section-title",
              style = "text-transform:none;letter-spacing:normal",
              "C\u00f3mo se compara este a\u00f1o con los anteriores?"
            ),
            shiny::tags$div(class = "widget ec-rp-ciclo-card",
              shiny::tags$div(class = "widget-header ec-rp-ciclo-header",
                shiny::tags$div(class = "ec-rp-ciclo-titles",
                  shiny::tags$span(id = paste0(ciclo_id, "_title"), class = "widget-title", ciclo_title),
                  shiny::tags$span(id = paste0(ciclo_id, "_subtitle"), class = "ec-rp-ciclo-subtitle", ciclo_subtitle)
                ),
                shiny::tags$div(class = "ec-rp-ciclo-controls",
                  shiny::tags$span(
                    id = paste0(ciclo_id, "_normal_period"),
                    class = "ec-normal-period-mount ec-legend-item"
                  ),
                  ciclo_pills
                )
              ),
              shiny::tags$div(id = paste0(ciclo_id, "_kpis"), class = "ec-rp-ciclo-kpis"),
              shiny::tags$p(id = paste0(ciclo_id, "_sr"), class = "ec-rp-ciclo-sr"),
              shiny::tags$p(
                id = paste0(ciclo_id, "_line_label"),
                class = "ec-rp-ciclo-panel-label",
                hidden = "hidden",
                "Acumulado"
              ),
              shiny::tags$div(class = "ec-rp-ciclo-chart-wrap",
                shiny::tags$canvas(id = ciclo_id, `aria-label` = "C\u00f3mo se compara este a\u00f1o con los anteriores?", role = "img")
              ),
              shiny::tags$div(
                id = paste0(ciclo_id, "_bars_wrap"),
                class = "ec-rp-ciclo-bars-block",
                hidden = "hidden",
                shiny::tags$p(class = "ec-rp-ciclo-panel-label", "Por mes"),
                shiny::tags$div(class = "ec-rp-ciclo-chart-wrap ec-rp-ciclo-bars",
                  shiny::tags$canvas(
                    id = paste0(ciclo_id, "_bars"),
                    `aria-label` = "Precipitaci\u00f3n por mes",
                    role = "img"
                  )
                )
              ),
              shiny::tags$p(id = paste0(ciclo_id, "_foot"), class = "ec-rp-ciclo-foot")
            )
          ),
          shiny::tags$div(class = "ec-rp-section",
            shiny::tags$h3(class = "ec-rp-section-title", "Registros hist\u00f3ricos"),
            shiny::tags$div(class = "ec-rw-yearbar", `data-role` = "yearbar", hidden = "hidden"),
            shiny::tags$div(class = "ec-rp-widgets-row", shiny::HTML(widget_shells))
          ),
          shiny::tags$div(class = "ec-rp-section",
            shiny::tags$div(class = "widget ec-rp-ts-card",
              shiny::tags$div(class = "widget-header ec-rp-ts-header",
                shiny::tags$div(class = "ec-rp-ts-titles",
                  shiny::tags$div(class = "ec-rp-ts-title",
                    paste0("Historial de ", variable_nombre)
                  ),
                  shiny::tags$p(
                    id = paste0(chart_id, "_ts_subtitle"),
                    class = "ec-rp-ts-subtitle",
                    "Totales anuales"
                  )
                ),
                shiny::tags$div(class = "ec-rp-ts-controls",
                  shiny::tags$div(class = "pill-group ec-ts-pills", role = "group",
                    `aria-label` = "Resoluci\u00f3n del historial",
                    shiny::tags$button(type = "button", class = "pill active", `data-ts-res` = "anual",
                      onclick = sprintf("ecTSSetRes(this,'%s','anual')", chart_id), "Anual"),
                    shiny::tags$button(type = "button", class = "pill", `data-ts-res` = "mensual",
                      onclick = sprintf("ecTSSetRes(this,'%s','mensual')", chart_id), "Mensual"),
                    shiny::tags$button(type = "button", class = "pill", `data-ts-res` = "completitud",
                      onclick = sprintf("ecTSSetRes(this,'%s','completitud')", chart_id), "% completitud datos")
                  ),
                  shiny::tags$div(
                    id = paste0(chart_id, "_span"),
                    class = "pill-group ec-ts-span",
                    role = "group",
                    `aria-label` = "Per\u00edodo del \u00faltimo a\u00f1o",
                    style = "display:none;",
                    shiny::tags$button(type = "button", class = "pill active", `data-ts-span` = "completo",
                      onclick = sprintf("ecTSSetSpan(this,'%s','completo')", chart_id), "A\u00f1o completo"),
                    shiny::tags$button(type = "button", class = "pill", `data-ts-span` = "hasta_hoy",
                      onclick = sprintf("ecTSSetSpan(this,'%s','hasta_hoy')", chart_id), "Hasta hoy")
                  )
                )
              ),
              shiny::tags$p(id = paste0(chart_id, "_sr"), class = "ec-rp-ts-sr"),
              shiny::tags$div(id = paste0(chart_id, "_kpis"), class = "ec-rp-ts-kpis"),
              shiny::tags$div(id = paste0(chart_id, "_ts_legend"), class = "ec-rp-ts-legend"),
              shiny::tags$div(class = "ec-rp-ts-chart-wrap is-annual",
                shiny::tags$svg(
                  id = paste0(chart_id, "_svg"),
                  class = "ec-rp-ts-svg",
                  role = "group",
                  `aria-label` = paste("Historial anual de", variable_nombre, "en", st$name %||% "la estaci\u00f3n")
                ),
                shiny::tags$div(id = paste0(chart_id, "_tt"), class = "ec-rp-ts-tip"),
                shiny::tags$canvas(
                  id = chart_id,
                  `aria-label` = paste("Serie temporal de", variable_nombre, "en", st$name %||% "la estaci\u00f3n"),
                  role = "img"
                )
              ),
              shiny::tags$div(class = "ec-rp-ts-foot",
                shiny::tags$div(class = "ec-rp-ts-ref",
                  shiny::tags$span(class = "ec-rp-ts-ref-label", "Per\u00edodo de referencia"),
                  shiny::tags$span(
                    id = paste0(chart_id, "_ts_normal_period"),
                    class = "ec-normal-period-mount ec-rp-ts-period"
                  )
                ),
                shiny::tags$p(id = paste0(chart_id, "_ts_note"), class = "ec-rp-ts-note")
              ),
              shiny::tags$table(id = paste0(chart_id, "_ts_table"), class = "ec-rp-ts-backup")
            )
          ),
          if (has_tabla) {
            shiny::tags$div(class = "ec-rp-section ec-rp-year-grid-section",
              shiny::tags$div(class = "widget ec-rp-year-grid-card",
                shiny::tags$div(class = "ec-rp-year-grid-mount", id = year_grid_id)
              )
            )
          }
        ),

        shiny::tags$script(shiny::HTML(sprintf(
          "window.__ecReportChartData=%s;window.__ecReportCicloData=%s;window.__ecReportTablaAnio=%s;window.__ecReportPronosticoLite=%s;window.__ecReportNormalClimatology=%s;",
          chart_json, ciclo_json, tabla_json, pronostico_lite_json,
          jsonlite::toJSON(payload$normalClimatology %||% list(), auto_unbox = TRUE, na = "null", digits = 2)
        ))),
        shiny::tags$script(shiny::HTML(sprintf(
          paste0(
            "(function(){function _go(){",
            "if(typeof ecBootReportPostRender==='function'){",
            "ecBootReportPostRender({eid:%d,vid:%d,pronMountId:'%s',pronSectionId:'%s',",
            "pronosticoLite:window.__ecReportPronosticoLite,",
            "chartId:'%s',cicloId:'%s',yearGridId:'%s',",
            "hasChart:!!window.__ecReportChartData,hasCiclo:!!window.__ecReportCicloData,",
            "hasTabla:!!window.__ecReportTablaAnio});",
            "}else{",
            "if(typeof ecInitReport==='function')ecInitReport();",
            "if(typeof ecInitTSChart==='function'&&window.__ecReportChartData){ecInitTSChart('%s',window.__ecReportChartData);}",
            "if(typeof ecInitCicloAnualChart==='function'&&window.__ecReportCicloData){ecInitCicloAnualChart('%s',window.__ecReportCicloData);}",
            "if(typeof ecInitYearGrid==='function'&&window.__ecReportTablaAnio){ecInitYearGrid('%s',window.__ecReportTablaAnio);}",
            "if(typeof ecLoadPronosticoCarousel==='function'&&%d){ecLoadPronosticoCarousel(%d,'%s','%s');}",
            "}",
            "}setTimeout(_go,0);})();"
          ),
          eid_js, vid_js, pron_mount_id, pron_section_id,
          chart_id, ciclo_id, year_grid_id,
          chart_id, ciclo_id, year_grid_id,
          eid_js, eid_js, pron_mount_id, pron_section_id
        )))
      )
      })
    })
    # Shell debe poder renderizar aunque el modal acabe de abrirse.
    shiny::outputOptions(output, "report_content", suspendWhenHidden = FALSE)
  })
}
