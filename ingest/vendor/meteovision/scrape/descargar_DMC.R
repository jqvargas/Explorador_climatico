DMC_MESES_NOMBRE <- c(
  "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
)

dmc_codigo_desde_url <- function(url) {
  m <- regexpr("/([0-9]{5,6})/[0-9]{4}$", url)
  if (m < 0) return(NA_character_)
  sub(".*/([0-9]{5,6})/[0-9]{4}$", "\\1", url)
}

dmc_preparar_tabla_anual <- function(tabla) {
  if (is.null(tabla) || nrow(tabla) == 0) return(tabla)
  hdr2 <- if (ncol(tabla) > 1) as.character(tabla[1, 2]) else ""
  if (hdr2 %in% DMC_MESES_NOMBRE) {
    names(tabla) <- as.character(tabla[1, ])
    tabla <- tabla[-1, , drop = FALSE]
  } else {
    hdr1 <- as.character(tabla[1, 1])
    if (hdr1 %in% c("D\u00eda", "Dia", "D\u00cdA", "day", "D\u00eda")) {
      names(tabla) <- as.character(tabla[1, ])
      tabla <- tabla[-1, , drop = FALSE]
    }
  }
  if (ncol(tabla) > 13) tabla <- tabla[, seq_len(13), drop = FALSE]
  names(tabla)[1] <- "day"
  if (ncol(tabla) > 1 && any(grepl("^Meses", names(tabla)[-1], ignore.case = TRUE))) {
    names(tabla)[2:ncol(tabla)] <- DMC_MESES_NOMBRE[seq_len(ncol(tabla) - 1)]
  }
  day_num <- suppressWarnings(as.numeric(tabla$day))
  keep <- !is.na(day_num) & day_num >= 1 & day_num <= 31
  tabla[keep, , drop = FALSE]
}

dmc_mes_a_num <- function(month) {
  dplyr::case_when(
    month == "Enero" ~ "01",
    month == "Febrero" ~ "02",
    month == "Marzo" ~ "03",
    month == "Abril" ~ "04",
    month == "Mayo" ~ "05",
    month == "Junio" ~ "06",
    month == "Julio" ~ "07",
    month == "Agosto" ~ "08",
    month == "Septiembre" ~ "09",
    month == "Octubre" ~ "10",
    month == "Noviembre" ~ "11",
    month == "Diciembre" ~ "12",
    TRUE ~ month
  )
}

# Predominante diario DMC: rumbo/nudos en la misma celda (no max/min/mean).
DMC_VIENTO_RUMBOS_DEG <- c(
  N = 0, NE = 45, E = 90, SE = 135,
  S = 180, SW = 225, W = 270, NW = 315
)
DMC_VIENTO_CELDA_RE <- "CALMA|VRB/[0-9]+|[NSEW]{1,2}/[0-9]+"

dmc_parse_viento_celda <- function(x) {
  raw <- trimws(as.character(x))
  n <- length(raw)
  kt <- rep(NA_real_, n)
  dire <- rep(NA_real_, n)
  if (n == 0L) {
    return(data.frame(u_10_kt = kt, u_10_dire = dire, stringsAsFactors = FALSE))
  }
  na_cell <- is.na(x) | !nzchar(raw) | raw == "."
  up <- toupper(raw)
  calma <- !na_cell & up == "CALMA"
  kt[calma] <- 0
  rest <- which(!na_cell & !calma)
  if (length(rest) == 0L) {
    return(data.frame(u_10_kt = kt, u_10_dire = dire, stringsAsFactors = FALSE))
  }
  s <- up[rest]
  slash <- grepl("/", s, fixed = TRUE)
  if (any(slash)) {
    idx <- rest[slash]
    parts <- strsplit(s[slash], "/", fixed = TRUE)
    left <- vapply(parts, function(p) p[[1L]], character(1))
    right <- vapply(parts, function(p) {
      if (length(p) >= 2L) p[[2L]] else NA_character_
    }, character(1))
    kt[idx] <- suppressWarnings(as.numeric(right))
    mapped <- unname(DMC_VIENTO_RUMBOS_DEG[left])
    dire[idx] <- ifelse(left == "VRB", NA_real_, mapped)
  }
  if (any(!slash)) {
    idx <- rest[!slash]
    tok <- s[!slash]
    mapped <- unname(DMC_VIENTO_RUMBOS_DEG[tok])
    num <- suppressWarnings(as.numeric(tok))
    is_num <- !is.na(num)
    dire[idx] <- ifelse(tok == "VRB" | is_num, NA_real_, mapped)
    kt[idx] <- ifelse(is_num, num, NA_real_)
  }
  data.frame(u_10_kt = kt, u_10_dire = dire, stringsAsFactors = FALSE)
}

dmc_viento_html_tiene_celdas <- function(html) {
  txt <- paste(rvest::html_text(html), collapse = " ")
  grepl(DMC_VIENTO_CELDA_RE, txt)
}

dmc_empty_viento_row <- function(url) {
  data.frame(
    year = dmc_year_from_url(url),
    month = NA, day = NA,
    codigo_nacional = dmc_codigo_desde_url(url),
    u_10_kt = NA_real_,
    u_10_dire = NA_real_,
    stringsAsFactors = FALSE
  )
}

# Timeout DMC: default 45s (12s era fail-fast y abria circuit con racha de timeouts).
# Override: INGEST_DMC_TIMEOUT_SEC.
dmc_timeout_sec <- function(default = 45) {
  raw <- Sys.getenv("INGEST_DMC_TIMEOUT_SEC", "")
  if (!nzchar(raw)) return(as.numeric(default))
  n <- suppressWarnings(as.numeric(raw))
  if (is.na(n) || n < 5) return(as.numeric(default))
  n
}

# Timeout: count_toward_circuit=FALSE (como DGA). 429/503 si cuentan.
# No marcar dead por timeout (SNIA/DMC lento != estacion muerta).
dmc_read_html_url <- function(url, retries = 2L, timeout_sec = NULL) {
  if (is.null(timeout_sec)) timeout_sec <- dmc_timeout_sec(45)
  assign(".scrape_last_was_timeout", FALSE, envir = .GlobalEnv)
  ua <- add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36")
  last_err <- NULL
  transport_fail <- FALSE
  parent <- if (exists("profile_current_parent", mode = "function")) {
    profile_current_parent(c("station", "var", "fuente"))
  } else {
    NULL
  }
  codigo <- tryCatch(dmc_codigo_desde_url(url), error = function(e) NA_character_)
  variable <- Sys.getenv("INGEST_SCRAPE_VARIABLE", "")
  for (i in seq_len(retries)) {
    t0 <- Sys.time()
    out <- tryCatch({
      response <- GET(
        url, ua,
        timeout(as.numeric(timeout_sec)),
        config(connecttimeout = as.numeric(timeout_sec))
      )
      code <- as.integer(response$status_code)
      wall_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
      if (exists("scrape_http_stats_bump", mode = "function")) {
        scrape_http_stats_bump(status_code = code, fuente = "dmc", elapsed_ms = wall_ms)
      }
      if (exists("scrape_register_dmc_transport", mode = "function")) {
        scrape_register_dmc_transport(FALSE)
      }
      if (exists("profile_record_http", mode = "function")) {
        profile_record_http(
          response = response, fuente = "dmc", variable = variable, codigo = codigo,
          http_code = code, timeout = FALSE, parent = parent, wall_ms = wall_ms
        )
      }
      if (!is.na(code) && code %in% c(429L, 503L)) {
        last_err <<- paste0("HTTP ", code)
        if (exists("scrape_register_outcome", mode = "function")) {
          scrape_register_outcome(FALSE, fuente = "dmc", count_toward_circuit = TRUE)
        }
        NULL
      } else if (!is.na(code) && code >= 400L) {
        last_err <<- paste0("HTTP ", code)
        if (!is.na(code) && code == 500L && exists("scrape_dead_mark", mode = "function")) {
          # RE5036 es region/dia: dmc_codigo_desde_url no matchea. NA no es estacion.
          cod_dead <- dmc_codigo_desde_url(url)
          if (!is.na(cod_dead) && nzchar(cod_dead)) scrape_dead_mark("dmc", cod_dead, 500L)
        }
        NULL
      } else {
        html <- read_html(response)
        if (exists("scrape_register_outcome", mode = "function")) {
          scrape_register_outcome(TRUE, fuente = "dmc")
        }
        html
      }
    }, error = function(e) {
      last_err <<- conditionMessage(e)
      is_to <- grepl("timeout|timed out|Timeout was reached", last_err, ignore.case = TRUE)
      is_tr <- if (exists("scrape_dmc_transport_failure", mode = "function")) {
        scrape_dmc_transport_failure(last_err)
      } else {
        is_to
      }
      if (is_tr) transport_fail <<- TRUE
      if (is_to) assign(".scrape_last_was_timeout", TRUE, envir = .GlobalEnv)
      if (exists("scrape_register_dmc_transport", mode = "function")) {
        scrape_register_dmc_transport(is_tr)
      }
      if (exists("scrape_http_stats_bump", mode = "function")) {
        wall_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
        scrape_http_stats_bump(
          is_error = TRUE, fuente = "dmc", is_timeout = is_to, elapsed_ms = wall_ms
        )
      }
      if (exists("scrape_register_outcome", mode = "function")) {
        # Timeout / transporte: no abrir circuit legado (alineado con DGA)
        scrape_register_outcome(FALSE, fuente = "dmc", count_toward_circuit = FALSE)
      }
      if (exists("profile_record_http", mode = "function")) {
        wall_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
        profile_record_http(
          response = NULL, fuente = "dmc", variable = variable, codigo = codigo,
          http_code = NA_integer_, timeout = is_to, parent = parent, wall_ms = wall_ms
        )
      }
      NULL
    })
    if (!is.null(out)) return(out)
    # Puerto cerrado o timeout: un segundo GET al mismo URL no ayuda y duplica el golpe.
    if (isTRUE(transport_fail)) break
    if (exists("dmc_budget_sleep", mode = "function")) {
      dmc_budget_sleep()
    } else {
      Sys.sleep(max(min(1.5 * i, 4), 5))
    }
  }
  stop("Error de conexion en ", url, ": ", last_err)
}

descargar_temp_min_DMC<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){

  getData<-function(url){
    tryCatch({
      html <- dmc_read_html_url(url)
      tabla<-html %>%
        html_elements("#excel") %>%
        html_table() %>%
        data.frame()

      names(tabla)<-tabla[1,]
      names(tabla)[1]<-"day"
      tabla<-tabla[-1,]  %>%
        pivot_longer(cols = -day,
                     names_to = "month",
                     values_to = "temp_min") %>%
        mutate(temp_min = as.numeric(temp_min))  %>%
        mutate(day = as.numeric(day)) %>%
        mutate(month = dmc_mes_a_num(month))
      celda_fin <- which(is.na(tabla$day))[1]
      if (!is.na(celda_fin) && celda_fin <= nrow(tabla)) {
        tabla <- tabla[-c(celda_fin:nrow(tabla)), , drop = FALSE]
      }
      tabla <- tabla %>%
        mutate(year = dmc_year_from_url(url)) %>%
        mutate(codigo_nacional = dmc_codigo_desde_url(url)) %>%
        arrange(month, day, year) %>%
        select(year, month, day, codigo_nacional, temp_min)

      if (nrow(tabla) > 0) {
        return(tabla)
      }
      dmc_empty_row(url, "temp_min")

    }, error = function(e) {
      dmc_empty_row(url, "temp_min")
    })

  }
  urls <- dmc_collect_urls(
    ano_inicio, ano_actual, estaciones,
    "https://climatologia.meteochile.gob.cl/application/anual/temperaturaMinimaAnual/"
  )
  temp <- dmc_run_url_loop(urls, getData, slow = FALSE, value_col = "temp_min")
  # Solo tope anti-parseo: el rango real lo aplica regla_de_magnitud y el QC
  # multinivel. Filtrar aca borraba la fila y el UPSERT dejaba congelado el valor
  # viejo, que es como un dato malo sobrevive a su propia correccion en la fuente.
  result <- temp %>%
    filter(is.na(temp_min) | abs(temp_min) < 1e4)

  result$month <- as.numeric(result$month)
  result$year <- as.numeric(result$year)

  result <- result %>% 
    filter(!(year == ano_actual & month > as.numeric(mes_ultimo)))

  

  return(result)
}


descargar_humedad_DMC<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){
  getData<-function(url){
    tryCatch({
      html <- dmc_read_html_url(url)
      tabla<-html %>%
        html_elements("#excel") %>%
        html_table() %>%
        data.frame()
      names(tabla)<-tabla[1,]
      names(tabla)[1]<-"day"
      tabla<-tabla[-1,]  %>%
        pivot_longer(cols = -day, names_to = "month", values_to = "hr_mean") %>%
        mutate(hr_mean = as.numeric(hr_mean))  %>%
        mutate(day = as.numeric(day)) %>%
        mutate(month = dmc_mes_a_num(month))
      celda_fin <- which(is.na(tabla$day))[1]
      if (!is.na(celda_fin) && celda_fin <= nrow(tabla)) {
        tabla <- tabla[-c(celda_fin:nrow(tabla)), , drop = FALSE]
      }
      tabla <- tabla %>%
        mutate(year = dmc_year_from_url(url)) %>%
        mutate(codigo_nacional = dmc_codigo_desde_url(url)) %>%
        arrange(month, day, year) %>%
        select(year, month, day, codigo_nacional, hr_mean)
      if (nrow(tabla) > 0) return(tabla)
      dmc_empty_row(url, "hr_mean")
    }, error = function(e) dmc_empty_row(url, "hr_mean"))
  }
  urls <- dmc_collect_urls(ano_inicio, ano_actual, estaciones,
    "https://climatologia.meteochile.gob.cl/application/anual/humedadAnual/")
  temp <- dmc_run_url_loop(urls, getData, slow = FALSE, value_col = "hr_mean")
  result <- temp %>% filter(is.na(hr_mean) | abs(hr_mean) < 1e4)
  result$month <- as.numeric(result$month)
  result$year <- as.numeric(result$year)
  result <- result %>% filter(!(year == ano_actual & month > as.numeric(mes_ultimo)))
  return(result)
}


descargar_temp_max_DMC<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){
  getData<-function(url){
    tryCatch({
      html <- dmc_read_html_url(url)
      tabla<-html %>%
        html_elements("#excel") %>%
        html_table() %>%
        data.frame()
      names(tabla)<-tabla[1,]
      names(tabla)[1]<-"day"
      tabla<-tabla[-1,]  %>%
        pivot_longer(cols = -day, names_to = "month", values_to = "temp_max") %>%
        mutate(temp_max = as.numeric(temp_max))  %>%
        mutate(day = as.numeric(day)) %>%
        mutate(month = dmc_mes_a_num(month))
      celda_fin <- which(is.na(tabla$day))[1]
      if (!is.na(celda_fin) && celda_fin <= nrow(tabla)) {
        tabla <- tabla[-c(celda_fin:nrow(tabla)), , drop = FALSE]
      }
      tabla <- tabla %>%
        mutate(year = dmc_year_from_url(url)) %>%
        mutate(codigo_nacional = dmc_codigo_desde_url(url)) %>%
        arrange(month, day, year) %>%
        select(year, month, day, codigo_nacional, temp_max)
      if (nrow(tabla) > 0) return(tabla)
      dmc_empty_row(url, "temp_max")
    }, error = function(e) dmc_empty_row(url, "temp_max"))
  }
  urls <- dmc_collect_urls(ano_inicio, ano_actual, estaciones,
    "https://climatologia.meteochile.gob.cl/application/anual/temperaturaMaximaAnual/")
  temp <- dmc_run_url_loop(urls, getData, slow = FALSE, value_col = "temp_max")
  result <- temp %>% filter(is.na(temp_max) | abs(temp_max) < 1e4)
  result$month<-as.numeric(result$month)
  result <- result %>% filter(!(year == ano_actual & month > as.numeric(mes_ultimo)))
  return(result)
}

descargar_pp_DMC<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){

######################################################               
############### Funcion: Crear direcciones URL ############
  getUrls2 <- function(year,cod_est){


    #Definir URL raíz del servidor (debe ser estática)
    root1<-"https://climatologia.meteochile.gob.cl/application/anual/aguaCaidaAnual/"
    #Se genera el url completo
    urls <- NULL
    urls <- c(urls,paste0(root1,cod_est,"/",year))
    return(urls)
  }

  ############### Función: Extraer data  #########################
  getData<-function(url){
    #Descargar html
    tryCatch({
      html <- dmc_read_html_url(url)
      #Extraer datos del html
      tabla<-html %>%
        html_elements("#excel") %>%
        html_table() %>%
        data.frame()
      tabla <- dmc_preparar_tabla_anual(tabla)
      if (nrow(tabla) == 0) {
        return(data.frame(
          day = NA, month = NA, pp_day = NA,
          year = as.numeric(substring(url, nchar(url) - 3)),
          codigo_nacional = dmc_codigo_desde_url(url)
        ))
      }
      tabla <- tabla %>%
        pivot_longer(cols = -day,
                     names_to = "month",
                     values_to = "pp_day") %>%
        mutate(pp_day = gsub("s/p", "0", pp_day, fixed = TRUE)) %>%
        mutate(pp_day = gsub("^\\.$", NA_character_, pp_day)) %>%
        mutate(pp_day = as.numeric(pp_day)) %>%
        mutate(day = as.numeric(day)) %>%
        mutate(month = dmc_mes_a_num(month)) %>%
        filter(!is.na(day)) %>%
        mutate(year = as.numeric(substring(url, nchar(url) - 3))) %>%
        mutate(codigo_nacional = dmc_codigo_desde_url(url)) %>%
        arrange(month, day, year) %>%
        select(year, month, day, codigo_nacional, pp_day)


      #En caso de que no haya informacion disponible:
      if (nrow(tabla) > 0) {
        return(tabla)
      } else { return(data.frame(day=NA,
                                 month=NA,
                                 pp_day=NA,
                                 year=as.numeric(substring(url, nchar(url) - 3)),
                                 codigo_nacional=dmc_codigo_desde_url(url))) }

    }, error = function(e) {
      message("[dmc/pp] fallo ", url, ": ", conditionMessage(e))
      return(data.frame(day=NA,
                        month=NA,
                        pp_day=NA,
                        year=as.numeric(substring(url, nchar(url) - 3)),
                        codigo_nacional=dmc_codigo_desde_url(url)))
      
    })

  }  

  urls <- dmc_collect_urls(
    ano_inicio, ano_actual, estaciones,
    "https://climatologia.meteochile.gob.cl/application/anual/aguaCaidaAnual/"
  )
  temp <- dmc_run_url_loop(urls, getData, slow = TRUE, value_col = "pp_day")
  result <- temp %>%
    filter(is.na(pp_day) | abs(pp_day) < 1e4) %>%
      select(year, month, day, codigo_nacional, pp_day) %>% drop_na(pp_day)

  result$month<-as.numeric(result$month)

  result <- result %>% 
    filter(!(year == ano_actual & month > as.numeric(mes_ultimo) ))

  




  #beepr::beep(8)
  return(result)


}



descargar_temp_media_DMC<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){
  getData<-function(url){
    tryCatch({
      html <- dmc_read_html_url(url)
      tabla<-html %>%
        html_elements("#excel") %>%
        html_table() %>%
        data.frame()
      names(tabla)<-tabla[1,]
      names(tabla)[1]<-"day"
      tabla<-tabla[-1,]  %>%
        pivot_longer(cols = -day, names_to = "month", values_to = "temp_mean") %>%
        mutate(temp_mean = as.numeric(temp_mean))  %>%
        mutate(day = as.numeric(day)) %>%
        mutate(month = dmc_mes_a_num(month))
      celda_fin <- which(is.na(tabla$day))[1]
      if (!is.na(celda_fin) && celda_fin <= nrow(tabla)) {
        tabla <- tabla[-c(celda_fin:nrow(tabla)), , drop = FALSE]
      }
      tabla <- tabla %>%
        mutate(year = dmc_year_from_url(url)) %>%
        mutate(codigo_nacional = dmc_codigo_desde_url(url)) %>%
        arrange(month, day, year) %>%
        select(year, month, day, codigo_nacional, temp_mean)
      if (nrow(tabla) > 0) return(tabla)
      dmc_empty_row(url, "temp_mean")
    }, error = function(e) dmc_empty_row(url, "temp_mean"))
  }
  urls <- dmc_collect_urls(ano_inicio, ano_actual, estaciones,
    "https://climatologia.meteochile.gob.cl/application/anual/temperaturaMediaAnual/")
  temp <- dmc_run_url_loop(urls, getData, slow = FALSE, value_col = "temp_mean")
  # Solo tope anti-parseo: el rango real lo aplica regla_de_magnitud y el QC
  # multinivel. Filtrar aca borraba la fila y el UPSERT dejaba congelado el valor
  # viejo (mismo bug que LOS LOROS 2026-08-22).
  result <- temp %>% filter(is.na(temp_mean) | abs(temp_mean) < 1e4)
  result$month <- as.numeric(result$month)
  result$year <- as.numeric(result$year)
  result <- result %>% filter(!(year == ano_actual & month > as.numeric(mes_ultimo)))
  return(result)
}


descargar_radiacion_DMC<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){
  getData<-function(url){
    tryCatch({
      html <- dmc_read_html_url(url)
      tabla<-html %>%
        html_elements("#excel") %>%
        html_table() %>%
        data.frame()
      tabla <- dmc_preparar_tabla_anual(tabla)
      if (nrow(tabla) == 0) return(dmc_empty_row(url, "rs_mean"))
      tabla <- tabla %>%
        pivot_longer(cols = -day, names_to = "month", values_to = "rs_mean") %>%
        mutate(rs_mean = gsub("^\\.$", NA_character_, rs_mean)) %>%
        mutate(rs_mean = as.numeric(rs_mean)) %>%
        mutate(day = as.numeric(day)) %>%
        mutate(month = dmc_mes_a_num(month)) %>%
        filter(!is.na(day)) %>%
        mutate(year = dmc_year_from_url(url)) %>%
        mutate(codigo_nacional = dmc_codigo_desde_url(url)) %>%
        arrange(month, day, year) %>%
        select(year, month, day, codigo_nacional, rs_mean)
      if (nrow(tabla) > 0) return(tabla)
      dmc_empty_row(url, "rs_mean")
    }, error = function(e) dmc_empty_row(url, "rs_mean"))
  }
  urls <- dmc_collect_urls(ano_inicio, ano_actual, estaciones,
    "https://climatologia.meteochile.gob.cl/application/anual/radiacionGlobalInstantaneaAnual/")
  temp <- dmc_run_url_loop(urls, getData, slow = TRUE, value_col = "rs_mean")
  result <- temp %>% filter(is.na(rs_mean) | abs(rs_mean) < 1e4)
  result$month <- as.numeric(result$month)
  result$year <- as.numeric(result$year)
  result <- result %>% filter(!(year == ano_actual & month > as.numeric(mes_ultimo)))
  return(result)
}

descargar_viento_DMC <- function(ano_inicio, ano_actual, mes_ultimo, estaciones) {
  getData <- function(url) {
    tryCatch({
      html <- dmc_read_html_url(url)
      # 200 + chrome sin celdas DIR/nudos: skip, no dead-mark (dead list es por fuente+codigo).
      if (!dmc_viento_html_tiene_celdas(html)) return(dmc_empty_viento_row(url))
      tabla <- html %>%
        html_elements("#excel") %>%
        html_table() %>%
        data.frame()
      tabla <- dmc_preparar_tabla_anual(tabla)
      if (nrow(tabla) == 0) return(dmc_empty_viento_row(url))
      tabla <- tabla %>%
        pivot_longer(cols = -day, names_to = "month", values_to = "celda") %>%
        mutate(day = as.numeric(day), month = dmc_mes_a_num(month)) %>%
        filter(!is.na(day))
      parsed <- dmc_parse_viento_celda(tabla$celda)
      tabla$u_10_kt <- parsed$u_10_kt
      tabla$u_10_dire <- parsed$u_10_dire
      tabla <- tabla %>%
        mutate(
          year = dmc_year_from_url(url),
          codigo_nacional = dmc_codigo_desde_url(url)
        ) %>%
        arrange(month, day, year) %>%
        select(year, month, day, codigo_nacional, u_10_kt, u_10_dire)
      if (nrow(tabla) > 0) return(tabla)
      dmc_empty_viento_row(url)
    }, error = function(e) {
      message("[dmc/u_10_kt] fallo ", url, ": ", conditionMessage(e))
      dmc_empty_viento_row(url)
    })
  }
  urls <- dmc_collect_urls(
    ano_inicio, ano_actual, estaciones,
    "https://climatologia.meteochile.gob.cl/application/anual/vientoPredominanteAnual10Direcciones/"
  )
  temp <- dmc_run_url_loop(urls, getData, slow = TRUE, value_col = "u_10_kt")
  # Solo tope anti-parseo; el rango meteorologico lo aplica QC (no congelar UPSERT).
  # No drop_na de la tabla ancha: NA en direccion no puede borrar el kt del dia.
  result <- temp %>% filter(is.na(u_10_kt) | abs(u_10_kt) < 1e4)
  if (!("u_10_dire" %in% names(result))) result$u_10_dire <- NA_real_
  result$month <- as.numeric(result$month)
  result$year <- as.numeric(result$year)
  result <- result %>%
    filter(!(year == ano_actual & month > as.numeric(mes_ultimo))) %>%
    select(year, month, day, codigo_nacional, u_10_kt, u_10_dire)
  result
}

# RE5036: region x dia, rafaga diaria (km/h HTML → m/s). Altura no indicada por la DMC. No es predominante ni u_max_diario.
DMC_VIENTO_MAX_REGIONES <- c(
  "15", "01", "02", "03", "04", "05", "13", "06",
  "07", "16", "08", "09", "14", "10", "11", "12"
)
DMC_VIENTO_MAX_KMH_TO_MS <- 1 / 3.6
DMC_VIENTO_MAX_URL_ROOT <-
  "https://climatologia.meteochile.gob.cl/application/diarioc/intensidadVientoEmaRedEstaciones/"

dmc_viento_max_regiones <- function() {
  raw <- trimws(Sys.getenv("INGEST_U10MAX_REGIONES", ""))
  if (!nzchar(raw)) return(DMC_VIENTO_MAX_REGIONES)
  toks <- trimws(unlist(strsplit(raw, ",", fixed = TRUE), use.names = FALSE))
  toks[nzchar(toks)]
}

dmc_viento_max_ventana <- function(fecha_ini, fecha_fin) {
  ini <- as.Date(fecha_ini)
  fin <- as.Date(fecha_fin)
  if (is.na(ini) || is.na(fin)) stop("dmc_viento_max_ventana: fechas invalidas")
  if (ini > fin) {
    tmp <- ini
    ini <- fin
    fin <- tmp
  }
  backfill <- if (exists("ingest_flag_true", mode = "function")) {
    ingest_flag_true("INGEST_U10MAX_BACKFILL")
  } else {
    tolower(Sys.getenv("INGEST_U10MAX_BACKFILL", "")) %in% c("1", "true", "yes", "si")
  }
  if (!isTRUE(backfill)) {
    lookback <- suppressWarnings(as.integer(Sys.getenv("INGEST_U10MAX_LOOKBACK_DAYS", "2")))
    if (is.na(lookback) || lookback < 0L) lookback <- 2L
    ini <- max(ini, fin - lookback)
  }
  list(ini = ini, fin = fin)
}

dmc_viento_max_empty_df <- function() {
  data.frame(
    year = integer(),
    month = integer(),
    day = integer(),
    codigo_nacional = character(),
    u_rafaga = numeric(),
    stringsAsFactors = FALSE
  )
}

dmc_viento_max_ymd_desde_url <- function(url) {
  m <- regexec(
    "intensidadVientoEmaRedEstaciones/([0-9]{1,2})/([0-9]{4})/([0-9]{2})/([0-9]{2})",
    url
  )
  g <- regmatches(url, m)[[1]]
  if (length(g) < 5L) return(as.Date(NA))
  as.Date(sprintf("%s-%s-%s", g[[3]], g[[4]], g[[5]]))
}

dmc_viento_max_urls <- function(fecha_ini, fecha_fin, regiones = NULL) {
  if (is.null(regiones)) regiones <- dmc_viento_max_regiones()
  fechas <- seq(as.Date(fecha_ini), as.Date(fecha_fin), by = "day")
  n <- length(fechas) * length(regiones)
  if (n == 0L) return(character(0))
  out <- character(n)
  k <- 0L
  for (i in seq_along(fechas)) {
    ymd <- format(fechas[[i]], "%Y/%m/%d")
    for (r in regiones) {
      k <- k + 1L
      out[[k]] <- paste0(DMC_VIENTO_MAX_URL_ROOT, r, "/", ymd)
    }
  }
  out
}

# Persistimos solo <strong> (max de las 24 h). La hermana no es hora-del-max.
dmc_parse_viento_max_html <- function(html, fecha) {
  fecha <- as.Date(fecha)
  if (is.na(fecha)) return(dmc_viento_max_empty_df())
  trs <- tryCatch(
    rvest::html_elements(html, "#excel tr.text-center"),
    error = function(e) NULL
  )
  if (is.null(trs) || length(trs) == 0L) return(dmc_viento_max_empty_df())
  y <- as.integer(format(fecha, "%Y"))
  mo <- as.integer(format(fecha, "%m"))
  d <- as.integer(format(fecha, "%d"))
  rows <- lapply(trs, function(tr) {
    tds <- rvest::html_elements(tr, "td")
    if (length(tds) < 1L) return(NULL)
    codigo <- trimws(rvest::html_text(tds[[1]]))
    if (!grepl("^[0-9]{5,6}$", codigo)) return(NULL)
    strong <- rvest::html_element(tr, "strong")
    kmh_raw <- trimws(rvest::html_text(strong))
    if (is.na(kmh_raw) || !nzchar(kmh_raw) || kmh_raw %in% c(".", "-")) return(NULL)
    kmh <- suppressWarnings(as.numeric(gsub(",", ".", kmh_raw, fixed = TRUE)))
    if (is.na(kmh) || abs(kmh) >= 1e4) return(NULL)
    data.frame(
      year = y, month = mo, day = d,
      codigo_nacional = codigo,
      u_rafaga = kmh * DMC_VIENTO_MAX_KMH_TO_MS,
      stringsAsFactors = FALSE
    )
  })
  keep <- !vapply(rows, is.null, logical(1))
  if (!any(keep)) return(dmc_viento_max_empty_df())
  dplyr::bind_rows(rows[keep])
}

dmc_viento_hora_empty_df <- function() {
  data.frame(
    year = integer(),
    month = integer(),
    day = integer(),
    hour = integer(),
    codigo_nacional = character(),
    u_rafaga_h = numeric(),
    stringsAsFactors = FALSE
  )
}

# Etiquetas <small>01</small>…<small>00</small> del encabezado. Sin encabezado: 01..23, 00.
dmc_viento_hora_labels <- function(html) {
  fallback <- c(seq.int(1L, 23L), 0L)
  smalls <- tryCatch(
    rvest::html_elements(html, "#excel th small"),
    error = function(e) NULL
  )
  if (is.null(smalls) || length(smalls) == 0L) return(fallback)
  labs <- suppressWarnings(as.integer(trimws(rvest::html_text(smalls))))
  labs <- labs[!is.na(labs) & labs >= 0L & labs <= 23L]
  if (length(labs) == 0L) return(fallback)
  labs <- labs[!duplicated(labs)]
  if (length(labs) > 24L) labs <- labs[seq_len(24L)]
  as.integer(labs)
}

# Hueco (vacio / "." / "-") no es fila. 0 si se queda. Hoy (<strong>) y Ayer no son horas.
dmc_parse_viento_hora_html <- function(html, fecha) {
  fecha <- as.Date(fecha)
  if (is.na(fecha)) return(dmc_viento_hora_empty_df())
  hours <- dmc_viento_hora_labels(html)
  trs <- tryCatch(
    rvest::html_elements(html, "#excel tr.text-center"),
    error = function(e) NULL
  )
  if (is.null(trs) || length(trs) == 0L) return(dmc_viento_hora_empty_df())
  y <- as.integer(format(fecha, "%Y"))
  mo <- as.integer(format(fecha, "%m"))
  d <- as.integer(format(fecha, "%d"))
  n_h <- length(hours)
  rows <- lapply(trs, function(tr) {
    tds <- rvest::html_elements(tr, "td")
    if (length(tds) < (2L + n_h)) return(NULL)
    codigo <- trimws(rvest::html_text(tds[[1]]))
    if (!grepl("^[0-9]{5,6}$", codigo)) return(NULL)
    out <- vector("list", n_h)
    for (i in seq_len(n_h)) {
      td <- tds[[2L + i]]
      if (length(rvest::html_elements(td, "strong")) > 0L) next
      raw <- trimws(rvest::html_text(td))
      if (is.na(raw) || !nzchar(raw) || raw %in% c(".", "-")) next
      kmh <- suppressWarnings(as.numeric(gsub(",", ".", raw, fixed = TRUE)))
      if (is.na(kmh) || abs(kmh) >= 1e4) next
      out[[i]] <- data.frame(
        year = y, month = mo, day = d,
        hour = as.integer(hours[[i]]),
        codigo_nacional = codigo,
        u_rafaga_h = kmh * DMC_VIENTO_MAX_KMH_TO_MS,
        stringsAsFactors = FALSE
      )
    }
    keep <- !vapply(out, is.null, logical(1))
    if (!any(keep)) return(NULL)
    dplyr::bind_rows(out[keep])
  })
  keep <- !vapply(rows, is.null, logical(1))
  if (!any(keep)) return(dmc_viento_hora_empty_df())
  dplyr::bind_rows(rows[keep])
}

dmc_viento_max_con_horario <- function(diario, horario) {
  if (is.null(diario) || !is.data.frame(diario)) diario <- dmc_viento_max_empty_df()
  if (is.null(horario) || !is.data.frame(horario) || nrow(horario) == 0L) {
    horario <- dmc_viento_hora_empty_df()
  }
  attr(diario, "horario") <- horario
  diario
}

descargar_viento_max_DMC <- function(fecha_ini, fecha_fin, regiones = NULL) {
  urls <- dmc_viento_max_urls(fecha_ini, fecha_fin, regiones)
  if (length(urls) == 0L) {
    return(dmc_viento_max_con_horario(dmc_viento_max_empty_df(), dmc_viento_hora_empty_df()))
  }
  getData <- function(url) {
    tryCatch({
      html <- dmc_read_html_url(url)
      fecha <- dmc_viento_max_ymd_desde_url(url)
      list(
        diario = dmc_parse_viento_max_html(html, fecha),
        horario = dmc_parse_viento_hora_html(html, fecha)
      )
    }, error = function(e) {
      message("[dmc/u_rafaga] fallo ", url, ": ", conditionMessage(e))
      list(
        diario = dmc_viento_max_empty_df(),
        horario = dmc_viento_hora_empty_df()
      )
    })
  }
  chunks <- vector("list", length(urls))
  for (i in seq_along(urls)) {
    if (exists("scrape_should_skip_rest", mode = "function") &&
        scrape_should_skip_rest("dmc")) {
      message("[dmc/u_rafaga] circuit skip resto URLs desde ", i, "/", length(urls))
      break
    }
    chunks[[i]] <- getData(urls[i])
    if (exists("dmc_budget_sleep", mode = "function")) {
      dmc_budget_sleep()
    } else if (exists("scrape_rate_limit_sleep", mode = "function")) {
      scrape_rate_limit_sleep("dmc", slow = TRUE)
    } else {
      Sys.sleep(5)
    }
    print(paste0("Voy en el enlace web número: ", i, "de ", length(urls)))
  }
  keep <- !vapply(chunks, is.null, logical(1))
  if (!any(keep)) {
    return(dmc_viento_max_con_horario(dmc_viento_max_empty_df(), dmc_viento_hora_empty_df()))
  }
  parts <- chunks[keep]
  diario <- dplyr::bind_rows(lapply(parts, `[[`, "diario"))
  horario <- dplyr::bind_rows(lapply(parts, `[[`, "horario"))
  if (nrow(diario) > 0L) {
    diario <- diario %>%
      filter(is.na(u_rafaga) | abs(u_rafaga) < 1e4) %>%
      select(year, month, day, codigo_nacional, u_rafaga)
  } else {
    diario <- dmc_viento_max_empty_df()
  }
  if (!is.data.frame(horario) || nrow(horario) == 0L) {
    horario <- dmc_viento_hora_empty_df()
  } else {
    horario <- horario %>%
      filter(!is.na(u_rafaga_h) & abs(u_rafaga_h) < 1e4) %>%
      select(year, month, day, hour, codigo_nacional, u_rafaga_h)
  }
  dmc_viento_max_con_horario(diario, horario)
}


