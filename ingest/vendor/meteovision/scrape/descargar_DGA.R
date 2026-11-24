#Extraer datos de la Direccion Meteorologica de Chile o bien Direccion general de Aguas. Depende de la variable.
#Es necesario correr archivo de Librerias.R para poder correr esta función

descargar_caudal_DGA<-function(ano_inicio, ano_actual, mes_ultimo, estaciones){
##################### Definimos funciones de utilidad #####################

#####1) Función para concatenar URL's #####

  getUrls2 <- function(year,cod_est){
    #Definir URL raíz del servidor (debe ser estática)
    root1<-"https://snia.mop.gob.cl/dgasat/pages/dgasat_param/dgasat_param_tablas.jsp?estacion1="
    #cod_est<-"07321005-4"
    root2<-"&estacion2=-1&estacion3=-1&parametros="
    root3<-"_12_Caudal+%28m3%2Fseg%29&accion=refresca&param=&tipo=ANO&fechaFinGrafico=04%2F05%2F2023&hora_fin=0&tiporep=S&period=rango&fechaInicioTabla=01%2F01%2F"
    #year<-"2018"
    root4<-"&fechaFinTabla=31%2F12%2F"
    root5<-"&UserID=nobody&EsDL1=&EsDL2=&EsDL3="
    #url<-paste0(root1,cod_est,root2,cod_est,root3,year,root4,year,root5)

    #Se genera el url completo
    urls <- NULL
    urls <- c(urls,paste0(root1,cod_est,root2,cod_est,root3,year,root4,year,root5))
    return(urls)
  }

  ####2) Función para descargar data desde Link ########

  getData <- function(url, intentos = 3) {
    tryCatch({
      response <- GET(url, add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36"))
      
      code <- suppressWarnings(as.integer(response$status_code))
      if (isTRUE(code == 500L)) {
        print(paste0("Estación '", substring(url, 85, 94), "' no está disponible en la web."))
        return(data.frame(year = as.numeric(substring(url, 322, 325)),
                         day = NA, month = NA, caudal_mean = NA,
                         codigo_nacional = substring(url, 85, 94), status_code = 500))
      }
      
      html <- read_html(response)

      # Extraer datos del HTML
      tabla <- html %>%
        html_elements("#datos") %>%
        html_table() %>%
        data.frame() 

      colnames(tabla) <- c("id", "fecha", "caudal_min", "caudal_max", "caudal_mean")

      # Convertir fecha y extraer componentes
      tabla$fecha <- as.Date(tabla$fecha, format = "%d/%m/%Y")
      tabla$month <- month(tabla$fecha)
      tabla$year <- year(tabla$fecha)
      tabla$day <- day(tabla$fecha)
      tabla$caudal_mean <- as.numeric(tabla$caudal_mean)  # Asegurar formato numérico

      # Procesar datos de caudal
      tabla <- tabla %>%
        mutate(caudal_mean = ifelse(caudal_mean < 0, NA, caudal_mean)) %>%
        mutate(codigo_nacional = substring(url, 85, 94), status_code = 200) %>%
        select(day, month, year, caudal_mean, codigo_nacional, status_code)

      # Si la tabla tiene datos, devolverla
      if (nrow(tabla) > 0) {
        return(tabla)
      } else {
        return(data.frame(year = as.numeric(substring(url, 322, 325)),
                          day = NA, month = NA, caudal_mean = NA,
                          codigo_nacional = substring(url, 85, 94), status_code = 204))
      }

    }, error = function(e) {
      if (intentos > 1) {
        message("Error de conexión en intento ", 4 - intentos, ": ", conditionMessage(e))
        Sys.sleep(0.5)
        return(getData(url, intentos - 1))
      } else {
        message("Error persistente tras varios intentos: ", conditionMessage(e))
        return(data.frame(year = NA, day = NA, month = NA, caudal_mean = NA, 
                         codigo_nacional = substring(url, 85, 94), status_code = NA))
      }
    })
  }
  ######################## Comienza rutina ################################

  result <- data.frame() # Inicializar resultado vacío
  estaciones_fallidas <- c() # Registro de estaciones con error
  
  # Generar todas las URLs como en la versión original
  urls <- NULL
  for(year in ano_inicio:ano_actual) {
    for(i in 1:length(estaciones$codigo_nacional)) {
      urls <- c(urls, getUrls2(year, estaciones$codigo_nacional[i]))
    }
  }

  # Procesar cada URL
  for(i in 1:length(urls)) {
    # Obtener el código de estación para esta URL
    codigo_estacion <- substring(urls[i], 85, 94)
    
    # Verificar si esta estación ya ha fallado
    if(codigo_estacion %in% estaciones_fallidas) {
      next
    }
    
    # Procesar la URL
    data <- getData(urls[i])
    
    # Si es error 500, registrar esta estación como fallida
    if(!is.na(data$status_code[1]) && data$status_code[1] == 500) {
      estaciones_fallidas <- c(estaciones_fallidas, codigo_estacion)
    } else if (!all(is.na(data$year))) {
      # Si hay datos, agregar al resultado
      data$status_code <- NULL # Remover columna auxiliar
      result <- rbind(result, data)
    }
    
    Sys.sleep(0.5)
    print(paste0("Voy en el enlace web número: ", i, " de ", length(urls)))
  }
  
  # Filtrar y procesar el resultado
  if (nrow(result) > 0) {
    result <- result %>% 
      # Solo tope anti-parseo: el rango real (0..1e6) lo aplica regla_de_magnitud.
      # El tope viejo de 10000 era mas estricto que el QC y borraba crecidas.
      filter(is.na(caudal_mean) | abs(caudal_mean) < 1e9) %>% 
      filter(!(year == ano_actual & month > as.numeric(mes_ultimo))) %>%
      select(year, month, day, codigo_nacional, caudal_mean)
  }
  
  return(result)
}
descargar_pp_DGA<-function(ano_inicio, ano_actual, mes_ultimo, estaciones,
                           fecha_tabla_ini = NULL, fecha_tabla_fin = NULL){
  param_root3 <- "_3_Pptaci%C3%B3n+Acum.+%28mm%29&accion=refresca&param=&tipo=ANO&fechaFinGrafico=04%2F05%2F2023&hora_fin=0&tiporep=S&period=rango&fechaInicioTabla="

  getData <- function(url, intentos = 2) {
  tryCatch({
    response <- dga_fetch_response(url, retries = intentos)
    code <- suppressWarnings(as.integer(response$status_code))
    if (isTRUE(code == 500L)) {
      print(paste0("Estación '", dga_codigo_desde_url(url), "' no está disponible en la web."))
      dga_scrape_mark(500L, "500")
      return(data.frame(year = NA, day = NA, month = NA, pp_day = NA,
                       codigo_nacional = dga_codigo_desde_url(url), status_code = 500))
    }

    tabla <- dga_read_datos_table(response, c("id", "fecha", "pp_min", "pp_max", "pp_mean"))
    if (is.null(tabla) || nrow(tabla) == 0L) {
      if (is.null(tabla)) {
        # dga_read_datos_table ya marco 204/parse
      } else {
        dga_scrape_mark(204L, "204")
      }
      return(data.frame(year = NA, day = NA, month = NA, pp_day = NA,
                        codigo_nacional = dga_codigo_desde_url(url), status_code = 204))
    }

    tabla$fecha <- as.Date(tabla$fecha, format = "%d/%m/%Y")
    tabla$month <- month(tabla$fecha)
    tabla$year <- year(tabla$fecha)
    tabla$day <- day(tabla$fecha)

    tabla$pp_day <- dga_pp_lag(tabla$pp_min)
    tabla <- tabla %>%
      mutate(codigo_nacional = dga_codigo_desde_url(url), status_code = 200) %>%
      select(day, month, year, pp_day, codigo_nacional, status_code)

    if (nrow(tabla) > 0) {
      dga_scrape_mark(200L, NA_character_)
      return(tabla)
    }
    dga_scrape_mark(204L, "204")
    return(data.frame(year = NA, day = NA, month = NA, pp_day = NA,
                      codigo_nacional = dga_codigo_desde_url(url), status_code = 204))

  }, error = function(e) {
    message("Error persistente: ", conditionMessage(e))
    return(data.frame(year = NA, day = NA, month = NA, pp_day = NA,
                      codigo_nacional = dga_codigo_desde_url(url), status_code = NA))
  })
  }

  urls <- dga_collect_urls(
    ano_inicio, ano_actual, estaciones, param_root3, fecha_tabla_ini, fecha_tabla_fin
  )
  result <- dga_run_scrape_loop(urls, getData)

  # Filtrar los resultados
  if (nrow(result) > 0) {
    result <- result %>% 
      # Solo tope anti-parseo: el rango real (0..500 mm) lo aplica regla_de_magnitud.
      filter(is.na(pp_day) | abs(pp_day) < 1e4) %>% 
      select(year, month, day, codigo_nacional, pp_day)
    
    result$month <- as.numeric(result$month)
    
    result <- result %>% 
      filter(!(year == ano_actual & month > as.numeric(mes_ultimo)))
  }

  return(result)
}
descargar_temp_DGA<-function(ano_inicio, ano_actual, mes_ultimo, estaciones,
                             fecha_tabla_ini = NULL, fecha_tabla_fin = NULL){
  param_root3 <- "_5_Temp.del+Aire+%28%C2%BAC%29&accion=refresca&param=&tipo=ANO&fechaFinGrafico=18%2F05%2F2023&hora_fin=0&tiporep=S&period=rango&fechaInicioTabla="

  getData <- function(url, intentos = 2) {
  tryCatch({
    response <- dga_fetch_response(url, retries = intentos)
    code <- suppressWarnings(as.integer(response$status_code))
    if (isTRUE(code == 500L)) {
      print(paste0("Estación '", dga_codigo_desde_url(url), "' no está disponible en la web."))
      dga_scrape_mark(500L, "500")
      return(data.frame(year = NA, day = NA, month = NA, temp_mean = NA,
                       temp_max = NA, temp_min = NA, codigo_nacional = dga_codigo_desde_url(url),
                       status_code = 500))
    }

    tabla <- dga_read_datos_table(response, c("id", "fecha", "temp_min", "temp_max", "temp_mean"))
    if (is.null(tabla) || nrow(tabla) == 0L) {
      if (!is.null(tabla)) dga_scrape_mark(204L, "204")
      return(data.frame(year = NA, day = NA, month = NA, temp_mean = NA, temp_max = NA,
                        temp_min = NA, codigo_nacional = dga_codigo_desde_url(url),
                        status_code = 204))
    }

    tabla$fecha <- as.Date(tabla$fecha, format = "%d/%m/%Y")
    tabla$month <- month(tabla$fecha)
    tabla$year <- year(tabla$fecha)
    tabla$day <- day(tabla$fecha)

    tabla <- tabla %>%
      mutate(codigo_nacional = dga_codigo_desde_url(url), status_code = 200) %>%
      select(day, month, year, codigo_nacional, temp_min, temp_max, temp_mean, status_code)

    if (nrow(tabla) > 0) {
      dga_scrape_mark(200L, NA_character_)
      return(tabla)
    }
    dga_scrape_mark(204L, "204")
    return(data.frame(year = NA, day = NA, month = NA, temp_mean = NA, temp_max = NA,
                      temp_min = NA, codigo_nacional = dga_codigo_desde_url(url),
                      status_code = 204))

  }, error = function(e) {
    message("Error persistente: ", conditionMessage(e))
    return(data.frame(year = NA, day = NA, month = NA, temp_mean = NA, temp_max = NA,
                      temp_min = NA, codigo_nacional = dga_codigo_desde_url(url),
                      status_code = NA))
  })
  }

  urls <- dga_collect_urls(
    ano_inicio, ano_actual, estaciones, param_root3, fecha_tabla_ini, fecha_tabla_fin
  )
  result <- dga_run_scrape_loop(urls, getData)

  # Filtrar los resultados
  if (nrow(result) > 0) {
    result <- result %>%
      # Sin filtro de rango meteorologico: el valor entra y el QC multinivel lo
      # marca. Filtrar aca borraba la fila entera, y como el persist es UPSERT el
      # valor viejo quedaba congelado en el mapa: LOS LOROS 2026-08-22 publicaba
      # 84,41 C en la fuente, se descartaba, y el 46,98 C anterior seguia vivo.
      # Solo se descarta residuo de parseo, no valores extremos.
      filter(
        is.na(temp_max)  | abs(temp_max)  < 1e4,
        is.na(temp_min)  | abs(temp_min)  < 1e4,
        is.na(temp_mean) | abs(temp_mean) < 1e4
      ) %>%
      filter(!(year == ano_actual & month > as.numeric(mes_ultimo))) %>%
      select(year, month, day, codigo_nacional, temp_min, temp_max, temp_mean)
  }

  return(result)
}



descargar_viento_DGA<-function(ano_inicio, ano_actual, mes_ultimo, estaciones,
                               fecha_tabla_ini = NULL, fecha_tabla_fin = NULL){
  param_root3 <- "_82_Velocidad%20del%20Vto.%20%28m%2Fs%29&accion=refresca&param=&tipo=ANO&fechaFinGrafico=18%2F05%2F2023&hora_fin=0&tiporep=S&period=rango&fechaInicioTabla="

  getData <- function(url, intentos = 2) {
  tryCatch({
    response <- dga_fetch_response(url, retries = intentos)
    code <- suppressWarnings(as.integer(response$status_code))
    if (isTRUE(code == 500L)) {
      print(paste0("Estación '", dga_codigo_desde_url(url), "' no está disponible en la web."))
      dga_scrape_mark(500L, "500")
      return(data.frame(year = NA, day = NA, month = NA, u2_mean = NA,
                       u_max_diario = NA, u2_min = NA, codigo_nacional = dga_codigo_desde_url(url),
                       status_code = 500))
    }

    tabla <- dga_read_datos_table(response, c("id", "fecha", "u2_min", "u_max_diario", "u2_mean"))
    if (is.null(tabla) || nrow(tabla) == 0L) {
      if (!is.null(tabla)) dga_scrape_mark(204L, "204")
      return(data.frame(year = NA, day = NA, month = NA, u2_mean = NA, u_max_diario = NA,
                        u2_min = NA, codigo_nacional = dga_codigo_desde_url(url),
                        status_code = 204))
    }

    tabla$fecha <- as.Date(tabla$fecha, format = "%d/%m/%Y")
    tabla$month <- month(tabla$fecha)
    tabla$year <- year(tabla$fecha)
    tabla$day <- day(tabla$fecha)

    tabla <- tabla %>%
      mutate(codigo_nacional = dga_codigo_desde_url(url), status_code = 200) %>%
      select(day, month, year, codigo_nacional, u2_min, u_max_diario, u2_mean, status_code)

    if (nrow(tabla) > 0) {
      dga_scrape_mark(200L, NA_character_)
      return(tabla)
    }
    dga_scrape_mark(204L, "204")
    return(data.frame(year = NA, day = NA, month = NA, u2_mean = NA, u_max_diario = NA,
                      u2_min = NA, codigo_nacional = dga_codigo_desde_url(url),
                      status_code = 204))

  }, error = function(e) {
    message("Error persistente: ", conditionMessage(e))
    return(data.frame(year = NA, day = NA, month = NA, u2_mean = NA, u_max_diario = NA,
                      u2_min = NA, codigo_nacional = dga_codigo_desde_url(url),
                      status_code = NA))
  })
  }

  urls <- dga_collect_urls(
    ano_inicio, ano_actual, estaciones, param_root3, fecha_tabla_ini, fecha_tabla_fin
  )
  result <- dga_run_scrape_loop(urls, getData)

  if (nrow(result) > 0) {
    result <- result %>%
      # Solo tope anti-parseo: el rango real lo aplica regla_de_magnitud.
      # Filtrar aca borraba la fila y el UPSERT dejaba congelado el valor viejo.
      filter(
        is.na(u_max_diario)  | abs(u_max_diario)  < 1e4,
        is.na(u2_min)  | abs(u2_min)  < 1e4,
        is.na(u2_mean) | abs(u2_mean) < 1e4
      ) %>%
      filter(!(year == ano_actual & month > as.numeric(mes_ultimo))) %>%
      select(year, month, day, codigo_nacional, u2_min, u_max_diario, u2_mean)
  }

  return(result)
}

# SNIA ANO: id, fecha, min, max, promedio (5). La direccion es circular: el
# promedio es la serie diaria. 3 columnas = un solo valor. Fuera de 0-360 es
# residuo de parseo (NA), no un descarte de la fila.
dga_viento_dire_vector <- function(tabla) {
  if (is.null(tabla) || !is.data.frame(tabla) || nrow(tabla) == 0L) return(NULL)
  nc <- ncol(tabla)
  raw <- if (nc == 5L) tabla[[5L]] else if (nc == 3L) tabla[[3L]] else NULL
  if (is.null(raw)) return(NULL)
  x <- suppressWarnings(as.numeric(as.character(raw)))
  x[!is.finite(x) | x < 0 | x > 360] <- NA_real_
  x
}

# status 204, nunca 500: dga_run_scrape_loop marca dead solo con 500, y esa
# lista es por fuente+codigo (saltaria PP/T de la estacion en el diario).
dga_empty_dire_row <- function(url) {
  data.frame(
    year = NA, day = NA, month = NA, u2_dire = NA_real_,
    codigo_nacional = dga_codigo_desde_url(url),
    status_code = 204L,
    stringsAsFactors = FALSE
  )
}

DGA_VIENTO_DIRE_PARAM <- paste0(
  "_66_Direccion+del+Viento+%28grados%29",
  "&accion=refresca&param=&tipo=ANO&fechaFinGrafico=18%2F05%2F2023",
  "&hora_fin=0&tiporep=S&period=rango&fechaInicioTabla="
)

descargar_viento_dire_DGA <- function(ano_inicio, ano_actual, mes_ultimo, estaciones,
                                      fecha_tabla_ini = NULL, fecha_tabla_fin = NULL) {
  getData <- function(url, intentos = 2) {
    tryCatch({
      response <- dga_fetch_response(url, retries = intentos)
      code <- suppressWarnings(as.integer(response$status_code))
      if (isTRUE(code == 500L)) {
        message("[dga/u2_dire] 500 ", dga_codigo_desde_url(url), " (sin dead-mark)")
        return(dga_empty_dire_row(url))
      }
      html <- xml2::read_html(response)
      tabs <- rvest::html_table(rvest::html_elements(html, "#datos"))
      if (length(tabs) < 1L) return(dga_empty_dire_row(url))
      tabla <- as.data.frame(tabs[[1L]], stringsAsFactors = FALSE)
      vals <- dga_viento_dire_vector(tabla)
      if (is.null(vals)) {
        dga_scrape_mark(200L, "parse")
        return(dga_empty_dire_row(url))
      }
      fecha <- as.Date(as.character(tabla[[2L]]), format = "%d/%m/%Y")
      out <- data.frame(
        year = year(fecha),
        month = month(fecha),
        day = day(fecha),
        codigo_nacional = dga_codigo_desde_url(url),
        u2_dire = vals,
        status_code = 200L,
        stringsAsFactors = FALSE
      )
      if (nrow(out) > 0L && any(!is.na(out$year))) {
        dga_scrape_mark(200L, NA_character_)
        return(out)
      }
      dga_empty_dire_row(url)
    }, error = function(e) {
      message("[dga/u2_dire] fallo ", url, ": ", conditionMessage(e))
      dga_empty_dire_row(url)
    })
  }

  urls <- dga_collect_urls(
    ano_inicio, ano_actual, estaciones, DGA_VIENTO_DIRE_PARAM,
    fecha_tabla_ini, fecha_tabla_fin
  )
  result <- dga_run_scrape_loop(urls, getData)
  if (nrow(result) > 0L) {
    result <- result %>%
      filter(!(year == ano_actual & month > as.numeric(mes_ultimo))) %>%
      select(year, month, day, codigo_nacional, u2_dire)
  }
  result
}

# Rafaga horaria SNIA _76_ (tiporep=I). km/h en el HTML, m/s al salir.
# No pasa por dga_run_scrape_loop: un 500 de sesion no puede dead-mark
# la estacion (eso saltaria PP/T en el diario).
DGA_RAFAGA_H_PARAM <- paste0(
  "_76_Velocidad%20del%20vto%20max.%20%28Km%2FHra%29",
  "&accion=refresca&param=&tipo=ANO&fechaFinGrafico=18%2F05%2F2023",
  "&hora_fin=0&tiporep=I&period=rango&fechaInicioTabla="
)
DGA_RAFAGA_H_PAG0 <- paste0(
  "https://snia.mop.gob.cl/dgasat/pages/dgasat_param/",
  "dgasat_param_tablas_instantaneos.jsp?pag=0"
)
DGA_RAFAGA_H_WINDOW_DAYS <- 60L
DGA_RAFAGA_KMH_TO_MS <- 1 / 3.6

dga_rafaga_h_rows <- function(tabla, cod) {
  if (is.null(tabla) || !is.data.frame(tabla) || nrow(tabla) == 0L || ncol(tabla) < 3L) {
    return(NULL)
  }
  ts_raw <- gsub("\u00a0", " ", as.character(tabla[[2L]]), fixed = TRUE)
  ts_raw <- trimws(ts_raw)
  ts <- as.POSIXct(ts_raw, format = "%d/%m/%Y %H:%M", tz = "America/Santiago")
  kmh <- suppressWarnings(as.numeric(gsub(",", ".", as.character(tabla[[3L]]), fixed = TRUE)))
  ok <- !is.na(ts) & is.finite(kmh) & abs(kmh) < 1e4
  if (!any(ok)) return(NULL)
  data.frame(
    codigo_nacional = as.character(cod),
    ts = ts[ok],
    kmh = kmh[ok],
    u_rafaga_h = kmh[ok] * DGA_RAFAGA_KMH_TO_MS,
    stringsAsFactors = FALSE
  )
}

dga_rafaga_h_parse_html <- function(html, cod = "05706002-6") {
  doc <- xml2::read_html(html)
  tabs <- rvest::html_table(rvest::html_elements(doc, "#datos"))
  if (length(tabs) < 1L) return(NULL)
  tabla <- as.data.frame(tabs[[1L]], stringsAsFactors = FALSE)
  dga_rafaga_h_rows(tabla, cod)
}

dga_rafaga_h_truncated <- function(rows, ini, fin) {
  if (is.null(rows) || !is.data.frame(rows) || nrow(rows) < 1L) return(FALSE)
  if (nrow(rows) >= 4000L) return(TRUE)
  dias <- as.Date(rows$ts, tz = "America/Santiago")
  min_d <- suppressWarnings(min(dias, na.rm = TRUE))
  max_d <- suppressWarnings(max(dias, na.rm = TRUE))
  ini <- as.Date(ini)
  fin <- as.Date(fin)
  nrow(rows) >= 100L &&
    !is.na(min_d) && min_d > ini + 2L &&
    !is.na(max_d) && max_d >= fin - 1L
}

dga_rafaga_h_bind <- function(parts) {
  rows <- lapply(parts, function(p) p$rows)
  rows <- rows[!vapply(rows, is.null, logical(1))]
  rows <- rows[vapply(rows, function(r) is.data.frame(r) && nrow(r) > 0L, logical(1))]
  out <- if (length(rows) == 0L) NULL else do.call(rbind, rows)
  if (!is.null(out) && nrow(out) > 1L) {
    key <- paste(out$codigo_nacional, format(out$ts, "%Y-%m-%d %H:%M", tz = "America/Santiago"))
    out <- out[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  }
  list(status = 200L, rows = out, incomplete = FALSE)
}

# pag=1 (el 302) cabe ~48 filas. pag=0 es el volcado de la ventana, con la
# cookie que dejo el GET de tablas.jsp. Ceiling: ~4400 filas; por eso 60 dias.
dga_rafaga_h_read_response <- function(response, cod) {
  if (is.null(response)) return(NULL)
  tabla <- tryCatch(
    dga_read_datos_table(response, c("id", "fecha", "kmh")),
    error = function(e) NULL
  )
  dga_rafaga_h_rows(tabla, cod)
}

# Un GET frio de tiporep=I redirige a http://instantaneos.jsp y esa JSP
# responde 500: la sesion no queda armada. El mismo handle hace el POST
# del formulario (sin recaptcha; asi respondio SNIA) y no sigue el 302.
# El volcado https ?pag=0 reusa la cookie.
dga_rafaga_h_get <- function(url, follow = TRUE) {
  ua <- httr::add_headers("User-Agent" = DGA_UA)
  h <- dga_get_handle()
  cfg <- if (isTRUE(follow)) httr::config() else httr::config(followlocation = 0L)
  tryCatch(
    httr::GET(url, ua, handle = h, cfg, httr::timeout(dga_timeout_sec(210))),
    error = function(e) NULL
  )
}

dga_rafaga_h_warm <- function(cod, ini, fin) {
  base <- "https://snia.mop.gob.cl/dgasat/pages/dgasat_param"
  dga_rafaga_h_get(paste0(base, "/dgasat_param.jsp?param=1"), follow = TRUE)
  fecha_ini <- format(as.Date(ini), "%d/%m/%Y")
  fecha_fin <- format(as.Date(fin), "%d/%m/%Y")
  body <- list(
    estacion1 = cod, estacion2 = "-1", estacion3 = "-1",
    tiporep = "I", period = "rango",
    fechaInicioTabla = fecha_ini, fechaFinTabla = fecha_fin,
    accion = "refresca", param = "1", tipo = "ANO",
    hora_fin = "0", fechaFinGrafico = fecha_fin, UserID = "nobody",
    EsDL1 = "", EsDL2 = "", EsDL3 = ""
  )
  h <- dga_get_handle()
  to <- httr::timeout(dga_timeout_sec(210))
  httr::POST(
    paste0(base, "/dgasat_param_1.jsp"),
    httr::add_headers("User-Agent" = DGA_UA, Referer = paste0(base, "/dgasat_param.jsp?param=1")),
    handle = h, body = body, encode = "form", to
  )
  body$param <- ""
  body$parametros <- paste0(cod, "_76_Velocidad del vto max. (Km/Hra)")
  httr::POST(
    paste0(base, "/dgasat_param_tablas.jsp"),
    httr::add_headers("User-Agent" = DGA_UA, Referer = paste0(base, "/dgasat_param_1.jsp")),
    handle = h, body = body, encode = "form",
    httr::config(followlocation = 0L), to
  )
}

dga_rafaga_h_fetch_url <- function(cod, ini, fin, retry500 = TRUE) {
  url <- dga_build_url(
    cod, DGA_RAFAGA_H_PARAM, format(as.Date(ini), "%Y"),
    as.Date(ini), as.Date(fin)
  )
  # url queda armada igual que el enlace de ingesta; la sesion la llena el POST.
  if (!grepl("tiporep=I", url, fixed = TRUE)) stop("url rafaga sin tiporep=I")
  warm <- tryCatch(dga_rafaga_h_warm(cod, ini, fin), error = function(e) NULL)
  warm_code <- if (is.null(warm)) NA_integer_ else suppressWarnings(as.integer(warm$status_code))
  full_resp <- if (!is.null(warm) && !isTRUE(warm_code == 500L)) {
    dga_rafaga_h_get(DGA_RAFAGA_H_PAG0, follow = TRUE)
  } else {
    NULL
  }
  full_code <- if (is.null(full_resp)) NA_integer_ else suppressWarnings(as.integer(full_resp$status_code))
  if ((is.null(warm) || isTRUE(warm_code == 500L) || isTRUE(full_code == 500L)) && isTRUE(retry500)) {
    message("[dga/u_rafaga_h] 500 ", cod, " reset handle y reintento")
    if (exists("dga_reset_handle", mode = "function")) dga_reset_handle()
    return(dga_rafaga_h_fetch_url(cod, ini, fin, retry500 = FALSE))
  }
  if (is.null(full_resp) || isTRUE(full_code == 500L)) {
    message("[dga/u_rafaga_h] 500 ", cod, " (sin dead-mark)")
    return(list(status = 500L, rows = NULL, incomplete = TRUE))
  }
  rows <- dga_rafaga_h_read_response(full_resp, cod)
  status <- if (is.null(rows) || nrow(rows) == 0L) 204L else 200L
  list(status = status, rows = rows, incomplete = FALSE)
}

dga_rafaga_h_fetch_range <- function(cod, ini, fin) {
  ini <- as.Date(ini)
  fin <- as.Date(fin)
  if (as.integer(fin - ini) + 1L > DGA_RAFAGA_H_WINDOW_DAYS) {
    parts <- list()
    cursor <- ini
    while (cursor <= fin) {
      wfin <- min(cursor + DGA_RAFAGA_H_WINDOW_DAYS - 1L, fin)
      part <- dga_rafaga_h_fetch_range(cod, cursor, wfin)
      if (isTRUE(part$incomplete) || isTRUE(part$status == 500L)) return(part)
      parts[[length(parts) + 1L]] <- part
      cursor <- wfin + 1L
    }
    return(dga_rafaga_h_bind(parts))
  }
  got <- dga_rafaga_h_fetch_url(cod, ini, fin)
  if (isTRUE(got$status == 500L) || isTRUE(got$incomplete)) return(got)
  if (dga_rafaga_h_truncated(got$rows, ini, fin) && as.integer(fin - ini) >= 1L) {
    mid <- ini + as.integer(floor(as.integer(fin - ini) / 2))
    if (mid < ini || mid >= fin) {
      got$incomplete <- TRUE
      return(got)
    }
    left <- dga_rafaga_h_fetch_range(cod, ini, mid)
    if (isTRUE(left$incomplete) || isTRUE(left$status == 500L)) return(left)
    right <- dga_rafaga_h_fetch_range(cod, mid + 1L, fin)
    if (isTRUE(right$incomplete) || isTRUE(right$status == 500L)) return(right)
    return(dga_rafaga_h_bind(list(left, right)))
  }
  got
}

# Una estacion, un rango. Ventanas de 60 dias. status 500 / incomplete:
# el backfill no avanza el cursor.
descargar_rafaga_h_DGA <- function(cod, fecha_ini, fecha_fin) {
  dga_rafaga_h_fetch_range(as.character(cod)[1], as.Date(fecha_ini), as.Date(fecha_fin))
}

