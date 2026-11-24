#* @apiTitle Estaciones
#* Lista estaciones (proyeccion segun 'minimal') con filtros opcionales
#* @param macrozona Filtro opcional (Norte/Centro/Sur/Austral)
#* @param region Codigo region opcional
#* @param chile_only Si "1" o "true", filtra solo Chile (bbox). Default: 1
#* @param minimal Si "1", devuelve solo id,nombre,lat,lon,macrozona (para mapa)
#* @param con_datos Si "1", solo estaciones con al menos una observacion
#* @param id_variable Filtrar por variable (solo estaciones con datos de esa variable)
#* @param id_fuente Filtrar por operador/fuente (ID numerico)
#* @param comuna Nombre de comuna (nom_com). Vacio = sin filtrar
#* @param catalog Si "1", proyeccion enriquecida para wizard descarga (geo + cuenca)
#* @param cuenca Filtro nom_cuen exacto (trim)
#* @param subcuenca Filtro nom_subc exacto (trim)
#* @param q Texto de busqueda (nombre o comuna); minimo 2 caracteres; LIMIT 20
#* @get /estaciones
function(macrozona = "", region = "", comuna = "", chile_only = "1", minimal = "", catalog = "",
         con_datos = "", id_fuente = "", id_variable = "", cuenca = "", subcuenca = "", q = "", req, res) {
  sel_catalog <- tolower(catalog) %in% c("1", "true", "yes", "si")
  sel_minimal <- tolower(minimal) %in% c("1", "true", "yes", "si") && !sel_catalog
  filtro_con_datos <- tolower(con_datos) %in% c("1", "true", "yes", "si")
  q_txt <- trimws(as.character(if (length(q)) q[[1]] else ""))
  q_txt <- gsub("[%_]", "", q_txt)
  if (nchar(q_txt) < 2L) q_txt <- ""
  q_like <- if (nzchar(q_txt)) paste0("%", q_txt, "%") else ""

  key <- cache_key("estaciones", as.integer(sel_minimal), as.integer(sel_catalog), macrozona, region, comuna,
                   tolower(chile_only), as.integer(filtro_con_datos), id_fuente, id_variable, cuenca, subcuenca,
                   q_txt,
                   as.integer(alerta_temprana_tabla_existe()), alerta_temprana_min_anios(),
                   alerta_temprana_nivel_minimo(), alerta_mapa_horizonte_corto(), alerta_mapa_horizonte_largo(), 6L)

  cache_or_compute(key, function() {
    if (sel_catalog || sel_minimal) {
      cal <- calidad_join_sql("cal")
      # catalog=1 es el wizard de descarga: geo + cuenca. Los LATERAL de alerta
      # son del mapa (minimal) y con LIMIT 5000 pasan el proxy_read_timeout de 8s.
      al <- if (sel_catalog) list(join = "", select = "") else alerta_mapa_dual_join_sql()
      tiene_calidad <- nzchar(cal$join)

      has_var_filter <- nzchar(id_variable) && !is.na(as.integer(id_variable))
      geo_cols <- if (sel_catalog) {
        ", e.cod_reg, e.nom_reg, e.nom_com, e.nom_cuen AS cuenca, e.nom_subc AS subcuenca, e.id_fuente, fu.nombre AS fuente"
      } else {
        ""
      }
      fuente_join <- if (sel_catalog) " LEFT JOIN cantera fu ON fu.id = e.id_fuente" else ""

      if (tiene_calidad) {
        q <- paste0(
          "SELECT e.id, e.nombre_publico AS nombre, e.lat, e.lon, e.macrozona",
          geo_cols,
          cal$select,
          al$select,
          " FROM mirador e",
          fuente_join,
          cal$join,
          al$join,
          " WHERE 1=1"
        )
        if (has_var_filter) {
          vids <- estaciones_expand_id_variable(id_variable)
          q <- paste0(q, estaciones_exists_variable_sql(vids))
        } else if (filtro_con_datos) {
          q <- paste0(q, " AND cal.id_estacion IS NOT NULL")
        }
      } else {
        q <- paste0(
          "SELECT e.id, e.nombre_publico AS nombre, e.lat, e.lon, e.macrozona",
          geo_cols, al$select,
          " FROM mirador e", fuente_join, al$join
        )
        fid_present <- nzchar(id_fuente) && id_fuente != "" && id_fuente != "0" && !is.na(as.integer(id_fuente))
        if (filtro_con_datos || has_var_filter) {
          vids <- if (has_var_filter) estaciones_expand_id_variable(id_variable) else integer(0)
          if (fid_present && has_var_filter && length(vids) > 0L) {
            q <- paste0(q, " WHERE 1=1")
            q <- paste0(q, estaciones_exists_variable_sql(vids))
          } else {
            subq <- if (has_var_filter && length(vids) > 0L) {
              estaciones_distinct_estaciones_subq(vids)
            } else {
              "SELECT DISTINCT id_estacion FROM bitacora_oficial"
            }
            q <- paste0(q, " JOIN (", subq, ") o ON e.id = o.id_estacion")
            q <- paste0(q, " WHERE 1=1")
          }
        } else {
          q <- paste0(q, " WHERE 1=1")
        }
      }
    } else {
      # ponytail: el DISTINCT de bitacora_oficial hace SkipScan de todos los chunks
      # (~43 s en frio). estacion_calidad coincide con tiene_datos y la refresca el diario.
      # Si la vista no existe, se queda el DISTINCT.
      tiene_calidad <- identical(mv_estacion_calidad_nombre(), "estacion_calidad")
      tiene_join <- if (tiene_calidad) {
        " LEFT JOIN estacion_calidad o ON e.id = o.id_estacion"
      } else {
        " LEFT JOIN (SELECT DISTINCT id_estacion FROM bitacora_oficial) o ON e.id = o.id_estacion"
      }
      nivel_sql <- if (tiene_calidad) ", COALESCE(o.nivel, 'limitada') AS nivel" else ""
      q <- paste0(
        "SELECT e.id, e.nombre_publico AS nombre, e.lat, e.lon, e.tipo, e.macrozona, e.nom_reg, e.nom_com, e.id_fuente,
        e.fecha_ini,
        (o.id_estacion IS NOT NULL) AS tiene_datos",
        nivel_sql,
        " FROM mirador e",
        tiene_join,
        "
        WHERE 1=1"
      )
    }
    params <- list()
    if (tolower(chile_only) %in% c("1", "true", "yes", "si")) {
      q <- paste0(q, " AND e.lat BETWEEN -56 AND -17 AND e.lon BETWEEN -76 AND -66")
    }
    if (!is.null(macrozona) && nzchar(macrozona)) {
      q <- paste0(q, " AND e.macrozona = $", length(params)+1)
      params <- c(params, macrozona)
    }
    if (!is.null(region) && nzchar(region)) {
      q <- paste0(q, " AND e.cod_reg = $", length(params)+1)
      params <- c(params, as.integer(region))
    }
    if (!is.null(comuna) && nzchar(comuna)) {
      q <- paste0(q, " AND TRIM(COALESCE(e.nom_com, '')) = $", length(params)+1)
      params <- c(params, trimws(as.character(comuna)))
    }
    if (!is.null(cuenca) && nzchar(cuenca)) {
      q <- paste0(q, " AND TRIM(COALESCE(e.nom_cuen, '')) = $", length(params)+1)
      params <- c(params, trimws(as.character(cuenca)))
    }
    if (!is.null(subcuenca) && nzchar(subcuenca)) {
      q <- paste0(q, " AND TRIM(COALESCE(e.nom_subc, '')) = $", length(params)+1)
      params <- c(params, trimws(as.character(subcuenca)))
    }
    if (!is.null(id_fuente) && id_fuente != "" && id_fuente != "0") {
      fid <- as.integer(id_fuente)
      if (!is.na(fid) && fid > 0) {
        q <- paste0(q, " AND e.id_fuente = $", length(params)+1)
        params <- c(params, fid)
      }
    }
    if (nzchar(q_like)) {
      q <- paste0(q, " AND (e.nombre_publico ILIKE $", length(params)+1,
                  " OR TRIM(COALESCE(e.nom_com, '')) ILIKE $", length(params)+1, ")")
      params <- c(params, q_like)
      if (filtro_con_datos && !sel_catalog && !sel_minimal) {
        q <- paste0(q, " AND o.id_estacion IS NOT NULL")
      }
    }
    if (nzchar(q_like)) {
      q <- paste0(q, " ORDER BY e.nombre_publico LIMIT 20")
    } else {
      q <- paste0(q, if (sel_minimal || sel_catalog) " ORDER BY e.id LIMIT 5000" else "")
    }
    df <- db_query(q, params = params)
    if (!sel_minimal && "tiene_datos" %in% names(df)) df$tiene_datos <- as.logical(df$tiene_datos)
    if (sel_minimal) df <- alerta_mapa_df_to_rows(alerta_mapa_df_attach(df))
    df
  }, ttl = 900, res = res)
}

#* Estaciones mas cercanas a un punto (Haversine)
#* @param lat Latitud (requerido)
#* @param lon Longitud (requerido)
#* @param limit Maximo resultados (default 5)
#* @param id_variable Filtrar por variable con datos
#* @param id_fuente Filtrar por operador/fuente
#* @param con_datos Si "1", solo estaciones con observaciones
#* @param chile_only Si "1", bbox Chile
#* @param minimal Si "1", proyeccion reducida para mapa
#* @get /estaciones/cercanas
function(lat = "", lon = "", limit = "5", id_fuente = "", id_variable = "",
         con_datos = "1", chile_only = "1", minimal = "1", req, res) {
  lat_n <- suppressWarnings(as.numeric(lat))
  lon_n <- suppressWarnings(as.numeric(lon))
  if (is.na(lat_n) || lat_n < -90 || lat_n > 90) {
    res$status <- 400
    return(list(error = "lat inv\u00e1lida (requerida, -90 a 90)"))
  }
  if (is.na(lon_n) || lon_n < -180 || lon_n > 180) {
    res$status <- 400
    return(list(error = "lon inv\u00e1lida (requerida, -180 a 180)"))
  }
  nlim <- suppressWarnings(as.integer(limit))
  if (is.na(nlim) || nlim < 1L) nlim <- 5L
  if (nlim > 20L) nlim <- 20L

  filtro_con_datos <- tolower(con_datos) %in% c("1", "true", "yes", "si")
  only_chile <- tolower(chile_only) %in% c("1", "true", "yes", "si")
  has_var <- nzchar(id_variable) && !is.na(as.integer(id_variable))
  vid <- if (has_var) as.integer(id_variable) else NA_integer_
  fid_present <- nzchar(id_fuente) && id_fuente != "0" && !is.na(as.integer(id_fuente))
  fid <- if (fid_present) as.integer(id_fuente) else NA_integer_

  key <- cache_key("estaciones:cercanas",
    round(lat_n, 3), round(lon_n, 3), nlim, fid %||% "-", vid %||% "-",
    as.integer(filtro_con_datos), as.integer(only_chile), 2L)

  df <- cache_or_compute(key, function() {
    cal <- calidad_join_sql("cal")
    dist_sql <- paste0(
      "(6371 * acos(LEAST(1.0, GREATEST(-1.0, ",
      "cos(radians($1)) * cos(radians(e.lat)) * cos(radians(e.lon) - radians($2)) + ",
      "sin(radians($1)) * sin(radians(e.lat)) ",
      ")))) AS dist_km"
    )
    q <- paste0(
      "SELECT e.id, e.nombre_publico AS nombre, e.nom_com, e.id_fuente, e.lat, e.lon, e.macrozona",
      cal$select, ", ", dist_sql,
      " FROM mirador e",
      cal$join,
      " WHERE e.lat IS NOT NULL AND e.lon IS NOT NULL"
    )
    params <- list(lat_n, lon_n)
    if (only_chile) {
      q <- paste0(q, " AND e.lat BETWEEN -56 AND -17 AND e.lon BETWEEN -76 AND -66")
    }
    if (fid_present && !is.na(fid)) {
      q <- paste0(q, " AND e.id_fuente = $", length(params) + 1)
      params <- c(params, fid)
    }
    if (has_var && !is.na(vid)) {
      vids <- estaciones_expand_id_variable(id_variable)
      q <- paste0(q, estaciones_exists_variable_sql(vids))
    } else if (filtro_con_datos && nzchar(cal$join)) {
      # ponytail: EXISTS sobre bitacora_oficial recorre ~43 M filas (15 s).
      # estacion_calidad ya lista las que tienen datos (2684 = el DISTINCT).
      q <- paste0(q, " AND cal.id_estacion IS NOT NULL")
    } else if (filtro_con_datos) {
      q <- paste0(q, " AND EXISTS (SELECT 1 FROM bitacora_oficial o WHERE o.id_estacion = e.id)")
    }
    q <- paste0(q, " ORDER BY dist_km LIMIT ", nlim)
    db_query(q, params = params)
  }, ttl = 900, res = res)

  if (is.list(df) && !is.null(df$error)) return(df)
  list(data = df)
}

#* Detalle de una estacion
#* @param id ID de la estacion
#* @get /estaciones/<id>
function(id, req, res) {
  eid <- as.integer(id)
  if (is.na(eid)) { res$status <- 400; return(list(error = "id inv\u00e1lido")) }
  cache_or_compute(cache_key("estacion", eid), function() {
    df <- db_query("SELECT id, nombre, lat, lon, tipo, macrozona, cod_reg, nom_reg, nom_com, id_fuente,
      fecha_ini, altura, nom_cuen as cuenca, nom_subc as subcuenca FROM mirador WHERE id = $1", params = list(eid))
    if (nrow(df) == 0) return(list(error = "No encontrada"))
    row <- df[1, , drop = FALSE]
    stats::setNames(lapply(seq_along(row), function(j) row[[j]][[1]]), names(row))
  }, ttl = 3600, res = res)
}

#* Aviso confirmado de extremos (lluvia y temperatura ya observadas) y su resumen.
#* @param id ID de la estacion
#* @serializer contentType list(type="application/json")
#* @get /estaciones/<id>/extremos-aviso
function(id, req, res) {
  eid <- as.integer(id)
  if (is.na(eid)) {
    res$status <- 400
    return(charToRaw(jsonlite::toJSON(list(error = "id inv\u00e1lido"), auto_unbox = TRUE)))
  }
  vacio <- list(avisos = list(), resumenes = list())
  tiene <- tryCatch(db_query("
    SELECT 1 FROM information_schema.tables
    WHERE table_name = 'umbral_de_extremo' LIMIT 1"), error = function(e) NULL)
  if (is.null(tiene) || nrow(tiene) == 0) {
    return(charToRaw(jsonlite::toJSON(vacio, auto_unbox = TRUE)))
  }
  df <- tryCatch(db_query("
    SELECT canal, copy, pie, fecha_fin::text AS fecha_fin, familia,
           duracion, rango, t_empirico
    FROM umbral_de_extremo
    WHERE id_estacion = $1::int AND fecha_fin >= CURRENT_DATE - 14
    ORDER BY fecha_fin DESC, t_empirico DESC
    LIMIT 8", params = list(eid)), error = function(e) NULL)
  ant <- tryCatch(db_query("
    SELECT copy, pie, copy_cierre, nivel, fecha_fin::text AS fecha_fin, criterio
    FROM umbral_antes_del_dia
    WHERE unidad = 'estacion' AND id_estacion = $1::int AND sale
      AND nivel IN ('aviso', 'posibilidad')
      AND fecha_fin >= CURRENT_DATE - 14
    ORDER BY fecha_inicio
    LIMIT 8", params = list(eid)), error = function(e) NULL)
  filas <- if (is.null(df) || !nrow(df)) list() else lapply(seq_len(nrow(df)), function(i) {
    list(
      canal = as.character(df$canal[i]),
      copy = as.character(df$copy[i]),
      pie = as.character(df$pie[i]),
      fecha_fin = as.character(df$fecha_fin[i]),
      familia = as.character(df$familia[i])
    )
  })
  if (!is.null(ant) && nrow(ant)) {
    filas <- c(filas, lapply(seq_len(nrow(ant)), function(i) {
      cierre <- trimws(as.character(ant$copy_cierre[i] %||% ""))
      list(
        canal = "aviso",
        copy = if (nzchar(cierre)) paste(as.character(ant$copy[i]), cierre) else as.character(ant$copy[i]),
        pie = as.character(ant$pie[i]),
        fecha_fin = as.character(ant$fecha_fin[i]),
        familia = as.character(ant$criterio[i])
      )
    }))
  }
  if (!length(filas)) {
    return(charToRaw(jsonlite::toJSON(vacio, auto_unbox = TRUE)))
  }
  avisos <- Filter(function(x) identical(x$canal, "aviso"), filas)
  resumenes <- Filter(function(x) identical(x$canal, "resumen"), filas)
  charToRaw(jsonlite::toJSON(list(avisos = avisos, resumenes = resumenes), auto_unbox = TRUE, null = "null"))
}

#* Alertas tempranas activas (pronostico top 10) para una estacion
#* @param id ID de la estacion
#* @serializer contentType list(type="application/json")
#* @get /estaciones/<id>/alertas-temprana
function(id, req, res) {
  eid <- as.integer(id)
  if (is.na(eid)) {
    res$status <- 400
    return(charToRaw(jsonlite::toJSON(list(error = "id inv\u00e1lido"), auto_unbox = TRUE)))
  }
  out <- listar_alertas_temprana_estacion(eid)
  charToRaw(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null"))
}

#* Variables disponibles en una estacion con rango de fechas
#* @param id ID de la estacion
#* @get /estaciones/<id>/variables
function(id, req, res) {
  eid <- as.integer(id)
  if (is.na(eid)) { res$status <- 400; return(list(error = "id inv\u00e1lido")) }
  cache_or_compute(cache_key("estacion:variables", eid), function() {
    df <- db_query("
      SELECT v.id, v.nombre, v.nombre_abr, v.unidad,
        MIN(o.fecha) as fecha_min, MAX(o.fecha) as fecha_max,
        COUNT(*) as registros
      FROM magnitud v
      JOIN bitacora_oficial o ON o.id_variable = v.id AND o.id_estacion = $1
      GROUP BY v.id, v.nombre, v.nombre_abr, v.unidad", params = list(eid))
    if (nrow(df) > 0 && "nombre_abr" %in% names(df)) {
      df$unidad <- mapply(display_unidad, df$nombre_abr, df$unidad %||% "", USE.NAMES = FALSE)
    }
    df
  }, ttl = 1800, res = res)
}

#* Lista regiones unicas (cod_reg, nom_reg)
#* @param chile_only Si "1", solo estaciones en Chile continental
#* @get /regiones
function(chile_only = "1", req, res) {
  only <- tolower(chile_only) %in% c("1", "true", "yes", "si")
  cache_or_compute(cache_key("regiones", as.integer(only)), function() {
    q <- "SELECT DISTINCT cod_reg AS id, nom_reg AS nombre FROM mirador WHERE cod_reg IS NOT NULL AND nom_reg IS NOT NULL AND TRIM(nom_reg) != ''"
    if (only) q <- paste0(q, " AND lat BETWEEN -56 AND -17 AND lon BETWEEN -76 AND -66")
    q <- paste0(q, " ORDER BY nom_reg")
    db_query(q)
  }, ttl = 86400, res = res)
}

#* Lista comunas unicas (nom_com)
#* @param region Filtrar por cod_reg. Vacio = todas las comunas
#* @param chile_only Si "1", solo estaciones en Chile continental
#* @get /comunas
function(region = "", chile_only = "1", req, res) {
  only <- tolower(chile_only) %in% c("1", "true", "yes", "si")
  reg <- if (nzchar(region) && !is.na(as.integer(region))) as.integer(region) else NULL
  cache_or_compute(cache_key("comunas", as.integer(only), reg %||% "-"), function() {
    q <- "SELECT DISTINCT TRIM(nom_com) AS nombre FROM mirador WHERE nom_com IS NOT NULL AND TRIM(nom_com) != ''"
    params <- list()
    if (only) q <- paste0(q, " AND lat BETWEEN -56 AND -17 AND lon BETWEEN -76 AND -66")
    if (!is.null(reg)) {
      q <- paste0(q, " AND cod_reg = $1")
      params <- list(reg)
    }
    q <- paste0(q, " ORDER BY nombre")
    db_query(q, params = params)
  }, ttl = 86400, res = res)
}
