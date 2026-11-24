# DEPRECATED: usar ingest/lib/orchestrate_meteovision.R + ingest/lib/persist.R
# Referencia legacy MeteoVision. NO usar insertar_datos() en produccion.

insertar_datos <- function(connection, data, estaciones_descargadas, id_variable, fuente, fecha_ini, fecha_fin, nombre_tabla){
    data <- data %>% drop_na()
    data <- data %>% filter(!sapply(data[[5]], is.logical))
    data$fecha <- sprintf("%d-%02d-%02d", data$year, data$month, data$day)
    nombre_abr <- obtener_nombre_abr_variable(connection, id_variable)
    print(paste0("Consultando datos existentes en la base de datos para la variable ", nombre_abr, " fuente ", fuente, "..."))

    if (nombre_tabla == "observacion"){
        fechas_existentes <- consultar_mediciones_existentes_validado(connection, id_variable, estaciones_descargadas, fecha_ini, fecha_fin)
    }
    else if (nombre_tabla == "observacion_preliminar"){
        fechas_existentes <- consultar_mediciones_existentes_preliminar(connection, id_variable, estaciones_descargadas, fecha_ini, fecha_fin)
    }
    else if (nombre_tabla == "bitacora_oficial"){
        fechas_existentes <- consultar_mediciones_existentes_final(connection, id_variable, estaciones_descargadas, fecha_ini, fecha_fin)
    }
    else{
        print("Error: El nombre de la tabla SQL a la cual se intenta insertar datos no es válido.")
        return()
    }

    fechas_existentes$fecha <- as.Date(fechas_existentes$fecha)
    print(paste0(nrow(fechas_existentes), " de los ", nrow(data), " registros descargados ya existen en la base de datos para la variable ", nombre_abr, " fuente ", fuente))
    data$fecha <- as.Date(data$fecha)
    registros_a_insertar <- data %>% anti_join(fechas_existentes, by = c("fecha" = "fecha", "codigo_nacional" = "codigo_nacional"))

    if(nrow(registros_a_insertar) == 0) {
        return()
    }

    print(paste0("Insertando ", nrow(registros_a_insertar), " nuevos registros para la variable ", nombre_abr, " fuente ", fuente))

    id_estaciones <- registros_a_insertar %>%
      distinct(codigo_nacional) %>%
      mutate(id_estacion = obtener_id_estacion(connection, codigo_nacional, fuente))

    registros_a_insertar <- registros_a_insertar %>%
      left_join(id_estaciones, by = "codigo_nacional") %>%
      filter(!is.na(id_estacion)) %>%
      mutate(fecha = trimws(as.character(fecha)))

    colnames(registros_a_insertar) <- c("year", "month", "day", "codigo_nacional", "valor", "fecha", "id_estacion")
    registros_a_insertar <- registros_a_insertar %>% drop_na()
    if(nrow(registros_a_insertar) > 0) {
        registros_a_insertar$id_variable <- id_variable
        registros_a_insertar$fecha <- as.character(registros_a_insertar$fecha)
        registros_a_insertar$fecha_unix <- as.numeric(as.POSIXct(registros_a_insertar$fecha, format="%Y-%m-%d", tz="UTC"))
        registros_a_insertar <- registros_a_insertar[, c("valor", "fecha", "fecha_unix", "day", "month", "year", "id_variable", "id_estacion")]
        tryCatch({
            dbWriteTable(connection, nombre_tabla, registros_a_insertar, append = TRUE, row.names = FALSE)
            return(nrow(registros_a_insertar))
        }, error = function(e) {
            print(paste("Error al insertar registros:", e$message))
            return(0)
        })
    }
    else{
        print(paste0("No hay nuevos registros para insertar para la variable ", nombre_abr, " fuente ", fuente ))
    }
    print(paste0("Proceso de inserción SQL completado para la variable ", nombre_abr, " fuente ", fuente))
}

actualizar_datos <- function(connection, fuente, variable, fecha_ini, fecha_fin, modo = 'test'){
    stop("DEPRECATED: use meteovision_actualizar_variable() desde ingest/lib/orchestrate_meteovision.R")
}

consultar_mediciones_existentes_preliminar <- function(connection, id_variable, estaciones_descargadas, fecha_ini, fecha_fin){
    codigos_estaciones <- paste(sprintf("'%s'", estaciones_descargadas$codigo_nacional), collapse = ",")
    query <- sprintf(
    "SELECT observacion_preliminar.fecha, mirador.codigo_nacional, observacion_preliminar.valor, mirador.nombre
     FROM observacion_preliminar
     JOIN mirador ON observacion_preliminar.id_estacion = mirador.id
     WHERE observacion_preliminar.id_variable = %d
     AND mirador.codigo_nacional IN (%s)
     AND observacion_preliminar.fecha BETWEEN '%s' AND '%s'",
     id_variable, codigos_estaciones, fecha_ini, fecha_fin)
    dbGetQuery(connection, query)
}

consultar_mediciones_existentes_validado <- function(connection, id_variable, estaciones_descargadas, fecha_ini, fecha_fin){
    codigos_estaciones <- paste(sprintf("'%s'", estaciones_descargadas$codigo_nacional), collapse = ",")
    query <- sprintf(
    "SELECT observacion.fecha, mirador.codigo_nacional, observacion.valor, mirador.nombre
     FROM observacion
     JOIN mirador ON observacion.id_estacion = mirador.id
     WHERE observacion.id_variable = %d
     AND mirador.codigo_nacional IN (%s)
     AND observacion.fecha BETWEEN '%s' AND '%s'",
     id_variable, codigos_estaciones, fecha_ini, fecha_fin)
    dbGetQuery(connection, query)
}

consultar_mediciones_existentes_final <- function(connection, id_variable, estaciones_descargadas, fecha_ini, fecha_fin){
    codigos_estaciones <- paste(sprintf("'%s'", estaciones_descargadas$codigo_nacional), collapse = ",")
    query <- sprintf(
    "SELECT bitacora_oficial.fecha, mirador.codigo_nacional, bitacora_oficial.valor, mirador.nombre
     FROM bitacora_oficial
     JOIN mirador ON bitacora_oficial.id_estacion = mirador.id
     WHERE bitacora_oficial.id_variable = %d
     AND mirador.codigo_nacional IN (%s)
     AND bitacora_oficial.fecha BETWEEN '%s' AND '%s'",
     id_variable, codigos_estaciones, fecha_ini, fecha_fin)
    dbGetQuery(connection, query)
}
