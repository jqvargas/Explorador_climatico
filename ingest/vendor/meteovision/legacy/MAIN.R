#Autor: Raimundo Rivera Figueroa
#Inicio proyecto BDD: Agosto 2024
#Actualización de la base de datos hidroclimática diaria

#Dirigir prints de descarga a un archivo .txt, solo para hacer seguimiento.
log_file <- "backend/scripts/Descargar/logs_descarga/program_log_04-02-2026.txt"
sink(log_file, append = TRUE)  # Redirect standard output

# Load required scripts
source("backend/scripts/funciones/queries.R")
source("backend/scripts/funciones/funciones_utilidad.R")
source("backend/scripts/funciones/connection.R")
source("backend/scripts/Descargar/descargar_DGA.R")
source("backend/scripts/Descargar/descargar_DMC.R")
source("backend/scripts/actualizar_bbdd.R")
#al acoplar scripts, eliminar librerias
source("backend/config/librerias.R")
########################################

# Main script logic
connection <- connect_to_db("backend/config/.env")

fecha_ini <- as.Date("2025-01-01")
fecha_fin <- as.Date("2025-12-31")

#Al llamar a u_mean, u_min o u_max se actualizan los tres, dado que la DGA proporciona la información para las 3 variables al mismo tiempo
#actualizar_datos(connection, "dmc", "hr_mean", fecha_ini, fecha_fin, modo = "completo")
#actualizar_datos(connection, "dga", "u2_mean", fecha_ini, fecha_fin, modo = "completo")
#actualizar_datos(connection, "dga", "pp", fecha_ini, fecha_fin, modo = "completo")
#actualizar_datos(connection, "dga", "q", fecha_ini, fecha_fin, modo = "completo")
#Al llamar t_max, t_min o t_mean, se actualizan los tres, dado que la DGA proporciona la información para las 3 variables al mismo tiempo
actualizar_datos(connection, "dga", "t_min", fecha_ini, fecha_fin, modo = "completo")
#actualizar_datos(connection, "dmc", "pp", fecha_ini, fecha_fin, modo = "completo")
#actualizar_datos(connection, "dmc", "t_max", fecha_ini, fecha_fin, modo = "completo")
#actualizar_datos(connection, "dmc", "t_min", fecha_ini, fecha_fin, modo = "completo")
#actualizar_datos(connection, "dmc", "t_mean", fecha_ini, fecha_fin, modo = "completo")
#actualizar_datos(connection, "dmc", "hr_mean", fecha_ini, fecha_fin)
#actualizar_datos(connection, "dmc", "rs_mean", fecha_ini, fecha_fin)


# Stop redirecting output
sink()  # Reset standard output
sink(type = "message")  # Reset messages



#dbExecute(connection, "CREATE ROLE colega_lectura WITH LOGIN PASSWORD '34hjk0pQS7EB6691i'")
#dbExecute(connection, "GRANT CONNECT ON DATABASE hidroclimatica_diaria_v2 TO colega_lectura;" )
#dbExecute(connection, "GRANT USAGE ON SCHEMA public TO colega_lectura;")
#dbExecute(connection, "GRANT SELECT ON ALL TABLES IN SCHEMA public TO colega_lectura;")
#dbExecute(connection, "ALTER DEFAULT PRIVILEGES IN SCHEMA public
#GRANT SELECT ON TABLES TO colega_lectura;")
#dbExecute(connection, "REVOKE INSERT, UPDATE, DELETE, TRUNCATE ON ALL TABLES IN SCHEMA public FROM colega_lectura;")
#Indices necesarios para acelerar consultas
#dbExecute(connection, "CREATE INDEX IF NOT EXISTS idx_obs_id_estacion ON observacion(id_estacion);")
#dbExecute(connection, "CREATE INDEX IF NOT EXISTS idx_obs_id_variable ON observacion(id_variable);")
#
## Índices tabla observacion_preliminar
#dbExecute(connection, "CREATE INDEX IF NOT EXISTS idx_obs_pre_id_estacion ON observacion_preliminar(id_estacion);")
#dbExecute(connection, "CREATE INDEX IF NOT EXISTS idx_obs_pre_id_variable ON observacion_preliminar(id_variable);")

#estaciones <- obtener_estaciones_con_datos(connection)
#
#obtener_estaciones_con_datos <- function(conexion) {
#  consulta = "select * 
#    FROM mirador; 
#    "
#  estaciones <- dbGetQuery(conexion, consulta)
#
#  estaciones <- estaciones %>% filter(!grepl("Estacion desconocida", nombre))
#  return(estaciones)
#}
#
#write.csv(estaciones, "metadatos_actualizados.csv")
#View(estaciones)
#
#estaciones <- estaciones %>% filter(id_fuente == 3)
#
#estaciones_calidad <- calcular_metricas_calidad_multi(connection, c("t_min", "t_max", "pp", "q"), 1990, 2025)
#write.csv(estaciones_calidad, "calidad_variables_estaciones_1990_2025.csv", row.names = FALSE)
#estaciones_contar <- contar_observaciones_multi(connection, c("t_min", "t_max", "pp", "q"), 1990, 2025)
#write.csv(estaciones_contar, "conteo_observaciones_variables_estaciones_1990_2025.csv", row.names = FALSE)
#
#estaciones_sin_datos <- estaciones_contar %>%
#  filter(n_obs_pp == 0 & n_obs_t_max == 0 & n_obs_t_min == 0 & n_obs_q == 0)        
#
#
#estaciones_caudal <- obtener_estaciones_con_datos_caudal(connection)
#txt_reg_natural <- read.table("codigos_cuencas_reg_natural.txt")
#
#
#estaciones_caudal$codigo_bna <- substr(estaciones_caudal$codigo_nacional, 1, 8)
#txt_reg_natural$V1 <- ifelse(nchar(txt_reg_natural$V1) == 7, paste0("0", txt_reg_natural$V1), txt_reg_natural$V1)
#
#estaciones_todas <- dbGetQuery(connection, "SELECT * FROM mirador WHERE id_fuente = 1;")
#estaciones_todas$codigo_bna <- substr(estaciones_todas$codigo_nacional, 1, 8)
#
#estaciones_caudal_filtro <- estaciones_todas %>% filter(codigo_bna %in% txt_reg_natural$V1)
#estaciones_caudal_filtro <- estaciones_caudal_filtro %>% select(-tipo, -id_fuente)
#write.csv(estaciones_caudal_filtro, "consultas/edu_DGA_res/metadatos_estaciones_caudal_reg_natural.csv", row.names = FALSE)
##4400001 - Estero Culebron En El Sifon (Lat. -29.98, Lon. -71.29)
#datos_caudal <- obtener_observaciones_por_variable_codigo(connection, "q", "bitacora_oficial", 1981, 2024, estaciones_caudal_filtro$codigo_nacional)
#
#datos_caudal_ancho <- preparar_datos_ancho(datos_caudal, "q")
#write.csv(datos_caudal_ancho, "consultas/edu_DGA_res/datos_caudal_estaciones_reg_natural_1981_2024.csv", row.names = FALSE)
#summary(datos_caudal_ancho)
#
##ALTER table mirador where codigo_nacional = 04400001-6, insert name = 'Estero Culebron En El Sifon', lat = -29.98, lon = -71.29;
#
##dbExecute(connection, "UPDATE mirador SET nombre = 'Estero Culebron En El Sifon', lat = -29.98, lon = -71.29 WHERE codigo_nacional = '04400001-6';")
#
#
#Cuenca 9101002 - Rio Puren En Puren (Lat. -38.01, Lon. -73.04) 09101002-K
#dbExecute(connection, "UPDATE mirador SET nombre = 'Rio Puren En Puren', lat = -38.01, lon = -73.04 WHERE codigo_nacional = '09101002-K';")
