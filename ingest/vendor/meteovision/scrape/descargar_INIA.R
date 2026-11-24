##Version nueva
library(rvest)
library(httr)
library(readtext)
library(dplyr)
library(lubridate)



obtener_codigos_nacionales <- function(){
  base_url <- "https://agrometeorologia.cl"
  webpage <- read_html(base_url)  

  option_nodes <- html_nodes(webpage, "option")
  #guardar un vector solo con los codigos nacionales de cada estacion.
  form_options <- unname(unlist(html_attrs(option_nodes)))
  codigos_nacionales <- form_options[grep("INIA|EXT", form_options)]
  return(codigos_nacionales)
}
obtener_variables <- function(){
  base_url <- "https://agrometeorologia.cl"
  webpage <- read_html(base_url)  

  # Guardar el vector de nodos HTML de tipo option
  #Ejemplo: <option value="INIA-302"> Pampa Concordia, Arica, INIA</option>\n
  option_nodes <- html_nodes(webpage, "option")
  #guardar un vector solo con los codigos nacionales de cada estacion.
  form_options <- unname(unlist(html_attrs(option_nodes)))
  #obtener variables disponibles desde opciones de formulario
  variables_disponibles <- form_options[grep("MAX|MIN|AVG|SUM", form_options)]
}

descargar_INIA <- function(fecha_ini, fecha_fin, intervalo_fecha, variable, codigo_nacional){
  # Parámetros de tiempo
  #fecha_ini <- "01-01-2000"
  #fecha_fin <- "31-12-2024"
  #intervalo_fecha <- "day" -> puede ser "hour", "day", "month", "year"
  base_url <- "https://agrometeorologia.cl"
  # Preparar datos para la solicitud
  request_body <- list(
    'estaciones[]' = codigo_nacional,
    'variables[]' = variable,
    'intervalo' = intervalo_fecha,
    'desde' = fecha_ini,
    'hasta' = fecha_fin,
    'month_desde' = "",
    'month_hasta' = "",
    'yearMonth_desde' = "",
    'yearMonth_hasta' = "",
    'year_desde' = "",
    'year_hasta' = "",
    'vista[]' = "tabla"
  )

  # Realizar solicitud POST
  response <- POST(
    base_url,
    body = request_body,
    encode = "form")


  # Verificar respuesta
  print(paste("Status code:", status_code(response)))

  # Procesar respuesta
  response_html <- read_html(content(response, "text"))

  # Buscar tablas en la respuesta
  tables <- html_nodes(response_html, "table")
  print(paste("Número de tablas encontradas:", length(tables)))

  # Extraer datos de la tabla
  table_data <- response_html %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  #seleccionar columnas utiles, renombrar columnas, eliminar filas con valores "s/i", convertir a numericos y a fecha
  if(length(table_data) > 0) {
    df <- table_data[[1]][, 1:2] %>%
      setNames(c("fecha", "valor")) %>%
      mutate(
        valor = na_if(valor, "s/i"),
        valor = as.numeric(valor),
        fecha = as.Date(fecha, format = "%d-%m-%Y"),
        codigo_nacional = codigo_nacional,
        year = year(fecha),
        month = month(fecha),
        day = day(fecha)
      ) %>%
      filter(!is.na(valor)) %>% select(year, month, day, codigo_nacional, valor)

    names(df)[names(df) == "valor"] <- variable

    return(df)
  } else {
    print("No se encontraron tablas en la respuesta")
    return(NULL)
  }
}  



