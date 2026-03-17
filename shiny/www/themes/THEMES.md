# Guia para rediseñar el dashboard

## Cambiar de tema
1. Edita config.R: APP_THEME <- "nombre_tema"
2. Crea www/themes/nombre_tema.css

## Crear tema nuevo
1. Copia default.css a mi_tema.css
2. Ajusta variables en :root o redefine clases ec-*
3. config.R: APP_THEME <- "mi_tema"

## Clases: ec-sidebar-filters, ec-filter-block, ec-btn-apply, ec-btn-ver-datos, ec-separator
## NO modificar: inputId (operador, variable, estacion, aplicar_*, ver_datos)