# ui.R - Carga config + componentes y construye el dashboard
# La logica esta en server.R; la estructura en ui_components.R; el tema en www/themes/
library(shiny)
library(shinydashboard)
ui <- ec_dashboard_layout()