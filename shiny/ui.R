library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Explorador Climatico"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filtros", tabName = "filtros", icon = icon("filter")),
      menuItem("Mapa", tabName = "mapa", icon = icon("map")),
      menuItem("Grafico", tabName = "grafico", icon = icon("chart-line")),
      menuItem("Tabla", tabName = "tabla", icon = icon("table")),
      menuItem("Descargar", tabName = "descarga", icon = icon("download"))
    ),
    hr(),
    div(
      class = "sidebar-menu-content",
      shiny::selectInput("operador", "Operador / Fuente", choices = NULL),
      shiny::selectInput("variable", "Variable climatica", choices = NULL),
      shiny::selectInput("estacion", "Estacion", choices = c("Selecciona una estacion..." = ""), selected = ""),
      shiny::actionButton("ver_datos", "Ver datos", icon = icon("play"), class = "btn-primary"),
      hr(),
      descarga_ui("descarga")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("mapa", fluidRow(box(width = 12, mapa_ui("mapa")))),
      tabItem("grafico", fluidRow(box(width = 12, grafico_ui("grafico")))),
      tabItem("tabla", fluidRow(box(width = 12, tabla_ui("tabla")))),
      tabItem("descarga", fluidRow(box(width = 12, status = "info", p("El formulario de descarga esta en el menu lateral izquierdo.")))),
      tabItem("filtros", fluidRow(
        box(width = 12, status = "info",
          h4("Instrucciones"),
          p("1. Selecciona operador (DGA, DMC, etc.), variable y estacion."),
          p("2. Haz clic en Ver datos."),
          p("3. Navega a Grafico o Tabla para ver los resultados."),
          p("Al seleccionar estacion se muestra toda la serie. Usa la barra del grafico para filtrar. La descarga tiene limites.")
        )
      ))
    )
  )
)


