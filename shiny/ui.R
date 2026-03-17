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
      shiny::actionButton("aplicar_operador", "Aplicar", width = "100%"),
      shiny::selectInput("variable", "Variable climatica", choices = NULL),
      shiny::actionButton("aplicar_variable", "Aplicar", width = "100%"),
      shiny::selectInput("estacion", "Estacion", choices = c("Selecciona estacion" = ""), selected = ""),
      shiny::actionButton("aplicar_estacion", "Aplicar", width = "100%"),
      shiny::actionButton("ver_datos", "Ver datos", icon = icon("play"), class = "btn-primary", width = "100%"),
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
          p("1. Cada filtro (Operador, Variable, Estacion) tiene su boton Aplicar: el mapa solo se actualiza al hacer clic."),
          p("2. Por defecto todos estan en Todas. Cambia un filtro y haz clic en Aplicar para actualizar el mapa."),
          p("3. Ver datos usa las selecciones actuales (sin necesidad de Aplicar) para mostrar grafico y tabla."),
          p("4. La descarga respeta los limites configurados.")
        )
      ))
    )
  )
)