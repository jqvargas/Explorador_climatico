# ui_components.R - Layout fullscreen con paneles flotantes
# Usa ec_sidebar_filters (misma estructura que funcionaba) como overlay
# NO cambiar inputId - el server.R depende de ellos

ec_filter_block <- function(select_id, label, choices, button_id, selected = NULL) {
  div(class = "ec-filter-block",
    shiny::selectInput(select_id, label, choices = choices, selected = selected),
    shiny::actionButton(button_id, "Aplicar", class = "ec-btn-apply")
  )
}

ec_sidebar_filters <- function() {
  div(class = "ec-sidebar-filters",
    ec_filter_block("operador", "Operador / Fuente", NULL, "aplicar_operador"),
    ec_filter_block("variable", "Variable climatica", NULL, "aplicar_variable"),
    ec_filter_block("estacion", "Estacion", c("Selecciona estacion" = ""), "aplicar_estacion", selected = ""),
    div(class = "ec-filter-block",
      shiny::actionButton("ver_datos", "Ver datos", icon = shiny::icon("play"), class = "btn-primary ec-btn-ver-datos")
    )
  )
}

ec_dashboard_layout <- function(title = APP_TITLE) {
  shiny::navbarPage(
    title = shiny::tags$span(
      shiny::tags$span("rodaja", style = "color:#DF9531;font-weight:700;"),
      shiny::tags$span(".cl", style = "color:#8C9091;font-weight:400;")
    ),
    theme = shinythemes::shinytheme("flatly"),
    windowTitle = "rodaja.cl",
    collapsible = TRUE,

    shiny::tabPanel("Explorador",
      shiny::div(class = "outer",
        shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "themes/rodaja.css")),

        leaflet::leafletOutput("mapa-mapa", width = "100%", height = "100%"),

        shiny::absolutePanel(
          id = "panel_filtros",
          fixed = TRUE, draggable = TRUE,
          top = 55, left = 20,
          width = 280,
          style = "background:rgba(255,255,255,0.95);padding:16px;border-radius:8px;box-shadow:0 4px 16px rgba(0,0,0,0.12);overflow-y:auto;max-height:calc(100vh - 70px);",
          ec_sidebar_filters()
        ),

        shiny::absolutePanel(
          id = "panel_datos",
          fixed = TRUE, draggable = TRUE,
          top = 55, right = 0,
          left = "auto", bottom = "auto",
          width = "30%", height = "auto",
          style = "background:rgba(255,255,255,0.95);padding:16px;border-radius:8px 0 0 8px;box-shadow:-2px 0 12px rgba(0,0,0,0.08);overflow-y:auto;max-height:calc(100vh - 60px);",
          shiny::h5("Serie temporal", style = "font-weight:600;margin-top:0;color:#394553;"),
          grafico_ui("grafico"),
          shiny::tags$hr(style = "border-color:#e0e0e0;margin:12px 0;"),
          shiny::h5("Datos", style = "font-weight:600;color:#394553;"),
          tabla_ui("tabla"),
          shiny::tags$hr(style = "border-color:#e0e0e0;margin:12px 0;"),
          descarga_ui("descarga")
        ),

        shiny::tags$div(id = "cite",
          style = "position:absolute;bottom:6px;right:50px;font-size:11px;color:#8C9091;",
          "rodaja.cl - datos climaticos de Chile")
      )
    ),

    shiny::tabPanel("Acerca de",
      shiny::fluidRow(shiny::column(8, offset = 2,
        shiny::br(),
        shiny::h3("Explorador Climatico - rodaja.cl"),
        shiny::p("Plataforma de visualizacion y descarga de datos climaticos de Chile."))))
  )
}