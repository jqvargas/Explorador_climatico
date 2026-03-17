# ui_components.R - Componentes reutilizables del dashboard
# Estructura semantica con clases ec-* para que los temas las estilicen
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
    ),
    hr(class = "ec-separator"),
    descarga_ui("descarga")
  )
}

ec_dashboard_layout <- function(title = APP_TITLE) {
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = paste0("themes/", APP_THEME, ".css"))
    ),
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(
        title = shiny::tags$span(
          shiny::tags$span("rodaja", style = "color:#DF9531; font-weight:700;"),
          shiny::tags$span(".cl", style = "color:#CACEB8; font-weight:400;")
        )
      ),
      shinydashboard::dashboardSidebar(
        ec_sidebar_filters()
      ),
      shinydashboard::dashboardBody(
        fresh::use_theme(tema_rodaja),
        shiny::tags$head(
          shiny::tags$link(rel = "stylesheet", href = "themes/rodaja.css")
        ),
        shiny::div(
          class = "ec-main-content",
          shiny::div(class = "ec-map-container", mapa_ui("mapa")),
          shiny::conditionalPanel(
            condition = "output.datos_listos == '1'",
            shiny::div(
              class = "ec-datos-section",
              shiny::fluidRow(shinydashboard::box(width = 12, grafico_ui("grafico"), title = "Grafico")),
              shiny::fluidRow(shinydashboard::box(width = 12, tabla_ui("tabla"), title = "Tabla"))
            )
          )
        )
      )
    )
  )
}

