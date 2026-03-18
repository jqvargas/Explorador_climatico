# ui_components.R - Layout fullscreen con paneles flotantes
# Usa ec_sidebar_filters (misma estructura que funcionaba) como overlay
# NO cambiar inputId - el server.R depende de ellos

ec_filter_block <- function(select_id, label, choices, button_id, selected = NULL) {
  div(class = "ec-filter-block",
    shiny::tags$label(label, class = "ec-filter-label"),
    div(class = "ec-filter-row",
      div(class = "ec-filter-select", shiny::selectInput(select_id, NULL, choices = choices, selected = selected)),
      shiny::actionButton(button_id, "", icon = shiny::icon("check"), class = "ec-btn-apply", title = "Aplicar")
    )
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
        shiny::tags$head(
          shiny::tags$link(rel = "stylesheet", type = "text/css", href = "themes/rodaja.css"),
          shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css", crossorigin = "anonymous"),
          shiny::tags$script(src = "resize-panel.js"), shiny::tags$script(src = "panel-filtros-toggle.js")
        ),

        leaflet::leafletOutput("mapa-mapa", width = "100%", height = "100%"),

        shiny::absolutePanel(
          id = "panel_filtros",
          fixed = TRUE, draggable = TRUE,
          top = 55, left = 20,
          width = 280,
          class = "ec-panel-filtros",
          style = "background: rgba(255,255,255,0.85); backdrop-filter: blur(12px); -webkit-backdrop-filter: blur(12px); border-radius: 16px; border: 1px solid rgba(255,255,255,0.6); box-shadow: 0 8px 32px rgba(0,0,0,0.12); padding: 0; overflow: visible; max-height: calc(100vh - 70px);",
          shiny::div(class = "ec-panel-filtros-header",
            shiny::span("Filtros", class = "ec-panel-filtros-title"),
            shiny::actionButton("toggle_filtros", NULL, icon = shiny::icon("angle-double-left"), class = "ec-btn-toggle-filtros", title = "Minimizar")
          ),
          shiny::div(class = "ec-panel-filtros-body",
            ec_sidebar_filters()
          )
        ),

        shiny::tags$script(HTML("
          Shiny.addCustomMessageHandler('panel_filtros_toggle', function(_) {
            var p = document.getElementById('panel_filtros');
            if (!p) return;
            p.classList.toggle('ec-collapsed');
            var btn = document.querySelector('#toggle_filtros');
            if (btn) {
              var icon = btn.querySelector('i');
              if (p.classList.contains('ec-collapsed')) {
                btn.title = 'Expandir';
                if (icon) icon.className = 'fa fa-angle-double-right';
              } else {
                btn.title = 'Minimizar';
                if (icon) icon.className = 'fa fa-angle-double-left';
              }
            }
          });
          Shiny.addCustomMessageHandler('panel_datos_state', function(val) {
            var p = document.getElementById('panel_datos_wrapper');
            if (!p) return;
            p.setAttribute('data-state', val);
            p.classList.remove('ec-panel-closed','ec-panel-minimized','ec-panel-maximized');
            if (val === 0) p.classList.add('ec-panel-closed');
            else if (val === 2) p.classList.add('ec-panel-minimized');
            else if (val === 3) p.classList.add('ec-panel-maximized');
            if (typeof Shiny !== 'undefined' && Shiny.setInputValue) Shiny.setInputValue('panel_datos_state', val, {priority: 'event'});
          });
        ")),
        shiny::div(style = "display:none;", shiny::numericInput("panel_datos_state", NULL, value = 0, min = 0, max = 3, step = 1)),
        shiny::div(
          id = "panel_datos_wrapper",
          class = "ec-panel-datos ec-panel-closed",
          "data-state" = "0",
          shiny::absolutePanel(
            id = "panel_datos",
            fixed = TRUE, draggable = TRUE,
            left = 0, right = 0, bottom = 0, top = "auto",
            width = "100%", height = "auto",
            class = "ec-panel-datos-inner",
            style = "background:rgba(255,255,255,0.95);padding:0;border-radius:8px 8px 0 0;box-shadow:0 -2px 12px rgba(0,0,0,0.08);overflow:visible;display:flex;flex-direction:column;min-height:220px;max-height:85vh;height:320px;",
            shiny::div(class = "ec-panel-top-bar",
              shiny::div(class = "ec-panel-resize-handle", title = "Arrastra para cambiar tamano"),
              shiny::div(class = "ec-panel-datos-btns",
                shiny::actionButton("panel_minimize", NULL, class = "ec-panel-btn ec-panel-btn-minimize", title = "Minimizar"),
                shiny::actionButton("panel_maximize", NULL, class = "ec-panel-btn ec-panel-btn-maximize", title = "Maximizar"),
                shiny::actionButton("panel_close", NULL, class = "ec-panel-btn ec-panel-btn-close", title = "Cerrar")
              )
            ),
            shiny::div(class = "ec-panel-datos-body",
              style = "padding:8px;overflow:visible;flex:1 1 auto;min-height:0;",
              grafico_ui("grafico")
            )
          )
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

