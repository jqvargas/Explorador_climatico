# ui_components.R - Layout fullscreen con paneles flotantes
# Usa ec_sidebar_filters (misma estructura que funcionaba) como overlay
# NO cambiar inputId - el server.R depende de ellos

ec_filter_block <- function(select_id, placeholder, choices, button_id, selected = NULL) {
  div(class = "ec-filter-block",
    div(class = "ec-filter-row",
      div(class = "ec-filter-select",
        shiny::selectInput(select_id, NULL,
          choices = choices,
          selected = selected,
          width = "100%", selectize = FALSE)
      ),
      shiny::actionButton(button_id, "", icon = shiny::icon("check"),
        class = "ec-btn-apply", title = "Aplicar")
    )
  )
}

ec_sidebar_filters <- function() {
  div(class = "ec-sidebar-filters",
    ec_filter_block("operador", NULL, c("Filtrar por operador" = "0"), "aplicar_operador", selected = "0"),
    ec_filter_block("variable", NULL, c("Filtrar por variable" = ""), "aplicar_variable", selected = ""),
    ec_filter_block("estacion", NULL, c("Filtrar por estación" = ""), "aplicar_estacion", selected = ""),
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
    windowTitle = "rodaja.cl",
    collapsible = TRUE,

    shiny::tabPanel("Explorador",
      shiny::div(class = "outer",
        shiny::tags$head(
          shiny::tags$link(rel = "stylesheet", type = "text/css", href = "themes/rodaja.css"),
          shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css", crossorigin = "anonymous"),
          shiny::tags$script(src = "resize-panel.js")
        ),

        leaflet::leafletOutput("mapa-mapa", width = "100%", height = "100%"),

        shiny::tags$script(HTML("
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
      ),

      HTML('<div id="ec_edge_bar" class="ec-edge-bar">
  <button type="button" class="ec-edge-btn" id="ec_btn_filtros" title="Filtros">
    <i class="fa fa-filter"></i>
  </button>
</div>'),
      shiny::div(id = "ec_drawer_overlay", class = "ec-drawer-overlay"),
      shiny::div(id = "ec_drawer_filtros", class = "ec-drawer",
        shiny::div(class = "ec-drawer-header",
          shiny::span("Filtros", class = "ec-drawer-title")
        ),
        shiny::div(class = "ec-drawer-body",
          ec_sidebar_filters()
        )
      ),
      shiny::tags$script(HTML('
        (function(){
          function toggle(){
            document.getElementById("ec_drawer_filtros").classList.toggle("ec-drawer-open");
            document.getElementById("ec_drawer_overlay").classList.toggle("ec-drawer-open");
          }
          function close(){
            document.getElementById("ec_drawer_filtros").classList.remove("ec-drawer-open");
            document.getElementById("ec_drawer_overlay").classList.remove("ec-drawer-open");
          }
          function attach(){
            var btn=document.getElementById("ec_btn_filtros");
            var cls=document.getElementById("ec_drawer_close");
            var ovr=document.getElementById("ec_drawer_overlay");
            if(btn){
              btn.onclick=toggle;
              btn.addEventListener("touchstart",function(e){e.preventDefault();toggle();},{passive:false});
            }
            if(cls) cls.onclick=close;
            if(ovr) ovr.onclick=close;
          }
          attach();
          setTimeout(attach, 500);
          setTimeout(attach, 2000);
          document.addEventListener("shiny:connected",function(){
            attach();
            Shiny.addCustomMessageHandler("drawer_close",function(_){close();});
          });
        })();
      ')),
      shiny::tags$style(HTML('
  #ec_drawer_filtros,
  #ec_drawer_filtros .ec-drawer-header,
  #ec_drawer_filtros .ec-drawer-body,
  #ec_drawer_filtros .ec-sidebar-filters,
  #ec_drawer_filtros .ec-filter-block,
  #ec_drawer_filtros .form-group,
  #ec_drawer_filtros .shiny-input-container,
  #ec_drawer_filtros .selectize-control,
  #ec_drawer_filtros .selectize-control .form-control {
    background: transparent !important;
    background-color: transparent !important;
    border: none !important;
    box-shadow: none !important;
  }
  #ec_drawer_filtros .selectize-input {
    background: rgba(255,255,255,0.10) !important;
    background-color: rgba(255,255,255,0.10) !important;
    border: 1px solid rgba(255,255,255,0.35) !important;
    box-shadow: none !important;
    color: #F7F9FA !important;
    font-weight: bold !important;
  }
  #ec_drawer_filtros .selectize-input .item {
    color: #F7F9FA !important;
    font-weight: 600 !important;
    opacity: 1 !important;
  }
  #ec_drawer_filtros .selectize-input .placeholder,
  #ec_drawer_filtros .selectize-input input,
  #ec_drawer_filtros .selectize-control .placeholder {
    color: #F7F9FA !important;
    font-weight: 600 !important;
    opacity: 1 !important;
  }
  #ec_drawer_filtros select.form-control {
    background: rgba(255,255,255,0.10) !important;
    border: 1px solid rgba(255,255,255,0.35) !important;
    color: #F7F9FA !important;
    font-weight: 600 !important;
  }
  /* Dropdown selectInput - fondo oscuro con texto claro */
  #ec_drawer_filtros .selectize-dropdown,
  #ec_drawer_filtros select option,
  #ec_drawer_filtros .bootstrap-select .dropdown-menu {
    background: #1e232a !important;
    background-color: #1e232a !important;
    border: 1px solid rgba(255,255,255,0.2) !important;
    border-radius: 6px !important;
    color: #F7F9FA !important;
  }
  #ec_drawer_filtros select {
    background: rgba(255,255,255,0.10) !important;
    color: #F7F9FA !important;
    border: 1px solid rgba(255,255,255,0.35) !important;
    border-radius: 6px !important;
  }
  #ec_drawer_filtros select option {
    background: #1e232a !important;
    color: #F7F9FA !important;
    padding: 8px !important;
  }
  #ec_drawer_filtros select option:hover,
  #ec_drawer_filtros select option:checked {
    background: #DF9531 !important;
    color: #fff !important;
  }
'))
    ),

    shiny::tabPanel("Acerca de",
      shiny::fluidRow(shiny::column(8, offset = 2,
        shiny::br(),
        shiny::h3("Explorador Climatico - rodaja.cl"),
        shiny::p("Plataforma de visualizacion y descarga de datos climaticos de Chile."))))
  )
}

