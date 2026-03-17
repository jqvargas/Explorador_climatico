source("modules/mapa.R", local = TRUE)
source("modules/grafico.R", local = TRUE)
source("modules/tabla.R", local = TRUE)
source("modules/descarga.R", local = TRUE)

header_tag <- shiny::tags$div(
  class = "d-flex align-items-center justify-content-between w-100 px-3 py-2",
  shiny::tags$a(
    href = "/",
    class = "navbar-brand d-flex align-items-center",
    shiny::img(src = "logo.svg", height = "32", alt = "rodaja.cl", onerror = "this.style.display='none'; this.nextElementSibling.style.display='inline';"),
    shiny::tags$span("rodaja.cl", style = "font-weight: 600; color: #1D9E75;")
  ),
  shiny::tags$nav(
    class = "nav",
    shiny::tags$a(class = "nav-link", href = "/", "Inicio"),
    shiny::tags$a(class = "nav-link", href = "/explorador", "Explorador"),
    shiny::tags$a(class = "nav-link", href = "/docs", "Documentación")
  ),
  shiny::actionButton(
    "login_btn",
    "Iniciar sesión",
    class = "btn btn-outline-primary btn-sm"
  )
)

ui <- bslib::page_sidebar(
  title = header_tag,
  theme = bslib::bs_theme(bootswatch = "flatly", primary = "#1D9E75"),
  sidebar = bslib::sidebar(
    width = 300,
    open = "open",
    shiny::selectInput("variable", "Variable", choices = NULL, width = "100%"),
    shiny::dateRangeInput(
      "fechas",
      "Rango de fechas",
      start = Sys.Date() - 365,
      end = Sys.Date(),
      language = "es",
      separator = " a ",
      width = "100%"
    ),
    shiny::selectInput(
      "tipo_dato",
      "Tipo de dato",
      choices = c("Observado" = "observado", "Modelo" = "modelo"),
      width = "100%"
    ),
    shiny::actionButton("ver_datos", "Ver datos", class = "btn-primary", width = "100%"),
    shiny::hr(),
    descarga_ui("descarga")
  ),
  fillable = TRUE,
  shiny::layout_columns(
    fill = TRUE,
    row_heights = c(400, 350, 1),
    shiny::card(
      full_screen = TRUE,
      card_header("Mapa de estaciones"),
      mapa_ui("mapa")
    ),
    shiny::card(
      full_screen = TRUE,
      card_header("Serie temporal"),
      grafico_ui("grafico")
    ),
    shiny::card(
      full_screen = TRUE,
      card_header("Vista previa de datos"),
      tabla_ui("tabla")
    )
  )
)
