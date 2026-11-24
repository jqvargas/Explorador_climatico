# ui_components.R - Layout fullscreen con paneles flotantes
# Usa ec_sidebar_filters (misma estructura que funcionaba) como overlay
# NO cambiar inputId - el server.R depende de ellos

ec_filter_select <- function(select_id, choices, selected = NULL) {
  div(class = "ec-filter-block ec-filter-select-block",
    shiny::selectizeInput(select_id, NULL,
      choices = choices,
      selected = selected,
      options = list(
        create = FALSE,
        searchField = "label",
        allowEmptyOption = TRUE,
        selectOnTab = TRUE,
        openOnFocus = TRUE,
        dropdownParent = "body"
      ))
  )
}

ec_sidebar_filters <- function() {
  div(class = "ec-sidebar-filters",
    shiny::tags$div(
      id = "ec_filter_principal",
      class = "ec-filter-principal",
      ec_filter_select("variable", c("Variable" = ""), selected = "")
    ),
    # Estacion solo visible con comuna seleccionada (mapa / breadcrumb).
    shiny::tags$div(
      id = "ec_filter_estacion_wrap",
      class = "ec-filter-estacion-wrap",
      style = "display:none;",
      ec_filter_select("estacion", c("Estacion" = ""), selected = "")
    ),
    shiny::div(class = "ec-contador-estaciones", shiny::textOutput("contador_estaciones")),
    div(class = "ec-filter-block ec-grupo-drawer-actions",
      shiny::actionButton("agregar_visibles_al_grupo", "Agregar visibles al grupo",
        icon = shiny::icon("object-group"), class = "btn ec-btn-drawer-secondary"),
      shiny::tags$button(type = "button", id = "ec-btn-seleccion-rectangulo",
        class = "btn ec-btn-drawer-secondary",
        shiny::icon("square-o"), " Seleccionar \u00e1rea")
    ),
    div(class = "ec-filter-block ec-drawer-primary-actions",
      id = "ec_tour_drawer_actions",
      shiny::actionButton("ver_datos", "Ver datos", icon = shiny::icon("play"),
        class = "btn btn-primary ec-btn-ver-datos"),
      shiny::actionButton("descargar_datos", "Descargar datos", icon = shiny::icon("download"),
        class = "btn btn-primary ec-btn-descargar-datos")
    ),
    div(class = "ec-filter-block",
      shiny::actionButton("limpiar_todo", "Limpiar todo", icon = shiny::icon("eraser"),
        class = "btn ec-btn-drawer-secondary ec-btn-limpiar-todo")
    ),
    # Operador y filtros tecnicos: colapsado por defecto (default = todos los operadores).
    shiny::tags$details(
      class = "ec-filter-advanced",
      shiny::tags$summary("Opciones avanzadas"),
      ec_filter_select("operador", c("Operador" = "0"), selected = "0")
    )
  )
}

ec_navbar_favorites <- function() {
  shiny::tags$li(
    class = "ec-nav-fav-item",
    shiny::tags$div(
      class = "btn-group ec-nav-fav-group",
      shiny::tags$button(
        type = "button",
        id = "ec-btn-mi-estacion",
        class = "btn btn-default ec-btn-mi-estacion",
        title = "Mi estaci\u00f3n favorita",
        onclick = "return ecNavIrMiEstacion();",
        shiny::icon("star"),
        shiny::tags$span(class = "ec-btn-mi-estacion-label", " Mi estaci\u00f3n")
      )
    )
  )
}

ec_app_icon <- function(name) {
  paths <- list(
    inicio = '<path d="M4 11 L12 4 l8 7 v9 a1 1 0 0 1-1 1h-5v-6H10v6H5a1 1 0 0 1-1-1z"/>',
    reporte = '<path d="M7 3.5h7.5L20 9v11.5H7z"/><path d="M14.5 3.5V9H20"/><path d="M10 13h7M10 17h5"/>',
    alertas = '<path d="M6 9a6 6 0 0 1 12 0c0 4 1.5 5.5 1.5 5.5H4.5S6 13 6 9z"/><path d="M10 19.5a2 2 0 0 0 4 0"/>',
    descargar = '<path d="M12 4v10"/><path d="M8 12l4 5 4-5"/><path d="M5 20.5h14"/>',
    cuenta = '<circle cx="12" cy="8" r="3.25"/><path d="M5 20c1.2-3.4 3.8-5 7-5s5.8 1.6 7 5"/>',
    explorador = '<path d="M12 21s7-5.2 7-11a7 7 0 1 0-14 0c0 5.8 7 11 7 11z"/><circle cx="12" cy="10" r="2.25"/>',
    acerca = '<circle cx="12" cy="12" r="9"/><path d="M12 10v6M12 7h.01"/>',
    guia = '<circle cx="12" cy="12" r="9"/><path d="M9.1 9a3 3 0 0 1 5.8 1c0 2-3 3-3 3"/><path d="M12 17h.01"/>',
    ext = '<path d="M10 5H5v14h14v-5"/><path d="M13 5h6v6M19 5l-8 8"/>',
    menu = '<path d="M4 7h16M4 12h16M4 17h16"/>'
  )
  shiny::HTML(paste0(
    '<svg class="ec-app-icon" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">',
    if (is.null(paths[[name]])) "" else paths[[name]],
    "</svg>"
  ))
}

ec_app_chrome <- function() {
  shiny::tagList(
    shiny::tags$div(
      id = "ec-app-shell",
      class = "ec-app-shell",
      shiny::tags$header(
        id = "ec-app-topbar",
        class = "ec-app-topbar",
        shiny::tags$button(
          type = "button",
          id = "ec-app-menu-btn",
          class = "ec-app-menu-btn",
          `aria-expanded` = "false",
          `aria-controls` = "ec-app-sidebar",
          `aria-label` = "Abrir men\u00fa",
          ec_app_icon("menu")
        ),
        shiny::tags$a(
          class = "ec-app-logo", href = "/",
          shiny::tags$span("rodaja", class = "ec-app-logo-a"),
          shiny::tags$span(".", class = "ec-app-logo-dot"),
          shiny::tags$span("cl", class = "ec-app-logo-b"),
          shiny::tags$span(
            class = "ra-logo-mark",
            `aria-hidden` = "true",
            shiny::tags$img(
              src = paste0("/images/rodaja-logo.webp?v=", APP_ASSET_VERSION),
              width = "500",
              height = "500",
              alt = ""
            )
          )
        )
      ),
      shiny::tags$button(
        type = "button",
        id = "ec-app-backdrop",
        class = "ec-app-backdrop",
        hidden = "hidden",
        `aria-label` = "Cerrar men\u00fa"
      ),
      shiny::tags$aside(
        id = "ec-app-sidebar",
        class = "ec-app-sidebar",
        `aria-label` = "Navegaci\u00f3n",
        shiny::tags$div(
          class = "ec-app-sidebar-logo",
          shiny::tags$a(
            class = "ec-app-logo", href = "/",
            shiny::tags$span("rodaja", class = "ec-app-logo-a"),
            shiny::tags$span(".", class = "ec-app-logo-dot"),
            shiny::tags$span("cl", class = "ec-app-logo-b"),
            shiny::tags$span(
              class = "ra-logo-mark",
              `aria-hidden` = "true",
              shiny::tags$img(
                src = paste0("/images/rodaja-logo.webp?v=", APP_ASSET_VERSION),
                width = "500",
                height = "500",
                alt = ""
              )
            )
          )
        ),
        shiny::tags$nav(
          class = "ec-app-nav ra-nav",
          `aria-label` = "Explorador"
        ),
        shiny::tags$div(
          class = "ec-app-sidebar-foot",
          shiny::tags$a(class = "ec-app-precios", href = "/precios/", "Suscribirse"),
          shiny::tags$span(class = "rodaja-pwa-install-slot"),
          shiny::tags$div(
            id = "ec-auth-bar",
            class = "ec-auth-bar ec-session ra-session",
            shiny::tags$a(
              class = "ec-session-cta ra-session-cta",
              href = "#",
              `data-rodaja-signin` = NA,
              "Entrar a su cuenta"
            )
          )
        )
      )
    ),
    shiny::tags$script(HTML("
(function(){
  function hoist(){
    var el=document.getElementById('ec-app-shell');
    if(!el)return;
    if(el.parentNode!==document.body)document.body.insertBefore(el,document.body.firstChild);
    document.documentElement.classList.add('ec-app');
  }
  function setNavOpen(on){
    document.documentElement.classList.toggle('ec-nav-open',!!on);
    var btn=document.getElementById('ec-app-menu-btn');
    var bd=document.getElementById('ec-app-backdrop');
    if(btn){
      btn.setAttribute('aria-expanded',on?'true':'false');
      btn.setAttribute('aria-label',on?'Cerrar men\\u00fa':'Abrir men\\u00fa');
    }
    if(bd)bd.hidden=!on;
    var side=document.getElementById('ec-app-sidebar');
    var mobile=window.matchMedia&&window.matchMedia('(max-width:899px)').matches;
    if(side){
      if(mobile)side.setAttribute('aria-hidden',on?'false':'true');
      else side.removeAttribute('aria-hidden');
    }
  }
  window.ecSetNavOpen=setNavOpen;
  function activateTab(name){
    var a=document.querySelector('.navbar-nav a[data-value=\"'+name+'\"]');
    if(a)a.click();
    document.querySelectorAll('.ec-app-nav [data-ec-tab]').forEach(function(el){
      var on=el.getAttribute('data-ec-tab')===name;
      el.classList.toggle('is-active',on);
      if(on)el.setAttribute('aria-current','page');
      else el.removeAttribute('aria-current');
    });
  }
  function showTab(name){
    activateTab(name);
    setNavOpen(false);
  }
  function bind(){
    var btn=document.getElementById('ec-app-menu-btn');
    var bd=document.getElementById('ec-app-backdrop');
    var side=document.getElementById('ec-app-sidebar');
    if(btn&&!btn._ecBound){btn._ecBound=true;btn.addEventListener('click',function(){setNavOpen(!document.documentElement.classList.contains('ec-nav-open'));});}
    if(bd&&!bd._ecBound){bd._ecBound=true;bd.addEventListener('click',function(){setNavOpen(false);});}
    if(side&&!side._ecDelegated){
      side._ecDelegated=true;
      side.addEventListener('click',function(e){
        var t=e.target;
        var act=t.closest?t.closest('[data-ec-action]'):null;
        if(act&&side.contains(act)){
          e.preventDefault();
          e.stopPropagation();
          var name=act.getAttribute('data-ec-action');
          var mobile=window.matchMedia&&window.matchMedia('(max-width:899px)').matches;
          activateTab('Explorador');
          if(name==='guia'){
            setNavOpen(false);
            if(typeof window.ecStartManualTour==='function'){
              if(mobile)setTimeout(function(){window.ecStartManualTour();},220);
              else window.ecStartManualTour();
            }
            return;
          }
          return;
        }
        var tab=t.closest?t.closest('[data-ec-tab]'):null;
        if(tab&&side.contains(tab)){
          e.preventDefault();
          showTab(tab.getAttribute('data-ec-tab'));
          return;
        }
        var a=t.closest?t.closest('a'):null;
        if(a&&side.contains(a))setNavOpen(false);
      });
    }
  }
  document.addEventListener('keydown',function(e){if(e.key==='Escape')setNavOpen(false);});
  if(window.matchMedia){
    var mq=window.matchMedia('(min-width:900px)');
    function onMq(ev){if(ev.matches)setNavOpen(false);}
    if(mq.addEventListener)mq.addEventListener('change',onMq);
    else if(mq.addListener)mq.addListener(onMq);
  }
  hoist();bind();
  document.addEventListener('DOMContentLoaded',function(){hoist();bind();});
  if(window.jQuery)jQuery(document).on('shiny:connected',function(){hoist();bind();});
  if(window.matchMedia&&window.matchMedia('(max-width:899px)').matches){
    var side=document.getElementById('ec-app-sidebar');
    if(side)side.setAttribute('aria-hidden','true');
  }
})();
"))
  )
}

ec_navbar_precios <- function() {
  shiny::tags$script(HTML(
    "(function(){function wirePrecios(){var acerca=document.querySelector('.navbar-nav a[data-value=\"Acerca de\"]');if(!acerca||document.getElementById('ec-nav-precios-item'))return;var li=acerca.parentElement;if(!li||!li.parentNode)return;var preciosLi=document.createElement('li');preciosLi.id='ec-nav-precios-item';preciosLi.className='ec-nav-precios-item';preciosLi.innerHTML='<a href=\"/precios/\">Suscribirse</a>';li.parentNode.insertBefore(preciosLi,li.nextSibling);}if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',wirePrecios);else wirePrecios();if(typeof jQuery!=='undefined')jQuery(document).on('shiny:connected',wirePrecios);else document.addEventListener('shiny:connected',wirePrecios);})();"
  ))
}

ec_consent_banner <- function() {
  shiny::tagList(
    shiny::tags$div(
      id = "rodaja-consent-banner",
      class = "rodaja-consent",
      hidden = "hidden",
      role = "dialog",
      `aria-modal` = "true",
      `aria-label` = "Preferencias de cookies",
      shiny::tags$p(
        class = "rodaja-consent-text",
        "Usamos cookies para anal\u00edtica y, con tu permiso, publicidad. ",
        shiny::tags$a(href = "/legal/privacidad/", "Privacidad")
      ),
      shiny::tags$div(
        class = "rodaja-consent-actions",
        shiny::tags$button(
          type = "button", id = "rodaja-consent-reject",
          class = "rodaja-consent-btn rodaja-consent-btn-secondary",
          "Solo necesarias"
        ),
        shiny::tags$button(
          type = "button", id = "rodaja-consent-accept",
          class = "rodaja-consent-btn rodaja-consent-btn-primary",
          "Aceptar"
        )
      )
    ),
    shiny::tags$style(HTML("
.rodaja-consent{position:fixed;bottom:0;left:0;right:0;z-index:100200!important;background:#1a1a1a;color:#eee;padding:1.25rem 1.75rem;padding-bottom:max(1.25rem,env(safe-area-inset-bottom));min-height:88px;box-sizing:border-box;display:flex;flex-wrap:wrap;gap:1.25rem;align-items:center;justify-content:space-between;font-family:system-ui,sans-serif;font-size:17px;line-height:1.5;box-shadow:0 -6px 32px rgba(0,0,0,.4);border-top:4px solid #1D9E75}
.rodaja-consent[hidden]{display:none!important}
.rodaja-consent--visible{display:flex!important}
.rodaja-consent-text{margin:0;flex:1 1 280px;max-width:52rem}
.rodaja-consent a{color:#7dcea0;text-decoration:underline;font-weight:500}
.rodaja-consent-actions{display:flex;gap:.75rem;flex-wrap:wrap;flex-shrink:0;align-items:center}
.rodaja-consent-btn{border:none;border-radius:8px;padding:.75rem 1.5rem;cursor:pointer;font-size:16px;font-weight:600;min-height:48px;min-width:9rem;line-height:1.2}
.rodaja-consent-btn-primary{background:#1D9E75;color:#fff}
.rodaja-consent-btn-secondary{background:transparent;color:#eee;border:2px solid #888}
html.rodaja-consent-visible body{padding-bottom:max(104px,env(safe-area-inset-bottom))}
@media (max-width:640px){
  .rodaja-consent{padding:1rem 1.25rem;font-size:16px;min-height:0}
  .rodaja-consent-actions{width:100%;justify-content:stretch}
  .rodaja-consent-btn{flex:1 1 auto;min-width:0}
}
"))
  )
}

ec_dashboard_layout <- function(title = APP_TITLE) {
  shiny::navbarPage(
    title = shiny::tags$span(
      class = "ec-navbar-brand-inner",
      shiny::tags$img(
        src = paste0("images/rodaja-logo.webp?v=", APP_ASSET_VERSION),
        alt = "rodaja.cl",
        class = "ec-navbar-logo"
      ),
      shiny::tags$span(class = "ec-navbar-wordmark",
        shiny::tags$span("rodaja", class = "ec-navbar-wordmark-a"),
        shiny::tags$span(".cl", class = "ec-navbar-wordmark-b")
      )
    ),
    windowTitle = "rodaja.cl",
    collapsible = TRUE,
    header = shiny::tagList(
      shiny::tags$link(rel = "icon", type = "image/png", sizes = "32x32",
        href = paste0("images/favicon-32.png?v=", APP_ASSET_VERSION)),
      shiny::tags$link(rel = "icon", type = "image/png", sizes = "16x16",
        href = paste0("/icons/favicon-16.png?v=", APP_ASSET_VERSION)),
      shiny::tags$link(rel = "icon", href = paste0("/favicon.ico?v=", APP_ASSET_VERSION), sizes = "any"),
      shiny::tags$link(rel = "apple-touch-icon",
        href = paste0("images/apple-touch-icon.png?v=", APP_ASSET_VERSION)),
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no, viewport-fit=cover"),
      if (nzchar(Sys.getenv("GTM_CONTAINER_ID", ""))) {
        gtm_id <- Sys.getenv("GTM_CONTAINER_ID", "")
        shiny::tagList(
          shiny::tags$script(src = "/analytics/consent.js?v=20260705consent5"),
          shiny::HTML(sprintf(
            "<script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src='https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);})(window,document,'script','dataLayer',%s);</script>",
            jsonlite::toJSON(gtm_id, auto_unbox = TRUE)
          )),
          shiny::HTML(sprintf(
            '<noscript><iframe src="https://www.googletagmanager.com/ns.html?id=%s" height="0" width="0" style="display:none;visibility:hidden"></iframe></noscript>',
            htmltools::htmlEscape(gtm_id)
          ))
        )
      },
      shiny::tags$script(HTML(sprintf(
        "window.RODAJA_CONFIG={clerkPublishableKey:%s,accountUrl:%s,gtmId:%s,siteCanonicalBase:%s};",
        jsonlite::toJSON(Sys.getenv("CLERK_PUBLISHABLE_KEY", ""), auto_unbox = TRUE),
        jsonlite::toJSON(Sys.getenv("CLERK_ACCOUNT_URL", "https://accounts.rodaja.cl"), auto_unbox = TRUE),
        jsonlite::toJSON(Sys.getenv("GTM_CONTAINER_ID", ""), auto_unbox = TRUE),
        jsonlite::toJSON(Sys.getenv("SITE_CANONICAL_BASE", "https://app.rodaja.cl"), auto_unbox = TRUE)
      ))),
      shiny::tags$script(src = paste0("/analytics/analytics-events.js?v=", APP_ASSET_VERSION)),
      shiny::tags$style(HTML(
        ".tab-pane[data-value='Acerca de']:not(.active):not(.show) .ec-acerca-outer{display:none!important;visibility:hidden!important;pointer-events:none!important}.tab-pane[data-value='Explorador'].active,.tab-pane[data-value='Explorador'].show{display:block!important}"
      )),
      shiny::tags$script(HTML(sprintf(
        "window.EC_FAVORITES_STORAGE_KEY = %s;",
        jsonlite::toJSON(EC_FAVORITES_STORAGE_KEY, auto_unbox = TRUE)
      ))),
      if (requireNamespace("cicerone", quietly = TRUE)) {
        cicerone::use_cicerone()
      },
      if (nzchar(EC_EXPLORADOR_BASE_PATH)) {
        shiny::tags$script(HTML(sprintf(
          "window.EC_EXPLORADOR_BASE_PATH = %s;",
          jsonlite::toJSON(EC_EXPLORADOR_BASE_PATH, auto_unbox = TRUE)
        )))
      },
      shiny::tags$script(HTML(sprintf(
        "window.EC_ASSET_VERSION = %s;\nwindow.EC_DEFAULT_VARIABLE_ID = %s;\nwindow.EC_VARIABLE_COOKIE = %s;",
        jsonlite::toJSON(APP_ASSET_VERSION, auto_unbox = TRUE),
        jsonlite::toJSON(as.integer(EC_DEFAULT_VARIABLE_ID), auto_unbox = TRUE),
        jsonlite::toJSON(EC_VARIABLE_COOKIE, auto_unbox = TRUE)
      ))),
      shiny::tags$script(src = paste0("ec_overlay_recovery.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_map_boot.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_favorites.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_user_geo.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_lazy_assets.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_station_group.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_icon_overlay.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_om_points.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_choropleth.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_positron_ocean.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("/rodaja_auth_core.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_alertas.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_auth.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = "/rodaja_pwa.js?v=20261008ayuda"),
      shiny::tags$script(src = paste0("ec_map_popup.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_map_popup_mobile.js?v=", APP_ASSET_VERSION)),
      shiny::tags$link(rel = "stylesheet", type = "text/css",
        href = paste0("/ec_soporte.css?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = "/ec_soporte.js?v=20261008llm"),
      shiny::tags$script(src = paste0("ec_onboarding.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_app_profile.js?v=", APP_ASSET_VERSION)),
      shiny::tags$script(src = paste0("ec_boot_fetch.js?v=", APP_ASSET_VERSION)),
      ec_app_chrome(),
      shiny::div(
        id = "ec_station_sheet",
        class = "ec-station-sheet",
        style = "display:none;",
        shiny::div(class = "ec-station-sheet-inner",
          shiny::tags$div(id = "ec_station_sheet_content", class = "ec-station-sheet-content")
        )
      ),
      shiny::div(style = "display:none;",
        shiny::textInput("ec_favorites_json", NULL, value = ""),
        shiny::textInput("ec_station_group_json", NULL, value = ""),
        shiny::textInput("ec_map_ready", NULL, value = "0"),
        shiny::textInput("ec_auth_token", NULL, value = ""),
        shiny::numericInput("ec_estacion_lazy_load", NULL, value = 0, min = 0),
        shiny::textInput("ec_onboarding_run", NULL, value = ""),
        shiny::textInput("ec_alertas_mapa_json", NULL, value = ""),
        shiny::textInput("ec_metadata_json", NULL, value = ""),
        shiny::textInput("ec_geo_conteos_json", NULL, value = ""),
        shiny::textInput("ec_boot_variable", NULL, value = ""),
        shiny::numericInput("ec_user_active", NULL, value = 0, min = 0)
      )
    ),

    shiny::tabPanel("Explorador",
      shiny::div(id = "ec_explorador_viewport", class = "outer",
        shiny::tags$head(
          shiny::tags$script(HTML(
            "(function(){var c='width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no, viewport-fit=cover';var list=document.getElementsByName('viewport');for(var i=0;i<list.length;i++)list[i].setAttribute('content',c);document.documentElement.style.touchAction='pan-x pan-y';})();"
          )),
          shiny::tags$link(rel = "manifest", href = "/manifest.webmanifest"),
          shiny::tags$meta(name = "theme-color", content = "#DF9531"),
          shiny::tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
          shiny::tags$meta(name = "apple-mobile-web-app-title", content = "rodaja"),
          shiny::tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "default"),
          shiny::tags$link(rel = "stylesheet", type = "text/css",
            href = paste0("themes/rodaja.css?v=", APP_ASSET_VERSION)),
          shiny::tags$link(rel = "stylesheet", type = "text/css",
            href = paste0("ec_onboarding.css?v=", APP_ASSET_VERSION)),
          shiny::tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
          shiny::tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=DM+Mono:wght@400;500&display=swap"),
          shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css", crossorigin = "anonymous"),
          shiny::tags$script(src = paste0("resize-panel.js?v=", APP_ASSET_VERSION))
        ),

        shiny::div(
          id = "ec-map-toolbar",
          class = "ec-map-toolbar",
          shiny::div(
            id = "ec-map-tools",
            class = "ec-auth-bar",
            shiny::tags$span(
              class = "ec-auth-fav-slot",
              shiny::tags$div(
                class = "btn-group ec-nav-fav-group",
                shiny::tags$button(
                  type = "button",
                  id = "ec-btn-mi-estacion",
                  class = "btn btn-default ec-btn-mi-estacion",
                  title = "Mi estaci\u00f3n favorita",
                  onclick = "return ecNavIrMiEstacion();",
                  shiny::icon("star"),
                  shiny::tags$span(class = "ec-btn-mi-estacion-label", " Mi estaci\u00f3n")
                )
              )
            )
          )
        ),
        shiny::uiOutput("mapa_mount"),
        shiny::div(
          id = "mapa_mount_placeholder",
          class = "ec-map-deferred",
          role = "status",
          `aria-live` = "polite",
          `aria-label` = "Cargando mapa",
          shiny::tags$span(
            class = "ec-map-deferred-spin",
            `aria-hidden` = "true",
            shiny::tags$span(
              class = "ec-logo-loader",
              shiny::tags$div(
                class = "ec-logo-loader-clip",
                shiny::tags$img(
                  class = "ec-logo-loader-mark",
                  src = paste0("images/rodaja-logo.webp?v=", APP_ASSET_VERSION),
                  alt = "",
                  width = "500",
                  height = "500"
                )
              ),
              shiny::tags$svg(
                class = "ec-logo-loader-orbit",
                viewBox = "0 0 500 352",
                overflow = "visible",
                fill = "none",
                shiny::tags$defs(
                  shiny::tags$linearGradient(
                    id = "ec-logo-loader-grad",
                    x1 = "0%", y1 = "0%", x2 = "100%", y2 = "100%",
                    shiny::tags$stop(offset = "0%", `stop-color` = "#F6B358"),
                    shiny::tags$stop(offset = "100%", `stop-color` = "#B97618")
                  )
                ),
                shiny::tags$path(
                  id = "ec-logo-loader-wave",
                  class = "ec-logo-loader-arc",
                  `stroke-linecap` = "round",
                  d = "M384.00,196.00L387.95,199.56L390.74,203.27L391.57,206.98L390.17,210.52L386.84,213.75L382.37,216.65L377.86,219.33L374.34,222.00L372.57,224.95L372.81,228.37L374.73,232.35L377.54,236.79L380.20,241.40L381.70,245.79L381.34,249.58L378.92,252.51L374.75,254.55L369.61,255.93L364.49,257.08L360.32,258.54L357.73,260.79L356.89,264.12L357.49,268.50L358.79,273.59L359.90,278.79L359.97,283.43L358.46,286.92L355.24,288.96L350.65,289.62L345.34,289.34L340.11,288.85L335.69,288.95L332.53,290.29L330.71,293.20L329.92,297.55L329.59,302.79L329.03,308.08L327.67,312.51L325.15,315.37L321.46,316.32L316.89,315.52L311.93,313.61L307.11,311.53L302.87,310.26L299.45,310.56L296.82,312.76L294.73,316.66L292.79,321.54L290.63,326.40L287.96,330.19L284.68,332.13L280.88,331.89L276.78,329.72L272.65,326.37L268.71,322.90L265.07,320.39L261.73,319.62L258.55,320.90L255.35,323.96L252.00,328.00L248.44,331.95L244.73,334.74L241.02,335.57L237.48,334.17L234.25,330.84L231.35,326.37L228.67,321.86L226.00,318.34L223.05,316.57L219.63,316.81L215.65,318.73L211.21,321.54L206.60,324.20L202.21,325.70L198.42,325.34L195.49,322.92L193.45,318.75L192.07,313.61L190.92,308.49L189.46,304.32L187.21,301.73L183.88,300.89L179.50,301.49L174.41,302.79L169.21,303.90L164.57,303.97L161.08,302.46L159.04,299.24L158.38,294.65L158.66,289.34L159.15,284.11L159.05,279.69L157.71,276.53L154.80,274.71L150.45,273.92L145.21,273.59L139.92,273.03L135.49,271.67L132.63,269.15L131.68,265.46L132.48,260.89L134.39,255.93L136.47,251.11L137.74,246.87L137.44,243.45L135.24,240.82L131.34,238.73L126.46,236.79L121.60,234.63L117.81,231.96L115.87,228.68L116.11,224.88L118.28,220.78L121.63,216.65L125.10,212.71L127.61,209.07L128.38,205.73L127.10,202.55L124.04,199.35L120.00,196.00L116.05,192.44L113.26,188.73L112.43,185.02L113.83,181.48L117.16,178.25L121.63,175.35L126.14,172.67L129.66,170.00L131.43,167.05L131.19,163.63L129.27,159.65L126.46,155.21L123.80,150.60L122.30,146.21L122.66,142.42L125.08,139.49L129.25,137.45L134.39,136.07L139.51,134.92L143.68,133.46L146.27,131.21L147.11,127.88L146.51,123.50L145.21,118.41L144.10,113.21L144.03,108.57L145.54,105.08L148.76,103.04L153.35,102.38L158.66,102.66L163.89,103.15L168.31,103.05L171.47,101.71L173.29,98.80L174.08,94.45L174.41,89.21L174.97,83.92L176.33,79.49L178.85,76.63L182.54,75.68L187.11,76.48L192.07,78.39L196.89,80.47L201.13,81.74L204.55,81.44L207.18,79.24L209.27,75.34L211.21,70.46L213.37,65.60L216.04,61.81L219.32,59.87L223.12,60.11L227.22,62.28L231.35,65.63L235.29,69.10L238.93,71.61L242.27,72.38L245.45,71.10L248.65,68.04L252.00,64.00L255.56,60.05L259.27,57.26L262.98,56.43L266.52,57.83L269.75,61.16L272.65,65.63L275.33,70.14L278.00,73.66L280.95,75.43L284.37,75.19L288.35,73.27L292.79,70.46L297.40,67.80L301.79,66.30L305.58,66.66L308.51,69.08L310.55,73.25L311.93,78.39L313.08,83.51L314.54,87.68L316.79,90.27L320.12,91.11L324.50,90.51L329.59,89.21L334.79,88.10L339.43,88.03L342.92,89.54L344.96,92.76L345.62,97.35L345.34,102.66L344.85,107.89L344.95,112.31L346.29,115.47L349.20,117.29L353.55,118.08L358.79,118.41L364.08,118.97L368.51,120.33L371.37,122.85L372.32,126.54L371.52,131.11L369.61,136.07L367.53,140.89L366.26,145.13L366.56,148.55L368.76,151.18L372.66,153.27L377.54,155.21L382.40,157.37L386.19,160.04L388.13,163.32L387.89,167.12L385.72,171.22L382.37,175.35L378.90,179.29L376.39,182.93L375.62,186.27L376.90,189.45L379.96,192.65Z"
                )
              ),
              shiny::tags$script(HTML(paste0(
                "(function(){function boot(){var p=document.getElementById('ec-logo-loader-wave');",
                "if(!p||!p.getTotalLength||p.dataset.ecWave)return;",
                "var len=p.getTotalLength();if(!len)return;p.dataset.ecWave='1';",
                "p.style.setProperty('--len',len+'px');",
                "p.style.strokeDasharray=(0.22*len)+' '+(0.78*len);}",
                "if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',boot);else boot();})();"
              )))
            )
          )
        ),

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
          Shiny.addCustomMessageHandler('panel_modo', function(msg) {
            var s = document.getElementById('ec_panel_serie');
            var t = document.getElementById('ec_panel_tabla');
            if (s && t) {
              if (msg && msg.modo === 'tabla') { s.style.display = 'none'; t.style.display = 'block'; }
              else { s.style.display = 'block'; t.style.display = 'none'; }
            }
          });
        ")),
        shiny::div(style = "display:none;",
          shiny::numericInput("panel_datos_state", NULL, value = 0, min = 0, max = 3, step = 1),
          shiny::numericInput("abrir_mi_estacion", NULL, value = 0, min = 0),
          shiny::numericInput("abrir_reporte_mi_estacion", NULL, value = 0, min = 0),
          shiny::numericInput("fijar_favorito_mapa", NULL, value = 0, min = 0),
          shiny::numericInput("wizard_geo_lat", NULL, value = NA_real_),
          shiny::numericInput("wizard_geo_lon", NULL, value = NA_real_),
          shiny::textInput("map_geo_payload", NULL, value = ""),
          shiny::numericInput("map_geo_pending", NULL, value = 0, min = 0),
          shiny::numericInput("map_geo_pick_eid", NULL, value = 0, min = 0),
          shiny::numericInput("wizard_abrir_eid", NULL, value = 0, min = 0),
          shiny::numericInput("wizard_guardar_eid", NULL, value = 0, min = 0),
          shiny::numericInput("wizard_open", NULL, value = 0, min = 0),
          shiny::numericInput("wizard_back_step", NULL, value = 0, min = 0),
          shiny::numericInput("reporte_tab_select", NULL, value = 0, min = 0),
          shiny::numericInput("reporte_tab_close", NULL, value = 0, min = 0)
        ),
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
            style = "background:rgba(255,255,255,0.95);padding:0;border-radius:8px 8px 0 0;box-shadow:0 -2px 12px rgba(0,0,0,0.08);display:flex;flex-direction:column;min-height:220px;height:320px;",
            shiny::div(class = "ec-panel-top-bar",
              shiny::div(class = "ec-panel-resize-handle", title = "Arrastra para cambiar tama\u00f1o"),
              shiny::div(class = "ec-panel-datos-btns",
                shiny::actionButton("panel_minimize", NULL, class = "ec-panel-btn ec-panel-btn-minimize", title = "Minimizar"),
                shiny::actionButton("panel_maximize", NULL, class = "ec-panel-btn ec-panel-btn-maximize", title = "Maximizar"),
                shiny::actionButton("panel_close", NULL, class = "ec-panel-btn ec-panel-btn-close", title = "Cerrar")
              )
            ),
            shiny::div(class = "ec-panel-datos-body",
              style = "padding:8px;flex:1 1 auto;min-height:0;display:flex;flex-direction:column;",
              shiny::div(id = "ec_panel_serie", class = "ec-panel-contenido",
                grafico_ui("grafico")
              ),
              shiny::div(id = "ec_panel_tabla", class = "ec-panel-contenido", style = "display:none;",
                tabla_estaciones_ui("tabla_estaciones")
              )
            )
          )
        ),

        shiny::tags$div(id = "cite",
          style = "position:absolute;bottom:6px;right:50px;font-size:11px;color:#8C9091;",
          "rodaja.cl - datos climaticos de Chile")
      ),

      # ponytail: inputs siguen vivos (server.R); el menú de filtros ya no se muestra.
      shiny::div(
        id = "ec_drawer_filtros",
        class = "ec-drawer",
        style = "display:none !important;",
        `aria-hidden` = "true",
        ec_sidebar_filters()
      ),
      shiny::tags$script(HTML("
        (function(){
          window.ecSyncFiltrosNav = function(){};
          window.ecToggleDrawer = function(){ return false; };
          window.ecCloseDrawer = function(){};
          window.ecOpenDrawer = function(){ return false; };
          function registerDrawerHandlers() {
            if (window._ecDrawerHandlersRegistered) return;
            if (typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler) return;
            window._ecDrawerHandlersRegistered = true;
            Shiny.addCustomMessageHandler('drawer_close', function(_){ });
          }
          document.addEventListener('shiny:connected', registerDrawerHandlers);
          registerDrawerHandlers();
        })();
      ")),
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
    border: 2px solid rgba(255,255,255,0.42) !important;
    box-shadow: none !important;
    color: #F7F9FA !important;
    font-weight: 700 !important;
    font-size: 18px !important;
    min-height: 52px !important;
    height: auto !important;
    line-height: 1.35 !important;
    padding: 12px 14px !important;
  }
  #ec_drawer_filtros .selectize-input .item {
    color: #F7F9FA !important;
    font-weight: 700 !important;
    font-size: 18px !important;
    opacity: 1 !important;
  }
  #ec_drawer_filtros .selectize-input .placeholder,
  #ec_drawer_filtros .selectize-input input,
  #ec_drawer_filtros .selectize-control .placeholder {
    color: #F7F9FA !important;
    font-weight: 700 !important;
    font-size: 18px !important;
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
    padding: 14px 16px !important;
    font-size: 18px !important;
    font-weight: 600 !important;
  }
  #ec_drawer_filtros select option:hover,
  #ec_drawer_filtros select option:checked {
    background: #DF9531 !important;
    color: #fff !important;
  }
  /* selectizeInput - campo de escritura */
  #ec_drawer_filtros .selectize-input input {
    color: #F7F9FA !important;
    background: transparent !important;
    font-weight: 600 !important;
    opacity: 1 !important;
    caret-color: #F7F9FA !important;
  }
  /* Dropdown oscuro con texto claro */
  #ec_drawer_filtros .selectize-dropdown {
    background: #1e232a !important;
    background-color: #1e232a !important;
    border: 1px solid rgba(255,255,255,0.2) !important;
    border-radius: 6px !important;
  }
  #ec_drawer_filtros .selectize-dropdown .option {
    color: #F7F9FA !important;
    background: transparent !important;
    padding: 14px 16px !important;
    font-size: 18px !important;
    font-weight: 600 !important;
    line-height: 1.35 !important;
  }
  #ec_drawer_filtros .selectize-dropdown .option:hover,
  #ec_drawer_filtros .selectize-dropdown .option.active {
    background: #DF9531 !important;
    color: #fff !important;
  }
  /* Item seleccionado visible */
  #ec_drawer_filtros .selectize-input .item {
    color: #F7F9FA !important;
    font-weight: 600 !important;
  }
  /* Placeholder visible */
  #ec_drawer_filtros .selectize-input .placeholder {
    color: rgba(247,249,250,0.9) !important;
    font-weight: 600 !important;
  }
  /* Dropdown fondo oscuro - .selectize-dropdown-content es el contenedor interno */
  #ec_drawer_filtros .selectize-dropdown,
  #ec_drawer_filtros .selectize-dropdown-content {
    background-color: #1e232a !important;
    background: #1e232a !important;
  }
  #ec_drawer_filtros .selectize-dropdown .option {
    background-color: #1e232a !important;
    color: #F7F9FA !important;
  }
  #ec_drawer_filtros .selectize-dropdown .option.selected,
  #ec_drawer_filtros .selectize-dropdown .option:hover,
  #ec_drawer_filtros .selectize-dropdown .option.active {
    background-color: #DF9531 !important;
    color: #fff !important;
  }
  /* body > .selectize-dropdown: dropdown con dropdownParent="body" */
  body > .selectize-dropdown {
    max-height: 400px !important;
    overflow-y: auto !important;
    z-index: 9999 !important;
    background: #1e232a !important;
    border: 1px solid rgba(255,255,255,0.2) !important;
    border-radius: 6px !important;
  }
  body > .selectize-dropdown .option {
    color: #F7F9FA !important;
    background: #1e232a !important;
    padding: 14px 16px !important;
    font-size: 18px !important;
    font-weight: 600 !important;
    line-height: 1.35 !important;
  }
  body > .selectize-dropdown .option:hover,
  body > .selectize-dropdown .option.active {
    background: #DF9531 !important;
    color: #fff !important;
  }
  body > .selectize-dropdown .selectize-dropdown-content {
    background: #1e232a !important;
    max-height: 400px !important;
    overflow-y: auto !important;
  }
  
  .ec-contador-estaciones { font-size: 16px !important; color: rgba(247,249,250,0.92) !important; text-align: center !important; padding: 8px 0 12px 0 !important; letter-spacing: 0.02em !important; font-weight: 600 !important; line-height: 1.45 !important; }
  #ec_drawer_filtros .ec-drawer-body { overflow: visible !important; }
  #ec_drawer_filtros.ec-drawer-open { overflow: visible !important; max-height: none !important; height: auto !important; }
  #ec_drawer_filtros .ec-filter-block { margin-bottom: 8px !important; }
  #ec_drawer_filtros .selectize-input { min-height: 44px !important; padding: 10px 12px !important; font-size: 16px !important; }
  #ec_drawer_filtros .ec-drawer-primary-actions { display: flex !important; gap: 10px !important; }
  #ec_drawer_filtros .ec-drawer-primary-actions .btn { flex: 1 !important; width: auto !important; margin-top: 0 !important; }
  #ec_drawer_filtros .ec-btn-ver-datos,
  #ec_drawer_filtros .ec-btn-descargar-datos {
    font-size: 17px !important;
    font-weight: 700 !important;
    min-height: 48px !important;
    padding: 12px 14px !important;
    border-radius: 10px !important;
  }
  #ec_drawer_filtros .ec-btn-drawer-secondary {
    width: 100% !important;
    background: transparent !important;
    border: 2px solid rgba(255,255,255,0.42) !important;
    color: #F7F9FA !important;
    font-size: 16px !important;
    font-weight: 700 !important;
    min-height: 46px !important;
    border-radius: 10px !important;
    box-shadow: none !important;
  }
  #ec_drawer_filtros .ec-btn-drawer-secondary:hover {
    background: rgba(255,255,255,0.12) !important;
    color: #fff !important;
  }
  #ec_drawer_filtros .ec-grupo-drawer-actions { gap: 8px !important; }
'))
    ),

    shiny::tabPanel("Acerca de",
      shiny::tags$div(class = "ec-acerca-outer",
        shiny::tags$div(class = "ec-acerca-wrap",
          shiny::tags$link(rel = "stylesheet",
            href = paste0("/acerca/styles.css?v=", APP_ASSET_VERSION)),
          shiny::includeHTML("www/acerca-sections.html"),
          shiny::tags$p(class = "ec-acerca-full-link",
            shiny::tags$a(href = "/acerca/", target = "_blank", rel = "noopener noreferrer",
              "Abrir p\u00e1gina completa en rodaja.cl"))
        )
      )
    ),

    footer = ec_consent_banner()
  )
}

