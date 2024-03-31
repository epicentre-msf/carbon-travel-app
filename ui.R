ui <- tagList(
  tags$head(
    tags$style(".value-box-area {padding: 5px !important;}"),
    # tags$link(href = google_font(p_font), rel = "stylesheet"),
    tags$style(
      HTML(glue::glue("p {{font-family: '{app_font}';}}"))
    ),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    sever::useSever()
  ),
  
  page_navbar(
    title = "MSF Carbon Travel App",
    fillable = TRUE,
    collapsible = TRUE,
    inverse = FALSE,
    theme = bs_theme(
      base_font = font_google(
        app_font, 
        wght = c(300, 400, 500, 600, 700, 800),
        ital = c(0, 1)
      ),
      font_scale = 0.8,
      bootswatch = "minty", 
      "navbar-bg" = "#f8f9fa"
    ),
    
    nav_panel(
      "Meeting Place Planner",
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          gap = 0,
          mod_origin_input_ui("origin")
        ),
        
        mod_meeting_place_ui("mp")
      )
    ),
    
    mod_amex_ui("flights"), 
    
    # nav images and links 
    nav_spacer(),
    nav_item(
      tags$a(
        shiny::icon("github"),
        "Report an issue",
        href = "https://github.com/epicentre-msf/carbon-travel-app/issues",
        target = "_blank"
      )
    ),
    nav_item(
      tags$a(
        tags$img(
          src = "epicentre_logo.png",
          alt = "Epicentre Logo",
          height = "35px"
        ),
        class = "py-0 d-none d-lg-block",
        title = "Epicentre",
        href = "https://epicentre.msf.org/",
        target = "_blank"
      )
    ),
    nav_item(
      tags$a(
        tags$img(
          src = "msf_logo.png",
          alt = "MSF Logo",
          height = "35px"
        ),
        class = "py-0 d-none d-lg-block",
        title = "MSF",
        href = "https://msf.org/",
        target = "_blank"
      )
    )
    
  ), 
  waiter::waiter_preloader(html = waiter::spin_3())
  
)