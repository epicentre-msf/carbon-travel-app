ui <- tagList(
  tags$head(
    tags$style("
    .vb .card-body {padding: 5px !important;} 
    .value-box-area {padding: 0 !important;}
    .selectize-dropdown, .selectize-input, .form-control {font-size: 0.8rem !important;}
    "),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    sever::useSever()
  ),
  
  page_navbar(
    id = "tabs",
    title = "MSF Carbon Travel App",
    collapsible = TRUE,
    inverse = FALSE,
    theme = bs_theme(
      preset = "minty",
      primary = "#4682B4",
      font_scale = 0.8
    ),
    
    # amex flight analysis
    mod_amex_ui("flights"), 

    # meeting place planner
    nav_panel(
      "Meeting Place Planner",
      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 300,
          gap = 0,
          mod_origin_input_ui("origin")
        ),
        mod_meeting_place_ui("mp")
      )
    ),

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
  waiter::waiter_preloader(
    html = tagList(
      tags$img(
        src = "epicentre_logo.png",
        width = 500,
        style = "padding: 20px;"
      ),
      h1("Data Science"),
      waiter::spin_3()
    ),
    color = "#f8f9fa"
  )
)