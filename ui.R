ui <- tagList(
  tags$head(
    tags$style("
    body {background-color: #f7f7f7;}
    .vb .card-body {padding: 5px !important;} 
    .value-box-area {padding: 0 !important;}
    .selectize-dropdown, .selectize-input, .form-control {font-size: 0.8rem !important;}
    "),
    shinyjs::useShinyjs(),
    waiter::use_waiter()
  ),
  page_navbar(
    title = app_title,
    collapsible = TRUE,
    inverse = FALSE,
    theme = bs_theme(
      font_scale = 0.8,
      bootswatch = "minty",
      primary = "#4682B4",
      "navbar-bg" = "#f8f9fa"
    ),
    mod_travel_analysis_ui("flights"),
    mod_meeting_place_ui("mp"),
    mod_travel_estim_ui("travel_estim"),
    
    # nav_panel(
    #   "About",
    #   icon = bsicons::bs_icon("info-circle"),
    #   div(
    #     class = "container",
    #     h1("Info")
    #   )
    # ),
    
    # nav images and links
    nav_spacer(),
    nav_item(
      tags$a(
        "Developed by Epi-DS",
        href = "https://epicentre-msf.github.io/gallery/",
        target = "_blank"
      )
    ),
    # nav_item(
    #   tags$a(
    #     shiny::icon("github"),
    #     "Report an issue",
    #     href = "https://github.com/epicentre-msf/carbon-travel-app/issues",
    #     target = "_blank"
    #   )
    # ),
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
          src = "climate_msf_logo.png",
          alt = "Climate Smart MSF Logo",
          height = "35px"
        ),
        class = "py-0 d-none d-lg-block",
        title = "Climate Smart MSF",
        href = "https://msfintl.sharepoint.com/sites/ClimateSmartMSF/?OR=Teams-HL&CT=1716448053305",
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
      tags$img(
        src = "climate_msf_logo.png",
        width = 300,
        style = "padding: 20px;"
      ),
      waiter::spin_3()
    ),
    color = "#ffffff"
  )
)
