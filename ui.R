ui <- tagList(
  tags$head(
    tags$style("
    body {background-color: #f7f7f7;}
    .value-box-area {padding: 0 !important;}
    "),
    shinyjs::useShinyjs(),
    waiter::use_waiter()
    # sever::useSever()
  ),
  page_navbar(
    title = app_title,
    collapsible = TRUE,
    inverse = FALSE,
    theme = bs_theme(
      # base_font = font_google(
      #   app_font,
      #   wght = c(300, 400, 500, 600, 700, 800),
      #   ital = c(0, 1)
      # ),
      font_scale = 0.8,
      bootswatch = "minty",
      primary = "#4682B4",
      "navbar-bg" = "#f8f9fa"
    ),
    mod_amex_ui("flights"),
    mod_meeting_place_ui("mp"),

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
      waiter::spin_3()
    ),
    color = "#ffffff"
  )
)
