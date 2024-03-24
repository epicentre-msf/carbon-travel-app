
ui <- page_navbar(
  title = "MSF Carbon Travel App",
  theme = bs_theme(
    bootswatch = "minty",
    font_scale = .8,
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
  
)