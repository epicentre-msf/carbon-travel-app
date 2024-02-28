library(shiny)
library(bslib)
library(tidyverse)
library(gt)

# Import data -------------------------------------------------------------
# Get the distance matrix
mat <- read_rds(here::here("data", "distance-matrix", "airports_distance_matrix.rds"))
# get the air_msf data
air_msf <- read_rds(here::here("data", "clean", "air_msf.rds"))
# get the conversion df - given by Maelle
df_conversion <- read_rds(here::here("data", "clean", "conversion_df.rds"))

cities <- air_msf %>%
  arrange(city_id) %>%
  mutate(across(c(country, city), str_to_title)) %>%
  shinyWidgets::prepare_choices(
    label = city,
    value = city_id,
    group_by = country
  )

# cities <- maps::world.cities %>%
#   filter(pop > 100000) %>%
#   # dplyr::mutate(iso2 = countrycode::countrycode(country.etc, "country.name", "iso2c")) %>%
#   # tidyr::unite(label, name, iso2, sep = ", ", na.rm = TRUE, remove = FALSE) %>%
#   dplyr::arrange(country.etc, name) %>% 
#   shinyWidgets::prepare_choices(
#     label = name,
#     value = name,
#     group_by = country.etc
#   )

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

  nav_panel(
    "AMEX Analysis",
    "placeholder"
  ),

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

server <- function(input, output, session) {

  df_origin <- mod_origin_input_server("origin", cities)
  # df_origin <- reactive({
  #   df_input()
  # }) %>% bindEvent(input$go, ignoreInit = TRUE)

  mod_meeting_place_server(
    id = "mp",
    mat, 
    air_msf,
    df_conversion,
    df_origin,
    msf_only = TRUE
  )
}

shinyApp(ui, server)