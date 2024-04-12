# Global for Carbon-travel-App 
library(shiny)
library(bslib)
library(tidyverse)
library(gt)
library(highcharter)
library(reactable)
library(leaflet)
source(here::here("R", "set_paths.R"))
source(here::here("R", "utils_labels.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "utils_hc.R"))

# Set paths -------------------------------------------------------------

paths <- set_paths()

# Path to clean data 
if (Sys.info()[["user"]] == "paul") {
  path_to_data <- fs::path(Sys.getenv("SHAREPOINT_PATH"), "Maelle CHARRIER - Carbon-travel-App", "Data")
} else {
  path_to_data <- fs::path(paths$maelle_charrier_tool, "Data")
}


# Setup -------------------------------------------------------------------
app_name <- "carbon_app"
app_title <- "Carbon Travel App"
app_font <- "Alegreya Sans"
p_font <- "Merriweather"

# Import data -------------------------------------------------------------
# Get the distance matrix from the project not sharepoint ! too big
mat <- read_rds(here::here("data", "distance-matrix", "airports_distance_matrix.rds"))

# get the air_msf data
air_msf <- read_rds(here::here(path_to_data, "clean", "air_msf.rds"))

# get the conversion df - given by Maelle
df_conversion <- read_rds(here::here(path_to_data, "clean", "conversion_df.rds"))

cities <- air_msf %>%
  arrange(city_id) %>%
  mutate(across(c(country, city), str_to_title)) %>%
  shinyWidgets::prepare_choices(
    label = city,
    value = city_id,
    group_by = country
  )

# Get AMEX data
df_amex <- read_rds(here::here(path_to_data, "clean", "amex_clean_lon_lat.rds")) %>%
  mutate(emission = emission / 1000)

#date range 
init_year <- unique(df_amex$year)
init_date_range <- format(seq.Date(min(df_amex$invoice_date), max(df_amex$invoice_date), by = "month"), "%Y-%m")
min_date <- min(init_date_range)
max_date <- max(init_date_range)

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

# local disk cache
shiny::shinyOptions(cache = cachem::cache_disk(here::here(".cache")))

disconnected <- sever::sever_default(
  title = "Disconnected",
  subtitle = "Sorry your session timed-out or something went wrong",
  button = "Reconnect",
  button_class = "info"
)
