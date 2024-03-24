# Global for Carbon-travel-App 
library(shiny)
library(bslib)
library(tidyverse)
library(gt)
library(highcharter)
source(here::here("R", "utils.R"))

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

# Get AMEX data 
df_amex <- read_rds(here::here("data", "amex", "amex_clean.rds"))

#date range 
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
