# ---------------------------
# Purpose of script:single travel estimator
#
# Author: Hugo Soubrier
#
# Date Created: 2024-01-12
# Email: hugo.soubrier@epicentre.msf.org
# ---------------------------
# Notes:
#
#
# ---------------------------

# Load packages ---------------------------

pacman::p_load(
  rio, # import funcs
  here, # create relative paths
  janitor, # data cleaning
  sf, 
  sfnetworks,
  tidyverse # data science
)


source(here::here("R", "set_paths.R"))
source(here::here("R", "utils.R"))

# Import data -------------------------------------------------------------

net <- readRDS(here::here("data","clean", "amex_network.rds"))

mat <- readRDS(here::here("data","clean", "distance_matrix.rds"))

dest <- readRDS(here::here("data","clean", "dest_cities.rds"))

msf <- readRDS(here::here("data","clean", "unique_msf_clean.rds"))

conversion_df <- readRDS(here::here("data", "clean", "conversion_df.rds"))

# Fake dataset of stop overs 
ori_df <- data.frame(
  origin_id = c("GVA", "DKR", "OUA"),
  n_participant = c(1, 1, 1)
)

# build the segments 
df <- data.frame(
  start_var = head(ori_df$origin_id, -1), 
  end_var = tail(ori_df$origin_id, - 1)
) |> 
  mutate(distance_km = purrr::map2_dbl(start_var, end_var, ~ unname(mat[.x, .y] ) ), 
         distance_cat = case_when(
           distance_km <= 999 ~ "short",
           distance_km >= 3500 ~ "long",
           .default = "medium"
         ) ) |>
  left_join(
    conversion_df |> select(distance_cat, emissions_factor = co2e),
    by = "distance_cat"
  ) |>
  mutate(
    trip_emissions = round(digits = 3, distance_km * emissions_factor)
  ) |> 
  left_join(select(dest, start_city = city_name, start_country = country_name), by = join_by(start_var == city_code)) |> 
  left_join(select(dest, end_city = city_name, end_country = country_name), by = join_by(end_var == city_code)) |> 
  
  select(start_city, start_country, end_city, end_country, distance_km, trip_emissions) 
