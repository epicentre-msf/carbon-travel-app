# ---------------------------
# Script name: meeting_planner.R
#
# Purpose of script: Main script : calculating distances from an origin df
#
# Author: Hugo Soubrier
#
# Date Created: 2024-01-12
#
# Copyright (c) Hugo SOUBRIER, 2024
# Email: soubrierhugo@gmail.com
# ---------------------------
# Notes:
#   
#
#
# ---------------------------

# Load packages ---------------------------

pacman::p_load(
  rio, # import funcs
  fs, # work with path
  here, # create relative paths
  janitor, # data cleaning
  airportr,
  gt, 
  tidyverse # data science
)


# Funcs -------------------------------------------------------------------

source("R/funcs.R")

# Import data -------------------------------------------------------------

# Get the distance matrix 

mat <- import("data/airports_distance_matrix.rds")

#get the air_sub 
air_sub <- import("data/airports_sub.rds")

#get the conversion df - taken from {footprint}
conversion_df <- import("data/conversion_factors_original.xlsx")

# #get the world cities
# cities  <- maps::world.cities %>% as_tibble()
# #get cities above 100 000 - subset for now for easier calculation
# cities_sub <- cities %>% filter(pop > 100000)

# Create a fake dataset of origins of participants
ori_df  <- data.frame(
  origin = c(
    "Paris",
    "London",
    "Brussels",
    "Geneva", 
    "Amsterdam",
    "Madrid",
    "Athens"
  ),
  
  n_participant = c(2, 2, 2, 2, 5, 2, 3)
)

# map this on all the possible destination
# this is quite long

all_dist <- purrr::map(air_sub$city, ~ get_dest_tot(ori_df, .x, conversion_df)) %>%
  
  bind_rows() %>% 
  
  distinct(destination, .keep_all = TRUE)

# format to arrange in decreasing order, and km 
all_dist <- all_dist %>% 
  
  arrange(grand_tot_km) %>% 
  
  mutate(rank = row_number()
  ) %>% relocate(rank, 1)

#format the output 

all_dist %>%
  
  head(., n = 20) %>% 
  
  epivis::epi_gtstyle() %>% 

  data_color(
    columns = grand_tot_emission,
    method = "numeric",
    palette = c("white", "orange"),
    reverse = FALSE
  ) %>% 
  
  cols_label(grand_tot_km ~ "Total distance (km)", 
             grand_tot_emission ~ "Total CO2 emissions (CO2e)") %>% 
  
  tab_footnote(location = cells_column_labels("grand_tot_emission"), 
               "carbon dioxide equivalent with radiative forcing") %>% 
  
  tab_footnote(location = cells_column_labels("grand_tot_km"), 
               "Calculated using one way travel to destination")

#WHY is distance and emissions not always correlated ? 