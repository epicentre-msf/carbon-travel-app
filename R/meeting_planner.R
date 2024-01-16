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
#conversion_df <- import("data/conversion_factors_original.xlsx")

conversion_df <- data.frame(
  distance = c("0-1000 km", "1000-3500 km", "+ 3500 km"), 
  distance_cat = c("short", "medium", "long"),
  co2e = c(0.25858, 0.18746, 0.15196)
)

# Create a fake dataset of origins of participants
ori_df  <- data.frame(
  origin = c(
    "paris",
    "london",
    "madrid", 
    "oslo"),
  country = c("France", "United Kingdom", "Spain", "Norway"), 
  n_participant = c(1, 3, 5, 7)
)

#Try with one city 
#get_dest_tot(ori_df, "london", conversion_df)

#define the list of destination possible 

only_msf <- TRUE
only_capital <- TRUE

if(only_msf){
  
  dest_list <- air_sub %>% filter(msf)
  
} else {
  
  dest_list <- air_sub
  
}

if(only_capital){
  
  dest_list <- dest_list %>% filter(capital)
  
}

dest_list <- dest_list %>% pull(city)

# map this on all the possible destination
# this is quite long
all_dist <- purrr::map(dest_list, ~ get_dest_tot(ori_df, .x, conversion_df)) %>%
  
  bind_rows() %>% 
  
  distinct(destination, .keep_all = TRUE)

# format to arrange in decreasing order, and km 
all_dist <- all_dist %>% 
  
  arrange(grand_tot_emission) %>% 
  
  mutate(rank = row_number()
  ) %>%
  
  relocate(rank, 1) %>% 
  
  left_join(., select(air_sub, city, msf), by = c("destination" = "city"))

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