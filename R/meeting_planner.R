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

mat <- import("data/distance-matrix/airports_distance_matrix.rds")

#get the air_msf data 
air_msf <- import("data/clean/air_msf.rds")

#get the conversion df - given by Maelle
conversion_df <- import("data/clean/conversion_df.rds")

# Create a fake dataset of origins of participants
ori_df  <- data.frame(
  origin = c(
    "paris",
    "london",
    "madrid", 
    "oslo"),
  country = c("france", 
              "united kingdom", 
              "spain", 
              "norway"), 
  n_participant = c(1, 3, 5, 7)
  
) %>% 
  
  mutate(origin_id = paste0(country, "-", origin))

# Define list of possible destinations ------------------------------------

# Parameters 

selected_dest <- NULL #c("france-paris", "netherlands-amsterdam")

only_msf <- TRUE

if(only_msf){
  
  msf_type_select <- c("MSF office", "MSF HQ OC")
}

# only_capital <- FALSE

if(is.null(selected_dest)){
  
  if(only_msf){
    
    air_msf <- air_msf %>% filter(msf_type %in% msf_type_select)
    
  }
  
  # if(only_capital){
  #   
  #   air_msf <- air_msf %>% filter(capital == 1)
  #   
  # }
  
  dest_list <- unique(air_msf %>% pull(city_id))
  
} else {
  
  dest_list <- selected_dest
  
}

#Try with one city - MISSING UK
# get_dest_tot(ori_df,
#              "united kingdom-london",
#              conversion_df)

# map this on all the possible destination
# this is quite long

# filter matrix to keep only origin 
# filter matrix to keep only destinations 

all_dist <- purrr::map(dest_list, ~ get_dest_tot(ori_df, .x, conversion_df)) %>%
  
  bind_rows() %>% 
  
  distinct(destination, .keep_all = TRUE)

# format to arrange in decreasing order, and km 
all_dist <- all_dist %>% 
  
  arrange(grand_tot_emission) %>% 
  
  mutate(rank = row_number()
         
  ) %>%
  
  relocate(rank, 1) %>% 
  
  left_join(., select(air_msf,
                      oc,
                      msf_type,
                      city_id), by = c("destination" = "city_id")) %>% 
  
  separate(destination, c("Country", "City"), "-")

#format the output 
all_dist %>%
  
  head(., n = 5) %>% 
  
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
