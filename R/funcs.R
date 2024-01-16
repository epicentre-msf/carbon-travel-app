# ---------------------------
# Script name: funcs.R
#
# Purpose of script: all functions related to meeting planer
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

#function to 
# 1) calculate the distance and total distance using N participant for a specific destination
# 2) sums the grand distance total for a destination

get_dest_tot <- function(origin_df, 
                         destination, 
                         conversion_df){ 
  
  #function to retrieve the distance from two cities 
  get_dist <- function(origin, destination){
    
    mat[origin, destination]
    
  }
  
  #vectorize the function
  get_dist <- Vectorize(get_dist)
  
  #function to filter the emission df 
  get_factor <- function(distance_type){
    
    filter(conversion_df, distance_cat == distance_type)$co2e
    
  }
  
  get_factor <- Vectorize(get_factor)
  
  df_details <- origin_df %>% 
    
    mutate( 
      destination = destination, 
      distance = get_dist(destination, origin), 
      distance_km = distance/1000, 
      total_distance_km = n_participant * distance_km,
      
      distance_type = case_when(distance_km <= 1000 ~ "short",
                                distance_km >= 3500 ~ "long", 
                                .default = "medium"), 
      emissions_factor = get_factor(distance_type), 
      trip_emissions = round(digits = 3, distance_km * emissions_factor), 
      total_emissions = n_participant * trip_emissions) 
  
  grand_df <- df_details %>% 
  
  group_by(destination) %>% 
  
  summarise(grand_tot_km = sum(total_distance_km), 
            grand_tot_emission = sum(total_emissions)
            )

  # output <- list(summary = df_details, 
  #                grand_total = grand_df) 

  return(grand_df)
  
}
