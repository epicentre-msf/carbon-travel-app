# ---------------------------
# Script name: prepare_data.R
#
# Purpose of script: Prepare the destination database
#
# Author: Hugo Soubrier
#
# Date Created: 2024-01-18
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
  
  rio,          # import funcs
  fs,           # work with path
  here,         # create relative paths
  janitor,      # data cleaning
  lubridate,    # date handling
  airportr,
  hmatch,
  tidyverse     # data science 
  
)

# Cities data  ------------------------------------------------------------
cities_clean <- maps::world.cities %>% 
  
  as_tibble() %>% 
  
  clean_names() %>% 
  
  rename(city = name, 
         country = country_etc) %>% 
  
  mutate(city = tolower(city), 
         city = str_remove(city, "'"), 
         city = str_squish(city), 
         country = str_to_lower(country), 
         capital = as.logical(capital), 
         country = case_when(country == "uk" ~ "united kingdom", 
                             country == "usa" ~ "united states of america", 
                             country == "congo" ~ "republic of congo",
                             country == "congo democratic republic" ~ "democratic republic of congo",
                             .default = country)
         
  ) %>% 
  
  select(country, city, capital)

# get capital cities
capital <- cities_clean %>% filter(capital == 1) %>% select(city, country)

# Airports data -----------------------------------------------------------
#prepare the airports data 
air_clean <- airports %>% 
  
  clean_names() %>% 
  
  filter(!is.na(country), 
         !is.na(city)) %>% 
  
  mutate(city = tolower(str_squish(city)), 
         country = tolower(str_squish(country)), 
         country = case_when(country == "congo (brazaville)" ~ "republic of congo", 
                             country == "congo (kinshasa)" ~ "democratic republic of congo",
                             country == "united states" ~ "united states of america",
                             
                             .default = country), 
         iso3 = countrycode::countrycode(country, "country.name", "iso3c"), 
         capital = city %in% capital$city)  %>% 
  
  # keep the airports with a valid IATA code
  # also some airports have no city name (39)
  filter(iata != "\\N", 
         !is.na(city)) %>% 
  
  select(country, iso3, city, name, latitude, longitude, iata, icao, capital)

# Add all airports cities ---------------------------------------------------

#For cities with multiple airports: take the mean lat/long of all airports
air_unique <- air_clean %>% 
  
  group_by(country, iso3, city) %>% 
  
  summarise(n_airports = n(), 
            iata = paste0(iata, collapse = ", "), 
            mean_latitude = mean(latitude), 
            mean_longitude = mean(longitude), 
            .groups = "drop") %>% 
  
  ungroup %>% 
  
  select(country, iso3, city, mean_latitude,  mean_longitude)

export(air_unique, "data/clean/air_unique.rds")

# MSF data ----------------------------------------------------------------

#load MSF project data from the GIS unit
msf_proj <- import("data/raw/msf_presence_layer.xlsx") %>% as_tibble() %>% clean_names()

#try to geo code the missing cities from lat/long ?
msf_raw <- msf_proj %>%
  
  rename(city = locality, 
         msf_type = type_2) %>% 
  
  #filter(!is.na(country), !is.na(city)) %>% 
  
  filter(!str_detect(city, "\\?")) %>% 
  
  mutate(across(c(country, city), ~ str_squish(str_to_lower(.x))), 
         iso3 = countrycode::countrycode(country, "country.name", "iso3c")) %>% 
  
  relocate(iso3, .after = country)

# Match the airport cities to MSF cities  ---------------------------------

#using the airport data as the reference 
#unique cities in airport data 
air_city <- air_unique %>% distinct(iso3, city)

#all the match rows
msf_clean <- hmatch_composite(msf_raw, 
                              air_city, 
                              by = c("iso3", "city"), 
                              fuzzy = TRUE) %>% 
  
  mutate(iso3 = coalesce(ref_iso3, iso3), 
         city = coalesce(ref_city, city)) %>% 
  
  select(!contains("ref_")) %>% 
  
  relocate(c(country, iso3, city), 1) %>% 
  
  arrange(country, city)

export(msf_clean, "data/clean/full_msf_clean.xlsx")

#keep one row per city
msf_unique <- msf_clean %>% 
  
  group_by(country, city) %>% 
  
  summarise(n_msf_oc = paste0(unique(oc), collapse = ', '), 
            
            oc = paste0(unique(oc), collapse = ", "),
            
            msf_type = paste0(unique(msf_type), collapse = ", "), 
            
            .groups = "drop"
  ) %>% 
  
  ungroup %>% 
  
  select(country, 
         city,
         oc,
         msf_type) %>% 
  
  filter(!is.na(city)) 

export(msf_unique, "data/clean/unique_msf_clean.rds")

# Join airport and MSF data -----------------------------------------------

air_msf <- air_unique %>% 
  
  left_join(msf_unique) %>% 
  
  mutate(msf = !is.na(msf_type), 
         
         #required to deal with cities that have the same name in different countries 
         city_id = paste0(country, "-", city)
         
         )

export(air_msf, "data/clean/air_msf.rds")

# COnversion factors (given by Maelle) ------------------------------------

conversion_df <- data.frame(
  distance = c("0-999 km", "1000-3499 km", "+ 3500 km"), 
  distance_cat = c("short", "medium", "long"),
  co2e = c(0.25858, 0.18746, 0.15196)
)

export(conversion_df, "data/clean/conversion_df.rds")
