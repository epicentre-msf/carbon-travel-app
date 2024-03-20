# ---------------------------
# Script name: data_prep_amex.R
#
# Purpose of script: data preparation for AMEX flights
#
# Author: Hugo Soubrier
#
# Date Created: 2024-03-20
#
# Copyright (c) Hugo SOUBRIER, 2024
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
  tidyverse     # data science 
  
)

#Import data 

amex <- import(here::here("data", "amex", "Air Carbon Emissions-MSFCH_2023.xlsx"), skip = 5) %>% as_tibble()

# clean data 
amex_clean <- amex %>% 
  
  clean_names() %>% 
  
  #everything to lower
  mutate(across(everything(), ~ tolower(.x)), 
         org = case_match(client_id_name,  "medecins sans frontieres switz" ~ "OCG"), 
         across(c(flight_mileage, flight_kilometers), ~ as.numeric(.x))) %>% 
  
  #id the flight that were refunded and remove them 
  mutate(.by = c(ticket_number, traveler_name, destination_city_code, origin_city_code), 
         refunded = sum(flight_kilometers) == 0) %>% 
  
  filter(refunded == FALSE) %>% 
  
  #remove the remaining negative
  filter(flight_kilometers >= 0) %>% 
  
  #filter out the non valid city code
  filter(str_detect(origin_city_code, "[a-z]")) %>% 
  
  arrange(ticket_number) %>% 
  
  select(-c(traveler_name, 
            ticket_number, 
            customer_defined_03, 
            customer_defined_08, 
            customer_defined_10, 
            invoice_number, 
            pnr, 
            client_id_name, 
            origin_city_code, 
            destination_city_code
  )) %>% 
  
  rename(distance_miles = flight_mileage, 
         distance_km = flight_kilometers, 
         origin = origin_city, 
         destination = destination_city)

# Calculate the distance between cities and compare with AMEX dist --------
#Amex uses terrestrial miles
#can't compare distance yet 



# Calculate emissions for flights -----------------------------------------

amex_clean <- amex_clean %>% 
  mutate(distance_km_cat = case_when(distance_km < 1000 ~ "short", 
                                     between(distance_km, 1000, 3499) ~ "medium", 
                                     distance_km  > 3499 ~ "long"), 
         coe2_fct = case_when(distance_km_cat == "short" ~ 0.25858, 
                              distance_km_cat == "medium" ~ 0.18746, 
                              distance_km_cat == "long" ~ 0.15196
         ), 
         emission = round(distance_km * coe2_fct, 2),
         origin = str_to_sentence(origin), 
         destination = str_to_sentence(destination)) 

# Export data -------------------------------------------------------------

export(amex_clean, "data/amex/amex_clean.rds")
