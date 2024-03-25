# ---------------------------
# Script name: data_prep_amex.R
#
# Purpose of script: preparation for AMEX flights data
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

source(here::here("R", "set_paths.R"))
source(here::here("R", "utils.R"))

# Set paths -------------------------------------------------------------

paths <- set_paths()

sharepoint_path <- paths$sharepoint_path

raw_path <- fs::path(sharepoint_path, "Maelle CHARRIER - TOOL", "data", "raw")
clean_path <- fs::path(sharepoint_path, "Maelle CHARRIER - TOOL", "data", "clean")

#Import data ------------------------

#country_codes 
country_codes  <- import(fs::path(sharepoint_path, "Maelle CHARRIER - TOOL", "Data", "country_codes.xlsx")) %>% clean_names() %>% rename(mission_name = name) %>% as_tibble()

dir_ls <- fs::dir_ls(fs::path(raw_path, "amex-data"), regexp = "ALLMSF")
myfiles <- lapply(dir_ls, rio::import, skip = 5)

#bind all together
amex  <- myfiles %>% bind_rows() %>% as_tibble()

# Clean data ----------

amex_clean <- amex %>%
  clean_names() %>%
  rename(
    org = client_id_name,
    distance_km = flight_kilometers,
    distance_miles = flight_mileage,
    carrier = carrier_validating,
    class_service = class_of_service_invoice_predominant, 
    gross_amount = flight_gross_amount, 
    net_amount = flight_net_amount, 
    city_pairs = non_directional_city_pair, 
    dest_code = destination_city_code, 
    ori_code = origin_city_code, 
    dest = destination_city, 
    mission = customer_defined_03,
    ori = origin_city, 
    reason_travel = customer_defined_08
  ) %>% 
  
  # Deal with character variables - everything to lower
  mutate(across(where(~ is.character(.x)), ~ tolower(.x)),
         org = case_match(
           client_id,
           "msfg98" ~ "OCG",
           "msfgch" ~ "MSF International",
           "msfgnl" ~ "OCA",
           "msfg95" ~ "OCA",
           "msfga1" ~ "MSF - Austria",
           "msfg94" ~ "MSF - Germany",
           "msfg91" ~ "OCB",
           "msfg97" ~ "MSF - Sweden"
         ), 
         
         reason_travel = case_match(reason_travel, 
                                    "r01" ~ "Field project/briefing",
                                    "ro1"~ "Field project/briefing", 
                                    "r02" ~ "Training",
                                    "r03" ~ "Field project visit",
                                    "r04" ~ "MSF meeting",
                                    "r05" ~ "non-MSF meeting",
                                    "r06" ~ "Medivac",
                                    "r07" ~ "Visa run",
                                    "r08" ~ "MSF paid personal travel",
                                    "r11" ~ "Personal travel (subaccount)", 
                                    .default = NA
         ), 
         mission_id = str_to_upper(str_extract(mission, "[a-z]{2}"))
  )  %>% 
  # deal with numeric variables
  mutate(across(c(distance_miles, distance_km, gross_amount, net_amount), ~ as.numeric(.x))) %>%
  # clean dates
  mutate(invoice_date = ymd(invoice_date)) %>% 
  arrange(ticket_number, traveler_name) %>% 
  #id the flight that were refunded and remove them 
  mutate(.by = c(ticket_number, traveler_name, dest_code, ori_code), 
         refunded = sum(distance_km) == 0) %>% 
  filter(refunded == FALSE) %>% 
  #remove the remaining negative
  filter(distance_km >= 0) %>% 
  
  #filter out the non valid city code
  filter(str_detect(ori_code, "[a-z]")) %>% 
  
  arrange(ticket_number) %>% 
  
  select(-c(traveler_name, 
            ticket_number, 
            reason_code,
            contains("customer_defined"), 
            pnr
  )) 

# add country codes - THEY NEED TO BE CHECKED 
amex_clean <- left_join(amex_clean, country_codes, by = c("mission_id" = "country_code"))
amex_clean %>% count(mission_name)

# Calculate the distance between cities and compare with AMEX dist --------
# Amex uses terrestrial miles
# can't compare distance yet 

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
         ori = str_to_sentence(ori), 
         dest = str_to_sentence(dest)) 
# Export data -------------------------------------------------------------

export(amex_clean, fs::path(clean_path, "amex_clean.rds"))
