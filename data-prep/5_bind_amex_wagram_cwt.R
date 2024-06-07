# Joigning amex and Wagram together 

pacman::p_load(
  rio, # import funcs
  fs, # work with path
  here, # create relative paths
  janitor, # data cleaning
  lubridate, # date handling
  tidyverse # data science
)
source(here::here("R", "utils.R"))

# Set paths -------------------------------------------------------------
clean_path <- here::here("data", "clean")

# Import clean data 

amex <- readRDS(fs::path(clean_path, "amex_clean_lon_lat.rds")) |> mutate(data_source = "Amex")
wagram <- readRDS(fs::path(clean_path, "wagram_clean_lon_lat.rds")) |> mutate(data_source = "Wagram")
cwt <- readRDS(fs::path(clean_path, "cwt_clean_lon_lat.rds")) |> mutate(data_source = "CWT")

# select only important variables 
amex <- amex |>
  select(org, 
         traveler_name, 
         ori_city_code, 
         ori_city_name, 
         ori_city_lon, 
         ori_city_lat, 
         ori_country_code, 
         ori_country_name, 
         dest_city_code, 
         dest_city_name, 
         dest_city_lon, 
         dest_city_lat, 
         dest_country_code, 
         dest_country_name, 
         invoice_date, 
         month, 
         quarter, 
         year,
         reason_travel, 
         hq_flying_mission, 
         mission_country_name, 
         mission_country_iso, 
         gross_amount, 
         distance_km, 
         distance_km_cat, 
         distance_miles, 
         coe2_fct, 
         emission,
         data_source) 

wagram <- wagram |> 
  select(org, 
         traveler_name, 
         ori_city_code, 
         ori_city_name, 
         ori_city_lon, 
         ori_city_lat, 
         ori_country_code, 
         ori_country_name, 
         dest_city_code, 
         dest_city_name, 
         dest_city_lon, 
         dest_city_lat, 
         dest_country_code, 
         dest_country_name, 
         invoice_date, 
         month, 
         quarter, 
         year, 
         gross_amount,
         #reason_travel, 
         #hq_flying_mission, 
         mission_country_name, 
         mission_country_iso, 
         distance_km, 
         distance_km_cat, 
         distance_miles, 
         coe2_fct, 
         emission,
         data_source
  )

cwt <- cwt |> 
  select(org, 
         traveler_name, 
         ori_city_code, 
         ori_city_name, 
         ori_city_lon, 
         ori_city_lat, 
         ori_country_code, 
         ori_country_name, 
         dest_city_code, 
         dest_city_name, 
         dest_city_lon, 
         dest_city_lat, 
         dest_country_code, 
         dest_country_name, 
         invoice_date, 
         month, 
         quarter, 
         year, 
         gross_amount,
         reason_travel, 
         #hq_flying_mission, 
         #mission_country_name, 
         #mission_country_iso, 
         distance_km, 
         distance_km_cat, 
         distance_miles, 
         coe2_fct, 
         emission,
         data_source)

# Bind rows 
full_df <- bind_rows(amex, wagram, cwt)

# cleaning before saving 

full_df <- full_df |> mutate(
  hq_flying_mission = case_match(hq_flying_mission, NA ~ "Unknown", "hq" ~ "HQ", .default = str_to_title(hq_flying_mission)), 
  hq_flying_mission = fct_relevel(hq_flying_mission, c("Mission", "Flying", "HQ", "Other", "Unknown")), 
  reason_travel = case_match(reason_travel, NA ~ "Unknown", "Missing" ~ "Unknown", .default = reason_travel), 
  reason_travel = fct_relevel(reason_travel, c("Field project visit", 
                                               "Field project briefing", 
                                               "MSF meeting", 
                                               "non-MSF meeting",
                                               "Training",
                                               "Medivac", 
                                               "Visa run",
                                               "MSF paid personal travel", 
                                               "Personal travel (subaccount)", 
                                               "Unknown") )
)

write_rds(full_df, fs::path(clean_path, "full_amex_wagram_cwt.rds"))