# Data prep for CWT 

# Load packages ---------------------------

pacman::p_load(
  rio, # import funcs
  fs, # work with path
  here, # create relative paths
  janitor, # data cleaning
  lubridate, # date handling
  tidyverse, # data science
  spatialrisk
)

source(here::here("R", "set_paths.R"))
source(here::here("R", "utils.R"))

# Set paths -------------------------------------------------------------

paths <- set_paths()

sharepoint_path <- paths$sharepoint_path
data_path <- get_sp_data_path()
raw_path <- fs::path(data_path, "raw")
clean_path <- here::here("data", "clean")

#import amex 
amex <- readRDS(here::here(clean_path, "amex_clean_lon_lat.rds"))

# Import data  -------------------------------------------------------------

if(fetch_data_offline){
  
  # OFFLINE
  path_offline <- here::here("data", "raw")
  raw_path <- path_offline
  
}

#Import CWT data 

cwt_raw <- rio::import(fs::path(raw_path, "cwt-data", "MSFOCBA complete report 2021 & 2022.xlsx"), skip = 2 ) |> as_tibble() |> clean_names()

# Map variables  ----------------------------------------------------------
#remove all frais and trains 

cwt_raw |> names()

cwt_clean <- cwt_raw |> 
  
  filter(travel_type == "AIR") |> 
  
  select(traveler_name, 
         invoice_number, 
         invoice_date = issue_date, 
         ori = origin_city_name, 
         dest = destination_city_name, 
         gross_amount = paid_fare, 
         reason_travel = client_defined_field_3
  ) |> 
  
  mutate(org = "OCBA") |> 
  
  relocate(org, .before = 1) |> 
  
  mutate(across(c(gross_amount), ~ as.numeric(.x)), 
         across(c(traveler_name, ori, dest), ~ tolower(.x)), 
         invoice_date = ymd(invoice_date), 
         month = floor_date(invoice_date, "month"),
         month = format(month, "%Y-%m"),
         quarter = lubridate::quarter(invoice_date, with_year = TRUE),
         quarter = str_replace(quarter, "\\.", "-Q"),
         year = floor_date(invoice_date, "year"),
         year = format(year, "%Y"), 
         
         reason_travel = tolower(reason_travel), 
         reason_travel = case_when(reason_travel %in% c("briefing travel", "debriefing travel" ) ~ "Field project briefing",
                                   reason_travel %in% c("hq visit to mission", "incorpor to position", "evacuation", "missiontravel-depart", "missiontravel-return") ~ "Field project visit",
                                   reason_travel %in% c("coordination week", "travel to hq", "visit/meetingotherof", "internationalmeeting", "internat. workshop", "visit/meeting hq") ~ "MSF meeting", 
                                   reason_travel %in% c("other visit/training", "training") ~ "Training",
                                   reason_travel == "visa management" ~ "Visa run",
                                   reason_travel == "holidays interstaff" ~ "MSF paid personal travel", 
                                   is.na(reason_travel) ~ "Missing", 
                                   reason_travel %in% c("xxxxxxxxxxxxxxxxxxxx", "m") ~ "Missing",
                                   .default = reason_travel)
         
  ) |> 
  
  relocate(c(month, quarter, year), .after = invoice_date) |> 
  
  #remove all the negative amounts - NEED to check that there is no duplicates
  filter(gross_amount > 0)

# Geocoding ---------------------------------------------------------------

#get raw name to match
raw_df <- data.frame(name = unique(c(cwt_clean$ori, cwt_clean$dest) )) |> as_tibble()

# get ref names to match 
ref_df <- readRDS(here::here(clean_path, "df_cities.rds"))

match_df <- hmatch::hmatch_composite(
  raw_df, 
  ref_df, 
  fuzzy = TRUE, 
  fuzzy_method = "osa",
  fuzzy_dist = 2L, 
  by = "name",
  by_ref = "city_name"
)

#extract the unmatched
unmatch <- match_df |> filter(is.na(city_name)) |> select(name, city_code)

if (!file.exists(fs::path("data-prep", "output", "unmatched_city_cwt.xlsx"))) {
  
  # save as excel to mnaually create a dict
  qxl::qxl(
    unmatch,
    file = fs::path("data-prep", "output", "unmatched_city_cwt.xlsx")
  ) 
}

# import excel file and match with dict
man_df <- import(fs::path("data-prep", "output", "unmatched_city_cwt.xlsx"))

match_df <- hmatch::hmatch_composite(
  raw_df, 
  ref_df, 
  fuzzy = TRUE, 
  fuzzy_method = "osa",
  fuzzy_dist = 2L, 
  man = man_df, 
  code_col = "city_code", 
  by = "name",
  by_ref = "city_name"
)

cwt_city <- cwt_clean |> 
  left_join(select(match_df,
                   ori = name, 
                   city_code,
                   city_name, 
                   city_lon = lon, 
                   city_lat = lat, 
                   country_name,
                   country_code) |> 
              rename_with(.cols = contains("_"), .fn = ~ paste0("ori_", .x)) |> distinct(ori, .keep_all = TRUE)
  ) |> 
  
  left_join(select(match_df,
                   dest = name, 
                   city_code,
                   city_name, 
                   city_lon = lon, 
                   city_lat = lat, 
                   country_name,
                   country_code) |> rename_with(.cols = contains("_"), .fn = ~ paste0("dest_", .x)) |> distinct(dest, .keep_all = TRUE)
  ) |> 
  select(-ori, -dest)

# Calculate distances  ----------------------------------------------------

cwt_clean_lon_lat <- cwt_city  |> 
  
  mutate(distance_km = round(digits = 2, spatialrisk::haversine(ori_city_lat, ori_city_lon, dest_city_lat, dest_city_lon)/1000 ), 
         distance_miles = distance_km / 1.609 , 
         distance_km_cat = case_when(
           distance_km < 1000 ~ "short",
           between(distance_km, 1000, 3499) ~ "medium",
           distance_km > 3499 ~ "long"
         ),
         coe2_fct = case_when(
           distance_km_cat == "short" ~ 0.25858,
           distance_km_cat == "medium" ~ 0.18746,
           distance_km_cat == "long" ~ 0.15196
         ),
         emission = round((distance_km * coe2_fct) / 1000, 2)
  )

write_rds(cwt_clean_lon_lat, fs::path(clean_path, "cwt_clean_lon_lat.rds"))
