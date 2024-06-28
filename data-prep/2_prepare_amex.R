# ---------------------------
# Script name: data_prep_amex.R
#
# Purpose of script: preparation for AMEX flights data
#
# Author: Hugo Soubrier
#
# Date Created: 2024-03-20
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
  lubridate, # date handling
  tidyverse # data science
)

source(here::here("R", "set_paths.R"))
source(here::here("R", "utils.R"))

# Set paths -------------------------------------------------------------

paths <- set_paths()

sharepoint_path <- paths$sharepoint_path
data_path <- get_sp_data_path()
raw_path <- fs::path(data_path, "raw")
clean_path <- here::here("data", "clean")

# Import data  -------------------------------------------------------------

fetch_data_offline <- FALSE

if(fetch_data_offline){
  
  # OFFLINE
  path_offline <- here::here("data", "raw")
  raw_path <- path_offline
  
}

# LOAD AIR
air_dir_ls <- fs::dir_ls(fs::path(raw_path, "amex-data"), regexp = "AirEmissions")
air_files <- lapply(air_dir_ls, rio::import, skip = 5, guess_max = 10000000)

air_files <- purrr::map(air_files, ~ .x |>
                          as_tibble() |>
                          clean_names())

# bind all together
air_amex <- air_files |>
  bind_rows() |>
  as_tibble()

#rename and select appropriate variable + remove refunds 
# ISSUE WITH DATES HERE
air_amex <-  air_amex |> 
  
  select(org = client_id, 
         ticket_number,
         traveler_name,
         invoice_date, 
         distance_km = flight_kilometers,
         distance_miles = flight_mileage,
         reason_travel = customer_defined_08, 
         gross_amount = flight_gross_amount,
         net_amount = flight_net_amount,
         dest_code = destination_city_code,
         dest = destination_city,
         ori_code = origin_city_code,
         ori = origin_city, 
         code = customer_defined_03, 
         carrier = carrier_validating
         
  ) |> 
  
  # id the flight that were refunded and remove them
  mutate(
    .by = c(ticket_number, dest_code, ori_code),
    refunded = sum(distance_km) == 0
  ) |> 
  filter(refunded == FALSE ) |> 
  #filter out the rails here 
  filter(!str_detect(carrier, "rail")) |>
  select(-c(ticket_number, refunded, carrier)) |> 
  mutate(travel_type = "air")

# LOAD TRAIN
rail_dir_ls <- fs::dir_ls(fs::path(raw_path, "amex-data"), regexp = "RailEmissions")
rail_files <- lapply(rail_dir_ls, rio::import, skip = 5, guess_max = 10000000)

names(rail_files) <- c("amex_2019", "amex_2020", "amex_2021", "amex_2022", "amex_2023", "amex_2024")

rail_files <- purrr::map(rail_files, ~ .x |>
                           as_tibble() |>
                           clean_names())

# bind all together
rail_amex <- rail_files |>
  bind_rows() |>
  as_tibble()

#rename and select appropriate variable + remove refunds 

rail_amex <- rail_amex  |> 
  
  select(org = client_id, 
         ticket_number,
         traveler_name,
         invoice_date, 
         distance_km = flight_kilometers,
         distance_miles = flight_mileage,
         reason_travel = customer_defined_08, 
         gross_amount = flight_gross_amount,
         net_amount = flight_net_amount,
         dest_code = destination_city_code,
         dest = destination_city,
         ori_code = origin_city_code,
         ori = origin_city, 
         code = customer_defined_03,
         reason_code_description
         
  ) |> 
  
  # id the flight that were refunded and remove them
  mutate(
    .by = c(ticket_number, traveler_name, dest_code, ori_code),
    refunded = sum(distance_km) == 0
  ) |> 
  
  filter(refunded == FALSE, 
         is.na(reason_code_description) ) |> 
  
  select(-c(ticket_number, refunded, reason_code_description)) |> 
  
  mutate(travel_type = "rail")

# Bind AIR and RAIL -------------------------------------------------------

amex <- bind_rows(air_amex, rail_amex)

# Clean AIR and RAIL data ----------

# what is MSFG96 - MEDECINS SANS FRONTIERES ? 

amex_clean <- amex |>
  # Deal with character variables - everything to lower
  mutate(
    across(where(~ is.character(.x)), ~ tolower(.x)),
    org = case_match(
      org,
      "msfg90" ~ "MSF - UK",
      "msfg91" ~ "OCB",
      "msfg94" ~ "MSF - Germany",
      "msfgnl" ~ "OCA",
      "msfg95" ~ "OCA",
      "msfg98" ~ "OCG",
      "msfgch" ~ "MSF International",
      "msfga1" ~ "MSF - Austria",
      "msfg97" ~ "MSF - Sweden"
    ),
    reason_travel = case_match(
      reason_travel,
      "r01" ~ "Field project briefing",
      "ro1" ~ "Field project briefing",
      "r02" ~ "Training",
      "r03" ~ "Field project visit",
      "r04" ~ "MSF meeting",
      "r05" ~ "non-MSF meeting",
      "r06" ~ "Medivac",
      "r07" ~ "Visa run",
      "r08" ~ "MSF paid personal travel",
      "r11" ~ "Personal travel (subaccount)",
      .default = NA
    )
  ) |>
  # deal with numeric variables
  mutate(across(c(distance_miles, distance_km, gross_amount, net_amount), ~ as.numeric(.x))) |>
  # clean dates
  mutate(
    invoice_date = ymd(invoice_date),
    month = floor_date(invoice_date, "month"),
    month = format(month, "%Y-%m"),
    quarter = lubridate::quarter(invoice_date, with_year = TRUE),
    quarter = str_replace(quarter, "\\.", "-Q"),
    year = floor_date(invoice_date, "year"),
    year = format(year, "%Y")
  ) |>
  # fix NAs
  mutate(across(c(reason_travel), ~ if_else(is.na(.x), "Missing", .x)), 
         traveler_name = tolower(traveler_name)) |>
  relocate(c(month, quarter, year), .after = invoice_date) |>
  # remove the remaining negative
  filter(distance_km >= 0) |>
  # filter out the non valid city code
  filter(str_detect(ori_code, "[a-z]")) |>
  arrange(invoice_date)

# Clean Mission code

# using project codes from OCG 
project <- import(here::here(raw_path, "project_codes", "OCG_FormatedProjectCode.xlsx")) |>
  as_tibble() |>
  clean_names()

project_clean <- project |>
  mutate(
    number = str_to_lower(str_squish(number)),
    country_code = str_to_lower(str_squish(country_code)),
    country_code = if_else(country_name == "International" | country_name == "Switzerland", NA, country_code),
    country_name = if_else(country_name == "International" | country_name == "Switzerland", NA, country_name),
    hq_flying_mission = str_to_lower(hq_flying_mission),
    hq_flying_mission = case_match(
      hq_flying_mission,
      "access" ~ "other",
      "autres sections" ~ "other",
      "flying & flex" ~ "flying",
      "io" ~ "other",
      "missions" ~ "mission",
      .default = hq_flying_mission
    ), 
    hq_flying_mission = if_else(number == "zz999", "other", hq_flying_mission)
  )

# add iso codes to country
country_codes <- distinct(select(
  project_clean,
  mission_country_code = country_code,
  mission_country_name = country_name
)) |>
  mutate(mission_country_iso = countrycode::countrycode(mission_country_name, "country.name", "iso3c"))

project_sub <- project_clean |>
  select(
    code = number,
    mission_country_code = country_code,
    hq_flying_mission
  )

amex_clean <- amex_clean |>
  mutate(
    code = str_squish(code),
    
    # flag all normal code
    normal_flag = str_detect(code, "^[a-z]{2}[0-9]{3}$")
  ) |> 
  
  # left join the project codes
  left_join(project_sub, by = join_by(code)) |>
  
  # fix all the missions that are not yet in the project file
  mutate(
    new_mission = is.na(hq_flying_mission) & !is.na(code) & normal_flag,
    mission_country_code = if_else(new_mission, str_extract(code, "^[a-z]{2}"), mission_country_code),
    hq_flying_mission = if_else(new_mission, "mission", hq_flying_mission),
    
    # all ZZ go to switzerland (ch)
    mission_country_code = if_else(code == "zz999", "ch", mission_country_code)
  ) |>
  # add the country name
  left_join(country_codes, by = join_by(mission_country_code)) |>
  select(-c(normal_flag, mission_country_code))

# Calculate the distance between cities and compare with AMEX dist --------
# Amex uses terrestrial miles
# can't compare distance yet

# Calculate emissions for travel -----------------------------------------

amex_clean <- amex_clean |>
  mutate(
    distance_km_cat = case_when(
      distance_km < 1000 ~ "short",
      between(distance_km, 1000, 3499) ~ "medium",
      distance_km > 3499 ~ "long"
    ),
    coe2_fct = case_when(
      travel_type == "air" & distance_km_cat == "short" ~ 0.25858,
      travel_type == "air" & distance_km_cat == "medium" ~ 0.18746,
      travel_type == "air" & distance_km_cat == "long" ~ 0.15196, 
      
      travel_type == "rail" ~ 0.15,
      
      
    ),
    emission = round((distance_km * coe2_fct) / 1000, 2),
    ori = str_to_sentence(ori),
    dest = str_to_sentence(dest)
  )

# Add city codes ----------------------------------------------------------
# import airports and cities data
df_airports <- readRDS(here::here(clean_path, "df_airports.rds"))
df_cities <- readRDS(here::here(clean_path, "df_cities.rds"))

df_airports <- df_airports |> left_join(df_cities |>
                                          distinct(city_code, .keep_all = TRUE) |>
                                          select(
                                            city_code,
                                            city_lon = lon,
                                            city_lat = lat
                                          ))
# rename codes for matching
# Blaise Diagne International Airport is in data but doesn't match Dakar as a city -
# we force it to DKR iata code to match DKR city

amex_clean <- amex_clean |>
  rename(
    raw_ori_iata_code = ori_code,
    raw_dest_iata_code = dest_code
  ) |>
  mutate(raw_ori_iata_code = case_match(
    raw_ori_iata_code,
    "dss" ~ "dkr",
    .default = raw_ori_iata_code
  )) |>
  select(-c(ori, dest))

df_amex_codes <- tibble(iata_code = c(
  amex_clean$raw_ori_iata_code,
  amex_clean$raw_dest_iata_code
)) |>
  mutate(iata_code = str_to_upper(str_trim(iata_code))) |>
  distinct()

df_amex_codes_matched_1 <- df_amex_codes |>
  inner_join(
    df_airports,
    by = join_by(iata_code)
  ) |>
  select(
    iata_code,
    city_code,
    city_name,
    city_lon,
    city_lat,
    country_name,
    country_code = country_code3,
    lon,
    lat
  )

df_amex_codes_matched_2 <- df_amex_codes |>
  anti_join(df_airports, by = join_by(iata_code == iata_code)) |>
  inner_join(df_cities, by = join_by(iata_code == city_code)) |>
  mutate(
    city_code = iata_code,
    .after = iata_code
  ) |>
  left_join(df_cities |> select(city_code, city_lon = lon, city_lat = lat), by = join_by(city_code))

df_amex_codes_matched <- bind_rows(
  df_amex_codes_matched_1,
  df_amex_codes_matched_2
) |>
  distinct(iata_code, .keep_all = TRUE)

# Join the matched codes to the data

df_amex_clean_lon_lat <- amex_clean |>
  mutate(across(ends_with("_code"), ~ str_to_upper(str_trim(.x)))) |>
  left_join(
    df_amex_codes_matched |> select(
      iata_code,
      ref_ori_city_code = city_code,
      ref_ori_city_name = city_name,
      ref_ori_city_lon = city_lon,
      ref_ori_city_lat = city_lat,
      ref_ori_lon = lon,
      ref_ori_lat = lat,
      ref_ori_country_name = country_name,
      ref_ori_country_code = country_code
    ),
    by = join_by(raw_ori_iata_code == iata_code)
  ) |>
  left_join(
    df_amex_codes_matched |> select(
      iata_code,
      ref_dest_city_code = city_code,
      ref_dest_city_name = city_name,
      ref_dest_city_lon = city_lon,
      ref_dest_city_lat = city_lat,
      ref_dest_lon = lon,
      ref_dest_lat = lat,
      ref_dest_country_name = country_name,
      ref_dest_country_code = country_code
    ),
    by = join_by(raw_dest_iata_code == iata_code)
  )

# coalesce raw and ref data
df_amex_clean_lon_lat <- df_amex_clean_lon_lat |>
  
  rename(
    ori_iata_code = raw_ori_iata_code,
    ori_city_code = ref_ori_city_code,
    ori_city_name = ref_ori_city_name,
    ori_city_lon = ref_ori_city_lon,
    ori_city_lat = ref_ori_city_lat,
    ori_lon = ref_ori_lon,
    ori_lat = ref_ori_lat,
    ori_country_name = ref_ori_country_name,
    ori_country_code = ref_ori_country_code,
    dest_iata_code = raw_dest_iata_code,
    dest_city_code = ref_dest_city_code,
    dest_city_name = ref_dest_city_name,
    dest_city_lon = ref_dest_city_lon,
    dest_city_lat = ref_dest_city_lat,
    dest_lon = ref_dest_lon,
    dest_lat = ref_dest_lat,
    dest_country_name = ref_dest_country_name,
    dest_country_code = ref_dest_country_code
  ) |>
  relocate(
    c(
      ori_iata_code,
      ori_lat,
      ori_lon,
      ori_city_code,
      ori_city_name,
      ori_city_lon,
      ori_city_lat,
      ori_lat,
      ori_country_name,
      ori_country_code
    ),
    .after = org
  ) |>
  relocate(
    c(
      dest_iata_code,
      dest_lon,
      dest_lat,
      dest_city_code,
      dest_city_name,
      dest_city_lon,
      dest_city_lat,
      dest_country_name,
      dest_country_code
    ),
    .after = ori_country_code
  )

write_rds(df_amex_clean_lon_lat, fs::path(clean_path, "amex_clean_lon_lat.rds"))
