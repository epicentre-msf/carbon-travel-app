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

raw_path <- here::here(sharepoint_path, "Maelle CHARRIER - TOOL", "data", "raw")
clean_path <- here::here("data", "clean")

#Import data  -------------------------------------------------------------

# country_codes
country_codes <- import(fs::path(sharepoint_path, "Maelle CHARRIER - TOOL", "Data", "country_codes.xlsx")) %>%
  clean_names() %>%
  rename(mission_name = name) %>%
  as_tibble()

dir_ls <- fs::dir_ls(fs::path(raw_path, "amex-data"), regexp = "ALLMSF")
myfiles <- lapply(dir_ls, rio::import, skip = 5, guess_max = 10000000)

names(myfiles) <- c("amex_2019", "amex_2020", "amex_2021", "amex_2022", "amex_2023", "amex_2024")

myfiles <- purrr::map(myfiles, ~ .x %>%
                        as_tibble() %>%
                        clean_names())

# bind all together
amex <- myfiles %>%
  bind_rows() %>%
  as_tibble()

# Clean data ----------

amex_clean <- amex %>%
  clean_names() %>%
  rename(
    org = client_id_name,
    flight_type = domestic_international,
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
  mutate(
    across(where(~ is.character(.x)), ~ tolower(.x)),
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
    reason_travel = case_match(
      reason_travel,
      "r01" ~ "Field project/briefing",
      "ro1" ~ "Field project/briefing",
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
    mission_id = str_to_upper(str_extract(mission, "[a-z]{2}")),
    flight_type = case_match(
      flight_type,
      "domestic" ~ NA,
      .default = flight_type
    )
  ) %>%
  # deal with numeric variables
  mutate(across(c(distance_miles, distance_km, gross_amount, net_amount), ~ as.numeric(.x))) %>%
  # clean dates
  mutate(
    invoice_date = ymd(invoice_date),
    month = floor_date(invoice_date, "month"),
    month = format(month, "%Y-%m"),
    quarter = lubridate::quarter(invoice_date, with_year = TRUE),
    quarter = str_replace(quarter, "\\.", "-Q"),
    year = floor_date(invoice_date, "year"),
    year = format(year, "%Y")
  ) %>%
  # fix NAs
  mutate(across(c(reason_travel, flight_type), ~ if_else(is.na(.x), "Missing", .x))) %>%
  relocate(c(month, quarter, year), .after = invoice_date) %>%
  arrange(ticket_number, traveler_name) %>%
  # id the flight that were refunded and remove them
  mutate(
    .by = c(ticket_number, traveler_name, dest_code, ori_code),
    refunded = sum(distance_km) == 0
  ) %>%
  filter(refunded == FALSE) %>%
  # remove the remaining negative
  filter(distance_km >= 0) %>%
  # filter out the non valid city code
  filter(str_detect(ori_code, "[a-z]")) %>%
  filter(!str_detect(carrier, "rail")) |>
  arrange(ticket_number) %>%
  select(-c(
    traveler_name,
    ticket_number,
    reason_code,
    contains("customer_defined"),
    pnr
  ))

# add country codes - THEY NEED TO BE CHECKED
amex_clean <- left_join(amex_clean, country_codes, by = c("mission_id" = "country_code"))

# Calculate the distance between cities and compare with AMEX dist --------
# Amex uses terrestrial miles
# can't compare distance yet

# Calculate emissions for flights -----------------------------------------

amex_clean <- amex_clean %>%
  mutate(
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
    emission = round((distance_km * coe2_fct) / 1000, 2),
    ori = str_to_sentence(ori),
    dest = str_to_sentence(dest),
    flight_type = str_to_sentence(flight_type)
  )

# Add city codes ----------------------------------------------------------
# import airports and cities data
df_airports <- readRDS(here::here(clean_path, "df_airports.rds"))
df_cities <- readRDS(here::here(clean_path, "df_cities.rds"))

df_airports <- df_airports |> left_join(df_cities |>
                                          distinct(city_code, .keep_all = TRUE) |> 
                                          select(city_code, 
                                                 city_lon = lon, 
                                                 city_lat = lat ))
#rename codes for matching 
# Blaise Diagne International Airport is in data but doesn't match Dakar as a city - 
# we force it to DKR iata code to match DKR city

amex_clean <- amex_clean |> 
  rename(
    raw_ori_iata_code = ori_code, 
    raw_dest_iata_code = dest_code
    
  ) |> 
  
  mutate(raw_ori_iata_code = case_match(raw_ori_iata_code, 
                                        "dss" ~ "dkr", 
                                        .default = raw_ori_iata_code)) |> 
  
  select(-c(ori, dest))

df_amex_codes <- tibble(iata_code = c(amex_clean$raw_ori_iata_code, 
                                      amex_clean$raw_dest_iata_code)) |>
  mutate(iata_code = str_to_upper(str_trim(iata_code))) |>
  distinct()

df_amex_codes_matched_1 <- df_amex_codes |>
  inner_join(df_airports, 
             by = join_by(iata_code)
  ) |>
  
  select(iata_code, 
         city_code, 
         city_name, 
         city_lon, 
         city_lat,
         country_name, 
         country_code = country_code3, 
         lon, 
         lat)

df_amex_codes_matched_2 <- df_amex_codes |>
  anti_join(df_airports, by = join_by(iata_code == iata_code)) |>
  inner_join(df_cities, by = join_by(iata_code == city_code)) |>
  mutate(city_code = iata_code, 
         .after = iata_code) |> 
  left_join(df_cities |> select(city_code, city_lon = lon, city_lat = lat), by = join_by(city_code) )

df_amex_codes_matched <- bind_rows(df_amex_codes_matched_1, 
                                   df_amex_codes_matched_2) |>
  distinct(iata_code, .keep_all = TRUE)

# Join the matched codes to the data

df_amex_clean_lon_lat <- amex_clean |>
  
  mutate(across(ends_with("_code"), ~ str_to_upper(str_trim(.x)))) |>
  
  left_join(
    df_amex_codes_matched |> select(iata_code, 
                                    ref_ori_city_code = city_code, 
                                    ref_ori_city_name = city_name, 
                                    ref_ori_city_lon = city_lon,
                                    ref_ori_city_lat = city_lat,
                                    ref_ori_lon = lon, 
                                    ref_ori_lat = lat, 
                                    ref_ori_country_name = country_name, 
                                    ref_ori_country_code = country_code
    ),
    by = join_by(raw_ori_iata_code == iata_code )
    
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

#coalesce raw and ref data 
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
  
  relocate(c(ori_iata_code, 
             ori_lat,  
             ori_lon, 
             ori_city_code, 
             ori_city_name, 
             ori_city_lon, 
             ori_city_lat, 
             ori_lat, 
             ori_country_name, 
             ori_country_code), .after = org) |> 
  
  relocate(c(dest_iata_code, 
             dest_lon, 
             dest_lat, 
             dest_city_code, 
             dest_city_name, 
             dest_city_lon, 
             dest_city_lat, 
             dest_country_name, 
             dest_country_code), .after = ori_country_code) 

write_rds(df_amex_clean_lon_lat, fs::path(clean_path, "amex_clean_lon_lat.rds"))

amex_clean <- amex_clean %>% 
  mutate(distance_km_cat = case_when(distance_km < 1000 ~ "short", 
                                     between(distance_km, 1000, 3499) ~ "medium", 
                                     distance_km  > 3499 ~ "long"), 
         
         coe2_fct = case_when(distance_km_cat == "short" ~ 0.25858, 
                              distance_km_cat == "medium" ~ 0.18746, 
                              distance_km_cat == "long" ~ 0.15196
         ), 
         emission = round((distance_km * coe2_fct)/1000, 2),
         ori = str_to_sentence(ori), 
         dest = str_to_sentence(dest), 
         flight_type = str_to_sentence(flight_type)) 

# Export data -------------------------------------------------------------

export(amex_clean, fs::path(clean_path, "amex_clean.rds"))