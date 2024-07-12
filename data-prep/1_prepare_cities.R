# ---------------------------
# Purpose of script: Prepare the data for distance matrix calculations
#
# Author: Hugo Soubrier
#
# Date Created: 2024-01-18
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
  airportr,
  hmatch,
  tidyverse # data science
)

# Source script ---------------------------

source(here::here("R", "set_paths.R"))
source(here::here("R", "utils.R"))

# Set paths -------------------------------------------------------------

path_data <- get_sp_data_path()

fs::dir_create("data", "clean")
raw_path <- fs::path(path_data, "raw")
clean_path <- fs::path("data", "clean")

airport_city_path <- fs::path(path_data, "raw", "airport-city-codes.xlsx")

# Airports & cities -------------------------------------------------------------

#Create a dataframe of all airports with the city code attached to it

airport_city_sheets <- readxl::excel_sheets(airport_city_path)

df_airport_city <- map_df(airport_city_sheets, ~ {
  readxl::read_excel(airport_city_path, sheet = .x) |>
    filter(!code %in% c("Airport", "Code", "City", "Code")) |>
    separate(code, into = c("iata_code", "city_code")) |>
    drop_na()
})

#fix wrong airpots 
df_airport_city <- df_airport_city |> filter(iata_code != "MQP" & city_code != "NLP")

# Airport database
df_airports <- df_airport_city |>
  left_join(
    airportr::airports |>
      select(
        iata_code = IATA,
        airport_name = Name,
        country_name = Country,
        country_code2 = `Country Code (Alpha-2)`,
        country_code3 = `Country Code (Alpha-3)`,
        city_name = City,
        lon = Longitude,
        lat = Latitude
      ),
    by = join_by(iata_code)
  ) |>
  select(iata_code, airport_name, city_name, city_code, everything())

# fix the duplicated for same code + different city name (BRU: Brussels and Charleroi)
df_airports <- df_airports |> 
  mutate(city_code = case_when(
    city_name == "Faleolo" ~ "FAL", 
    city_name == "Alanya" ~ "AYL", 
    city_name == "Charleroi" ~ "CHR", 
    city_name == "Dallas" ~ "DAL", 
    city_name == "Prestwick" ~ "PRW", 
    city_name == "Rio Negro" ~ "RIN", 
    city_name == "Bergamo" ~ "BEO", 
    city_name == "Augsburg" ~ "AUG", 
    city_name == "Newark" ~ "NWK", 
    city_name == "Kadena" ~ "KDA", 
    city_name == "Sandefjord" ~ "SAJ", 
    city_name == "Keflavik" ~ "KEF", 
    city_name == "Campinas" ~ "CAM", 
    city_name == "Hewandorra" ~ "HWR", 
    city_name == "Charlotte Amalie" ~ "CHM", 
    city_name == "Treviso" ~ "TVI", 
    city_name == "Baltimore" ~ "BAT", 
    .default = city_code
  ), 
  
  city_name = case_when(city_name == "Milan" ~"Milano", 
                        city_name == "Teheran" ~ "Tehran",
                        .default = city_name)
  
  ) 
write_rds(df_airports, here::here(clean_path, "df_airports.rds"))

# City database
# group for cities with more than 1 airport
df_cities <- df_airports |>
  mutate(city_name = case_match(city_name, "Panama" ~ "Panama City", .default = city_name)) |> 
  summarise(
    .by = c(city_name, country_name, country_code3),
    
    #required as some city_name +country_name have different city_Code : eg Kabul, so here we take the first one
    city_code = city_code[1],
    lon = mean(lon, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE)
  ) |>
  rename(country_code = country_code3)
                                            
write_rds(df_cities, here::here(clean_path, "df_cities.rds"))

# MSF data ----------------------------------------------------------------

# load MSF project data from the GIS unit
msf_proj <- import(fs::path(raw_path, "msf-data", "msf_presence_layer.xlsx")) |>
  as_tibble() |>
  clean_names()

# try to geo code the missing cities from lat/long ?
msf_raw <- msf_proj |>
  rename(
    city = locality,
    msf_type = type_2
  ) |>
  filter(!str_detect(city, "\\?")) |>
  mutate(
    across(c(country, city), ~ str_squish(str_to_lower(.x))),
    country_code = countrycode::countrycode(country, "country.name", "iso3c"),
    lat = as.numeric(lat),
    long = as.numeric(long)
  ) |>
  relocate(country_code, .after = country)

# get unique MSF cities
cities_raw <- msf_raw |>
  distinct(city, country_code) |>
  rename(city_name = city)

# Match the airport cities to MSF cities
msf_match <- hmatch_composite(
  cities_raw,
  df_cities,
  by = c(
    "city_name",
    "country_code"
  ),
  fuzzy = TRUE
) |>
  mutate(match = !is.na(ref_city_name)) |>
  filter(match)

# join to MSF data
msf_clean <- left_join(
  msf_raw,
  msf_match,
  by = c("city" = "city_name")
) |>
  mutate(
    city_name = coalesce(
      ref_city_name,
      city
    ),
    country_name = coalesce(country_name, country),
    country_code = coalesce(ref_country_code, country_code.x),
    lat = coalesce(lat.y, lat.x),
    long = coalesce(lon, long)
  ) |>
  relocate(c(city_name, city_code, country_name, country_code), .after = oc) |>
  relocate(c(lat, long), .after = intervention_type) |>
  select(-c(
    city,
    country,
    country_code.y,
    country_code.x,
    lon,
    lat.x,
    lat.y,
    ref_country_code,
    ref_city_name,
    match_type,
    match,
    update_by,
    close_date_precision
  )) |>
  mutate(across(c(city_name, country_name), ~ str_to_sentence(.x)))

write_rds(msf_clean, fs::path(clean_path, "full_msf_clean.rds"))

# keep one row per city
msf_unique <- msf_clean |>
  summarise(
    .by = c(country_name, country_code, city_name, city_code),
    n_msf_oc = n_distinct(oc),
    oc = paste0(unique(oc), collapse = ", "),
    msf_type = paste0(unique(msf_type), collapse = ", ")
  ) |>
  select(
    country_name,
    country_code,
    city_name,
    city_code,
    oc,
    msf_type
  )

write_rds(msf_unique, fs::path(clean_path, "unique_msf_clean.rds"))

# Conversion factors (given by Maelle) ------------------------------------
conversion_df <- data.frame(
  distance = c("0-999 km", "1000-3499 km", "+ 3500 km"),
  distance_cat = c("short", "medium", "long"),
  co2e = c(0.25858, 0.18746, 0.15196)
)

export(conversion_df, fs::path(clean_path, "conversion_df.rds"))
