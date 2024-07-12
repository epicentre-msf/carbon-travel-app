# Data prep for Wagram 

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

fetch_data_offline <- TRUE

if(fetch_data_offline){
  
  # OFFLINE
  path_offline <- here::here("data", "raw")
  raw_path <- path_offline
  
}

#Import Wagram data 
wagram_files <- fs::dir_ls(fs::path(raw_path, "wagram-data"), regexp = "Standard")

w_df_ls <- lapply(wagram_files, rio::import)

w_df_ls <- purrr::map2(w_df_ls,
                       c("2019", "2020", "2021", "2022", "2023"), ~ .x %>% as_tibble() %>% 
                         clean_names() %>% 
                         mutate(across(everything(), ~ as.character(.x)), 
                                year = .y))
#name the files
names(w_df_ls) <- c("w_2019", "w_2020", "w_2021", "w_2022", "w_2023")

amex |> select(-c(contains("ori_"), contains("dest_"))) |> names()

# Map variables  ----------------------------------------------------------
#remove all frais and trains 

# 2019 --------------------------------------------------------------------
w_df_ls$w_2019 |> names()

w_2019 <- w_df_ls$w_2019 |> 
  select(
    org = nom_client,
    traveler_name = voyageur, 
    invoice_number = no_de_facture, 
    code = code_projet, 
    invoice_date = date_de_facture, 
    full_trip = parcours,
    trip_type = trajet,
    ori_city_name = origine, 
    dest_city_name = destination, 
    dest_country_name = p_ays_destination, 
    gross_amount = ttc, 
    contains("segment"), 
    activite
  ) |>  
  rename_with( .fn =  ~ paste0("seg_", str_extract(.x, "[1-9]{1}")), .cols = contains("segment")) |> 
  
  #remove frais keep air and rails
  filter(!str_detect(full_trip, "FRAIS"), 
         activite %in% c("AERIEN BSP", "AERIEN HORS BSP", "AERIEN LOW COST", "FER") 
  ) |> 
  mutate(travel_type =  
           case_when(str_detect(activite, "AERIEN") ~"air", 
                     activite == "FER" ~ "rail")) |> select(-activite)

# 2020 --------------------------------------------------------------------
w_df_ls$w_2020 |> names()

w_2020 <- w_df_ls$w_2020 |> 
  select(
    org = nom_client,
    traveler_name = voyageur, 
    invoice_number = no_de_facture, 
    code = code_projet, 
    invoice_date = date_de_facture, 
    full_trip = parcours,
    trip_type = trajet,
    ori_city_name = origine, 
    dest_city_name = destination, 
    dest_country_name = p_ays_destination, 
    gross_amount = ttc, 
    contains("segment"), 
    activite
  ) |> 
  
  rename_with( .fn =  ~ paste0("seg_", str_extract(.x, "[1-9]{1}")), .cols = contains("segment")) |>
  
  #remove frais keep air and rails
  filter(!str_detect(full_trip, "FRAIS"), 
         activite %in% c("AERIEN BSP", "AERIEN HORS BSP", "AERIEN LOW COST", "FER") 
  ) |> 
  mutate(travel_type =  
           case_when(str_detect(activite, "AERIEN") ~"air", 
                     activite == "FER" ~ "rail")) |> select(-activite)

# 2022 --------------------------------------------------------------------
w_df_ls$w_2022 |> names()

w_2022 <- w_df_ls$w_2022 |> 
  select(org = nom_client,
         traveler_name = voyageur, 
         invoice_number = numero_piece, 
         invoice_date = date_facture,
         full_trip = parcours,
         trip_type = trajet, 
         ori_city_name = ville_origine, 
         dest_city_name = ville_destination, 
         dest_country_name = pays_destination, 
         gross_amount = montant_ttc, 
         code = a3, 
         contains("par_sg"), 
         activite
  ) |>  
  rename_with( .fn =  ~ paste0("seg_", str_extract(.x, "[1-9].*$")), .cols = contains("par_sg")) |> 
  
  #remove frais keep air and rails
  filter(!str_detect(full_trip, "FRAIS"), 
         activite %in% c("AERIEN BSP", "AERIEN HORS BSP", "AERIEN LOW COST", "FER") 
  ) |> 
  mutate(travel_type =  
           case_when(str_detect(activite, "AERIEN") ~"air", 
                     activite == "FER" ~ "rail")) |> select(-activite)

# 2023 --------------------------------------------------------------------
w_df_ls$w_2023 |> names()

w_2023 <- w_df_ls$w_2023 |> 
  select(org = nom_client,
         traveler_name = voyageur, 
         invoice_number = numero_piece, 
         invoice_date = date_facture,
         full_trip = parcours,
         trip_type = trajet, 
         ori_city_name = ville_origine, 
         dest_city_name = ville_destination, 
         dest_country_name = pays_destination, 
         gross_amount = montant_ttc, 
         code = a3, 
         contains("par_sg"), 
         activite 
  ) |> 
  rename_with( .fn =  ~ paste0("seg_", str_extract(.x, "[1-9].*$")), .cols = contains("par_sg")) |> 
  
  #remove frais keep air and rails
  filter(!str_detect(full_trip, "FRAIS"), 
         activite %in% c("AERIEN BSP", "AERIEN HORS BSP", "AERIEN LOW COST", "FER") 
  ) |> 
  mutate(travel_type =  
           case_when(str_detect(activite, "AERIEN") ~"air", 
                     activite == "FER" ~ "rail")) |> select(-activite)

# Bind all together -------------------------------------------------------

w_raw <- list(w_2019, w_2020, w_2022, w_2023) |> bind_rows()

# Cleaning ----------------------------------------------------------------
w_clean <- w_raw |> 
  
  mutate(
    org = case_match(org, 
                     "EPICENTRE - BTA" ~ "Epicentre", 
                     "FONDATION MSF - BTA" ~"Fondation MSF", 
                     "MEDECINS SANS FRONTIERES - AUSTRALIA" ~"MSF - Australia", 
                     "MEDECINS SANS FRONTIERES - BTA" ~"OCP", 
                     "MSF - BUREAU INTERNATIONAL" ~"MSF International", 
                     "MSF INTERNATIONAL  - SOCS" ~"MSF International", 
                     "MSF LOGISTIQUE" ~"MSF Logistique"
    ) ) |>  
  mutate(across(c(gross_amount), ~ as.numeric(.x)), 
         across(c(traveler_name, trip_type, travel_type), ~ tolower(.x)), 
         invoice_date = ymd(invoice_date), 
         month = floor_date(invoice_date, "month"),
         month = format(month, "%Y-%m"),
         quarter = lubridate::quarter(invoice_date, with_year = TRUE),
         quarter = str_replace(quarter, "\\.", "-Q"),
         year = floor_date(invoice_date, "year"),
         year = format(year, "%Y")
         
  ) |> 
  
  relocate(c(month, quarter, year), .after = invoice_date) |> 
  
  #remove all the negative amounts - NEED to check that there is no duplicates
  filter(gross_amount > 0)

#Pivot to split the trips - and split the prices 
w_seg <- w_clean |> 
  pivot_longer(contains("seg"), 
               names_to = "seg_nb", 
               values_to = "cities", 
               values_drop_na = TRUE, 
  ) |> 
  
  separate(cities, into = c("ori", "dest"), sep = "/") |> 
  
  mutate(.by = c(traveler_name, invoice_number, full_trip), 
         ori = tolower(ori), 
         dest = tolower(dest), 
         gross_amount = round(digits = 2, gross_amount/n() )
  ) |> 
  select(-c(trip_type, ori_city_name, dest_city_name, dest_country_name))

# 2021 --------------------------------------------------------------------
#looks like these data are not with multiple segments in one line

w_df_ls$w_2021 |> names()

w_2021 <- w_df_ls$w_2021 |> 
  select(
    org = nom_client,
    traveler_name = voyageur, 
    invoice_number = no_de_facture, 
    code = code_projet, 
    invoice_date = date_de_facture, 
    full_trip = parcours,
    ori = origine, 
    dest = destination,
    gross_amount = ttc, 
    activite = activite2
  ) |> 
  #remove frais and rails
  filter(full_trip != "FRAIS", 
         activite %in% c("AERIEN", "FER")
  ) |> 
  
  mutate(travel_type = case_when(activite == "FER" ~ "rail", 
                                 activite == "AERIEN" ~"air"), 
         
         across(c(traveler_name, ori, dest, travel_type), ~ tolower(.x)), 
         invoice_date = ymd(invoice_date), 
         month = floor_date(invoice_date, "month"),
         month = format(month, "%Y-%m"),
         quarter = lubridate::quarter(invoice_date, with_year = TRUE),
         quarter = str_replace(quarter, "\\.", "-Q"),
         year = floor_date(invoice_date, "year"),
         year = format(year, "%Y"),
         across(c(gross_amount), ~ as.numeric(.x)),
         org = case_match(org, 
                          "EPICENTRE - BTA" ~ "Epicentre", 
                          "FONDATION MSF - BTA" ~"Fondation MSF", 
                          "MEDECINS SANS FRONTIERES - AUSTRALIA" ~"MSF - Australia", 
                          "MEDECINS SANS FRONTIERES - BTA" ~"OCP", 
                          "MSF - BUREAU INTERNATIONAL" ~"MSF International", 
                          "MSF INTERNATIONAL  - SOCS" ~"MSF International", 
                          "MSF LOGISTIQUE" ~"MSF Logistique"
         )
  )

# Add 2021 
w_seg <- bind_rows(w_seg, w_2021)

# Geocoding ---------------------------------------------------------------

#get raw name to match
raw_df <- data.frame(name = unique(c(w_seg$ori, w_seg$dest) )) |> as_tibble()

# get ref names to match 
df_airports <- readRDS(here::here(clean_path, "df_airports.rds"))
df_cities <- readRDS(here::here(clean_path, "df_cities.rds"))

df_airports <- df_airports |> left_join(df_cities |>
                                          distinct(city_code, .keep_all = TRUE) |>
                                          select(
                                            city_code,
                                            city_lon = lon,
                                            city_lat = lat
                                          ))

# the data are composite of city and airport names so createa composite ref from df_airports

airport <- select(df_airports, name = airport_name,  city_code, city_name, city_lon, city_lat, country_name, country_code3) 
cities <- df_airports |> mutate(name = city_name) |> select(name, city_code, city_name, city_lon, city_lat, country_name, country_code3)|> distinct(name, .keep_all = TRUE)

ref_df <- bind_rows(airport, cities)

match_df <- hmatch::hmatch_composite(
  raw_df, 
  ref_df, 
  fuzzy = TRUE, 
  fuzzy_method = "osa",
  fuzzy_dist = 2L
)

#extract the unmatched
unmatch <- match_df |> filter(is.na(ref_name)) |> select(name, city_code)

# create an excel file to manually match 
choices <- df_cities |>
  select(city_code, city_name) 

if (!file.exists(fs::path("data-prep", "output", "unmatched_city_wagram.xlsx"))) {
  
  # save as excel to mnaually create a dict
  qxl::qxl(
    unmatch,
    file = fs::path("data-prep", "output", "unmatched_city_wagram.xlsx")
  ) 
}

# import excel file and match with dict
man_df <- import(fs::path("data-prep", "output", "unmatched_city_wagram.xlsx"))

match_df <- hmatch::hmatch_composite(
  raw_df, 
  ref_df, 
  fuzzy = TRUE, 
  fuzzy_method = "osa",
  fuzzy_dist = 2L, 
  man = man_df, 
  code_col = "city_code"
)

df_w_city <- w_seg |> left_join(select(match_df,
                                       ori = name, 
                                       city_code,
                                       city_name, 
                                       city_lon, 
                                       city_lat, 
                                       country_name,
                                       country_code = country_code3) |> rename_with(.cols = contains("_"), .fn = ~ paste0("ori_", .x)) |> distinct(ori, .keep_all = TRUE)
) |> 
  
  left_join(select(match_df,
                   dest = name, 
                   city_code,
                   city_name, 
                   city_lon, 
                   city_lat, 
                   country_name,
                   country_code = country_code3) |> rename_with(.cols = contains("_"), .fn = ~ paste0("dest_", .x)) |> distinct(dest, .keep_all = TRUE)
  ) |> 
  select(-ori, -dest)

# Calculate distances  ----------------------------------------------------
df_w_clean_lon_lat <- df_w_city  |> 
  
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


# Add mission codes -------------------------------------------------------
#get country names and codes from OCG
clean_country_names <- import(here::here(raw_path, "project_codes", "OCG_FormatedProjectCode.xlsx")) |>
  as_tibble() |>
  clean_names() |> 
  distinct(country_name) |> 
  mutate(mission_country_iso = countrycode::countrycode(country_name, "country.name", "iso3c"))

# using project codes from OCP 
country_codes <- import(here::here(raw_path, "project_codes", "OCP_201810_code_projet.xlsx")) |>
  as_tibble() |>
  clean_names() |> 
  filter(str_detect(code_mission_code_projet, "[A-Z]{2}1.$")) |> 
  mutate(
    code_mission_code_projet = str_remove(code_mission_code_projet, "\\)" ), 
    code_mission_code_projet = str_remove(code_mission_code_projet, "\\(" ),
    code_mission_code_projet = str_remove(code_mission_code_projet, "1"),
    code_mission_code_projet = tolower(code_mission_code_projet),
    pays_projet = str_remove(pays_projet,  " \\(.*\\)"), 
    pays_projet = tolower(pays_projet), 
    pays_projet = case_match(pays_projet, 
                             "rca" ~ "central african republic", 
                             "rdc" ~"democratic republic of congo",
                             "papouasie n.g." ~"Papua New Guinea",
                             .default = pays_projet), 
    mission_country_iso = countrycode::countrycode(pays_projet, "country.name", "iso3c"), 
    mission_country_iso = case_when(
      pays_projet == "armenie" ~ "ARM",
      pays_projet == "cambodge" ~ "KHM",
      pays_projet == "georgie" ~ "GEO",
      pays_projet == "liban" ~ "LBN",
      pays_projet == "libye" ~ "LBY",
      pays_projet == "Papua New Guinea" ~ "PNG",
      pays_projet == "russie" ~ "RUS",
      pays_projet == "somalie" ~ "SOM",
      pays_projet == "sud-soudan" ~ "SSD",
      pays_projet == "syrie" ~ "SYR",
      pays_projet == "tchad" ~ "TCD",
      .default = mission_country_iso
    )
  )|> 
  #make sure names are written like in OCG
  left_join(clean_country_names, join_by(mission_country_iso)) |> 
  mutate(country_name = coalesce(country_name, str_to_sentence(pays_projet))) |>
  select(
    code = code_mission_code_projet, 
    mission_country_name = country_name, 
    mission_country_iso
  ) 

project_codes <- import(here::here(raw_path, "project_codes", "OCP_GENERAL_CODE A1_BRUT.xlsx")) |>
  as_tibble() |>
  clean_names() |> 
  select(code = code_projet) |> 
  mutate(
    code = tolower(code),
    country = str_extract(code, "^[a-z]{2}") ) |> 
  left_join(country_codes, by = join_by(country == code)) |> 
  summarise(.by = code, country = unique(country), 
            mission_country_name = unique(mission_country_name), 
            mission_country_iso = unique(mission_country_iso) ) |> 
  select(-country)

# Add the contract type 
df_w_clean_lon_lat <- df_w_clean_lon_lat |> 
  mutate(
    code = str_squish(code),
    code = tolower(code) ) |> 
  left_join(project_codes, join_by(code))

write_rds(df_w_clean_lon_lat, fs::path(clean_path, "wagram_clean_lon_lat.rds"))
