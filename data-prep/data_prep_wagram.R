# Data prep for Wagram 

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

#import amex 

amex <- readRDS(here::here(clean_path, "amex_clean_lon_lat.rds"))

# Import data  -------------------------------------------------------------

# OFFLINE
path_offline <- here::here("data", "raw")
raw_path <- path_offline

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
    #flight_type = zone, 
    #code = code_projet, 
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
  #remove frais and rails
  filter(full_trip != "FRAIS", 
         activite %in% c("AERIEN BSP", "AERIEN HORS BSP", "AERIEN LOW COST") )

# 2020 --------------------------------------------------------------------
w_df_ls$w_2020 |> names()

w_2020 <- w_df_ls$w_2020 |> 
  select(
    org = nom_client,
    traveler_name = voyageur, 
    invoice_number = no_de_facture, 
    #flight_type = zone,
    #code = code_projet, 
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
  #remove frais and rails
  filter(full_trip != "FRAIS", 
         activite %in% c("AERIEN BSP", "AERIEN HORS BSP", "AERIEN LOW COST"))

# 2021 --------------------------------------------------------------------
#looks like these data are not with multiple segments in one line

w_df_ls$w_2021 |> names()

w_2021 <- w_df_ls$w_2021 |> 
  select(
    org = nom_client,
    traveler_name = voyageur, 
    invoice_number = no_de_facture, 
    #flight_type = zone,
    #code = code_projet, 
    invoice_date = date_de_facture, 
    full_trip = parcours,
    trip_type = trajet,
    ori_city_name = origine, 
    dest_city_name = destination, 
    dest_country_name = pays_destination, 
    gross_amount = ttc, 
    activite2
  ) |> 
  #remove frais and rails
  filter(full_trip != "FRAIS", 
         activite2 != "AERIEN")

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
         #code = code_projet, 
         contains("par_sg"), 
         activite
  ) |> 
  
  #remove frais and rails
  filter(full_trip != "FRAIS", 
         activite == "AERIEN BSP")

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
         #code = code_projet, 
         contains("par_sg"), 
         activite 
  ) |> 
  #remove frais and rails
  filter(full_trip != "FRAIS", 
         activite == "AERIEN BSP")


# Bind all together -------------------------------------------------------

w_raw <- list(w_2019, w_2020, w_2021, w_2022, w_2023) |> bind_rows()

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
         across(c(traveler_name, trip_type, activite), ~ tolower(.x)), 
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


#Deal with trips
w_clean |> select(contains("segment"), contains("par_sg"))
