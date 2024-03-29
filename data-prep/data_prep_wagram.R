# Data prep for Wagram 

# Load packages ---------------------------

pacman::p_load(

rio,          # import funcs
fs,           # work with path
here,         # create relative paths
janitor,      # data cleaning
lubridate,    # date handling
tidyverse     # data science 

)

source("R/set_paths.R")

paths <- set_paths()
path_to_raw <- fs::path(paths$maelle_charrier_tool, "Data", "raw")

#Import Wagram data 

wagram_files <- fs::dir_ls(fs::path(path_to_raw, "wagram-data"))

w_df <- lapply(wagram_files, rio::import)

w_df <- purrr::map(w_df, ~ .x %>% as_tibble() %>% clean_names() %>% mutate(across(everything(), ~ as.character(.x))))

# Bind together

w_df <- w_df %>% bind_rows()


# Cleaning 

w_df %>% 
  select(voyageur, date_de_facture, date_de_depart, date_de_retour, parcours, 
         origine, destination, pays_destination, prestation, codeclassevoyage, ttc, montant_ttc, codeactivite, co2, code_projet, trajet, km)

w_df %>% count(ttc)
