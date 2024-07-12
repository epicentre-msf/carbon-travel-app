# Load packages ---------------------------

pacman::p_load(
  
  rio,          # import funcs
  fs,           # work with path
  here,         # create relative paths
  janitor,      # data cleaning
  lubridate,    # date handling
  tidyverse     # data science 
  
)


rr_dat <- import(
  here("data", 
       "raw", "RR-analysis", "Status of 2021 of distance and assignements IS TO SHARE GDPR.xlsx"), 
  skip = 3, 
  which = 3
) |> as_tibble() |>
  clean_names()

rr_dat |> 
  select(-grand_total, -blank, -unknown) |> 
  rename(pp_id = countries_of_domicile) |> 
  mutate(home_country = if_else(str_detect(pp_id, "[A-Z]{2}"), tolower(pp_id), NA ), 
         pp_id = if_else(str_detect(pp_id, "[A-Z]{2}"), NA, pp_id) ) |> 
  fill(home_country, .direction = "down") |> 
  filter(!is.na(pp_id), pp_id != "Grand Total", pp_id != "Unknown") |> 
  relocate(home_country, .after = pp_id) |> 
  pivot_longer( !pp_id & !home_country, 
                names_to = "destination_country", 
                values_to = "length_days") |> 
  filter(!is.na(length_days))
