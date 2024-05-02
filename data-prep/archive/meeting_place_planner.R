# ---------------------------
# Script name: meeting place planner
#
# Purpose of script: uses the distance matrix, and a  origin df to get the list of optimal meeting location 
#
# Author: Hugo Soubrier
#
# Date Created: 2024-01-12
# Email: hugo.soubrier@epicentre.msf.org
# ---------------------------
# Notes:
#
#
# ---------------------------

# Load packages ---------------------------

pacman::p_load(
  rio, # import funcs
  here, # create relative paths
  janitor, # data cleaning
  sf, 
  sfnetworks,
  tidyverse # data science
)


source(here::here("R", "set_paths.R"))
source(here::here("R", "utils.R"))

# Import data -------------------------------------------------------------

net <- readRDS(here::here("data","clean", "amex_network.rds"))

mat <- readRDS(here::here("data","clean", "distance_matrix.rds"))

dest <- readRDS(here::here("data","clean", "dest_cities.rds"))

msf <- readRDS(here::here("data","clean", "unique_msf_clean.rds"))

conversion_df <- readRDS(here::here("data", "clean", "conversion_df.rds"))

# All paths ---------------------------------------------------------------
# This gets very (too) long 

start <- Sys.time()

# df <- expand.grid(nodes$name, 
#                   nodes$name) |> 
#   as_tibble() |> 
#   mutate(across(c(Var1, Var2), ~ as.character(.x)) ) |> 
#   filter(Var1 != Var2) |> 
#   
#   mutate(path = purrr::map2_chr(Var1, Var2, ~ paste0(st_network_paths(net, from = .x, to = .y )$node_paths[[1]], collapse = " - ") ))
end <- Sys.time()

diff <- end - start
print(diff)

# when want this to be displayed in the final table
paste(st_network_paths(net, from = "NIM", to = "AAE" )$node_paths[[1]], collapse = "-")

# Calculate best location  ------------------------------------------------

# Function to retrieve the distance/emissions/path to a destination 

get_dest_tot <- function(df_origin,
                         destination,
                         df_conversion,
                         dist_mat) {
  
  distances <- unname(dist_mat[df_origin$origin_id, destination])
  
  df_details <- df_origin |>
    mutate(
      destination = destination,
      #path = purrr::map2_chr(origin_id, destination, ~ paste(sfnetworks::st_network_paths(net, from = .x, to = .y )$node_paths[[1]], collapse = "-")),
      distance_km = distances,
      total_distance_km = n_participant * distance_km,
      distance_cat = case_when(
        distance_km <= 999 ~ "short",
        distance_km >= 3500 ~ "long",
        .default = "medium"
      )
    ) |>
    left_join(
      df_conversion |> select(distance_cat, emissions_factor = co2e),
      by = "distance_cat"
    ) |>
    mutate(
      trip_emissions = round(digits = 3, distance_km * emissions_factor),
      total_emissions = n_participant * trip_emissions
    )
  
  grand_df <- df_details |>
    group_by(destination) |>
    summarise(
      grand_tot_km = sum(total_distance_km),
      grand_tot_emission = sum(total_emissions)
    )
  
  ls <- list(details = df_details, 
             total = grand_df)
  
  return(ls)
  
  names(ls) <- destination
}

# Fake dataset of origins 
ori_df <- data.frame(
  origin_id = c("LON", "PAR", "CHR", "GVA", "BCN"),
  n_participant = c(2, 2, 3, 4, 5)
)

#try with one destination 
get_dest_tot(ori_df, 
             "BOY", 
             df_conversion = conversion_df, 
             dist_mat = mat)

# Map the function over all possible destinations 
all_dest <- purrr::map(
  dest$city_code,
  ~ get_dest_tot(
    ori_df,
    .x,
    df_conversion = conversion_df,
    dist_mat = mat
  )
)

names(all_dest) <- dest$city_code

# Bind the grand totals together arrange and display
purrr::map(dest$city_code, ~ all_dest[[.x]]$total) |> 
  bind_rows() |> 
  arrange(grand_tot_emission) |> 
  mutate(rank = row_number()) |> 
  relocate(rank, 1) |> 
  left_join(
    select(
      dest, 
      city_code,
      city_name,
      country_name, 
      msf, 
      msf_type
    ), 
    by = join_by(destination == city_code)
  )

#CHECK DUPLICATED 
# mat <- geosphere::distm(
#   select(cities, lon, lat),
#   select(cities, lon, lat),
#   fun = geosphere::distHaversine
# )