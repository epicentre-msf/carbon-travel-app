# ---------------------------
# Purpose of script: Create the emissions and distance matrix using previously created networks
#
# Author: Hugo Soubrier
#
# Date Created: 2024-01-12
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


# Import data ---------------------------------------------------------

#flight network
net <- readRDS(here::here("data", "clean", "flights_network.rds"))

#the distance matrix between all points 
mat_all <- readRDS(here::here("data", "clean", "distance_matrix_all.rds"))

conversion_df <- readRDS(here::here("data", "clean", "conversion_df.rds"))

# Create an Emission Matrix between all cities -------------------------

# 1. Get shortest paths between all cities using the flight network (~ 12 mins)
start_time <- Sys.time()

net_nodes <- net |> activate("nodes") |> pull(name)

list_path <- purrr::map(  net_nodes , 
                          ~ sfnetworks::st_network_paths(net, 
                                                         from = .x )$node_paths 
) 

list_path <- purrr::map( list_path , ~  purrr::map( .x , 
                                                    ~ data.frame(
                                                      start_var = head(.x, -1), 
                                                      end_var = tail(.x, - 1) ) ) )

list_path <- purrr::map( list_path, ~ .x |> purrr::map2( 1:892, ~ .x |>  mutate(index = .y)) |> bind_rows() )

end_time <- Sys.time()

end_time - start_time

# 2. extract the distance from the full matrix and calculate emisisons (~ 5 seconds)
start_time <- Sys.time()

list_distance_df <- purrr::map(list_path, ~ .x |> 
                                 
                                 mutate(
                                   
                                   #need distance matrix across all cities
                                   distance_km = mat_all[cbind(start_var, end_var)], 
                                   distance_cat= case_when(distance_km <1000 ~ "short", 
                                                           between(distance_km, 1000, 3499.9) ~"medium", 
                                                           distance_km >= 3500 ~"long", 
                                   ), 
                                   emissions = case_when(
                                     distance_cat == "short" ~ 0.25858,
                                     distance_cat == "medium" ~ 0.18746,
                                     distance_cat == "long" ~ 0.15196
                                   )
                                 )
)
end_time <- Sys.time()

end_time - start_time


# 3. Summarise all distances and emissions for all nodes then pivot_wider (~ 50 sec)

start_time <- Sys.time()
list_emissions_wide <- purrr::map(list_distance_df, ~ .x  |> 
                                    
                                    summarise(
                                      .by = index,
                                      start = dplyr::first(start_var),
                                      end = dplyr::last(end_var),
                                      tot_km = sum(distance_km),
                                      tot_emissions = sum(emissions)
                                    ) |> 
                                    
                                    select(start, end, tot_emissions) |> 
                                    
                                    pivot_wider(names_from = end, 
                                                values_from = tot_emissions) 
)

end_time <- Sys.time()

end_time - start_time


# 4. Bind all rows and then make it a matrix (~ < 1 min)
start_time <- Sys.time()

list_emissions_wide <- list_emissions_wide |> bind_rows() |> mutate(across(everything() & !start, ~ replace_na(.x, 0 ) ) )

mat <- as.matrix(list_emissions_wide[,-1])

rownames(mat) <- list_emissions_wide$start

end_time <- Sys.time()

end_time - start_time

#5. Save the Emissions Matrix 
readr::write_rds(mat, here::here("data", "clean", "emissions_matrix.rds"))

# Check. st_network calculates the shortest distance using Haversine 
# 
# mat <- sfnetworks::st_network_cost(net)
# 
# #distance between Melbourne and Buenos aires 
# #in network: #12270.240
# mat[["TBS", "SCL"]]
# 
# # using haversine: #12259.955
# nodes |> filter(name %in% c("TBS", "SCL"))
# geosphere::distHaversine(c(-58.4757, -34.6907), c(28.77981, 41.25644) )