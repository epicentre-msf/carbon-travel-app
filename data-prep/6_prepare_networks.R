# ---------------------------
# Purpose of script: Create all the required network for distance matrix
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

#source(here::here("R", "set_paths.R"))
#source(here::here("R", "utils.R"))

# Import data -------------------------------------------------------------
dat <- readRDS(here::here("data","clean", "full_amex_wagram_cwt.rds")) |> 
  filter(data_source != "CWT") #remove OCBA flights which have no stop overs 

msf <- readRDS(here::here("data","clean", "unique_msf_clean.rds"))

df_cities <- readRDS(here::here("data", "clean", "df_cities.rds"))
df_airports <- readRDS(here::here("data", "clean", "df_airports.rds"))

# Create a Network from flights data -----------------------------------------

routes <- dat |> 
  
  summarise(
    .by = c(ori_city_code, 
            dest_city_code, 
    ), 
    n = n(),
    ori_city_lat = unique(ori_city_lat), 
    ori_city_lon = unique(ori_city_lon), 
    dest_city_lat = unique(dest_city_lat), 
    dest_city_lon = unique(dest_city_lon), 
    
  ) |> 
  tidyr::drop_na()

#prepare Edges 
edges <- routes |> 
  rename(from = ori_city_code, 
         to = dest_city_code) |> 
  select(from, to ) 

#prepare destination cities 
net_dest <- tibble(city_code = unique(c(routes$ori_city_code,routes$dest_city_code)), 
                   city_lon = unique(c(routes$ori_city_lon,routes$dest_city_lon)), 
                   city_lat = unique(c(routes$ori_city_lat,routes$dest_city_lat))) |>  
  
  left_join(select(df_cities, -c(lon, lat)), by = join_by(city_code)) |> 
  
  left_join(select(msf, city_code, oc, msf_type ), by = join_by(city_code)) |> 
  
  mutate(msf = !is.na(msf_type))

# Create a destination dataframe using the nodes
readr::write_rds(net_dest, here::here("data", "clean", "dest_cities.rds"))

#prepare nodes
nodes <- net_dest |> 
  rename(name = city_code) |> 
  sf::st_as_sf(coords = c("city_lon", "city_lat"), crs =4326) 

#create the network
net <- sfnetwork(nodes, edges, node_key = "name", edges_as_lines = TRUE, directed = FALSE)

#save the network
readr::write_rds(net, here::here("data", "clean", "flights_network.rds"))

# Networks between all cities ---------------------------------------------

#get all combinations of cities for edges
edges_all <- expand.grid(nodes$name, nodes$name) |> as_tibble() |> rename("from" = Var1, "to" = Var2)

#create the network
net_all <- sfnetwork(nodes, edges_all, node_key = "name", edges_as_lines = TRUE, directed = FALSE)

#save the network
readr::write_rds(net_all, here::here("data", "clean", "cities_network.rds"))

mat_all <- sfnetworks::st_network_cost(net_all)

# Divide the matrix by 1000 to get in kilometers 
mat_all <- mat_all/1000

units(mat_all) <-NULL

#save the matrix
readr::write_rds(mat_all, here::here("data", "clean", "distance_matrix_all.rds"))
