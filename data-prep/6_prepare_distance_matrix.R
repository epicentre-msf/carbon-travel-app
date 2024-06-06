# ---------------------------
# Purpose of script: Create the distance matrix between airports
#
# Author: Hugo Soubrier
#
# Date Created: 2024-01-12
# ---------------------------
# Notes:
# uses the {airportr} package (https://dshkol.github.io/airportr/index.html)
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
source(here::here("R", "utils.R"))

# Import data -------------------------------------------------------------
dat <- readRDS(here::here("data","clean", "full_amex_wagram.rds"))

msf <- readRDS(here::here("data","clean", "unique_msf_clean.rds"))

df_cities <- readRDS(here::here("data", "clean", "df_cities.rds"))
df_airports <- readRDS(here::here("data", "clean", "df_airports.rds"))

conversion_df <- readRDS(here::here("data", "clean", "conversion_df.rds"))

# Create a Network from flights data -----------------------------------------

dat <- dat |> 
  
  select(contains("city")) 

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
net_dest <- tibble(city_code = c(routes$ori_city_code,routes$dest_city_code), 
                 city_lon = c(routes$ori_city_lon,routes$dest_city_lon), 
                 city_lat = c(routes$ori_city_lat,routes$dest_city_lat)) |>  
  
  left_join(df_cities, by = join_by(city_code)) |> 
  
  distinct(city_code, .keep_all = TRUE) |> 
  
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

# Plot the network 
mapview::mapview( net |> activate("nodes") |> st_as_sf(),
                  col.regions = "red",
                  legend = NULL,
                  layer.name = "nodes" ) +

  mapview::mapview( net |> activate("edges") |> st_as_sf(),
                    layer.name = "edges")

#save the network
readr::write_rds(net, here::here("data", "clean", "flights_network.rds"))

# Distance Matrix ---------------------------------------------------------
#st_network calculates the shortest distance using Haversine 

mat <- sfnetworks::st_network_cost(net)

#distance between Melbourne and Buenos aires 
#in network
mat[["TBS", "SCL"]]
# using haversine 

nodes |> filter(name %in% c("TBS", "SCL"))
geosphere::distHaversine(c(-58.4757, -34.6907), c(28.77981, 41.25644) )

#12270.240
#12259.955

# Divide the matrix by 1000 to get in kilometers 
mat <- mat/1000

units(mat) <-NULL

#save the matrix
readr::write_rds(mat, here::here("data", "clean", "distance_matrix.rds"))

# Create a distance Matrix of cities between them (single travel estimation tab) -------------------------
#need to create a network 
#get all points (all cities)
nodes

#get all combinations of cities for edges
edges2 <- expand.grid(nodes$name, nodes$name) |> as_tibble() |> rename("from" = Var1, "to" = Var2)

#create the network
net2 <- sfnetwork(nodes, edges2, node_key = "name", edges_as_lines = TRUE, directed = FALSE)

# Plot the network 
mapview::mapview( net2 |> activate("nodes") |> st_as_sf(),
                  col.regions = "red",
                  legend = NULL,
                  layer.name = "nodes" ) +
  
  mapview::mapview( net2 |> activate("edges") |> st_as_sf(),
                    layer.name = "edges")

#save the network
readr::write_rds(net2, here::here("data", "clean", "cities_network.rds"))


# mat_all <- geosphere::distm(df_cities[,c('lon','lat')], df_cities[,c('lon','lat')], fun=geosphere::distHaversine)
# 
# colnames(mat_all) <- df_cities$city_code
# rownames(mat_all) <- df_cities$city_code
# 
# #save the matrix
# readr::write_rds(mat_all, here::here("data", "clean", "distance_matrix_all_city.rds"))

