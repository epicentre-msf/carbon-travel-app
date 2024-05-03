# ---------------------------
# Purpose of script:single travel estimator
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

# Fake dataset of stop overs 
ori_df <- data.frame(
  origin_id = c("GVA", "DKR", "OUA"),
  n_participant = c(1, 1, 1)
)

# build the segments 
df <- data.frame(
  start_var = head(ori_df$origin_id, -1), 
  end_var = tail(ori_df$origin_id, - 1)
) |> 
  mutate(distance_km = purrr::map2_dbl(start_var, end_var, ~ unname(mat[.x, .y] ) ), 
         distance_cat = case_when(
           distance_km <= 999 ~ "short",
           distance_km >= 3500 ~ "long",
           .default = "medium"
         ) ) |>
  left_join(
    conversion_df |> select(distance_cat, emissions_factor = co2e),
    by = "distance_cat"
  ) |>
  mutate(
    trip_emissions = round(digits = 3, distance_km * emissions_factor)
  ) |> 
  left_join(select(dest, city_code, start_city = city_name, start_country = country_name), by = join_by(start_var == city_code)) |> 
  left_join(select(dest, city_code, end_city = city_name, end_country = country_name), by = join_by(end_var == city_code)) #|> 
  
  #select(start_city, start_country, end_city, end_country, distance_km, trip_emissions) 

# Get a map

#origin df


#get the shortest path in network for all origin and this destination 
short_paths <- purrr::map2(df$start_var, 
                               df$end_var, 
                          ~ sfnetworks::st_network_paths(net, from = .x, to = .y)
                          )

short_nodes <- unique(unname(unlist(purrr::map( short_paths, ~ .x |> pull(node_paths) |> unlist() ))))

short_edges <- unname(unlist(purrr::map( short_paths, ~ .x |> pull(edge_paths) |> unlist() ) ) )

nodes <- net |> activate("nodes") |> filter(name %in% short_nodes) |> st_as_sf()
edges <- net |> activate("edges") |> slice(short_edges) |> st_as_sf()

#quick map
mapview::mapview(nodes) +
  mapview::mapview(edges)

leaflet::leaflet() |> 
  leaflet::addProviderTiles("CartoDB.Positron", group = "Light") |>
  leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = FALSE)) |>
  leaflet.extras::addFullscreenControl(position = "topleft") |>
  leaflet.extras::addResetMapButton()  |>
  leaflet::addCircleMarkers(
    data = nodes,
    lng = ~lon,
    lat = ~lat,
    radius = 7, 
    fillColor = ~ "darkred",
    color = ~ "white",
    fillOpacity = 0.8,
    weight = 1,
    label = ~ city_name
  ) |> 
  leaflet::addPolylines(data = edges) 






