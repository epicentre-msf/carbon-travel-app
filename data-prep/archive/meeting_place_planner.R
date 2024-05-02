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
  origin_id = c("GVA", "PAR", "BRU", "LON", "AMS"),
  n_participant = c(2, 2, 3, 4, 5)
)

dest_select <- dest |> filter(msf, str_detect(msf_type, "OC")) |> pull(city_code)

#try with one destination 
get_dest_tot(ori_df, 
             "BOY", 
             df_conversion = conversion_df, 
             dist_mat = mat)

# Map the function over all possible destinations 
all_dest <- purrr::map(
  dest_select,
  ~ get_dest_tot(
    ori_df,
    .x,
    df_conversion = conversion_df,
    dist_mat = mat
  )
)

names(all_dest) <- dest_select

# Bind the grand totals together arrange and display
tbl <- purrr::map(dest$city_code, ~ all_dest[[.x]]$total) |> 
  bind_rows() |> 
  arrange(grand_tot_emission) |> 
  mutate(rank = row_number()) |> 
  relocate(rank, 1) |> 
  left_join(
    select(
      dest, 
      city_code,
      city_lon, city_lat,
      city_name,
      country_name, 
      msf, 
      oc,
      msf_type
    ), 
    by = join_by(destination == city_code)
  ) |> select(rank,
              city_code = destination, 
              city_lon, 
              city_lat,
              city_name,
              country_name,
              grand_tot_km,
              grand_tot_emission,
              oc,
              msf_type)

# Make a reactable 

orange_pal <- function(x) rgb(colorRamp(c("#B8CCAD", "#BF6C67"))(x), maxColorValue = 255)

tbl_sub <- tbl |> select(rank,
                         city_name,
                         country_name,
                         grand_tot_km,
                         grand_tot_emission,
                         oc,
                         msf_type) |> head(50)


reactable(tbl_sub,
          highlight = TRUE,
          searchable = TRUE,
          compact = TRUE,
          defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE, locales = "fr-Fr")),
          columns = list(
            rank = colDef("Rank", align = "left", minWidth = 30),
            city_name = colDef("City", align = "left", minWidth = 80),
            country_name = colDef("Country", align = "left", minWidth = 80),
            grand_tot_km = colDef("Total Km", align = "left", format = colFormat(digits = 0), minWidth = 50),
            grand_tot_emission = colDef("Total Emissions (kg CO2e)", 
                                        align = "left", 
                                        minWidth = 50,
                                        format = colFormat(digits = 0, separators = TRUE), 
                                        style = function(value) {
                                          normalized <- (value - min(head(tbl, 50)$grand_tot_emission)) / (max(head(tbl, 50)$grand_tot_emission) - min(head(tbl, 50)$grand_tot_emission))
                                          color <- orange_pal(normalized)
                                          list(background = color)
                                        }
            ),
            oc = colDef("Operational Center", align = "left", minWidth = 80),
            msf_type = colDef("MSF type", align = "left")
          )
)

# Make A leaflet map of the selected destinations 

#origin df
map_ori <- ori_df |> left_join(dest, by = join_by(origin_id == city_code) )

#destination selected 
map_dest <- "AMS"

#get the shortest path in network for all origin and this destination 
short_paths <- purrr::map(ori_df$origin_id, 
                          ~ sfnetworks::st_network_paths(net, from = .x, to = map_dest))

short_nodes <- unique(unname(unlist(purrr::map( short_paths, ~ .x |> pull(node_paths) |> unlist() ))))

short_edges <- unname(unlist(purrr::map( short_paths, ~ .x |> pull(edge_paths) |> unlist() ) ) )

nodes <- net |> activate("nodes") |> filter(name %in% short_nodes) |> st_as_sf() |> mutate(type = if_else(name == map_dest, "destination", "origin"))
edges <- net |> activate("edges") |> slice(short_edges) |> st_as_sf()

#quick map
mapview::mapview(nodes) +
  mapview::mapview(edges)

pal <- colorFactor(c("darkred", "steelblue"), c("destination", "origin"))

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
    fillColor = ~ pal(type),
    color = ~ "white",
    fillOpacity = 0.8,
    weight = 1,
    label = ~ city_name
  ) |> 
  leaflet::addPolylines(data = edges) |> 
  
  addLegend(position = "topright",
            pal = pal, 
            values = unique(nodes$type))
