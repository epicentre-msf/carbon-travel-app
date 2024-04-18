library(tidyverse)
library(sf)

path_data <- fs::path(
  Sys.getenv("SHAREPOINT_PATH"),
  "Maelle CHARRIER - Carbon-travel-App",
  "Data"
)

airport_city_path <- fs::path(path_data, "raw", "airport-city-codes.xlsx")
airport_city_sheets <- readxl::excel_sheets(airport_city_path)

df_airport_city <- map_df(airport_city_sheets, ~ {
  readxl::read_excel(airport_city_path, sheet = .x) %>%
    filter(!code %in% c("Airport", "Code", "City", "Code")) %>%
    separate(code, into = c("iata_code", "city_code")) %>%
    drop_na()
})

df_airports <- df_airport_city %>%
  left_join(
    airportr::airports %>%
      select(
        iata_code = IATA,
        airport_name = Name,
        country_name = Country,
        country_code2 = `Country Code (Alpha-2)`,
        country_code3 = `Country Code (Alpha-3)`,
        city_name = City,
        lon = Longitude,
        lat = Latitude
      ),
    by = join_by(iata_code)
  ) %>%
  select(iata_code, airport_name, city_name, city_code, everything())

df_cities <- df_airports %>%
  summarise(
    .by = c(city_code),
    lon = mean(lon, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE)
  )

df_amex <- read_rds(fs::path(path_data, "clean", "amex_clean.rds"))
  
df_amex_codes <- tibble(code = c(df_amex$ori_code, df_amex$dest_code)) %>%
  mutate(code = str_to_upper(str_trim(code))) %>%
  distinct()

df_amex_codes_matched_1 <- df_amex_codes %>%
  inner_join(df_airports, by = join_by(code == iata_code)) %>%
  select(code, city_code, lon, lat)

df_amex_codes_matched_2 <- df_amex_codes %>%
  anti_join(df_airports, by = join_by(code == iata_code)) %>% 
  inner_join(df_cities, by = join_by(code == city_code))  %>% 
  mutate(city_code = code, .after = code)
  
df_amex_codes_matched <- bind_rows(df_amex_codes_matched_1, df_amex_codes_matched_2) %>%
  distinct(code, .keep_all = TRUE)

df_amex_clean_lon_lat <- df_amex %>%
  mutate(across(ends_with("_code"), ~ str_to_upper(str_trim(.x)))) %>%
  left_join(
    df_amex_codes_matched %>% select(ori_code = code, ori_city = city_code, ori_lon = lon, ori_lat = lat),
    by = join_by(ori_code)
  ) %>%
  left_join(
    df_amex_codes_matched %>% select(dest_code = code, dest_city = city_code, dest_lon = lon, dest_lat = lat),
    by = join_by(dest_code)
  ) 

write_rds(df_amex_clean_lon_lat, fs::path(path_data, "clean", "amex_clean_lon_lat.rds"))

# Vis Testing ---------------------------------------------------------
library(opencage)
library(threejs)
library(mapdeck)

top_10_cities <- count(df_map, dest_city, sort = TRUE) %>%
  head(10) %>% 
  pull(dest_city)

df_arc <- df_map %>%
  count(ori_city, dest_city, ori_lat, ori_lon, dest_lat, dest_lon, sort = TRUE) %>%
  # filter(dest_city %in% top_10_cities) %>%
  select(ori_lat, ori_lon, dest_lat, dest_lon, n)

url <- "https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv"
flights <- read.csv(url)
flights$id <- seq_len(nrow(flights))
flights$stroke <- sample(1:3, size = nrow(flights), replace = T)

mapdeck(style = mapdeck_style("dark"), pitch = 45) %>%
  add_arc(
    data = flights,
    layer_id = "arc_layer",
    origin = c("start_lon", "start_lat"),
    destination = c("end_lon", "end_lat"),
    stroke_from = "airport1",
    stroke_to = "airport2",
    stroke_width = "stroke"
  )

df_arc$stroke <- sample(1:3, size = nrow(df_arc), replace = T)
mapdeck(style = mapdeck_style("dark"), pitch = 45) %>%
  add_arc(
    data = df_arc %>% drop_na(),
    layer_id = "arc_layer",
    origin = c("ori_lon", "ori_lat"),
    destination = c("dest_lon", "dest_lat"),
    stroke_from = "ori_city",
    stroke_to = "dest_city",
    stroke_width = "stroke"
  )

# Plot frequent destinations as bars, and the flights to and from
# them as arcs. Adjust arc width and color by frequency.
globejs(
  # lat = ll[, 1],
  # long = ll[, 2],
  arcs = df_arc[,1:4],
  bodycolor = "#aaaaff",
  arcsHeight = 0.3,
  # arcsLwd = df_arc$n,
  arcsColor = "#ffff00",
  arcsOpacity = 0.15,
  atmosphere = TRUE,
  color = "#00aaff",
  pointsize = 0.5
)

df_geocoded <- oc_forward_df(
  df_geocode_test,
  placename = city_name,
  countrycode = country_code
)