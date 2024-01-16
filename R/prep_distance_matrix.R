# ---------------------------
# Script name: prep_distance_matrix.R
#
# Purpose of script: Create the distance matrix between airports
#
# Author: Hugo Soubrier
#
# Date Created: 2024-01-12
#
# Copyright (c) Hugo SOUBRIER, 2024
# Email: hugo.soubrier@epicentre.msf.org
# ---------------------------
# Notes:
# uses the {airportr} package (https://dshkol.github.io/airportr/index.html)
#
#
# ---------------------------

# Load packages ---------------------------

pacman::p_load(
  rio, # import funcs
  fs, # work with path
  here, # create relative paths
  janitor, # data cleaning
  airportr,
  tidyverse # data science
)


# Import data -------------------------------------------------------------

#import msf_country 

msf_country <- import("data/msf_country.xlsx")

# import world cities 
capital <- maps::world.cities %>% 
  mutate(name = tolower(name), 
         name = str_remove(name, "'")) %>% 
  filter(capital == 1) %>% 
  select(country = country.etc, name, lat, long) %>% as_tibble()

# keep the airports with a IATA code
# also some airports have no city name (39)
# in MSF country

air_sub <- airports %>% 
  
  clean_names() %>% 
  
  mutate(city = tolower(city)) %>% 
  
  filter(iata != "\\N", 
         !is.na(city)) %>% 
  
  filter(
    !(city == "london" & country != "United Kingdom"), 
    !(city == "paris" & country != "France"),
    !(city == "athens" & country != "Greece"),
    !(city == "dublin" & country != "Ireland"),
    !(city == "rome" & country != "Italy"),
    
    !(city == "newcastle" & country != "United Kingdom"), 
    !(city == "san jose" & country != "Costa Rica"), 
    !(city == "brest" & country != "France"), 
    !(city == "glasgow" & country != "United Kingdom"), 
    !(city == "barcelona" & country != "Spain"), 
    !(city == "la paz" & country != "Bolivia"), 
    !(city == "lima" & country != "Peru"), 
    !(city == "los angeles" & country != "United States"), 
    !(city == "naples" & country != "Italy"), 
    !(city == "panama city" & country != "Panama"), 
    !(city == "st. petersburg" & country != "Russia"),
    !(city == "sydney" & country != "Australia"),
    !(city == "valencia" & country != "Spain"),
    !(city == "venice" & country != "Italy"),
    
  ) %>% 
  
  mutate(msf = country %in% msf_country$Country, 
         capital = city %in% capital$name)

#remove weird cities that share name with a famous one 

# El dorado  3
# Georgetown 3
# Hamilton   4
# London     3
# Newcastle  3
# San jose   4
# Santa ana  3
# Santa rosa 4
# Santiago   4
# Victoria   3

#For cities with multiple airports: take the mean lat/long of all airports
air_sub <- air_sub %>% 
  
  group_by(city, country, msf, capital) %>% 
  
  summarise(n_airports = n(), 
            iata = paste0(iata, collapse = ", "), 
            latitude = mean(latitude), 
            longitude = mean(longitude), 
            .groups = "drop") %>% 
  
  ungroup

#calculate the haversine distance between all airports in df
#this can take time if many airports
start <- Sys.time()
mat <- geosphere::distm(select(air_sub, longitude, latitude), select(air_sub, longitude, latitude))
end <- Sys.time()
diff <- start - end

print(diff)

#name the matrix cols and rows
colnames(mat) <- air_sub$city
rownames(mat) <- air_sub$city

#save the matrix
saveRDS(mat, "data/airports_distance_matrix.rds")

#save the airport sub df 
saveRDS(air_sub, "data/airports_sub.rds")