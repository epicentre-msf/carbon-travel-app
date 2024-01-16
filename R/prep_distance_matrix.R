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

# keep the airports with a IATA code
# also some airports have no city name (39)

air_sub <- airports %>% 
  
  clean_names() %>% 
  
  mutate(city = str_to_sentence(city)) %>% 
  
  filter(iata != "\\N", 
         !is.na(city)) %>% 
  
  filter(
    !(city == "London" & country != "United Kingdom"), 
    !(city == "Newcastle" & country != "United Kingdom"), 
    !(city == "San jose" & country != "Costa Rica"), 
    !(city == "London" & country != "United Kingdom"), 
    !(city == "London" & country != "United Kingdom"), 
  )

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
  
  group_by(city, country) %>% 
  
  summarise(n_airports = n(), 
            iata = paste0(iata, collapse = ", "), 
            latitude = mean(latitude), 
            longitude = mean(longitude)) %>% 
  ungroup

#calculate the haversine distance between all airports in df
#this can take time if many airports
start <- Sys.time()
mat <- geosphere::distm(air_sub[,6:5], air_sub[,6:5])
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