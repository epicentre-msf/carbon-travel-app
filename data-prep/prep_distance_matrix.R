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


source(here::here("R", "set_paths.R"))
source(here::here("R", "utils.R"))

# Set paths -------------------------------------------------------------

paths <- set_paths()

sharepoint_path <- paths$sharepoint_path

raw_path <- fs::path(sharepoint_path, "Maelle CHARRIER - TOOL", "data", "raw")
clean_path <- fs::path(sharepoint_path, "Maelle CHARRIER - TOOL", "data", "clean")

# Import data -------------------------------------------------------------

# Airport + MSF data 

dat <- import(fs::path(clean_path, "air_msf.rds"))

# Distance Matrix ---------------------------------------------------------

#calculate the haversine distance between all airports in df
#this can take time if many airports

start <- Sys.time()
mat <- geosphere::distm(
  select(dat, mean_longitude, mean_latitude),
  select(dat, mean_longitude, mean_latitude),
  fun = geosphere::distHaversine
)
end <- Sys.time()
diff <- end - start
print(diff)

#name the matrix cols and rows
colnames(mat) <- dat$city_id
rownames(mat) <- dat$city_id

#save the matrix
readr::write_rds(mat, fs::path(sharepoint_path, "Maelle CHARRIER - TOOL", "Data", "distance-matrix", "airports_distance_matrix.rds"))

# # Create a IATA distance matrix -------------------------------------------
# start <- Sys.time()
# mat_iata <- geosphere::distm(select(iata, longitude, latitude), 
#                              select(iata, longitude, latitude))
# end <- Sys.time()
# diff <- start - end
# 
# #name the matrix cols and rows
# colnames(mat_iata) <- iata$iata
# rownames(mat_iata) <- iata$iata
# 
# saveRDS(mat_iata, "data/distance-matrix/iata_distance_matrix.rds")