#Data preparation for Carbon travel app 

fetch_data_offline <- TRUE

#1. prepare cities name and code

source(here::here("data-prep", "1_prepare_cities.R"))

#2. Prepare AMEX data

source(here::here("data-prep", "2_prepare_amex.R"))

#3. Prepare WAGRAM data

source(here::here("data-prep", "3_prepare_wagram.R"))

#4. Prepare CWT data

source(here::here("data-prep", "4_prepare_cwt.R"))

#5. Bind together all flights data

source(here::here("data-prep", "5_bind_amex_wagram_cwt.R"))

#  =============================================  These scripts are long to run ! (~20 min) =============================================

#6. Create networks 

source(here::here("data-prep", "6_prepare_networks.R"))

#7. Prepare the emissions matrix

source(here::here("data-prep", "7_prepare_emission_matrix.R"))