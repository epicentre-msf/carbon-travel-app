#Constants 

date_intervals <- c( "Month" = "month", "Quarter" = "quarter", "Year" = "year")

# Groups
group_vars <- c(
  "Organisation" = "org",
  "Reasons for Travel" = "reason_travel",
  "Flight cat" = "distance_km_cat"
)
display_var <- c( "Emissions (tCO2e)" = "emission",
                     "Expenses ($)" = "gross_amount",
                     "Flights" = "n_flights", 
                     "Distance (Km)" = "dist_km", 
                     "Distance (Miles)" = "dist_miles")

display_lab <- data.frame(from = unname(display_var), 
                            to = names(display_var))

dist_var <- c( "Emissions (tCO2e)" = "emission",
               "Expenses ($)" = "gross_amount",
               "Distance (Km)" = "distance_km", 
               "Distance (Miles)" = "distance_miles")

dict_lab <- data.frame(from = unname(dist_var), 
                       to = names(dist_var))

bar_group <- c(
  "Organisation" = "org",
  "Reason for travel" = "reason_travel",
  "Year" = "year")