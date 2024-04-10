#Constants 

date_intervals <- c( "Month" = "month", 
                     "Quarter" = "quarter", 
                     "Year" = "year")

# Group variable
group_vars <- c(
  "Organisation" = "org",
  "Reasons for Travel" = "reason_travel",
  "Flight type" = "flight_type",
  "Flight cat" = "distance_km_cat"
)

#Display variable
display_var <- c( "Emissions (tCO2e)" = "emission",
                  "Expenses ($)" = "gross_amount",
                  "Flights" = "n_flights", 
                  "Distance (Km)" = "distance_km", 
                  "Distance (Miles)" = "distance_miles")


display_lab <- data.frame(from = unname(display_var), 
                          to = names(display_var))



bar_group <- c(
  "Organisation" = "org",
  "Reason for travel" = "reason_travel",
  "Year" = "year")

dict_bar_group <- data.frame(from = unname(bar_group), 
                       to = names(bar_group))
