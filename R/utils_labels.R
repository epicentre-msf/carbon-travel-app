#Constants 

date_intervals <- c( "Month" = "month", 
                     "Quarter" = "quarter", 
                     "Year" = "year")

travel_type_var <- c("Air" = "air", 
                     "Rail" = "rail")

# Group variable
group_vars <- c(
  "Travel Type" = "travel_type",
  "Organisation" = "org",
  "Mission" = "mission_country_name",
  "Contract type" = "hq_flying_mission",
  "Reasons for Travel" = "reason_travel",
  "Flight length" = "distance_km_cat"
)

#Display variable
display_var <- c( "Emissions (tCO2e)" = "emission",
                  "Expenses (€)" = "gross_amount",
                  "Flights" = "n_flights", 
                  "Distance (Km)" = "distance_km", 
                  "Distance (Miles)" = "distance_miles")

bar_group <- c(
  "Travel Type" = "travel_type",
  "Organisation" = "org",
  "Mission" = "mission_country_name",
  "Contract type" = "hq_flying_mission",
  "Reason for travel" = "reason_travel",
  "Year" = "year")
