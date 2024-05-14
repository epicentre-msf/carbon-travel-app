#Constants 

date_intervals <- c( "Month" = "month", 
                     "Quarter" = "quarter", 
                     "Year" = "year")

# Group variable
group_vars <- c(
  "Organisation" = "org",
  "Mission" = "mission_country_name",
  "HQ/Flying/Mission" = "hq_flying_mission",
  "Reasons for Travel" = "reason_travel",
  "Flight lenght" = "distance_km_cat"
)

#Display variable
display_var <- c( "Emissions (tCO2e)" = "emission",
                  "Expenses (€)" = "gross_amount",
                  "Flights" = "n_flights", 
                  "Distance (Km)" = "distance_km", 
                  "Distance (Miles)" = "distance_miles")

bar_group <- c(
  "Organisation" = "org",
  "Mission" = "mission_country_name",
  "HQ/Flying/Mission" = "hq_flying_mission",
  "Reason for travel" = "reason_travel",
  "Year" = "year")


