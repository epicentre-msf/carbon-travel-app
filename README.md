# Carbon Travel App

# Purpose

This is a multi-function app that help users look into the Carbon emissions due to travel for MSF/Epicentre

The app is organised in two parts

1. **The Amex Analysis dashboard**
 Provides an analysis into the AMEX flight data. This dataset summarises all the air travels booked through the AMEX travel agency
   
2. **The travel planner**
helps MSF/EPICENTRE decision makers to identify suitable meeting/events locations in order to minimise the *plane travel* CO2 emissions.

# Set-up 

In order to run the App locally, you need to have access to `Carbon-travel-App` in Maëlle Charrier's onedrive. Once this is done, you can run the scripts in `data-prep` to prepare the data and the distance matrix: 

1. `prepare_data.R` - prepares the cities, airports and MSF data
2. `data_prep_amex.R` - binds, clean and add cities to the AMEX data
3. `prep_distance_matrix.R` - uses the cleaned AMEX data to generate a network and compute a distance matrix from it

# Data sources

The App currently uses

- MSF presence layer - summarises the city where MSF has offices/projects
- The airport dataset from `{airportr}` package - summarises world airport location. For cities with multiple airport, these are aggregated as the mean of latitued ans longitudes
- AMEX data as provided from the API 

# Roadmap
- [ ] Find which missions/country emits the most in terms of flights
- [ ] Estimate emission for one travel (tomorrow I go to Geneva - what are my emissions ?)
- [ ] Compile all flight travel data together (Wagram + Amex )
- [ ] How to take into account the stop overs ? Make a network of travelled cities 
- [ ] Add a way for individual to see their own data
- [ ] Extent to include train data for European destination
