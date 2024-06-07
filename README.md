# Carbon Travel App

# Purpose

This is a multi-function app that help users look into the Carbon emissions due to travel for MSF/Epicentre

The app is organised in three parts

1. **The Amex Analysis dashboard**
 Provides an analysis into the AMEX flight data. This dataset summarises all the air travels booked through the AMEX travel agency
   
2. **The travel planner**
helps MSF/EPICENTRE decision makers to identify suitable meeting/events locations in order to minimise the *plane travel* CO2 emissions.

# Set-up 

In order to run the App locally, you need to have access to `Carbon-travel-App` in Maëlle Charrier's onedrive. Once this is done, you can run the scripts in `data-prep` to prepare the data and the distance matrix: 

You only need to run `0_master_data_preparation.R` - this will source in the right order the following script:

1. `1_prepare_cities.R` - prepares the cities, airports and MSF data
2. `2_prepare_amex.R` - binds, clean and add cities to the AMEX data (OCB, OCA, OCG)
3. `3_prepare_wagram.R` - binds, clean and add cities to the WAGRAM data (Epicentre, OCP)
4. `4_prepare_cwt.R` - binds, clean and add cities to the CWT data (OCBA)
5. `5_bind_amex_wagram_cwt.R` - binds all cleaned flights data together
5. `6_prepare_distance_matrix.R` - uses the cleaned flights data to generate a network and compute distance matrix from it

# Roadmap

- [ ] Extent to include train data from the various data source
- [ ] Add a way for individual to see their own data