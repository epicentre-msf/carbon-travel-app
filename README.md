# Carbon Travel App

# Purpose

This is a multi-function app that help users look into the Carbon emissions due to travel for MSF/Epicentre

The app is organised in two parts

1. **The travel planner**
   helps MSF/EPICENTRE decision makers to identify suitable meeting/events locations in order to minimise the *plane travel* CO2 emissions. This currently supports only locations where MSF is present (Office or Project)

  **Note:** this algorithms uses straight line travel between cities and does not take into account possible stop overs

2. **The Amex Analysis**
   This tab provides a *preliminary* analysis into the AMEX data. This dataset summarises all the air travels booked through the AMEX travel agency
   
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
