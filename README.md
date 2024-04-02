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

# To do on App 
- [ ] Add a download data button - No sensitive data 
- [ ] Add a Tab for calculating one travel 

# Roadmap
- [ ] Compile all flight travel data together 
- [ ] How to take into account the stop overs ? 
- [ ] Estimate emission for one travel (tomorrow I go to Geneva - what are my emissions ?)
- [ ] Extent to include train data for European destination
- [ ] Add a way for individual to see their own data
