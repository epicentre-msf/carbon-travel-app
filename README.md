# Meeting place planner 

# Purpose

This is an attempt to build a R tool to help MSF/EPICENTRE decision makers to identify suitable meeting/events locations in order to minimise the *plane travel* CO2 emissions.

- The tool must take into account the origins and number of participants
- Then output the location with the smallest CO2 emissions, that is the smallest sum of straight line distances.
- Ideally then use a formula to convert the plane distance into estimated CO2 emissions

# Roadmap

- [X] Calculate the distance matrix between main cities that contain airports
- [X] A script that can get/ sort the distances (and emissions) depending on origins and number of participants
- [X] Compare between a selection of destinations
- [ ] Estimate emission for one travel (tomorrow I go to Geneva - what are my emissions ?)
- [ ] Create a small interactive shiny app 
- [ ] Extent to include train data for European destination

# Requisite to Maëlle 

- [ ] Need the list of cities where MSF has offices


# To be fixed in MSF presence layer 

- Branch offices location cities to be fixed 