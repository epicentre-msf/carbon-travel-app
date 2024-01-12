# Meeting place planner 

# Purpose

This is an attempt to build a R tool to help MSF/EPICENTRE decision makers to identify suitable meeting/events locations in order to minimise the *plane travel* CO2 emissions.

- The tool must take into account the origins and number of participants
- Then output the location with the smallest CO2 emissions, that is the smallest sum of straight line distances.
- Ideally then use a formula to convert the plane distance into estimated CO2 emissions

# Roadmap

- [X] Calculate the distance matrix between main airports
- [X] Create a script that can get/ sort the distances and emissions depending on origins and number of participants
- [ ] Create a small interactive shiny app 
- [ ] Extent to include train data ?