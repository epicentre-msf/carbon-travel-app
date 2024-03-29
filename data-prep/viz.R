# some visualisation for AMEX data 

# Load packages ---------------------------

pacman::p_load(
  
  rio,          # import funcs
  fs,           # work with path
  here,         # create relative paths
  janitor,      # data cleaning
  lubridate,    # date handling
  tidyverse     # data science 
  
)
library(highcharter)

source("R/set_paths.R")
paths <- set_paths()

# Import amex clean data 

dat <- import(fs::path(paths$maelle_charrier_tool, "Data", "clean", "amex_clean.rds"))

names(dat)

# Time - series -------------------------------------------------------------------------

group <-  "no grouping"

df <- dat %>% 
  
  mutate(date_group = format(floor_date(invoice_date, "month"), "%Y-%m")) %>%  
  
  summarise(.by = c(date_group), 
            n_flights = n(), 
            dist_km = sum(distance_km), 
            dist_miles = sum(distance_miles), 
            gross_amount = sum(gross_amount),
            emission = sum(emission)
  ) %>% 
  
  arrange(date_group)

#1. Emissions 
hchart(df,
       "column", 
       hcaes(x = date_group, 
             y = emission ))




hc_colors(hc_pal) %>%
  hc_xAxis(type = x_type, title = list(text = date_lab), crosshair = TRUE) %>%
  highcharter::hc_yAxis_multiples(
    list(
      title = list(text = isolate(var_lab())),
      allowDecimals = FALSE
    ),
    list(
      title = list(text = ""),
      allowDecimals = FALSE,
      opposite = TRUE,
      linkedTo = 0
    )
  ) %>%
  hc_tooltip(shared = TRUE) %>%
  hc_credits(enabled = FALSE) %>%
  hc_legend(
    title = list(text = ""),
    layout = "vertical",
    align = "right",
    verticalAlign = "top",
    x = -10,
    y = 40
  ) %>%
  my_hc_export()
