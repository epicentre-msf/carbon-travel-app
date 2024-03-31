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

dat <- import(fs::path(paths$maelle_charrier_tool, "Data", "clean", "amex_clean.rds")) %>% mutate(year = year(invoice_date))

names(dat)

fmt_n <- function(n) {
  
  if (is.na(n)) {
    out <- "(Unknown)"
    return(out)
    
  } else if (n < 1000) {
    out <-  n
    
    return(out)
    
  } else {
    out <- scales::number(
      n,
      accuracy = .1,
      scale_cut = c(0, K = 1e3, M = 1e6))
    
    return( stringr::str_remove(out, "\\.0"))
  }
}
fmt_n <- Vectorize(fmt_n)
fmt_n(dat$emission)

mean(dat$emission)

# Time - series -------------------------------------------------------------------------

group <- "no grouping"

df <- dat %>% 
  
  mutate(date_group = format(floor_date(invoice_date, "month"), "%Y-%m")) %>%  
  
  summarise(.by = c(date_group), 
            n_flights = n(), 
            dist_km = sum(distance_km), 
            dist_miles = sum(distance_miles), 
            gross_amount = sum(gross_amount),
            emission = round(digits = 1, sum(emission))
  ) %>% 
  mutate(label = fmt_n(emission)) %>% 
  
  arrange(date_group)

#1. Emissions 
hchart(df,
       "column", 
       hcaes(x = date_group, 
             y = emission )) %>% 
  
  hc_tooltip(useHTML = TRUE,
             formatter = JS("
    function(){
    outHTML =  '<b>' + this.point.label
     return(outHTML)
     
     }")
  ) 




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


# Distribution  -------------------------------------------------------------------------



hchart(filter(dat, year == "2019")$emission) %>% 
  hc_add_series(type = "histogram", filter(dat, year == "2020")$emission) 
hc_add_series(filter(dat, year == "2021")$emission) %>% 
  hc_add_series(filter(dat, year == "2022")$emission) %>% 
  hc_add_series(filter(dat, year == "2023")$emission) %>% 
  hc_add_series(filter(dat, year == "2024")$emission) %>% 
  
  hc_tooltip(share = TRUE)






