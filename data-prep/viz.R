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
source("R/utils.R")

paths <- set_paths()

# Import amex clean data 

dat <- import(fs::path(paths$maelle_charrier_tool, "Data", "clean", "amex_clean.rds")) %>% mutate(year = year(invoice_date))

names(dat)

# Time - series -------------------------------------------------------------------------

group_var <- "org"


df <- dat %>% 
  
  mutate(date_group = format(floor_date(invoice_date, "month"), "%Y-%m"),
         date_group = fct_relevel(date_group)
  ) %>% 
  
  summarise(.by = c(org, date_group), 
            n_flights = n(), 
            dist_km = sum(distance_km), 
            dist_miles = sum(distance_miles), 
            gross_amount = sum(gross_amount),
            emission = round(digits = 1, sum(emission))
  ) %>% 
  
  mutate(date_group = fct_relevel(date_group)) %>% 
  
  arrange(date_group) %>% 
  
  mutate(.by = org,
         label = fmt_n(emission), 
         cumm_sum = cumsum(emission))





#1. Emissions 

highchart() %>% 
  
  hc_yAxis_multiples(
    list(lineWidth = 3),
    list(showLastLabel = FALSE, 
         opposite = TRUE)) %>% 
  
  hc_add_series(df,
                "spline", 
                hcaes(x = date_group, 
                      y = cumm_sum, 
                      group = org), 
                showInLegend = TRUE) %>% 
  
  hc_add_series(df, 
                "column", 
                hcaes(x = date_group, 
                      y = emission, 
                      group = org),
                yAxis = 0, 
                showInLegend = TRUE) %>% 
  
  
  hc_xAxis(categories = levels(df$date_group))



# Distribution  -------------------------------------------------------------------------



hchart(filter(dat, year == "2019")$emission) %>% 
  hc_add_series(type = "histogram", filter(dat, year == "2020")$emission) 
hc_add_series(filter(dat, year == "2021")$emission) %>% 
  hc_add_series(filter(dat, year == "2022")$emission) %>% 
  hc_add_series(filter(dat, year == "2023")$emission) %>% 
  hc_add_series(filter(dat, year == "2024")$emission) %>% 
  
  hc_tooltip(share = TRUE)

# Global parts ================== ================== ================== ================== ==================

group <- "reason_travel"

group_sym <- sym(group)

var <- "emission"
var_sym <- sym(var)

df <- dat %>% 
  drop_na(!!group_sym) %>% 
  
  summarise(.by = c(!!group_sym), 
            n_flights = n(), 
            dist_km = sum(distance_km), 
            dist_miles = sum(distance_miles), 
            gross_amount = sum(gross_amount),
            emission = round(digits = 1, sum(emission))
  ) %>% 
  
  mutate(label = fmt_n(emission), 
         percent = scales::percent(!!var_sym/sum(!!var_sym), accuracy = .1 ) ) %>% 
  
  rename(group_var = all_of(group)) %>% 
  
  arrange(desc(group_var)) 



hchart(df, 
       "column", 
       hcaes(x = group_var, 
             y = emission)) %>% 
  
  hc_tooltip(useHTML = TRUE,
             formatter = JS("
    function(){
    outHTML =  '<b><i>' + this.point.group_var + '</i> <br>' + this.point.label + ' (' + this.point.percent + ')' 
     return(outHTML)
     
     }")
  )

# Geo Table ================== ================== ================== ================== ==================

geo_tab <- dat %>% 
  summarise(.by = dest, 
            n_flights = n(), 
            emission = sum(emission), 
            main_org = org[max(n())]
            
  ) %>%
  
  mutate(emission_pct = scales::percent(emission / sum(emission)), 
         emission = fmt_n(emission), 
         emission = paste0(emission, " (", emission_pct, ")")) %>% 
  
  select(-emission_pct) %>% 
  
  arrange(desc(n_flights)) %>% 
  
  head(n = 20)

reactable(geo_tab, 
          highlight = TRUE,
          searchable = TRUE,
          compact = TRUE,
          
          defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE, locales = "fr-Fr")),
          columns = list(
            dest = colDef("Destination", align = "left"),
            n_flights = colDef("N Flights"), 
            emission = colDef("Emissions (tCO2e)"), 
            main_org = colDef("Main organisation") 
          ) )

