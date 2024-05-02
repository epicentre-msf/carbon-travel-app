mod_travel_estim_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Single travel estimation",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 300,
        gap = 0,
        mod_origin_input_ui(ns("origin")), 
        
        actionButton(ns("go_estim"), "Get travel emissions", width = "100%", class = "btn-primary")
      ),
      
      layout_column_wrap(
        width = 1 / 2,
        
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "d-flex align-items-center",
            "Travel Emissions"
          ),
          min_height = "550px",
          reactableOutput(ns("tbl"))
        ),
        
        bslib::card(
          bslib::card_header(
            class = "d-flex align-items-center",
            "Travel map"
          ), 
          min_height = "550px",
          leaflet::leafletOutput(ns("map"))
        ) 
      )
    )
  )
}

mod_travel_estim_server <- function(
    id,
    mat,
    air_msf,
    df_conversion
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df_stop <- mod_origin_input_server("origin", orig_cities)
    
    output$tbl <- renderReactable({
      
      # build the segments 
      df <- data.frame(
        start_var = head(df_stop$origin_id, -1), 
        end_var = tail(df_stop$origin_id, - 1)
      ) |> 
        mutate(distance_km = purrr::map2_dbl(start_var, end_var, ~ unname(mat[.x, .y] ) ), 
               distance_cat = case_when(
                 distance_km <= 999 ~ "short",
                 distance_km >= 3500 ~ "long",
                 .default = "medium"
               ) ) |>
        left_join(
          conversion_df |> select(distance_cat, emissions_factor = co2e),
          by = "distance_cat"
        ) |>
        mutate(
          trip_emissions = round(digits = 3, distance_km * emissions_factor)
        ) |> 
        left_join(select(dest, start_city = city_name, start_country = country_name), by = join_by(start_var == city_code)) |> 
        left_join(select(dest, end_city = city_name, end_country = country_name), by = join_by(end_var == city_code)) |> 
        
        select(start_city, start_country, end_city, end_country, distance_km, trip_emissions) %>% 
        bindEvent(input$go_estim)
      
      #get a table 
      react_tbl <- reactable(
        df,
        highlight = TRUE,
        searchable = TRUE,
        compact = TRUE,
        defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE, locales = "fr-Fr")),
        columns = list(
          start_city = colDef("City", align = "left", maxWidth = 150),
          start_country = colDef("Country", align = "left", maxWidth = 150),
          end_city = colDef("City", align = "left", maxWidth = 150),
          end_country = colDef("Country", align = "left", maxWidth = 150),
          
          distance_km = colDef("Segment distance (Km)", align = "left", format = colFormat(digits = 0), maxWidth = 150),
          
          trip_emissions = colDef(
            "Segment Emissions (kg CO2e)",
            align = "left",
            format = colFormat(digits = 0),
            maxWidth = 150,
            style = if(nrow(df) >1) { function(value) {
              normalized <- (value - min(df$trip_emission)) / (max(df$trip_emission) - min(df$trip_emission) +1)
              color <- orange_pal(normalized)
              list(background = color)
            } } else { background = "white" }
          )
        ) )
      
      return(react_tbl)
    })
  }
  )
}
