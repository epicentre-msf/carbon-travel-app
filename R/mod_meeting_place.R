mod_meeting_place_ui <- function(id) {
  ns <- NS(id)
  tagList(
    gt::gt_output(ns("tbl"))
  )
}

mod_meeting_place_server <- function(id,
                                     mat,
                                     air_msf,
                                     df_conversion,
                                     df_origin
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter the destinations to map on, this will also update choices for destination selector
    
    
    dest_fil <- reactive( {
      
      browser()
      
      msf_type <- paste(input$msf_type_select, collapse = "|")
      
      if(input$msf_all == "msf") {
        
        dest |> filter(msf) |> filter(str_detect(msf_type, pattern = msf_type() )) 
        
      } else {
        
        dest |> filter( str_detect(msf_type, pattern = msf_type() )) 
      }
      
    } )
    
    df_dists <- reactive({
      
      req(df_origin())
      ntf <- showNotification("Calculating optimal meeting locations", duration = NULL)
      on.exit(removeNotification(ntf))
      
      # Map the function over all possible destinations 
      all_dest <- purrr::map(
        
        #the desired destinations - for now include all of them. Add filters to this if you wish
        dest_fil()$city_code,
        ~ get_dest_tot(
          df_origin(),
          destination = .x,
          df_conversion = conversion_df,
          dist_mat = mat
        )
      )
      #name the lists elements
      names(all_dest) <- dest_fil()$city_code
      
      # Bind the grand totals together arrange and display
      grand_tot <- purrr::map(dest_fil()$city_code, 
                              ~ all_dest[[.x]]$total) |> 
        bind_rows() |> 
        arrange(grand_tot_emission) |> 
        mutate(rank = row_number()) |> 
        relocate(rank, 1) |> 
        left_join(select(dest_fil(),
                         city_code,
                         city_name,
                         country_name
        ), 
        by = c("name_dest" = "city_code")) |> 
        
        select(rank, 
               city_name, 
               country_name, 
               grand_tot_km,
               grand_tot_emission,
               oc,
               msf_type
        )
      
    })
    
    #   all_dist <- purrr::map(
    #     dest_list,
    #     ~ get_dest_tot(
    #       df_origin(),
    #       .x,
    #       df_conversion,
    #       mat
    #     )
    #   ) %>%
    #     bind_rows() %>%
    #     distinct(destination, .keep_all = TRUE) %>%
    #     arrange(grand_tot_emission) %>%
    #     mutate(rank = row_number()) %>%
    #     relocate(rank, 1) %>%
    #     left_join(., select(
    #       air_msf,
    #       oc,
    #       msf_type,
    #       city_id
    #     ), by = c("destination" = "city_id")) %>%
    #     separate(destination, c("Country", "City"), "-") %>%
    #     mutate(across(c(Country, City), str_to_title))
    # })
    
    output$tbl <- gt::render_gt({
      req(df_dists())
      df_dists() %>%
        #add a filter to number of row to show - reactable ?
        head(n = 10) %>%
        gt() %>%
        tab_header(title = "Optimal Meeting Locations") %>% 
        data_color(
          columns = grand_tot_emission,
          method = "numeric",
          palette = c("white", "orange"),
          reverse = FALSE
        ) %>%
        fmt_number(
          sep_mark = " ",
          columns = c(grand_tot_km, grand_tot_emission)
        ) %>%
        cols_label(
          rank ~ "Rank",
          city_name ~"City name", 
          country_name ~"Country name",
          grand_tot_km ~ "Total distance (km)",
          grand_tot_emission ~ "Total CO2 emissions (CO2e)",
          oc ~ "OC",
          msf_type ~ "MSF type"
        ) %>%
        tab_footnote(
          location = cells_column_labels("grand_tot_emission"),
          "carbon dioxide equivalent with radiative forcing"
        ) %>%
        tab_footnote(
          location = cells_column_labels("grand_tot_km"),
          "Calculated using one way travel to destination"
        )
    })
  })
}

# Function to retrieve the distance/emissions/path to a destination
# gives a grand total df and the details df 

get_dest_tot <- function(df_origin,
                         destination,
                         df_conversion,
                         dist_mat) {
  
  distances <- unname(dist_mat[df_origin$origin_id, destination])
  
  df_details <- df_origin |>
    
    mutate(
      name_dest = destination,
      #path = purrr::map2_chr(origin_id, destination, ~ paste(sfnetworks::st_network_paths(net, from = .x, to = .y )$node_paths[[1]], collapse = "-")),
      distance_km = distances,
      total_distance_km = n_participant * distance_km,
      distance_cat = case_when(
        distance_km <= 999 ~ "short",
        distance_km >= 3500 ~ "long",
        .default = "medium"
      )
    ) |>
    left_join(
      df_conversion |> select(distance_cat, emissions_factor = co2e),
      by = "distance_cat"
    ) |>
    mutate(
      trip_emissions = round(digits = 3, distance_km * emissions_factor),
      total_emissions = n_participant * trip_emissions
    )
  
  grand_df <- df_details |>
    group_by(name_dest) |>
    summarise(
      grand_tot_km = sum(total_distance_km),
      grand_tot_emission = sum(total_emissions)
    )
  
  ls <- list(details = df_details, 
             total = grand_df)
  
  return(ls)
  
  names(ls) <- destination
}
