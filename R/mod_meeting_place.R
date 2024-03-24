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
                                     df_origin,
                                     msf_only = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    msf_type_select <- paste(c("MSF office", "MSF HQ OC"), collapse = "|")
    
    df_dists <- reactive({
      req(df_origin())
      ntf <- showNotification("Calculating optimal meeting locations", duration = NULL)
      on.exit(removeNotification(ntf))
      
      if (msf_only) {
        dest_possible <- air_msf %>% filter(str_detect(msf_type, msf_type_select))
      } else {
        dest_possible <- air_msf
      }
      dest_list <- dest_possible %>%
        distinct(city_id) %>%
        pull()
      
      all_dist <- purrr::map(
        dest_list,
        ~ get_dest_tot(
          df_origin(),
          .x,
          df_conversion,
          mat
        )
      ) %>%
        bind_rows() %>%
        distinct(destination, .keep_all = TRUE) %>%
        arrange(grand_tot_emission) %>%
        mutate(rank = row_number()) %>%
        relocate(rank, 1) %>%
        left_join(., select(
          air_msf,
          oc,
          msf_type,
          city_id
        ), by = c("destination" = "city_id")) %>%
        separate(destination, c("Country", "City"), "-") %>%
        mutate(across(c(Country, City), str_to_title))
    })
    
    output$tbl <- gt::render_gt({
      req(df_dists())
      df_dists() %>%
        head(n = 10) %>%
        gt() %>%
        tab_header(title = "Optimal Meeting Locations with an MSF office") %>% 
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
          grand_tot_km ~ "Total distance (km)",
          grand_tot_emission ~ "Total CO2 emissions (CO2e)",
          oc ~ "OC",
          msf_type ~ "Location"
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

# function to
# 1) calculate the distance and total distance using N participant for a specific destination
# 2) sums the grand distance total for a destination

get_dest_tot <- function(df_origin,
                         destination,
                         df_conversion,
                         mat) {
  matrix_city <- colnames(mat)
  
  if (destination %in% matrix_city == FALSE) {
    stop(paste0(destination, " is not in the distance matrix"))
  }
  
  distances <- unname(mat[df_origin$origin_id, destination])
  
  df_details <- df_origin %>%
    mutate(
      destination = destination,
      distance = distances,
      distance_km = distance / 1000,
      total_distance_km = n_participant * distance_km,
      distance_cat = case_when(
        distance_km <= 999 ~ "short",
        distance_km >= 3500 ~ "long",
        .default = "medium"
      )
    ) %>%
    left_join(
      df_conversion %>% select(distance_cat, emissions_factor = co2e),
      by = "distance_cat"
    ) %>%
    mutate(
      trip_emissions = round(digits = 3, distance_km * emissions_factor),
      total_emissions = n_participant * trip_emissions
    )
  
  grand_df <- df_details %>%
    group_by(destination) %>%
    summarise(
      grand_tot_km = sum(total_distance_km),
      grand_tot_emission = sum(total_emissions)
    )
  
  return(grand_df)
}