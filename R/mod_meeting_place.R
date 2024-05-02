mod_meeting_place_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Meeting Place Planner",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 300,
        gap = 0,
        mod_origin_input_ui(ns("origin")),
        h4("Destinations"),
        hr(),
        shinyWidgets::radioGroupButtons(
          inputId = ns("msf_all"),
          label = "All destinations or only MSF ?",
          choices = c("All" = "all", "MSF" = "msf"),
          size = "sm",
          selected = "all",
          justified = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("msf_type_select"),
          label = "MSF type",
          multiple = TRUE,
          choices = msf_type_vec,
          selected = msf_type_vec
        ),
        shinyWidgets::virtualSelectInput(
          inputId = ns("select_dest"),
          label = tooltip(
            span(
              "Destinations",
              bsicons::bs_icon("info-circle")
            ),
            "Select all possible destinations for a meeting"
          ),
          choices = NULL,
          multiple = TRUE,
          search = TRUE,
          autoSelectFirstOption = TRUE,
          placeholder = "Select cities...",
          position = "bottom",
          dropboxWrapper = "body",
          showOptionsOnlyOnSearch = FALSE,
          optionsCount = 5
        ),
        hr(),
        actionButton(ns("go"), "Get meeting places", width = "100%", class = "btn-primary")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex align-items-center",
          "Optimal Meeting locations"
        ),
        min_height = "550px",
        reactableOutput(ns("tbl"))
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex align-items-center",
          "Map of travels",
          shinyWidgets::virtualSelectInput(
            inputId = ns("map_dest"),
            label = tooltip(
              span(
                "Optimal destination",
                bsicons::bs_icon("info-circle")
              ),
              "Select one of the optimal destinations to display"
            ),
            selected = 1,
            choices = NULL,
            search = TRUE,
            autoSelectFirstOption = TRUE,
            placeholder = "Select city...",
            position = "bottom",
            dropboxWrapper = "body",
            showOptionsOnlyOnSearch = FALSE,
            optionsCount = 5
          )
        ),
        min_height = "550px",
        leaflet::leafletOutput(ns("map"))
      )
    )
  )
}

mod_meeting_place_server <- function(
    id,
    mat,
    air_msf,
    df_conversion
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df_origin <- mod_origin_input_server("origin", orig_cities)
    
    # Filter the destinations to map on, this will also update choices for destination selector
    dest_fil <- reactive({
      req(input$msf_type_select)
      
      if (input$msf_all == "msf") {
        msf_type_select <- paste(input$msf_type_select, collapse = "|")
        
        dest |>
          filter(msf) |>
          filter(str_detect(msf_type, pattern = msf_type_select)) 
      } else {
        dest 
      }
    })
    
    observe({
      choices <- dest_fil() |>
        shinyWidgets::prepare_choices(
          label = city_name,
          value = city_code,
          group_by = country_name
        )
      shinyWidgets::updateVirtualSelect("select_dest", choices = choices)
    })
    
    
    df_dists <- reactive({
      req(df_origin())
      ntf <- showNotification("Calculating optimal meeting locations", duration = NULL, type = "warning")
      on.exit(removeNotification(ntf))
      
      #final selection of destinations using the picket inputs
      dest_cities <- dest_fil() |> filter(city_code %in% input$select_dest) |> pull(city_code)
      
      # Map the function over all possible destinations
      all_dest <- purrr::map(
        # the desired destinations - for now include all of them. Add filters to this if you wish
        dest_cities,
        ~ get_dest_tot(
          df_origin(),
          destination = .x,
          df_conversion = conversion_df,
          dist_mat = mat
        )
      )
      
      # name the lists elements
      names(all_dest) <- dest_cities
      
      # Bind the grand totals together arrange and display
      grand_tot <- purrr::map(
        dest_cities,
        ~ all_dest[[.x]]$total
      ) |>
        bind_rows() |>
        arrange(grand_tot_emission) |>
        mutate(rank = row_number()) |>
        relocate(rank, 1) |>
        left_join(
          select(
            dest_fil(),
            city_code,
            city_name,
            city_lon,
            city_lat,
            country_name,
            oc,
            msf_type
          ),
          by = c("name_dest" = "city_code")
        ) |>
        select(
          rank,
          city_code = name_dest,
          city_lon,
          city_lat,
          city_name,
          country_name,
          grand_tot_km,
          grand_tot_emission,
          oc,
          msf_type
        )
    }) %>% bindEvent(input$go)
    
    # Make a reactable
    orange_pal <- function(x) rgb(colorRamp(c("#B8CCAD", "#BF6C67"))(x), maxColorValue = 255)
    
    output$tbl <- reactable::renderReactable({
      req(df_dists())
      
      df <- df_dists() |>
        select(-c(city_code, city_lon, city_lat)) |>
        head(50)
      
      reactable(
        df,
        highlight = TRUE,
        searchable = TRUE,
        compact = TRUE,
        defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE, locales = "fr-Fr")),
        columns = list(
          rank = colDef("Rank", align = "left", maxWidth = 50),
          city_name = colDef("City", align = "left", maxWidth = 150),
          country_name = colDef("Country", align = "left", maxWidth = 150),
          grand_tot_km = colDef("Total Km", align = "left", format = colFormat(digits = 0), maxWidth = 150),
          grand_tot_emission = colDef(
            "Total Emissions (kg CO2e)",
            align = "left",
            format = colFormat(digits = 0),
            maxWidth = 150,
            style = if(nrow(df) >1) { function(value) {
              normalized <- (value - min(df$grand_tot_emission)) / (max(df$grand_tot_emission) - min(df$grand_tot_emission) +1)
              color <- orange_pal(normalized)
              list(background = color)
            } } else { background = "white" }
          ),
          oc = colDef("Operational Center", align = "left", maxWidth = 200),
          msf_type = colDef("MSF type", align = "left")
        )
      )
    })
    
    # Map  =========================================================================
    
    # update choices of input
    
    observeEvent(df_dists(), {
      choices <- df_dists() |>
        shinyWidgets::prepare_choices(
          label = city_name,
          value = city_code,
          group_by = country_name
        )
      shinyWidgets::updateVirtualSelect("map_dest", choices = choices)
    })
    
    output$map <- leaflet::renderLeaflet({
      req(input$map_dest)
      
      pal <- colorFactor(c("darkred", "steelblue"), c("destination", "origin"))
      
      # origins
      map_ori <- df_origin() |> left_join(dest, by = join_by(origin_id == city_code))
      
      # destination selected
      map_dest <- input$map_dest
      
      # get the shortest path in network for all origin and this destination
      short_paths <- purrr::map(
        map_ori$origin_id,
        ~ sfnetworks::st_network_paths(net, from = .x, to = map_dest)
      )
      
      short_nodes <- unique(unname(unlist(purrr::map(short_paths, ~ .x |>
                                                       pull(node_paths) |>
                                                       unlist()))))
      
      short_edges <- unname(unlist(purrr::map(short_paths, ~ .x |>
                                                pull(edge_paths) |>
                                                unlist())))
      
      nodes <- net |>
        activate("nodes") |>
        filter(name %in% short_nodes) |>
        st_as_sf() |>
        mutate(type = if_else(name == map_dest, "destination", "origin"))
      edges <- net |>
        activate("edges") |>
        slice(short_edges) |>
        st_as_sf()
      
      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron", group = "Light") |>
        leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = FALSE)) |>
        leaflet.extras::addFullscreenControl(position = "topleft") |>
        leaflet.extras::addResetMapButton() |>
        leaflet::addCircleMarkers(
          data = nodes,
          lng = ~lon,
          lat = ~lat,
          radius = 7,
          color = ~"white",
          fillOpacity = 0.8,
          weight = 1,
          fillColor = ~ pal(type),
          label = ~city_name
        ) |>
        leaflet::addPolylines(data = edges) |>
        addLegend(
          position = "topright",
          pal = pal,
          values = unique(nodes$type)
        )
    })
  })
}

# Function to retrieve the distance/emissions/path to a destination
# gives a grand total df and the details df

get_dest_tot <- function(
    df_origin,
    destination,
    df_conversion,
    dist_mat
) {
  distances <- unname(dist_mat[df_origin$origin_id, destination])
  
  df_details <- df_origin |>
    mutate(
      name_dest = destination,
      # path = purrr::map2_chr(origin_id, destination, ~ paste(sfnetworks::st_network_paths(net, from = .x, to = .y )$node_paths[[1]], collapse = "-")),
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
  
  ls <- list(
    details = df_details,
    total = grand_df
  )
  
  return(ls)
  
  names(ls) <- destination
}
