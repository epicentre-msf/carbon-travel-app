mod_travel_estim_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Single travel estimation",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        gap = 0,
        mod_stopover_input_ui(ns("travel_estim")),
        bslib::input_task_button(
          ns("go_estim"),
          "Get travel emissions",
          label_busy = "Calculating...",
          width = "100%",
          class = "btn-primary"
        )
      ),
      bslib::card(
        full_screen = TRUE,
        # min_height = 300,
        bslib::card_header(
          class = "d-flex align-items-center",
          bslib::card_title("Travel Emissions")
        ),
        bslib::card_body(
          padding = 0,
          reactableOutput(ns("tbl"))
        )
      ),
      bslib::card(
        full_screen = TRUE,
        min_height = 300,
        bslib::card_header(
          class = "d-flex align-items-center",
          bslib::card_title("Travel map")
        ),
        bslib::card_body(
          padding = 0,
          leaflet::leafletOutput(ns("map"))
        )
      )
    )
  )
}

mod_travel_estim_server <- function(id,
                                    mat,
                                    air_msf,
                                    df_conversion,
                                    network) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df_stop <- mod_stopover_input_server("travel_estim", orig_cities)

    # build the segments
    df <- reactive({
      df_stop() |>
        mutate(
          distance_km = purrr::map2_dbl(start_var, end_var, ~ unname(mat[.x, .y])),
          distance_cat = case_when(
            distance_km <= 999 ~ "short",
            distance_km >= 3500 ~ "long",
            .default = "medium"
          )
        ) |>
        left_join(
          conversion_df |> select(distance_cat, emissions_factor = co2e),
          by = "distance_cat"
        ) |>
        mutate(
          trip_emissions = round(digits = 3, distance_km * emissions_factor)
        ) |>
        left_join(select(dest, city_code, start_city = city_name, start_country = country_name), by = join_by(start_var == city_code)) |>
        left_join(select(dest, city_code, end_city = city_name, end_country = country_name), by = join_by(end_var == city_code)) |>
        select(start_var, start_city, start_country, end_var, end_city, end_country, distance_km, trip_emissions)
    }) %>%
      bindEvent(input$go_estim)

    output$tbl <- renderReactable({
      validate(
        need(input$go_estim > 0, "Select your origin, any stop-overs and destination then click 'Get travel emissions' to see results.")
      )
      req(df())

      # Make a reactable
      orange_pal <- function(x) rgb(colorRamp(c("#B8CCAD", "#BF6C67"))(x), maxColorValue = 255)

      df_tbl <- df() |>
        mutate(
          start_city = paste0(start_city, " (", start_country, ")"),
          end_city = paste0(end_city, " (", end_country, ")"),
        ) |>
        select(-c(start_var, start_country, end_var, end_country))

      # get a table
      react_tbl <- reactable(
        df_tbl,
        highlight = TRUE,
        searchable = FALSE,
        compact = TRUE,
        defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE, locales = "fr-Fr")),
        columns = list(
          start_city = colDef("Start city", align = "left", footer = htmltools::tags$b("Total")),
          end_city = colDef("End city", align = "left"),
          distance_km = colDef("Segment distance (Km)",
            align = "left",
            format = colFormat(separators = TRUE, locales = "fr-Fr", digits = 0),
            footer = function(values) {
              htmltools::tags$b(sprintf("%.0f km", sum(values)))
            }
          ),
          trip_emissions = colDef(
            "Segment Emissions (kg CO2e)",
            align = "left",
            format = colFormat(separators = TRUE, locales = "fr-Fr", digits = 0),
            footer = function(values) {
              htmltools::tags$b(sprintf("%.0f kgCO2e", sum(values)))
            },
            style = if (nrow(df_tbl > 1)) {
              function(value) {
                normalized <- (value - min(df_tbl$trip_emission)) / (max(df_tbl$trip_emission) - min(df_tbl$trip_emission) + 1)
                color <- orange_pal(normalized)
                list(background = color)
              }
            } else {
              background <- "white"
            }
          )
        )
      )
      return(react_tbl)
    })

    # Map

    output$map <- renderLeaflet({

      # browser()
      # get the shortest path in network for all origin and this destination
      short_paths <- purrr::map2(
        df()$start_var,
        df()$end_var,
        ~ sfnetworks::st_network_paths(cities_network, from = .x, to = .y)
      )

      short_nodes <- unique(unname(unlist(purrr::map(short_paths, ~ .x |>
        pull(node_paths) |>
        unlist()))))

      short_edges <- unname(unlist(purrr::map(short_paths, ~ .x |>
        pull(edge_paths) |>
        unlist())))

      nodes <- cities_network |>
        activate("nodes") |>
        filter(name %in% short_nodes) |>
        st_as_sf()
      edges <- cities_network |>
        activate("edges") |>
        slice(short_edges) |>
        st_as_sf()

      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron", group = "Light") |>
        leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = FALSE)) |>
        leaflet.extras::addFullscreenControl(position = "topleft") |>
        leaflet.extras::addResetMapButton() |>
        leaflet::addPolylines(data = edges) |>
        leaflet::addCircleMarkers(
          data = nodes,
          lng = ~lon,
          lat = ~lat,
          radius = 10,
          fillColor = ~"darkred",
          color = ~"white",
          fillOpacity = 1,
          weight = 1,
          label = ~city_name
        )
    })
  })
}
