mod_travel_analysis_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Travel Data Analysis",
    icon = bsicons::bs_icon("bar-chart-line-fill"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        id = ns("sb"),
        gap = 0,
        bg = "#eaeaea",
        bslib::layout_columns(
          col_widths = 12,
          shinyWidgets::sliderTextInput(
            inputId = ns("date_range"),
            label = "Time period",
            choices = init_date_range,
            selected = c(min_date, max_date),
            grid = FALSE,
            animate = FALSE,
            width = "95%"
          ),
          shiny::selectizeInput(
            inputId = ns("select_travel_type"),
            label = "Travel Type",
            choices = travel_type_var,
            multiple = TRUE,
            options = list(placeholder = "All", plugins = "remove_button")
          ),
          shiny::selectizeInput(
            inputId = ns("select_org"),
            label = "Organisation",
            choices = unique(df_travels$org),
            multiple = TRUE,
            options = list(placeholder = "All", plugins = "remove_button")
          ),
          shiny::selectizeInput(
            inputId = ns("select_type"),
            label = "Contract type",
            choices = unique(df_travels$hq_flying_mission),
            multiple = TRUE,
            options = list(placeholder = "All", plugins = "remove_button")
          ),
          bslib::input_task_button(
            ns("go"),
            "Filter data",
            icon = icon("filter"),
            width = "100%",
            class = "btn-primary"
          ),
          shiny::downloadButton(
            outputId = ns("download"),
            label = "Download data",
            width = "100%"
          )
        )
      ),
      
      # VALUE BOXES ========================================================
      layout_column_wrap(
        width = 1 / 4,
        fill = FALSE,
        value_box(
          title = "travels",
          theme = "primary",
          class = "vb",
          value = textOutput(ns("travel")),
          uiOutput(ns("travel_info"))
        ),
        
        value_box(
          title = "Total Distance",
          theme = "primary",
          class = "vb",
          value = textOutput(ns("dist")),
          uiOutput(ns("dist_info"))
        ),
        value_box(
          title = "Total Emissions",
          theme = "primary",
          class = "vb",
          value = textOutput(ns("emission")),
          uiOutput(ns("emission_info"))
        ),
        value_box(
          title = "Routes",
          theme = "primary",
          class = "vb",
          value = textOutput(ns("segment")),
          uiOutput(ns("segment_info"))
        )
      ),
      
      # MAP & DEST TABLE ========================================================
      layout_column_wrap(
        width = 1 / 1,
        navset_card_tab(
          full_screen = TRUE,
          wrapper = \(...) {
            bslib::card_body(..., padding = 0)
          },
          id = ns("geo_tabs"),
          title = div(
            class = "d-flex justify-content-between align-items-center",
            tags$span(
              class = "pe-2",
              tagList(shiny::icon("earth-africa"), "Map of flights destinations & origins")
            )
          ),
          nav_panel(
            title = shiny::icon("map"),
            value = "map",
            leaflet::leafletOutput(ns("map"))
          ),
          nav_panel(
            title = shiny::icon("table"),
            value = "table",
            reactableOutput(ns("table"))
          )
        )
      ),
      
      # TIME-SERIE & BOXPLOT ========================================================
      layout_column_wrap(
        width = 1 / 1,
        navset_card_tab(
          full_screen = TRUE,
          wrapper = \(...) {
            bslib::card_body(..., padding = 0)
          },
          id = ns("time_tabs"),
          title = div(
            class = "d-flex justify-content-between align-items-center",
            tags$span(
              class = "pe-2",
              tagList(shiny::icon("clock"), "Distributions")
            ),
            div(
              class = "pe-2",
              bslib::popover(
                trigger = actionButton(
                  ns("dropdown"),
                  icon = shiny::icon("sliders"),
                  label = "Options",
                  class = "btn-light btn-sm pe-2 me-2"
                ),
                shinyWidgets::radioGroupButtons(
                  ns("date_interval"),
                  label = "Date Interval",
                  size = "sm",
                  status = "outline-dark",
                  choices = date_intervals
                ),
                shiny::selectizeInput(
                  ns("select_year"),
                  label = "Year",
                  choices = c(purrr::set_names("All years", NULL), init_year),
                  selected = NULL,
                  multiple = FALSE,
                  width = "100%"
                ),
                shiny::selectizeInput(
                  ns("display"),
                  label = "Display",
                  choices = display_var,
                  selected = display_var[[1]],
                  multiple = FALSE,
                  width = "95%"
                ),
                shiny::selectizeInput(
                  inputId = ns("group"),
                  label = "Group",
                  choices = c(purrr::set_names("no grouping", NULL), group_vars[group_vars != "mission_country_name"]),
                  multiple = FALSE,
                  width = "95%"
                ),
                shiny::checkboxInput(
                  ns("cumulative"),
                  "Show cumulative data ?",
                  value = FALSE,
                  width = "100%"
                ),
              )
            ),
          ),
          nav_panel(
            title = shiny::icon("chart-line"),
            value = "boxplot",
            highcharter::highchartOutput(ns("time_serie"))
          ),
          nav_panel(
            title = shiny::icon("chart-column"),
            value = "boxplot",
            highcharter::highchartOutput(ns("dist_boxplot"))
          ),
        )
      ),
      layout_column_wrap(
        width = 1 / 2,
        
        # EMISSONS RATIOS ========================================================
        bslib::card(
          full_screen = FALSE,
          bslib::card_header(
            class = "d-flex align-items-center",
            "Emisions ratios"
          ),
          div(
            class = "pe-2",
            shiny::selectizeInput(
              ns("select_travel_type_ratio"),
              label = "Display",
              choices = travel_type_var,
              multiple = FALSE,
              width = "50%"
            ) 
          ),
          reactableOutput(ns("ratio_tab"))
        ),
        
        # BAR PLOT ========================================================
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "d-flex align-items-center",
            # title
            "Bar plots",
            bslib::popover(
              trigger = actionButton(
                ns("dropdown"),
                icon = shiny::icon("sliders"),
                label = "Options",
                class = "btn-light btn-sm pe-2 me-2"
              ),
              shiny::selectizeInput(
                ns("bar_var"),
                label = "Display",
                choices = display_var,
                selected = display_var[[1]],
                multiple = FALSE
              ),
              shiny::selectizeInput(
                ns("bar_group"),
                label = "Group",
                choices = bar_group,
                selected = bar_group[[1]],
                multiple = FALSE
              )
            )
          ),
          highchartOutput(ns("barplot"))
        )
      )
    )
  )
}

mod_travel_analysis_server <- function(
    id,
    df_travels,
    is_mobile
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter Organisation anc contract type
    travel_org <- reactive({
      df_out <- df_travels
      if (length(input$select_org)) {
        df_out <- df_out |> filter(org %in% input$select_org)
      } 
      if (length(input$select_type)) {
        df_out <- df_out |> filter(hq_flying_mission %in% input$select_type)
      } 
      
      if (length(input$select_travel_type)) {
        df_out <- df_out |> filter(travel_type %in% input$select_travel_type)
      } 
      
      df_out
    })
    
    # Filter travel_org with date and reason value
    travel_ready <- reactive({
      date <- as.Date(paste0(input$date_range, "-01"))
      # move date[2] to last day of month to capture everything in that month
      date[2] <- ceiling_date(date[2], unit = "month") - days(1)
      
      df <- travel_org() |>
        filter(invoice_date >= date[1], invoice_date <= date[2])
      
      return(df)
    }) |> bindEvent(input$go, ignoreNULL = FALSE)
    
    # Summary travel_ready() for value boxes
    travel_summary <- reactive({
      req(travel_ready(), !is.null(is_mobile()))
      on.exit({
        if (is_mobile()) {
          toggle_sidebar(id = "sb", open = FALSE)
        }
      })
    
      main_segment <- travel_ready() |> 
        
        filter(!is.na(ori_city_code), 
               !is.na(dest_city_code)) |> 
        
        count(ori_city_name, dest_city_name) |>
        mutate(segment = paste(ori_city_name, dest_city_name, sep = "-")) |>
        arrange(desc(n))
      
      dat_summ <- travel_ready() |>
        summarise(
          n_travel = frmt_num(n()),
          n_flight = paste0(frmt_num(sum(travel_type == "air")), " (", round(digits = 1, sum(travel_type == "air")/n() *100 ), "%)"), 
          n_segment = nrow(main_segment),
          main_seg = main_segment |> filter(row_number() == 1) |> pull(segment),
          main_seg_n = main_segment |> filter(row_number() == 1) |> pull(n),
          tot_distance_miles = frmt_num(sum(distance_miles, na.rm = TRUE)),
          tot_distance_km = round(digits = 2, sum(distance_km, na.rm = TRUE) ),
          tot_distance_km_fmt = frmt_num(tot_distance_km),
          tot_emissions = round(digits = 2, sum(emission, na.rm = TRUE)),
          tot_emissions_fmt = frmt_num(tot_emissions),
          emission_km = round(digits = 10, sum(emission, na.rm = TRUE) / sum(distance_km, na.rm = TRUE))
        )
      
      return(dat_summ)
    })
    
    
    # VALUE BOXES ============================
    
    output$travel <- renderText({
      req(travel_summary())
      paste(travel_summary()$n_travel, " Travels")
    })
    
    output$travel_info <- renderUI({
      req(travel_summary())
      paste(travel_summary()$n_flight, " Flights")
    })
    
    output$segment <- renderText({
      req(travel_summary())
      paste(travel_summary()$n_segment, " unique")
    })
    
    output$segment_info <- renderUI({
      
      req(travel_summary())
      tags$small(glue::glue("{travel_summary()$main_seg} is most travelled"))
    })
    
    output$dist <- renderText({
      req(travel_summary())
      paste(travel_summary()$tot_distance_km_fmt, " km")
    })
    
    output$dist_info <- renderUI({
      
      req(travel_summary())
      tags$small(glue::glue("{fmt_n(round(digits = 2, travel_summary()$tot_distance_km/40000))} times the Earth's circumference !"))
    })
    
    output$emission <- renderText({
      req(travel_summary())
      paste(travel_summary()$tot_emissions_fmt, " tCO2e")
    })
    
    output$emission_info <- renderUI({
      req(travel_summary())
      tags$small(glue::glue("{fmt_n(round(digits = 2, travel_summary()$tot_emissions * 0.013 ) )} tanker trucks worth of gasoline !"))
    })
    
    # Ratio table  ===========================================
    
    output$ratio_tab <- renderReactable({
      
      df <- travel_ready() |> filter(travel_type == input$select_travel_type_ratio)
      
      validate(need(nrow(df) > 0, "No data available"))
      
      dat_year <- df |> 
        
        summarise(
          .by = year,
          emissions = round(sum(emission, na.rm = TRUE), digits = 0),
          emissions_kg = emissions * 1000,
          spent = sum(gross_amount, na.rm = TRUE),
          tot_km = sum(distance_km, na.rm = TRUE),
          tot_miles = sum(distance_miles, na.rm = TRUE),
          flights = n(),
          passengers = n_distinct(traveler_name),
          em_km = round(emissions_kg / tot_km, digits = 2),
          em_spent = round(emissions_kg / spent, digits = 2),
          em_flights = round(emissions_kg / flights, digits = 2),
          em_passengers = round(emissions_kg / passengers, digits = 2)
        )  |> 
        select(year, emissions, contains("em_"))
      
      dat_tot <- travel_ready() |>
        summarise(
          emissions = round(sum(emission, na.rm = TRUE), digits = 0),
          emissions_kg = emissions * 1000,
          spent = sum(gross_amount, na.rm = TRUE),
          tot_km = sum(distance_km, na.rm = TRUE),
          tot_miles = sum(distance_miles, na.rm = TRUE),
          flights = n(),
          passengers = n_distinct(traveler_name),
          em_km = round(emissions_kg / tot_km, digits = 2),
          em_spent = round(emissions_kg / spent, digits = 2),
          em_flights = round(emissions_kg / flights, digits = 2),
          em_passengers = round(emissions_kg / passengers, digits = 2)
        ) |>
        mutate(year = "Global") |>
        select(year, emissions, contains("em_"))
      
      dat <- bind_rows(dat_year, dat_tot) |> mutate(year = fct_relevel(year, c("Global")))
      
      reactable(
        arrange(dat, desc(year)),
        highlight = TRUE,
        compact = TRUE,
        defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE, locales = "fr-Fr")),
        columns = list(
          year = colDef("Year", align = "left", maxWidth = 55),
          emissions = colDef("Emissions (tC02e)", align = "left", maxWidth = 120),
          em_km = colDef("kg CO2e per km", align = "left", maxWidth = 80),
          # em_miles = colDef("per miles", align = "left"),
          em_spent = colDef("kg CO2e per €", align = "left", maxWidth = 80),
          em_flights = colDef("kg CO2e per flights", align = "left", maxWidth = 80),
          em_passengers = colDef("kg CO2e per traveller", align = "left", maxWidth = 90)
        )
      )
    })
    
    # Map  ===========================================
    df_origin <- reactive({
      
      travel_ready() |>
        filter(travel_type == "air") |> 
        summarise(
          .by = c(ori_city_code, ori_city_name),
          ori_lon = unique(ori_city_lon, na.rm = TRUE),
          ori_lat = unique(ori_city_lat, na.rm = TRUE),
          n = n()
        ) |>
        tidyr::drop_na()
    })
    
    df_destination <- reactive({
      travel_ready() |> 
        filter(travel_type == "air") |> 
        summarise(
          .by = c(dest_city_code, dest_city_name),
          dest_lon = unique(dest_city_lon, na.rm = TRUE),
          dest_lat = unique(dest_city_lat, na.rm = TRUE),
          n = n()
        ) |>
        tidyr::drop_na()
    })
    
    output$map <- leaflet::renderLeaflet({
      
      
      validate(need(nrow(df_origin()) > 0, "No data available"))
      
      leaflet::leaflet() |>
        leaflet::setView(0, 10, zoom = 2) |>
        leaflet::addMapPane(name = "circles", zIndex = 410) |>
        leaflet::addMapPane(name = "place_labels", zIndex = 450) |>
        leaflet::addProviderTiles("CartoDB.Positron", group = "Light") |>
        # leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = FALSE)) |>
        leaflet.extras::addFullscreenControl(position = "topleft") |>
        leaflet.extras::addResetMapButton() |>
        leaflet::addLayersControl(
          baseGroups = c("Origin", "Destination"),
          position = "topright",
          options = layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::addCircleMarkers(
          data = df_origin(),
          lng = ~ori_lon,
          lat = ~ori_lat,
          radius = ~ calc_radius(n),
          fillColor = "steelblue",
          fillOpacity = 0.8,
          weight = 1,
          color = "#FFFFFF",
          label = ~ paste(ori_city_name, n, "flights"),
          group = "Origin",
          options = leaflet::pathOptions(pane = "circles")
        ) |>
        leaflet::addCircleMarkers(
          data = df_destination(),
          lng = ~dest_lon,
          lat = ~dest_lat,
          radius = ~ calc_radius(n),
          fillColor = "red",
          fillOpacity = 0.8,
          weight = 1,
          color = "#FFFFFF",
          label = ~ paste(dest_city_name, n, "flights"),
          group = "Destination",
          options = leaflet::pathOptions(pane = "circles")
        )
    })
    
    # Time-Series ===========================================
    
    # Prepare data
    hc_df <- reactive({
      y_var <- sym(input$display)
      
      if (input$select_year != "All years") {
        df <- travel_ready() |>
          filter(year == input$select_year)
      } else {
        df <- travel_ready()
      }
      
      # Set filters
      if (input$group != "no grouping") {
        group_sym <- sym(input$group)
      } else {
        group_sym <- NULL
      }
      
      df_hist <- df |>
        rename("date_group" = input$date_interval) |>
        mutate(date_group = fct_relevel(as.character(date_group))) |>
        summarise(
          .by = c(!!group_sym, date_group),
          n_flights = n(),
          distance_km = round(sum(distance_km, na.rm = TRUE), digits = 1),
          distance_miles = round(sum(distance_miles, na.rm = TRUE), digits = 1),
          gross_amount = round(sum(gross_amount, na.rm = TRUE), digits = 1),
          emission = round(sum(emission, na.rm = TRUE), digits = 1)
        ) |>
        arrange(date_group) |>
        mutate(
          lab = fmt_n(!!y_var),
          n_c = cumsum(!!y_var)
        )
      
      return(df_hist)
    })
    
    output$time_serie <- renderHighchart({
      
      validate(need(nrow(hc_df()) > 0, "No data available"))
      
      # Set filters
      if (input$group != "no grouping") {
        group_sym <- sym(input$group)
      } else {
        group_sym <- NULL
      }
      
      n_var <- dplyr::if_else(input$cumulative, "n_c", input$display)
      n_lab <- names(display_var[display_var == input$display])
      
      if (input$group == "no grouping") {
        base_hc <- hchart(
          hc_df(),
          "column",
          name = n_lab,
          hcaes(
            x = date_group,
            y = !!sym(n_var)
          )
        )
      } else {
        # Not in right x order if group is org
        base_hc <- hchart(
          hc_df(),
          "column",
          hcaes(
            x = date_group,
            y = !!sym(n_var),
            group = !!group_sym
          )
        )
      }
      
      base_hc |>
        hc_xAxis(
          title = list(text = str_to_sentence(input$date_interval)),
          categories = levels(hc_df()$date_group),
          crosshair = TRUE
        ) |>
        hc_yAxis(title = list(text = n_lab)) |>
        hc_tooltip(
          shared = TRUE
          # useHTML = TRUE,
          # formatter = JS("
          # function(){
          #   outHTML =  '<i>' + this.point.date_group +'</i><b><br>' + this.point.lab + '</b>'
          #   return(outHTML)
          # }")
        )
    })
    
    
    # Distributions =======================================
    
    # observe Event for distribution year input
    
    observeEvent(input$date_range, {
      year_choices <- sort(unique(travel_ready()$year), decreasing = TRUE)
      
      shiny::updateSelectizeInput(
        "dist_year",
        choices = c(purrr::set_names("All years", NULL), year_choices),
        session = session
      )
    })
    
    # Histograms
    output$dist_hist <- renderHighchart({
      
      validate(need(nrow(travel_ready()) > 0, "No data available"))
      
      dist_var_sym <- sym(input$dist_var)
      
      if (input$dist_year != "All years") {
        hc_var <- travel_ready() |>
          filter(year == input$dist_year) |>
          pull(!!dist_var_sym)
      } else {
        hc_var <- travel_ready() |>
          pull(!!dist_var_sym)
      }
      
      hchart(
        hc_var,
        name = names(display_var[display_var == input$dist_var])
      ) |>
        hc_xAxis(
          plotLines = list(
            list(
              color = "red",
              zIndex = 1,
              value = median(hc_var),
              label = list(text = paste("Median", median(hc_var), "days"), verticalAlign = "bottom", textAlign = "left")
            )
          )
        )
    })
    
    # Boxplot
    output$dist_boxplot <- renderHighchart({
      
      validate(need(nrow(travel_ready()) > 0, "No data available"))
      
      dist_var_sym <- sym(input$display)
      
      date_interval_sym <- sym(input$date_interval)
      
      dist_group_var2 <- sym(input$group)
      
      if (input$select_year != "All years") {
        travel_box <- travel_ready() |>
          filter(year == input$select_year)
      } else {
        travel_box <- travel_ready()
      }
      
      box_df <- data_to_boxplot(
        travel_box,
        !!dist_var_sym,
        group_var = !!date_interval_sym,
        #group_var2 = !!dist_group_var2,
        name = names(display_var[display_var == input$display])
        # showInLegend = FALSE
      )
      
      highchart() |>
        hc_chart(zoomType = "x") |>
        hc_xAxis(
          type = "category",
          crosshair = TRUE
        ) |>
        hc_yAxis(
          title = list(text = names(display_var[display_var == input$display]))
        ) |>
        hc_add_series_list(box_df) |>
        hc_tooltip(shared = TRUE)
    })
    
    # Global parts ========================================
    output$barplot <- renderHighchart({
      
      validate(need(nrow(travel_ready()) > 0, "No data available"))
      
      bar_var_sym <- sym(input$bar_var)
      
      bar_group_sym <- sym(input$bar_group)
      
      hc_df <- travel_ready() |>
        # filter out Nas for group var
        drop_na(!!bar_group_sym) |>
        summarise(
          .by = c(!!bar_group_sym),
          n_flights = n(),
          distance_km = sum(distance_km, na.rm = TRUE),
          distance_miles = sum(distance_miles, na.rm = TRUE),
          gross_amount = sum(gross_amount, na.rm = TRUE),
          emission = round(digits = 1, sum(emission, na.rm = TRUE))
        ) |>
        mutate(
          label = fmt_n(!!bar_var_sym),
          percent = scales::percent(!!bar_var_sym / sum(!!bar_var_sym, na.rm = TRUE), accuracy = .1)
        ) |>
        rename("group_var" = input$bar_group) |>
        arrange(desc(!!bar_var_sym))
      
      hchart(
        hc_df,
        "column",
        hcaes(
          x = group_var,
          y = !!bar_var_sym,
          percent = percent,
          label = label
        )
      ) |>
        hc_chart(
          scrollablePlotArea = list(minHeight = 20 * n_distinct(hc_df$group_var))
        ) |>
        hc_yAxis(title = list(text = names(display_var[display_var == input$bar_var]))) |>
        hc_xAxis(
          title = list(text = names(bar_group[bar_group == input$bar_group])),
          labels = list(step = 1),
          tickInterval = 1,
          crosshair = TRUE
        ) |>
        hc_tooltip(
          shared = TRUE,
          pointFormat = "<b>{point.label} ({point.percent:.1f})</b><br/>"
          # useHTML = TRUE,
          # formatter = JS("
          # function(){
          #   outHTML =  '<i>' + this.point.group_var + '</i> <br> <b>' + this.point.label + ' (' + this.point.percent + ')</b>'
          #   return(outHTML)
          # }")
        ) |>
        hc_chart(inverted = TRUE)
    })
    
    # GEO TABLE ==========================================
    
    output$table <- renderReactable({
      validate(need(nrow(travel_ready()) > 0, "No data available"))
      geo_tab <- travel_ready() |>
        summarise(
          .by = c(dest_city_name),
          n_flights = n(),
          emission = sum(emission, na.rm = TRUE),
          main_org = org[max(n())]
        ) |>
        mutate(
          emission_pct = scales::percent(emission / sum(emission, na.rm = TRUE)),
          emission = fmt_n(emission),
          emission = paste0(emission, " (", emission_pct, ")")
        ) |>
        select(-emission_pct) |>
        arrange(desc(n_flights)) |>
        head(n = 20)
      
      reactable(
        geo_tab,
        highlight = TRUE,
        searchable = TRUE,
        compact = TRUE,
        defaultColDef = colDef(align = "center", format = colFormat(separators = TRUE)),
        columns = list(
          dest_city_name = colDef("Destination", align = "left"),
          n_flights = colDef("N Flights"),
          emission = colDef("Emissions (tCO2e)"),
          main_org = colDef("Main organisation")
        )
      )
    })
    
    # # Download  ==================================================
    output$download <- downloadHandler(
      filename = function() {
        dates <- paste(input$date_range[1], input$date_range[2], sep = "-")
        org <- if (length(input$select_org)) {
          "_filtered-org"
        } else {
          "_All-org"
        }
        paste0("travel_data_", dates, org, ".xlsx")
      },
      content = function(file) {
        qxl::qxl(travel_ready() |> select(-traveler_name), file)
      }
    )
  })
}
