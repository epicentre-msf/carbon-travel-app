mod_amex_ui <- function(id) {
  
  ns <- NS(id)
  
  nav_panel(
    "Flight data analysis",
    
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 300,
        gap = 0,
        
        shinyWidgets::sliderTextInput(
          inputId = ns("date_range"),
          label = "Year & Months",
          choices = init_date_range, 
          selected = c(min_date, max_date), 
          grid = FALSE,
          animate = FALSE,
          width = "95%"
        ),
        
        shiny::selectizeInput(
          inputId = ns("select_org"),
          label = "Organisation",
          choices = unique(df_amex$org),
          multiple = TRUE, 
          options = list(placeholder = "All", plugins = "remove_button")
        ), 
        
        shiny::selectizeInput(
          inputId = ns("reasons"),
          label = "Reasons for Travel",
          choices = unique(df_amex$reason_travel) %>% na.omit(),
          multiple = TRUE,
          options = list(placeholder = "All", plugins = "remove_button")
        )
      ),
      
      # VALUE BOXES ========================
      layout_column_wrap(
        width = 1/4,
        fill = FALSE,
        value_box(
          title = "Flights",
          theme = "primary",
          class = "vb",
          value = textOutput(ns("flight")),
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
        ),
        
        # value_box(
        #   title = "Main route",
        #   theme = "primary",
        #   class = "vb",
        #   value = textOutput(ns("main_segment")),
        #   uiOutput(ns("main_segment_info"))
        # )
        
        
      ), 
      
      layout_column_wrap(width = 1 / 2,
                         
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
                               tagList(shiny::icon("clock"), "Map")
                             )
                           ),
                           nav_panel(
                             title = shiny::icon("map"),
                             value = "map",
                             highcharter::highchartOutput(ns("map"))
                           ),
                           nav_panel(
                             title = shiny::icon("table"),
                             value = "table",
                             reactableOutput(ns("table"))
                           )
                           
                         ), 
                         
                         bslib::card(
                           full_screen = TRUE,
                           
                           bslib::card_header( 
                             class = "d-flex align-items-center",
                             # title
                             "Time-serie", 
                             
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
                                 choices = c(purrr::set_names("no grouping", NULL), group_vars),
                                 multiple = FALSE,
                                 width = "95%"
                               )
                             )), 
                           highchartOutput(ns("time_serie"))
                         ), 
                         
                         bslib::card(
                           full_screen = TRUE,
                           
                           bslib::card_header( 
                             class = "d-flex align-items-center",
                             # title 
                             "Bar plots", 
                             
                             shiny::selectizeInput(
                               ns("bar_var"),
                               label = "Display",
                               choices = display_var,
                               selected = display_var[[1]],
                               multiple = FALSE,
                               width = "30%"
                             ),
                             shiny::selectizeInput(
                               ns("bar_group"),
                               label = "Group",
                               choices = bar_group,
                               selected = bar_group[[1]],
                               multiple = FALSE,
                               width = "25%"
                             )),
                           
                           highchartOutput(ns("barplot")) ), 
                         
                         
                         # DISTRIBUTIONS CHARTS ========================================================
                         navset_card_tab(
                           full_screen = TRUE,
                           wrapper = \(...) {
                             bslib::card_body(..., padding = 0)
                           },
                           id = ns("dist_tabs"),
                           title = div(
                             class = "d-flex justify-content-between align-items-center",
                             tags$span(
                               class = "pe-2",
                               tagList(shiny::icon("clock"), "Distributions")
                             ),
                             
                             div(class = "pe-2",
                                 
                                 bslib::popover(
                                   trigger = actionButton(
                                     ns("dropdown"),
                                     icon = shiny::icon("sliders"),
                                     label = "Options",
                                     class = "btn-light btn-sm pe-2 me-2"
                                   ), 
                                   
                                   shinyWidgets::radioGroupButtons(
                                     ns("dist_interval"),
                                     label = "Date Interval",
                                     size = "sm",
                                     status = "outline-dark",
                                     choices = date_intervals, 
                                     selected = "year"
                                   ),
                                   
                                   shiny::selectizeInput(
                                     ns("dist_year"),
                                     label = "Year",
                                     choices = c(purrr::set_names("All years", NULL), init_year),
                                     selected = init_year[[1]],
                                     multiple = FALSE,
                                     width = "100%"
                                   ),
                                   
                                   shiny::selectizeInput(
                                     ns("dist_var"),
                                     label = "Display",
                                     choices = dist_var,
                                     selected = dist_var[[1]],
                                     multiple = FALSE,
                                     width = "100%"
                                   )
                                 )
                             )
                           ),
                           nav_panel(
                             title = shiny::icon("chart-line"),
                             value = "boxplot",
                             highcharter::highchartOutput(ns("dist_boxplot"))
                           ),
                           nav_panel(
                             title = shiny::icon("chart-column"),
                             value = "chart",
                             highcharter::highchartOutput(ns("dist_hist"))
                           )
                           
                         )
                         
      )
      
    )
    
  )
  
}

mod_amex_server <- function(id, 
                            df_amex) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    #Prepare Amex data
    amex_org <- reactive({
      
      if(length(input$select_org)){ df_amex %>% filter(org %in% input$select_org)
      } else { df_amex }
      
    })
    
    #Update Reasons and Date Input depending on select_org 
    observeEvent(input$select_org, 
                 
                 {
                   reason_choices <- unique(amex_org()$reason_travel) %>% na.omit()
                   
                   shiny::updateSelectizeInput("reasons",
                                               choices = reason_choices, 
                                               session = session
                   )
                   
                   date_seq <- format(seq.Date(min(amex_org()$invoice_date), max(amex_org()$invoice_date), by = "month"), "%Y-%m")
                   
                   shinyWidgets::updateSliderTextInput(session = session, 
                                                       inputId = "date_range",
                                                       choices = date_seq, 
                   )
                   
                 }
    )
    
    #Filter amex_org with date and reason value 
    amex_ready <- reactive({
      
      date <- paste0(input$date_range, "-01")
      
      df <- amex_org() %>%
        filter(invoice_date >= date[1], invoice_date <= date[2])
      
      if (length(input$reasons)) df <-  df %>% filter(reason_travel %in% input$reasons)
      
      return(df)
      
    })
    
    # Summary amex_ready() for value boxes
    amex_summary <- reactive({
      
      main_segment <- amex_ready() %>% count(ori, dest) %>% mutate(segment = paste(ori, dest, sep = "-")) %>% arrange(desc(n))
      
      dat_summ <- amex_ready() %>%
        
        summarise(
          n_flight = frmt_num( n()),
          n_segment = nrow(main_segment), 
          main_seg =  main_segment %>% filter(row_number() == 1) %>% pull(segment),
          main_seg_n = main_segment %>% filter(row_number() == 1) %>% pull(n),
          tot_distance_miles = frmt_num(sum(distance_km)),
          tot_distance_km = frmt_num(sum(distance_miles)),
          tot_emissions = frmt_num(sum(emission)), 
          emission_km = frmt_num(round(digits = 2, sum(emission)/sum(distance_km)))
        )
      
      return(dat_summ)
    })
    
    
    # VALUE BOXES ============================
    
    output$flight <- renderText({
      req(amex_summary())
      paste(amex_summary()$n_flight, " Flights")
    })
    
    output$segment <- renderText({
      req(amex_summary())
      paste(amex_summary()$n_segment, " unique")
    })
    
    output$main_segment <- renderText({
      req(amex_summary())
      paste(amex_summary()$main_seg)
    })
    
    output$main_segment_info <- renderUI({
      req(amex_summary())
      tags$small(glue::glue("travelled {amex_summary()$main_seg_n} times"))
    })
    
    output$dist <- renderText({
      req(amex_summary())
      paste(amex_summary()$tot_distance_km, " km")
    })
    
    output$dist_info <- renderUI({
      req(amex_summary())
      tags$small(glue::glue(
        "{amex_summary()$tot_distance_miles} miles"
      ))
    })
    
    output$emission <- renderText({
      
      req(amex_summary())
      paste(amex_summary()$tot_emissions, " tCO2e")
    })
    
    output$emission_info <-  renderText({
      req(amex_summary())
      
      paste(amex_summary()$emission_km, "tCO2e per Km")
    })
    
    # Time-Series ===========================================
    
    output$time_serie <- renderHighchart({
      
      req(amex_summary())
      
      # Set filters 
      if(input$group != "no grouping"){
        group_sym <- sym(input$group)} else { group_sym <- NULL}
      
      
      # Prepare data 
      hc_df <- reactive( { 
        y_var <- sym(input$display)
        
        if(input$date_interval == "month"){
          fmt_date <- "%Y-%m"
          
        } else {
          
          fmt_date <- "%Y"
          
        }
        
        if(input$date_interval == "quarter"){
          
          df <- amex_ready() %>% 
            mutate(
              date_group = lubridate::quarter(invoice_date, with_year = TRUE), 
              date_group = str_replace(date_group, "\\.", "-Q")
            ) 
        } else {
          
          df <- amex_ready() %>% 
            mutate(
              date_group = floor_date(invoice_date, input$date_interval), 
              date_group = format.Date(date_group, format = fmt_date), 
              date_group = fct_relevel(date_group)
            ) 
          
        }
        
        df <- df %>% 
          summarise(
            .by = c(!!group_sym, date_group), 
            n_flights = n(), 
            dist_km = round( sum(distance_km), digits = 1), 
            dist_miles = round(sum(distance_miles), digits = 1), 
            gross_amount = round(sum(gross_amount), digits = 1),
            emission = round(sum(emission), digits = 1)
          ) %>% 
          
          arrange(date_group) %>% 
          
          mutate(lab = fmt_n(!!y_var), 
                 
                 cumm_sum = cumsum(!!y_var))
        
        return(df)
        
      })
      
      if(input$group == "no grouping") {
        y_var <- sym(input$display)
        
        base_hc <- highchart() %>% 
          
          # hc_yAxis_multiples(
          #   list(lineWidth = 3),
          #   list(showLastLabel = FALSE, 
          #        opposite = TRUE)
          # ) %>%
          
          # hc_add_series(hc_df(), 
          #               "spline", 
          #               hcaes(x = date_group, 
          #                     y = cumm_sum), 
        #               yAxis = 1) %>% 
        
        hc_add_series(hc_df(),
                      "column", 
                      hcaes(x = date_group, 
                            y = !!y_var)
                      
        )
      } else { 
        
        y_var <- sym(input$display)
        
        # Not in right x order if group is org 
        base_hc <- highchart() %>% 
          
          # hc_yAxis_multiples(
          #   title = list(text = matchmaker::match_vec(input$display, display_lab, from = 1, to = 2)),
          #   list(lineWidth = 3),
          #   list(showLastLabel = FALSE, 
          #        opposite = TRUE)
          # ) %>%
          # 
          # hc_add_series(hc_df(),
          #               "spline",
          #               hcaes(x = date_group,
        #                     y = cumm_sum,
        #                     group = !!group_sym),
        #               yAxis = 1) %>%
        
        hc_add_series(hc_df(),
                      "column", 
                      hcaes(x = date_group, 
                            y = !!y_var, 
                            group = !!group_sym)
        )
        
      }
      
      base_hc %>%
        
        hc_xAxis(title = list(text = str_to_sentence(input$date_interval)), 
                 categories = levels(hc_df()$date_group)) %>% 
        
        hc_yAxis(title = list(text = matchmaker::match_vec(input$display, display_lab, from = 1, to = 2)
                              
        )) %>%
        
        hc_tooltip(useHTML = TRUE,
                   formatter = JS("
      function(){
      outHTML =  '<i>' + this.point.date_group +'</i><b><br>' + this.point.lab + '</b>'
       return(outHTML)

       }")
        )
      
    })
    
    
    # Distributions =======================================
    
    #observe Event for distirbution year input 
    
    observeEvent(input$date_range, {
      
      year_choices <- sort(unique(amex_ready()$year), decreasing = TRUE)
      
      shiny::updateSelectizeInput("dist_year",
                                  choices = c(purrr::set_names("All years", NULL), year_choices),
                                  session = session
      )
      
    }
    
    )
    
    # Histograms
    output$dist_hist <- renderHighchart({
      
      req(amex_summary())
      
      dict_lab <- data.frame(from = unname(dist_var), 
                             to = names(dist_var))
      
      
      dist_var_sym <- sym(input$dist_var)
      
      if(input$dist_year != "All years"){
        
        hc_var <- amex_ready() %>% 
          
          filter(year == input$dist_year) %>% 
          
          pull(!!dist_var_sym)
        
      } else {
        
        hc_var <- amex_ready() %>% 
          
          pull(!!dist_var_sym)
      }
      
      hchart(hc_var, 
             
             name = matchmaker::match_vec(input$dist_var, dict_lab, from = 1, to = 2) ) %>% 
        
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
      
      req(amex_summary())
      
      
      dist_var_sym <- sym(input$dist_var)
      date_interval_sym <- sym(input$dist_interval) 
      
      if(input$dist_year != "All years"){
        
        amex_box <- amex_ready() %>% 
          filter(year == input$dist_year)
        
      } else {
        
        amex_box <- amex_ready()
        
      }
      
      box_df <- data_to_boxplot(amex_box, 
                                !!dist_var_sym, 
                                group_var = !!date_interval_sym,
                                name = matchmaker::match_vec(input$dist_var, dict_lab, from = 1, to = 2), 
                                #showInLegend = FALSE
      )
      
      highchart() %>%
        hc_chart(zoomType = "x") %>%
        hc_xAxis(
          type = "category",
          crosshair = TRUE
        ) %>%
        hc_yAxis(
          title = list(text = matchmaker::match_vec(input$dist_var, dict_lab, from = 1, to = 2))
        ) %>%
        
        hc_add_series_list(box_df) %>%
        
        hc_tooltip(shared = TRUE) 
    })
    
    # Global parts ========================================
    output$barplot <- renderHighchart({
      
      bar_var_sym <- sym(input$bar_var)
      
      bar_group_sym <- sym(input$bar_group)
      
      hc_df <- amex_ready() %>% 
        
        #filter out Nas for group var
        drop_na(!!bar_group_sym) %>% 
        
        summarise(.by = c(!!bar_group_sym), 
                  n_flights = n(), 
                  dist_km = sum(distance_km), 
                  dist_miles = sum(distance_miles), 
                  gross_amount = sum(gross_amount),
                  emission = round(digits = 1, sum(emission))
        ) %>% 
        mutate(label = fmt_n(!!bar_var_sym), 
               percent = scales::percent(!!bar_var_sym/sum(!!bar_var_sym), accuracy = .1 )) %>% 
        
        rename(group_var = input$bar_group) %>% 
        
        arrange(desc(group_var))
      
      
      hchart(hc_df, 
             "column", 
             hcaes(x = group_var, 
                   y = !!bar_var_sym)) %>% 
        
        hc_tooltip(useHTML = TRUE,
                   formatter = JS("
    function(){
    outHTML =  '<i>' + this.point.group_var + '</i> <br> <b>' + this.point.label + ' (' + this.point.percent + ')</b>' 
     return(outHTML)
     
     }") ) %>% 
        
        hc_chart(inverted=TRUE)
      
    })
    
    # GEO TABLE ==========================================
    
    output$table <- renderReactable({
      
      geo_tab <- amex_ready() %>% 
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
      
    }
    )
    
    # Map ==================================================
    
  })}
