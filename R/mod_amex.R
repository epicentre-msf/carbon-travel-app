mod_amex_ui <- function(id) {
  
  ns <- NS(id)
  
  nav_panel(
    "Flight data analysis",
    
    layout_sidebar(
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
      
      bslib::card(
        bslib::card_header( 
          class = "d-flex align-items-center",
          # title 
          " Map"), 
        leaflet::leafletOutput(ns("map"))),
      
      bslib::card(
        bslib::card_header( 
          class = "d-flex align-items-center",
          # title 
          " Time-serie"), 
        highchartOutput(ns("time-serie")))
      
    ),
    
    # VALUE BOXES ========================
    layout_column_wrap(
      width = 1/5,
      fill = FALSE,
      value_box(
        title = "Flights",
        theme = "primary",
        class = "vb",
        value = textOutput(ns("flight")),
      ),
      value_box(
        title = "Routes",
        theme = "primary",
        class = "vb",
        value = textOutput(ns("segment")),
      ),
      value_box(
        title = "Main route",
        theme = "primary",
        class = "vb",
        value = textOutput(ns("main_segment")),
        uiOutput(ns("main_segment_info"))
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
        #uiOutput(ns("emission_info"))
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
          tot_emissions = frmt_num(sum(emission))
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
      paste(amex_summary()$n_segment, " Unique routes")
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
      paste(amex_summary()$tot_distance_km, " kilometers")
    })
    
    output$dist_info <- renderUI({
      req(amex_summary())
      tags$small(glue::glue(
        "{amex_summary()$tot_distance_miles} miles"
      ))
    })
    
    output$emission <- renderText({
      req(amex_summary())
      paste(amex_summary()$tot_emissions, "CO2 emissions")
    })
    
    # Time-Series ===========================================
    
    
    
    # Map ==================================================
    
    
  })}
