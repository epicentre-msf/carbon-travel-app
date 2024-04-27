mod_origin_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    div(
      id = ns("inputs"),
      city_input(
        ns,
        index = 1,
        city_lab = tooltip(
          span("City of origin", bsicons::bs_icon("info-circle")),
          "Where are people travelling from?"
        ),
        num_lab = tooltip(
          span("People", bsicons::bs_icon("info-circle")),
          "How many people are travelling from this location?"
        )
      ),
      city_input(ns, index = 2),
    ),
    div(
      class = "d-flex mb-3 justify-content-end",
      div(
        class = "pe-2",
        actionButton(
          ns("remove"),
          "-",
          class = "btn-danger btn-sm"
        ) %>% tooltip("Remove a location", placement = "top")
      ),
      actionButton(
        ns("add"),
        "+",
        class = "btn-info btn-sm"
      ) %>% tooltip("Add another location", placement = "top")
    ),
    
    div( 
      div(shinyWidgets::radioGroupButtons(
        
        inputId = ns("msf_all"), 
        label = "All destinations or only MSF ?", 
        choices = c("All" = "all", 
                    "MSF" = "msf"), 
        size = "normal", 
        selected = "all",
        justified = TRUE
      )
      ),
      
      div(
        shinyWidgets::pickerInput(
          inputId = ns("msf_type_select"), 
          label = "MSF type", 
          multiple = TRUE,
          choices = msf_type_vec,
          selected = msf_type_vec
        )
      ),
      
      div(
        id = ns("dest_select"),
        
        dest_input(
          ns,
          dest_lab = tooltip(
            span("Destinations", 
                 bsicons::bs_icon("info-circle")),
            "Select all possible destinations for a meeting"
          )
        )
      )
    ),
    
    div(
      actionButton(ns("go"), "Get meeting places", width = "100%", class = "btn-primary"), 
      
    )
  )
}

mod_origin_input_server <- function(id, cities) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # set this to number of inputs you starts with
    n_inputs <- reactiveVal(2)
    
    observe({
      n <- n_inputs()
      shinyjs::toggleState("remove", condition = n > 2)
    })
    
    observe({
      shinyWidgets::updateVirtualSelect("p1", 
                                        choices = cities, 
                                        selected = "PAR")
      shinyWidgets::updateVirtualSelect("p2", 
                                        choices = cities, 
                                        selected = "LON")
    })
    
    observeEvent(input$add, {
      index <- n_inputs() + 1
      insertUI(
        selector = paste0("#", ns("inputs")),
        where = "beforeEnd",
        ui = city_input(ns, 
                        index, 
                        choices = cities
                        
        )
        # ui = city_input(ns, input$add + 2, cities)
      )
      n_inputs(index)
    })
    
    observeEvent(input$remove, {
      index <- n_inputs()
      removeUI(selector = paste0("#", "origin", index))
      n_inputs(index - 1)
    })
    
    #Destinations filters 
    
    cities <- dest |>
      
      shinyWidgets::prepare_choices(
        label = city_name,
        value = city_code,
        group_by = country_name
      )
    
    # observe(input$msf_all, { 
    #   
    #   if(input$msf_all == "msf"){
    #     
    #     cities <- dest |> 
    #       filter(msf) |> 
    #       filter(str_detect(msf_type, msf_type())) |> 
    #       shinyWidgets::prepare_choices(
    #         label = city_name,
    #         value = city_code,
    #         group_by = country_name
    #       )
    #     
    #     shinyWidgets::updateVirtualSelect("dest_select", 
    #                                       choices = cities)
    #     
    #     
    #   } else {
    #     
    #     cities <- dest |> 
    #       filter(str_detect(msf_type, msf_type())) |> 
    #       shinyWidgets::prepare_choices(
    #         label = city_name,
    #         value = city_code,
    #         group_by = country_name
    #       )
    #     
    #     shinyWidgets::updateVirtualSelect("dest_select", 
    #                                       choices = cities)
    #     
    #   }
    #   
    # })
    
    
    
    df_origin <- reactive({
      #index <- input$add + 2
      index <- n_inputs()
      req(input[[paste0("n", index)]])
      selected_cities <- purrr::map_chr(1:index, ~ input[[paste0("p", .x)]])
      n_people <- purrr::map_int(1:index, ~ as.integer(input[[paste0("n", .x)]]))
      tibble::tibble(
        origin_id = selected_cities,
        n_participant = n_people
      ) %>% dplyr::filter(origin_id != "", !is.na(n_participant))
    }) %>% bindEvent(input$go, ignoreInit = TRUE)
    
    # return city df
    reactive(df_origin())
  })
}

city_input <- function(ns,
                       index,
                       choices = NULL,
                       selected = NULL,
                       city_lab = NULL,
                       num_lab = NULL,
                       n_val = 1) {
  div(
    id = paste0("origin", index),
    class = "d-flex p-0 justify-content-center",
    div(
      class = "p-0 flex-grow-1",
      shinyWidgets::virtualSelectInput(
        inputId = ns(paste0("p", index)),
        label = city_lab,
        choices = choices,
        selected = selected,
        search = TRUE,
        autoSelectFirstOption = TRUE,
        placeholder = "Select city...",
        position = "bottom",
        dropboxWrapper = "body",
        showOptionsOnlyOnSearch = FALSE,
        optionsCount = 5
      )
    ),
    div(
      class = "p-0",
      numericInput(
        inputId = ns(paste0("n", index)),
        label = num_lab,
        value = n_val,
        min = 1,
        step = 1,
        width = "60px"
      )
    )
  )
}

dest_input <- function(ns,
                       choices = NULL,
                       selected = NULL,
                       dest_lab = NULL) {
  
  div(
    class = "d-flex p-0 justify-content-center",
    div(
      class = "p-0 flex-grow-1",
      shinyWidgets::virtualSelectInput(
        inputId = ns("select_dest"),
        label = dest_lab,
        choices = choices,
        selected = selected,
        search = TRUE,
        autoSelectFirstOption = TRUE,
        placeholder = "Select city...",
        position = "bottom",
        dropboxWrapper = "body",
        showOptionsOnlyOnSearch = FALSE,
        optionsCount = 5
      )
    )
  ) 
}




