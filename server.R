# server file for Carbon-Travel-App


server <- function(input, output, session) {
  
  df_origin <- mod_origin_input_server("origin", cities)
  # df_origin <- reactive({
  #   df_input()
  # }) %>% bindEvent(input$go, ignoreInit = TRUE)
  
  mod_meeting_place_server(
    id = "mp",
    mat, 
    air_msf,
    df_conversion,
    df_origin,
    msf_only = TRUE
  )
  
  mod_amex_server(id = "flights", 
                  df_amex)
  
}