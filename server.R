# server file for Carbon-Travel-App

server <- function(input, output, session) {

  # send usage logs to /root/logs if running in shinyproxy container
  # add "/var/log/shinylogs:/root/logs" to the container volumes in the
  # shinyproxy app config to save logs to central location on machine
  if (is_sp_env) {
    # user logs
    track_usage(
      storage_mode = store_json(path = "/root/logs"),
      app_name = app_name,
      what = "session",
      exclude_users = c("paul.campbell@epicentre.msf.org", "paul")
    )
  }
  
  df_origin <- mod_origin_input_server("origin", cities)
  
  mod_meeting_place_server(
    id = "mp",
    mat, 
    air_msf,
    df_conversion,
    df_origin,
    msf_only = TRUE
  )
  
  mod_amex_server(
    id = "flights",
    df_amex
  )
  
}