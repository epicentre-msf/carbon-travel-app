
vb_ui <- function(id) {
  ns <- NS(id)
}

vb_server <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    amex_summary <- reactive({
      main_segment <- dat %>%
        count(ori, dest) %>%
        mutate(segment = paste(ori, dest, sep = "-")) %>%
        arrange(desc(n))

      dat_summ <- dat %>%
        summarise(
          n_flight = frmt_num(n()),
          n_segment = nrow(main_segment()),
          main_seg = main_segment() %>% filter(row_number() == 1) %>% pull(segment),
          main_seg_n = main_segment() %>% filter(row_number() == 1) %>% pull(n),
          tot_distance_miles = frmt_num(sum(distance_km)),
          tot_distance_km = frmt_num(sum(distance_miles)),
          tot_emissions = frmt_num(sum(emission))
        )

      return(dat_summ)
    })

    # df_summary <- reactive({
    #   df <- force_reactive(df_ll)
    #   pf <- place_filter()
    #   if (length(pf)) {
    #     df <- df %>% dplyr::filter(.data[[pf$geo_col]] == pf$region_select)
    #   }
    #   tf <- time_filter()
    #   if (length(tf)) {
    #     df <- df %>% dplyr::filter(dplyr::between(.data[[tf$date_var]], tf$from, tf$to))
    #   }
    #   req(nrow(df) > 0)
    #   df %>%
    #     dplyr::summarise(
    #       date_min = min(date_notification, na.rm = TRUE),
    #       date_max = max(date_notification, na.rm = TRUE),
    #       period = (date_max - date_min) + 1,
    #       n_sites = n_distinct(site[site != getOption("epishiny.na.label")], na.rm = TRUE),
    #       n_ocs = n_distinct(ll_org[ll_org != getOption("epishiny.na.label")], na.rm = TRUE),
    #       n_countries = n_distinct(ref_adm0_name[ref_adm0_name != getOption("epishiny.na.label")], na.rm = TRUE),
    #       n_adm1 = n_distinct(ref_adm1_pcode[ref_adm1_pcode != getOption("epishiny.na.label")], na.rm = TRUE),
    #       n_consultations = n(),
    #       pcnt_male = sum(sex_id == "Male", na.rm = TRUE) / sum(sex_id %in% c("Male", "Female"), na.rm = TRUE),
    #       pcnt_under_5 = sum(age_years < 5, na.rm = TRUE) / sum(!is.na(age_years)),
    #       pcnt_under_16 = sum(age_years < 16, na.rm = TRUE) / sum(!is.na(age_years)),
    #       n_hospitalised = sum(hospitalised_yn == "Yes", na.rm = TRUE),
    #       cfr_num = sum(outcome %in% cfr_num(), na.rm = TRUE),
    #       cfr_denom = sum(outcome %in% c(cfr_num(), cfr_denom()), na.rm = TRUE),
    #       cfr = cfr_num / cfr_denom
    #     )
    # })

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
    #
    # output$emission_info <- renderUI({
    #   req(df_summary())
    #   tags$small(glue::glue(
    #     "{ } "
    #   ))
    # })
  })
}
