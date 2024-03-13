tableUI <- function(id) {
  formattableOutput(
    NS(id, "table"),
    height = "100%"
  )
}

tableServer <- function(id, selected, type) {
  moduleServer(id, function(input, output, session) {
    # Select dataset based on geographical selection and type of data
    dataset <- reactive({
      if (selected$geography == "england_ltla_shp") {
        switch(type,
               "people_subdomain" = england_ltla_hi_outcomes_sub,
               "places_subdomain" = england_ltla_hi_risk_factors_sub,
               "lives_subdomain" = england_ltla_hi_social_determinants_sub,
               stop("No data selected", call. = FALSE)
        )
      } # else if (selected$geography == "england_icb_shp") {
      #   switch(type,
      #          "summary_metrics" = england_icb_summary_metrics,
      #          "secondary_care" = england_icb_secondary_care,
      #          "demographics" = england_icb_demographics,
      #          "health_index" = england_icb_health_index,
      #          stop("No data selected", call. = FALSE)
      #   )
      # } else if (selected$geography == "scotland_ltla_shp") {
      #   switch(type,
      #          "summary_metrics" = scotland_ltla_summary_metrics,
      #          "secondary_care" = scotland_ltla_secondary_care,
      #          "demographics" = scotland_ltla_demographics,
      #          stop("No data selected", call. = FALSE)
      #   )
      # } else if (selected$geography == "scotland_hb_shp") {
      #   switch(type,
      #          "summary_metrics" = scotland_hb_summary_metrics,
      #          "secondary_care" = scotland_hb_secondary_care,
      #          "demographics" = scotland_hb_demographics,
      #          stop("No data selected", call. = FALSE)
      #   )
      # } else if (selected$geography == "northern_ireland_ltla_shp") {
      #   switch(type,
      #          "summary_metrics" = northern_ireland_ltla_summary_metrics,
      #          "secondary_care" = northern_ireland_ltla_secondary_care,
      #          "demographics" = northern_ireland_ltla_demographics,
      #          stop("No data selected", call. = FALSE)
      #   )
      # } else if (selected$geography == "northern_ireland_hsct_shp") {
      #   switch(type,
      #          "summary_metrics" = northern_ireland_hsct_summary_metrics,
      #          "secondary_care" = northern_ireland_hsct_secondary_care,
      #          "demographics" = northern_ireland_hsct_demographics,
      #          stop("No data selected", call. = FALSE)
      #   )
      # } else if (selected$geography == "wales_ltla_shp") {
      #   switch(type,
      #          "summary_metrics" = wales_ltla_summary_metrics,
      #          "demographics" = wales_ltla_demographics,
      #          stop("No data selected", call. = FALSE)
      #   )
      # } else if (selected$geography == "wales_lhb_shp") {
      #   switch(type,
      #          "summary_metrics" = wales_lhb_summary_metrics,
      #          "secondary_care" = wales_lhb_secondary_care,
      #          "demographics" = wales_lhb_demographics,
      #          stop("No data selected", call. = FALSE)
      #   )
      # }
    })
    
    output$table <- renderFormattable({
      # if (is.null(selected$areas)) {
      #   table_null(data = dataset())
      # } else {
      table_prep(data = dataset(), selected_areas = selected$areas) |>
        table_selected(
          selected_areas = selected$areas
        )
      # }
    })
  })
}

tableTest <- function() {
  ui <- fluidPage(
    tableUI("test")
  )
  
  server <- function(input, output, session) {
    selected <- reactiveValues(
      areas = c("Tower Hamlets", "Ashford"), geography = "england_ltla_shp"
    )
    tableServer("test", selected, type = "people_subdomain")
  }
  
  shinyApp(ui, server)
}

# Examples
tableTest()
