tableUI <- function(id) {
  formattableOutput(
    NS(id, "table"),
    height = "100%"
  )
}

tableServer <- function(id, selected, type) {
  moduleServer(id, function(input, output, session) {
    valid_geographies <- c("scotland_ltla_shp", "england_ltla_shp", "brc_central_shp", "brc_london_shp", "brc_north_shp", "brc_south_shp", "brc_southeast_shp")
    
    # Select dataset based on geographical selection and type of data
    dataset <- reactive({
      if (selected$geography %in% c("england_ltla_shp", "brc_central_shp", "brc_london_shp", "brc_north_shp", "brc_south_shp", "brc_southeast_shp")) {
        switch(type,
          "people_subdomain" = england_ltla_hi_outcomes_sub,
          "places_subdomain" = england_ltla_hi_social_determinants_sub,
          "lives_subdomain" = england_ltla_hi_risk_factors_sub,
          stop("No data selected", call. = FALSE)
        )
      } else if (selected$geography == "scotland_ltla_shp") {
        switch(type,
          "people_subdomain" = scotland_ltla_hi_outcomes_sub,
          "places_subdomain" = scotland_ltla_hi_social_determinants_sub,
          "lives_subdomain" = scotland_ltla_hi_risk_factors_sub,
          stop("No data selected", call. = FALSE)
        )
      }
      # } else if (selected$geography == "england_icb_shp" ||
      #   selected$geography == "scotland_ltla_shp" ||
      #   selected$geography == "scotland_hb_shp" ||
      #   selected$geography == "northern_ireland_ltla_shp" ||
      #   selected$geography == "northern_ireland_hsct_shp" ||
      #   selected$geography == "wales_ltla_shp" ||
      #   selected$geography == "wales_lhb_shp") {
      #   stop("Domain data only currently available for England LTLA.", call. = FALSE)
      # }
    })

    output$table <- renderFormattable({
      # if (is.null(selected$areas)) {
      #   table_null(data = dataset())
      # } else {
      if (selected$geography %in% valid_geographies) {
        table_prep(data = dataset(), selected_areas = selected$areas) |>
          table_selected(
            selected_areas = selected$areas
          )
      } else {
        table_null()
      }
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
      # areas = vector(), geography = "england_ltla_shp"
    )
    tableServer("test", selected, type = "people_subdomain")
  }

  shinyApp(ui, server)
}

# Examples
tableTest()
