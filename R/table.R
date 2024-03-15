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
               "places_subdomain" = england_ltla_hi_social_determinants_sub,
               "lives_subdomain" = england_ltla_hi_risk_factors_sub,
               stop("No data selected", call. = FALSE)
        )
      }else if (selected$geography == "england_icb_shp" ||
                      selected$geography == "scotland_ltla_shp" ||
                      selected$geography == "scotland_hb_shp" ||
                      selected$geography == "northern_ireland_ltla_shp" ||
                      selected$geography == "northern_ireland_hsct_shp" ||
                      selected$geography == "wales_ltla_shp" ||
                      selected$geography == "wales_lhb_shp") {
        stop("Data not available yet", call. = FALSE)
      }
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
      #areas = vector(), geography = "england_ltla_shp"
      
    )
    tableServer("test", selected, type = "people_subdomain")
  }
  
  shinyApp(ui, server)
}

# Examples
tableTest()
