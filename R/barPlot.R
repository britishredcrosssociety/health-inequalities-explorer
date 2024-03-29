barPlotUI <- function(id) {
  plotlyOutput(
    NS(id, "plot"),
    height = "100%"
  )
}

barPlotServer <- function(id, selected, type) {
  moduleServer(id, function(input, output, session) {
    valid_geographies <- c("england_ltla_shp", "brc_central_shp", "brc_london_shp", "brc_north_shp", "brc_south_shp", "brc_southeast_shp")
    
    # Select dataset based on geographical selection and type of data
    dataset <- reactive({
      if (selected$geography %in% valid_geographies) {
        switch(type,
          "hi_outcomes" = england_ltla_hi_outcomes,
          "hi_risk_factors" = england_ltla_hi_risk_factors,
          "hi_social_determinants" = england_ltla_hi_social_determinants,
          stop("No data selected", call. = FALSE)
        )
      }
    })

    output$plot <- renderPlotly({
      if (selected$geography %in% valid_geographies) {
        if (is.null(selected$areas)) {
          bar_plot_mean_only(
            data = dataset()
            # selected_geography = selected$geography
          )
        } else {
          bar_plot_selected(
            data = dataset(),
            selected_areas = selected$areas
          )
        }
      } else {
        # Empty plot
      }
    })
  })
}

barPlotTest <- function() {
  ui <- fluidPage(
    barPlotUI("test")
  )

  server <- function(input, output, session) {
    selected <- reactiveValues(
      areas = c("Redcar and Cleveland", "Lewisham"), geography = "england_ltla_shp"
    )
    barPlotServer("test", selected, type = "hi_risk_factors")
  }

  shinyApp(ui, server)
}

# Examples
# barPlotTest()
