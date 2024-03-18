barPlotUI <- function(id) {
  plotlyOutput(
    NS(id, "plot"),
    height = "100%"
  )
}

barPlotServer <- function(id, selected, type) {
  moduleServer(id, function(input, output, session) {
    # Select dataset based on geographical selection and type of data
    dataset <- reactive({
      if (selected$geography == "england_ltla_shp") {
        switch(type,
          "hi_outcomes" = england_ltla_hi_outcomes,
          "hi_risk_factors" = england_ltla_hi_risk_factors,
          "hi_social_determinants" = england_ltla_hi_social_determinants,
          stop("No data selected", call. = FALSE)
        )
      } else if (selected$geography == "england_icb_shp" ||
        selected$geography == "scotland_ltla_shp" ||
        selected$geography == "scotland_hb_shp" ||
        selected$geography == "northern_ireland_ltla_shp" ||
        selected$geography == "northern_ireland_hsct_shp" ||
        selected$geography == "wales_ltla_shp" ||
        selected$geography == "wales_lhb_shp") {
        stop("No data selected", call. = FALSE)
      }
    })


    output$plot <- renderPlotly({
      if (is.null(selected$areas)) {
        bar_plot_null(data = dataset())
      } else {
        bar_plot_selected(
          data = dataset(),
          selected_areas = selected$areas
        )
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
