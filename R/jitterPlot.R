jitterPlotUI <- function(id) {
  plotlyOutput(
    NS(id, "plot"),
    height = "100%"
  )
}

jitterPlotServer <- function(id, selected, type) {
  moduleServer(id, function(input, output, session) {
    # Select dataset based on geographical selection and type of data
    dataset <- reactive({
      if (selected$geography == "ltla_shp_england" & type == "demographics") {
        ltla_demographics_england
      } else if (selected$geography == "ltla_shp_england" & type == "summary_metrics") {
        ltla_summary_metrics_england
      } else if (selected$geography == "ltla_shp_england" & type == "secondary_care") {
        ltla_secondary_care_england
      } else if (selected$geography == "icb_shp_england" & type == "demographics") {
        icb_demographics_england
      } else if (selected$geography == "icb_shp_england" & type == "summary_metrics") {
        icb_summary_metrics_england
      } else if (selected$geography == "icb_shp_england" & type == "secondary_care") {
        icb_secondary_care_england
      }
    })

    output$plot <- renderPlotly({
      if (is.null(selected$areas)) {
        jitter_plot_null(data = dataset())
      } else {
        jitter_plot_prep(data = dataset(), selected_areas = selected$areas) |>
          jitter_plot_selected(
            selected_areas = selected$areas
          )
      }
    })
  })
}

jitterPlotTest <- function() {
  ui <- fluidPage(
    jitterPlotUI("test")
  )

  server <- function(input, output, session) {
    selected <- reactiveValues(
      areas = vector(), geography = "ltla_shp_england"
    )
    jitterPlotServer("test", selected, type = "demographics_age")
  }

  shinyApp(ui, server)
}

# Examples
# jitterPlotTest()
