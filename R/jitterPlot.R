jitterPlotUI <- function(id) {
  plotOutput(
    NS(id, "plot")
  )
}

jitterPlotServer <- function(id, selected, type) {
  moduleServer(id, function(input, output, session) {

    # Select dataset based on geographical selection and type of data
    dataset <- reactive({
      if (selected$geography == "ltla_shp_england" & type == "demographics_age") {
        ltla_demographics_age_england
      } else if (selected$geography == "ltla_shp_england" & type == "summary_metrics") {
        ltla_summary_metrics_england
      }
    })

    output$plot <- renderPlot({
      if (is.null(selected$areas) & type == "demographics_age") {
        jitter_plot_age_null(
          data = dataset(),
          x = population_relative,
          y = age
        )
      } else if (is.null(selected$areas) & type == "summary_metrics") {
        jitter_plot_summary_null(
          data = dataset(),
          x = value,
          y = variable
        )
      } else if (type == "demographics_age") {
        jitter_plot_prep(
          data = dataset(),
          selected_areas = selected$areas
        ) |>
          jitter_plot_age_selected(
            x = population_relative,
            y = age,
            fill = selected,
            selected_areas = selected$areas
          )
      } else if (type == "summary_metrics") {
        jitter_plot_prep(
          data = dataset(),
          selected_areas = selected$areas
        ) |>
          jitter_plot_summary_selected(
            x = value,
            y = variable,
            fill = selected,
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
