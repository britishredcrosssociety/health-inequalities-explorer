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
      if (selected$geography == "england_ltla_shp") {
        switch(type,
               "summary_metrics" = england_ltla_summary_metrics,
               "secondary_care" = england_ltla_secondary_care,
               "demographics" = england_ltla_demographics
        )
      } else if (selected$geography == "england_icb_shp") {
        switch(type,
               "summary_metrics" = england_icb_summary_metrics,
               "secondary_care" = england_icb_secondary_care,
               "demographics" = england_icb_demographics
        )
      } else if (selected$geography == "scotland_ltla_shp") {
        switch(type,
               "summary_metrics" = scotland_ltla_summary_metrics,
               "secondary_care" = scotland_ltla_secondary_care,
               "demographics" = scotland_ltla_demographics
        )
      } else if (selected$geography == "scotland_hb_shp") {
        switch(type,
               "summary_metrics" = scotland_hb_summary_metrics,
               "secondary_care" = scotland_hb_secondary_care,
               "demographics" = scotland_hb_demographics
        )
      }
    })
    
    
    # dataset <- reactive({
    #   if (selected$geography == "england_ltla_shp" & type == "summary_metrics") {
    #     england_ltla_summary_metrics
    #   } else if (selected$geography == "england_ltla_shp" & type == "secondary_care") {
    #     england_ltla_secondary_care
    #   } else if (selected$geography == "england_ltla_shp" & type == "demographics") {
    #     england_ltla_demographics
    #   } else if (selected$geography == "england_icb_shp" & type == "summary_metrics") {
    #     england_icb_summary_metrics
    #   } else if (selected$geography == "england_icb_shp" & type == "secondary_care") {
    #     england_icb_secondary_care
    #   } else if (selected$geography == "england_icb_shp" & type == "demographics") {
    #     england_icb_demographics
    #   } else if (selected$geography == "scotland_ltla_shp" & type == "summary_metrics") {
    #     scotland_ltla_summary_metrics
    #   } else if (selected$geography == "scotland_ltla_shp" & type == "secondary_care") {
    #     scotland_ltla_secondary_care
    #   } else if (selected$geography == "scotland_ltla_shp" & type == "demographics") {
    #     scotland_ltla_demographics
    #   } else if (selected$geography == "scotland_hb_shp" & type == "summary_metrics") {
    #     scotland_hb_summary_metrics
    #   } else if (selected$geography == "scotland_hb_shp" & type == "secondary_care") {
    #     scotland_hb_secondary_care
    #   } else if (selected$geography == "scotland_hb_shp" & type == "demographics") {
    #     scotland_hb_demographics
    #   } 
    # })

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
      areas = vector(), geography = "england_ltla_shp"
    )
    jitterPlotServer("test", selected, type = "demographics")
  }

  shinyApp(ui, server)
}

# Examples
# jitterPlotTest()
