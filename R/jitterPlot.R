jitterPlotUI <- function(id) {
  plotOutput(
    NS(id, "plot")
  )
}

jitterPlotServer <- function(id, selected) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactive({
      ltla_summary_metrics_england
    })

    output$plot <- renderPlot({

      # Set sensible plot/legend default if no area has been selected
      if (is.null(selected$areas)) {
        dataset <- dataset() |>
          dplyr::mutate(selected = "not selected") |>
          dplyr::mutate(alpha = 0.2)
        legend_break_name <- NULL
      } else {
        dataset <- dataset() |>
          dplyr::mutate(
            selected = dplyr::if_else(
              area_name %in% selected$areas,
              area_name,
              "not selected"
            )
          ) |>
          dplyr::mutate(
            alpha = dplyr::if_else(selected != "not selected", 1, 0.1)
          ) |>
          dplyr::mutate(selected = factor(selected)) |>
          dplyr::mutate(selected = relevel(selected, ref = "not selected"))
        legend_break_name <- selected$areas
      }

      # Create plot object
      dataset |>
        ggplot(aes(x = value, y = variable, fill = selected)) +
        geom_vline(
          xintercept = 0,
          size = 1,
          alpha = .7,
          colour = "#262626",
          linetype = "dashed"
        ) +
        geom_point(
          aes(alpha = alpha),
          position = position_jitter(height = 0.25, width = 0.1, seed = 123),
          size = 5,
          shape = 21,
          colour = "#262626"
        ) +
        annotate("text", x = 0, y = 3.75, label = "bold(Mean)", parse = TRUE) +
        theme_minimal() +
        theme(
          legend.position = "top",
          legend.title = element_blank()
        ) +
        scale_fill_manual(
          values = c("#D0021B", "#40A22A", "#F1B13B", "#6A9EAA"),
          breaks = legend_break_name
        ) +
        scale_alpha(guide = "none") +
        labs(x = NULL, y = NULL) +
        theme(text = element_text(face = "bold", size = 15))
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
    jitterPlotServer("test", selected)
  }

  shinyApp(ui, server)
}

# Examples
# jitterPlotTest()
