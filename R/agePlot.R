agePlotUI <- function(id) {
  plotOutput(
    NS(id, "plot")
  )
}

agePlotServer <- function(id, selected) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactive({
      ltla_demographics_age_england
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
        ggplot(aes(x = population_relative, y = age, fill = selected)) +
        geom_point(
          aes(alpha = alpha),
          position = position_jitter(height = 0.25, width = 0.1, seed = 123),
          size = 5,
          shape = 21,
          colour = "#262626"
        ) +
        scale_x_continuous(labels = scales::percent) +
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

agePlotTest <- function() {
  ui <- fluidPage(
    agePlotUI("test")
  )

  server <- function(input, output, session) {
    selected <- reactiveValues(
      areas = vector(), geography = "ltla_shp_england"
    )
    agePlotServer("test", selected)
  }

  shinyApp(ui, server)
}

# Examples
# agePlotTest()
