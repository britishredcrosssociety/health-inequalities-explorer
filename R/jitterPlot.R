jitterPlotUI <- function(id) {
  girafeOutput(
    NS(id, "plot")
  )
}

jitterPlotServer <- function(id, data, selected_area) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderGirafe({

      # Set sensible plot/legend default if no area has been selected
      if (is.null(selected_area())) {
        data <- data |>
          dplyr::mutate(selected = "not selected")
        legend_break_name <- NULL
      } else {
        data <- data |>
          dplyr::mutate(
            selected = dplyr::if_else(
              area_name == selected_area(),
              area_name,
              "not selected"
            )
          ) |>
          dplyr::mutate(
            selected = factor(
              selected,
              levels = c("not selected", selected_area())
            )
          )
        legend_break_name <- selected_area()
      }

      # Create plot object
      gg <- data |>
        ggplot(aes(x = value, y = variable, colour = selected)) +
        geom_vline(
          xintercept = 150, size = 2, alpha = .5, colour = "#FFA500"
        ) +
        geom_jitter_interactive(
          aes(tooltip = area_name, data_id = area_name),
          height = 0.25,
          size = 3, alpha = .7
        ) +
        theme_minimal() +
        theme(
          legend.position = "top",
          legend.title = element_blank()
        ) +
        scale_color_viridis_d(
          option = "C", alpha = .5, begin = .2, end = .8,
          breaks = legend_break_name
        ) +
        labs(x = NULL, y = NULL)

      # Render plot
      girafe(
        ggobj = gg,
        options = list(
          # opts_hover_inv(css = "opacity:0.3;"),
          opts_toolbar(saveaspng = FALSE),
          opts_selection(type = "none"),
          opts_sizing(rescale = TRUE, width = .5)
        )
      )
    })
  })
}

jitterPlotTest <- function(data) {
  ui <- fluidPage(
    jitterPlotUI("test")
  )

  server <- function(input, output, session) {
    selected_area <- reactiveVal()
    jitterPlotServer("test", data, selected_area)
  }

  shinyApp(ui, server)
}

# Examples
jitterPlotTest(data = hi_vul_england)