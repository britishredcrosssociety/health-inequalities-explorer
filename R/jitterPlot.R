jitterPlotUI <- function(id) {
  girafeOutput(
    NS(id, "plot")
  )
}

jitterPlotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    gg <- data |>
      ggplot(aes(x = value, y = variable)) +
      geom_vline(xintercept = 150, size = 2, alpha = .5, colour = "#FFA500") +
      geom_jitter_interactive(
        aes(tooltip = area_name, data_id = area_name),
        height = 0.25, color = "steelblue3", size = 4, alpha = .7
      ) +
      theme_minimal() +
      labs(x = NULL, y = NULL)

    output$plot <- renderGirafe({
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
    jitterPlotServer("test", data)
  }

  shinyApp(ui, server) 
}

# Examples
jitterPlotTest(data = hi_vul_england)
