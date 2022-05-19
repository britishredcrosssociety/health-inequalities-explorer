jitterPlotUI <- function(id) {
  girafeOutput(
    NS(id, "plot")
  )
}

jitterPlotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    gg <- hi_vul_england |>
      ggplot(aes(x = value, y = name)) +
      geom_vline(xintercept = 150, size = 2, alpha = .5, colour = "#FFA500") +
      geom_jitter_interactive(
        aes(tooltip = ltla21_name, data_id = ltla21_name),
        height = 0.25, color = "steelblue3", size = 4, alpha = .7
      ) +
      theme_minimal() +
      labs(x = NULL, y = NULL)

    output$plot <- renderGirafe({
      girafe(
        ggobj = gg,
        options = list(
          opts_hover_inv(css = "opacity:0.3;"),
          opts_toolbar(saveaspng = FALSE),
          opts_selection(type = "none")
        )
      )
    })
  })
}

jitterPlotTest <- function() {
  ui <- fluidPage(
    jitterPlotUI("test")
  )

  server <- function(input, output, session) {
    jitterPlotServer("test")
  }

  shinyApp(ui, server)
}