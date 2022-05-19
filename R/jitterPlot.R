jitterPlotUI <- function(id) {
  girafeOutput(
    NS(id, "plot")
  )
}

jitterPlotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderGirafe({
      girafe(ggobj = gg)
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