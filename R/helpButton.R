helpButtonUI <- function(id) {
  actionButton(
    NS(id, "button"),
    label = "?"
  )
}

helpButtonServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$button, {
      showModal(modalDialog(
        title = NULL,
        easyClose = TRUE,
        tags$h5("Why do the indicators go from -1 to 1?"),
        tags$p(
          "Because each indicator uses different scales, the data was
          normalised into the range of -1 to 1 to make them comparable. The
          underlying distribution of points for each indicator remains
          unchanged."
        ),
        tags$h5("How do I find the original indicator values?"),
        tags$p(
          "Hover over points to view the non-normalised values."
        ),
        tags$h5("How do I download a plot?"),
        tags$p(
          "Hover over the top-right corner of the plot and press the camera
           button."
        ),
        tags$h5("How do I zoom into a set of points?"),
        tags$p(
          "Click and drag the cursor to zoom into a set of points. Double-click
          anywhere in the plot to cancel."
        ),
        tags$h5("How do I toggle points on/off the plot?"),
        tags$p(
          "Click any points in the legend to toggle them on/off."
        )
      ))
    })
  })
}


helpButtonTest <- function() {
  ui <- fluidPage(
    helpButtonUI("test")
  )
  server <- function(input, output, session) {
    helpButtonServer("test")
  }
  shinyApp(ui, server)
}
