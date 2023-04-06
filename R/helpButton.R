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
          "Because each indicator uses different units, the data was
          normalised into the range of -1 to 1 to make them comparable. The
          underlying distribution of points for each indicator remains
          unchanged."
        ),
        tags$h5("How do I find the original indicator values?"),
        tags$p(
          "Hover over points to view their original non-normalised values."
        ),
        tags$h5("Why do clusters of points have similar values?"),
        tags$p(
          "Some data sets contain multiple areas scoring the same value. To 
          prevent these points being stacked on top of each other, random noise
          is added to their position. This means that some points that appear
          better/worse than others may actually perform identically. Always
          check the original values by hovering over points. Alternative
          plotting techniques to prevent this from happening are being 
          explored."
        ),
        tags$h5("How do I download a plot?"),
        tags$p(
          "Press the camera button in the top-right corner of the plot."
        ),
        tags$h5("How do I zoom into a set of points?"),
        tags$p(
          "Use the zoom and pan buttons in the top-right corner of the plot.
          Click the 'Reset axes' button to return to the original view."
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
