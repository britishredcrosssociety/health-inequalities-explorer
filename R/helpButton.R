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
        tags$h5("Download a plot"),
        tags$p(
          "Hover over the top-right corner of the plot and press the camera
           symbol."
        ),
        tags$h5("Understand the x-axes"),
        tags$p(
          "Because the indicators use different units, they have all been
          squashed to a range of -1 to 1 so they can all be lined up on the same
          plot."
        ),
        tags$h5("View the original units"),
        tags$p(
          "Hover over any point on the plot."
        ),
        tags$h5("Zoom into an area"),
        tags$p(
          "Click and drag the cursor in the plot area to create a box and select
          a set of points to zoom in on. Double click in the plot area to return
          to the original view."
        ),
        tags$h5("Remove points from the plot"),
        tags$p(
          "Click any points in the legend to toggle them on/off the plot."
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
