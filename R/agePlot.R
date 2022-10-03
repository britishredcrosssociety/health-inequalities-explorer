agePlotUI <- function(id) {
  plotOutput(
    NS(id, "plot")
  )
}

agePlotServer <- function(id) {
  moduleServer(id, function(input, output, session){
    output$plot <- renderPlot({
      
    })
  })
}

agePlotTest <- function() {
  ui <- fluidPage(
    agePlotUI("test")
  )

  server <- function(input, output, session) {
    agePlotServer("test")
  }

  shinyApp(ui, server)
}

# Examples
# agePlotTest()