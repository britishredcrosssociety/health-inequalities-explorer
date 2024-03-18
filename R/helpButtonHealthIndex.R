helpButtonHIUI <- function(id) {
  actionButton(
    NS(id, "button"),
    label = "?",
    style = "text-align: right;"
  )
}

helpButtonHIServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$button, {
      showModal(modalDialog(
        title = NULL,
        easyClose = TRUE,
        tags$h5("How do I interpret the Health Index Domain and Sub-Domain scores?"),
        tags$p(
          "Higher numbers always mean better health and a lower number means worse
          health. A score of 100 represents average levels of health in England in 2015.
          Scores higher than 100 therefore indicate better health than England in 2015."
        ),
        tags$h5("What is the minimum and maximum score?"),
        tags$p(
          "The minimum score across domains is 70 and the maximum score is 130."
        )
      ))
    })
  })
}


helpButtonHITest <- function() {
  ui <- fluidPage(
    helpButtonHIUI("test")
  )
  server <- function(input, output, session) {
    helpButtonHIServer("test")
  }
  shinyApp(ui, server)
}

# test
helpButtonHITest()
