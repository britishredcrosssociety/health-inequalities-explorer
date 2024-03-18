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
        ),
        tags$h5("How are sub-domain scores calculated?"),
        tags$p(
          "Each sub-domain consists of a number of indicators. For example, 
          the sub-domain - difficulties in daily life - takes the combined score of two indicators:"
        ),
        tags$ul(
          tags$li("The percentage of working-age adults (aged 16 to 64 years) 
              who are disabled under the Equality Act or work-limiting disabled; and"),
          tags$li("The number of emergency hospital admissions for a fractured neck 
              or femur in people aged 65 years and over, per 100k people, age-standardised")
        ),
        tags$p(
          "Click ",
          tags$a(
            href = "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/methodologies/healthindexindicatorsanddefinitions", 
            "here"
          ),
          " for more information on the underlying indicators per sub-domain."
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
