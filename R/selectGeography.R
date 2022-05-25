selectGeographyUI <- function(id) {
  selectizeInput(
    NS(id, "selectGeography"),
    label = NULL,
    choices = c("Integrated Care System", "Local Authority"),
    multiple = FALSE
  )
}

selectGeographyServer <- function(id, selected_geography) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$selectGeography,
      {
        selected_geography(input$selectGeography)
      }
    )
  })
}

selectGeographyTest <- function() {
  ui <- fluidPage(
    selectGeographyUI("test")
  )
  server <- function(input, output, session) {
    selected_geography <- reactiveVal()
    selectGeographyServer("test", selected_geography)
    observe({print(selected_geography())})
  }
  shinyApp(ui, server)
}

# Examples
# selectGeographyTest()