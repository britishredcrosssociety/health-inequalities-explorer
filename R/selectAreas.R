selectAreasUI <- function(id) {
  selectizeInput(
    NS(id, "selectbox"),
    label = NULL,
    choices = sort(unique(boundaries_ltla21_england$ltla21_name)),
    options = list(
      placeholder = "Select a Local Authority",
      onInitialize = I('function() { this.setValue(""); }')
    ) # list
  ) # selectizeInput
}

selectAreasServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    selected_area <- reactiveVal()
    observeEvent(input$selectbox,
      {
        boundaries_ltla21_england |>
          st_drop_geometry() |>
          filter(ltla21_name == input$selectbox) |>
          select(ltla21_name) |>
          pull() |>
          selected_area()
      },
      ignoreInit = TRUE
    ) # observeEvent
    # Debug
    observe({
      print(selected_area())
    }) # observe
  }) # moduleServer
}

selectAreasApp <- function() {
  ui <- fluidPage(
    selectAreasUI("test")
  )
  server <- function(input, output, session) {
    selectAreasServer("test")
  }
  shinyApp(ui, server)
}