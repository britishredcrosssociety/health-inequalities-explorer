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

selectAreasServer <- function(id, selected_area) {
  moduleServer(id, function(input, output, session) {
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
  }) # moduleServer
}

selectAreasTest <- function() {
  ui <- fluidPage(
    selectAreasUI("test")
  )
  server <- function(input, output, session) {
    selected_area <- reactiveVal()

    selectAreasServer("test", selected_area)

    # # Debug
    # observe({
    #   print(selected_area())
    # })
  }
  shinyApp(ui, server)
}