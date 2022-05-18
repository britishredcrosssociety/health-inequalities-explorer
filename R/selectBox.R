selectBoxUI <- function(id) {
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

selectBoxServer <- function(id, selected_area) {
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

    # This sits in its own observer because it needs to track any changes to
    # the global `selected_area` reactive value, not just the selectbox (above)
    observeEvent(selected_area(), {
      updateSelectInput(
        session,
        "selectbox",
        selected = selected_area()
      )
    })
  }) # moduleServer
}

selectBoxTest <- function() {
  ui <- fluidPage(
    selectBoxUI("test")
  )
  server <- function(input, output, session) {
    selected_area <- reactiveVal()

    selectBoxServer("test", selected_area)

    # # Debug
    # observe({
    #   print(selected_area())
    # })
  }
  shinyApp(ui, server)
}