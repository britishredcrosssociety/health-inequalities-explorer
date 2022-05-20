selectBoxUI <- function(id, data) {
  selectizeInput(
    NS(id, "selectbox"),
    label = NULL,
    choices = sort(unique(data$area_name)),
    options = list(
      placeholder = "Select an area",
      onInitialize = I('function() { this.setValue(""); }')
    ) # list
  ) # selectizeInput
}

selectBoxServer <- function(id, selected_area) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$selectbox,
      {
        input$selectbox |>
          selected_area()
      },
      ignoreInit = TRUE
    )

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

selectBoxTest <- function(data) {
  ui <- fluidPage(
    selectBoxUI("test", data)
  )
  server <- function(input, output, session) {
    selected_area <- reactiveVal()
    selectBoxServer("test", selected_area)
  }
  shinyApp(ui, server)
}

# Examples
# selectBoxTest(data = boundaries_ltla21_england)