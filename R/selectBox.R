selectBoxUI <- function(id, data) {
  selectizeInput(
    NS(id, "selectbox"),
    label = NULL,
    choices = c("Select area(s)" = "", sort(unique(data$area_name))),
    multiple = TRUE
  )
}

selectBoxServer <- function(id, selected) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$selectbox,
      {
        selected$areas <- input$selectbox
      },
      ignoreInit = TRUE
    )

    # This sits in its own observer because it needs to track any changes to
    # the global `selected_area` reactive value, not just the selectbox (above)
    observeEvent(selected$areas, {
      updateSelectizeInput(
        session,
        "selectbox",
        selected = selected$areas
      )
    })
  })
}

selectBoxTest <- function(data) {
  ui <- fluidPage(
    selectBoxUI("test", data)
  )
  server <- function(input, output, session) {
    selected <- reactiveValues(areas = NULL)
    selectBoxServer("test", selected)
    observe({print(selected$areas)})
  }
  shinyApp(ui, server)
}

# Examples
# selectBoxTest(data = boundaries_ltla21_england)