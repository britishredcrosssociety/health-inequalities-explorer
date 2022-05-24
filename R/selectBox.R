selectBoxUI <- function(id, data) {
  selectizeInput(
    NS(id, "selectbox"),
    label = NULL,
    choices = c("Select areas to compare..." = "", sort(unique(data$area_name))),
    multiple = TRUE,
    options = list(
      plugins = list("remove_button")
    )
  )
}

selectBoxServer <- function(id, selected) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$selectbox, {
      selected$areas <- input$selectbox
    }, ignoreNULL = FALSE)

    # This sits in its own observer because it needs to track any changes to
    # the global `selected$areas` reactive values, not just the selectbox
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
    selected <- reactiveValues(areas = vector())
    selectBoxServer("test", selected)
  }
  shinyApp(ui, server)
}

# Examples
# selectBoxTest(data = boundaries_ltla21_england)