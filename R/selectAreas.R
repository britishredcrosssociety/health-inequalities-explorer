selectAreasUI <- function(id, data) {
  selectizeInput(
    NS(id, "selectAreas"),
    label = NULL,
    choices = NULL,
    multiple = TRUE,
    options = list(
      plugins = list("remove_button"),
      placeholder = "Select areas to compare...",
      onInitialize = I('function() { this.setValue(""); }')
    )
  )
}

selectAreasServer <- function(id, data, selected) {
  moduleServer(id, function(input, output, session) {

    # Render server side to minimise user load
    updateSelectizeInput(
      session,
      "selectAreas",
      choices = sort(unique(data$area_name)),
      server = TRUE
    )

    observeEvent(input$selectAreas,
      {
        selected$areas <- input$selectAreas
      },
      ignoreNULL = FALSE
    )

    # This sits in its own observer because it needs to track any changes to
    # the global `selected$areas` reactive values, not just the selectizeInput
    observeEvent(selected$areas, {
      updateSelectizeInput(
        session,
        "selectAreas",
        selected = selected$areas
      )
    })
  })
}

selectAreasTest <- function(data) {
  ui <- fluidPage(
    selectAreasUI("test", data)
  )
  server <- function(input, output, session) {
    selected <- reactiveValues(areas = vector())
    selectAreasServer("test", data, selected)
  }
  shinyApp(ui, server)
}

# Examples
# selectAreasTest(data = boundaries_ltla21_england)