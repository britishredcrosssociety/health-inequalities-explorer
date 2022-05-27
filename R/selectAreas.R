selectAreasUI <- function(id) {
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

selectAreasServer <- function(id, selected) {
  moduleServer(id, function(input, output, session) {
    observeEvent(selected$geography, {
      # Render server side to minimise user load
      updateSelectizeInput(
        session,
        "selectAreas",
        choices = sort(unique(get(selected$geography)$area_name)),
        server = TRUE
      )
    })

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

selectAreasTest <- function() {
  ui <- fluidPage(
    selectAreasUI("test")
  )
  server <- function(input, output, session) {
    selected <- reactiveValues(
      areas = vector(), geography = "boundaries_ltla21_england"
    )
    selectAreasServer("test", selected)
  }
  shinyApp(ui, server)
}

# Examples
# selectAreasTest()