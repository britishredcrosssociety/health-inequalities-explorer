selectAreasUI <- function(id) {
  selectizeInput(
    NS(id, "selectAreas"),
    label = NULL,
    choices = NULL,
    multiple = TRUE,
    options = list(
      maxItems = 8,
      onInitialize = I('function() { this.setValue(""); }'),
      plugins = list("remove_button"),
      placeholder = "Type to search or click the map..."
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
        print(selected$areas)
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
      areas = vector(), geography = "england_ltla_shp"
    )
    selectAreasServer("test", selected)
  }
  shinyApp(ui, server)
}

# Examples
selectAreasTest()
