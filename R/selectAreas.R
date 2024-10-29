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
      area_choices <- NULL
      if(selected$geography != "") area_choices <- sort(unique(get(selected$geography)$area_name))
      
      # Render server side to minimise user load
      updateSelectizeInput(
        session,
        "selectAreas",
        choices = area_choices,
        server = TRUE
      )
    })
    
    # Debounce the selectAreas input to reduce updates frequency
    debounced_areas <- debounce(reactive(input$selectAreas), 1000)
    
    # Update selected$areas only after the debounced input triggers
    observeEvent(debounced_areas(), {
      selected$areas <- debounced_areas()
      selected$timestamp <- Sys.time() 
    }, ignoreNULL = FALSE)
    
    # Observe changes to selected$areas and update selectize input
    observeEvent(selected$areas, {
      updateSelectizeInput(
        session,
        "selectAreas",
        selected = selected$areas
      )
    })
  })
}

# Test
selectAreasTest <- function() {
  ui <- fluidPage(
    selectAreasUI("test"),
    tableOutput("selectedInfo") 
  )
  
  server <- function(input, output, session) {
    selected <- reactiveValues(
      areas = vector(), geography = "england_ltla_shp", timestamp = NULL
    )
    
    selectAreasServer("test", selected)
    
    output$selectedInfo <- renderTable({
      data.frame(
        Selected_Areas = paste(selected$areas, collapse = ", "),
        Timestamp = as.character(selected$timestamp)
      )
    })
  }
  
  shinyApp(ui, server)
}

# Examples
selectAreasTest()
