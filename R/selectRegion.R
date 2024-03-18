selectRegionUI <- function(id) {
  selectizeInput(
    NS(id, "selectRegion"),
    label = NULL,
    choices = list(
      "Nations" = c(
        "England" = "england",
        "Northern Ireland" = "northern_ireland",
        "Scotland" = "scotland",
        "Wales" = "wales"
      ),
      
      "Regions in England" = c(
        "Central" = "england_central",
        "London" = "england_london",
        "North" = "england_north",
        "South and the Channel Islands" = "england_south",
        "South East" = "england_southeast"
      )
    ),
    selected = "England",
    multiple = FALSE
  )
}

selectRegionServer <- function(id, selected) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$selectRegion, {
      selected$region <- input$selectRegion
    })
  })
}

selectRegionTest <- function() {
  ui <- fluidPage(
    selectRegionUI("test")
  )
  server <- function(input, output, session) {
    selected <- reactiveValues(region = vector())
    selectRegionServer("test", selected)
  }
  shinyApp(ui, server)
}

# Examples
# selectRegionTest()
