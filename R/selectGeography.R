selectGeographyUI <- function(id) {
  selectizeInput(
    NS(id, "selectGeography"),
    label = NULL,
    choices = c(
      "Local Authority" = "ltla_shp_england",
      "Integrated Care Board" = "icb_shp_england"
      # "NHS Trusts" = "trusts_shp_england"
    ),
    multiple = FALSE
  )
}

selectGeographyServer <- function(id, selected) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$selectGeography, {
      selected$geography <- input$selectGeography
    })
  })
}

selectGeographyTest <- function() {
  ui <- fluidPage(
    selectGeographyUI("test")
  )
  server <- function(input, output, session) {
    selected <- reactiveValues(geography = vector())
    selectGeographyServer("test", selected)
  }
  shinyApp(ui, server)
}

# Examples
# selectGeographyTest()
