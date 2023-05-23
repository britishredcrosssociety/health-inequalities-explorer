selectGeographyUI <- function(id) {
  selectizeInput(
    NS(id, "selectGeography"),
    label = NULL,
    choices = c(
      "England: Local Authorities" = "england_ltla_shp",
      "England: Integrated Care Boards" = "england_icb_shp",
      "Scotland: Local Authorities" = "scotland_ltla_shp",
      "Scotland: Health Boards" = "scotland_hb_shp"
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
