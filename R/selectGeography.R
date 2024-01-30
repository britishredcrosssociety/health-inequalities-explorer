selectGeographyUI <- function(id) {
  selectizeInput(
    NS(id, "selectGeography"),
    label = NULL,
    choices = list("Local Authorities and ICB" = c(
      "England: Local Authorities" = "england_ltla_shp",
      "England: Integrated Care Boards" = "england_icb_shp",
      "Scotland: Local Authorities" = "scotland_ltla_shp",
      "Scotland: Health Boards" = "scotland_hb_shp",
      "Northern Ireland: Local Authorities" = "northern_ireland_ltla_shp",
      "Northern Ireland: Health and Social Care Trusts" = "northern_ireland_hsct_shp",
      "Wales: Local Authorities" = "wales_ltla_shp",
      "Wales: Local Health Boards" = "wales_lhb_shp"
    ),
    "BRC Areas" = c(
      "Central" = "brc_central_shp",
      "London" = "brc_london_shp",
      "North" = "brc_north_shp",
      "Northern Ireland and Isle of Man" = "northern_ireland_ltla_shp_copy",
      "Scotland" = "scotland_ltla_shp_copy",
      "South and the Channel Islands" = "brc_south_shp",
      "South East" = "brc_southeast_shp",
      "Wales" = "wales_ltla_shp_copy"
      
    )),
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
 selectGeographyTest()
