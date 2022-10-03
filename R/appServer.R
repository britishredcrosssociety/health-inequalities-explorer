server <- function(input, output, session) {

  # Set an empty global reactive values list to be passed between modules
  selected <- reactiveValues(areas = vector(), geography = vector())

  # Call module server functions
  selectGeographyServer("geography", selected)
  selectAreasServer("areas", selected)
  mapServer("leafletMap", selected)
  jitterPlotServer("summaryPlot", selected)
  agePlotServer("agePlot", selected)

  # Debug
  # observe({
  #   print(selected$geography)
  # })
}
