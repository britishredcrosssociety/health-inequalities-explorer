server <- function(input, output, session) {

  # Set an empty global reactive values list to be passed between modules
  selected <- reactiveValues(areas = vector(), geography = vector())

  # Call module server functions
  selectGeographyServer("geography", selected)
  selectAreasServer("areas", selected)
  mapServer("leafletMap", selected)
  jitterPlotServer("summaryPlot", selected, "summary_metrics")
  jitterPlotServer("secondaryCarePlot", selected, "secondary_care")
  jitterPlotServer("agePlot", selected, "demographics_age")
  
  # Debug
  # observe({
  #   print(selected$geography)
  # })
}
