server <- function(input, output, session) {
  # Set an empty global reactive values list to be passed between modules
  selected <- reactiveValues(areas = vector(), geography = vector())

  # Call module server functions
  # Selections
  selectGeographyServer("geography", selected)
  selectAreasServer("areas", selected)
  mapServer("leafletMap", selected)

  # Plots
  jitterPlotServer("summaryPlot", selected, "summary_metrics")
  jitterPlotServer("secondaryCarePlot", selected, "secondary_care")
  jitterPlotServer("demographicsPlot", selected, "demographics")

  # Indicator descriptions
  indicatorDescriptionsServer("summaryDescriptions")
  indicatorDescriptionsServer("secondaryCareDescriptions")
  indicatorDescriptionsServer("demographicsDescriptions")

  # Help buttons
  helpButtonServer("help_summary")
  helpButtonServer("help_secondary")
  helpButtonServer("help_demographics")

  # Debug
  # observe({
  #   print(selected$geography)
  # })
}
