server <- function(input, output, session) {
  
  # Debugging - check if rlock file being used
  if (file.exists("renv.lock")) {
    print("renv.lock file found. Using renv for package management.\n")
  } else {
    print("renv.lock file not found. Ensure that renv is configured for this project.\n")
  }
  
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
  indicatorDescriptionsServer("summaryDescriptions", selected, "summary_metrics")
  indicatorDescriptionsServer("secondaryCareDescriptions", selected, "secondary_care")
  indicatorDescriptionsServer("demographicsDescriptions", selected, "demographics")

  # Help buttons
  helpButtonServer("help_summary")
  helpButtonServer("help_secondary")
  helpButtonServer("help_demographics")

  # Debug
  # observe({
  #   print(selected$geography)
  # })
}
