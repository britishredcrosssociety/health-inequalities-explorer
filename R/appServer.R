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
  jitterPlotServer("healthindexPlot", selected, "health_index")
  
  # Health Index subdomains
  tableServer("peopleSubdomainTable", selected, "people_subdomain")
  tableServer("placesSubdomainTable", selected, "places_subdomain")
  tableServer("livesSubdomainTable", selected, "lives_subdomain")

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
