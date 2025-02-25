server <- function(input, output, session) {
  # Set an empty global reactive values list to be passed between modules
  selected <- reactiveValues(areas = vector(), geography = vector(), region = vector())

  # Call module server functions
  # Selections
  selectRegionServer("region", selected)
  selectGeographyServer("geography", selected)
  selectAreasServer("areas", selected)
  mapServer("leafletMap", selected)

  # Jitter Plots
  jitterPlotServer("summaryPlot", selected, "summary_metrics")
  jitterPlotServer("secondaryCarePlot", selected, "secondary_care")
  jitterPlotServer("demographicsPlot", selected, "demographics")
  jitterPlotServer("healthindexPlot", selected, "health_index")

  # Health Index domain plots
  barPlotServer("hioutcomesPlot", selected, "hi_outcomes")
  barPlotServer("hiriskfactorsPlot", selected, "hi_risk_factors")
  barPlotServer("hisocialdeterminantsPlot", selected, "hi_social_determinants")

  # Health Index subdomains
  tableServer("subdomainTable", selected)

  # Indicator descriptions
  indicatorDescriptionsServer("summaryDescriptions", selected, "summary_metrics")
  indicatorDescriptionsServer("secondaryCareDescriptions", selected, "secondary_care")
  indicatorDescriptionsServer("demographicsDescriptions", selected, "demographics")

  # Help buttons
  helpButtonServer("help_summary")
  helpButtonServer("help_secondary")
  helpButtonServer("help_demographics")
  helpButtonServer("help_health_index")
  helpButtonHIServer("help_hi_domains")

  # Debug
  # observe({
  #   print(selected$geography)
  # })
}
