server <- function(input, output, session) {

  # - Set an empty global reactive values list to be passed between modules -
  selected <- reactiveValues(areas = vector(), geography = vector())

  # - Geography Selection -
  selectGeographyServer("geography", selected)

  # - Area Selection (module) -
  selectAreasServer("areas", selected)

  # - Map (module) -
  mapServer("leafletMap", selected)

  # - Jitter Plot Left (module) -
  jitterPlotServer("jitterPlotVulnerability", selected)

  # Debug
  # observe({
  #   print(selected$geography)
  # })
}
