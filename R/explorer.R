library(shiny)
library(leaflet)
library(ggplot2)
library(sf)
library(gridlayout)

explorer <- function() {

  # ---- UI ----
  ui <- grid_page(
    layout = grid_config,
    grid_card(
      "header",
      h3("Health Inequalities Explorer"),
      alignment = "center",
      has_border = FALSE
    ),
    grid_card(
      "select_geography",
      selectGeographyUI("geography"),
      has_border = FALSE,
      scrollable = TRUE
    ),
    grid_card(
      "select_areas",
      selectAreasUI("areas"),
      has_border = FALSE,
      scrollable = TRUE
    ),
    grid_card(
      "map",
      mapUI("leafletMap"),
      has_border = FALSE
    ),
    grid_card(
      "plot_1",
      jitterPlotUI("jitterPlotVulnerability"),
      has_border = FALSE
    ),
    grid_card(
      "plot_2",
      jitterPlotUI("jitterPlotCapacity"),
      has_border = FALSE
    )
  )

  # ---- Server ----
  server <- function(input, output, session) {

    # Load data sets (to be replaced withy dynamic selection from selected$geography)
    vulnerability <- ltla_vul_england
    capacity <- ltla_cap_england

    # - Set an empty global reactive values list to be passed between modules -
    selected <- reactiveValues(areas = vector(), geography = vector())

    # - Geography Selection -
    selectGeographyServer("geography", selected)

    # - Area Selection (module) -
    selectAreasServer("areas", selected)

    # - Map (module) -
    mapServer("leafletMap", selected)

    # - Jitter Plot Left (module) -
    jitterPlotServer("jitterPlotVulnerability", selected, "vulnerability")

    # - Jitter Plot Right (module) -
    jitterPlotServer("jitterPlotCapacity", selected, "capacity")

    # Debug
    # observe({
    #   print(selected$geography)
    # })
  }

  shinyApp(ui, server)
}
