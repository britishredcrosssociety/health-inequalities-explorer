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
      tags$h3("Health Inequalities Explorer"),
      alignment = "center",
      has_border = TRUE
    ),
    grid_card(
      "select_geography",
      title = "Select type of care",
      selectGeographyUI("geography"),
      has_border = TRUE,
      scrollable = TRUE
    ),
    grid_card(
      "select_areas",
      title = "Select type of geography",
      selectAreasUI("areas"),
      has_border = TRUE,
      scrollable = TRUE
    ),
    grid_card(
      "map",
      mapUI("leafletMap"),
      has_border = TRUE
    ),
    grid_card(
      "summary_metrics",
      title = "Summary metrics",
      jitterPlotUI("jitterPlotVulnerability"),
      has_border = TRUE
    ),
    grid_card(
      "demographics",
      title = "Demographics",
      has_border = TRUE
    ),
    grid_card(
      "secondary_care",
      title = "Secondary Care",
      has_border = TRUE
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
