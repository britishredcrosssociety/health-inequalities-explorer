library(shiny)
library(leaflet)
library(ggplot2)
library(ggiraph)
library(sf)
library(gridlayout)

explorer <- function() {

  # ---- UI ----
  ui <- grid_page(
    layout = c(
      "     1fr              1fr              1fr             ",
      "85px header           header           header          ",
      "85px select_areas     select_areas     select_areas    ",
      "85px select_geography select_geography select_geography",
      "1fr  map              plot_1           plot_2          "
    ),

    grid_card_text(
      "header",
      "Health Inequalities Explorer",
      alignment = "center"
    ),

    grid_card(
      "select_geography",
      selectGeographyUI("geography")
    ),

    grid_card(
      "select_areas",
      selectAreasUI("areas")
    ),

    grid_card(
      "map",
      mapUI("leafletMap")
    ),

    grid_card(
      "plot_1",
      jitterPlotUI("jitterPlotVulnerability")
    ),

    grid_card(
      "plot_2",
      jitterPlotUI("jitterPlotCapacity")
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
