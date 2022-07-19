library(shiny)
library(leaflet)
library(ggplot2)
library(ggiraph)
library(sf)

explorer <- function() {

  # ---- UI ----
  ui <- fluidPage(

    # - Set CSS -
    includeCSS("inst/www/styles.css"),

    # - Logo -
    logo(),

    # - Instructions -
    instructions(),

    # - Geography Selection (module) -
    fluidRow(
      selectGeographyUI("geography"),
      align = "center"
    ),

    # - Area Selection (module) -
    fluidRow(
      selectAreasUI("areas"),
      align = "center"
    ),

    # - Map (module) & Plots (module) -
    fluidRow(

      # Column 1: map
      column(
        width = 4,
        align = "center",
        mapUI("leafletMap")
      ),

      # Column 2: left plot
      column(
        width = 4,
        align = "center",
        tags$div(
          id = "card",
          h4("Vulnerability"),
          h6("Last updated: 25.05.22"),
          tabsetPanel(
            tabPanel("Plot", jitterPlotUI("jitterPlotVulnerability"))
          )
        )
      ),

      # Column 3: right plot
      column(
        width = 4,
        align = "center",
        tags$div(
          id = "card",
          h4("Capacity"),
          h6("Last updated: 25.05.22"),
          tabsetPanel(
            tabPanel("Plot", jitterPlotUI("jitterPlotCapacity"))
          )
        )
      )
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