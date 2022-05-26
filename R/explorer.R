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
      column(width = 4, align = "center", mapUI("leafletMap")),

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

    # - Set an empty global reactive values list to be passed between modules -
    selected <- reactiveValues(areas = vector(), geography = vector())

    # ==== Work in progress ====================================================
    # Currently set data sets from global values loaded into namespace with
    # pkgload::load_all(".")
    vulnerability <- hi_vul_england
    capacity <- hi_cap_england
    boundaries <- boundaries_ltla21_england
    
    # - Set data sets based off geographical selection -
    # Require reactive value to be calculated before retrieving data set as
    # boundaries intitiates to logical(0) as it waits for reactive depency to
    # first 'kick in'.

    # This reactive is returning a quosure (function), so it isn't subsettable.
    # Needs to be convered into an object
    # boundaries <- reactive({
    #   req(selected$geography) |> get()
    # })

    # observe({print(all.equal(dataset(), boundaries))})
    # ==========================================================================

    # - Geography Selection -
    selectGeographyServer("geography", selected)

    # - Area Selection (module) -
    selectAreasServer("areas", boundaries, selected)

    # - Map (module) -
    mapServer("leafletMap", boundaries, selected)

    # - Jitter Plot Left (module) -
    jitterPlotServer("jitterPlotVulnerability", vulnerability, selected)

    # - Jitter Plot Right (module) -
    jitterPlotServer("jitterPlotCapacity", capacity, selected)

    # Debug
    # observe({
    #   print(selected$geography)
    # })
  }

  shinyApp(ui, server)
}