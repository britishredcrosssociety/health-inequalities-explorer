library(shiny)
library(leaflet)
library(ggplot2)
library(ggiraph)

explorerApp <- function() {

  # ---- UI ----
  ui <- fluidPage(

    # - Set CSS -
    includeCSS("inst/www/styles.css"),

    # - Logo -
    logo(),

    # - Instructions -
    instructions(),

    # - Search Box (module) -
    fluidRow(
      selectBoxUI("searchBox", boundaries_ltla21_england),
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

    # - Set an empty global reactive to be passed between modules -
    selected <- reactiveValues(areas = vector())

    # - Search Box (module) -
    selectBoxServer("searchBox", boundaries_ltla21_england, selected)

    # - Map (module) -
    mapServer("leafletMap", boundaries_ltla21_england, selected)

    # - Jitter Plot Left (module) -
    jitterPlotServer("jitterPlotVulnerability", hi_vul_england, selected)

    # - Jitter Plot Right (module) -
    jitterPlotServer("jitterPlotCapacity", hi_cap_england, selected)

    # Debug
    # observe({
    #   print(selected$areas)
    # })
  }

  shinyApp(ui, server)
}