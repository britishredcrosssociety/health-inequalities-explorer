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
      selectBoxUI("searchbox", boundaries_ltla21_england),
      align = "center"
    ),

    # - Map (module) & Plot (module) -
    fluidRow(
      column(width = 4, align = "center", mapUI("leafletmap")),
      column(width = 8, align = "center", jitterPlotUI("jitterplot"))
    )
  )

  # ---- Server ----
  server <- function(input, output, session) {

    # - Set an empty global reactive to be passed between modules -
    selected <- reactiveValues(areas = vector())

    # - Search Box (module) -
    selectBoxServer("searchbox", selected)

    # - Map (module) -
    mapServer("leafletmap", boundaries_ltla21_england, selected)

    # - Jitter Plot (module) -
    jitterPlotServer("jitterplot", hi_vul_england, selected)

    # Debug
    # observe({
    #   print(selected$areas)
    # })
  }

  shinyApp(ui, server)
}