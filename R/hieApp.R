library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(ggiraph)

hieApp <- function() {

  # ---- UI ----
  ui <- fluidPage(

    # - Set CSS -
    includeCSS("inst/www/styles.css"),

    # - Logo -
    logo(),

    # - Instructions -
    instructions(),

    # - Search Box (module) -
    selectBoxUI("searchbox"),

    # - Map (module) -
    mapUI("leafletmap")
  )

  # ---- Server ----
  server <- function(input, output, session) {

    # - Set an empty global reactive to be passed between modules -
    selected_area <- reactiveVal()

    # - Search Box (module) -
    selectBoxServer("searchbox", selected_area)

    # - Map (module) -
    mapServer("leafletmap", selected_area)

    # Debug
    observe({
      print(selected_area())
    })
  }

  shinyApp(ui, server)
}