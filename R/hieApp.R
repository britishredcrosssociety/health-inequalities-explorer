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
    fluidRow(
      align = "center",
      tags$div(
        class = "brc-logo",
        tags$a(
          href = "https://redcross.org.uk",
          target = "_blank",
          img(src = "www/brc-team-logo.jpg", width = 400)
        )
      )
    ),

    # - Instructions -
    fluidRow(
      column(width = 2),
      column(
        width = 8,
        align = "center",
        tags$h1(
          "Health Inequalities Explorer"
        ), # tags$h1
        tags$p(
          "You can use this tool to explore health inequalities and how they
          compare across different geographies across the UK."
        )
      ),
      column(width = 2)
    ),

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