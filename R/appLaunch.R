library(shiny)
library(leaflet)
library(ggplot2)
library(sf)
library(gridlayout)
library(plotly)

appLaunch <- function() {
  shinyApp(ui, server, enableBookmarking = "url")
}
