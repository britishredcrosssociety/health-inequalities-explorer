library(shiny)
library(leaflet)
library(ggplot2)
library(sf)
library(gridlayout)

appLaunch <- function() {
  shinyApp(ui, server, enableBookmarking = "url")
}
