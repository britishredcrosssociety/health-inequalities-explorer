library(shiny)
library(cicerone)
library(gridlayout)
library(leaflet)
library(ggplot2)
library(sf)
library(plotly)

appLaunch <- function() {
  shinyApp(ui, server, enableBookmarking = "url")
}
