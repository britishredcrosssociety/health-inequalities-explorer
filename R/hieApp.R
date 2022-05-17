library(shiny)
library(sf)
library(dplyr)

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
        ) # tags$a
      ) # tags$div
    ), # fluidRow

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
        ) # tags$p
      ), # column
      column(width = 2)
    ) # fluidRow
  ) # fluidPage

  # ---- Server ----
  server <- function(input, output, session) {

  }
  shinyApp(ui, server)
}