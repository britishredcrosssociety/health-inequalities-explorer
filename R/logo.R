logo <- function() {
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
  )
}