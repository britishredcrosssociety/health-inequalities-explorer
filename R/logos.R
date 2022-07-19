logos <- function() {
  fluidRow(
     column(
      width = 4,
      align = "left",
      tags$div(
        class = "logo-brc-team",
        tags$a(
          href = "https://redcross.org.uk",
          target = "_blank",
          img(src = "www/logo-brc-team.jpg", width = 350)
        )
      )
    ),
    column(
      width = 4,
      align = "center",
      tags$div(
        class = "logo-hei",
        img(src = "www/logo-hei.jpg", width = 150)
      )
    ),
    column(
      width = 4,
      align = "right",
      tags$div(
        class = "logo-github",
        tags$a(
          href = "https://github.com/britishredcrosssociety/health-inequalities-explorer",
          target = "_blank",
          icon("github", "fa-2x")
        )
      )
    )
  )
}