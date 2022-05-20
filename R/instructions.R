instructions <- function() {
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
  )
}