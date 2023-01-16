library(shiny)
library(gridlayout)
library(cicerone)

guide <- Cicerone$
  new(allow_close = FALSE)$
  step(
  "bins",
  "Test",
  "Test"
)

shinyApp(
  ui = grid_page(
    use_cicerone(),
    layout = c(
      "     200px   1fr   ",
      "85px header  header",
      "1fr  sidebar plot  "
    ),
    grid_card_text("header", "Geysers!", is_title = TRUE),
    grid_card(
      "sidebar",
      title = "Settings",
      sliderInput("bins", "Number of bins:",
        min = 1, max = 50, value = 30, width = "100%"
      )
    ),
    grid_card(
      "plot",
      plotOutput("distPlot", height = "100%")
    )
  ),
  server = function(input, output) {
    guide$init()$start()

    output$distPlot <- renderPlot({
      x <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = "darkgray", border = "white")
    })
  }
)
