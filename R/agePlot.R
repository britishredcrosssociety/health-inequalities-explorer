agePlotUI <- function(id) {
  plotOutput(
    NS(id, "plot")
  )
}

agePlotServer <- function(id, selected) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactive({
      ltla_demographics_age_england
    })

    output$plot <- renderPlot({
      dataset() |>
        dplyr::filter(area_name == "Hartlepool" | area_name == "Rochdale" | area_name == "Bury") |>
        ggplot(aes(x = population_relative, y = age, fill = area_name)) +
        facet_wrap(vars(sex), strip.position = "top") +
        geom_col(position = "dodge", alpha = .4) +
        geom_text(
          aes(label = population_label),
          position = position_dodge(width = .9),
          hjust = 1.35,
          # fontface = "bold",
          colour = "#262626"
        ) +
        scale_fill_manual(
          values = c("#D0021B", "#40A22A", "#F1B13B")
        ) +
        scale_x_continuous(labels = scales::percent) +
        labs(title = "Area populations", x = NULL, y = NULL) +
        theme_minimal() +
        theme(
          legend.position = "right",
          legend.title = element_blank(),
          text = element_text(face = "bold", size = 15)
        )
    })
  })
}

agePlotTest <- function() {
  ui <- fluidPage(
    agePlotUI("test")
  )

  server <- function(input, output, session) {
    selected <- reactiveValues(
      areas = vector(), geography = "ltla_shp_england"
    )
    agePlotServer("test", selected)
  }

  shinyApp(ui, server)
}

# Examples
# agePlotTest()
