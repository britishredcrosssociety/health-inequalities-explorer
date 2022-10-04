jitterPlotUI <- function(id) {
  plotOutput(
    NS(id, "plot")
  )
}

jitterPlotServer <- function(id, selected, type) {
  moduleServer(id, function(input, output, session) {

    # Select dataset based on geographical selection and type of data
    dataset <- reactive({
      if (selected$geography == "ltla_shp_england" & type == "demographics_age") {
        ltla_demographics_age_england
      } else if (selected$geography == "ltla_shp_england" & type == "summary_metrics") {
        ltla_summary_metrics_england
      }
    })

    output$plot <- renderPlot({
      if (is.null(selected$areas) & type == "demographics_age") {
        dataset() |>
          ggplot(aes(x = population_relative, y = age)) +
          geom_point(
            position = position_jitter(height = 0.25, width = 0.1, seed = 123),
            size = 5,
            shape = 21,
            alpha = 0.1,
            fill = "#717171",
            colour = "#262626",
          ) +
          scale_x_continuous(labels = scales::percent) +
          theme_minimal() +
          labs(x = NULL, y = NULL) +
          theme(text = element_text(face = "bold", size = 15))
      } else if (is.null(selected$areas) & type == "summary_metrics") {
        dataset() |>
          ggplot(aes(x = value, y = variable)) +
          geom_vline(
            xintercept = 0,
            size = 1,
            alpha = .7,
            colour = "#262626",
            linetype = "dashed"
          ) +
          geom_point(
            position = position_jitter(height = 0.25, width = 0.1, seed = 123),
            size = 5,
            shape = 21,
            alpha = 0.1,
            fill = "#717171",
            colour = "#262626",
          ) +
          annotate("text", x = 0, y = 3.75, label = "bold(Mean)", parse = TRUE) +
          theme_minimal() +
          labs(x = NULL, y = NULL) +
          theme(text = element_text(face = "bold", size = 15))
      } else if (type == "demographics_age") {
        dataset() |>
          dplyr::mutate(
            selected = dplyr::if_else(
              area_name %in% selected$areas,
              area_name,
              "not selected"
            )
          ) |>
          dplyr::mutate(
            alpha = dplyr::if_else(selected != "not selected", 1, 0.1)
          ) |>
          dplyr::mutate(selected = factor(selected)) |>
          dplyr::mutate(selected = relevel(selected, ref = "not selected")) |>
          ggplot(aes(x = population_relative, y = age, fill = selected)) +
          geom_point(
            aes(alpha = alpha),
            position = position_jitter(height = 0.25, width = 0.1, seed = 123),
            size = 5,
            shape = 21,
            colour = "#262626"
          ) +
          scale_x_continuous(labels = scales::percent) +
          theme_minimal() +
          theme(
            legend.position = "top",
            legend.title = element_blank()
          ) +
          scale_fill_manual(
            values = c("#D0021B", "#40A22A", "#F1B13B", "#6A9EAA"),
            breaks = selected$areas
          ) +
          scale_alpha(guide = "none") +
          labs(x = NULL, y = NULL) +
          theme(text = element_text(face = "bold", size = 15))
      } else if (type == "summary_metrics") {
        dataset() |>
          dplyr::mutate(
            selected = dplyr::if_else(
              area_name %in% selected$areas,
              area_name,
              "not selected"
            )
          ) |>
          dplyr::mutate(
            alpha = dplyr::if_else(selected != "not selected", 1, 0.1)
          ) |>
          dplyr::mutate(selected = factor(selected)) |>
          dplyr::mutate(selected = relevel(selected, ref = "not selected")) |>
          ggplot(aes(x = value, y = variable, fill = selected)) +
          geom_vline(
            xintercept = 0,
            size = 1,
            alpha = .7,
            colour = "#262626",
            linetype = "dashed"
          ) +
          geom_point(
            aes(alpha = alpha),
            position = position_jitter(height = 0.25, width = 0.1, seed = 123),
            size = 5,
            shape = 21,
            colour = "#262626"
          ) +
          annotate("text", x = 0, y = 3.75, label = "bold(Mean)", parse = TRUE) +
          theme_minimal() +
          theme(
            legend.position = "top",
            legend.title = element_blank()
          ) +
          scale_fill_manual(
            values = c("#D0021B", "#40A22A", "#F1B13B", "#6A9EAA"),
            breaks = selected$areas
          ) +
          scale_alpha(guide = "none") +
          labs(x = NULL, y = NULL) +
          theme(text = element_text(face = "bold", size = 15))
      }
    })
  })
}

jitterPlotTest <- function() {
  ui <- fluidPage(
    jitterPlotUI("test")
  )

  server <- function(input, output, session) {
    selected <- reactiveValues(
      areas = vector(), geography = "ltla_shp_england"
    )
    jitterPlotServer("test", selected, type = "demographics_age")
  }

  shinyApp(ui, server)
}

# Examples
# jitterPlotTest()
