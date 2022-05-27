jitterPlotUI <- function(id) {
  girafeOutput(
    NS(id, "plot")
  )
}

jitterPlotServer <- function(id, selected, type) {
  moduleServer(id, function(input, output, session) {

    # Set dataset based on geography selection and type of dataset
    dataset <- reactive({
      if (selected$geography == "ltla_shp_england" && type == "vulnerability") {
        ltla_vul_england
      } else {
        ltla_cap_england
      }
    })

    output$plot <- renderGirafe({

      # Set sensible plot/legend default if no area has been selected
      if (is.null(selected$areas)) {
        dataset <- dataset() |>
          dplyr::mutate(selected = "not selected") |>
          dplyr::mutate(alpha = 0.2)
        legend_break_name <- NULL
      } else {
        dataset <- dataset() |>
          dplyr::mutate(
            selected = dplyr::if_else(
              area_name %in% selected$areas,
              area_name,
              "not selected"
            )
          ) |>
          dplyr::mutate(
            alpha = dplyr::if_else(selected != "not selected", 1, 0.2)
          ) |>
          dplyr::mutate(selected = factor(selected)) |>
          dplyr::mutate(selected = relevel(selected, ref = "not selected"))
        legend_break_name <- selected$areas
      }

      # Create plot object
      gg <- dataset |>
        ggplot(aes(x = value, y = variable, colour = selected)) +
        geom_vline(
          xintercept = 150, size = 2, alpha = .5, colour = "#5C747A"
        ) +
        geom_jitter_interactive(
          aes(alpha = alpha, tooltip = area_name, data_id = area_name),
          height = 0.25,
          size = 4
        ) +
        theme_minimal() +
        theme(
          legend.position = "top",
          legend.title = element_blank()
        ) +
        scale_color_viridis_d(
          option = "C", begin = .2, end = .8, direction = -1,
          breaks = legend_break_name
        ) +
        scale_alpha(guide = "none") +
        labs(x = NULL, y = NULL)

      # Render plot
      girafe(
        ggobj = gg,
        options = list(
          # opts_hover_inv(css = "opacity:0.3;"),
          opts_toolbar(saveaspng = FALSE),
          opts_selection(type = "none"),
          opts_sizing(rescale = TRUE, width = .5)
        )
      )
    })
  })
}

jitterPlotTest <- function(type) {
  ui <- fluidPage(
    jitterPlotUI("test")
  )

  server <- function(input, output, session) {
    selected <- reactiveValues(
      areas = vector(), geography = "ltla_shp_england"
    )
    jitterPlotServer("test", selected, type)
  }

  shinyApp(ui, server)
}

# Examples
# jitterPlotTest(type = "vulnerability")