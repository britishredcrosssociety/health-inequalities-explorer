# ---- ggplotly fun ----
ggplotly_default_bar <- function(plot, number_areas) {
  # Set Height of plot to be a factor of the number of areas selected
  # number_areas <- length(data$area_name)

  pixel <- ifelse(number_areas == 2,100,
                    ifelse(number_areas == 1, 200, 60))
  y_offset <- ifelse(number_areas <2, 0.5, 0.6)
  
  ggplotly(
    plot,
    height = number_areas * pixel,
    width = 300,
    tooltip = c("text")
  ) |> 
    add_annotations(
      x = 85,
      y = number_areas + y_offset, 
      text = "◄ Worse than mean",
      showarrow = FALSE,
      font = list(size = 8) 
    ) |>
    add_annotations(
      x = 120,
      y = number_areas + y_offset, 
      text = "Better than mean ►",
      showarrow = FALSE,
      font = list(size = 8) 
    ) |>
    config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = list(
        "zoom",
        # "pan",
        "select",
        # "zoomIn",
        # "zoomOut",
        "autoScale",
        # "resetScale",
        "lasso2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      ),
      # Download button
      toImageButtonOptions = list(
        height = NULL,
        width = NULL,
        scale = 6
      )
    )
}

# ---- Plot while waiting for selection ----
# To plot England Average as default
bar_plot_mean_only <- function(data, selected_geography) {
  number_areas <- 1
  
  # if (selected_geography == "england_ltla_shp") {
  data <- data |> filter(area_name %in% c("England Average", "Scotland Average"))
  number_areas <- length(data$area_name)
  # }
  
  plot <-
    data |>
    ggplot(
      aes(
        x = number,
        y = area_name,
        text = label
      )
    ) +
    geom_point(size = 3, color = "black") +
    geom_segment(aes(x = 70, xend = number, yend = area_name), color = "black") +
    geom_text(aes(x = 90, y = area_name, label = area_name), color = "black", size = 3, nudge_y = 0.2) +
    xlim(70, 130) +
    labs(
      x = "Score",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 9),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  
  ggplotly_default_bar(plot, number_areas)
}

# Example usage:
# bar_plot_null(england_ltla_hi_social_determinants)


# ---- Plot selected areas ----
bar_plot_selected <- function(data, selected_areas) {
  data <- 
    data |>
    filter(area_name %in% c(selected_areas, "England Average", "Scotland Average"))
  
  plot <-
    data |>
    ggplot() +
    aes(
      x = number,
      y = area_name,
      text = label,
      fill = area_name
    ) +
    geom_point(size = 3) +
    geom_segment(aes(x = 70, xend = number - 1, yend = area_name), color = "black") +
    geom_text(aes(x = 95, y = area_name, label = area_name), color = "black", size = 3, nudge_y = 0.3) +
    xlim(70, 130) +
    scale_fill_manual(
      # https://waldyrious.net/viridis-palette-generator/
      # No. of colours = maximum number of area selections + 1
      values = c("#0d0887", "#febd2a", "#b83289", "#f48849", "#f0f921", "#8b0aa5", "#db5c68", "#5302a3", "#000004"),
    ) +
    labs(
      x = "Score", y = NULL
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 9),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
  
  number_areas <- length(data$area_name)

  ggplotly_default_bar(plot, number_areas)
}


# Testing
# bar_plot_selected(england_ltla_hi_risk_factors, c("Kensington and Chelsea", "Redcar and Cleveland"))
