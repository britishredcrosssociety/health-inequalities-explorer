# ---- ggplotly fun ----
ggplotly_default_bar <- function(plot, number_areas) {
  # Set Height of plot to be a factor of the number of areas selected
  # number_areas <- length(data$area_name)

  pixel <- ifelse(number_areas == 2, 100,
    ifelse(number_areas == 1, 200, 60)
  )
  y_offset <- ifelse(number_areas < 2, 0.5, 0.6)

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
    geom_point(size = 3, color = "grey") +
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
# bar_plot_mean_only(england_ltla_hi_social_determinants)


# ---- Plot selected areas ----
bar_plot_selected <- function(data, selected_areas) {
  data <- data |>
    filter(area_name %in% c(selected_areas, "England Average", "Scotland Average"))

  national_average <- data |>
    filter(area_name %in% c("England Average", "Scotland Average")) |>
    pull(number)

  selected_areas_data <- data |>
    filter(!(area_name %in% c("England Average", "Scotland Average")))

  color_palette <- c("#0d0887", "#febd2a", "#b83289", "#f48849", "#f0f921", "#8b0aa5", "#db5c68", "#5302a3", "#000004")

  sorted_areas <- sort(selected_areas)

  area_colors <- setNames(
    color_palette[1:length(sorted_areas)],
    sorted_areas
  )

  plot_data <- selected_areas_data |>
    mutate(
      national_avg = national_average,
      area_label = label,
      diff_from_avg = number - national_avg
    ) |>
    mutate(area_name = factor(area_name, levels = area_name[order(diff_from_avg)]))

  # Create the plot
  plot <-
    plot_data |>
    ggplot() +
    aes(
      y = area_name,
      fill = area_name
    ) +
    geom_point(aes(x = number), size = 3) +
    geom_point(aes(x = national_avg), size = 3, shape = 21, fill = "grey", color = "black") +
    geom_segment(aes(x = national_avg, xend = number, y = area_name, yend = area_name), color = "black") +
    geom_text(aes(x = 95, label = area_name), color = "black", size = 3, nudge_y = 0.3) +
    xlim(70, 130) +
    scale_fill_manual(
      values = area_colors
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

  number_areas <- length(plot_data$area_name)

  ggplotly_default_bar(plot, number_areas)
}


# Testing
# bar_plot_selected(england_icb_hi_social_determinants, c("Lincolnshire", "Norfolk and Waveney"))
