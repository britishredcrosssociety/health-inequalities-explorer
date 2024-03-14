# ---- Prepare selected data ----
bar_plot_prep <- function(data, selected_areas) {
  data$area_name <- string_wrap(data$area_name)

  data$selected <- ifelse(data$area_name %in% string_wrap(selected_areas), data$area_name, "not selected")

  data$alpha <- ifelse(data$selected != "not selected", 1, 0.1)

  # Make 'alpha' a one-level factor
  data$alpha <- I(data$alpha)

  data$selected <- factor(data$selected)

  if ("not selected" %in% levels(data$selected)) {
    data$selected <- relevel(data$selected, ref = "not selected")
  }

  return(data)
}

# ---- ggplotly fun ----
ggplotly_default_bar <- function(plot, data) {
  # Set Height of plot to be a factor of the number of areas selected
  number_areas <- length(data$area_name)
  pixel <- ifelse(number_areas == 1, 200, 60)

  ggplotly(
    plot,
    height = number_areas * pixel,
    width = 300,
    tooltip = c("text")
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
# To plot national mean as default
bar_plot_null <- function(data) {
  plot <-
    data |>
    filter(area_name %in% c("National Mean")) |>
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

  ggplotly_default_bar(plot, data = data |>
    filter(area_name %in% c("National Mean")))
}

# Example usage:
# bar_plot_null(england_ltla_hi_social_determinants)


# ---- Plot selected areas ----
bar_plot_selected <- function(data, selected_areas) {
  plot <-
    data |>
    filter(area_name %in% c(selected_areas, "National Mean")) |>
    ggplot() +
    aes(
      x = number,
      y = area_name,
      text = label,
      fill = area_name
    ) +
    geom_point(size = 3) +
    geom_segment(aes(x = 70, xend = number - 1, yend = area_name), color = "black") +
    geom_text(aes(x = 85, y = area_name, label = area_name), color = "black", size = 3, nudge_y = 0.3) +
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

  ggplotly_default_bar(plot, data = data |>
    filter(area_name %in% c(selected_areas, "National Mean")))
}

#bar_plot_selected(england_ltla_hi_outcomes, c("York", "Newham"))
