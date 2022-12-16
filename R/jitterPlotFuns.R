# ---- Prepare selected data ----
jitter_plot_prep <- function(data, selected_areas) {
  data |>
    dplyr::mutate(
      selected = dplyr::if_else(
        area_name %in% selected_areas,
        area_name,
        "not selected"
      )
    ) |>
    dplyr::mutate(alpha = dplyr::if_else(selected != "not selected", 1, 0.1)) |>
    dplyr::mutate(selected = factor(selected)) |>
    dplyr::mutate(selected = relevel(selected, ref = "not selected"))
}

# ---- ggplotly fun ----
ggplotly_default <- function(plot, annotation_y) {
  ggplotly(
    plot,
    tooltip = c("text")
  ) |>
    config(displayModeBar = FALSE) |>
    layout(
      xaxis = list(range = list(-1.15, 1.15)),
      legend = list(
        orientation = "h",
        title = NA,
        x = 0,
        y = 1.1
      )
    ) |>
    add_annotations(
      x = 0,
      y = annotation_y,
      text = "Mean",
      showarrow = F
    ) |>
    add_annotations(
      x = -0.75,
      y = annotation_y,
      text = "◄ Lower",
      showarrow = F
    ) |>
    add_annotations(
      x = 0.75,
      y = annotation_y,
      text = "Higher ►",
      showarrow = F
    )
}

# ---- Plot while waiting for selection ----
jitter_plot_null <- function(data) {
  plot <- ggplot(
    data,
    aes(
      x = scaled_1_1,
      y = variable,
      text = paste0(
        "<b>", area_name, "</b>",
        "<br>", "Number: ", round(number),
        "<br>", "Percent: ", round(percent * 100, 1)
      )
    )
  ) +
    geom_point(
      position = position_jitter(height = 0.25, width = 0.1, seed = 123),
      size = 5,
      shape = 21,
      alpha = 0.1,
      fill = "#717171",
      colour = "#262626"
    ) +
    annotate(
      "segment",
      x = 0,
      xend = 0,
      y = 0.5,
      yend = 3.5,
      colour = "#262626",
      linetype = "dashed",
      alpha = .5,
      size = .5
    ) +
    theme_minimal() +
    labs(x = NULL, y = NULL) +
    theme(text = element_text(size = 12))

  annotation_y <- length(unique(data$variable)) + 0.6

  ggplotly_default(plot, annotation_y)
}

# ---- Plot selected areas ----
jitter_plot_selected <- function(data, selected_areas) {
  plot <- ggplot(
    data,
    aes(
      x = scaled_1_1,
      y = variable,
      fill = selected,
      text = paste0(
        "<b>", area_name, "</b>",
        "<br>", "Number: ", round(number),
        "<br>", "Percent: ", round(percent * 100, 1)
      )
    )
  ) +
    geom_point(
      aes(alpha = alpha),
      position = position_jitter(height = 0.25, width = 0.1, seed = 123),
      size = 5,
      shape = 21,
      colour = "#262626"
    ) +
    annotate(
      "segment",
      x = 0,
      xend = 0,
      y = 0.5,
      yend = 3.5,
      colour = "#262626",
      linetype = "dashed",
      alpha = .5,
      size = .5
    ) +
    theme_minimal() +
    scale_fill_manual(
      values = c("#D0021B", "#40A22A", "#F1B13B", "#6A9EAA"),
      breaks = selected_areas
    ) +
    labs(x = NULL, y = NULL) +
    theme(text = element_text(size = 12))

  annotation_y <- length(unique(data$variable)) + 0.6

  ggplotly_default(plot, annotation_y)
}
