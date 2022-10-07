jitter_plot_prep <- function(data, selected_areas) {
  data |>
    dplyr::mutate(
      selected = dplyr::if_else(
        area_name %in% selected_areas,
        area_name,
        "not selected"
      )
    ) |>
    dplyr::mutate(
      alpha = dplyr::if_else(selected != "not selected", 1, 0.1)
    ) |>
    dplyr::mutate(selected = factor(selected)) |>
    dplyr::mutate(selected = relevel(selected, ref = "not selected"))
}

jitter_plot_age_null <- function(data, x, y) {
  plot_object <- data |>
    ggplot(aes(x = {{ x }}, y = {{ y }})) +
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

  return(plot_object)
}

jitter_plot_summary_null <- function(data, x, y) {
  plot_object <- data |>
    ggplot(aes(x = {{ x }}, y = {{ y }})) +
    geom_point(
      position = position_jitter(height = 0.25, width = 0.1, seed = 123),
      size = 5,
      shape = 21,
      alpha = 0.1,
      fill = "#717171",
      colour = "#262626",
    ) +
    geom_vline(
      xintercept = 0,
      size = 1,
      alpha = .7,
      colour = "#262626",
      linetype = "dashed"
    ) +
    annotate(
      "text",
      x = 0,
      y = 3.75,
      label = "bold(Mean)",
      parse = TRUE
    ) +
    theme_minimal() +
    labs(x = NULL, y = NULL) +
    theme(text = element_text(face = "bold", size = 15))

  return(plot_object)
}

jitter_plot_age_selected <- function(data, x, y, fill, selected_areas) {
  plot_object <- data |>
    ggplot(aes(x = {{ x }}, y = {{ y }}, fill = {{ fill }})) +
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
      breaks = selected_areas
    ) +
    scale_alpha(guide = "none") +
    labs(x = NULL, y = NULL) +
    theme(text = element_text(face = "bold", size = 15))

  return(plot_object)
}

jitter_plot_summary_selected <- function(data, x, y, fill, selected_areas) {
  plot_object <- data |>
    ggplot(aes(x = {{ x }}, y = {{ y }}, fill = {{ fill }})) +
    geom_point(
      aes(alpha = alpha),
      position = position_jitter(height = 0.25, width = 0.1, seed = 123),
      size = 5,
      shape = 21,
      colour = "#262626"
    ) +
    geom_vline(
      xintercept = 0,
      size = 1,
      alpha = .7,
      colour = "#262626",
      linetype = "dashed"
    ) +
    annotate(
      "text",
      x = 0,
      y = 3.75,
      label = "bold(Mean)",
      parse = TRUE
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    ) +
    scale_fill_manual(
      values = c("#D0021B", "#40A22A", "#F1B13B", "#6A9EAA"),
      breaks = selected_areas
    ) +
    scale_alpha(guide = "none") +
    labs(x = NULL, y = NULL) +
    theme(text = element_text(face = "bold", size = 15))

  return(plot_object)
}
