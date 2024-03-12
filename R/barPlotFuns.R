# ---- Plot while waiting for selection ----
bar_plot_null <- function(data) {

  plot <- ggplot(
    data,
    aes(
      x = scaled_1_1,
      y = variable,
      text = label
    )
  ) +
    geom_point(
      position = position_jitter(height = 0.25, width = 0.1, seed = 123),
      size = 4,
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
      yend = indicator_count + 0.5,
      colour = "#262626",
      linetype = "dashed",
      alpha = .5,
      size = .5
    ) +
    theme_minimal() +
    labs(x = NULL, y = NULL) +
    theme(text = element_text(size = 12))}


# ggplot(england_ltla_hi_outcomes, aes(x = number, y = area_name)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   labs(x = "Value", y = "Category") +
#   theme_minimal()
