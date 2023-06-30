# ---- Prepare selected data ----
jitter_plot_prep <- function(data, selected_areas) {
  data |>
    mutate(area_name = string_wrap(area_name)) |>
    mutate(
      selected = if_else(
        area_name %in% string_wrap(selected_areas),
        area_name,
        "not selected"
      )
    ) |>
    mutate(alpha = if_else(selected != "not selected", 1, 0.1)) |>
    mutate(selected = factor(selected)) |>
    mutate(selected = relevel(selected, ref = "not selected"))
}

# ---- ggplotly fun ----
ggplotly_default <- function(plot, indicator_count) {
  ggplotly(
    plot,
    height = indicator_count * 130,
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
    ) |>
    layout(
      margin = list(t = 50),
      xaxis = list(
        title = list(
          text = "Normalised units \n(see help button for more details)",
          font = list(
            color = "#717171",
            size = 14
          )
        ),
        range = list(-1.15, 1.15)
      ),
      legend = list(
        orientation = "v",
        title = NA
        # x = 0,
        # y = 1.4
      )
    ) |>
    add_annotations(
      x = 0,
      y = indicator_count + 0.6,
      text = "Mean",
      showarrow = F
    )
}

# ---- Plot while waiting for selection ----
jitter_plot_null <- function(data) {
  indicator_count <- length(unique(data$variable))

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
    theme(text = element_text(size = 12))

  # Group demographics by type (age and gender vs. ethnicity)
  if (
    any(grepl("Younger", unique(data$variable), fixed = TRUE)) &&
      as.character(length(unique(data$variable))) == 8
  ) {
    plot <- plot +
      scale_y_discrete(
        limits = c(
          "White",
          "Other ethnic \ngroup",
          "Mixed or Multiple \nethnic group",
          "Black, Black British, \nBlack Welsh, \nCaribbean or African",
          "Asian, Asian \nBritish or \nAsian Welsh",
          "Older \npeople (65+)",
          "Working \nage (18-65)",
          "Younger \npeople (< 18)"
        )
      ) +
      annotate(geom = "text", x = 0.94, y = 8.4, label = "Age", colour = "#717171") +
      geom_hline(yintercept = 5.5, size = 0.1, linetype = "dotted") +
      annotate(geom = "text", x = 0.99, y = 5.4, label = "Ethnicity", colour = "#717171")
  } else if (
    any(grepl("Younger", unique(data$variable), fixed = TRUE)) &&
      as.character(length(unique(data$variable))) == 11
  ) {
    plot <- plot +
      scale_y_discrete(
        limits = c(
          "White",
          "Other ethnic \ngroup",
          "Mixed or Multiple \nethnic groups",
          "Black, Black British, \nBlack Welsh, \nCaribbean or African",
          "Asian, Asian \nBritish or \nAsian Welsh",
          "Older \nmales (65+)",
          "Working age \nmales (18-65)",
          "Younger \nmales (< 18)",
          "Older \nfemales (65+)",
          "Working age \nfemales (18-65)",
          "Younger \nfemales (< 18)"
        )
      ) +
      annotate(geom = "text", x = 0.94, y = 11.4, label = "Age & Gender", colour = "#717171") +
      geom_hline(yintercept = 5.5, size = 0.1, linetype = "dotted") +
      annotate(geom = "text", x = 0.99, y = 5.4, label = "Ethnicity", colour = "#717171")
  } else if (
    any(grepl("Younger", unique(data$variable), fixed = TRUE)) &&
      as.character(length(unique(data$variable))) == 6
  ) {
    plot <- plot +
      scale_y_discrete(
        limits = c(
          "Older \nmales (65+)",
          "Working age \nmales (18-65)",
          "Younger \nmales (< 18)",
          "Older \nfemales (65+)",
          "Working age \nfemales (18-65)",
          "Younger \nfemales (< 18)"
        )
      ) +
      annotate(geom = "text", x = 0.94, y = 6.4, label = "Age & Gender", colour = "#717171")
  }

  # Set plot annotations to higher/lower if the data is demographics, else set
  # to better/worse
  if (any(grepl("Younger", unique(data$variable), fixed = TRUE))) {
    ggplotly_default(plot, indicator_count) |>
      add_annotations(
        x = -0.75,
        y = indicator_count + 0.6,
        text = "◄ Lower than mean",
        showarrow = F
      ) |>
      add_annotations(
        x = 0.75,
        y = indicator_count + 0.6,
        text = "Higher than mean ►",
        showarrow = F
      )
  } else {
    ggplotly_default(plot, indicator_count) |>
      add_annotations(
        x = -0.75,
        y = indicator_count + 0.6,
        text = "◄ Worse than mean",
        showarrow = F
      ) |>
      add_annotations(
        x = 0.75,
        y = indicator_count + 0.6,
        text = "Better than mean ►",
        showarrow = F
      )
  }
}

# ---- Plot selected areas ----
jitter_plot_selected <- function(data, selected_areas) {
  indicator_count <- length(unique(data$variable))

  plot <- ggplot(
    data,
    aes(
      x = scaled_1_1,
      y = variable,
      fill = selected,
      text = label
    )
  ) +
    geom_point(
      aes(alpha = alpha),
      position = position_jitter(height = 0.25, width = 0.1, seed = 123),
      size = 4,
      shape = 21,
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
    scale_fill_manual(
      # https://waldyrious.net/viridis-palette-generator/
      # No. of colours = maximum number of area selections + 1
      values = c("#0d0887", "#febd2a", "#b83289", "#f48849", "#f0f921", "#8b0aa5", "#db5c68", "#5302a3", "#000004"),
      breaks = string_wrap(selected_areas)
    ) +
    labs(x = NULL, y = NULL) +
    theme(text = element_text(size = 12))

  # Group demographics by type (age and gender vs. ethnicity)
  if (
    any(grepl("Younger", unique(data$variable), fixed = TRUE)) &&
      as.character(length(unique(data$variable))) == 8
  ) {
    plot <- plot +
      scale_y_discrete(
        limits = c(
          "White",
          "Other ethnic \ngroup",
          "Mixed or Multiple \nethnic group",
          "Black, Black British, \nBlack Welsh, \nCaribbean or African",
          "Asian, Asian \nBritish or \nAsian Welsh",
          "Older \npeople (65+)",
          "Working \nage (18-65)",
          "Younger \npeople (< 18)"
        )
      ) +
      annotate(geom = "text", x = 0.94, y = 8.4, label = "Age", colour = "#717171") +
      geom_hline(yintercept = 5.5, size = 0.1, linetype = "dotted") +
      annotate(geom = "text", x = 0.99, y = 5.4, label = "Ethnicity", colour = "#717171")
  } else if (
    any(grepl("Younger", unique(data$variable), fixed = TRUE)) &&
      as.character(length(unique(data$variable))) == 11
  ) {
    plot <- plot +
      scale_y_discrete(
        limits = c(
          "White",
          "Other ethnic \ngroup",
          "Mixed or Multiple \nethnic groups",
          "Black, Black British, \nBlack Welsh, \nCaribbean or African",
          "Asian, Asian \nBritish or \nAsian Welsh",
          "Older \nmales (65+)",
          "Working age \nmales (18-65)",
          "Younger \nmales (< 18)",
          "Older \nfemales (65+)",
          "Working age \nfemales (18-65)",
          "Younger \nfemales (< 18)"
        )
      ) +
      annotate(geom = "text", x = 0.94, y = 11.4, label = "Age & Gender", colour = "#717171") +
      geom_hline(yintercept = 5.5, size = 0.1, linetype = "dotted") +
      annotate(geom = "text", x = 0.99, y = 5.4, label = "Ethnicity", colour = "#717171")
  } else if (
    any(grepl("Younger", unique(data$variable), fixed = TRUE)) &&
      as.character(length(unique(data$variable))) == 6
  ) {
    plot <- plot +
      scale_y_discrete(
        limits = c(
          "Older \nmales (65+)",
          "Working age \nmales (18-65)",
          "Younger \nmales (< 18)",
          "Older \nfemales (65+)",
          "Working age \nfemales (18-65)",
          "Younger \nfemales (< 18)"
        )
      ) +
      annotate(geom = "text", x = 0.94, y = 6.4, label = "Age & Gender", colour = "#717171")
  }

  # Set plot annotations to higher/lower if the data is demographics, else set
  # to better/worse
  if (any(grepl("Younger", unique(data$variable), fixed = TRUE))) {
    ggplotly_default(plot, indicator_count) |>
      add_annotations(
        x = -0.75,
        y = indicator_count + 0.6,
        text = "◄ Lower than mean",
        showarrow = F
      ) |>
      add_annotations(
        x = 0.75,
        y = indicator_count + 0.6,
        text = "Higher than mean ►",
        showarrow = F
      )
  } else {
    ggplotly_default(plot, indicator_count) |>
      add_annotations(
        x = -0.75,
        y = indicator_count + 0.6,
        text = "◄ worse than mean",
        showarrow = F
      ) |>
      add_annotations(
        x = 0.75,
        y = indicator_count + 0.6,
        text = "Better than mean ►",
        showarrow = F
      )
  }
}
