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
ggplotly_default_bar <- function(plot) {
  #indicator_count <- length(unique(data$variable))

  ggplotly(
    plot,
    #height = indicator_count * 160,
    #width = 1000,
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

ggplotly_default_bar(plot = plot)
ggplotly(plot)
plot
# ---- Plot while waiting for selection ----
bar_plot_null <- function(data) {
  plot <-
    data |>
      filter(area_name %in% c("National Mean")) |>
    ggplot(
      aes(
        x = number,
        y = area_name,
        text = label)
      ) +
    geom_bar(
      stat = "identity",
      fill = "#717171",
      colour = "#262626"
      ) +
    labs(title = paste0(unique(data$variable)),
         x = paste0("Score (Max =", round(max(data$number)), "; Min =",
                    round(min(data$number)), ")"), y = NULL) +
    theme_minimal()+
    theme(text = element_text(size = 12))
  }

# Test
plot <- bar_plot_null(england_ltla_hi_risk_factors)
plot

# ---- Plot selected areas ----
bar_plot_selected <- function(data, selected_areas) {
  plot <-
    data |>
      filter(area_name %in% c(selected_areas, "National Mean")) |>
    ggplot() +
    aes(
      x = number,
      y = area_name,
      text = label

  ) +
    geom_bar(
      stat = "identity"
    ) +
    theme_minimal() +
    scale_fill_manual(
      # https://waldyrious.net/viridis-palette-generator/
      # No. of colours = maximum number of area selections + 1
      values = c("#0d0887", "#febd2a", "#b83289", "#f48849", "#f0f921", "#8b0aa5", "#db5c68", "#5302a3", "#000004"),
    ) +
    labs(title = paste0(unique(data$variable)),
         x = paste0("Score (Max =", round(max(data$number)), "; Min =",
                    round(min(data$number)), ")"), y = NULL) +
    theme_minimal()+
    theme(text = element_text(size = 12))
  plot
}

bar_plot_selected(england_ltla_hi_outcomes, c("York", "Newham"))

