# ---- Prepare selected data ----
jitter_plot_prep <- function(data, selected_areas) {
  data |>
    mutate(
      selected = if_else(
        area_name %in% selected_areas,
        area_name,
        "not selected"
      )
    ) |>
    mutate(alpha = if_else(selected != "not selected", 1, 0.1)) |>
    mutate(selected = factor(selected)) |>
    mutate(selected = relevel(selected, ref = "not selected"))
}

# ---- ggplotly fun ----
ggplotly_default <- function(plot, annotation_y) {
  ggplotly(
    plot,
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
      y = annotation_y,
      text = "Mean",
      showarrow = F
    )
}

# ---- Plot while waiting for selection ----
jitter_plot_null <- function(data) {
  annotation_y <- length(unique(data$variable)) + 0.6

  plot <- ggplot(
    data,
    aes(
      x = scaled_1_1,
      y = variable,
      text = case_when(
        geography_type == "LTLA" & data_type == "Summary metrics" & variable == "Index of Multiple \nDeprivation rank" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "IMD rank: ", round(number)
        ),
        geography_type == "LTLA" & data_type == "Summary metrics" & variable == "Left-behind areas" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of left-behind wards in the area: ", round(number),
          "<br>", "Percentage of all wards that are left-behind: ", round(percent * 100, 1), "%"
        ),
        geography_type == "LTLA" & data_type == "Summary metrics" & variable == "ONS Health \nIndex rank" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "Health Index rank: ", round(number)
        ),
        geography_type == "LTLA" & data_type == "Secondary care" & variable == "Bed availability \n(Jan 23 - Mar 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of available beds: ", round(number),
          "<br>", "Percentage of all beds available: ", round(percent * 100, 1), "%"
        ),
        geography_type == "LTLA" & data_type == "Secondary care" & variable == "Beds not meeting \ncriteria to reside \n(Jan 23 - Mar 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of beds not meeting criteria to reside: ", round(number),
          "<br>", "Percentage of all beds not meeting criteria to reside: ", round(percent * 100, 1), "%"
        ),
        geography_type == "LTLA" & data_type == "Secondary care" & variable == "Discharged beds \n(Jan 23 - Mar 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of discharged beds: ", round(number),
          "<br>", "Percentage of all beds discharged: ", round(percent * 100, 1), "%"
        ),
        geography_type == "LTLA" & data_type == "Secondary care" & variable == "Talking therapies: \nfinished a course of \ntreatment in 18 weeks \n(Nov 22 - Jan 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "Percentage that finished treatment: ", round(percent * 100, 1), "%"
        ),
        geography_type == "LTLA" & data_type == "Demographics" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "Count: ", round(number),
          "<br>", "Percent ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Summary metrics" & variable == "Deprivation" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "The no. of LSOAs in the ICB that are in the top 10% most deprived nationally: ", round(number),
          "<br>", "Percentage of LSOAs in the ICB that are in the top 10% most deprived nationally: ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Summary metrics" & variable == "Left-behind areas" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of left-behind LSOAs in the ICB: ", round(number),
          "<br>", "Percentage of LSOAs in ICB that are left-behind: ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Summary metrics" & variable == "ONS Health \nIndex rank" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "Health Index rank: ", round(number)
        ),
        geography_type == "ICB" & data_type == "Secondary care" & variable == "Bed availability \n(Jan 23 - Mar 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of available beds: ", round(number),
          "<br>", "Percentage of all beds available: ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Secondary care" & variable == "Beds not meeting \ncriteria to reside \n(Jan 23 - Mar 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of beds not meeting criteria to reside: ", round(number),
          "<br>", "Percentage of all beds not meeting criteria to reside: ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Secondary care" & variable == "Discharged beds \n(Jan 23 - Mar 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of discharged beds: ", round(number),
          "<br>", "Percentage of all beds discharged: ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Secondary care" & variable == "Talking therapies: \nfinished a course of \ntreatment in 18 weeks \n(Nov 22 - Jan 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "Percentage that finished treatment: ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Demographics" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "Count: ", round(number),
          "<br>", "Percent ", round(percent * 100, 1), "%"
        )
      )
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
      yend = annotation_y - 0.1,
      colour = "#262626",
      linetype = "dashed",
      alpha = .5,
      size = .5
    ) +
    theme_minimal() +
    labs(x = NULL, y = NULL) +
    theme(text = element_text(size = 12))

  # Set plot annotations to higher/lower if the data is demographics, else set
  # to better/worse
  if (unique(data$data_type) == "Demographics") {
    ggplotly_default(plot, annotation_y) |>
      add_annotations(
        x = -0.75,
        y = annotation_y,
        text = "◄ Lower than mean",
        showarrow = F
      ) |>
      add_annotations(
        x = 0.75,
        y = annotation_y,
        text = "Higher than mean ►",
        showarrow = F
      )
  } else {
    ggplotly_default(plot, annotation_y) |>
      add_annotations(
        x = -0.75,
        y = annotation_y,
        text = "◄ Worse than mean",
        showarrow = F
      ) |>
      add_annotations(
        x = 0.75,
        y = annotation_y,
        text = "Better than mean ►",
        showarrow = F
      )
  }
}

# ---- Plot selected areas ----
jitter_plot_selected <- function(data, selected_areas) {
  annotation_y <- length(unique(data$variable)) + 0.6

  plot <- ggplot(
    data,
    aes(
      x = scaled_1_1,
      y = variable,
      fill = selected,
      text = case_when(
        geography_type == "LTLA" & data_type == "Summary metrics" & variable == "Index of Multiple \nDeprivation rank" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "IMD rank: ", round(number)
        ),
        geography_type == "LTLA" & data_type == "Summary metrics" & variable == "Left-behind areas" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of left-behind wards in the area: ", round(number),
          "<br>", "Percentage of all wards that are left-behind: ", round(percent * 100, 1), "%"
        ),
        geography_type == "LTLA" & data_type == "Summary metrics" & variable == "ONS Health \nIndex rank" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "Health Index rank: ", round(number)
        ),
        geography_type == "LTLA" & data_type == "Secondary care" & variable == "Bed availability \n(Jan 23 - Mar 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of available beds: ", round(number),
          "<br>", "Percentage of all beds available: ", round(percent * 100, 1), "%"
        ),
        geography_type == "LTLA" & data_type == "Secondary care" & variable == "Beds not meeting \ncriteria to reside \n(Jan 23 - Mar 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of beds not meeting criteria to reside: ", round(number),
          "<br>", "Percentage of all beds not meeting criteria to reside: ", round(percent * 100, 1), "%"
        ),
        geography_type == "LTLA" & data_type == "Secondary care" & variable == "Discharged beds \n(Jan 23 - Mar 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of discharged beds: ", round(number),
          "<br>", "Percentage of all beds discharged: ", round(percent * 100, 1), "%"
        ),
        geography_type == "LTLA" & data_type == "Secondary care" & variable == "Talking therapies: \nfinished a course of \ntreatment in 18 weeks \n(Nov 22 - Jan 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "Percentage that finished treatment: ", round(percent * 100, 1), "%"
        ),
        geography_type == "LTLA" & data_type == "Demographics" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "Count: ", round(number),
          "<br>", "Percent ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Summary metrics" & variable == "Deprivation" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "The no. of LSOAs in the ICB that are in the top 10% most deprived nationally: ", round(number),
          "<br>", "Percentage of LSOAs in the ICB that are in the top 10% most deprived nationally: ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Summary metrics" & variable == "Left-behind areas" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of left-behind LSOAs in the ICB: ", round(number),
          "<br>", "Percentage of LSOAs in ICB that are left-behind: ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Summary metrics" & variable == "ONS Health \nIndex rank" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "Health Index rank: ", round(number)
        ),
        geography_type == "ICB" & data_type == "Secondary care" & variable == "Bed availability \n(Jan 23 - Mar 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of available beds: ", round(number),
          "<br>", "Percentage of all beds available: ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Secondary care" & variable == "Beds not meeting \ncriteria to reside \n(Jan 23 - Mar 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of beds not meeting criteria to reside: ", round(number),
          "<br>", "Percentage of all beds not meeting criteria to reside: ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Secondary care" & variable == "Discharged beds \n(Jan 23 - Mar 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of discharged beds: ", round(number),
          "<br>", "Percentage of all beds discharged: ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Secondary care" & variable == "Talking therapies: \nfinished a course of \ntreatment in 18 weeks \n(Nov 22 - Jan 23 average)" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "Percentage that finished treatment: ", round(percent * 100, 1), "%"
        ),
        geography_type == "ICB" & data_type == "Demographics" ~ paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "Count: ", round(number),
          "<br>", "Percent ", round(percent * 100, 1), "%"
        )
      )
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
      yend = annotation_y - 0.1,
      colour = "#262626",
      linetype = "dashed",
      alpha = .5,
      size = .5
    ) +
    theme_minimal() +
    scale_fill_manual(
      # https://waldyrious.net/viridis-palette-generator/
      # No. of colours = maximum number of area selections + 1
      values = c("#0d0887", "#f89540", "#cc4778", "#f0f921", "#7e03a8", "#000004"),
      breaks = selected_areas
    ) +
    labs(x = NULL, y = NULL) +
    theme(text = element_text(size = 12))

  # Set plot annotations to higher/lower if the data is demographics, else set
  # to better/worse
  if (unique(data$data_type) == "Demographics") {
    ggplotly_default(plot, annotation_y) |>
      add_annotations(
        x = -0.75,
        y = annotation_y,
        text = "◄ Lower than mean",
        showarrow = F
      ) |>
      add_annotations(
        x = 0.75,
        y = annotation_y,
        text = "Higher than mean ►",
        showarrow = F
      )
  } else {
    ggplotly_default(plot, annotation_y) |>
      add_annotations(
        x = -0.75,
        y = annotation_y,
        text = "◄ worse than mean",
        showarrow = F
      ) |>
      add_annotations(
        x = 0.75,
        y = annotation_y,
        text = "Better than mean ►",
        showarrow = F
      )
  }
}
