# ---- Prepare selected data ----
table_selected <- function(data, selected_areas) {
  data <- data |>
    filter(region %in% selected_areas)

  data |>
    gt() |>
    tab_header(
      title = md("### Subdomain indicator scores"),
      subtitle = md("Red = worse | Green = better")
    ) |>
    cols_label(region = "") |>
    tab_options(
      table.font.size = px(12),
      column_labels.font.weight = "bold"
    ) |>
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body(columns = !region)
    ) |>
    tab_spanner_delim(delim = "--") |>
    data_color(
      columns = !region,
      palette = c("#e41a1c", "#ffffbf", "#4daf4a")
    ) |>
    fmt_number(
      !region,
      decimals = 1
    ) |>
    tab_style(
      style = cell_borders(sides = "all", style = "solid", color = "#e9e9e9"),
      locations = cells_body(
        columns = !region
      )
    ) |>
    tab_footnote(
      "Cells are coloured by column. This means you should compare colours across places for a single indicator, not across indicators for a single place."
    )
}

# ---- Blank table ----
table_null <- function() {
  formattable(data.frame(`Subdomain` = "No data available"))
}
