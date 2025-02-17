pkgload::load_all()
library(tidyverse)
library(gt)

outcomes <- england_ltla_hi_outcomes_sub |>
  pivot_longer(!variable, names_to = "region", values_to = "score") |>
  mutate(sub_domain = "Outcomes", .before = "variable")

risk_factors <- england_ltla_hi_risk_factors_sub |>
  pivot_longer(!variable, names_to = "region", values_to = "score") |>
  mutate(sub_domain = "Risk factors", .before = "variable")

social_determinants <- england_ltla_hi_social_determinants_sub |>
  pivot_longer(!variable, names_to = "region", values_to = "score") |>
  mutate(sub_domain = "Social determinants", .before = "variable")

sub_domains <- bind_rows(outcomes, risk_factors, social_determinants)

example_regions <- c(
  "Hartlepool",
  "Enfield",
  "Birmingham",
  "Wirral",
  "Westminster"
)

example_data <- sub_domains |>
  filter(region %in% example_regions)

example_data |>
  mutate(variable = paste(sub_domain, variable, sep = "--")) |>
  select(-sub_domain) |>
  pivot_wider(names_from = variable, values_from = score) |>
  gt() |>
  tab_header(
    title = md("## Example heat map"),
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
  tab_style(
    style = cell_borders(sides = "all", style = "solid", color = "#e9e9e9"),
    locations = cells_body(
      columns = !region
    )
  )
