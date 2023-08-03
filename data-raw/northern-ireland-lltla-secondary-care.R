# # Northern Ireland - Secondary Care Indicators
# Local Government Districts

# ---- Load libs & helpers ----
library(tidyverse)
library(geographr)
library(sf)
library(healthyr)
library(demographr)
library(compositr)
library(ggridges)
library(devtools)
library(readODS)

ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^N"))

# ---- Provision of unpaid care ----
unpaid_care <- ni_unpaid_care_21 |>
  filter(str_detect(variable, "_0_hours$")) |>
  mutate(variable = str_extract(variable, "^[^_]+(?=_)")) |>
  summarise(value = sum(value), .by = c(ltla21_name, variable)) |>
  pivot_wider(names_from = "variable", values_from = value) |>
  rename(area_name = ltla21_name, number = count) |>
  mutate(variable = "Hours of unpaid \ncare (2021)", .after = area_name)

# ---- Combine & rename (pretty printing) ----
# Leaving this template code as it allows for easy future additions
metrics_joined <- unpaid_care

# ---- Normalise/scale ----
secondary_care_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(scaled = positional_normalisation(percent)) |>
  ungroup()

# ---- Align indicator polarity ----
# Leaving this template code as it allows for easy future additions
# Align so higher value = better performance
# Flip unpaid care
secondary_care_polarised <- secondary_care_scaled |>
  mutate(
    scaled = case_when(
      variable == "Hours of unpaid \ncare (2021)" ~ scaled * -1,
      TRUE ~ scaled
    )
  )

# Check distributions
secondary_care_polarised |>
  ggplot(aes(x = scaled, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +
  # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") +
  # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

# ---- Add plot labels ----
northern_ireland_ltla_secondary_care <- secondary_care_polarised |>
  mutate(
    label = case_when(
      variable == "Hours of unpaid \ncare (2021)" ~
        paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. of persons with no unpaid care: ", round(number),
          "<br>", "Percentage of persons with no unpaid care: ", round(percent * 100, 1),
          "%"
        )
    )
  )

usethis::use_data(northern_ireland_ltla_secondary_care, overwrite = TRUE)
