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

lgd <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^N"))

# ---- Provision of unpaid care ----
# Higher = worse performance
count_keyword <- "count"
hours_count_keyword <- "_0_hours"
count_rows <- ni_unpaid_care_21[grepl(
  paste0(
    count_keyword, ".*",
    hours_count_keyword
  ),
  ni_unpaid_care_21$variable
), ]

percent_keyword <- "percent"
hours_percent_keyword <- "_0_hours"
percent_rows <- ni_unpaid_care_21[grepl(
  paste0(
    percent_keyword, ".*",
    hours_percent_keyword
  ),
  ni_unpaid_care_21$variable
), ]

count_unpaid_care <- count_rows |>
  group_by(ltla21_name) |>
  summarise(
    number = sum(value)
  ) |>
  mutate(
    variable = "Hours of unpaid \ncare (2021)",
    .after = ltla21_name
  )

percent_unpaid_care <- percent_rows |>
  group_by(ltla21_name) |>
  summarise(
    percent = sum(value)
  ) |>
  mutate(
    variable = "Hours of unpaid \ncare (2021)",
    .after = ltla21_name
  )

unpaid_care <- left_join(count_unpaid_care, percent_unpaid_care,
  by = c("ltla21_name", "variable")
)


# ---- Combine & rename (pretty printing) ----
metrics_joined <-
  left_join(unpaid_care, lgd) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

secondary_care_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(scaled_1_1 = scale_1_1(percent)) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better performance
secondary_care_polarised <- secondary_care_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Hours of unpaid \ncare (2021)" ~ scaled_1_1 * -1,
      TRUE ~ scaled_1_1
    )
  )

# Check distributions
secondary_care_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +
  # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") +
  # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

# ---- Add plot labels ----
northern_ireland_lgd_secondary_care <- secondary_care_polarised |>
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

usethis::use_data(northern_ireland_lgd_secondary_care, overwrite = TRUE)
