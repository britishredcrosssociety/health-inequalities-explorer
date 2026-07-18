library(tidyverse)
library(geographr)
library(healthyr)
library(compositr)
library(sf)
library(readxl)
library(ggridges)
library(demographr)

ltla <-
  boundaries_ltla24 |>
  st_drop_geometry() |>
  filter(str_detect(ltla24_code, "^E"))

lookup_england_ltla <-
  lookup_ltla_ltla |>
  filter(str_detect(ltla21_code, "^E"))

lookup_ltla21_ltla24 <-
  lookup_england_ltla |>
  distinct(ltla21_code, ltla24_code)

# ---- ONS Health Index score ----
# Higher score = better health
# Higher rank (calculated here) = better health
health_index_2021 <- england_health_index |>
  filter(year == "2021") |>
  select(ltla21_code, health_index_score = overall_score)

# Data is missing for two ltla's
#   - Map Iscles of Scilly to Cornwall
#   - Map City of London to Hackney
cornwall_score <-
  health_index_2021 |>
  filter(ltla21_code == "E06000052") |>
  pull(health_index_score)

hackney_score <-
  health_index_2021 |>
  filter(ltla21_code == "E09000012") |>
  pull(health_index_score)

health_index_missing_added <-
  health_index_2021 |>
  add_row(ltla21_code = "E06000053", health_index_score = cornwall_score) |>
  add_row(ltla21_code = "E09000001", health_index_score = hackney_score)

# health_index <-
#   health_index_missing_added |>
#   mutate(number = rank(health_index_score)) |>
#   mutate(percent = NA) |>
#   mutate(variable = "ONS Health \nIndex rank", .after = ltla21_code)
#select(-health_index_score)

# Recast to LTLA 2024 codes using mean scores where there are more than one 2021 LA for one 2024 LA
health_index <-
  health_index_missing_added |>
  left_join(lookup_ltla21_ltla24) |>
  group_by(ltla24_code) |>
  summarise(number = mean(health_index_score)) |>
  ungroup() |>
  mutate(percent = NA) |>
  mutate(variable = "ONS Health \nIndex rank", .after = ltla24_code)

# ---- Rename (pretty printing) ----
metrics_joined <-
  health_index |>
  left_join(ltla) |>
  select(-ltla24_code) |>
  rename(area_name = ltla24_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

ltla_health_index_england_scaled <-
  metrics_joined |>
  mutate(
    scaled_1_1 = scale_1_1(number)
  )

# Check distributions
ltla_health_index_england_scaled |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
england_ltla_health_index <- ltla_health_index_england_scaled |>
  mutate(
    label = paste0(
      "<b>",
      area_name,
      "</b>",
      "<br>",
      "<br>",
      "Health Index score: ",
      round(number)
    )
  )

usethis::use_data(england_ltla_health_index, overwrite = TRUE)
