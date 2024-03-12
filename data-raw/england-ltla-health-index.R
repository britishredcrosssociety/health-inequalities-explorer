library(tidyverse)
library(geographr)
library(healthyr)
library(compositr)
library(sf)
library(readxl)
library(ggridges)
library(demographr)

ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^E"))

lookup_england_ltla <-
  lookup_ltla_ltla |>
  filter(str_detect(ltla21_code, "^E"))

lookup_england_lsoa_ltla <-
  lookup_lsoa11_lsoa21_ltla22 |>
  distinct(lsoa11_code, lsoa21_code, ltla22_code) |>
  filter(str_detect(ltla22_code, "^E"))

population_lsoa <-
  population20_lsoa11 |>
  select(lsoa11_code, total_population) |>
  filter(str_detect(lsoa11_code, "^E"))

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

health_index <-
  health_index_missing_added |>
  mutate(number = rank(health_index_score)) |>
  mutate(percent = NA) |>
  mutate(variable = "ONS Health \nIndex rank", .after = ltla21_code) |>
  select(-health_index_score)


# ---- Rename (pretty printing) ----
metrics_joined <-
  health_index |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
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
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Health Index rank: ", round(number)
    )
  )

usethis::use_data(england_ltla_health_index, overwrite = TRUE)
