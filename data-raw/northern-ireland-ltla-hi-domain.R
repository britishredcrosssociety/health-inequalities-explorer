library(tidyverse)
library(geographr)
library(sf)
library(healthyr)
library(ggridges)
library(healthindexni)

# --- Lookups ----
ltla <-
  boundaries_ltla24 |>
  st_drop_geometry() |>
  filter(str_detect(ltla24_code, "^N"))

# --- Healthy People / Health Outcomes Domain ----
northern_ireland_ltla_hi_outcomes <- ni_health_index |>
  select(ltla24_code, number = healthy_people_score) |>
  mutate(variable = "Health Outcomes Score") |>
  relocate(variable, .after = ltla24_code) |>
  left_join(ltla) |>
  select(-ltla24_code) |>
  rename(area_name = ltla24_name) |>
  add_row(number = mean(ni_health_index$healthy_people_score), area_name = "Northern Ireland Average") |>
  mutate(number = number + 100) |>
  mutate(rank = round(rank(number))) |>
  relocate(area_name) |>
  mutate(label = paste0(
    "<b>", area_name, "</b>",
    "<br>",
    "<br>", "Health Outcomes Score: ", round(number),
    "<br>", "Rank: ", rank
  ), )

# --- Healthy Lives / Preventable Risk Factors ----
northern_ireland_ltla_hi_risk_factors <- ni_health_index |>
  select(ltla24_code, number = healthy_lives_score) |>
  mutate(variable = "Preventable Risk Factors Score") |>
  relocate(variable, .after = ltla24_code) |>
  left_join(ltla) |>
  select(-ltla24_code) |>
  rename(area_name = ltla24_name) |>
  add_row(number = mean(ni_health_index$healthy_lives_score), area_name = "Northern Ireland Average") |>
  mutate(number = number + 100) |>
  mutate(rank = round(rank(number))) |>
  relocate(area_name) |>
  mutate(label = paste0(
    "<b>", area_name, "</b>",
    "<br>",
    "<br>", "Preventable Risk Factors Score: ", round(number),
    "<br>", "Rank: ", rank
  ), )

# --- Healthy Places / Social Determinants of Health----
northern_ireland_ltla_hi_social_determinants <- ni_health_index |>
  select(ltla24_code, number = healthy_places_score) |>
  mutate(variable = "Social Determinants of Health Score") |>
  relocate(variable, .after = ltla24_code) |>
  left_join(ltla) |>
  select(-ltla24_code) |>
  rename(area_name = ltla24_name) |>
  add_row(number = mean(ni_health_index$healthy_places_score), area_name = "Northern Ireland Average") |>
  mutate(number = number + 100) |>
  mutate(rank = round(rank(number))) |>
  relocate(area_name) |>
  mutate(label = paste0(
    "<b>", area_name, "</b>",
    "<br>",
    "<br>", "Social Determinants of Health Score", round(number),
    "<br>", "Rank: ", rank
  ), )

# ---- Check distributions ----
check_distribution <- function(data) {
  data |>
    ggplot(aes(x = number, y = variable)) +
    geom_density_ridges(scale = 4) +
    scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
    scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
    coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
    theme_ridges()
}

check_distribution(northern_ireland_ltla_hi_outcomes)
check_distribution(northern_ireland_ltla_hi_risk_factors)
check_distribution(northern_ireland_ltla_hi_social_determinants)

# ---- Save datasets ----
usethis::use_data(northern_ireland_ltla_hi_outcomes, overwrite = TRUE)
usethis::use_data(northern_ireland_ltla_hi_risk_factors, overwrite = TRUE)
usethis::use_data(northern_ireland_ltla_hi_social_determinants, overwrite = TRUE)
