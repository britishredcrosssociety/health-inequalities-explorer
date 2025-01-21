library(tidyverse)
library(geographr)
library(sf)
library(healthyr)
library(ggridges)
library(healthindexscotland)

# --- Lookups ----
ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^S"))

# --- Healthy People / Health Outcomes Domain ----
scotland_ltla_hi_outcomes <- scotland_health_index |>
  select(ltla21_code = ltla24_code, number = healthy_people_score) |>
  mutate(variable = "Health Outcomes Score") |>
  relocate(variable, .after = ltla21_code) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  add_row(number = mean(scotland_health_index$healthy_people_score), area_name = "England Average") |>
  mutate(rank = round(rank(number))) |>
  relocate(area_name) |>
  mutate(label = paste0(
    "<b>", area_name, "</b>",
    "<br>",
    "<br>", "Health Outcomes Score: ", round(number),
    "<br>", "Rank: ", rank
  ), )

# --- Healthy Lives / Preventable Risk Factors ----
scotland_ltla_hi_risk_factors <- scotland_health_index |>
  select(ltla21_code = ltla24_code, number = healthy_lives_score) |>
  mutate(variable = "Preventable Risk Factors Score") |>
  relocate(variable, .after = ltla21_code) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  add_row(number = mean(scotland_health_index$healthy_lives_score), area_name = "England Average") |>
  mutate(rank = round(rank(number))) |>
  relocate(area_name) |>
  mutate(label = paste0(
    "<b>", area_name, "</b>",
    "<br>",
    "<br>", "Preventable Risk Factors Score: ", round(number),
    "<br>", "Rank: ", rank
  ), )

# --- Healthy Places / Social Determinants of Health----
scotland_ltla_hi_social_determinants <- scotland_health_index |>
  select(ltla21_code = ltla24_code, number = healthy_places_score) |>
  mutate(variable = "Social Determinants of Health Score") |>
  relocate(variable, .after = ltla21_code) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  add_row(number = mean(scotland_health_index$healthy_places_score), area_name = "England Average") |>
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

check_distribution(scotland_ltla_hi_outcomes)
check_distribution(scotland_ltla_hi_risk_factors)
check_distribution(scotland_ltla_hi_social_determinants)

# ---- Save datasets ----
usethis::use_data(scotland_ltla_hi_outcomes, overwrite = TRUE)
usethis::use_data(scotland_ltla_hi_risk_factors, overwrite = TRUE)
usethis::use_data(scotland_ltla_hi_social_determinants, overwrite = TRUE)
