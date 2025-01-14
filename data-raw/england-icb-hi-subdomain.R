library(tidyverse)
library(geographr)
library(sf)
library(healthyr)
library(ggridges)

# --- Lookups ----
icb <- boundaries_icb22 |>
  st_drop_geometry() |>
  mutate(icb22_name = str_remove_all(icb22_name, "^NHS ")) |>
  mutate(icb22_name = str_remove_all(icb22_name, " Integrated Care Board$"))

outcomes <- c(
  "difficulties_in_daily_life",
  "mental_health",
  "mortality",
  "personal_well_being",
  "physical_health_conditions"
)

risk_factors <- c(
  "behavioural_risk_factors",
  "children_and_young_people",
  "physiological_risk_factors",
  "protective_measures"
)

social_determinants <- c(
  "access_to_green_space",
  "access_to_services",
  "crime",
  "economic_and_working_conditions",
  "living_conditions"
)

all_subdomains <- england_health_index_ics_subdomains |>
  filter(year == 2021) |>
  select(-icb22_name, -year) |>
  left_join(icb) |>
  select(-icb22_code) |>
  rename(area_name = icb22_name, variable = sub_domain) |>
  relocate(area_name) |>
  pivot_wider(names_from = area_name, values_from = value) |>
  rowwise() |>
  mutate(`England Average` = mean(c_across(-variable)), .after = variable) |>
  ungroup()

# ---- Healthy People / Health Outcomes ----
england_icb_hi_outcomes_sub <- all_subdomains |>
  filter(variable %in% outcomes) |>
  mutate(variable = str_replace_all(variable, "_", " ")) |>
  mutate(variable = str_to_sentence(variable))

# --- Healthy Lives / Preventable Risk Factors ----
england_icb_hi_risk_factors_sub <- all_subdomains |>
  filter(variable %in% risk_factors) |>
  mutate(variable = str_replace_all(variable, "_", " ")) |>
  mutate(variable = str_to_sentence(variable))

# --- Healthy Places / Access to green space ----
england_icb_hi_social_determinants_sub <- all_subdomains |>
  filter(variable %in% social_determinants) |>
  mutate(variable = str_replace_all(variable, "_", " ")) |>
  mutate(variable = str_to_sentence(variable))

# ---- Check distributions ----
check_distribution <- function(data) {
  data |>
    pivot_longer(-c(variable, `England Average`), names_to = "area_name", values_to = "value") |>
    ggplot(aes(x = value, y = variable)) +
    geom_density_ridges(scale = 4) +
    scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
    scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
    coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
    theme_ridges()
}

check_distribution(england_icb_hi_outcomes_sub)
check_distribution(england_icb_hi_risk_factors_sub)
check_distribution(england_icb_hi_social_determinants_sub)

# ---- Save datasets ----
usethis::use_data(england_icb_hi_outcomes_sub, overwrite = TRUE)
usethis::use_data(england_icb_hi_risk_factors_sub, overwrite = TRUE)
usethis::use_data(england_icb_hi_social_determinants_sub, overwrite = TRUE)
