library(tidyverse)
# library(janitor)
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


# ---- Healthy People / Health Outcomes ----
scotland_ltla_hi_outcomes_sub <- scotland_health_index_subdomains |>
  select(1, starts_with("people_")) |>
  rename(ltla21_code = ltla24_code) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name) |>
  pivot_longer(cols = -area_name, names_to = "variable") |>
  pivot_wider(names_from = area_name, values_from = value) |>
  mutate(`Scotland Average` = rowMeans(across(-c(variable)))) |>
  relocate(`Scotland Average`, .after = variable) |>
  mutate(variable = str_remove(variable, "^people_")) |>
  mutate(variable = str_replace_all(variable, "_", " ")) |>
  mutate(variable = str_to_sentence(variable))

# --- Healthy Lives / Preventable Risk Factors ----
scotland_ltla_hi_risk_factors_sub <- scotland_health_index_subdomains |>
  select(1, starts_with("lives_")) |>
  rename(ltla21_code = ltla24_code) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name) |>
  pivot_longer(cols = -area_name, names_to = "variable") |>
  pivot_wider(names_from = area_name, values_from = value) |>
  mutate(`Scotland Average` = rowMeans(across(-c(variable)))) |>
  relocate(`Scotland Average`, .after = variable) |>
  mutate(variable = str_remove(variable, "^lives_")) |>
  mutate(variable = str_replace_all(variable, "_", " ")) |>
  mutate(variable = str_to_sentence(variable))

# --- Healthy Places / Access to green space ----
scotland_ltla_hi_social_determinants_sub <- scotland_health_index_subdomains |>
  select(1, starts_with("places_")) |>
  rename(ltla21_code = ltla24_code) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name) |>
  pivot_longer(cols = -area_name, names_to = "variable") |>
  pivot_wider(names_from = area_name, values_from = value) |>
  mutate(`Scotland Average` = rowMeans(across(-c(variable)))) |>
  relocate(`Scotland Average`, .after = variable) |>
  mutate(variable = str_remove(variable, "^places_")) |>
  mutate(variable = str_replace_all(variable, "_", " ")) |>
  mutate(variable = str_to_sentence(variable))

# ---- Check distributions ----
check_distribution <- function(data) {
  data |>
    pivot_longer(-c(variable, `Scotland Average`), names_to = "area_name", values_to = "value") |>
    ggplot(aes(x = value, y = variable)) +
    geom_density_ridges(scale = 4) +
    scale_y_discrete(expand = c(0, 0)) +    # will generally have to set the `expand` option
    scale_x_continuous(expand = c(0, 0)) +  # for both axes to remove unneeded padding
    coord_cartesian(clip = "off") +         # to avoid clipping of the very top of the top ridgeline
    theme_ridges()
}

check_distribution(scotland_ltla_hi_outcomes_sub)
check_distribution(scotland_ltla_hi_risk_factors_sub)
check_distribution(scotland_ltla_hi_social_determinants_sub)

# ---- Save datasets ----
usethis::use_data(scotland_ltla_hi_outcomes_sub, overwrite = TRUE)
usethis::use_data(scotland_ltla_hi_risk_factors_sub, overwrite = TRUE)
usethis::use_data(scotland_ltla_hi_social_determinants_sub, overwrite = TRUE)
