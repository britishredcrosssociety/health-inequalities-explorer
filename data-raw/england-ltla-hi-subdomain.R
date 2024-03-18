library(tidyverse)
# library(janitor)
library(geographr)
library(sf)
library(healthyr)
library(ggridges)

# --- Lookups ----
ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^E"))

# ---- Healthy People / Health Outcomes ----
england_ltla_hi_outcomes_sub <- england_health_index_subdomains |>
  filter(year == 2021) |>
  select(1, 3:7) |>
  left_join(ltla) |>
  select(-ltla21_code, ) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name) |>
  pivot_longer(cols = -area_name, names_to = "variable") |>
  pivot_wider(names_from = area_name, values_from = value) |>
  mutate(`England Average` = rowMeans(across(-c(variable)))) |>
  relocate(`England Average`, .after = variable)

# --- Healthy Lives / Preventable Risk Factors ----
england_ltla_hi_risk_factors_sub <- england_health_index_subdomains |>
  filter(year == 2021) |>
  select(1, 8:11) |>
  left_join(ltla) |>
  select(-ltla21_code, ) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name) |>
  pivot_longer(cols = -area_name, names_to = "variable") |>
  pivot_wider(names_from = area_name, values_from = value) |>
  mutate(`England Average` = rowMeans(across(-c(variable)))) |>
  relocate(`England Average`, .after = variable)

# --- Healthy Places / Access to green space ----
england_ltla_hi_social_determinants_sub <- england_health_index_subdomains |>
  filter(year == 2021) |>
  select(1, 12:16) |>
  left_join(ltla) |>
  select(-ltla21_code, ) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name) |>
  pivot_longer(cols = -area_name, names_to = "variable") |>
  pivot_wider(names_from = area_name, values_from = value) |>
  mutate(`England Average` = rowMeans(across(-c(variable)))) |>
  relocate(`England Average`, .after = variable)

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

check_distribution(england_ltla_hi_outcomes_sub)
check_distribution(england_ltla_hi_risk_factors_sub)
check_distribution(england_ltla_hi_social_determinants_sub)

# ---- Save datasets ----
usethis::use_data(england_ltla_hi_outcomes_sub, overwrite = TRUE)
usethis::use_data(england_ltla_hi_risk_factors_sub, overwrite = TRUE)
usethis::use_data(england_ltla_hi_social_determinants_sub, overwrite = TRUE)
