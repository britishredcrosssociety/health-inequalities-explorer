library(tidyverse)
#library(janitor)
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
  #clean_names() |>
  select(1, 3:7) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name)|>
  pivot_longer(cols = -area_name, names_to = "variable") |>
  pivot_wider(names_from = area_name, values_from = value)

