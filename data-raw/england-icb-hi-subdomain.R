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

# ---- Healthy People / Health Outcomes ----
# england_icb_hi_outcomes_sub <-
england_health_index_ics_subdomains |>
  filter(year == 2021) |>
  select(-icb22_name, -year) |>
  left_join(icb) |>
  select(-icb22_code) |>
  rename(area_name = icb22_name) |>
  relocate(area_name)

# TODO:
# - filter only relevant sub-domains
# - calculate england mean
# - transpose
# - Do other two subdomains
