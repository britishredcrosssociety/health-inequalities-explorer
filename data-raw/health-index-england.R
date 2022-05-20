library(tidyverse)
library(geographr)
library(sf)

ltla_names <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  select(starts_with("ltla21_"))

raw <- read_csv(
  "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/england/health-index-2019.csv"
)

hi_vul_england <-
  raw |>
  left_join(ltla_names, by = c("lad_21_code" = "ltla21_code")) |>
  mutate(ltla21_name = str_replace_all(ltla21_name, "'", "")) |>
  select(
    area_name = ltla21_name,
    `Overall Score` = overall_score_rank,
    `Healthy People` = healthy_people_rank,
    `Healthy Lives` = healthy_lives_rank,
    `Healthy Places` = healthy_places_rank
  ) |>
  pivot_longer(cols = -area_name, names_to = "variable")

usethis::use_data(hi_vul_england, overwrite = TRUE)