library(tidyverse)
library(geographr)
library(sf)

ltla_names <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  select(starts_with("ltla21_"))

# Data not yet ready, so a demo version has been calculated locally for testing
raw <- read_csv(
  ""
)

ltla_cap_england <-
  raw |>
  left_join(ltla_names, by = c("lad_code" = "ltla21_code")) |>
  mutate(ltla21_name = str_replace_all(ltla21_name, "'", "")) |>
  select(
    area_name = ltla21_name,
    `Overall Score` = health_inequalities_composite_rank,
    `Access & Availability` = access_availability_domain_rank,
    `Workforce` = workforce_domain_rank,
    `Quality` = quality_domain_rank
  ) |>
  pivot_longer(cols = -area_name, names_to = "variable")

usethis::use_data(ltla_cap_england, overwrite = TRUE)