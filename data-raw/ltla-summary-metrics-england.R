library(tidyverse)
library(geographr)
library(sf)
library(IMD)

ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^E"))

lookup_england_ltla <-
  lookup_ltla_ltla |>
  filter(str_detect(ltla21_code, "^E"))

# ---- IMD score ----
# Higher score = more deprived
imd <-
  imd_england_lad |>
  select(ltla19_code = lad_code, imd_score = Score) |>
  left_join(lookup_england_ltla, by = "ltla19_code") |>
  select(ltla21_code, imd_score) |>
  group_by(ltla21_code) |>
  summarise(imd_score = mean(imd_score))

# ---- % Left-behind areas ----
lba <-
  cni_england_ward17 |>
  left_join(lookup_england_ltla, by = c("lad19_code" = "ltla19_code")) |>
  select(ward17_code, ltla21_code, lba = `Left Behind Area?`) |>
  group_by(ltla21_code) |>
  count(lba) |>
  mutate(lba_percentage = n / sum(n)) |>
  ungroup() |>
  filter(lba == TRUE) |>
  right_join(ltla) |>
  select(ltla21_code, lba_percentage) |>
  mutate(lba_percentage = replace_na(lba_percentage, 0))

# ---- ONS Health Index score ----

health_index_raw <- read_csv(
  "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/england/health-index-2019.csv"
)

health_index <-
  health_index_raw |>
  select(
    ltla21_code = lad_21_code,
    health_index_score = overall_score_rank
  )

# ---- % NHS priority wards ----
# To be added

# ---- Combine and export ----
ltla_summary_metrics_england <-
  ltla |>
  left_join(imd) |>
  left_join(lba) |>
  left_join(health_index)

usethis::use_data(ltla_summary_metrics_england, overwrite = TRUE)
