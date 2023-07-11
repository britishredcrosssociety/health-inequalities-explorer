library(tidyverse)
library(IMD)
library(geographr)
library(sf)
library(ggridges)
ltla <- boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^W"))
View (ltla)
# ---- IMD score ----
# Higher extent = more deprived
# Higher rank  = more deprived

imd <-
  imd_wales_lad |>
  select(ltla21_code = lad_code, imd_score = Extent) |>
  mutate(number = rank(imd_score)) |>
  select(-imd_score) |>
  mutate(variable = "Index of Multiple \nDeprivation rank", .after = ltla21_code) |>
  mutate(percent = NA, .after = number)
#### ---- % Left-behind areas ----
# Higher number/percent = more left-behind
View(cni_wales_msoa11)
lba <-
  cni_wales_msoa11 |>
  rename(ltla21_code=msoa11_code) |>
  select(ltla21_code, lad21_name, lba = `Left Behind Area?`) |>
  group_by(lad21_name) |>
  count(lba) |>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  filter(lba == TRUE) |>
  cross_join(ltla) |>
  mutate(percent = replace_na(percent, 0)) |>
  mutate(n = replace_na(n, 0)) |>
  select(ltla21_code, number = n, percent) |>
  mutate(variable = "Left-behind areas", .after = ltla21_code)
# ---- Health Index Score ----
#  Use the BRC Resilience on git hub
# Index version
# Higher score = worse health
# Higher rank  = worse health
health_index_raw <- read_csv(
  "https://github.com/britishredcrosssociety/resilience-index/blob/main/data/vulnerability/health-inequalities/wales/healthy-people-domain.csv")
View (health_index_raw)
health_index <- health_index_raw |>
  select(ltla21_code = lad_code, number =healthy_people_domain_rank) |>
  mutate(percent = NA) |>
  mutate(variable = "Health Index \nrank") |>
  relocate(variable, .after = ltla21_code)