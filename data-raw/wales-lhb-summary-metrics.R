library(tidyverse)
library(IMD)
library(geographr)
library(sf)
library(ggridges)
install.packages("geographr")


hb <- boundaries_lhb20 |>
  st_drop_geometry()

# ---- IMD score ----
colnames(hb)
imd<-
  imd_wales_lsoa|>
  left_join(hb, by="lhb20_code") |>
  select(lsoa_code, lhb20_code, IMD_decile) |>
  mutate(top_10 = if_else(IMD_decile == 1, "yes", "no")) |>
  group_by(lhb20_code, top_10) |>
  summarise(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(total_number_lsoas = sum(n)) |>
  ungroup() |>
  filter(top_10 == "no") |> 
  mutate(number = total_number_lsoas - n) |>
  mutate(percent = 1 - freq) |>
  mutate(variable = "Deprivation", .after = lhb20_code) |>
  select(-top_10, -n, -freq, -total_number_lsoas)

# ---- resilience index ----
url <- "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/wales/healthy-people-domain.csv"
resilience_index_raw <- read_csv(url)
resilience_index <- resilience_index_raw |>
  select(lhb20_code= lad_code, number = healthy_people_domain_rank)|>
  mutate(percent = NA) |>
  mutate(variable = "Resilience Index \nrank") |>
  relocate(variable, .after = lhb20_code)

# ---- % Left-behind areas ----
lba_msoa <- lookup_msoa11_ward17 |>
  filter(str_detect(msoa11_code, "^W")) |> # Step 1
  left_join(cni_wales_msoa11) |>
  select(msoa11_code, ward17_code, left_behind = `Left Behind Area?`) |>
  mutate(left_behind = if_else(is.na(left_behind), FALSE, left_behind))

lba_lhb <- lba_msoa |>
  cross_join(hb) |>
  select(lhb20_code, left_behind)

lba <- lba_lhb |>
  group_by(lhb20_code, left_behind) |>
  summarise(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(total_number_lsoas = sum(n)) |>
  ungroup() |>
  filter(left_behind == FALSE) |> 
  mutate(number = total_number_lsoas - n) |>
  mutate(percent = 1 - freq) |>
  mutate(variable = "Left-behind areas", .after = lhb20_code) |>
  select(-left_behind, -n, -freq, -total_number_lsoas)
