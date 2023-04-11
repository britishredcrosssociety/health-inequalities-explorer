library(tidyverse)
library(geographr)
library(compositr)
library(sf)
library(IMD)
library(ggridges)

# ---- IMD score ----
# Decile 1 = most deprived
imd <- imd_england_lsoa |>
  rename(lsoa11_code = lsoa_code) |> 
  left_join(lookup_lsoa11_sicbl22_icb22_ltla22) |> 
  select(
    lsoa11_code,
    decile = IMD_decile,
    icb22_code
  ) |> 
  mutate(top_10 = if_else(decile == 1, "yes", "no")) |> 
  group_by(icb22_code, top_10) |>
  summarise(n = n()) |>
  mutate(freq = n / sum(n)) |>
  ungroup()|> 
  filter(top_10 == "no") |> # Not all ICBs have top 10% most vulnerable LSOAs
  mutate(percent = 1 - freq) |> 
  mutate(variable = "The proportion of LSOAs within the ICB that are highly deprived (decile = 1)", .after = icb22_code) |>
  mutate(number = NA, .before = percent) |> 
  select(-top_10, -n, -freq)