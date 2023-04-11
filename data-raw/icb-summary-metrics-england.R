library(tidyverse)
library(readxl)
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
    area_code = icb22_code
  ) |>
  mutate(top_10 = if_else(decile == 1, "yes", "no")) |>
  group_by(area_code, top_10) |>
  summarise(n = n()) |>
  mutate(freq = n / sum(n)) |>
  ungroup() |>
  filter(top_10 == "no") |> # Not all ICBs have top 10% most vulnerable LSOAs
  mutate(percent = 1 - freq) |>
  mutate(variable = "The proportion of LSOAs within the ICB that are highly deprived (decile = 1)", .after = area_code) |>
  mutate(number = NA, .before = percent) |>
  select(-top_10, -n, -freq)

# ---- ONS Health Index ----
# Higher score = better health
# Higher rank (calculated here) = better health
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/datasets/healthindexscoresintegratedcaresystemsengland
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/datasets/healthindexscoresintegratedcaresystemsengland/current/healthindexscoresintegratedcaresystemsengland.xlsx"

raw <- download_file(
  url,
  ".xlsx"
)

health_index_raw <- read_excel(raw, sheet = "Table_2_Index_scores", skip = 2)

health_index <- health_index_raw |>
  select(area_code = `Area Code`, number = `2020`) |> 
  mutate(number = rank(number)) |> 
  mutate(percent = NA) |> 
  mutate(variable = "ONS Health \nIndex rank") |> 
  relocate(variable, .after = area_code)

# ---- Left-behind areas ----
# Wards / ICBs are not coterminous:
library(leaflet)

leaflet() |>
  setView(lat = 54.59, lng = -5.93, zoom = 6) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = boundaries_icb22,
    weight = 0.7,
    opacity = 0.5,
    color = "#482677FF",
    dashArray = "0.1",
    fillOpacity = 0.2
  ) |>
  addPolygons(
    data = boundaries_wards21,
    weight = 0.7,
    opacity = 0.5,
    color = "#5C747A",
    dashArray = "0.1",
    fillOpacity = 0.05
  )

# Solution: work backwords:
# 1. Do a ward to LSOA lookup.
# 2. Assign LSOA's as left-behind or not.
# 3. Lookup LSOAs to ICBS.
# 4. Count the percentage of left-behind LSOAs per ICB.
# Limitation: it assums all smaller areas share the property of the larger area
# which is likely not to be the case. This means that areas can be wrongly assigned
# as left-behind when this is not actually the case. Given wards are already small
# areas, the effects of this should be low.