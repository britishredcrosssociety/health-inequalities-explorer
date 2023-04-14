# ---- Load libs ----
library(tidyverse)
library(sf)
library(demographr)
library(geographr)

# Note: ICBs and LTLAs are not coterminous, so ltla demographic data can't be
#       repurposed for ICBs
# Source: https://www.housinglin.org.uk/_assets/Resources/Housing/OtherOrganisation/The-evolving-role-of-county-authorities-in-Integrated-Care-Systems.pdf

# ---- Create lookups ----
icb <- boundaries_icb22 |>
  st_drop_geometry() |>
  mutate(icb22_name = str_remove_all(icb22_name, "^NHS ")) |>
  mutate(icb22_name = str_remove_all(icb22_name, " Integrated Care Board$"))

lsoa_lsoa <- lookup_lsoa11_lsoa21_ltla22 |>
  select(lsoa11_code, lsoa21_code) |>
  filter(str_detect(lsoa21_code, "^E"))

# 837 LSOAs were split from 2011 to 2021
# Run `lsoa_lsoa |> filter(lsoa11_code == "E01033583")` for an example
lsoa_lsoa |>
  count(lsoa11_code, sort = TRUE) |>
  filter(n > 1)

lsoas_split_from_2011_to_2021 <- lsoa_lsoa |>
  count(lsoa11_code) |>
  filter(n > 1) |>
  pull(lsoa11_code)

# 100 LSOAs were merged from 2011 to 2021
# Run `lsoa_lsoa |> filter(lsoa21_code == "E01033769")` for an example
lsoa_lsoa |>
  count(lsoa21_code, sort = TRUE) |>
  filter(n > 1)

lsoas_merged_from_2011_to_2021 <- lsoa_lsoa |>
  count(lsoa21_code) |>
  filter(n > 1) |>
  pull(lsoa21_code)

# LSOA aggregation stategy going from 2021 codes to 2011 codes:
# - When the lsoa11_code is in lsoas_merged_from_2011_to_2021, group by
#   lsoa11_code and calculate sum/mean for number/percent
# - When the lsoa21_code is in lsoas_merged_from_2011_to_2021, the value needs
#   dividing in two and assigned to the matching 2011 LSOAs (two is the maximum
#   number of 2011 LSOAs that were merged together)
# - Another approach: when the lsoa21_code is NOT in
#   lsoas_merged_from_2011_to_2021, group by the lsoa11_code and calculate
#   sum/mean for number/percent, else split the value across 2011 LSOAs.

# ---- Population ----
# Source: https://www.nomisweb.co.uk/sources/census_2021_ts
# Data set: TS007A - Age by five-year age bands
# Note: a breakdown by sex, currently isn't available
population_raw <- read_csv(
  "data-raw/census-raw/TS007A_age_by_five_year_age_bands_lsoa11.csv",
  skip = 4
)

population_groupings <- population_raw |>
  select(-`2021 super output area - lower layer`) |>
  rename(lsoa21_code = mnemonic, population_lsoa = Total) |>
  pivot_longer(!c(lsoa21_code, population_lsoa), names_to = "age", values_to = "population") |>
  mutate(
    age = case_when(
      age == "Aged 4 years and under" ~ "Younger \npeople (< 18)",
      age == "Aged 5 to 9 years" ~ "Younger \npeople (< 18)",
      age == "Aged 10 to 14 years" ~ "Younger \npeople (< 18)",
      age == "Aged 15 to 19 years" ~ "Younger \npeople (< 18)",
      age == "Aged 20 to 24 years" ~ "Working \nage (18-65)",
      age == "Aged 25 to 29 years" ~ "Working \nage (18-65)",
      age == "Aged 30 to 34 years" ~ "Working \nage (18-65)",
      age == "Aged 35 to 39 years" ~ "Working \nage (18-65)",
      age == "Aged 40 to 44 years" ~ "Working \nage (18-65)",
      age == "Aged 45 to 49 years" ~ "Working \nage (18-65)",
      age == "Aged 50 to 54 years" ~ "Working \nage (18-65)",
      age == "Aged 55 to 59 years" ~ "Working \nage (18-65)",
      age == "Aged 60 to 64 years" ~ "Working \nage (18-65)",
      age == "Aged 65 to 69 years" ~ "Older \npeople (65+)",
      age == "Aged 70 to 74 years" ~ "Older \npeople (65+)",
      age == "Aged 75 to 79 years" ~ "Older \npeople (65+)",
      age == "Aged 80 to 84 years" ~ "Older \npeople (65+)",
      age == "Aged 85 years and over" ~ "Older \npeople (65+)"
    )
  ) |>
  group_by(lsoa21_code, population_lsoa, age) |>
  summarise(population = sum(population)) |>
  ungroup()

population_refactored <- population_groupings |>
  mutate(
    age = factor(
      age,
      levels = c(
        "Younger \npeople (< 18)",
        "Working \nage (18-65)",
        "Older \npeople (65+)"
      )
    )
  ) |>
  relocate(population_lsoa, .after = population)

# Aggregate from 2021 to 2011 codes for ICB lookup
population_merged <- population_refactored |>
  left_join(lsoa_lsoa) |>
  filter(!(lsoa21_code %in% lsoas_merged_from_2011_to_2021)) |>
  group_by(lsoa11_code, age) |>
  summarise(
    population = sum(population),
    population_lsoa = mean(population_lsoa)
  ) |>
  ungroup()

# The method of keeping lsoa populations hasn't worked. Can the population_lsoa
# be calculated from the recalculated populations. Does this make sense?
population_merged |> filter(lsoa11_code == "E01033583")

# population_split <- population_refactored |>
#   left_join(lsoa_lsoa) |>
#   filter(lsoa21_code %in% lsoas_merged_from_2011_to_2021) |>
#   mutate(population = population / 2) |>
#   select(-lsoa21_code) |>
#   relocate(lsoa11_code)

# population_lsoa11 <- bind_rows(
#   population_merged, population_split
# )
