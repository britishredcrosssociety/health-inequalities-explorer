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

# 837 LSOAs were split from 2011 to 2021: one 2011 LSOAs to many 2021 LSOAs
# Run `lsoa_lsoa |> filter(lsoa11_code == "E01033583")` for an example, and then
# check the lsoa21_codes, and it can be seen that each lsoa21_code comes from
# the split and no where else
lsoa_lsoa |>
  count(lsoa11_code, sort = TRUE) |>
  filter(n > 1)

# Note: one 2011 to many 2021
lsoas_one_to_many <- lsoa_lsoa |>
  count(lsoa11_code) |>
  filter(n > 1) |>
  select(-n)

# 100 LSOAs were merged from 2011 to 2021: many 2011 LSOAs to one 2021 LSOA
# Run `lsoa_lsoa |> filter(lsoa21_code == "E01033769")` for an example, and then
# check the lsoa11_codes, and it can be seen that each lsoa11_code went directly
# into the merge and no where else
lsoa_lsoa |>
  count(lsoa21_code, sort = TRUE) |>
  filter(n > 1)

# Note: many 2011 to one 2021
lsoas_many_to_one <- lsoa_lsoa |>
  count(lsoa21_code) |>
  filter(n > 1) |>
  select(-n)

# Three LSOAs are involved in both splits and merge in a many to many
# relationship
lsoas_many_to_many <- lsoa_lsoa |>
  mutate(
    one_to_many = if_else(
      lsoa11_code %in% lsoas_one_to_many$lsoa11_code, TRUE, FALSE
    )
  ) |>
  mutate(
    many_to_one = if_else(
      lsoa21_code %in% lsoas_many_to_one$lsoa21_code, TRUE, FALSE
    )
  ) |>
  filter(one_to_many == TRUE & many_to_one == TRUE) |>
  select(starts_with("lsoa"))

# Remove the LSOAs that appear in lsoas_many_to_many from the one_to_many
# and many_to_one lists
lsoas_one_to_many <- lsoas_one_to_many |>
  filter(!(lsoa11_code %in% lsoas_many_to_many$lsoa11_code))

lsoas_many_to_one <- lsoas_many_to_one |>
  filter(!(lsoa21_code %in% lsoas_many_to_many$lsoa21_code))

# LSOA aggregation stategy going from 2021 codes to 2011 codes:
# - When the lsoa11_code is in lsoas_one_to_many, group by lsoa11_code and
#   sum the 'number' (i.e., count) to get the total count. Think: to get the
#   one, you must group and sum the many. Note: all other LSOAs not found in
#   the step below can also be grouped/summed too without effecting their
#   counts.
# - When the lsoa21_code is in lsoas_many_to_one, the value needs dividing in
#   two to apportion the value to the matching 2011 LSOAs (two is the maximum
#   number of 2011 LSOAs that were merged together, so an assumption is made
#   that the population is split equally between the two 2011 areas). Think: to
#   get the many, you must divide the (bigger) one.
# - The three edge cases where LSOAs map many to many get handled manually.
# - All other LSOAs remain unchanged and map one to one

# ---- Population ----
# Source: https://www.nomisweb.co.uk/sources/census_2021_ts
# Data set: TS007A - Age by five-year age bands
# Note: a breakdown by sex, currently isn't available
population_raw <- read_csv(
  "data-raw/census-raw/TS007A_age_by_five_year_age_bands_lsoa11.csv",
  skip = 4
)

population_groupings <- population_raw |>
  filter(str_detect(mnemonic, "^E")) |>
  select(-`2021 super output area - lower layer`, -Total) |>
  rename(lsoa21_code = mnemonic) |>
  pivot_longer(!lsoa21_code, names_to = "age", values_to = "population") |>
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
  group_by(lsoa21_code, age) |>
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
  )

# Resolve the one (2011) to many (2021) changes:
population_one_to_many_resolved <- population_refactored |>
  left_join(lsoa_lsoa) |>
  relocate(lsoa11_code) |>
  semi_join(lsoas_one_to_many) |>
  group_by(lsoa11_code, age) |>
  summarise(population = sum(population)) |>
  ungroup()

# Resolve the many (2011) to one (2021) changes:
population_many_to_one_resolved <- population_refactored |>
  left_join(lsoa_lsoa) |>
  relocate(lsoa11_code) |>
  semi_join(lsoas_many_to_one) |>
  mutate(population = population / 2) |>
  select(-lsoa21_code) |>
  relocate(lsoa11_code)

# Resolve the many to many changes:
# population_many_to_many_resolved <-
population_refactored |>
  left_join(lsoa_lsoa) |>
  relocate(lsoa11_code) |>
  filter(
    lsoa11_code %in% lsoas_many_to_many$lsoa11_code |
      lsoa21_code %in% lsoas_many_to_many$lsoa21_code
  ) |> 
  group_by(age) |> 
  # See note book with method to calculate aggregations. Need to find a way
  # to transpose this into code








population_unchanged <- population_refactored |>
  left_join(lsoa_lsoa) |>
  relocate(lsoa11_code) |>
  select(-lsoa21_code) |>
  anti_join(population_one_to_many_resolved) |>
  anti_join(population_many_to_one_resolved) |>
  anti_join(population_many_to_many_resolved)

population_2011_lsoas <- bind_rows(
  population_one_to_many_resolved,
  population_many_to_one_resolved,
  population_many_to_many_resolved,
  population_unchanged
)

# NOTE: some of the recombined 2011 LSOAs (e.g., "E01033583") have large
# populations that create a skewed distribution with a long right-tail. This
# matches the figures and distribtuion of the latest mid-year population
# estimates that use the same 2011 LSOA areas.
