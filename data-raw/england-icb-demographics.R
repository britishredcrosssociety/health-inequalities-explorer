# ---- Load libs ----
library(tidyverse)
library(sf)
library(demographr)
library(geographr)
library(compositr)
library(ggridges)

# Note: ICBs and LTLAs are not coterminous, so ltla demographic data can't be
#       repurposed for ICBs
# Source: https://www.housinglin.org.uk/_assets/Resources/Housing/OtherOrganisation/The-evolving-role-of-county-authorities-in-Integrated-Care-Systems.pdf

# ---- Create lookups ----
icb <- boundaries_icb22 |>
  st_drop_geometry() |>
  mutate(icb22_name = str_remove_all(icb22_name, "^NHS ")) |>
  mutate(icb22_name = str_remove_all(icb22_name, " Integrated Care Board$"))

lsoa_icb <- lookup_lsoa11_sicbl22_icb22_ltla22 |>
  distinct(lsoa11_code, icb22_code)

# ---- 2011 to 2021 LSOA census changes ----
lsoa_lsoa <- lookup_lsoa11_lsoa21_ltla22 |>
  filter(str_detect(lsoa21_code, "^E")) |>
  distinct(lsoa11_code, lsoa21_code, change_code) |>
  relocate(change_code, .after = lsoa21_code)

# Change codes:
# - U = Unchanged
# - S = Split
# - M = Merged
# - X = Fragmented (irregular lookups that can't be easilt mapped)

# U: 31,810 LSOAs remained unchanged from 2011 to 2021
lsoa_lsoa |>
  filter(change_code == "U") |>
  distinct(lsoa11_code)

# S: 834 LSOAs were split from 2011 to 2021
lsoa_lsoa |>
  filter(change_code == "S") |>
  distinct(lsoa11_code)

# M: 194 LSOAs were merged from 2011 to 2021
lsoa_lsoa |>
  filter(change_code == "M") |>
  distinct(lsoa11_code)

# X: 6 LSOAs were fragmented from 2011 to 2021
lsoa_lsoa |>
  filter(change_code == "X") |>
  distinct(lsoa11_code)

# Aggregation stategy going from 2021 codes to 2011 codes:
# - change_code == "U": No action required
# - change_code == "S": group by lsoa11_code and sum the count to undo the
#   original split.
# - change_code == "M": divide the count by two to undo the original merge. Two
#   is the maximum number of 2011 LSOAs that were merged together, so an
#   assumption is made that the population is split equally between the two
#   2011 areas. The maximum number of merged areas can be checked with:
#   `lsoa_lsoa |> filter(change_code == "M") |> count(lsoa21_code) |> count(n)`
# - change_code == "X": count the number of times the lsoa21_code appears, when
#   the lsoa21_code appears only once, assign the original count, when it
#   appears twice, assign half the count. Then, group by the lsoa11_code and
#   sum the counts.
aggregate_census_lsoas <- function(data, count, group = NULL) {
  data_u <- data |>
    left_join(lsoa_lsoa) |>
    filter(change_code == "U") |>
    select(lsoa11_code, {{ group }}, number = {{ count }})

  data_s <- data |>
    left_join(lsoa_lsoa) |>
    relocate(lsoa11_code) |>
    filter(change_code == "S") |>
    group_by(lsoa11_code, {{ group }}) |>
    summarise(number = sum({{ count }})) |>
    ungroup()

  data_m <- data |>
    left_join(lsoa_lsoa) |>
    relocate(lsoa11_code) |>
    filter(change_code == "M") |>
    mutate(number = {{ count }} / 2) |>
    select(lsoa11_code, {{ group }}, number)

  data_x <- data |>
    left_join(lsoa_lsoa) |>
    relocate(lsoa11_code) |>
    filter(change_code == "X") |>
    group_by({{ group }}) |>
    add_count(lsoa21_code) |>
    mutate(
      new_count = case_when(
        n == 1 ~ {{ count }},
        TRUE ~ {{ count }} * 0.5
      )
    ) |>
    group_by(lsoa11_code, {{ group }}) |>
    summarise(number = sum(new_count)) |>
    ungroup()

  data_aggregated <- bind_rows(data_u, data_s, data_m, data_x)

  data_aggregated
}

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

# Aggregate census LSOAs for use in ICB lookup
# Note: some of the recombined 2011 LSOAs (e.g., "E01033583") have large
# populations that create a skewed distribution with a long right-tail. This
# matches the figures and distribtuion of the latest mid-year population
# estimates that use the same 2011 LSOA areas.
population_aggregated <- population_refactored |>
  aggregate_census_lsoas(count = population, group = age)

# Aggregate to ICB's
population_icb <- population_aggregated |>
  left_join(lsoa_icb) |>
  left_join(icb) |>
  group_by(icb22_name, age) |>
  summarise(number = sum(number)) |>
  mutate(icb_population = sum(number)) |>
  ungroup() |>
  mutate(percent = number / icb_population) |>
  select(-icb_population) |>
  rename(area_name = icb22_name, variable = age)

# ---- Ethnicity ----
# Source: https://www.nomisweb.co.uk/sources/census_2021_ts
# Data set: TS007A - Age by five-year age bands
ethnicity_raw <- read_csv(
  "data-raw/census-raw/TS021_ethnic_group_lsoa11.csv",
  skip = 7
) |>
  slice(1:(n() - 5))

ethnicity_groupings <- ethnicity_raw |>
  filter(str_detect(mnemonic, "^E")) |>
  select(
    -`2021 super output area - lower layer`,
    -`Total: All usual residents`
  ) |>
  rename(lsoa21_code = mnemonic) |>
  pivot_longer(
    !lsoa21_code,
    names_to = "ethnicity",
    values_to = "group_count"
  ) |>
  mutate(
    ethnicity = case_when(
      ethnicity == "Asian, Asian British or Asian Welsh" ~ "Asian, Asian \nBritish or \nAsian Welsh",
      ethnicity == "Black, Black British, Black Welsh, Caribbean or African" ~ "Black, Black British, \nBlack Welsh, \nCaribbean or African",
      ethnicity == "Mixed or Multiple ethnic groups" ~ "Mixed or Multiple \nethnic group",
      ethnicity == "White" ~ "White",
      ethnicity == "Other ethnic group" ~ "Other ethnic \ngroup",
    )
  )

ethnicity_aggregated <- ethnicity_groupings |>
  aggregate_census_lsoas(count = group_count, group = ethnicity)

ethnicity_icb <- ethnicity_aggregated |>
  left_join(lsoa_icb) |>
  left_join(icb) |>
  group_by(icb22_name, ethnicity) |>
  summarise(number = sum(number)) |>
  mutate(icb_population = sum(number)) |>
  ungroup() |>
  mutate(percent = number / icb_population) |>
  select(-icb_population) |>
  rename(area_name = icb22_name, variable = ethnicity)

# ---- Join ----
joined <- bind_rows(
  population_icb, ethnicity_icb
)

# ---- Normalise/scale ----
england_icb_demographics_scaled <-
  joined |>
  group_by(variable) |>
  mutate(scaled = positional_normalisation(percent)) |>
  ungroup()

# Check distributions
england_icb_demographics_scaled |>
  ggplot(aes(x = scaled, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

# ---- Add plot labels ----
england_icb_demographics <- england_icb_demographics_scaled |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Count: ", round(number),
      "<br>", "Percent ", round(percent * 100, 1), "%"
    )
  )

# ---- Export data ----
usethis::use_data(england_icb_demographics, overwrite = TRUE)
