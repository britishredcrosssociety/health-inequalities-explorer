library(tidyverse)
library(sf)
library(demographr)
library(geographr)

ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^E"))

# source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationandhouseholdestimatesenglandandwalescensus2021
# Note: the data is available in demographr, but not with the breakdown of sex
file <- compositr::download_file(
  "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationandhouseholdestimatesenglandandwalescensus2021/census2021/census2021firstresultsenglandwales1.xlsx",
  ".xlsx"
)

unprocessed <-
  readxl::read_excel(
    file,
    sheet = "P03",
    range = "A8:AO383"
  )

# Finish processing
age_sex <-
  unprocessed |>
  select(-`Area name`, -`All persons`) |>
  rename(area_code = `Area code [note 2]`) |>
  pivot_longer(!area_code, names_to = "age") |>
  filter(area_code %in% ltla$ltla21_code) |>
  mutate(sex = if_else(str_detect(age, "^Females"), "Female", "Male")) |>
  relocate(sex, .after = area_code) |>
  mutate(age = str_remove_all(age, "Females:\r\nAged ")) |>
  mutate(age = str_remove_all(age, "Males:\r\nAged ")) |>
  mutate(age = str_remove_all(age, "\r\n\\[note 12]")) |>
  mutate(age = str_remove_all(age, " years")) |>
  mutate(age = str_replace_all(age, " to ", "-")) |>
  mutate(
    age = case_when(
      age == "4 and under" ~ "0-4",
      age == "90 and over" ~ "90+",
      TRUE ~ age
    )
  )

# test age pyramid by sex visualisation to establish best data layout and how
# to best minimse dataset size.
