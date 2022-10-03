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
population_file <- compositr::download_file(
  "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationandhouseholdestimatesenglandandwalescensus2021/census2021/census2021firstresultsenglandwales1.xlsx",
  ".xlsx"
)

population_raw <-
  readxl::read_excel(
    population_file,
    sheet = "P03",
    range = "A8:AO383"
  )

population_longer <-
  population_raw |>
  select(-`Area name`, -`All persons`) |>
  rename(area_code = `Area code [note 2]`) |>
  pivot_longer(!area_code, names_to = "age", values_to = "population")

population_filter_areas <-
  population_longer |>
  filter(area_code %in% ltla$ltla21_code)

population_tidy_age <-
  population_filter_areas |>
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

population_grouppings <-
  population_tidy_age |>
  mutate(
    age = case_when(
      age == "0-4" ~ "Younger \npeople (< 18)",
      age == "5-9" ~ "Younger \npeople (< 18)",
      age == "10-14" ~ "Younger \npeople (< 18)",
      age == "15-19" ~ "Younger \npeople (< 18)",
      age == "20-24" ~ "Working \nage (18-65)",
      age == "25-29" ~ "Working \nage (18-65)",
      age == "30-34" ~ "Working \nage (18-65)",
      age == "35-39" ~ "Working \nage (18-65)",
      age == "40-44" ~ "Working \nage (18-65)",
      age == "45-49" ~ "Working \nage (18-65)",
      age == "50-54" ~ "Working \nage (18-65)",
      age == "55-59" ~ "Working \nage (18-65)",
      age == "60-64" ~ "Working \nage (18-65)",
      age == "65-69" ~ "Older \npeople (65+)",
      age == "70-74" ~ "Older \npeople (65+)",
      age == "75-79" ~ "Older \npeople (65+)",
      age == "80-84" ~ "Older \npeople (65+)",
      age == "85-89" ~ "Older \npeople (65+)",
      age == "90+" ~ "Older \npeople (65+)"
    )
  ) |>
  group_by(area_code, sex, age) |>
  summarise(population = sum(population)) |>
  ungroup()

population_sex_age_joined <-
  population_grouppings |>
  mutate(
    age = case_when(
      sex == "Female" & age == "Older \npeople (65+)" ~ "Older \nfemales (65+)",
      sex == "Female" & age == "Working \nage (18-65)" ~ "Working age \nfemales (18-65)",
      sex == "Female" & age == "Younger \npeople (< 18)" ~ "Younger \nfemales (< 18)",
      sex == "Male" & age == "Older \npeople (65+)" ~ "Older \nmales (65+)",
      sex == "Male" & age == "Working \nage (18-65)" ~ "Working age \nmales (18-65)",
      sex == "Male" & age == "Younger \npeople (< 18)" ~ "Younger \nmales (< 18)"
    )
  ) |>
  mutate(
    age = factor(
      age,
      levels = c(
        "Younger \nfemales (< 18)",
        "Younger \nmales (< 18)",
        "Working age \nfemales (18-65)",
        "Working age \nmales (18-65)",
        "Older \nfemales (65+)",
        "Older \nmales (65+)"
      )
    )
  ) |> 
  select(-sex)

population_relative <-
  population_sex_age_joined |>
  group_by(area_code) |>
  mutate(population_ltla = sum(population)) |>
  ungroup() |>
  mutate(population_relative = population / population_ltla) |>
  select(-population_ltla)

ltla_demographics_age_england <-
  population_relative |>
  left_join(ltla, by = c("area_code" = "ltla21_code")) |>
  select(-area_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name)

usethis::use_data(ltla_demographics_age_england, overwrite = TRUE)

# see comparisons-across-nations.R in ad-hoc for other demographic stats that
# could be included. Ad-hoc also contains code to scrape ethnicity data from NOMIS
