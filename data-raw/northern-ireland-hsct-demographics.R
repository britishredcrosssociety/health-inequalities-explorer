library(tidyverse)
library(demographr)
library(readxl)
library(httr2)
library(geographr)
library(compositr)

# ---- Age & Sex ----
age_sex_hsct <-
  age_gender_hsct20_ni |>
  select(-hsct20_code, -total_female_population, -total_male_population) |>
  mutate(
    across(younger_females:older_males,
      ~ (. / total_population),
      .names = "percent__{.col}"
    )
  ) |>
  select(-total_population) |>
  rename(
    area_name = hsct20_name,
    number__younger_females = younger_females,
    number__working_age_females = working_age_females,
    number__older_females = older_females,
    number__younger_males = younger_males,
    number__working_age_males = working_age_males,
    number__older_males = older_males
  ) |>
  pivot_longer(!area_name,
    names_to = c(".value", "variable"),
    names_sep = "__"
  ) |>
  mutate(
    variable = case_when(
      variable == "older_females" ~ "Older \nfemales (65+)",
      variable == "working_age_females" ~ "Working age \nfemales (16-64)",
      variable == "younger_females" ~ "Younger \nfemales (< 16)",
      variable == "older_males" ~ "Older \nmales (65+)",
      variable == "working_age_males" ~ "Working age \nmales (16-64)",
      variable == "younger_males" ~ "Younger \nmales (< 16)"
    )
  )

# ---- Ethnicity ----
lookup_northern_ireland_ltla_hsct <-
  lookup_ltla21_hsct18 |>
  select(lad_name = ltla21_name, trust_name = trust18_name)

# Source: https://www.nisra.gov.uk/publications/census-2021-main-statistics-ethnicity-tables
download <- tempfile(fileext = ".xlsx")

request(
  "https://www.nisra.gov.uk/system/files/statistics/census-2021-ms-b01.xlsx"
) |>
  req_perform(download)

raw_count <- read_excel(
  download,
  sheet = "MS-B01",
  range = "A9:P21"
)

ethnicity_hsct <-
  raw_count |>
  select(-`Geography code`) |>
  filter(Geography != "Northern Ireland") |>
  rename(lad_name = Geography) |>
  left_join(lookup_northern_ireland_ltla_hsct) |>
  group_by(trust_name) |>
  summarise(across(-lad_name, sum)) |>
  mutate(
    across(White:`Other ethnicities`,
      ~ (. / `All usual residents`) * 100,
      .names = "percent_{.col}"
    )
  ) |>
  ungroup() |>
  select(-`All usual residents`) |>
  rename(
    area_name = trust_name,
    number_White = White,
    `number_Irish Traveller` = `Irish Traveller`,
    number_Roma = Roma,
    number_Indian = Indian,
    number_Chinese = Chinese,
    number_Filipino = Filipino,
    number_Pakistani = Pakistani,
    number_Arab = Arab,
    `number_Other Asian` = `Other Asian`,
    `number_Black African` = `Black African`,
    `number_Black Other` = `Black Other`,
    number_Mixed = Mixed,
    `number_Other ethnicities` = `Other ethnicities`
  ) |>
  pivot_longer(!area_name,
    names_to = c(".value", "variable"),
    names_sep = "_"
  ) |>
  mutate(percent = percent / 100)

# NISRA 2021 Census does not include high-level groupings
# Create group summaries as close as possible to ONS groupings:
# https://www.ons.gov.uk/peoplepopulationandcommunity/culturalidentity/ethnicity/bulletins/ethnicgroupenglandandwales/census2021
ethnicity_higher_level_groupings <- ethnicity_hsct |>
  mutate(
    variable =
      case_when(
        str_detect(variable, "Chinese|Indian|Filipino|Pakistani|Other Asian") ~ "Asian",
        str_detect(variable, "Black African|Black Other") ~ "Black",
        str_detect(variable, "Mixed") ~ "Mixed or Multiple \nethnic groups",
        str_detect(variable, "White|Irish Traveller|Roma") ~ "White",
        str_detect(variable, "Arab|Other ethnicities") ~ "Other ethnic \ngroup",
      )
  ) |>
  group_by(area_name, variable) |>
  summarise(
    number = sum(number),
    percent = sum(percent)
  ) |>
  ungroup()

joined <-
  bind_rows(
    age_sex_hsct,
    ethnicity_higher_level_groupings
  )

# ---- Normalise/scale ----
northern_ireland_hsct_demographics_scaled <-
  joined |>
  group_by(variable) |>
  mutate(scaled = positional_normalisation(percent)) |>
  ungroup()

# --- Add plot labels ----
northern_ireland_hsct_demographics <- northern_ireland_hsct_demographics_scaled |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Count: ", round(number),
      "<br>", "Percent ", round(percent * 100, 1), "%"
    )
  )

# ---- Export data ----
usethis::use_data(northern_ireland_hsct_demographics, overwrite = TRUE)
