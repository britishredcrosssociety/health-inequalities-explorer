library(tidyverse)
library(demographr)
library(compositr)

# ---- Age & Sex ----
# Note: term 'sex' is used to correspond to the 2021 Census from which the data
# has been collected
age_sex_ltla <-
  age_gender_ltla21_ni |>
  select(-ltla21_code, -total_female_population, -total_male_population) |>
  mutate(
    across(younger_females:older_males,
      ~ (. / total_population),
      .names = "percent__{.col}"
    )
  ) |>
  select(-total_population) |>
  rename(
    area_name = ltla21_name,
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
      variable == "working_age_females" ~ "Working age \nfemales (20-64)",
      variable == "younger_females" ~ "Younger \nfemales (< 20)",
      variable == "older_males" ~ "Older \nmales (65+)",
      variable == "working_age_males" ~ "Working age \nmales (20-64)",
      variable == "younger_males" ~ "Younger \nmales (< 20)"
    )
  )

# ---- Ethnicity ----
# NISRA 2021 Census does not include high-level groupings
# Create group summaries as close as possible to ONS groupings:
# https://www.ons.gov.uk/peoplepopulationandcommunity/culturalidentity/ethnicity/bulletins/ethnicgroupenglandandwales/census2021
ethnicity_higher_level_groupings <-
  ethnicity21_ltla21_ni |>
  mutate(
    high_level_category =
      case_when(
        str_detect(ethnic_group, "Asian.*\\(number\\)$") ~ "Asian: number",
        str_detect(ethnic_group, "Asian.*\\(percent\\)$") ~ "Asian: percent",
        str_detect(ethnic_group, "Black.*\\(number\\)$") ~ "Black: number",
        str_detect(ethnic_group, "Black.*\\(percent\\)$") ~ "Black: percent",
        str_detect(ethnic_group, "Mixed.*\\(number\\)$") ~ "Mixed or Multiple \nethnic groups: number",
        str_detect(ethnic_group, "Mixed.*\\(percent\\)$") ~ "Mixed or Multiple \nethnic groups: percent",
        str_detect(ethnic_group, "White.*\\(number\\)$") ~ "White: number",
        str_detect(ethnic_group, "White.*\\(percent\\)$") ~ "White: percent",
        str_detect(ethnic_group, "^Other.*\\(number\\)$") ~ "Other ethnic \ngroup: number",
        str_detect(ethnic_group, "^Other.*\\(percent\\)$") ~ "Other ethnic \ngroup: percent",
        str_detect(ethnic_group, "Arab.*\\(number\\)$") ~ "Other ethnic \ngroup: number",
        str_detect(ethnic_group, "Arab.*\\(percent\\)$") ~ "Other ethnic \ngroup: percent",
      )
  ) |>
  group_by(ltla21_name, high_level_category) |>
  summarise(value = sum(value)) |>
  ungroup()

# Separate into number/percent cols
ethnicity_separate_cols <- ethnicity_higher_level_groupings |>
  separate(high_level_category, into = c("indicator", "value_type"), ": ") |>
  pivot_wider(names_from = "value_type", values_from = "value")

# Select cols
ethnicity_ltla <- ethnicity_separate_cols |>
  select(
    area_name = ltla21_name,
    variable = indicator,
    number,
    percent
  )

joined <-
  bind_rows(
    age_sex_ltla,
    ethnicity_ltla
  )

# ---- Normalise/scale ----
northern_ireland_ltla_demographics_scaled <-
  joined |>
  group_by(variable) |>
  mutate(scaled = positional_normalisation(percent)) |>
  ungroup()

# --- Add plot labels ----
northern_ireland_ltla_demographics <-
  northern_ireland_ltla_demographics_scaled |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Count: ", round(number),
      "<br>", "Percent ", round(percent * 100, 1), "%"
    )
  )

# ---- Export data ----
usethis::use_data(northern_ireland_ltla_demographics, overwrite = TRUE)
