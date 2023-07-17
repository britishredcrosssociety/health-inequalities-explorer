library(tidyverse)
library(demographr)
library(readxl)

# ---- Age & Sex ----
# Note: term 'sex' is used to correspond to the 2021 Census from which the data
# has been collected 

age_sex_ltla <-
  age_gender_ltla21_ni |>
  select(-ltla21_code, -total_female_population, -total_male_population) |> 
  mutate(
    across(younger_females:older_males,
           ~ (. / total_population) * 100, 
           .names = "percent__{.col}")
  ) |> 
  select(-total_population) |> 
  rename(area_name = ltla21_name,
         number__younger_females = younger_females,
         number__working_age_females = working_age_females,
         number__older_females = older_females,
         number__younger_males = younger_males,
         number__working_age_males = working_age_males,
         number__older_males = older_males) |> 
  pivot_longer(!area_name,
               names_to = c(".value", "variable"), 
               names_sep = "__") |> 
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
ethnicity_ltla <-
  ethnicity21_ltla21_ni
