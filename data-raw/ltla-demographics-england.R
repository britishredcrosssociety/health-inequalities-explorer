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

population_unprocessed <-
  readxl::read_excel(
    population_file,
    sheet = "P03",
    range = "A8:AO383"
  )

# Finish processing
population_all_ages <-
  population_unprocessed |>
  select(-`Area name`, -`All persons`) |>
  rename(area_code = `Area code [note 2]`) |>
  pivot_longer(!area_code, names_to = "age", values_to = "population") |>
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

population_sub_groups <-
  population_all_ages |>
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
  ungroup() |>
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

population_totals <-
  population_sub_groups |>
  group_by(area_code, age) |>
  summarise(population = sum(population)) |>
  mutate(sex = "Total") |>
  relocate(sex, .after = "area_code") |>
  bind_rows(population_sub_groups) |>
  arrange(area_code, sex)

population_totals |>
  filter(area_code == "E06000002" | area_code == "E06000003" | area_code == "E06000004") |>
  ggplot(aes(x = population, y = age, fill = area_code)) +
  facet_wrap(vars(sex), strip.position = "top") +
  geom_col(position = "dodge", colour = "black", alpha = .7) +
  scale_fill_manual(
    values = c("#D0021B", "#40A22A", "#F1B13B")
  ) +
  labs(x = "Count", y = NULL) +
  theme_light() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    text = element_text(face = "bold", size = 15),
    strip.background = element_rect(fill = "#5C747A")
  )

# see comparisons-across-nations.R in ad-hoc for other demographic stats that
# could be included. Ad-hoc also contains code to scrape ethnicity data from NOMIS