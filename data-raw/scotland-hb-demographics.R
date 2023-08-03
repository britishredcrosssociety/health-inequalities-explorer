library(tidyverse)
library(readxl)
library(demographr)
library(compositr)
library(geographr)
library(sf)
library(ggridges)

hb <- boundaries_hb19 |>
  st_drop_geometry()

# ---- Population ----
# Source: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2021
url <- "https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-21/mid-year-pop-est-21-data.xlsx"

population_file <- download_file(url, ".xlsx")

population_raw <- read_excel(
  population_file,
  sheet = "Table 1",
  range = "B4:CR145"
)

population_raw

population_groupings <- population_raw |>
  filter(`Area type` == "Health board") |>
  filter(Sex != "Persons") |>
  rowwise() |>
  mutate(
    age_18_less = sum(c_across(`0`:`17`)),
    age_18_65 = sum(c_across(`18`:`65`)),
    age_65_plus = sum(c_across(`66`:`90+`))
  ) |>
  ungroup() |>
  select(matches("^[a-z]"))

population_age_sex_joined <- population_groupings |>
  pivot_longer(cols = starts_with("age"), values_to = "population") |>
  mutate(
    age = case_when(
      Sex == "Females" & name == "age_18_less" ~ "Younger \nfemales (< 18)",
      Sex == "Males" & name == "age_18_less" ~ "Younger \nmales (< 18)",
      Sex == "Females" & name == "age_18_65" ~ "Working age \nfemales (18-65)",
      Sex == "Males" & name == "age_18_65" ~ "Working age \nmales (18-65)",
      Sex == "Females" & name == "age_65_plus" ~ "Older \nfemales (65+)",
      Sex == "Males" & name == "age_65_plus" ~ "Older \nmales (65+)"
    )
  )

population_relative <- population_age_sex_joined |>
  mutate(population_relative = population / `All ages`)

population_scotland <- population_relative |>
  left_join(hb, by = c("Area code" = "hb19_code")) |>
  select(
    area_name = hb19_name,
    variable = age,
    number = population,
    percent = population_relative
  )

# ---- Join ----
joined <- population_scotland

# ---- Normalise/scale ----
scotland_hb_demographics_scaled <- joined |>
  group_by(variable) |>
  mutate(scaled = positional_normalisation(percent)) |>
  ungroup()

# Check distributions
scotland_hb_demographics_scaled |>
  ggplot(aes(x = scaled, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

# ---- Add plot labels ----
scotland_hb_demographics <- scotland_hb_demographics_scaled |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Count: ", round(number),
      "<br>", "Percent ", round(percent * 100, 1), "%"
    )
  )

# ---- Export data ----
usethis::use_data(scotland_hb_demographics, overwrite = TRUE)
