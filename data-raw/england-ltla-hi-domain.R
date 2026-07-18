library(tidyverse)
library(geographr)
library(sf)
library(healthyr)
library(ggridges)

# --- Lookups ----
ltla <-
  boundaries_ltla24 |>
  st_drop_geometry() |>
  filter(str_detect(ltla24_code, "^E"))

lookup_ltla21_ltla24 <-
  lookup_ltla_ltla |>
  filter(str_detect(ltla21_code, "^E")) |>
  distinct(ltla21_code, ltla24_code)

# --- Healthy People / Health Outcomes Domain ----
england_ltla_hi_outcomes <- england_health_index |>
  filter(year == 2021) |>
  select(ltla21_code, number = healthy_people_domain_score) |>

  # Recast to LTLA 2024 codes using mean scores where there are more than one 2021 LA for one 2024 LA
  left_join(lookup_ltla21_ltla24) |>
  group_by(ltla24_code) |>
  summarise(number = mean(number)) |>
  ungroup() |>

  mutate(variable = "Health Outcomes Score") |>
  relocate(variable, .after = ltla24_code) |>
  left_join(ltla) |>
  select(-ltla24_code) |>
  rename(area_name = ltla24_name)

england_ltla_hi_outcomes <- england_ltla_hi_outcomes |>
  bind_rows(data.frame(
    number = mean(as.numeric(england_ltla_hi_outcomes$number)),
    area_name = "England Average"
  )) |>
  mutate(rank = round(rank(number))) |>
  relocate(area_name) |>
  mutate(
    label = paste0(
      "<b>",
      area_name,
      "</b>",
      "<br>",
      "<br>",
      "Health Outcomes Score: ",
      round(number),
      "<br>",
      "Rank: ",
      rank
    ),
  )

# --- Healthy Lives / Preventable Risk Factors ----
england_ltla_hi_risk_factors <- england_health_index |>
  filter(year == 2021) |>
  select(ltla21_code, number = healthy_lives_domain_score) |>

  # Recast to LTLA 2024 codes using mean scores where there are more than one 2021 LA for one 2024 LA
  left_join(lookup_ltla21_ltla24) |>
  group_by(ltla24_code) |>
  summarise(number = mean(number)) |>
  ungroup() |>

  mutate(variable = "Preventable Risk Factors Score") |>
  relocate(variable, .after = ltla24_code) |>
  left_join(ltla) |>
  select(-ltla24_code) |>
  rename(area_name = ltla24_name)

england_ltla_hi_risk_factors <- england_ltla_hi_risk_factors |>
  bind_rows(data.frame(
    number = mean(as.numeric(england_ltla_hi_risk_factors$number)),
    area_name = "England Average"
  )) |>
  mutate(rank = round(rank(number))) |>
  relocate(area_name) |>
  mutate(
    label = paste0(
      "<b>",
      area_name,
      "</b>",
      "<br>",
      "<br>",
      "Preventable Risk Factors Score: ",
      round(number),
      "<br>",
      "Rank: ",
      rank
    ),
  )

# --- Healthy Places / Social Determinants of Health----
england_ltla_hi_social_determinants <- england_health_index |>
  filter(year == 2021) |>
  select(ltla21_code, number = healthy_places_domain_score) |>

  # Recast to LTLA 2024 codes using mean scores where there are more than one 2021 LA for one 2024 LA
  left_join(lookup_ltla21_ltla24) |>
  group_by(ltla24_code) |>
  summarise(number = mean(number)) |>
  ungroup() |>

  mutate(variable = "Social Determinants of Health Score") |>
  relocate(variable, .after = ltla24_code) |>
  left_join(ltla) |>
  select(-ltla24_code) |>
  rename(area_name = ltla24_name)

england_ltla_hi_social_determinants <-
  england_ltla_hi_social_determinants |>
  bind_rows(data.frame(
    number = mean(as.numeric(england_ltla_hi_social_determinants$number)),
    area_name = "England Average"
  )) |>
  mutate(rank = round(rank(number))) |>
  relocate(area_name) |>
  mutate(
    label = paste0(
      "<b>",
      area_name,
      "</b>",
      "<br>",
      "<br>",
      "Social Determinants of Health Score: ",
      round(number),
      "<br>",
      "Rank: ",
      rank
    ),
  )


# ---- Check distributions ----
check_distribution <- function(data) {
  data |>
    ggplot(aes(x = number, y = variable)) +
    geom_density_ridges(scale = 4) +
    scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
    scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
    coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
    theme_ridges()
}

check_distribution(england_ltla_hi_outcomes)
check_distribution(england_ltla_hi_risk_factors)
check_distribution(england_ltla_hi_social_determinants)

# ---- Save datasets ----
usethis::use_data(england_ltla_hi_outcomes, overwrite = TRUE)
usethis::use_data(england_ltla_hi_risk_factors, overwrite = TRUE)
usethis::use_data(england_ltla_hi_social_determinants, overwrite = TRUE)
