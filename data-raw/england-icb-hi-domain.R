library(tidyverse)
library(geographr)
library(sf)
library(healthyr)
library(ggridges)

# --- Lookups ----
icb <- boundaries_icb22 |>
  st_drop_geometry() |>
  mutate(icb22_name = str_remove_all(icb22_name, "^NHS ")) |>
  mutate(icb22_name = str_remove_all(icb22_name, " Integrated Care Board$"))

# --- Healthy People / Health Outcomes Domain ----
england_icb_hi_outcomes <- england_health_index_ics |>
  filter(year == 2021) |>
  select(icb22_code, number = healthy_people_domain) |>
  mutate(variable = "Health Outcomes Score") |>
  relocate(variable, .after = icb22_code) |>
  left_join(icb) |>
  select(-icb22_code) |>
  rename(area_name = icb22_name) |>
  mutate(rank = round(rank(number))) |>
  relocate(area_name)

england_icb_hi_outcomes_mean <- england_icb_hi_outcomes |>
  summarise(number = mean(number)) |>
  mutate(area_name = "England Average", .before = number) |>
  mutate(variable = "Health Outcomes Score", .before = number)

england_icb_hi_outcomes <- bind_rows(
  england_icb_hi_outcomes, england_icb_hi_outcomes_mean
) |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Health Outcomes Score: ", round(number),
      "<br>", "Rank: ", rank
    )
  )

# --- Healthy Lives / Preventable Risk Factors ----
england_icb_hi_risk_factors <- england_health_index_ics |>
  filter(year == 2021) |>
  select(icb22_code, number = healthy_lives_domain) |>
  mutate(variable = "Preventable Risk Factors Score") |>
  relocate(variable, .after = icb22_code) |>
  left_join(icb) |>
  select(-icb22_code) |>
  rename(area_name = icb22_name) |>
  mutate(rank = round(rank(number))) |>
  relocate(area_name)

england_icb_hi_risk_factors_mean <- england_icb_hi_risk_factors |>
  summarise(number = mean(number)) |>
  mutate(area_name = "England Average", .before = number) |>
  mutate(variable = "Preventable Risk Factors Score", .before = number)

england_icb_hi_outcomes <- bind_rows(
  england_icb_hi_outcomes, england_icb_hi_risk_factors_mean
) |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Preventable Risk Factors Score: ", round(number),
      "<br>", "Rank: ", rank
    )
  )

# --- Healthy Places / Social Determinants of Health----
england_icb_hi_social_determinants <- england_health_index_ics |>
  filter(year == 2021) |>
  select(icb22_code, number = healthy_places_domain) |>
  mutate(variable = "Preventable Risk Factors Score") |>
  relocate(variable, .after = icb22_code) |>
  left_join(icb) |>
  select(-icb22_code) |>
  rename(area_name = icb22_name) |>
  mutate(rank = round(rank(number))) |>
  relocate(area_name)

england_icb_hi_social_determinants_mean <- england_icb_hi_social_determinants |>
  summarise(number = mean(number)) |>
  mutate(area_name = "England Average", .before = number) |>
  mutate(variable = "Preventable Risk Factors Score", .before = number)

england_icb_hi_social_determinants <- bind_rows(
  england_icb_hi_social_determinants, england_icb_hi_social_determinants_mean
) |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Social Determinants of Health Score: ", round(number),
      "<br>", "Rank: ", rank
    )
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

check_distribution(england_icb_hi_outcomes)
check_distribution(england_icb_hi_risk_factors)
check_distribution(england_icb_hi_social_determinants)

# ---- Save datasets ----
usethis::use_data(england_icb_hi_outcomes, overwrite = TRUE)
usethis::use_data(england_icb_hi_risk_factors, overwrite = TRUE)
usethis::use_data(england_icb_hi_social_determinants, overwrite = TRUE)
