library(tidyverse)
library(geographr)
library(healthyr)
library(compositr)
library(sf)
library(IMD)
library(readxl)
library(ggridges)
library(DEPAHRI)
library(demographr)
library(loneliness)

ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^E"))

lookup_england_ltla <-
  lookup_ltla_ltla |>
  filter(str_detect(ltla21_code, "^E"))

lookup_england_lsoa_ltla <-
  lookup_lsoa11_lsoa21_ltla22 |>
  distinct(lsoa11_code, lsoa21_code, ltla22_code) |>
  filter(str_detect(ltla22_code, "^E"))

population_lsoa <-
  population20_lsoa11 |> 
  select(lsoa11_code, total_population) |> 
  filter(str_detect(lsoa11_code, "^E"))

# ---- IMD score ----
# Higher score = more deprived
# Higher rank (calculated here) = more deprived
imd <-
  imd_england_lad |>
  select(ltla19_code = lad_code, imd_score = Score) |>
  left_join(lookup_england_ltla, by = "ltla19_code") |>
  select(ltla21_code, imd_score) |>
  group_by(ltla21_code) |>
  summarise(imd_score = mean(imd_score)) |>
  mutate(number = rank(imd_score)) |>
  select(-imd_score) |>
  mutate(
    variable = "Index of Multiple \nDeprivation rank",
    .after = ltla21_code
  ) |>
  mutate(percent = NA, .after = number)

# ---- % Left-behind areas ----
# Higher number/percent = more left-behind
lba <-
  cni2023_england_lsoa21 |>
  left_join(lookup_england_lsoa_ltla) |>
  select(lsoa21_code, ltla22_code, lba = `Left Behind Area?`) |>
  group_by(ltla22_code) |>
  count(lba) |>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  filter(lba == TRUE) |>
  right_join(ltla, by = c("ltla22_code" = "ltla21_code")) |>
  mutate(percent = replace_na(percent, 0)) |>
  mutate(n = replace_na(n, 0)) |>
  select(ltla21_code = ltla22_code, number = n, percent) |>
  mutate(variable = "Left-behind areas", .after = ltla21_code)

# ---- ONS Health Index score ----
# Higher score = better health
# Higher rank (calculated here) = better health
health_index_2021 <- england_health_index |>
  filter(year == "2021") |>
  select(ltla21_code, health_index_score = overall_score)

# Data is missing for two ltla's
#   - Map Iscles of Scilly to Cornwall
#   - Map City of London to Hackney
cornwall_score <-
  health_index_2021 |>
  filter(ltla21_code == "E06000052") |>
  pull(health_index_score)

hackney_score <-
  health_index_2021 |>
  filter(ltla21_code == "E09000012") |>
  pull(health_index_score)

health_index_missing_added <-
  health_index_2021 |>
  add_row(ltla21_code = "E06000053", health_index_score = cornwall_score) |>
  add_row(ltla21_code = "E09000001", health_index_score = hackney_score)

health_index <-
  health_index_missing_added |>
  mutate(number = rank(health_index_score)) |>
  mutate(percent = NA) |>
  mutate(variable = "ONS Health \nIndex rank", .after = ltla21_code) |>
  select(-health_index_score)

# ---- DEPAHRI score ----
# Data is at LSOA level: need to aggregate to LTLA level using calculate_extent
# Extent is the proportion of the local population that live in areas 
# classified as among the most deprived (here at risk) in the higher geography
# Higher score = higher risk of exclusion
# Higher rank (calculated here) = higher risk of exclusion
depahri_lsoa <- 
  england_lsoa_depahri |> 
  left_join(lookup_england_lsoa_ltla) |> 
  distinct(lsoa11_code, depahri_score_national, ltla21_code = ltla22_code) |> 
  left_join(population_lsoa)

depahri <-
  calculate_extent(depahri_lsoa, 
                   depahri_score_national, 
                   ltla21_code, 
                   total_population, 
                   weight_high_scores = TRUE) |> 
  mutate(number = rank(extent))|>
  select(-extent) |>
  mutate(
    variable = "Access to Healthcare \n (Physical and Digital)",
    .after = ltla21_code
  ) |>
  mutate(percent = NA, .after = number)

# ---- Loneliness score ----
# Higher percentage / number = more lonely
# Used mean weighted by population size to aggregate to ltla from lsoa

loneliness_lsoa <- 
  england_cls_loneliness_lsoa |> 
  left_join(lookup_england_lsoa_ltla, by = "lsoa21_code") |> 
  distinct(lsoa11_code, perc, ltla21_code = ltla22_code) |> 
  left_join(population_lsoa, by ="lsoa11_code")

loneliness <- loneliness_lsoa |>
  group_by(ltla21_code) |>
  summarise(percent = weighted.mean(perc, w = total_population, na.rm = TRUE)) |>
    mutate(
      variable = "Loneliness",
      .after = ltla21_code
    ) |>
  mutate(percent = percent/100) |>
    mutate(number = NA, .before = percent)

# loneliness <-
#   calculate_extent(loneliness_lsoa,
#                    perc,
#                    ltla21_code,
#                    total_population,
#                    weight_high_scores = TRUE) |>
#   rename(percent = extent)|>
#   mutate(
#     variable = "Loneliness",
#     .after = ltla21_code
#   ) |>
#   mutate(number = NA, .before = percent)

# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
  health_index,
  depahri,
  loneliness
) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

ltla_summary_metrics_england_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Index of Multiple \nDeprivation rank" ~ scale_1_1(number),
      variable == "Left-behind areas" ~ scale_1_1(percent),
      variable == "ONS Health \nIndex rank" ~ scale_1_1(number),
      variable == "Access to Healthcare \n (Physical and Digital)" ~ scale_1_1(number),
      variable == "Loneliness" ~ scale_1_1(percent),
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, and DEPAHRI as currently higher = worse health
england_ltla_summary_metrics_polarised <- ltla_summary_metrics_england_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Index of Multiple \nDeprivation rank" ~ scaled_1_1 * -1,
      variable == "Left-behind areas" ~ scaled_1_1 * -1,
      variable == "Access to Healthcare \n (Physical and Digital)" ~ scaled_1_1 * -1,
      variable == "Loneliness" ~ scaled_1_1 * -1,
      TRUE ~ scaled_1_1
    )
  )

# Check distributions
england_ltla_summary_metrics_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
england_ltla_summary_metrics <- england_ltla_summary_metrics_polarised |>
  mutate(
    label = case_when(
      variable == "Index of Multiple \nDeprivation rank" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "IMD rank: ", round(number)
      ),
      variable == "Left-behind areas" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of left-behind LSOA's in the area: ", round(number),
        "<br>", "Percentage of all LSOA's that are left-behind: ", round(percent * 100, 1), "%"
      ),
      variable == "ONS Health \nIndex rank" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Health Index rank: ", round(number)
      ),
      variable == "Access to Healthcare \n (Physical and Digital)" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "DEPAHRI rank: ", round(number)
      ),
      variable == "Loneliness" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Percentage of people who 'often', 'always' or 'some of the time' feel lonely: ", round(percent*100), "%"
      )
    )
  )

usethis::use_data(england_ltla_summary_metrics, overwrite = TRUE)
