library(tidyverse)
library(IMD)
library(geographr)
library(sf)
library(ggridges)
library(DEPAHRI)
library(demographr)
library(loneliness)
library(healthindexscotland)

ltla <- boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^S"))

lookup_iz_ltla <- lookup_dz11_iz11_ltla20 |>
  select(iz11_code, ltla21_code = ltla20_code) |>
  distinct()

lookup_dz_ltla <- lookup_dz11_iz11_ltla20 |>
  select(dz11_code, ltla21_code = ltla20_code) |>
  distinct()

population_dz <-
  population20_dz11 |>
  filter(sex == "All") |>
  select(dz11_code, total_population)

# ---- IMD score ----
# Higher extent = more deprived
# Higher rank (calculated here) = more deprived
imd <-
  imd2020_scotland_ltla24 |> 
  select(ltla21_code = ltla24_code, imd_score = Extent) |>
  mutate(number = rank(imd_score)) |>
  select(-imd_score) |>
  mutate(variable = "Deprivation", .after = ltla21_code) |>
  mutate(percent = NA, .after = number)

# ---- % Left-behind areas ----
# Higher number/percent = more left-behind
lba <-
  cni2022_scotland_iz11 |>
  left_join(lookup_iz_ltla) |>
  select(iz11_code, ltla21_code, lba = `Left Behind Area?`) |>
  group_by(ltla21_code) |>
  count(lba) |>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  filter(lba == TRUE) |>
  right_join(ltla) |>
  mutate(percent = replace_na(percent, 0)) |>
  mutate(n = replace_na(n, 0)) |>
  select(ltla21_code, number = n, percent) |>
  mutate(variable = "Left-behind areas", .after = ltla21_code)

# ---- DEPAHRI score - digital only ----
# Isolate digital access from DEPAHRI
# Data is at LSOA level: need to aggregate to LTLA level using calculate_extent
# Extent is the proportion of the local population that live in areas
# classified as among the most deprived (here at risk) in the higher geography
# Higher score = higher risk of exclusion
# Higher rank (calculated here) = higher risk of exclusion
deri_lsoa <-
  scotland_lsoa_depahri |>
  left_join(lookup_dz_ltla, by = c("lsoa11_code" = "dz11_code")) |>
  select(lsoa11_code, deri_score_national, ltla21_code) |>
  left_join(population_dz, by = c("lsoa11_code" = "dz11_code"))

deri <-
  calculate_extent(deri_lsoa,
    deri_score_national,
    ltla21_code,
    total_population,
    weight_high_scores = TRUE
  ) |>
  mutate(number = rank(extent)) |>
  select(-extent) |>
  mutate(
    variable = "Access to Healthcare - Digital",
    .after = ltla21_code
  ) |>
  mutate(percent = NA, .after = number)

# ---- DEPHARI - physical only ----
# Isolate physical access to healthcare from DEPAHRI
# Score made up of equal weighting of demography, deprivation and physical access
# Higher score = higher risk of exclusion
# Higher rank (calculated here) = higher risk of exclusion
physical_lsoa <-
  scotland_lsoa_depahri |>
  mutate(
    physical_score =
      demography_comp_national * 0.33 +
        deprivation_comp_national * 0.33 +
        health_access_comp_national * 0.33
  ) |>
  left_join(lookup_dz_ltla, by = c("lsoa11_code" = "dz11_code")) |>
  select(lsoa11_code, physical_score, ltla21_code) |>
  left_join(population_dz, by = c("lsoa11_code" = "dz11_code"))

physical_access <-
  calculate_extent(physical_lsoa,
    physical_score,
    ltla21_code,
    total_population,
    weight_high_scores = TRUE
  ) |>
  mutate(number = rank(extent)) |>
  select(-extent) |>
  mutate(
    variable = "Access to Healthcare - Physical",
    .after = ltla21_code
  ) |>
  mutate(percent = NA, .after = number)


# ---- Loneliness----
# Decile 1 = least lonely
loneliness <-
  scotland_clinical_loneliness_dz |>
  left_join(lookup_dz_ltla) |>
  select(dz11_code, ltla21_code, deciles) |>
  group_by(ltla21_code) |>
  mutate(
    number = sum(deciles %in% c(9, 10), na.rm = TRUE),
    percent = sum(deciles %in% c(9, 10), na.rm = TRUE) / n()
  ) |>
  summarise(
    percent = first(percent),
    number = first(number)
  ) |>
  mutate(variable = "Loneliness", .after = ltla21_code)

# ---- Health Index Score ----
# Taken from healthindexscotland package
health_index <- scotland_health_index |>
  select(ltla21_code = ltla24_code, number = health_inequalities_rank) |>
  mutate(percent = NA) |>
  mutate(variable = "Population health") |>
  relocate(variable, .after = ltla21_code)

# ---- Combine & reanme (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
  deri,
  physical_access,
  loneliness,
  health_index
) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

ltla_summary_metrics_scotland_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Deprivation" ~ scale_1_1(number),
      variable == "Left-behind areas" ~ scale_1_1(percent),
      variable == "Access to Healthcare - Digital" ~ scale_1_1(number),
      variable == "Access to Healthcare - Physical" ~ scale_1_1(number),
      variable == "Loneliness" ~ scale_1_1(percent),
      variable == "Population health" ~ scale_1_1(number),
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, health index, and DEPAHRI as currently higher = worse health
# For IMD and DEPHARI also flip ranks (so that worse = lower rank)
scotland_ltla_summary_metrics_polarised <- ltla_summary_metrics_scotland_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Deprivation" ~ scaled_1_1 * -1,
      variable == "Left-behind areas" ~ scaled_1_1 * -1,
      variable == "Access to Healthcare - Digital" ~ scaled_1_1 * -1,
      variable == "Access to Healthcare - Physical" ~ scaled_1_1 * -1,
      variable == "Loneliness" ~ scaled_1_1 * -1,
      TRUE ~ scaled_1_1
    )
  ) |>
  mutate(
    number = case_when(
      variable == "Deprivation"                     ~ ave(-number, variable, FUN = rank),
      variable == "Access to Healthcare - Digital"  ~ ave(-number, variable, FUN = rank),
      variable == "Access to Healthcare - Physical" ~ ave(-number, variable, FUN = rank),
      TRUE ~ number
    )
  )

# Check distributions
scotland_ltla_summary_metrics_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
scotland_ltla_summary_metrics <- scotland_ltla_summary_metrics_polarised |>
  mutate(
    label = case_when(
      variable == "Deprivation" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "IMD rank: ", round(number)
      ),
      variable == "Left-behind areas" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of left-behind Intermediate Zones in the Local Authority: ", round(number),
        "<br>", "Percentage of all left-behind Intermediate Zones in the Local Authority: ", round(percent * 100, 1), "%"
      ),
      variable == "Access to Healthcare - Digital" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Digital Access to Healthcare rank: ", round(number)
      ),
      variable == "Access to Healthcare - Physical" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Physical Access to Healthcare rank: ", round(number)
      ),
      variable == "Loneliness" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of Intermediate Zones in the Local Authority that are in the 20% most lonely nationally: ", round(number),
        "<br>", "Percentage of all Intermediate Zones in the Local Authority that are in the 20% most lonely nationally: ", round(percent * 100, 1), "%"
      ),
      variable == "Population health" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Population health score (higher = better health): ", round(number)
      )
    )
  )

usethis::use_data(scotland_ltla_summary_metrics, overwrite = TRUE)
