library(tidyverse)
library(IMD)
library(geographr)
library(sf)
library(ggridges)
library(DEPAHRI)
library(demographr)
library(loneliness)

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
  imd_scotland_lad |>
  select(ltla21_code = lad_code, imd_score = Extent) |>
  mutate(number = rank(imd_score)) |>
  select(-imd_score) |>
  mutate(variable = "Index of Multiple \nDeprivation rank", .after = ltla21_code) |>
  mutate(percent = NA, .after = number)

# ---- % Left-behind areas ----
# Higher number/percent = more left-behind
lba <-
  cni_scotland_iz11 |>
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
    variable = "Digital Access to Healthcare",
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
    variable = "Physical Access to Healthcare",
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

# ---- Combine & reanme (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
  deri,
  physical_access,
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

ltla_summary_metrics_scotland_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Index of Multiple \nDeprivation rank" ~ scale_1_1(number),
      variable == "Left-behind areas" ~ scale_1_1(percent),
      variable == "Digital Access to Healthcare" ~ scale_1_1(number),
      variable == "Physical Access to Healthcare" ~ scale_1_1(number),
      variable == "Loneliness" ~ scale_1_1(percent)
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, health index, and DEPAHRI as currently higher = worse health
scotland_ltla_summary_metrics_polarised <- ltla_summary_metrics_scotland_scaled |>
  mutate(scaled_1_1 = scaled_1_1 * -1)

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
      variable == "Index of Multiple \nDeprivation rank" ~ paste0(
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
      variable == "Digital Access to Healthcare" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Digital Access to Healthcare rank: ", round(number)
      ),
      variable == "Physical Access to Healthcare" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Physical Access to Healthcare rank: ", round(number)
      ),
      variable == "Loneliness" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of Intermediate Zones in the Local Authority that are in the 20% most lonely nationally: ", round(number),
        "<br>", "Percentage of all Intermediate Zones in the Local Authority that are in the 20% most lonely nationally: ", round(percent * 100, 1), "%"
      )
    )
  )

usethis::use_data(scotland_ltla_summary_metrics, overwrite = TRUE)
