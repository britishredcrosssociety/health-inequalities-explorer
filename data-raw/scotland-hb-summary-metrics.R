library(tidyverse)
library(IMD)
library(geographr)
library(sf)
library(ggridges)
library(DEPAHRI)
library(demographr)
library(loneliness)

hb <- boundaries_hb19 |>
  st_drop_geometry()

lookup_dz_hb <- lookup_dz11_ltla19_hb19 |>
  distinct(dz11_code, hb19_code)

lookup_iz_hb <- lookup_dz11_iz11_ltla20 |>
  left_join(lookup_dz11_ltla19_hb19) |>
  distinct(iz11_code, hb19_code)

lookup_ltla_hb <- lookup_dz11_ltla19_hb19 |>
  distinct(ltla19_code, hb19_code)

population_dz <-
  population20_dz11 |>
  filter(sex == "All") |>
  select(dz11_code, total_population)

# ---- IMD ----
# Decile 1 = most deprived
imd <- imd_scotland_dz |>
  select(dz11_code = dz_code, decile = IMD_decile) |>
  left_join(lookup_dz_hb) |>
  mutate(most_deprived = if_else(decile == 1, "yes", "no")) |>
  group_by(hb19_code, most_deprived) |>
  summarise(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(total_number_dzs = sum(n)) |>
  ungroup() |>
  filter(most_deprived == "no") |> # Not all HBs have top 10% most vulnerable DZs
  mutate(number = total_number_dzs - n) |>
  mutate(percent = 1 - freq) |>
  mutate(variable = "Deprivation", .after = hb19_code) |>
  select(-most_deprived, -n, -freq, -total_number_dzs)

# ---- % Left-behind areas ----
# Higher number/percent = more left-behind
lba <-
  cni_scotland_iz11 |>
  left_join(lookup_iz_hb) |>
  select(iz11_code, hb19_code, lba = `Left Behind Area?`) |>
  group_by(hb19_code) |>
  count(lba) |>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  filter(lba == TRUE) |>
  right_join(hb) |>
  mutate(percent = replace_na(percent, 0)) |>
  mutate(n = replace_na(n, 0)) |>
  select(hb19_code, number = n, percent) |>
  mutate(variable = "Left-behind areas", .after = hb19_code)

# ---- DEPAHRI score - digital only ----
# Isolate digital access from DEPAHRI
# Extent is the proportion of the local population that live in areas
# classified as among the most deprived (here at risk) in the higher geography
# Higher score = higher risk of exclusion
# Higher rank (calculated here) = higher risk of exclusion
deri_lsoa <-
  scotland_lsoa_depahri |>
  left_join(lookup_dz_hb, by = c("lsoa11_code" = "dz11_code")) |>
  select(lsoa11_code, deri_score_national, hb19_code) |>
  left_join(population_dz, by = c("lsoa11_code" = "dz11_code"))

deri <-
  calculate_extent(deri_lsoa,
    deri_score_national,
    hb19_code,
    total_population,
    weight_high_scores = TRUE
  ) |>
  mutate(number = rank(extent)) |>
  select(-extent) |>
  mutate(
    variable = "Digital Access to Healthcare",
    .after = hb19_code
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
  left_join(lookup_dz_hb, by = c("lsoa11_code" = "dz11_code")) |>
  select(lsoa11_code, physical_score, hb19_code) |>
  left_join(population_dz, by = c("lsoa11_code" = "dz11_code"))

physical_access <-
  calculate_extent(physical_lsoa,
    physical_score,
    hb19_code,
    total_population,
    weight_high_scores = TRUE
  ) |>
  mutate(number = rank(extent)) |>
  select(-extent) |>
  mutate(
    variable = "Physical Access to Healthcare",
    .after = hb19_code
  ) |>
  mutate(percent = NA, .after = number)

# ---- Loneliness  ----
# Decile 1 = least lonely
# Calculate % of dzs in decile 1 per hb
loneliness <-
  scotland_clinical_loneliness_dz |>
  left_join(lookup_dz_hb) |>
  select(dz11_code, hb19_code, deciles) |>
  group_by(hb19_code) |>
  mutate(
    number = sum(deciles %in% c(9, 10), na.rm = TRUE),
    percent = sum(deciles %in% c(9, 10), na.rm = TRUE) / n()
  ) |>
  summarise(
    percent = first(percent),
    number = first(number)
  ) |>
  mutate(variable = "Loneliness", .after = hb19_code)


# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
  deri,
  physical_access,
  loneliness
) |>
  left_join(hb) |>
  select(-hb19_code) |>
  rename(area_name = hb19_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

hb_summary_metrics_scotland_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Deprivation" ~ scale_1_1(percent),
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
scotland_hb_summary_metrics_polarised <- hb_summary_metrics_scotland_scaled |>
  mutate(scaled_1_1 = scaled_1_1 * -1)

# Check distributions
scotland_hb_summary_metrics_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
scotland_hb_summary_metrics <- scotland_hb_summary_metrics_polarised |>
  mutate(
    label = case_when(
      variable == "Deprivation" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of Intermediate Zones in the Health Board that are in the 10% most deprived nationally: ", round(number),
        "<br>", "Percentage of all Intermediate Zones in the Health Board that are in the 10% most deprived nationally: ", round(percent * 100, 1), "%"
      ),
      variable == "Left-behind areas" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of left-behind Intermediate Zones in the Health Board: ", round(number),
        "<br>", "Percentage of left-behind Intermediate Zones in the Health Board: ", round(percent * 100, 1), "%"
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
        "<br>", "No. of Intermediate Zones in the Health Board that are in the 20% most lonely nationally: ", round(number),
        "<br>", "Percentage of all Intermediate Zones in the Health Board that are in the 20% most lonely nationally: ", round(percent * 100, 1), "%"
      )
    )
  )

usethis::use_data(scotland_hb_summary_metrics, overwrite = TRUE)
