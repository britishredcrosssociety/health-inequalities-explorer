library(tidyverse)
library(IMD)
library(geographr)
library(sf)
library(ggridges)
library(stringr)
library(DEPAHRI)
library(demographr)
library(loneliness)

ltla <- boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^W")) |>
  select(ltla21_code, ltla21_name)

lookup_msoa_ltla <- lookup_msoa11_ltla21 |>
  select(msoa11_code, ltla21_code) |>
  filter(str_starts(ltla21_code, "W"))

lookup_lsoa_ltla <-
  lookup_lsoa11_lsoa21_ltla22 |>
  distinct(lsoa11_code, ltla22_code) |>
  filter(str_detect(ltla22_code, "^W"))

population_lsoa <-
  population20_lsoa11 |>
  select(lsoa11_code, total_population) |>
  filter(str_detect(lsoa11_code, "^W"))

# ---- IMD score ----
# Higher extent = more deprived /
# Higher rank (calculated here) = more deprived
imd <-
  imd_wales_lad |>
  select(ltla21_code = lad_code, imd_score = Extent) |>
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
  cni_wales_msoa11 |>
  left_join(lookup_msoa_ltla) |>
  select(ltla21_code, lba = `Left Behind Area?`) |>
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

# ---- DEPAHRI score ----
# Data is at LSOA level: need to aggregate to LTLA level using calculate_extent
# Extent is the proportion of the local population that live in areas
# classified as among the most deprived (here at risk) in the higher geography
# Higher score = higher risk of exclusion
# Higher rank (calculated here) = higher risk of exclusion
depahri_lsoa <-
  wales_lsoa_depahri |>
  left_join(lookup_lsoa_ltla) |>
  distinct(lsoa11_code, depahri_score_national, ltla21_code = ltla22_code) |>
  left_join(population_lsoa)

depahri <-
  calculate_extent(depahri_lsoa,
    depahri_score_national,
    ltla21_code,
    total_population,
    weight_high_scores = TRUE
  ) |>
  mutate(number = rank(extent)) |>
  select(-extent) |>
  mutate(
    variable = "Access to Healthcare \n (Physical and Digital)",
    .after = ltla21_code
  ) |>
  mutate(percent = NA, .after = number)

# ---- Loneliness score ----
# Decile 1 - least lonely
# Calculate % of LSOAs in decile 1 per LHB

# Convert LSOA'21 to LSOA'11 codes
# Aggregation strategy going from 2021 codes to 2011 codes:
# - change_code == "U": no action required
# - change_code == "S": take the average score of the 2021 LSOA
# - change_code == "M": 2011 LSOA inherits the score of the 2021 LSOA
# - change_code == "X": 2011 LSOA inherits the score of 2021 LSOA.
#   then group by 2011 LSOA
lsoa_lsoa <- lookup_lsoa11_lsoa21_ltla22 |>
  filter(str_detect(lsoa21_code, "^W")) |>
  distinct(lsoa11_code, lsoa21_code, change_code) |>
  relocate(change_code, .after = lsoa21_code)

aggregate_loneliness_lsoas <- function(data) {
  data_u <- data |>
    left_join(lsoa_lsoa) |>
    filter(change_code == "U") |>
    select(lsoa11_code, deciles)

  data_s <- data |>
    left_join(lsoa_lsoa) |>
    filter(change_code == "S") |>
    group_by(lsoa11_code) |>
    summarize(deciles = mean(deciles, na.rm = TRUE), ) |>
    ungroup()

  data_m <- data |>
    left_join(lsoa_lsoa) |>
    relocate(lsoa11_code) |>
    filter(change_code == "M") |>
    select(lsoa11_code, deciles)

  data_x <- data |>
    left_join(lsoa_lsoa) |>
    filter(change_code == "X") |>
    group_by(lsoa11_code) |>
    summarize(deciles = mean(deciles, na.rm = TRUE), ) |>
    ungroup()

  data_aggregated <- bind_rows(data_u, data_s, data_m, data_x)

  data_aggregated
}

loneliness <-
  aggregate_loneliness_lsoas(wales_clinical_loneliness_lsoa) |>
  left_join(lookup_lsoa_ltla) |>
  select(ltla21_code = ltla22_code, lsoa11_code, deciles) |>
  group_by(ltla21_code) |>
  mutate(
    number = sum(deciles  %in% c(9, 10), na.rm = TRUE),
    percent = sum(deciles %in% c(9, 10), na.rm = TRUE) / n()
  ) |>
  summarise(
    number = first(number),
    percent = first(percent)
  ) |>
  mutate(variable = "Loneliness", .after = ltla21_code)

# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
  depahri,
  loneliness
) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name)

# ---- Normalise/scale ---- quotient transformation (x/sum)
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

ltla_summary_metrics_wales_scaled <- metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Index of Multiple \nDeprivation rank" ~ scale_1_1(number),
      variable == "Left-behind areas" ~ scale_1_1(percent),
      variable == "Access to Healthcare \n (Physical and Digital)" ~ scale_1_1(number),
      variable == "Loneliness" ~ scale_1_1(percent)
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, health index, DEPAHRI, loneliness as currently higher = worse health
wales_ltla_summary_metrics_polarised <-
  ltla_summary_metrics_wales_scaled |>
  mutate(scaled_1_1 = scaled_1_1 * -1)

# Check distributions
wales_ltla_summary_metrics_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
wales_ltla_summary_metrics <- wales_ltla_summary_metrics_polarised |>
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
        "<br>", "No. of left-behind smaller areas (MSOA's) in the Local Authority: ", round(number),
        "<br>", "Percentage of all left-behind smaller areas (MSOA's) in the Local Authority: ", round(percent * 100, 1), "%"
      ),
      variable == "Access to Healthcare \n (Physical and Digital)" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "DEPAHRI rank: ", round(number)
      ),
      variable == "Loneliness" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of LSOAs in the Local Authority that are in the 20% most lonely nationally: ", round(number),
        "<br>", "Percentage of all LSOAs in the Local Authority that are in the 20% most lonely nationally: ", round(percent * 100, 1), "%"
      )
    )
  )

usethis::use_data(wales_ltla_summary_metrics, overwrite = TRUE)
