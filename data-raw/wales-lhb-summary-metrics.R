library(tidyverse)
library(IMD)
library(geographr)
library(sf)
library(ggridges)
library(DEPAHRI)
library(demographr)
library(loneliness)

lhb <- boundaries_lhb20 |>
  st_drop_geometry()

lookup_lsoa11_ltla21_lhb22 <- lookup_lsoa11_ltla21 |>
  filter(str_detect(ltla21_code, "^W")) |>
  left_join(lookup_ltla21_lhb22) |>
  distinct(lsoa11_code, lhb22_code, lhb22_name, ltla21_code) |>
  select(lsoa11_code, lhb20_code = lhb22_code, ltla21_code)

population_lsoa <-
  population20_lsoa11 |>
  select(lsoa11_code, total_population) |>
  filter(str_detect(lsoa11_code, "^W"))

# ---- IMD score ----
# Decile 1 = most deprived
# Higher percentage / number = worse health
imd <-
  imd_wales_lsoa |>
  left_join(lookup_lsoa11_ltla21_lhb22, by = c("lsoa_code" = "lsoa11_code")) |>
  select(lsoa_code, lhb20_code, IMD_decile) |>
  mutate(top_10 = if_else(IMD_decile == 1, "yes", "no")) |>
  group_by(lhb20_code, top_10) |>
  summarise(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(total_number_lsoas = sum(n)) |>
  ungroup() |>
  filter(top_10 == "no") |>
  mutate(number = total_number_lsoas - n) |>
  mutate(percent = 1 - freq) |>
  mutate(variable = "Deprivation", .after = lhb20_code) |>
  select(-top_10, -n, -freq, -total_number_lsoas)

# ---- health ----
# An official Health Index for Scotland does not exists. Use the BRC Resilience
# Index version
# Source: https://github.com/britishredcrosssociety/resilience-index

# Higher score = worse health
# Higher rank (calculated here) = worse health
url <- "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/wales/healthy-people-domain.csv"

resilience_index_raw <- read_csv(url)

health_index <- resilience_index_raw |>
  rename(ltla21_code = lad_code) |>
  left_join(lookup_ltla21_lhb22) |>
  select(lhb22_code, score = healthy_people_domain_score) |>
  group_by(lhb22_code) |>
  summarise(score = sum(score)) |>
  mutate(number = rank(score)) |>
  mutate(percent = NA) |>
  mutate(variable = "Health Index \nrank") |>
  select(lhb20_code = lhb22_code, variable, number, percent)

# ---- % Left-behind areas ----
lba <-
  cni_wales_msoa11 |>
  select(ltla21_name = lad21_name, lba = `Left Behind Area?`) |>
  left_join(lookup_ltla21_lhb22) |>
  group_by(lhb22_code) |>
  count(lba) |>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  rename(lhb20_code = lhb22_code) |>
  filter(lba == TRUE) |>
  right_join(lhb) |>
  mutate(percent = replace_na(percent, 0)) |>
  mutate(n = replace_na(n, 0)) |>
  select(lhb20_code, number = n, percent) |>
  mutate(variable = "Left-behind areas", .after = lhb20_code)

# ---- DEPAHRI score ----
# Data is at LSOA level: need to aggregate to LHB level using calculate_extent
# Extent is the proportion of the local population that live in areas
# classified as among the most deprived (here at risk) in the higher geography
# Higher score = higher risk of exclusion
# Higher rank (calculated here) = higher risk of exclusion

# TO CHECK - THE TREND DOESN'T SEEM IN LINE WITH OTHER INDICATORS
depahri_lsoa <-
  wales_lsoa_depahri |>
  left_join(lookup_lsoa11_ltla21_lhb22) |>
  select(lsoa11_code, depahri_score_national, lhb20_code) |>
  left_join(population_lsoa)

depahri <-
  calculate_extent(depahri_lsoa,
    depahri_score_national,
    lhb20_code,
    total_population,
    weight_high_scores = TRUE
  ) |>
  mutate(number = rank(extent)) |>
  select(-extent) |>
  mutate(
    variable = "Access to Healthcare \n (Physical and Digital)",
    .after = lhb20_code
  ) |>
  mutate(percent = NA, .after = number)

# ---- Loneliness  ----
# Decile 1 = least lonely
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
  left_join(lookup_lsoa11_ltla21_lhb22) |>
  select(ltla21_code, lhb20_code, deciles) |>
  group_by(lhb20_code) |>
  mutate(
    number = sum(deciles == 10, na.rm = TRUE),
    percent = sum(deciles == 10, na.rm = TRUE) / n()
  ) |>
  summarise(
    number = first(number),
    percent = first(percent)
  ) |>
  mutate(variable = "Loneliness", .after = lhb20_code)

# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
  health_index,
  depahri,
  loneliness
) |>
  left_join(lhb) |>
  select(-lhb20_code) |>
  rename(area_name = lhb20_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

lhb_summary_metrics_wales_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Deprivation" ~ scale_1_1(percent),
      variable == "Left-behind areas" ~ scale_1_1(percent),
      variable == "Health Index \nrank" ~ scale_1_1(number),
      variable == "Access to Healthcare \n (Physical and Digital)" ~ scale_1_1(number),
      variable == "Loneliness" ~ scale_1_1(percent)
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, health index, and DEPAHRI, as currently higher = worse health
wales_lhb_summary_metrics_polarised <- lhb_summary_metrics_wales_scaled |>
  mutate(scaled_1_1 = scaled_1_1 * -1)

# Check distributions
wales_lhb_summary_metrics_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
wales_lhb_summary_metrics <- wales_lhb_summary_metrics_polarised |>
  mutate(
    label = case_when(
      variable == "Deprivation" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "The no. of LSOAs in the LHB that are in the top 10% most deprived nationally: ", round(number),
        "<br>", "Percentage of LSOAs in the LHB that are in the top 10% most deprived nationally: ", round(percent * 100, 1), "%"
      ),
      variable == "Left-behind areas" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of left-behind LSOAs in the LHB: ", round(number),
        "<br>", "Percentage of LSOAs in the LHB that are left-behind: ", round(percent * 100, 1), "%"
      ),
      variable == "Health Index \nrank" ~ paste0(
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
        "<br>", "No. of LSOAs in the LHB that are in the 10% most lonely nationally: ", round(number),
        "<br>", "Percentage of all LSOAs in the LHB that are in the 10% most lonely nationally: ", round(percent * 100, 1), "%"
      )
    )
  )

usethis::use_data(wales_lhb_summary_metrics, overwrite = TRUE)
