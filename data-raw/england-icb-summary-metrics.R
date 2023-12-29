library(tidyverse)
library(readxl)
library(geographr)
library(compositr)
library(sf)
library(IMD)
library(ggridges)
library(DEPAHRI)
library(demographr)
library(loneliness)

# ---- Create lookups ----
icb <- boundaries_icb22 |>
  st_drop_geometry() |>
  mutate(icb22_name = str_remove_all(icb22_name, "^NHS ")) |>
  mutate(icb22_name = str_remove_all(icb22_name, " Integrated Care Board$"))

lsoa_icb <- lookup_lsoa11_sicbl22_icb22_ltla22 |>
  distinct(lsoa11_code, icb22_code)

population_lsoa <-
  population20_lsoa11 |> 
  select(lsoa11_code, total_population) |> 
  filter(str_detect(lsoa11_code, "^E"))

# ---- IMD score ----
# Decile 1 = most deprived
# Higher percentage / number = worse health
imd <- imd_england_lsoa |>
  rename(lsoa11_code = lsoa_code) |>
  left_join(lookup_lsoa11_sicbl22_icb22_ltla22) |>
  select(
    lsoa11_code,
    decile = IMD_decile,
    icb22_code
  ) |>
  mutate(top_10 = if_else(decile == 1, "yes", "no")) |>
  group_by(icb22_code, top_10) |>
  summarise(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(total_number_lsoas = sum(n)) |>
  ungroup() |>
  filter(top_10 == "no") |> # Not all ICBs have top 10% most vulnerable LSOAs
  mutate(number = total_number_lsoas - n) |>
  mutate(percent = 1 - freq) |>
  mutate(variable = "Deprivation", .after = icb22_code) |>
  select(-top_10, -n, -freq, -total_number_lsoas)

# ---- ONS Health Index ----
# Higher score = better health
# Higher rank (calculated here) = better health
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/datasets/healthindexscoresintegratedcaresystemsengland
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/datasets/healthindexscoresintegratedcaresystemsengland/current/healthindexscoresintegratedcaresystemsengland.xlsx"

raw <- download_file(
  url,
  ".xlsx"
)

health_index_raw <- read_excel(raw, sheet = "Table_2_Index_scores", skip = 2)

health_index <- health_index_raw |>
  select(icb22_code = `Area Code`, number = `2021`) |>
  mutate(number = rank(number)) |>
  mutate(percent = NA) |>
  mutate(variable = "ONS Health \nIndex rank") |>
  relocate(variable, .after = icb22_code)

# ---- % Left-behind areas ----
# Higher number/percent = more left-behind

# ---- 2021 left-behind LSOA's to 2011 LSOA's ----
lsoa_lsoa <- lookup_lsoa11_lsoa21_ltla22 |>
  filter(str_detect(lsoa21_code, "^E")) |>
  distinct(lsoa11_code, lsoa21_code, change_code) |>
  relocate(change_code, .after = lsoa21_code)

# Change codes:
# - U = Unchanged - 31,810 LSOAs remained unchanged from 2011 to 2021
# - S = Split - 834 LSOAs were split from 2011 to 2021
# - M = Merged - 194 LSOAs were merged from 2011 to 2021
# - X = Fragmented - 6 LSOAs were fragmented from 2011 to 2021

# Aggregation stategy going from 2021 codes to 2011 codes:
# - change_code == "U": no action required
# - change_code == "S": if ANY of the 2021 LSOAs resulting from the split is
#   left behind, 2011 LSOA is considered left-behind
# - change_code == "M": 2011 LSOA's inherit the left-behind characteristics of
#   the 2021 LSOA that resulted from the merge: if the 2021 LSOA is left behind,
#   all corresponding 2011 LSOA's are considered left-behind, and vice versa
# - change_code == "X": 2011 LSOA's inherit the left behind characteristics of
#   their corresponding 2021 LSOA. Then group by 2011 LSOA, if any is
#   left-behind, consider the LSOA left-behind
aggregate_lba_lsoas <- function(data) {
  data_u <- data |>
    left_join(lsoa_lsoa) |>
    filter(change_code == "U") |>
    select(lsoa11_code, lba = `Left Behind Area?`)

  data_s <- data |>
    left_join(lsoa_lsoa) |>
    relocate(lsoa11_code) |>
    filter(change_code == "S") |>
    group_by(lsoa11_code) |>
    summarize(lba = any(`Left Behind Area?` == TRUE)) |>
    ungroup()

  data_m <- data |>
    left_join(lsoa_lsoa) |>
    relocate(lsoa11_code) |>
    filter(change_code == "M") |>
    select(lsoa11_code, lba = `Left Behind Area?`)

  data_x <- data |>
    left_join(lsoa_lsoa) |>
    relocate(lsoa11_code) |>
    filter(change_code == "X") |>
    group_by(lsoa11_code) |>
    summarize(lba = any(`Left Behind Area?` == TRUE)) |>
    ungroup()

  data_aggregated <- bind_rows(data_u, data_s, data_m, data_x)

  data_aggregated
}

lba <-
  aggregate_lba_lsoas(cni2023_england_lsoa21) |>
  left_join(lsoa_icb) |>
  group_by(icb22_code) |>
  count(lba) |>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  filter(lba == TRUE) |>
  right_join(icb) |>
  mutate(percent = replace_na(percent, 0)) |>
  mutate(n = replace_na(n, 0)) |>
  select(icb22_code, number = n, percent) |>
  mutate(variable = "Left-behind areas", .after = icb22_code)

# ---- DEPAHRI score ----
# Data is at LSOA level: need to aggregate to ICB level using calculate_extent
# Extent is the proportion of the local population that live in areas 
# classified as among the most deprived (here at risk) in the higher geography
# Higher score = higher risk of exclusion
# Higher rank (calculated here) = higher risk of exclusion
depahri_lsoa <- 
  england_lsoa_depahri |> 
  left_join(lsoa_icb) |> 
  distinct(lsoa11_code, depahri_score_national, icb22_code) |> 
  left_join(population_lsoa)

depahri <-
  calculate_extent(depahri_lsoa, 
                   depahri_score_national, 
                   icb22_code, 
                   total_population, 
                   weight_high_scores = TRUE) |> 
  mutate(number = rank(extent))|>
  select(-extent) |>
  mutate(
    variable = "Access to Healthcare \n (Physical and Digital)",
    .after = icb22_code
  ) |>
  mutate(percent = NA, .after = number)

# ---- Loneliness score ----
# Higher percentage / number = worse health

# Convert LSOA'21 to LSOA'11 codes
# Aggregation strategy going from 2021 codes to 2011 codes:
# - change_code == "U": no action required
# - change_code == "S": take the average score of the 2021 LSOA
# - change_code == "M": 2011 LSOA inherits the score of the 2021 LSOA
# - change_code == "X": 2011 LSOA inherits the score of 2021 LSOA.
#   then group by 2011 LSOA

aggregate_loneliness_lsoas <- function(data) {
  data_u <- data |>
    left_join(lsoa_lsoa) |>
    filter(change_code == "U") |>
    select(lsoa11_code, perc)
  
  data_s <- data |>
    left_join(lsoa_lsoa) |>
    filter(change_code == "S") |>
    group_by(lsoa11_code) |>
    summarize(perc = mean(perc, na.rm = TRUE),
              ) |>
    ungroup()
  
  data_m <- data |>
    left_join(lsoa_lsoa) |>
    relocate(lsoa11_code) |>
    filter(change_code == "M") |>
    select(lsoa11_code, perc)
  
  data_x <- data |>
    left_join(lsoa_lsoa) |>
    filter(change_code == "X") |>
    group_by(lsoa11_code) |>
    summarize(perc = mean(perc, na.rm = TRUE),
              ) |>
    ungroup()
  
  data_aggregated <- bind_rows(data_u, data_s, data_m, data_x)
  
  data_aggregated
}

# Used mean weighted by population size to aggregate to icb from lsoa
loneliness_lsoa <-
  aggregate_loneliness_lsoas(england_cls_loneliness_lsoa) |>
  left_join(lsoa_icb) |>
  left_join(population_lsoa) 

loneliness <- loneliness_lsoa |>
  group_by(icb22_code) |>
  summarise(percent = weighted.mean(perc, w = total_population, na.rm = TRUE)) |>
  mutate(
    variable = "Loneliness",
    .after = icb22_code
  ) |>
  mutate(percent = percent/100) |>
  mutate(number = NA, .before = percent)

# loneliness <-
#   calculate_extent(loneliness_lsoa, 
#                    perc,
#                    icb22_code,
#                    total_population,
#                    weight_high_scores = TRUE) |>
#   rename(percent = extent) |>
#   mutate(variable = "Loneliness", .after = icb22_code) |>
#   mutate(number = NA, .after = variable)


# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
  health_index,
  depahri,
  loneliness
) |>
  left_join(icb) |>
  select(-icb22_code) |>
  rename(area_name = icb22_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

icb_summary_metrics_england_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Deprivation" ~ scale_1_1(percent),
      variable == "Left-behind areas" ~ scale_1_1(percent),
      variable == "ONS Health \nIndex rank" ~ scale_1_1(number),
      variable == "Access to Healthcare \n (Physical and Digital)" ~ scale_1_1(number),
      variable == "Loneliness" ~ scale_1_1(percent)
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, DEPAHRI, loneliness as currently higher = worse health
england_icb_summary_metrics_polarised <- icb_summary_metrics_england_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Deprivation" ~ scaled_1_1 * -1,
      variable == "Left-behind areas" ~ scaled_1_1 * -1,
      variable == "Access to Healthcare \n (Physical and Digital)" ~ scaled_1_1 * -1,
      variable == "Loneliness" ~ scaled_1_1 * -1,
      TRUE ~ scaled_1_1
    )
  )

# Check distributions
england_icb_summary_metrics_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
england_icb_summary_metrics <- england_icb_summary_metrics_polarised |>
  mutate(
    label = case_when(
      variable == "Deprivation" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "The no. of LSOAs in the ICB that are in the top 10% most deprived nationally: ", round(number),
        "<br>", "Percentage of LSOAs in the ICB that are in the top 10% most deprived nationally: ", round(percent * 100, 1), "%"
      ),
      variable == "Left-behind areas" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of left-behind LSOAs in the ICB: ", round(number),
        "<br>", "Percentage of LSOAs in ICB that are left-behind: ", round(percent * 100, 1), "%"
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

usethis::use_data(england_icb_summary_metrics, overwrite = TRUE)
