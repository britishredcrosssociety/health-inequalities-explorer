library(tidyverse)
library(readxl)
library(geographr)
library(compositr)
library(sf)
library(IMD)
library(ggridges)

icb <- boundaries_icb22 |>
  st_drop_geometry() |>
  mutate(icb22_name = str_remove_all(icb22_name, "^NHS ")) |>
  mutate(icb22_name = str_remove_all(icb22_name, " Integrated Care Board$"))

lookup_lsoa11_lsoa21_icb22 <-
  lookup_lsoa11_sicbl22_icb22_ltla22 |>
  select(lsoa11_code, icb22_code) |>
  left_join(lookup_lsoa11_lsoa21_ltla22) |>
  select(lsoa21_code, icb22_code)

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
# Lookup LSOAs to ICBS (note: LSOAs are coterminous with ICBs).

lba <-
  cni2023_england_lsoa21 |>
  select(lsoa21_code, lba = `Left Behind Area?`) |>
  left_join(lookup_lsoa11_lsoa21_icb22) |>
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

# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
  health_index
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
      variable == "ONS Health \nIndex rank" ~ scale_1_1(number)
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD and LBA, as currently higher = worse health
england_icb_summary_metrics_polarised <- icb_summary_metrics_england_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Deprivation" ~ scaled_1_1 * -1,
      variable == "Left-behind areas" ~ scaled_1_1 * -1,
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
      )
    )
  )

usethis::use_data(england_icb_summary_metrics, overwrite = TRUE)
