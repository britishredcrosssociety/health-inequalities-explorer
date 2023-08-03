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

# Wards / ICBs are not coterminous. Solution (to work backwards):
#   1. Do a ward to LSOA lookup to assign LSOA's as left-behind or not.
#   2. Lookup LSOAs to ICBS (note: LSOAs are coterminous with ICBs).
#   3. Count the percentage of left-behind LSOAs per ICB.
# Limitation: it assumes all smaller areas share the property of the larger area
# which is likely not to be the case. This means that areas can be wrongly
# assigned as left-behind when this is not actually the case. Given wards are
# already small areas, the effects of this should be low.

# Step 1.
lba_lsoas <- lookup_lsoa11_ward17 |>
  filter(str_detect(lsoa11_code, "^E")) |> # Step 1
  left_join(cni_england_ward17) |>
  select(lsoa11_code, ward17_code, left_behind = `Left Behind Area?`) |>
  # Note that 7 LSOAs are assigned NA. These LSOAs are in City of London of
  # Isles of Scilly which are not left-behind areas.
  mutate(left_behind = if_else(is.na(left_behind), FALSE, left_behind))

# Step 2.
lba_icbs <- lba_lsoas |>
  left_join(lookup_lsoa11_sicbl22_icb22_ltla22) |>
  select(icb22_code, left_behind)

# Step 3.
lba <- lba_icbs |>
  group_by(icb22_code, left_behind) |>
  summarise(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(total_number_lsoas = sum(n)) |>
  ungroup() |>
  filter(left_behind == FALSE) |> # Not all ICBs have left-behind LSOAs
  mutate(number = total_number_lsoas - n) |>
  mutate(percent = 1 - freq) |>
  mutate(variable = "Left-behind areas", .after = icb22_code) |>
  select(-left_behind, -n, -freq, -total_number_lsoas)

# ---- Combine & reanme (pretty printing) ----
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
icb_summary_metrics_england_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled = case_when(
      variable == "Deprivation" ~ positional_normalisation(percent),
      variable == "Left-behind areas" ~ positional_normalisation(percent),
      variable == "ONS Health \nIndex rank" ~ positional_normalisation(number)
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD and LBA, as currently higher = worse health
england_icb_summary_metrics_polarised <- icb_summary_metrics_england_scaled |>
  mutate(
    scaled = case_when(
      variable == "Deprivation" ~ scaled * -1,
      variable == "Left-behind areas" ~ scaled * -1,
      TRUE ~ scaled
    )
  )

# Check distributions
england_icb_summary_metrics_polarised |>
  ggplot(aes(x = scaled, y = variable)) +
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
