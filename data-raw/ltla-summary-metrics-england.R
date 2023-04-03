library(tidyverse)
library(geographr)
library(healthyr)
library(compositr)
library(sf)
library(IMD)
library(readxl)
library(ggridges)

ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^E"))

lookup_england_ltla <-
  lookup_ltla_ltla |>
  filter(str_detect(ltla21_code, "^E"))

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
  mutate(variable = "Index of Multiple \nDeprivation rank", .after = ltla21_code) |>
  mutate(percent = NA, .after = number)

# ---- % Left-behind areas ----
# Higher number/percent = more left-behind
lba <-
  cni_england_ward17 |>
  left_join(lookup_england_ltla, by = c("lad19_code" = "ltla19_code")) |>
  select(ward17_code, ltla21_code, lba = `Left Behind Area?`) |>
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

# ---- ONS Health Index score ----
# Higher score = better health
# Higher rank (calculated here) = better health
health_index_2020 <- england_health_index |>
  filter(year == "2020") |>
  select(ltla21_code, health_index_score = overall_score)

# Data is missing for two ltla's
#   - Map Iscles of Scilly to Cornwall
#   - Map City of London to Hackney
cornwall_score <-
  health_index_2020 |>
  filter(ltla21_code == "E06000052") |>
  pull(health_index_score)

hackney_score <-
  health_index_2020 |>
  filter(ltla21_code == "E09000012") |>
  pull(health_index_score)

health_index_missing_added <-
  health_index_2020 |>
  add_row(ltla21_code = "E06000053", health_index_score = cornwall_score) |>
  add_row(ltla21_code = "E09000001", health_index_score = hackney_score)

# Scores need flipping so polarity matches other summary metrics
health_index <-
  health_index_missing_added |>
  mutate(number = rank(health_index_score)) |>
  mutate(percent = NA) |>
  mutate(variable = "ONS Health \nIndex rank", .after = ltla21_code) |>
  select(-health_index_score)

# ---- Combine & reanme (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
  health_index
) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name) |>
  mutate(data_type = "Summary metrics") |>
  relocate(data_type, .after = area_name)

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
      variable == "ONS Health \nIndex rank" ~ scale_1_1(number)
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD and LBA, as currently higher = worse health
ltla_summary_metrics_england <- ltla_summary_metrics_england_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Index of Multiple \nDeprivation rank" ~ scaled_1_1 * -1,
      variable == "Left-behind areas" ~ scaled_1_1 * -1,
      TRUE ~ scaled_1_1
    )
  )

# Check distributions
ltla_summary_metrics_england |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

usethis::use_data(ltla_summary_metrics_england, overwrite = TRUE)
