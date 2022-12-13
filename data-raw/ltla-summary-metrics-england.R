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
imd <-
  imd_england_lad |>
  select(ltla19_code = lad_code, imd_score = Score) |>
  left_join(lookup_england_ltla, by = "ltla19_code") |>
  select(ltla21_code, imd_score) |>
  group_by(ltla21_code) |>
  summarise(number = mean(imd_score)) |>
  mutate(variable = "IMD Score", .after = ltla21_code) |>
  mutate(percent = NA, .after = number)

# ---- % Left-behind areas ----
# Higher score = more deprived
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
  mutate(variable = "Left-beind areas", .after = ltla21_code)

# ---- ONS Health Index score ----
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
  mutate(health_index_score = health_index_score * -1) |>
  mutate(number = rank(health_index_score)) |> # Higher rank = worse health
  mutate(percent = NA) |>
  mutate(variable = "ONS Health Index \nrank", .after = ltla21_code) |>
  select(-health_index_score)

# ---- Combine & reanme (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
  health_index
) |>
  rename(area_name = ltla21_code)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

ltla_summary_metrics_england <-
  metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled_1_1 = case_when(
      variable == "IMD Score" ~ scale_1_1(number),
      variable == "Left-beind areas" ~ scale_1_1(percent),
      variable == "ONS Health Index \nrank" ~ scale_1_1(number)
    )
  ) |>
  ungroup()

# Check distributions
ltla_summary_metrics_england |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

usethis::use_data(ltla_summary_metrics_england, overwrite = TRUE)
