library(tidyverse)
library(IMD)
library(geographr)
library(compositr)
library(sf)
library(ggridges)
library(loneliness)

ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^N"))

lookup_sdz_ltla <- lookup_dz21_sdz21_dea14_lgd14 |>
  select(sdz21_code, ltla21_code = lgd14_code) |>
  distinct()

# ---- IMD score ----
# Higher extent = more deprived /
# Higher rank (calculated here) = more deprived
imd <-
  imd_northern_ireland_lad |>
  select(ltla21_code = lad_code, imd_score = Extent) |>
  mutate(number = rank(imd_score)) |>
  select(-imd_score) |>
  mutate(
    variable = "Index of Multiple \nDeprivation rank",
    .after = ltla21_code
  ) |>
  mutate(percent = NA, .after = number)

# ---- % Left-behind areas ----
# Come from the community needs index (loaded from the IMD package)
# Higher number/percent = more left-behind
lba <-
  cni_northern_ireland_soa11 |>
  select(soa11_code, ltla21_code = lgd14_code, lba = `Left Behind Area?`) |>
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

# ---- Loneliness  ----
# Decile 1 = least lonely
# Calculate % of sdz in ltla in decile 10 nationally
loneliness <-
  ni_clinical_loneliness_sdz |>
  left_join(lookup_sdz_ltla) |>
  select(sdz21_code, ltla21_code, deciles) |>
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


# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
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

ltla_summary_metrics_northern_ireland_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Index of Multiple \nDeprivation rank" ~ scale_1_1(number),
      variable == "Left-behind areas" ~ scale_1_1(percent),
      variable == "Loneliness" ~ scale_1_1(number)
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, health index and loneliness as currently higher = worse health
northern_ireland_ltla_summary_metrics_polarised <-
  ltla_summary_metrics_northern_ireland_scaled |>
  mutate(scaled_1_1 = scaled_1_1 * -1)

# Check distributions
northern_ireland_ltla_summary_metrics_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
northern_ireland_ltla_summary_metrics <-
  northern_ireland_ltla_summary_metrics_polarised |>
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
        "<br>", "No. of left-behind smaller areas (SOA's) in the Local Authority: ", round(number),
        "<br>", "Percentage of all left-behind smaller areas (SOA's) in the Local Authority: ", round(percent * 100, 1), "%"
      ),
      variable == "Loneliness" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of Super Data Zones in the Local Authority that are in the 20% most lonely nationally: ", round(number),
        "<br>", "Percentage of all Super Data Zones in the Local Authority that are in the 20% most lonely nationally: ", round(percent * 100, 1), "%"
      )
    )
  )

usethis::use_data(northern_ireland_ltla_summary_metrics, overwrite = TRUE)
