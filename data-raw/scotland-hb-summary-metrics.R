library(tidyverse)
library(IMD)
library(geographr)
library(sf)
library(ggridges)
library(compositr)

hb <- boundaries_hb19 |>
  st_drop_geometry()

lookup_dz_hb <- lookup_dz11_ltla19_hb19 |>
  distinct(dz11_code, hb19_code)

lookup_iz_hb <- lookup_dz11_iz11_ltla20 |>
  left_join(lookup_dz11_ltla19_hb19) |>
  distinct(iz11_code, hb19_code)

lookup_ltla_hb <- lookup_dz11_ltla19_hb19 |>
  distinct(ltla19_code, hb19_code)

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

# ---- Health Index Score ----
# An official Health Index for Scotland does not exists. Use the BRC Resilience
# Index version

# Higher score = worse health
# Higher rank (calculated here) = worse health
health_index_raw <- read_csv(
  "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/scotland/index-unweighted-all-indicators.csv"
)

# Strategy: combine scores and rank
health_index <- health_index_raw |>
  select(ltla19_code = lad_code, score = health_inequalities_composite_score) |>
  left_join(lookup_ltla_hb) |>
  group_by(hb19_code) |>
  summarise(score = sum(score)) |>
  mutate(number = rank(score)) |>
  mutate(percent = NA) |>
  mutate(variable = "Health Index \nrank") |>
  relocate(variable, .after = hb19_code) |>
  select(-score)

# ---- Combine & reanme (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
  health_index
) |>
  left_join(hb) |>
  select(-hb19_code) |>
  rename(area_name = hb19_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
hb_summary_metrics_scotland_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled = case_when(
      variable == "Deprivation" ~ positional_normalisation(percent),
      variable == "Left-behind areas" ~ positional_normalisation(percent),
      variable == "Health Index \nrank" ~ positional_normalisation(number)
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, and health index, as currently higher = worse health
scotland_hb_summary_metrics_polarised <- hb_summary_metrics_scotland_scaled |>
  mutate(scaled = scaled * -1)

# Check distributions
scotland_hb_summary_metrics_polarised |>
  ggplot(aes(x = scaled, y = variable)) +
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
      variable == "Health Index \nrank" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Health Index rank: ", round(number)
      )
    )
  )

usethis::use_data(scotland_hb_summary_metrics, overwrite = TRUE)
