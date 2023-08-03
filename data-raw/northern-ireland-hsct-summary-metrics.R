library(tidyverse)
library(IMD)
library(geographr)
library(compositr)
library(sf)
library(ggridges)

pkgload::load_all(".")

lookup_northern_ireland_ltla_hsct <-
  lookup_ltla21_hsct18 |>
  select(lad_name = ltla21_name, trust_name = trust18_name)

list_northern_ireland_hsct <-
  lookup_northern_ireland_ltla_hsct |>
  distinct(trust_name)

# ---- Aggregate Local Authorities measures ----
# Metrics where rank is used: get maximum LGD rank for each trust
# Higher rank (calculated here) = worse health
imd_health <-
  northern_ireland_ltla_summary_metrics |>
  select(lad_name = area_name, variable, number) |>
  filter(variable %in%
    c("Index of Multiple \nDeprivation rank", "Health Index \nrank")) |>
  left_join(lookup_northern_ireland_ltla_hsct) |>
  group_by(trust_name, variable) |>
  summarise(max_rank = max(number)) |>
  ungroup() |>
  group_by(variable) |>
  mutate(
    number = rank(max_rank),
    percent = NA
  ) |>
  select(-max_rank) |>
  ungroup()

# Left-behind areas: adding all left-behind areas of the LGDs of each trust
lba <-
  cni_northern_ireland_soa11 |>
  select(soa11_code, lad_name = lgd14_name, lba = `Left Behind Area?`) |>
  left_join(lookup_northern_ireland_ltla_hsct) |>
  group_by(trust_name) |>
  count(lba) |>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  filter(lba == TRUE) |>
  right_join(list_northern_ireland_hsct) |>
  mutate(percent = replace_na(percent, 0)) |>
  mutate(n = replace_na(n, 0)) |>
  select(trust_name, number = n, percent) |>
  mutate(variable = "Left-behind areas", .after = trust_name)

# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  imd_health,
  lba
) |>
  rename(area_name = trust_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
hsct_summary_metrics_northern_ireland_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled = case_when(
      variable == "Index of Multiple \nDeprivation rank" ~ positional_normalisation(number),
      variable == "Left-behind areas" ~ positional_normalisation(percent),
      variable == "Health Index \nrank" ~ positional_normalisation(number)
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, and health index, as currently higher = worse health
northern_ireland_hsct_summary_metrics_polarised <-
  hsct_summary_metrics_northern_ireland_scaled |>
  mutate(scaled = scaled * -1)

# Check distributions
northern_ireland_hsct_summary_metrics_polarised |>
  ggplot(aes(x = scaled, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
northern_ireland_hsct_summary_metrics <-
  northern_ireland_hsct_summary_metrics_polarised |>
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
        "<br>", "No. of left-behind smaller areas (SOA's) in the Health and Social Care Trust: ", round(number),
        "<br>", "Percentage of all left-behind smaller areas (SOA's) in the Health and Social Care Trust: ", round(percent * 100, 1), "%"
      ),
      variable == "Health Index \nrank" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Health Index rank: ", round(number)
      )
    )
  )

usethis::use_data(northern_ireland_hsct_summary_metrics, overwrite = TRUE)
