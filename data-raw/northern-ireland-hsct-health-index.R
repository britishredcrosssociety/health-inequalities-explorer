library(tidyverse)
library(IMD)
library(geographr)
library(compositr)
library(sf)
library(ggridges)
library(healthindexni)

pkgload::load_all(".")

lookup_northern_ireland_ltla_hsct <-
  lookup_ltla21_hsct18 |>
  select(lad_name = ltla21_name, trust_name = trust18_name)

list_northern_ireland_hsct <-
  lookup_northern_ireland_ltla_hsct |>
  distinct(trust_name)

lookup_sdz_ltla <- lookup_dz21_sdz21_dea14_lgd14 |>
  select(sdz21_code, ltla21_code = lgd14_code, lad_name = lgd14_name) |>
  distinct()

# ---- Aggregate Local Authorities measures ----
# Metrics where rank is used: get maximum LGD rank for each trust
# Higher rank (calculated here) = worse health
imd_health <-
  northern_ireland_ltla_health_index |>
  select(lad_name = area_name, variable, number) |>
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

# ---- Combine & rename (pretty printing) ----
metrics_joined <- imd_health |>
  rename(area_name = trust_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

hsct_health_index_northern_ireland_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(scaled_1_1 = scale_1_1(number)) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# NOTE: No need since current healthindexni data follows higher = better
northern_ireland_hsct_health_index_polarised <- hsct_health_index_northern_ireland_scaled

# Check distributions
northern_ireland_hsct_health_index_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
northern_ireland_hsct_health_index <-
  northern_ireland_hsct_health_index_polarised |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Health Index rank: ", round(number)
    )
  )

usethis::use_data(northern_ireland_hsct_health_index, overwrite = TRUE)
