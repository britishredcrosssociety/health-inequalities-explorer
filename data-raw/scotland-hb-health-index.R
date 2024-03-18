library(tidyverse)
library(geographr)
library(sf)
library(ggridges)
library(demographr)

hb <- boundaries_hb19 |>
  st_drop_geometry()

lookup_dz_hb <- lookup_dz11_ltla19_hb19 |>
  distinct(dz11_code, hb19_code)

lookup_iz_hb <- lookup_dz11_iz11_ltla20 |>
  left_join(lookup_dz11_ltla19_hb19) |>
  distinct(iz11_code, hb19_code)

lookup_ltla_hb <- lookup_dz11_ltla19_hb19 |>
  distinct(ltla19_code, hb19_code)

population_dz <-
  population20_dz11 |>
  filter(sex == "All") |>
  select(dz11_code, total_population)

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


# ---- Combine & rename (pretty printing) ----
metrics_joined <- health_index |>
  left_join(hb) |>
  select(-hb19_code) |>
  rename(area_name = hb19_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

hb_health_index_scotland_scaled <-
  metrics_joined |>
  mutate(
    scaled_1_1 = scale_1_1(number)
  )

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, health index, and DEPAHRI as currently higher = worse health
scotland_hb_health_index_polarised <- hb_health_index_scotland_scaled |>
  mutate(scaled_1_1 = scaled_1_1 * -1)

# Check distributions
scotland_hb_health_index_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
scotland_hb_health_index <- scotland_hb_health_index_polarised |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Health Index rank: ", round(number)
    )
  )

usethis::use_data(scotland_hb_health_index, overwrite = TRUE)
