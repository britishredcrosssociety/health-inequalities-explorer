library(tidyverse)
library(geographr)
library(sf)
library(ggridges)
library(demographr)


ltla <- boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^S"))

lookup_iz_ltla <- lookup_dz11_iz11_ltla20 |>
  select(iz11_code, ltla21_code = ltla20_code) |>
  distinct()

lookup_dz_ltla <- lookup_dz11_iz11_ltla20 |>
  select(dz11_code, ltla21_code = ltla20_code) |>
  distinct()

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

health_index <- health_index_raw |>
  select(ltla21_code = lad_code, number = health_inequalities_composite_rank) |>
  mutate(percent = NA) |>
  mutate(variable = "Health Index \nrank") |>
  relocate(variable, .after = ltla21_code)

# ---- Combine & reanme (pretty printing) ----
metrics_joined <- health_index |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

ltla_health_index_scotland_scaled <-
  metrics_joined |>
  mutate(
    scaled_1_1 = scale_1_1(number)
  )

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, health index, and DEPAHRI as currently higher = worse health
scotland_ltla_health_index_polarised <- ltla_health_index_scotland_scaled |>
  mutate(scaled_1_1 = scaled_1_1 * -1)

# Check distributions
scotland_ltla_health_index_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
scotland_ltla_health_index <- scotland_ltla_health_index_polarised |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Health Index rank: ", round(number)
    )
  )

usethis::use_data(scotland_ltla_health_index, overwrite = TRUE)
