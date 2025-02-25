library(tidyverse)
library(geographr)
library(compositr)
library(sf)
library(ggridges)
library(healthindexni)

ltla <-
  boundaries_ltla24 |>
  st_drop_geometry() |>
  filter(str_detect(ltla24_code, "^N"))

lookup_sdz_ltla <- lookup_dz21_sdz21_dea14_lgd14 |>
  select(sdz21_code, ltla24_code = lgd14_code) |>
  distinct()

# ---- Health Index Score ----
# Taken from healthindexni package
health_index <- ni_health_index |>
  select(ltla24_code, number = health_inequalities_rank) |>
  mutate(percent = NA) |>
  mutate(variable = "Health Index \nrank") |>
  relocate(variable, .after = ltla24_code)

# ---- Combine & rename (pretty printing) ----
metrics_joined <- health_index |>
  left_join(ltla) |>
  select(-ltla24_code) |>
  rename(area_name = ltla24_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

ltla_health_index_northern_ireland_scaled <-
  metrics_joined |>
  mutate(
    scaled_1_1 = scale_1_1(number),
  )

# ---- Align indicator polarity ----
# Align so higher value = better health
# NOTE: No need since current healthindexni data follows higher = better
northern_ireland_ltla_health_index_polarised <- ltla_health_index_northern_ireland_scaled

# Check distributions
northern_ireland_ltla_health_index_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
northern_ireland_ltla_health_index <-
  northern_ireland_ltla_health_index_polarised |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Health Index rank: ", round(number)
    )
  )

usethis::use_data(northern_ireland_ltla_health_index, overwrite = TRUE)
