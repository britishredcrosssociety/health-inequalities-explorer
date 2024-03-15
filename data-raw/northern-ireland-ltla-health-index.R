library(tidyverse)
library(geographr)
library(compositr)
library(sf)
library(ggridges)

ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^N"))

lookup_sdz_ltla <- lookup_dz21_sdz21_dea14_lgd14 |>
  select(sdz21_code, ltla21_code = lgd14_code) |>
  distinct()

# ---- ONS Health Index ----
# An official Health Index for Northern Ireland does not exists.
# Use the BRC Resilience Index version
# Higher score = worse health
# Higher rank (calculated here) = worse health
health_index_raw <- read_csv(
  "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/northern-ireland/index-unweighted-all-indicators.csv"
)

health_index <-
  health_index_raw |>
  select(ltla21_code = lad_code, number = health_inequalities_composite_rank) |>
  mutate(percent = NA) |>
  mutate(variable = "Health Index \nrank") |>
  relocate(variable, .after = ltla21_code)


# ---- Combine & rename (pretty printing) ----
metrics_joined <- health_index |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
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
# Flip IMD, LBA, health index and loneliness as currently higher = worse health
northern_ireland_health_index_polarised <-
  ltla_health_index_northern_ireland_scaled |>
  mutate(scaled_1_1 = scaled_1_1 * -1)

# Check distributions
northern_ireland_health_index_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
northern_ireland_ltla_health_index <-
  northern_ireland_health_index_polarised |>
  mutate(
    label = paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Health Index rank: ", round(number)
      )
  )

usethis::use_data(northern_ireland_ltla_health_index, overwrite = TRUE)
