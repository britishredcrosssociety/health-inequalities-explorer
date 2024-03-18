library(tidyverse)
library(geographr)
library(sf)
library(ggridges)
library(stringr)
library(demographr)

ltla <- boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^W")) |>
  select(ltla21_code, ltla21_name)

lookup_msoa_ltla <- lookup_msoa11_ltla21 |>
  select(msoa11_code, ltla21_code) |>
  filter(str_starts(ltla21_code, "W"))

lookup_lsoa_ltla <-
  lookup_lsoa11_lsoa21_ltla22 |>
  distinct(lsoa11_code, ltla22_code) |>
  filter(str_detect(ltla22_code, "^W"))

population_lsoa <-
  population20_lsoa11 |>
  select(lsoa11_code, total_population) |>
  filter(str_detect(lsoa11_code, "^W"))

# ---- Health Index Score ----
# Higher score = worse health
# Higher rank (calculated here) = worse health
url <- "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/wales/healthy-people-domain.csv"

health_index_raw <- read_csv(url)

health_index <- health_index_raw |>
  select(ltla21_code = lad_code, number = healthy_people_domain_rank) |>
  mutate(percent = NA) |>
  mutate(variable = "Health Index \nrank") |>
  relocate(variable, .after = ltla21_code)


# ---- Combine & rename (pretty printing) ----
metrics_joined <- health_index |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name)

# ---- Normalise/scale ---- quotient transformation (x/sum)
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

ltla_health_index_wales_scaled <- metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled_1_1 = scale_1_1(number)
  )

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, health index, DEPAHRI, loneliness as currently higher = worse health
wales_ltla_health_index_polarised <-
  ltla_health_index_wales_scaled |>
  mutate(scaled_1_1 = scaled_1_1 * -1)

# Check distributions
wales_ltla_health_index_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
wales_ltla_health_index <- wales_ltla_health_index_polarised |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Health Index rank: ", round(number)
    )
  )

usethis::use_data(wales_ltla_health_index, overwrite = TRUE)
