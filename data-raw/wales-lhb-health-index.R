library(tidyverse)
library(geographr)
library(sf)
library(ggridges)
library(demographr)

lhb <- boundaries_lhb20 |>
  st_drop_geometry()

lookup_lsoa11_ltla21_lhb22 <- lookup_lsoa11_ltla21 |>
  filter(str_detect(ltla21_code, "^W")) |>
  left_join(lookup_ltla21_lhb22) |>
  distinct(lsoa11_code, lhb22_code, lhb22_name, ltla21_code) |>
  select(lsoa11_code, lhb20_code = lhb22_code, ltla21_code)

population_lsoa <-
  population20_lsoa11 |>
  select(lsoa11_code, total_population) |>
  filter(str_detect(lsoa11_code, "^W"))


# ---- health ----
# An official Health Index for Wales does not exists. Use the BRC Resilience
# Index version
# Source: https://github.com/britishredcrosssociety/resilience-index

# Higher score = worse health
# Higher rank (calculated here) = worse health
url <- "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/wales/healthy-people-domain.csv"

resilience_index_raw <- read_csv(url)

health_index <- resilience_index_raw |>
  rename(ltla21_code = lad_code) |>
  left_join(lookup_ltla21_lhb22) |>
  select(lhb22_code, score = healthy_people_domain_score) |>
  group_by(lhb22_code) |>
  summarise(score = sum(score)) |>
  mutate(number = rank(score)) |>
  mutate(percent = NA) |>
  mutate(variable = "Health Index \nrank") |>
  select(lhb20_code = lhb22_code, variable, number, percent)

# ---- Combine & rename (pretty printing) ----
metrics_joined <- health_index |>
  left_join(lhb) |>
  select(-lhb20_code) |>
  rename(area_name = lhb20_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

lhb_health_index_wales_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled_1_1 = scale_1_1(number)
      
  ) 

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, health index, and DEPAHRI, as currently higher = worse health
wales_lhb_health_index_polarised <- lhb_health_index_wales_scaled |>
  mutate(scaled_1_1 = scaled_1_1 * -1)

# Check distributions
wales_lhb_health_index_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
wales_lhb_health_index <- wales_lhb_health_index_polarised |>
  mutate(
    label = paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Health Index rank: ", round(number)
      )
    
  )

usethis::use_data(wales_lhb_health_index, overwrite = TRUE)
