library(tidyverse)
library(readxl)
library(geographr)
library(sf)
library(ggridges)
library(compositr)

# ---- Create lookups ----
icb <- boundaries_icb22 |>
  st_drop_geometry() |>
  mutate(icb22_name = str_remove_all(icb22_name, "^NHS ")) |>
  mutate(icb22_name = str_remove_all(icb22_name, " Integrated Care Board$"))

lsoa_icb <- lookup_lsoa11_sicbl22_icb22_ltla22 |>
  distinct(lsoa11_code, icb22_code)

# ---- ONS Health Index ----
# Higher score = better health
# Higher rank (calculated here) = better health
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/datasets/healthindexscoresintegratedcaresystemsengland
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/datasets/healthindexscoresintegratedcaresystemsengland/current/healthindexscoresintegratedcaresystemsengland.xlsx"

raw <- tempfile(fileext = ".xlsx")

httr2::request(url) |>
  httr2::req_perform(
    path = raw
  )

health_index_raw <- read_excel(raw, sheet = "Table_2_Index_scores", skip = 2)

health_index <- health_index_raw |>
  select(icb22_code = `Area Code`, number = `2021`) |>
  mutate(number = rank(number)) |>
  mutate(percent = NA) |>
  mutate(variable = "ONS Health \nIndex rank") |>
  relocate(variable, .after = icb22_code)

# ---- Rename (pretty printing) ----
metrics_joined <-
  health_index |>
  left_join(icb) |>
  select(-icb22_code) |>
  rename(area_name = icb22_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

icb_health_index_england_scaled <-
  metrics_joined |>
  mutate(
    scaled_1_1 = scale_1_1(number)
  ) |>
  ungroup()

# Check distributions
icb_health_index_england_scaled |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
england_icb_health_index <- icb_health_index_england_scaled |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "Health Index rank: ", round(number)
    )
  )


usethis::use_data(england_icb_health_index, overwrite = TRUE)
