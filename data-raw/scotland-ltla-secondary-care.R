library(tidyverse)
library(lubridate)
library(healthyr)
library(geographr)
library(sf)
library(ggridges)

ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^S"))

# Popuation counts
# Source: https://www.opendata.nhs.scot/dataset/population-estimates
pop_raw <- read_csv(
  "https://www.opendata.nhs.scot/dataset/7f010430-6ce1-4813-b25c-f7f335bdc4dc/resource/09ebfefb-33f4-4f6a-8312-2d14e2b02ace/download/ca2019_pop_est_15072022.csv"
)

pop_scotland <- pop_raw |>
  filter(Year == 2021) |>
  filter(Sex == "All") |>
  filter(CA != "S92000003") |>
  select(ltla21_code = CA, population = AllAges) |>
  print(n = Inf)

# ---- Delayed discharge ----
delayed_discharge_monthly <- scotland_delayed_discharge_ltla |>
  drop_na() |>
  filter(ltla_code != "S92000003") |>
  filter(date >= max(date) %m-% months(2)) |> # Last quarter
  filter(age_group == "18-74" | age_group == "75plus") |>
  filter(delay_reason == "All Delay Reasons") |>
  rename(ltla21_code = ltla_code) |>
  group_by(ltla21_code, date) |>
  summarise(
    num_delayed_bed_days = sum(num_delayed_bed_days),
    average_daily_delayed_beds = sum(average_daily_delayed_beds)
  ) |>
  ungroup()

delayed_discharge_summary <- delayed_discharge_monthly |>
  group_by(ltla21_code) |>
  summarise(
    num_delayed_bed_days = mean(num_delayed_bed_days),
    average_daily_delayed_beds = mean(average_daily_delayed_beds)
  ) |>
  select(-num_delayed_bed_days)

delayed_discharges <- delayed_discharge_summary |>
  left_join(pop_scotland) |>
  mutate(beds_per_10000 = average_daily_delayed_beds / population * 10000) |>
  select(
    ltla21_code,
    number = average_daily_delayed_beds,
    percent = beds_per_10000
  ) |>
  mutate(
    variable = "Delayed discharges \n(Jan 23 - Mar 23 average)",
    .after = ltla21_code
  )

# ---- Combine ----
metrics_joined <- delayed_discharges |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

scotland_ltla_secondary_care_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(scaled_1_1 = scale_1_1(percent)) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip delayed discharges, as currently higher = worse health
scotland_ltla_secondary_care_polarised <- scotland_ltla_secondary_care_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Delayed discharges \n(Jan 23 - Mar 23 average)" ~ scaled_1_1 * -1,
      TRUE ~ scaled_1_1
    )
  )

# Check distributions
scotland_ltla_secondary_care_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

# ---- Add plot labels ----
scotland_ltla_secondary_care <- scotland_ltla_secondary_care_polarised |>
  mutate(
    label = case_when(
      variable == "Delayed discharges \n(Jan 23 - Mar 23 average)" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Average daily no. of delayed beds: ", round(number),
        "<br>", "Average daily no. of delayed beds per 10,000 people: ", round(percent * 100, 1), "%"
      )
    )
  )

usethis::use_data(scotland_ltla_secondary_care, overwrite = TRUE)
