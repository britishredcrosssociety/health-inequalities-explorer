library(tidyverse)
library(geographr)
library(healthyr)
library(lubridate)
library(sf)
library(ggridges)

icb <- boundaries_icb22 |>
  st_drop_geometry() |>
  mutate(icb22_name = str_remove_all(icb22_name, "^NHS ")) |>
  mutate(icb22_name = str_remove_all(icb22_name, " Integrated Care Board$"))

# ---- Attendances over 4 hours ----
attendances_4_hours <- england_icb_accidents_emergency |>
  mutate(date = parse_date_time(date, orders = "B Y")) |>
  filter(date >= ymd("2022-05-01"), date <= ymd("2023-04-01")) |>
  subset(select = c(icb22_code, attendances_over_4hours, pct_attendance_over_4hours)) |>
  rename(number = attendances_over_4hours, percent = pct_attendance_over_4hours) |>
  group_by(icb22_code) |>
  summarize(number = sum(number), percent = mean(percent)) |>
  mutate(variable = "Attendances over 4 hours \n(May 22 - April 23 average)", .after = icb22_code) |>
  left_join(icb) |>
  select(-icb22_code) |>
  rename(area_name = icb22_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

icb_accidents_emergency_england_scaled <-
  attendances_4_hours |>
  mutate(
    scaled_1_1 = scale_1_1(percent)
  )

# ---- Align indicator polarity ----
# Align so higher value = lower attendance over 4 hours
england_icb_accidents_emergency_polarised <- icb_accidents_emergency_england_scaled |>
  mutate(
    scaled_1_1 = scaled_1_1 * -1
  )

# Check distributions
england_icb_accidents_emergency_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
england_icb_accidents_emergencies <- england_icb_accidents_emergency_polarised |>
  mutate(
    label = paste0(
      "<b>", area_name, "</b>",
      "<br>",
      "<br>", "The total no. of A&E patients in the ICB that spend over 4 hours from arrival to admission, transfer or discharge: ", round(number),
      "<br>", "The percentage of A&E patients in the ICB that spend over 4 hours from arrival to admission, transfer or discharge: ", round(percent * 100, 1), "%"
    )
  )

usethis::use_data(england_icb_accidents_emergencies, overwrite = TRUE)
