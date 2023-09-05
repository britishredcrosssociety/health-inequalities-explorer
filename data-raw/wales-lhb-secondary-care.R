library(tidyverse)
library(healthyr)
library(geographr)
library(sf)
library(ggridges)

devtools::load_all(".")

lhb <- boundaries_lhb20 |>
  st_drop_geometry()

pop_wales_lhb <- wales_lhb_demographics |>
  filter(variable %in% c("Younger \npeople (< 18)", "Working \nage (18-65)", "Older \npeople (65+)")) |>
  group_by(area_name) |>
  summarise(total_pop = sum(number)) |>
  rename(lhb20_name = area_name) |>
  left_join(lhb)

# ---- RTT ----
# Higher = worse performance
rtt <- wales_rtt_lhb |>
  rename(lhb20_code = lhb22_code) |>
  filter(date >= max(date) %m-% months(2)) |> # Last quarter
  group_by(lhb20_code) |>
  summarise(
    number = mean(waits_over_18_weeks),
  ) |>
  left_join(pop_wales_lhb) |>
  mutate(
    percent = number / total_pop
  ) |>
  select(-total_pop, -lhb20_name) |> 
  mutate(
    variable = "Referral to treatment \nwaiting times (Mar 23 - May 23)",
    .after = lhb20_code
  )

# ---- Beds ----
# Higher = better performance
available_beds <- wales_health_board_critical_general_acute_beds |>
  rename(lhb20_name = health_board_name) |>
  right_join(lhb) |>
  relocate(lhb20_code) |>
  filter(date == "Mar-22") |>
  filter(specialism == "all_specialties") |>
  mutate(beds_occupancy_rate = beds_occupancy_rate / 100) |>
  mutate(percent_avilable = 1 - beds_occupancy_rate) |>
  mutate(variable = "Bed availability \n(Mar 2022)") |>
  select(
    lhb20_code,
    variable,
    number = daily_beds_available,
    percent = percent_avilable
  )

# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  rtt,
  available_beds
) |>
  left_join(lhb) |>
  select(-lhb20_code) |>
  rename(area_name = lhb20_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

secondary_care_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(scaled_1_1 = scale_1_1(percent)) |>
  ungroup()

# ---- Align indicator polarity ----
secondary_care_polarised <- secondary_care_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Referral to treatment \nwaiting times (Mar 23 - May 23)" ~ scaled_1_1 * -1,
      TRUE ~ scaled_1_1
    )
  )

# Check distributions
secondary_care_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
wales_lhb_secondary_care <- secondary_care_polarised |>
  mutate(
    label = case_when(
      variable == "Referral to treatment \nwaiting times (Mar 23 - May 23)" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. waiting over 18 weeks: ", round(number),
        "<br>", "Waiting times over 18 weeks per 10,000 people: ", round(percent * 10000, 1)
      ),
      variable == "Bed availability \n(Mar 2022)" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of available beds: ", round(number),
        "<br>", "Percentage of all beds available: ", round(percent * 100, 1), "%"
      )
    )
  )

usethis::use_data(wales_lhb_secondary_care, overwrite = TRUE)
