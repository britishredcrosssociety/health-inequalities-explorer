library(tidyverse)
library(healthyr)
library(geographr)
library(sf)
library(ggridges)

lhb <- boundaries_lhb20 |>
  st_drop_geometry()

# ---- RTT ----
rtt <- wales_rtt_lhb |>
  filter(date >= max(date) %m-% months(2)) |> # Last quarter
  group_by(lhb22_code) |>
  summarise(
    number = mean(waits_over_18_weeks),
    percent = mean(waits_over_18_weeks)
  ) |>
  rename(lhb20_code= lhb22_code)|>
  mutate(
    variable = "Referral to treatment \nwaiting times (Nov 22 - Dec 22)",
    .after = lhb20_code
  )

# ---- Beds ----
available_beds <-wales_health_board_critical_general_acute_beds |>
  filter(date >= max(date)) |> 
  filter(specialism == "all_specialties")|>
  mutate(percent_avilable = 1 - beds_occupancy_rate) |>
  rename(lhb20_code=health_board_code)|>
  mutate(variable = "Bed availability \n(Q4 2022)") |>
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
      variable == "Referral to treatment \nwaiting times (Nov 22 - Dec 22)" ~ scaled_1_1 * -1,
      variable == "Bed availability \n(Q4 2022)" ~ scaled_1_1 * -1,
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
      variable == "Referral to treatment \nwaiting times (Nov 22 - Dec 22)" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. waiting over 18 weeks: ", round(number),
        "<br>", "Percentage waiting over 18 weeks: ", round(percent * 100, 1), "%"
      ),
      variable == "Bed availability \n(Q4 2022)" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of available beds: ", round(number),
        "<br>", "Percentage of all beds available: ", round(percent * 100, 1), "%"
      )
    )
  )

usethis::use_data(wales_lhb_secondary_care, overwrite = TRUE)


