# ---- Load libs & helpers ----
library(tidyverse)
library(sf)
library(healthyr)
library(compositr)
library(ggridges)
library(devtools)
library(readODS)
library(geographr)

hsct <- boundaries_trusts_ni18 |>
  st_drop_geometry()

# ---- RTT ----
# Higher = worse performance
# Data is quarterly
rtt <- ni_rtt_hsct |>
  select(
    trust18_name = "hsct22_name",
    date,
    waits_over_18_weeks,
    total_waits,
  ) |>
  filter(date >= max(date) %m-% months(2)) # Last quarter

# Create dynamic label
max_date_rtt <- max(rtt$date) |>
  format("%B %Y")
min_date_rtt <- max(rtt$date) %m-% months(2) |>
  format("%B %Y")

rtt_label <- paste("Referral to treatment \nwaiting times \n(", min_date_rtt, " - ", max_date_rtt, " average)", sep = "")

rtt <- rtt |>
  group_by(trust18_name, date) |>
  summarise(
    waits_over_18_weeks = sum(waits_over_18_weeks),
    total_waits = sum(total_waits)
  ) |>
  mutate(percent = waits_over_18_weeks / sum(total_waits, na.rm = TRUE)) |>
  rename(
    number = waits_over_18_weeks,
  ) |>
  mutate(
    variable = rtt_label,
    .after = trust18_name
  ) |>
  filter(!trust18_name %in% c("DPC", "Day Case Procedure Centre"))

# ---- Beds ----
# Higher = better performance
available_beds <- ni_beds |>
  mutate(date = as.Date(date, format = "%Y-%m-%d")) |>
  left_join(hsct, by = "trust18_code") |>
  mutate(
    percent_available = total_available_beds /
      (total_available_beds + total_occupied_beds)
  ) |>
  filter(date >= max(date) %m-% months(2)) # Last quarter

# Create dynamic label
max_date_beds <- max(available_beds$date) |>
  format("%B %Y")
min_date_beds <- max(available_beds$date) %m-% months(2) |>
  format("%B %Y")
beds_label <- paste("Bed availablity \n(", min_date_beds, " - ", max_date_beds, " average)", sep = "")

available_beds <- available_beds |>
  group_by(trust18_name) |>
  summarise(
    number = sum(total_available_beds),
    percent = mean(percent_available, na.rm = TRUE)
  ) |>
  mutate(
    variable = beds_label,
    .after = trust18_name
  )

# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  rtt,
  available_beds
) |>
  left_join(hsct) |>
  select(-trust18_code) |>
  rename(area_name = trust18_name) |>
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
# Align so higher value = better health
# Flip RTT
secondary_care_polarised <- secondary_care_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable == rtt_label ~ scaled_1_1 * -1,
      TRUE ~ scaled_1_1
    )
  )

# Check distributions
secondary_care_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +
  # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") +
  # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

# ---- Add plot labels ----
northern_ireland_hsct_secondary_care <- secondary_care_polarised |>
  mutate(
    label = case_when(
      variable == rtt_label ~
        paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. people waiting over 18 weeks to start treatment: ", round(number),
          "<br>", "Percentage of people waiting over 18 weeks: ", round(percent * 100, 1),
          "%"
        ),
      variable == beds_label ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of available beds: ", round(number),
        "<br>", "Percentage of all beds available: ", round(percent * 100, 1),
        "%"
      )
    )
  )

usethis::use_data(northern_ireland_hsct_secondary_care, overwrite = TRUE)
