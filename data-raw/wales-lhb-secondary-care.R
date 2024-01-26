library(tidyverse)
devtools::install_github("humaniverse/healthyr", ref = "pull/30/head")
library(healthyr)
library(geographr)
library(sf)
library(ggridges)
library(lubridate)

lhb <- boundaries_lhb20 |>
  st_drop_geometry()

# ---- RTT ----
# Higher = worse performance
rtt_raw <- wales_rtt_lhb |>
  rename(lhb20_code = lhb22_code) |>
  filter(date >= max(date) %m-% months(2)) # Last quarter

# Create dynamic label
min_date_rtt <- min(rtt_raw$date) |>
  format("%B %Y")
max_date_rtt <- max(rtt_raw$date) |>
  format("%B %Y")
rtt_label <- paste("Referral to treatment \nwaiting times \n(",
                      min_date_rtt, " - ", max_date_rtt, " average)",
                      sep = ""
)

rtt <- rtt_raw |>
  group_by(lhb20_code) |>
  summarise(
    number = mean(waits_over_18_weeks),
    total = mean(total_waits)
  ) |>
  mutate(
    percent = number/total,
    variable = rtt_label,
  ) |>
  relocate(variable, .before = number) |>
  select(-total)

# ---- Beds ----
# Higher = better performance
available_beds_raw <- wales_health_board_critical_general_acute_beds |>
  rename(lhb20_name = health_board_name) |>
  right_join(lhb) |>
  relocate(lhb20_code) |>
  mutate(date = dmy(paste("01-", date, sep = ""))) |>
  filter(date >= max(date) %m-% months(2)) # Last quarter

# Create dynamic label
min_date_bed <- min(available_beds_raw$date) |>
  format("%B %Y")
max_date_bed <- max(available_beds_raw$date) |>
  format("%B %Y")
bed_label <- paste("Bed availability\n(",
                   min_date_bed, " - ", max_date_bed, " average)",
                   sep = ""
)

available_beds <- available_beds_raw |>
  filter(specialism == "all_specialties") |>
  group_by(lhb20_code) |>
  summarise(
    beds_occupancy_rate = mean(as.numeric(beds_occupancy_rate), na.rm = TRUE),
    daily_beds_available = mean(as.numeric(daily_beds_available), na.rm = TRUE)
  ) |>
  mutate(beds_occupancy_rate = beds_occupancy_rate / 100) |>
  mutate(percent_available = 1 - beds_occupancy_rate) |>
  mutate(variable = bed_label) |>
  select(
    lhb20_code,
    variable,
    number = daily_beds_available,
    percent = percent_available
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
      variable == rtt_label ~ scaled_1_1 * -1,
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
      variable == rtt_label ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. waiting over 18 weeks: ", round(number),
        "<br>", "Percentage waiting over 18 weeks: ", round(percent * 100, 1), "%"
      ),
      variable == bed_label ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of available beds: ", round(number),
        "<br>", "Percentage of all beds available: ", round(percent * 100, 1), "%"
      )
    )
  )

usethis::use_data(wales_lhb_secondary_care, overwrite = TRUE)
