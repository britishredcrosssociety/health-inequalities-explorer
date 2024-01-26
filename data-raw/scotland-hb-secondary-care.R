# ---- Load libs & helpers ----
library(tidyverse)
library(healthyr)
library(geographr)
library(sf)
library(ggridges)

hb <- boundaries_hb19 |>
  st_drop_geometry()

# Popuation counts
# Source: https://www.opendata.nhs.scot/dataset/population-estimates
pop_raw <- read_csv(
  "https://www.opendata.nhs.scot/datastore/dump/27a72cc8-d6d8-430c-8b4f-3109a9ceadb1?bom=True"
)

pop_scotland <- pop_raw |>
  filter(Year == 2021) |>
  filter(Sex == "All") |>
  filter(HB != "S92000003") |>
  select(hb19_code = HB, population = AllAges)

# ---- RTT ----
# Higher = worse performance
rtt <- scotland_rtt_hb |>
  filter(date >= max(date) %m-% months(2)) # Last quarter

# Create dynamic label
min_date_rtt <- min(rtt$date) |>
  format("%B %Y")
max_date_rtt <- max(rtt$date) |>
  format("%B %Y")
rtt_label <- paste("Referral to treatment \nwaiting times\n(", min_date_rtt, " - ", max_date_rtt, " average)", sep = "")

rtt <- rtt |>
  filter(hb19_code != "SB0801") |>
  group_by(hb19_code) |>
  summarise(
    number = mean(waits_over_18_weeks_count),
    percent = mean(waits_over_18_weeks_percent)
  ) |>
  mutate(
    variable = rtt_label,
    .after = hb19_code
  )

# ---- Beds ----
# Higher = better performance
available_beds <- scotland_beds |>
  filter(date >= max(date)) # Note: data is quarterly

# Create dynamic label
min_date_bed <- max(available_beds$date) %m-% months(2) |>
  format("%B %Y")
max_date_bed <- max(available_beds$date) |>
  format("%B %Y")
bed_label <- paste("Bed availability\n(", min_date_bed, " - ", max_date_bed, " average)", sep = "")

available_beds <- available_beds |>
  filter(specialty == "All Specialties") |>
  mutate(percent_avilable = 1 - percent_occupied_beds) |>
  mutate(variable = bed_label) |>
  select(
    hb19_code,
    variable,
    number = average_number_available_staffed_beds,
    percent = percent_avilable
  )

# ---- Delayed discharges ----
# Higher = worse
delayed_discharge_monthly <- scotland_delayed_discharge_hb |>
  filter(hb_code != "S92000003") |>
  filter(date >= max(date) %m-% months(2))  # Last quarter

# Create dynamic label
min_date_delayed <- max(delayed_discharge_monthly$date) %m-% months(2) |>
  format("%B %Y")
max_date_delayed <- max(delayed_discharge_monthly$date) |>
  format("%B %Y")
delayed_label <- paste("Delayed discharge\n(", min_date_delayed, " - ", max_date_delayed, " average)", sep = "")

delayed_discharge_monthly <- delayed_discharge_monthly |>
  filter(age_group == "18-74" | age_group == "75plus") |>
  filter(delay_reason == "All Delay Reasons") |>
  rename(hb19_code = hb_code) |>
  group_by(hb19_code, date) |>
  summarise(
    num_delayed_bed_days = sum(num_delayed_bed_days),
    average_daily_delayed_beds = sum(average_daily_delayed_beds)
  ) |>
  ungroup()

delayed_discharge_summary <- delayed_discharge_monthly |>
  group_by(hb19_code) |>
  summarise(
    num_delayed_bed_days = mean(num_delayed_bed_days),
    average_daily_delayed_beds = mean(average_daily_delayed_beds)
  ) |>
  select(-num_delayed_bed_days)

delayed_discharges <- delayed_discharge_summary |>
  left_join(pop_scotland) |>
  mutate(beds_per_10000 = average_daily_delayed_beds / population * 10000) |>
  select(
    hb19_code,
    number = average_daily_delayed_beds,
    percent = beds_per_10000
  ) |>
  mutate(
    variable = delayed_label,
    .after = hb19_code
  )

# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  rtt,
  available_beds,
  delayed_discharges
) |>
  left_join(hb) |>
  select(-hb19_code) |>
  rename(area_name = hb19_name) |>
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
# Flip RTT and delayed discharges
secondary_care_polarised <- secondary_care_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable == rtt_label ~ scaled_1_1 * -1,
      variable == delayed_label ~ scaled_1_1 * -1,
      TRUE ~ scaled_1_1
    )
  )

# Check distributions
secondary_care_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

# ---- Add plot labels ----
scotland_hb_secondary_care <- secondary_care_polarised |>
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
      ),
      variable == delayed_label ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Average daily no. of delayed beds: ", round(number),
        "<br>", "Average daily no. of delayed beds per 10,000 people: ", round(percent, 1)
      )
    )
  )

usethis::use_data(scotland_hb_secondary_care, overwrite = TRUE)
