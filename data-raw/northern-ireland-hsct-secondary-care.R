# Northern Ireland - Secondary Care Indicators
# Health and Social Care Trusts

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

# Population counts
# Source: https://www.ninis2.nisra.gov.uk/public/AreaProfileReportViewer.aspx?FromAPAddressMulipleRecords=Northern@@Northern@20?

url <- "https://www.ninis2.nisra.gov.uk/Download/Population/Population%20Estimates%20Broad%20Age%20Bands%20(administrative%20geographies).ods"

pop_raw <- download_file(url, ".ods")

pop_ni_hsct <- read_ods(
  pop_raw,
  sheet = "HSCT",
  range = "A4:C9"
) |>
  select(
    trust18_name = "HSCT",
    trust18_code = "HSCT Code",
    population = "Persons"
  )

# ---- RTT ----
# Higher = worse performance
rtt <- ni_rtt_hsct |>
  select(
    trust18_name = "hsct22_name",
    date,
    waits_over_18_weeks
  ) |>
  filter(date >= max(date) %m-% months(3)) |> # Last 3 months
  left_join(pop_ni_hsct) |>
  group_by(trust18_code, date) |>
  mutate(waits_over_18_weeks = sum(waits_over_18_weeks)) |>
  mutate(percent = waits_over_18_weeks / population) |>
  group_by(trust18_name) |>
  summarise(
    number = sum(waits_over_18_weeks),
    percent = mean(percent, na.rm = FALSE)
  ) |>
  mutate(
    variable = "Referral to treatment \nwaiting times (Mar 22 - Jun 22)",
    .after = trust18_name
  ) |>
  filter(!trust18_name %in% c("DPC", "Day Case Procedure Centre"))

# ---- Beds ----
# Higher = better performance
available_beds <- ni_beds |>
  left_join(hsct, by = "trust18_code") |>
  mutate(
    percent_available = total_available_beds /
      (total_available_beds + total_occupied_beds)
  ) |>
  filter(date >= max(date) %m-% months(2)) |> # Last quarter
  group_by(trust18_name) |>
  summarise(
    number = sum(total_available_beds),
    percent = mean(percent_available, na.rm = TRUE)
  ) |>
  mutate(
    variable = "Bed availability \n(Q1 2022)",
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
      variable == "Referral to treatment \nwaiting times (Mar 22 - Jun 22)" ~ scaled_1_1 * -1,
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
      variable == "Referral to treatment \nwaiting times (Mar 22 - Jun 22)" ~
        paste0(
          "<b>", area_name, "</b>",
          "<br>",
          "<br>", "No. waiting over 18 weeks: ", round(number),
          "<br>", "Percentage waiting over 18 weeks: ", round(percent * 100, 1),
          "%"
        ),
      variable == "Bed availability \n(Q1 2022)" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of available beds: ", round(number),
        "<br>", "Percentage of all beds available: ", round(percent * 100, 1),
        "%"
      )
    )
  )

usethis::use_data(northern_ireland_hsct_secondary_care, overwrite = TRUE)
