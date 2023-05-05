library(tidyverse)
library(lubridate)
library(geographr)
library(sf)
library(healthyr)
library(compositr)
library(ggridges)

trust_names <-
  points_nhs_trusts22 |>
  st_drop_geometry() |>
  select(starts_with("nhs_"))

ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^E"))

# ---- Criteria to reside ----
# The hospital discharge data states:
# This SitRep collects data for all inpatients 18 and over including critical
# care and COVID-19 positive patients but excluding paediatrics, maternity, and
# deceased patients. This includes data for acute trusts with a type 1 A&E
# department. Mental Health Trusts, specialised Trusts (including Children’s and
# Women’s Trusts) are not in scope of this collection.

# Match available bed numbers to requirements above (18+ including critical)
available_beds <-
  england_critical_general_acute_beds |>
  select(
    nhs_trust22_code,
    date,
    general_acute_beds_available,
    adult_critical_care_beds_available
  ) |>
  mutate(available_beds = general_acute_beds_available + adult_critical_care_beds_available) |>
  select(nhs_trust22_code, month = date, available_beds)

# Divide criteria to reside by bed availability, matching by month
# These figures are daily, not monthly figures:
criteria_to_reside_trust <-
  england_trust_criteria_to_reside |>
  mutate(
    month = str_c(
      as.character(month(date, label = TRUE, abbr = FALSE)),
      year(date),
      sep = " "
    )
  ) |>
  left_join(available_beds) |>
  mutate(perc_not_meet_criteria = do_not_meet_criteria_to_reside / available_beds) |>
  select(-month) |>
  filter(date >= max(date) %m-% months(3)) |> # Last quarter
  left_join(trust_names) |>
  relocate(nhs_trust22_name) |>
  group_by(nhs_trust22_code) |>
  mutate(
    mean_not_meet_criteria = mean(do_not_meet_criteria_to_reside),
    mean_available_beds = mean(available_beds),
    mean_perc_not_meet_criteria = mean(perc_not_meet_criteria)
  ) |>
  ungroup() |>
  select(starts_with("nhs_"), starts_with("mean_")) |>
  distinct()

criteria_to_reside_ltla <-
  criteria_to_reside_trust |>
  left_join(lookup_nhs_trusts22_ltla21) |>
  mutate(
    proportion_mean_not_meet_criteria = proportion_trust_came_from_ltla * mean_not_meet_criteria,
    proportion_mean_available_beds = proportion_trust_came_from_ltla * mean_available_beds,
    proportion_mean_perc_not_meet_criteria = proportion_trust_came_from_ltla * mean_perc_not_meet_criteria
  ) |>
  group_by(ltla21_code) |>
  summarise(
    number = sum(proportion_mean_not_meet_criteria),
    percent = sum(proportion_mean_perc_not_meet_criteria)
  ) |>
  mutate(
    variable = "Beds not meeting \ncriteria to reside \n(Jan 23 - Mar 23 average)",
    .after = ltla21_code
  )

# ---- Dishcarged patients ----
discharged_patients_trust <-
  england_trust_discharged_patients |>
  mutate(
    month = str_c(
      as.character(month(date, label = TRUE, abbr = FALSE)),
      year(date),
      sep = " "
    )
  ) |>
  left_join(available_beds) |>
  mutate(percent_discharged = discharged_total / available_beds) |>
  filter(date >= max(date) %m-% months(3)) |> # Last quarter
  select(nhs_trust22_code, percent_discharged, number_discharged = discharged_total) |>
  left_join(trust_names) |>
  relocate(nhs_trust22_name) |>
  group_by(nhs_trust22_code) |>
  mutate(
    mean_number_discharged = mean(number_discharged),
    mean_percentage_discharged = mean(percent_discharged)
  ) |>
  ungroup() |>
  select(starts_with("nhs_"), starts_with("mean_")) |>
  distinct()

discharged_patients_ltla <-
  discharged_patients_trust |>
  left_join(lookup_nhs_trusts22_ltla21) |>
  mutate(
    proportion_mean_number_discharged = proportion_trust_came_from_ltla * mean_number_discharged,
    proportion_mean_percentage_discharged = proportion_trust_came_from_ltla * mean_percentage_discharged
  ) |>
  group_by(ltla21_code) |>
  summarise(
    number = sum(proportion_mean_number_discharged),
    percent = sum(proportion_mean_percentage_discharged)
  ) |>
  mutate(
    variable = "Discharged beds \n(Jan 23 - Mar 23 average)",
    .after = ltla21_code
  )

# ---- Bed Occupancy ----
bed_occupancy_trust <-
  england_critical_general_acute_beds |>
  select(nhs_trust22_code, date, ends_with("occupied"), ends_with("available")) |>
  pivot_longer(cols = !c(nhs_trust22_code, date)) |>
  mutate(type = if_else(str_detect(name, "_occupied$"), "occupied", "available")) |>
  mutate(date = my(date)) |>
  filter(date >= max(date) %m-% months(2)) |> # Last quarter
  group_by(nhs_trust22_code, date, type) |>
  summarise(all_beds = sum(value, na.rm = TRUE)) |>
  group_by(nhs_trust22_code, type) |>
  summarise(all_beds = mean(all_beds, na.rm = TRUE)) |>
  ungroup() |>
  pivot_wider(names_from = type, values_from = all_beds) |>
  mutate(
    number = available - occupied,
    percent = number / available
  ) |>
  select(nhs_trust22_code, number, percent)

bed_occupancy_ltla <-
  bed_occupancy_trust |>
  left_join(lookup_nhs_trusts22_ltla21) |>
  mutate(
    number = proportion_trust_came_from_ltla * number,
    percent = proportion_trust_came_from_ltla * percent
  ) |>
  group_by(ltla21_code) |>
  summarise(
    number = sum(number),
    percent = sum(percent)
  ) |>
  mutate(
    variable = "Bed availability \n(Jan 23 - Mar 23 average)",
    .after = ltla21_code
  )

# ---- IAPT ----
iapt_ltla <- england_trust_iapt |>
  filter(name == "Percentage_AccessingServices18WeeksFinishedCourseTreatment") |>
  mutate(value = as.numeric(value)) |>
  mutate(date = my(date)) |>
  filter(date >= max(date) %m-% months(2)) |> # Last quarter
  select(nhs_trust22_code, iapt = value) |>
  group_by(nhs_trust22_code) |>
  summarise(iapt = mean(iapt)) |>
  mutate(iapt = iapt / 100) |> # Convert percentage to decimal
  right_join(lookup_nhs_trusts22_ltla21) |>
  mutate(proportion_iapt = iapt * proportion_trust_came_from_ltla) |>
  group_by(ltla21_code) |>
  summarise(iapt = sum(proportion_iapt, na.rm = TRUE)) |>
  mutate(
    variable = "Talking therapies: \nfinished a course of \ntreatment in 18 weeks \n(Nov 22 - Jan 23 average)",
    number = NA
  ) |>
  select(ltla21_code, variable, number, percent = iapt)

# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  criteria_to_reside_ltla,
  discharged_patients_ltla,
  bed_occupancy_ltla,
  iapt_ltla
) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name) |>
  mutate(geography_type = "LTLA") |>
  mutate(data_type = "Secondary care") |>
  relocate(geography_type, data_type, .after = area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

ltla_secondary_care_england_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(scaled_1_1 = scale_1_1(percent)) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip criteria to reside, as currently higher = worse health
england_ltla_secondary_care <- ltla_secondary_care_england_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Beds not meeting \ncriteria to reside \n(Jan 23 - Mar 23 average)" ~ scaled_1_1 * -1,
      TRUE ~ scaled_1_1
    )
  )

# Check distributions
england_ltla_secondary_care |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

usethis::use_data(england_ltla_secondary_care, overwrite = TRUE)
