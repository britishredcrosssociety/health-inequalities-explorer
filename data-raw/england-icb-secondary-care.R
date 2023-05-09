# ---- Load libs ----
library(tidyverse)
library(lubridate)
library(healthyr)
library(geographr)
library(sf)
library(ggridges)

# ---- Create lookups ----
icb <- boundaries_icb22 |>
  st_drop_geometry() |>
  mutate(icb22_name = str_remove_all(icb22_name, "^NHS ")) |>
  mutate(icb22_name = str_remove_all(icb22_name, " Integrated Care Board$"))

sicb_icb <- lookup_lsoa11_sicbl22_icb22_ltla22 |>
  distinct(sicbl22_code, icb22_code)

# ---- Bed occupancy ----
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

bed_occupancy_icb <-
  bed_occupancy_trust |>
  left_join(lookup_nhs_trusts22_icb22) |>
  mutate(
    number = proportion_trust_came_from_icb * number,
    percent = proportion_trust_came_from_icb * percent
  ) |>
  group_by(icb22_code) |>
  summarise(
    number = sum(number),
    percent = sum(percent)
  ) |>
  mutate(
    variable = "Bed availability \n(Jan 23 - Mar 23 average)",
    .after = icb22_code
  )

# ---- Criteria to reside ----
# The hospital discharge data states:
# This SitRep collects data for all inpatients 18 and over including critical
# care and COVID-19 positive patients but excluding paediatrics, maternity, and
# deceased patients. This includes data for acute trusts with a type 1 A&E
# department. Mental Health Trusts, specialised Trusts (including Children’s and
# Women’s Trusts) are not in scope of this collection.

# Match available bed numbers to requirements above (18+ including critical)
available_trust_beds <-
  england_critical_general_acute_beds |>
  select(
    nhs_trust22_code,
    date,
    general_acute_beds_available,
    adult_critical_care_beds_available
  ) |>
  mutate(available_beds = general_acute_beds_available + adult_critical_care_beds_available) |>
  select(nhs_trust22_code, month = date, available_beds)

available_icb_beds <-
  available_trust_beds |>
  left_join(lookup_nhs_trusts22_icb22) |>
  mutate(available_beds = proportion_trust_came_from_icb * available_beds) |>
  group_by(icb22_code, month) |>
  summarise(available_beds = sum(available_beds, na.rm = TRUE)) |>
  ungroup()

criteria_to_reside_icb <-
  england_icb_criteria_to_reside |>
  mutate(
    month = str_c(
      as.character(month(date, label = TRUE, abbr = FALSE)),
      year(date),
      sep = " "
    )
  ) |>
  left_join(available_icb_beds) |>
  mutate(perc_not_meet_criteria = do_not_meet_criteria_to_reside / available_beds) |>
  filter(date >= max(date) %m-% months(3)) |>
  group_by(icb22_code) |>
  summarise(
    number = mean(do_not_meet_criteria_to_reside),
    percent = mean(perc_not_meet_criteria)
  ) |>
  mutate(
    variable = "Beds not meeting \ncriteria to reside \n(Jan 23 - Mar 23 average)",
    .after = icb22_code
  )

# ---- Discharged patients ----
discharged_patients_icb <-
  england_icb_discharged_patients |>
  mutate(
    month = str_c(
      as.character(month(date, label = TRUE, abbr = FALSE)),
      year(date),
      sep = " "
    )
  ) |>
  left_join(available_icb_beds) |>
  mutate(percent_discharged = discharged_total / available_beds) |>
  filter(date >= max(date) %m-% months(3)) |>
  group_by(icb22_code) |>
  summarise(
    number = mean(discharged_total),
    percent = mean(percent_discharged)
  ) |>
  mutate(
    variable = "Discharged beds \n(Jan 23 - Mar 23 average)",
    .after = icb22_code
  )

# ---- IAPT ----
iapt_icb_aggregated <- england_sicb_iapt |>
  filter(name == "Percentage_AccessingServices18WeeksFinishedCourseTreatment") |>
  mutate(value = as.numeric(value)) |>
  left_join(sicb_icb) |>
  group_by(icb22_code, date, name) |>
  summarise(value = mean(value)) |>
  ungroup()

iapt_icb <- iapt_icb_aggregated |>
  mutate(date = my(date)) |>
  filter(date >= max(date) %m-% months(2)) |>
  group_by(icb22_code) |>
  summarise(percent = mean(value)) |>
  mutate(percent = percent / 100) |>
  mutate(
    variable = "Talking therapies: \nfinished a course of \ntreatment in 18 weeks \n(Nov 22 - Jan 23 average)",
    number = NA
  ) |>
  relocate(percent, .after = number)

# ---- Combine ----
joined <-
  bind_rows(
    bed_occupancy_icb,
    criteria_to_reside_icb,
    discharged_patients_icb,
    iapt_icb
  ) |>
  left_join(icb) |>
  select(-icb22_code) |>
  rename(area_name = icb22_name) |>
  relocate(area_name) |>
  mutate(geography_type = "ICB") |>
  mutate(data_type = "Secondary care") |>
  relocate(geography_type, data_type, .after = area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

icb_secondary_care_england_scaled <-
  joined |>
  group_by(variable) |>
  mutate(scaled_1_1 = scale_1_1(percent)) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip criteria to reside, as currently higher = worse health
england_icb_secondary_care <- icb_secondary_care_england_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Beds not meeting \ncriteria to reside \n(Dec 22 - Feb 23 average)" ~ scaled_1_1 * -1,
      TRUE ~ scaled_1_1
    )
  )

# Check distributions
england_icb_secondary_care |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

usethis::use_data(england_icb_secondary_care, overwrite = TRUE)