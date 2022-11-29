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
    general_acute_beds_availabile,
    adult_critical_care_beds_available
  ) |>
  mutate(available_beds = general_acute_beds_availabile + adult_critical_care_beds_available) |>
  select(nhs_trust22_code, month = date, available_beds)

# Divide criteria to reside by bed availability, matching by month
criteria_to_reside_trust <-
  england_criteria_to_reside |>
  mutate(
    month = str_c(
      as.character(month(date, label = TRUE, abbr = FALSE)),
      " 2022"
    )
  ) |>
  left_join(available_beds) |>
  mutate(perc_not_meet_criteria = do_not_meet_criteria_to_reside / available_beds) |>
  select(-month) |>
  left_join(trust_names) |>
  relocate(nhs_trust22_name) |>
  group_by(nhs_trust22_code) |>
  mutate(mean_perc_not_meet_criteria = mean(perc_not_meet_criteria)) |>
  ungroup() |>
  select(starts_with("nhs_"), criteria_to_reside = mean_perc_not_meet_criteria) |>
  distinct()

criteria_to_reside_ltla <-
  criteria_to_reside_trust |>
  left_join(lookup_nhs_trusts22_ltla21) |>
  mutate(proportion_criteria_to_reside = criteria_to_reside * proportion_trust_came_from_ltla) |>
  group_by(ltla21_code) |>
  summarise(criteria_to_reside = sum(proportion_criteria_to_reside))

# ---- Dishcarged patients ----
discharged_patients_trust <-
  england_discharged_patients |>
  mutate(
    month = str_c(
      as.character(month(date, label = TRUE, abbr = FALSE)),
      " 2022"
    )
  ) |>
  left_join(available_beds) |>
  mutate(perc_discharged_patients = discharged_total / available_beds) |>
  select(nhs_trust22_code, perc_discharged_patients) |>
  left_join(trust_names) |>
  relocate(nhs_trust22_name) |>
  group_by(nhs_trust22_code) |>
  mutate(mean_perc_discharged_patients = mean(perc_discharged_patients)) |>
  ungroup() |>
  select(starts_with("nhs_"), discharged_patients = mean_perc_discharged_patients) |>
  distinct()

discharged_patients_ltla <-
  discharged_patients_trust |>
  left_join(lookup_nhs_trusts22_ltla21) |>
  mutate(proportion_discharged_patients = discharged_patients * proportion_trust_came_from_ltla) |>
  group_by(ltla21_code) |>
  summarise(discharged_patients = sum(proportion_discharged_patients))

# ---- Bed Occupancy ----
bed_occupancy_ltla <-
  england_critical_general_acute_beds |>
  select(nhs_trust22_code, ends_with("rate")) |>
  group_by(nhs_trust22_code) |>
  summarise(across(ends_with("rate"), ~ mean(.x, na.rm = TRUE))) |>
  left_join(lookup_nhs_trusts22_ltla21) |>
  mutate(across(ends_with("rate"), ~ .x * proportion_trust_came_from_ltla)) |>
  group_by(ltla21_code) |>
  summarise(across(ends_with("rate"), ~ sum(.x, na.rm = TRUE))) |>
  select(ltla21_code, general_acute_beds_occupancy_rate)

# ---- IAPT ----
# Not enough data?
iapt_ltla <- england_iapt |>
  filter(name == "Percentage_FirstTreatment18WeeksFinishedCourseTreatment") |>
  select(nhs_trust22_code, iapt = value) |>
  mutate(iapt = as.double(iapt)) |>
  left_join(lookup_nhs_trusts22_ltla21) |>
  mutate(proportion_iapt = iapt * proportion_trust_came_from_ltla) |>
  group_by(ltla21_code) |>
  summarise(iapt = sum(proportion_iapt)) |>
  drop_na()

# ---- Combine & rename (pretty printing) ----
metrics_joined <- ltla |>
  left_join(discharged_patients_ltla) |>
  left_join(criteria_to_reside_ltla) |>
  left_join(bed_occupancy_ltla) |>
  pivot_longer(cols = !starts_with("ltla21_"), names_to = "variable") |>
  select(-ltla21_code) |>
  mutate(ltla21_name = str_replace_all(ltla21_name, "'", "")) |>
  rename(area_name = ltla21_name) |>
  mutate(
    variable = case_when(
      variable == "discharged_patients" ~ "Rate of discharged \nbeds (Apr-Oct)",
      variable == "criteria_to_reside" ~ "Rate of beds \nnot meeting criteria \nto reside (Apr-Oct)",
      variable == "general_acute_beds_occupancy_rate" ~ "Rate of acute bed \noccupancy (Apr-Oct)"
    )
  )

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

ltla_secondary_care_england <-
  metrics_joined |>
  group_by(variable) |>
  mutate(value = scale_1_1(value)) |>
  ungroup()

# Check distributions
ltla_secondary_care_england |>
  ggplot(aes(x = value, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

usethis::use_data(ltla_secondary_care_england, overwrite = TRUE)

# Datasets to add:
#   - Reattendance
#   - Ambulance waiting times
#   - Social care demand / need
#   - RTT
#   - A & E
