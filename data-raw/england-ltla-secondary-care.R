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
criteria_to_reside_trust_filtered <-
  england_trust_criteria_to_reside |>
  mutate(
    month = str_c(
      as.character(month(date, label = TRUE, abbr = FALSE)),
      year(date),
      sep = " "
    )
  ) |>
  left_join(available_beds) |>
  mutate(perc_not_meet_criteria = coalesce(do_not_meet_criteria_to_reside / available_beds, 0)) |>
  select(-month) |>
  filter(date >= max(date) %m-% months(2))  # Last quarter

# Create dynamic label
min_date_reside <- min(criteria_to_reside_trust_filtered$date) |>
  format("%B %Y")
max_date_reside <- max(criteria_to_reside_trust_filtered$date) |>
  format("%B %Y")
reside_label <- paste("Beds not meeting \ncriteria to reside \n(", 
  min_date_reside, " - ", max_date_reside, " average)", sep = "")

criteria_to_reside_trust <- criteria_to_reside_trust_filtered |>
  left_join(trust_names) |>
  relocate(nhs_trust22_name) |>
  group_by(nhs_trust22_code) |>
  mutate(
    mean_not_meet_criteria = mean(do_not_meet_criteria_to_reside, na.rm = TRUE),
    mean_available_beds = mean(available_beds, na.rm = TRUE),
    mean_perc_not_meet_criteria = mean(perc_not_meet_criteria, na.rm = TRUE)
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
    variable = reside_label,
    .after = ltla21_code
  )

# ---- Discharged patients ----
discharged_patients_trust_filtered <-
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
  filter(date >= max(date) %m-% months(3)) # Last quarter
  
# Create dynamic label
min_date_discharged <- min(discharged_patients_trust_filtered$date) |>
  format("%B %Y")
max_date_discharged <- max(discharged_patients_trust_filtered$date) |>
  format("%B %Y")
discharged_label <- paste("Discharged beds \n(", 
                      min_date_reside, " - ", max_date_reside, " average)", sep = "")
  
discharged_patients_trust <- discharged_patients_trust_filtered |>  
  select(nhs_trust22_code, percent_discharged, number_discharged = discharged_total) |>
  left_join(trust_names) |>
  relocate(nhs_trust22_name) |>
  group_by(nhs_trust22_code) |>
  mutate(
    mean_number_discharged = mean(number_discharged, na.rm = TRUE),
    mean_percentage_discharged = mean(percent_discharged, na.rm = TRUE)
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
    number = sum(proportion_mean_number_discharged, na.rm = TRUE),
    percent = sum(proportion_mean_percentage_discharged, na.rm = TRUE)
  ) |>
  mutate(
    variable = discharged_label,
    .after = ltla21_code
  )

# ---- Bed Occupancy ----
bed_occupancy_trust_filtered <-
  england_critical_general_acute_beds |>
  select(nhs_trust22_code, date, ends_with("occupied"), ends_with("available")) |>
  pivot_longer(cols = !c(nhs_trust22_code, date)) |>
  mutate(type = if_else(str_detect(name, "_occupied$"), "occupied", "available")) |>
  mutate(date = my(date)) |>
  filter(date >= max(date) %m-% months(2)) # Last quarter
  
# Create dynamic label
min_date_occupancy <- min(bed_occupancy_trust_filtered$date) |>
  format("%B %Y")
max_date_occupancy <- max(bed_occupancy_trust_filtered$date) |>
  format("%B %Y")
occupancy_label <- paste("Bed availability \n(", 
                          min_date_reside, " - ", max_date_reside, " average)", sep = "")

bed_occupancy_trust <- bed_occupancy_trust_filtered |>
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
    number = sum(number, na.rm = TRUE),
    percent = sum(percent, na.rm = TRUE)
  ) |>
  mutate(
    variable = occupancy_label,
    .after = ltla21_code
  )

# ---- A&E attendances over 4 hours ----
# Data is collected for all A&E types including walk-in centres and
# minor injury units. Only acute hospitals are mapped to LTLAs; therefore
# A&E types are mapped to acute hospitals.

attendances_4hours_trust_filtered <- england_trust_accidents_emergency |>
  mutate(date = parse_date_time(date, orders = "B Y")) |>
  filter(date >= max(date) %m-% months(2))  # Last quarter

# Create dynamic label
min_date_aande <- min(attendances_4hours_trust_filtered$date) |>
  format("%B %Y")
max_date_aande <- max(attendances_4hours_trust_filtered$date) |>
  format("%B %Y")
aande_label <- paste("A&E attendances over 4 hours \n(", 
                          min_date_reside, " - ", max_date_reside, " average)", sep = "")

attendances_4hours_trust <- attendances_4hours_trust_filtered |>
  select(nhs_trust22_code, attendances_over_4hours, total_attendances) |>
  group_by(nhs_trust22_code) |>
  summarise(
    number_mean = mean(attendances_over_4hours, na.rm = TRUE),
    total_mean = mean(total_attendances, na.rm = TRUE)
  )

# Apportion attendances from non-acute trusts to acute trusts
acute_trusts <- england_ae_acute_trust_attribution |>
  filter(nhs_trust22_code_all %in% attendances_4hours_trust$nhs_trust22_code) |>
  left_join(attendances_4hours_trust, by = c("nhs_trust22_code_all" = "nhs_trust22_code")) |>
  mutate(
    attributed_number_acute = number_mean * proportion_attendances_attributed_to_acute_trust,
    attributed_total_acute = total_mean * proportion_attendances_attributed_to_acute_trust
  ) |>
  group_by(nhs_trust22_code_acute) |>
  summarise(
    attributed_number_acute = sum(attributed_number_acute, na.rm = TRUE),
    attributed_total_acute = sum(attributed_total_acute, na.rm = TRUE)
  )

# Add NHS trust names
attendances_4hours_acute <- acute_trusts |>
  left_join(trust_names, by = c("nhs_trust22_code_acute" = "nhs_trust22_code")) |>
  mutate(percent = attributed_number_acute / attributed_total_acute) |>
  select(-attributed_total_acute)

# Join to LTLA
attendances_4hours_ltla <- attendances_4hours_acute |>
  left_join(lookup_nhs_trusts22_ltla21, c("nhs_trust22_code_acute" = "nhs_trust22_code")) |>
  mutate(
    proportion_number = proportion_trust_came_from_ltla * attributed_number_acute, 
    proportion_percentage = proportion_trust_came_from_ltla * percent
  ) |>
  group_by(ltla21_code) |>
  summarise(
    number = sum(proportion_number, na.rm = TRUE),
    percent = sum(proportion_percentage, na.rm = TRUE)
  ) |>
  mutate(
    variable = aande_label,
    .after = ltla21_code
  ) |>
  filter(!is.na(ltla21_code))


# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  criteria_to_reside_ltla,
  discharged_patients_ltla,
  bed_occupancy_ltla,
  attendances_4hours_ltla
) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name)

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
england_ltla_secondary_care_polarised <- ltla_secondary_care_england_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable %in% c(
        reside_label,
        aande_label
      )
      ~ scaled_1_1 * -1,
      TRUE ~ scaled_1_1
    )
  )

# Check distributions
england_ltla_secondary_care_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

# ---- Add plot labels ----
england_ltla_secondary_care <- england_ltla_secondary_care_polarised |>
  mutate(
    label = case_when(
      variable == occupancy_label ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of available beds: ", round(number),
        "<br>", "Percentage of all beds available: ", round(percent * 100, 1), "%"
      ),
      variable == reside_label ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of beds not meeting criteria to reside: ", round(number),
        "<br>", "Percentage of all beds not meeting criteria to reside: ", round(percent * 100, 1), "%"
      ),
      variable == discharged_label ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of discharged beds: ", round(number),
        "<br>", "Percentage of all beds discharged: ", round(percent * 100, 1), "%"
      ),
      variable == aande_label ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of A&E patients that spend over 4 hours from arrival to admission, transfer or discharge: ", round(number),
        "<br>", "Percentage of all A&E patients that spend over 4 hours from arrival to admission, transfer or discharge: ", round(percent * 100, 1), "%"
      )
    )
  )

usethis::use_data(england_ltla_secondary_care, overwrite = TRUE)
