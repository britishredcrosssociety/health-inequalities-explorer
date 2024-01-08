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
# Yeovil District Hospital NHS Foundation Trust -- code RA4 -- was merged into
# Somerset NHS Foundation Trust -- code RH5 -- in April 2023
bed_occupancy_trust <-
  england_critical_general_acute_beds |>
  select(
    nhs_trust22_code,
    date,
    ends_with("occupied"),
    ends_with("available")
  ) |>
  pivot_longer(cols = !c(nhs_trust22_code, date)) |>
  mutate(type = if_else(str_detect(name, "_occupied$"),
                        "occupied",
                        "available"
  )) |>
  mutate(date = my(date)) |>
  filter(date >= max(date) %m-% months(2)) # Last quarter

# Create dynamic label
min_date_bed <- min(bed_occupancy_trust$date) |>
  format("%B %Y")
max_date_bed <- max(bed_occupancy_trust$date) |>
  format("%B %Y")
bed_label <- paste("Bed availability \n(", min_date_bed, " - ", max_date_bed, " average)", sep = "")

bed_occupancy_grouped <- bed_occupancy_trust |>
  filter(nhs_trust22_code != "RA4") |> # Account for trusts merger
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
  bed_occupancy_grouped |>
  left_join(lookup_nhs_trusts22_icb22) |>
  mutate(
    number = proportion_trust_came_from_icb * number,
    percent = proportion_trust_came_from_icb * percent
  ) |>
  group_by(icb22_code) |>
  summarise(
    number = sum(number, na.rm = TRUE),
    percent = sum(percent, na.rm = TRUE)
  ) |>
  mutate(
    variable = bed_label,
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

criteria_to_reside_icb_ungrouped <-
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
  filter(date >= max(date) %m-% months(2))

# Create dynamic label
min_date_reside <- min(criteria_to_reside_icb_ungrouped$date) |>
  format("%B %Y")
max_date_reside <- max(criteria_to_reside_icb_ungrouped$date) |>
  format("%B %Y")
reside_label <- paste("Beds not meeting \ncriteria to reside\n(", min_date_reside, " - ", max_date_reside, " average)", sep = "")

criteria_to_reside_icb <- criteria_to_reside_icb_ungrouped |>
  group_by(icb22_code) |>
  summarise(
    number = mean(do_not_meet_criteria_to_reside, na.rm = TRUE),
    percent = mean(perc_not_meet_criteria, na.rm = TRUE)
  ) |>
  mutate(
    variable = reside_label,
    .after = icb22_code
  )

# ---- Discharged patients ----
discharged_patients_icb_ungrouped <-
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
  filter(date >= max(date) %m-% months(2))

# Create dynamic label
min_date_discharged <- min(discharged_patients_icb_ungrouped$date) |>
  format("%B %Y")
max_date_discharged <- max(discharged_patients_icb_ungrouped$date) |>
  format("%B %Y")
discharged_patients_label <- paste("Discharged beds \n(", min_date_discharged, " - ", max_date_discharged, " average)", sep = "")

discharged_patients_icb <- discharged_patients_icb_ungrouped |>
  group_by(icb22_code) |>
  summarise(
    number = mean(discharged_total, na.rm = TRUE),
    percent = mean(percent_discharged, na.rm = TRUE)
  ) |>
  mutate(
    variable = discharged_patients_label,
    .after = icb22_code
  )

# ---- IAPT ----
iapt_icb_aggregated <- england_sicb_iapt |>
  filter(name == "Percentage_AccessingServices18WeeksFinishedCourseTreatment") |>
  mutate(value = as.numeric(value)) |>
  left_join(sicb_icb) |>
  group_by(icb22_code, date, name) |>
  summarise(value = mean(value), na.rm = TRUE) |>
  ungroup() |>
  mutate(date = my(date)) |>
  filter(date >= max(date) %m-% months(2))

# Create dynamic label
min_date_iapt <- min(iapt_icb_aggregated$date) |>
  format("%B %Y")
max_date_iapt <- max(iapt_icb_aggregated$date) |>
  format("%B %Y")
iapt_label <- paste("Talking therapies: \nfinished a course of \ntreatment in 18 weeks \n(", min_date_iapt, " - ", max_date_iapt, " average)", sep = "")

iapt_icb <- iapt_icb_aggregated |>
  group_by(icb22_code) |>
  summarise(percent = mean(value, na.rm = TRUE)) |>
  mutate(percent = percent / 100) |>
  mutate(
    variable = iapt_label,
    number = NA
  ) |>
  relocate(percent, .after = number)

# ---- A&E attendances over 4 hours ----
attendances_4_hours_ungrouped <- england_icb_accidents_emergency |>
  mutate(date = parse_date_time(date, orders = "B Y")) |>
  filter(date >= max(date) %m-% months(2))

# Create dynamic label
min_date_aande <- min(attendances_4_hours_ungrouped$date) |>
  format("%B %Y")
max_date_aande <- max(attendances_4_hours_ungrouped$date) |>
  format("%B %Y")
aande_label <- paste("A&E attendances over 4 hours \n(", min_date_aande, " - ",
  max_date_aande, " average)",
  sep = ""
)

attendances_4_hours_icb <- attendances_4_hours_ungrouped |>
  select(icb22_code, attendances_over_4hours, pct_attendance_over_4hours) |>
  rename(number = attendances_over_4hours, percent = pct_attendance_over_4hours) |>
  group_by(icb22_code) |>
  summarise(
    number = mean(number, na.rm = TRUE),
    percent = mean(percent, na.rm = TRUE)
  ) |>
  mutate(
    variable = aande_label,
    .after = icb22_code
  )

# ---- Combine ----
joined <-
  bind_rows(
    bed_occupancy_icb,
    criteria_to_reside_icb,
    discharged_patients_icb,
    iapt_icb,
    attendances_4_hours_icb
  ) |>
  left_join(icb) |>
  select(-icb22_code) |>
  rename(area_name = icb22_name) |>
  relocate(area_name)

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
england_icb_secondary_care_polarised <- icb_secondary_care_england_scaled |>
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
england_icb_secondary_care_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

# ---- Add plot labels ----
england_icb_secondary_care <- england_icb_secondary_care_polarised |>
  mutate(
    label = case_when(
      variable == bed_label ~ paste0(
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
      variable == discharged_patients_label ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of discharged beds: ", round(number),
        "<br>", "Percentage of all beds discharged: ", round(percent * 100, 1), "%"
      ),
      variable == iapt_label ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Percentage that finished treatment: ", round(percent * 100, 1), "%"
      ),
      variable == aande_label ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of A&E waits of over four hours: ", round(number),
        "<br>", "Percentage of A&E waits of over four hours: ", round(percent * 100, 1), "%"
      )
    )
  )

usethis::use_data(england_icb_secondary_care, overwrite = TRUE)
