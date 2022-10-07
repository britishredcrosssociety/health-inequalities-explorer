library(tidyverse)
library(lubridate)
library(geographr)
library(sf)
library(healthyr)
library(compositr)

# ---- Trust to ltla lookup ----
raw <- download_file(
  "https://github.com/britishredcrosssociety/resilience-index/blob/main/data/lookup_trust_lad.rds?raw=true",
  ".rds"
)

trust_ltla_lookup <- read_rds(raw)

# ---- Trust names ----
trust_names <-
  points_nhs_trusts22 |>
  st_drop_geometry() |>
  select(starts_with("nhs_"))

# ---- Criteria to reside ----
# The hospital discharge data states:
# This SitRep collects data for all inpatients 18 and over including critical
# care and COVID-19 positive patients but excluding paediatrics, maternity, and
# deceased patients. This includes data for acute trusts with a type 1 A&E
# department. Mental Health Trusts, specialised Trusts (including Children’s and
# Women’s Trusts) are not in scope of this collection.

# Match available bed numbers to requirements above (18+ including critical)
available_beds <-
  nhs_critical_general_acute_beds_22 |>
  select(
    nhs_trust22_code,
    date,
    general_acute_beds_availabile,
    adult_critical_care_beds_available
  ) |>
  mutate(available_beds = general_acute_beds_availabile + adult_critical_care_beds_available) |>
  select(nhs_trust22_code, month = date, available_beds)

# Divide criteria to reside by bed availability, matching by month
criteria_to_reside <-
  nhs_discharge_criteria_22 |>
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

# ---- Aggregate ----