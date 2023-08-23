library(tidyverse)
library(geographr)
library(healthyr)
library(compositr)
library(sf)

ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^E"))

trust_names <-
  points_nhs_trusts22 |>
  st_drop_geometry() |>
  select(starts_with("nhs_"))

# ---- A&E attendances over 4 hours ----
# Data is collected for all A&E types including walk-in centres and 
# minor injury units. Only acute hospitals are mapped to LTLAs; therefore  
# A&E types are mapped to acute hospitals.
attendances_4hours_trust <- england_trust_accidents_emergency |>
  mutate(date = parse_date_time(date, orders = "B Y")) |>
  filter(date >= ymd("2022-05-01"), date <= ymd("2023-04-01")) |>
  select(nhs_trust22_code, attendances_over_4hours, total_attendances) |>
  group_by(nhs_trust22_code) |>
  summarise(number_sum = sum(attendances_over_4hours, na.rm = TRUE), 
            total_sum = sum(total_attendances, na.rm = TRUE)) 

# Apportion attendances from non-acute trusts to acute trusts
acute_trusts <- england_ae_acute_trust_attribution |>
  filter(nhs_trust22_code_all %in% attendances_4hours_trust$nhs_trust22_code) |>
  left_join(attendances_4hours_trust, by =c("nhs_trust22_code_all" = "nhs_trust22_code")) |>
  mutate(attributed_number_acute = number_sum * proportion_attendances_attributed_to_acute_trust,
         attributed_total_acute = total_sum * proportion_attendances_attributed_to_acute_trust) |>
  group_by(nhs_trust22_code_acute) |>
  summarise(attributed_number_acute = sum(attributed_number_acute, na.rm = TRUE), 
            attributed_total_acute = sum(attributed_total_acute, na.rm = TRUE))

# Add NHS trust names
attendances_4hours_acute <- acute_trusts |>
  left_join(trust_names, by = c("nhs_trust22_code_acute" = "nhs_trust22_code")) |>
  mutate(percent = attributed_number_acute / attributed_total_acute) |>
  select(-attributed_total_acute)

# Join to LTLA 
attendances_4_hours_ltla <- attendances_4hours_acute |>
  left_join(lookup_nhs_trusts22_ltla21, c("nhs_trust22_code_acute" = "nhs_trust22_code")) |>
  mutate(
    proportion_number = coalesce(proportion_trust_came_from_ltla * attributed_number_acute, 0),
    proportion_percentage = coalesce(proportion_trust_came_from_ltla * percent, 0)
  ) |>
  group_by(ltla21_code) |>
  summarise(number = sum(proportion_number, na.rm = TRUE),
            percent = sum(proportion_percentage, na.rm = TRUE)) |>
  mutate(variable = "A&E attendances over 4 hours \n(May 22 - April 23 average)",
         .after = ltla21_code) |>
  slice(-1)

hist(attendances_4_hours_ltla$percent)