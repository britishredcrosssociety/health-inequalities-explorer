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

# ---- Attendances over 4 hours ----
attendances_4hours_trust <- england_trust_accidents_emergency |>
  mutate(date = parse_date_time(date, orders = "B Y")) |>
  filter(date >= ymd("2022-05-01"), date <= ymd("2023-04-01")) |>
  subset(select = c(nhs_trust22_code, attendances_over_4hours, pct_attendance_over_4hours)) |>
  mutate(pct_attendance_over_4hours = case_when(
    attendances_over_4hours == 0 ~ 0,
    TRUE ~ pct_attendance_over_4hours
  ))|> # Replace NA in pct column with 0 when its value is actually 0
  group_by(nhs_trust22_code) |>
  summarize(number_sum = sum(attendances_over_4hours), percent_mean = mean(pct_attendance_over_4hours)) |>
  mutate(variable = "Attendances over 4 hours \n(May 22 - April 23 average)", .after = nhs_trust22_code) |>
  left_join(trust_names) |>
  relocate(nhs_trust22_name) |>
  distinct()

attendances_4_hours_ltla <-
  attendances_4hours_trust |>
  left_join(lookup_nhs_trusts22_ltla21) |>
  mutate(
    proportion_number = proportion_trust_came_from_ltla * number_sum,
    proportion_percentage = proportion_trust_came_from_ltla * percent_mean
  ) 

# ---- Check NAs ----
# Check no new NAs have appeared at NHS trust level in the joins
source_nas <- attendances_4hours_trust  |> 
  filter(is.na(number_sum)) 

joined_nas <- attendances_4_hours_ltla  |> 
  filter(is.na(number_sum)) 

unique(joined_nas$nhs_trust22_code) == unique(source_nas$nhs_trust22_code)

# Identify the NHS trusts that has not matched with an LTLA
trusts_not_matched <- attendances_4_hours_ltla |> 
  filter(is.na(ltla21_code)) 

print(unique(trusts_not_matched$nhs_trust22_code))
print(length(unique(trusts_not_matched$nhs_trust22_code)))


# ---- Group by LTLA ----
attendances_4_hours_grouped <- attendances_4_hours_ltla |>
  group_by(ltla21_code) |>
  summarise(
    number = sum(proportion_number),
    percent = sum(proportion_percentage)
  ) 

print(nrow(attendances_4_hours_grouped)/sum(is.na(attendances_4_hours_grouped)))


