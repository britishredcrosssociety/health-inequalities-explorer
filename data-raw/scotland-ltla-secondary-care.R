library(tidyverse)
library(lubridate)
library(healthyr)
library(geographr)
library(sf)

# ---- Delayed discharge ----
delayed_discharge_monthly <- scotland_delayed_discharge_ltla |>
  drop_na() |> 
  filter(date >= max(date) %m-% months(2)) |> # Last quarter
  filter(age_group == "18-74" | age_group == "75plus") |>
  filter(delay_reason == "All Delay Reasons") |>
  group_by(ltla_code, date) |>
  summarise(
    num_delayed_bed_days = sum(num_delayed_bed_days),
    average_daily_delayed_beds = sum(average_daily_delayed_beds)
  ) |> 
  ungroup()

delayed_discharge_summary <- delayed_discharge_monthly |>
  group_by(ltla_code) |>
  summarise(
    num_delayed_bed_days = mean(num_delayed_bed_days),
    average_daily_delayed_beds = mean(average_daily_delayed_beds)
  )

# To Do:
# - The numbers (above) are absolute and need dividing by the total number of 
#   available beds to calculate percent (relative) scores.