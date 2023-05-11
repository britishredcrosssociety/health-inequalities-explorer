library(tidyverse)
library(lubridate)
library(healthyr)
library(geographr)
library(sf)

# Popuation counts
# Source: https://www.opendata.nhs.scot/dataset/population-estimates
pop_raw <- read_csv(
  "https://www.opendata.nhs.scot/dataset/7f010430-6ce1-4813-b25c-f7f335bdc4dc/resource/09ebfefb-33f4-4f6a-8312-2d14e2b02ace/download/ca2019_pop_est_15072022.csv"
)

pop_scotland <- pop_raw |>
  filter(Year == 2021) |> 
  filter(Sex == "All") |> 
  filter(CA != "S92000003") |> 
  select(ltla21_code = CA, population = AllAges) |> print(n=Inf)

# ---- Delayed discharge ----
delayed_discharge_monthly <- scotland_delayed_discharge_ltla |>
  drop_na() |> 
  filter(ltla_code != "S92000003") |> 
  filter(date >= max(date) %m-% months(2)) |> # Last quarter
  filter(age_group == "18-74" | age_group == "75plus") |>
  filter(delay_reason == "All Delay Reasons") |>
  rename(ltla21_code = ltla_code) |> 
  group_by(ltla21_code, date) |>
  summarise(
    num_delayed_bed_days = sum(num_delayed_bed_days),
    average_daily_delayed_beds = sum(average_daily_delayed_beds)
  ) |> 
  ungroup()

delayed_discharge_summary <- delayed_discharge_monthly |>
  group_by(ltla21_code) |>
  summarise(
    num_delayed_bed_days = mean(num_delayed_bed_days),
    average_daily_delayed_beds = mean(average_daily_delayed_beds)
  ) |> 
  select(-num_delayed_bed_days)

delayed_discharge_summary |> 
  left_join(pop_scotland) |>
  mutate(beds_per_10000 = average_daily_delayed_beds / population * 10000)

