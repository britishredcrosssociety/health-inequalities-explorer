library(tidyverse)
library(IMD)
library(geographr)
library(compositr)
library(sf)
library(ggridges)

pkgload::load_all(".")

lookup_northern_ireland_ltla_hsct <-
  lookup_ltla21_hsct18 |> 
  select(lad_name = ltla21_name, trust_name = trust18_name)

# ---- Aggregate Local Authorities measures ----

# Metrics where rank is used: aggregate with mean rank
imd_health_hsct <-
  northern_ireland_ltla_summary_metrics |> 
  select(lad_name = area_name, variable, number, percent) |> 
  filter(variable %in% c("Index of Multiple \nDeprivation rank", "Health Index \nrank")) |> 
  left_join(lookup_northern_ireland_ltla_hsct) |> 
  group_by(trust_name, variable) |> 
  summarise(number = mean(number))

# Left-behind areas: aggregate by adding all left-behind areas of LGDs
lba_hsct <-
  northern_ireland_ltla_summary_metrics |> 
  select(lad_name = area_name, variable, number, percent) |> 
  filter(variable == "Left-behind areas") |> 
  left_join(lookup_northern_ireland_ltla_hsct) |> 
  group_by(trust_name, variable) |> 
  summarise(number = sum(number))



northern_ireland_hsct_summary_metrics



usethis::use_data(northern_ireland_hsct_summary_metrics, overwrite = TRUE)