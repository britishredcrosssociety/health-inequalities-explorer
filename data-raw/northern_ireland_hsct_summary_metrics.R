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

list_northern_ireland_hsct <- 
  lookup_northern_ireland_ltla_hsct |> 
  distinct(trust_name)

# ---- Aggregate Local Authorities measures ----
# Metrics where rank is used: aggregate with maximum rank
imd_health <-
  northern_ireland_ltla_summary_metrics |> 
  select(lad_name = area_name, variable, number) |> 
  filter(variable %in% 
           c("Index of Multiple \nDeprivation rank", "Health Index \nrank")) |> 
  left_join(lookup_northern_ireland_ltla_hsct) |> 
  group_by(trust_name, variable) |> 
  summarise(max_rank = max(number)) |> 
  ungroup() |> 
  group_by(variable) |> 
  mutate(number = rank(max_rank),
         percent = NA) |> 
  select(-max_rank) |> 
  ungroup()

# Left-behind areas: aggregate by adding all left-behind areas of LGDs
lba <-
  cni_northern_ireland_soa11 |>
  select(soa11_code, lad_name = lgd14_name, lba = `Left Behind Area?`) |> 
  left_join(lookup_northern_ireland_ltla_hsct) |> 
  group_by(trust_name) |>
  count(lba) |>
  mutate(percent = n / sum(n)) |> 
  ungroup() |>
  filter(lba == TRUE) |>
  right_join(list_northern_ireland_hsct) |> 
  mutate(percent = replace_na(percent, 0)) |>
  mutate(n = replace_na(n, 0)) |>
  select(trust_name, number = n, percent) |>
  mutate(variable = "Left-behind areas", .after = trust_name)

# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  imd_health,
  lba
) |>
  rename(area_name = trust_name) |>
  relocate(area_name)



northern_ireland_hsct_summary_metrics



usethis::use_data(northern_ireland_hsct_summary_metrics, overwrite = TRUE)