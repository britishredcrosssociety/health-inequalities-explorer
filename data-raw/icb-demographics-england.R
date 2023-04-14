# ---- Load libs ----
library(tidyverse)
library(sf)
library(demographr)
library(geographr)

# ---- Create England ICB lookup ----
icb <- boundaries_icb22 |>
  st_drop_geometry() |>
  mutate(icb22_name = str_remove_all(icb22_name, "^NHS ")) |>
  mutate(icb22_name = str_remove_all(icb22_name, " Integrated Care Board$"))

# ---- Population ----
# Source: https://www.nomisweb.co.uk/sources/census_2021_ts
population_file <- compositr::download_file(
  "",
  ".xlsx"
)
