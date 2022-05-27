library(tidyverse)
library(sf)
library(geographr)
library(rmapshaper)

ltla_shp_england <-
  boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^E")) |>
  mutate(ltla21_name = str_replace_all(ltla21_name, "'", "")) |>
  ms_simplify(keep = 0.5) |> # Any less and areas get dropped
  select(area_name = ltla21_name, area_code = ltla21_code)

usethis::use_data(ltla_shp_england, overwrite = TRUE)