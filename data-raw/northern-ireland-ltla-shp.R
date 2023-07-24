library(tidyverse)
library(sf)
library(geographr)
library(rmapshaper)

northern_ireland_ltla_shp <-
  boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^N")) |>
  ms_simplify(keep = 0.5) |>
  select(area_name = ltla21_name, area_code = ltla21_code)

usethis::use_data(northern_ireland_ltla_shp, overwrite = TRUE)
