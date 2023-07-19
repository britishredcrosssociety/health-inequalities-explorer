library(tidyverse)
library(sf)
library(geographr)
library(rmapshaper)

northern_ireland_lgd_shp <-
  boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^N")) |>
  ms_simplify(keep = 0.5) |> # Any less and areas get dropped
  select(area_name = ltla21_name, area_code = ltla21_code)

usethis::use_data(northern_ireland_lgd_shp, overwrite = TRUE)
