library(tidyverse)
library(sf)
library(geographr)
library(rmapshaper)

wales_ltla_shp <-
  boundaries_ltla24 |>
  filter(str_detect(ltla24_code, "^W")) |>
  ms_simplify(keep = 0.5) |>
  select(area_name = ltla24_name, area_code = ltla24_code)

usethis::use_data(wales_ltla_shp, overwrite = TRUE)
