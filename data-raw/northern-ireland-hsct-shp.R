library(tidyverse)
library(sf)
library(geographr)
library(rmapshaper)

northern_ireland_hsct_shp <-
  boundaries_trusts_ni18 |>
  ms_simplify(keep = 0.5) |> # Any less and areas get dropped
  select(area_name = trust18_name, area_code = trust18_code)

usethis::use_data(england_ltla_shp, overwrite = TRUE)