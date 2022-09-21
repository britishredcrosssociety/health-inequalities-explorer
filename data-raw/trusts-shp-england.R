library(tidyverse)
library(geographr)
library(sf)

trusts_shp_england <-
  points_nhs_trusts22 |>
  filter(status == "open") |>
  select(area_name = nhs_trust22_name, area_code = nhs_trust22_code)

usethis::use_data(trusts_shp_england, overwrite = TRUE)
