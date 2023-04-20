library(tidyverse)
library(geographr)
library(sf)

icb_shp_england <-
  boundaries_icb22 |>
  mutate(icb22_name = str_remove_all(icb22_name, "^NHS ")) |>
  mutate(icb22_name = str_remove_all(icb22_name, " Integrated Care Board$")) |>
  select(area_name = icb22_name, area_code = icb22_code)

usethis::use_data(icb_shp_england, overwrite = TRUE)
