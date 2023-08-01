library(tidyverse)
library(geographr)
library(sf)

wales_lhb_shp <-
  boundaries_lhb20 |>
  select(area_name = lhb20_name, area_code = lhb20_code)

usethis::use_data(wales_lhb_shp, overwrite = TRUE)
