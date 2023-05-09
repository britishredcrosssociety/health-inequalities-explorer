library(tidyverse)
library(geographr)
library(sf)

scotland_hb_shp <-
  boundaries_hb19 |>
  select(area_name = hb19_name, area_code = hb19_code)

usethis::use_data(scotland_hb_shp, overwrite = TRUE)
