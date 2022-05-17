library(tidyverse)
library(sf)
library(geographr)
library(rmapshaper)

boundaries_ltla21_england <-
  boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^E")) |>
  ms_simplify(keep = 0.5) # Any less and areas get dropped

usethis::use_data(boundaries_ltla21_england, overwrite = TRUE)