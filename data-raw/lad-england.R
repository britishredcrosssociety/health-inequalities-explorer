library(tidyverse)
library(sf)
library(geographr)
library(rmapshaper)

boundaries_ltla21_england <-
  boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^E")) |>
  mutate(ltla21_name = str_replace_all(ltla21_name, "'", "")) |>
  ms_simplify(keep = 0.5) # Any less and areas get dropped

usethis::use_data(boundaries_ltla21_england, overwrite = TRUE)