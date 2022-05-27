library(tidyverse)
library(geographr)
library(sf)
library(rmapshaper)

# Note: STP's have evolved into ICS's (i.e., they are identical):
# https://www.kingsfund.org.uk/blog/2018/05/what-has-stp-ics-done

ics_shp_england <-
  boundaries_stp21 |>
  mutate(stp21_name = str_replace_all(stp21_name, "'", "")) |>
  ms_simplify(keep = 0.5) |> # Any less and areas get dropped
  select(area_name = stp21_name, area_code = stp21_code)

usethis::use_data(ics_shp_england, overwrite = TRUE)