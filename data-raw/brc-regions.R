library(tidyverse)
library(sf)
library(geographr)

#----England regions----
brc_north_shp <- lookup_ltla21_brc |>
  left_join(england_ltla_shp, by = c("ltla21_code" = "area_code")) |>
  filter(brc_area == "North") |>
  select(-brc_area) |>
  rename(area_code = ltla21_code) |>
  st_as_sf()

usethis::use_data(brc_north_shp, overwrite = TRUE)

brc_central_shp <- lookup_ltla21_brc |>
  left_join(england_ltla_shp, by = c("ltla21_code" = "area_code")) |>
  filter(brc_area == "Central") |>
  select(-brc_area) |>
  rename(area_code = ltla21_code) |>
  st_as_sf()

usethis::use_data(brc_central_shp, overwrite = TRUE)

brc_south_shp <- lookup_ltla21_brc |>
  left_join(england_ltla_shp, by = c("ltla21_code" = "area_code")) |>
  filter(brc_area == "South and the Channel Islands") |>
  select(-brc_area) |>
  rename(area_code = ltla21_code) |>
  st_as_sf()

usethis::use_data(brc_south_shp, overwrite = TRUE)

brc_southeast_shp <- lookup_ltla21_brc |>
  left_join(england_ltla_shp, by = c("ltla21_code" = "area_code")) |>
  filter(brc_area == "South East") |>
  select(-brc_area) |>
  rename(area_code = ltla21_code) |>
  st_as_sf()

usethis::use_data(brc_southeast_shp, overwrite = TRUE)

brc_london_shp <- lookup_ltla21_brc |>
  left_join(england_ltla_shp, by = c("ltla21_code" = "area_code")) |>
  filter(brc_area == "London") |>
  select(-brc_area) |>
  rename(area_code = ltla21_code) |>
  st_as_sf()

usethis::use_data(brc_london_shp, overwrite = TRUE)

#---- Devolved nations ----
# Copy of dfs are needed for use in selectGeography
wales_ltla_shp_copy <- wales_ltla_shp
usethis::use_data(wales_ltla_shp_copy, overwrite = TRUE)

northern_ireland_ltla_shp_copy <- northern_ireland_ltla_shp
usethis::use_data(northern_ireland_ltla_shp_copy, overwrite = TRUE)

scotland_ltla_shp_copy <- scotland_ltla_shp
usethis::use_data(scotland_ltla_shp_copy, overwrite = TRUE)
