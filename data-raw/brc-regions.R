library(tidyverse)
library(sf)
library(geographr)

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

# ---- Health Boards within BRC regions ----
# Use the centroids of Health Boards to subset by BRC region
points_icb22 <- 
  boundaries_icb22 |>
  st_centroid()

# Get list of ICB codes within this region's Local Authorities
brc_north_icb_codes <- 
  st_join(points_icb22, brc_north_shp) |> 
  filter(!is.na(area_code)) |> 
  st_drop_geometry() |> 
  select(icb22_code)

# Subset the ICB polygons in this region
brc_north_icb_shp <- 
  england_icb_shp |> 
  filter(area_code %in% brc_north_icb_codes$icb22_code)

usethis::use_data(brc_north_icb_shp, overwrite = TRUE)

# Get list of ICB codes within this region's Local Authorities
brc_london_icb_codes <- 
  st_join(points_icb22, brc_london_shp) |> 
  filter(!is.na(area_code)) |> 
  st_drop_geometry() |> 
  select(icb22_code)

# Subset the ICB polygons in this region
brc_london_icb_shp <- 
  england_icb_shp |> 
  filter(area_code %in% brc_london_icb_codes$icb22_code)

usethis::use_data(brc_london_icb_shp, overwrite = TRUE)

# Get list of ICB codes within this region's Local Authorities
brc_central_icb_codes <- 
  st_join(points_icb22, brc_central_shp) |> 
  filter(!is.na(area_code)) |> 
  st_drop_geometry() |> 
  select(icb22_code)

# Subset the ICB polygons in this region
brc_central_icb_shp <- 
  england_icb_shp |> 
  filter(area_code %in% brc_central_icb_codes$icb22_code)

usethis::use_data(brc_central_icb_shp, overwrite = TRUE)

# Get list of ICB codes within this region's Local Authorities
brc_south_icb_codes <- 
  st_join(points_icb22, brc_south_shp) |> 
  filter(!is.na(area_code)) |> 
  st_drop_geometry() |> 
  select(icb22_code)

# Subset the ICB polygons in this region
brc_south_icb_shp <- 
  england_icb_shp |> 
  filter(area_code %in% brc_south_icb_codes$icb22_code)

usethis::use_data(brc_south_icb_shp, overwrite = TRUE)

# Get list of ICB codes within this region's Local Authorities
brc_southeast_icb_codes <- 
  st_join(points_icb22, brc_southeast_shp) |> 
  filter(!is.na(area_code)) |> 
  st_drop_geometry() |> 
  select(icb22_code)

# Subset the ICB polygons in this region
brc_southeast_icb_shp <- 
  england_icb_shp |> 
  filter(area_code %in% brc_southeast_icb_codes$icb22_code)

usethis::use_data(brc_southeast_icb_shp, overwrite = TRUE)
