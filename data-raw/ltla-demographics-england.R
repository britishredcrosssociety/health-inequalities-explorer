# ---- Load libs ----
library(tidyverse)
library(sf)
library(demographr)
library(geographr)

# source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationandhouseholdestimatesenglandandwalescensus2021
# Note: the data is available in demographr, but not with the breakdown of sex

file <- compositr::download_file(
  "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationandhouseholdestimatesenglandandwalescensus2021/census2021/census2021firstresultsenglandwales1.xlsx",
  ".xlsx"
)

unprocessed <-
  readxl::read_excel(
    file,
    sheet = "P03",
    range = "A8:AO383"
  )

# Finish processing

# test age pyramid by sex visualisation to establish best data layout and how
# to best minimse dataset size.
