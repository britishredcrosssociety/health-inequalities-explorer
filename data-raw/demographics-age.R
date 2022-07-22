# ---- Load libs ----
library(tidyverse)
library(sf)
library(demographr)
library(geographr)

# Three data sets need exporting:
#   - Neighbourhoods (LSOA)
#   - Places (CCG and LTLA)
#   - Systems (STP)

# Use the below data sets and lookup table to create these data sets. Think
# about how the data will be visualised and preprocess now to save computation
# time in app (i.e., pivot longer and group into age bands)
population20_lsoa11
population20_ltla21
lookup_lsoa11_ccg21_stp21_ltla21