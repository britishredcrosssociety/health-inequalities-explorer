library(tidyverse)
library(geographr)
library(compositr)
library(sf)
library(IMD)

ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^N"))

lookup_northern_ireland_ltla <-
  lookup_ltla_ltla |>
  filter(str_detect(ltla21_code, "^N"))

# ---- IMD score ----
imd <- imd_northern_ireland_lad




# ---- ONS Health Index ----

# An official Health Index for Northern Ireland does not exists. Use the BRC Resilience
# Index version

# Higher score = worse health
# Higher rank (calculated here) = worse health
health_index_raw <- read_csv(
  "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/northern-ireland/index-unweighted.csv"
)





# ---- % Left-behind areas ----

#The left-behind areas come from the community needs index
#The latest data of which can be loaded into the HIE directly from the IMD R package

# Higher number/percent = more left-behind

cni <- cni_northern_ireland_soa11

#use proportions (& add absolute number)





# ---- Combine & reanme (pretty printing) ----




# ---- Normalise/scale ----


# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD and LBA, as currently higher = worse health



# ---- Add plot labels ----


usethis::use_data(england_icb_summary_metrics, overwrite = TRUE)
