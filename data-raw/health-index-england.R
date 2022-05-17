library(tidyverse)

raw <- read_csv(
  "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/england/health-index-2019.csv"
)

health_index_vulnerability_england <-
  raw |>
  select(
    lad_21_code,
    ends_with("_rank")
  )

usethis::use_data(health_index_vulnerability_england, overwrite = TRUE)