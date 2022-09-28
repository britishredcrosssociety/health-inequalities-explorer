library(tidyverse)
library(geographr)
library(compositr)
library(sf)
library(IMD)
library(readxl)
library(ggridges)

ltla <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^E"))

lookup_england_ltla <-
  lookup_ltla_ltla |>
  filter(str_detect(ltla21_code, "^E"))

# ---- IMD score ----
# Higher score = more deprived
imd <-
  imd_england_lad |>
  select(ltla19_code = lad_code, imd_score = Score) |>
  left_join(lookup_england_ltla, by = "ltla19_code") |>
  select(ltla21_code, imd_score) |>
  group_by(ltla21_code) |>
  summarise(imd_score = mean(imd_score))

# ---- % Left-behind areas ----
# Higher score = more deprived
lba <-
  cni_england_ward17 |>
  left_join(lookup_england_ltla, by = c("lad19_code" = "ltla19_code")) |>
  select(ward17_code, ltla21_code, lba = `Left Behind Area?`) |>
  group_by(ltla21_code) |>
  count(lba) |>
  mutate(lba_percentage = n / sum(n)) |>
  ungroup() |>
  filter(lba == TRUE) |>
  right_join(ltla) |>
  select(ltla21_code, lba_percentage) |>
  mutate(lba_percentage = replace_na(lba_percentage, 0))

# ---- ONS Health Index score ----
# Higher score = better health
health_index_file <-
  download_file(
    "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/datasets/healthindexscoresengland/current/healthindexscoresatnationalregionalandlocalauthoritylevelsenglandtimeseries.xlsx",
    ".xlsx"
  )

health_index_scores <-
  read_excel(
    health_index_file,
    sheet = "Table_2_Index_scores",
    range = "A3:G343"
  )

health_index_2019 <-
  health_index_scores |>
  select(
    ltla21_code = `Area Code`,
    health_index_score = `2019`
  ) |>
  filter(ltla21_code %in% ltla$ltla21_code)

# Data is missing for two ltla's
#   - Map Iscles of Scilly to Cornwall
#   - Map City of London to Hackney
cornwall_score <-
  health_index_2019 |>
  filter(ltla21_code == "E06000052") |>
  pull(health_index_score)

hackney_score <-
  health_index_2019 |>
  filter(ltla21_code == "E09000012") |>
  pull(health_index_score)

health_index_missing_added <-
  health_index_2019 |>
  add_row(ltla21_code = "E06000053", health_index_score = cornwall_score) |>
  add_row(ltla21_code = "E09000001", health_index_score = hackney_score)

# Scores need flipping so polarity matches other summary metrics
health_index <-
  health_index_missing_added |>
  mutate(health_index_score = health_index_score * -1)

# ---- Combine ----
metrics_joined <-
  ltla |>
  left_join(imd) |>
  left_join(lba) |>
  left_join(health_index) |>
  pivot_longer(cols = !starts_with("ltla21_"), names_to = "variable") |>
  select(-ltla21_code) |>
  mutate(ltla21_name = str_replace_all(ltla21_name, "'", "")) |>
  rename(area_name = ltla21_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

ltla_summary_metrics_england <-
  metrics_joined |>
  group_by(variable) |>
  mutate(value = scale_1_1(value))

# Check distributions
ltla_summary_metrics_england |>
  ggplot(aes(x = value, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()

usethis::use_data(ltla_summary_metrics_england, overwrite = TRUE)
