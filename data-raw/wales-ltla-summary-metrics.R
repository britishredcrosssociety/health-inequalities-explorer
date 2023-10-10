library(tidyverse)
library(IMD)
library(geographr)
library(sf)
library(ggridges)
library(stringr)

ltla <- boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^W")) |>
  select(ltla21_code, ltla21_name)

lookup_msoa_ltla <- lookup_msoa11_ltla21 |>
  select(msoa11_code, ltla21_code) |>
  filter(str_starts(ltla21_code, "W"))

# ---- IMD score ----
# Higher extent = more deprived /
# Higher rank (calculated here) = more deprived
imd <-
  imd_wales_lad |>
  select(ltla21_code = lad_code, imd_score = Extent) |>
  mutate(number = rank(imd_score)) |>
  select(-imd_score) |>
  mutate(
    variable = "Index of Multiple \nDeprivation rank",
    .after = ltla21_code
  ) |>
  mutate(percent = NA, .after = number)

# ---- % Left-behind areas ----
# Higher number/percent = more left-behind
lba <-
  cni_wales_msoa11 |>
  left_join(lookup_msoa_ltla) |>
  select(ltla21_code, lba = `Left Behind Area?`) |>
  group_by(ltla21_code) |>
  count(lba) |>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  filter(lba == TRUE) |>
  right_join(ltla) |>
  mutate(percent = replace_na(percent, 0)) |>
  mutate(n = replace_na(n, 0)) |>
  select(ltla21_code, number = n, percent) |>
  mutate(variable = "Left-behind areas", .after = ltla21_code)

# ---- Health Index Score ----
# Higher score = worse health
# Higher rank (calculated here) = worse health
url <- "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/wales/healthy-people-domain.csv"

health_index_raw <- read_csv(url)

health_index <- health_index_raw |>
  select(ltla21_code = lad_code, number = healthy_people_domain_rank) |>
  mutate(percent = NA) |>
  mutate(variable = "Health Index \nrank") |>
  relocate(variable, .after = ltla21_code)

# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
  health_index
) |>
  left_join(ltla) |>
  select(-ltla21_code) |>
  rename(area_name = ltla21_name) |>
  relocate(area_name)

# ---- Normalise/scale ---- quotient transformation (x/sum)
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

ltla_summary_metrics_wales_scaled <- metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Index of Multiple \nDeprivation rank" ~ scale_1_1(number),
      variable == "Left-behind areas" ~ scale_1_1(percent),
      variable == "Health Index \nrank" ~ scale_1_1(number)
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
# Align so higher value = better health
# Flip IMD, LBA, and health index, as currently higher = worse health
wales_ltla_summary_metrics_polarised <-
  ltla_summary_metrics_wales_scaled |>
  mutate(scaled_1_1 = scaled_1_1 * -1)

# Check distributions
wales_ltla_summary_metrics_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
wales_ltla_summary_metrics <- wales_ltla_summary_metrics_polarised |>
  mutate(
    label = case_when(
      variable == "Index of Multiple \nDeprivation rank" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "IMD rank: ", round(number)
      ),
      variable == "Left-behind areas" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of left-behind smaller areas (MSOA's) in the Local Authority: ", round(number),
        "<br>", "Percentage of all left-behind smaller areas (MSOA's) in the Local Authority: ", round(percent * 100, 1), "%"
      ),
      variable == "Health Index \nrank" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Health Index rank: ", round(number)
      )
    )
  )

usethis::use_data(wales_ltla_summary_metrics, overwrite = TRUE)
