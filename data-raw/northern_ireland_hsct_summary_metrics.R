library(tidyverse)
library(IMD)
library(geographr)
library(compositr)
library(sf)
library(ggridges)

hsct <-
  boundaries_trusts_ni18|>
  st_drop_geometry()



# ---- IMD score ----
# Higher extent = more deprived /
# Higher rank (calculated here) = more deprived
imd <- 
  imd_northern_ireland_lad |>
  select(ltla21_code = lad_code, imd_score = Extent) |>
  mutate(number = rank(imd_score)) |>
  select(-imd_score) |>
  mutate(variable = "Index of Multiple \nDeprivation rank", .after = ltla21_code) |>
  mutate(percent = NA, .after = number)

# ---- ONS Health Index ----

# An official Health Index for Northern Ireland does not exists. Use the BRC Resilience Index version

# Higher score = worse health
# Higher rank (calculated here) = worse health
health_index_raw <- read_csv(
  "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/northern-ireland/index-unweighted-all-indicators.csv"
)

health_index <- 
  health_index_raw |>
  select(ltla21_code = lad_code, number = health_inequalities_composite_rank) |>
  mutate(percent = NA) |>
  mutate(variable = "Health Index \nrank") |>
  relocate(variable, .after = ltla21_code)

# ---- % Left-behind areas ----

#The left-behind areas come from the community needs index (loaded from the IMD package)
# Higher number/percent = more left-behind
lba <- 
  cni_northern_ireland_soa11 |>
  left_join(lookup_northern_ireland_ltla, by = c("lgd14_code" = "ltla19_code")) |>
  select(soa11_code, ltla21_code, lba = `Left Behind Area?`) |>
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

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

ltla_summary_metrics_northern_ireland_scaled <-
  metrics_joined |>
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
northern_ireland_ltla_summary_metrics_polarised <- 
  ltla_summary_metrics_northern_ireland_scaled |>
  mutate(scaled_1_1 = scaled_1_1 * -1)

# Check distributions
northern_ireland_ltla_summary_metrics_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----

northern_ireland_ltla_summary_metrics <- northern_ireland_ltla_summary_metrics_polarised |>
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
        "<br>", "No. of left-behind Intermediate Zones in the Local Authority: ", round(number),
        "<br>", "Percentage of all left-behind Intermediate Zones in the Local Authority: ", round(percent * 100, 1), "%"
      ),
      variable == "Health Index \nrank" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Health Index rank: ", round(number)
      )
    )
  )

usethis::use_data(northern_ireland_ltla_summary_metrics, overwrite = TRUE)
