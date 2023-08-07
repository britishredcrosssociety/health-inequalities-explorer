library(tidyverse)
library(IMD)
library(geographr)
library(sf)
library(ggridges)

lhb <- boundaries_lhb20 |>
  st_drop_geometry()

lookup_lsoa11_ltla21_lhb22<-lookup_lsoa11_ltla21|>
                 filter(str_detect(ltla21_code, "^W"))|>
                         left_join(lookup_ltla21_lhb22)|>
                          distinct(lsoa11_code,lhb22_code,lhb22_name,ltla21_code)|>
                          select(lsoa_code=lsoa11_code,lhb20_code=lhb22_code,lhb20_name=lhb22_name,ltla21_code)

# ---- IMD score ----
imd<-
  imd_wales_lsoa|>
  left_join(lookup_lsoa11_ltla21_lhb22) |>
  select(lsoa_code, lhb20_code, IMD_decile) |>
  mutate(top_10 = if_else(IMD_decile == 1, "yes", "no")) |>
  group_by(lhb20_code, top_10) |>
  summarise(n = n()) |>
  mutate(freq = n / sum(n)) |>
  mutate(total_number_lsoas = sum(n)) |>
  ungroup() |>
  filter(top_10 == "no") |> 
  mutate(number = total_number_lsoas - n) |>
  mutate(percent = 1 - freq) |>
  mutate(variable = "Deprivation", .after = lhb20_code) |>
  select(-top_10, -n, -freq, -total_number_lsoas)

# ---- health ----
url <- "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/wales/healthy-people-domain.csv"
resilience_index_raw <- read_csv(url)
health_index <- resilience_index_raw |>
  rename(ltla21_code = lad_code) |>
  left_join(lookup_ltla21_lhb22) |>
  select(lhb22_code, number = healthy_people_domain_rank) |>
  group_by(lhb22_code) |>
  count(number) |>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  rename(lhb20_code = lhb22_code) |>
  distinct(lhb20_code, .keep_all = TRUE) |>
  mutate(number = rank(number)) |>
  mutate(percent = NA) |>
  mutate(variable = "ONS Health \nIndex rank") |>
  relocate(variable, .after = lhb20_code)

# ---- % Left-behind areas ----
lba <- cni_wales_msoa11 |>
  select(ltla21_name=lad21_name, lba=`Left Behind Area?`)|>
  left_join(lookup_ltla21_lhb22)|>
  group_by(lhb22_code) |>
  count(lba)|>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  rename(lhb20_code=lhb22_code) |>
  filter(lba == TRUE) |> 
  right_join(lhb)|>
  mutate(percent = replace_na(percent, 0)) |>
  mutate(n = replace_na(n, 0)) |>
  mutate(variable = "Left-behind areas", .after = lhb20_code) |>
  select(-lba, -lhb20_name)

# ---- Combine & rename (pretty printing) ----
metrics_joined <- bind_rows(
  imd,
  lba,
  health_index
) |>
  left_join(lhb) |>
  select(-lhb20_code) |>
  rename(area_name = lhb20_name) |>
  relocate(area_name)

# ---- Normalise/scale ----
scale_1_1 <- function(x) {
  (x - mean(x)) / max(abs(x - mean(x)))
}

lhb_summary_metrics_wales_scaled <-
  metrics_joined |>
  group_by(variable) |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Deprivation" ~ scale_1_1(percent),
      variable == "Left-behind areas" ~ scale_1_1(percent),
      variable == "ONS Health \nIndex rank" ~ scale_1_1(number)
    )
  ) |>
  ungroup()

# ---- Align indicator polarity ----
wales_lhb_summary_metrics_polarised <- lhb_summary_metrics_wales_scaled |>
  mutate(
    scaled_1_1 = case_when(
      variable == "Deprivation" ~ scaled_1_1 * -1,
      variable == "Left-behind areas" ~ scaled_1_1 * -1,
      TRUE ~ scaled_1_1
    )
  )

# Check distributions
wales_lhb_summary_metrics_polarised |>
  ggplot(aes(x = scaled_1_1, y = variable)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges()

# ---- Add plot labels ----
wales_lhb_summary_metrics <- wales_lhb_summary_metrics_polarised |>
  mutate(
    label = case_when(
      variable == "Deprivation" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "The no. of LSOAs in the LHB that are in the top 10% most deprived nationally: ", round(number),
        "<br>", "Percentage of LSOAs in the LHB that are in the top 10% most deprived nationally: ", round(percent * 100, 1), "%"
      ),
      variable == "Left-behind areas" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "No. of left-behind LSOAs in the LHB: ", round(number),
        "<br>", "Percentage of LSOAs in the LHB that are left-behind: ", round(percent * 100, 1), "%"
      ),
      variable == "ONS Health \nIndex rank" ~ paste0(
        "<b>", area_name, "</b>",
        "<br>",
        "<br>", "Health Index rank: ", round(number)
      )
    )
  )

usethis::use_data(wales_lhb_summary_metrics, overwrite = TRUE)

