library(tidyverse)
library(geographr)
library(sf)

ltla_names <-
  boundaries_ltla21 |> 
  st_drop_geometry() |> 
  select(starts_with("ltla21_"))

raw <- read_csv(
  "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/data/vulnerability/health-inequalities/england/health-index-2019.csv"
)

hi_vul_england <-
  raw |>
  left_join(ltla_names, by = c("lad_21_code" = "ltla21_code")) |> 
  select(
    ltla21_name,
    `Overall Score` = overall_score_rank,
    `Healthy People` = healthy_people_rank,
    `Healthy Lives` = healthy_lives_rank,
    `Healthy Places` = healthy_places_rank
  ) |> 
  pivot_longer(cols = -ltla21_name)

# usethis::use_data(hi_vul_england, overwrite = TRUE)

hi_vul_england |> 
  ggplot(aes(x = value, y = name)) +
  geom_vline(xintercept = 150, size = 2, alpha = .5, colour = "#FFA500") +
  geom_jitter(height = 0.25, color = "steelblue3", size = 5, alpha = .5) +
  theme_minimal() +
  labs(x = NULL, y = NULL)

library(ggiraph)

gg <- hi_vul_england |>
mutate(ltla21_name = str_replace_all(ltla21_name, "'", "")) |> 
  ggplot(aes(x = value, y = name)) +
  geom_vline(xintercept = 150, size = 2, alpha = .5, colour = "#FFA500") +
  geom_jitter_interactive(
    aes(tooltip = ltla21_name, data_id = ltla21_name),
    height = 0.25, color = "steelblue3", size = 5, alpha = .5
    ) +
  theme_minimal() +
  labs(x = NULL, y = NULL)

girafe(code = print(gg))

# alter non hovered elemts
# https://davidgohel.github.io/ggiraph/articles/offcran/customizing.html

