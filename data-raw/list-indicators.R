# ---- Load libraries ----
library(tibble)
library(shiny)

# ---- Build data ----
list_indicators <-
  tribble(
    
    # ----Column Names----
    ~id, ~type , ~indicator, ~tag,
    
    # ----Summary metrics----
    "summary_intro", "summary_metrics", "intro",  tags$p(
      "These indicators summarise a selection of health metrics into a single
        score. They can be useful for comparing the overall health of different
        areas and are a good place to start. But, they should not be used in
        isolation to make judgements about all aspects of an area's health. For
        example, an area may score poorly in a summary metric, yet still excel
        in certain aspects of health."
    ),
    "health_index", "summary_metrics", "health_index", tags$p(
      "The ONS Health Index provides an indication of health outcomes, risk
        factors, and the wider determinants of health. A detailed breakdown
        of the index can be viewed ",
      tags$a(
        href = "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/bulletins/healthinengland/2015to2021",
        target = "_blank",
        "here."
      )
    ),
    "lba", "summary_metrics", "left_behind_areas", tags$p(
      "Left-behind areas are places high in deprivation and socio-economic
        challenges, and low in social infrastructure and investment to meet those
        challenges. Research has shown they are associated with high health
        inequalities. More information on these areas can be found ",
      tags$a(
        href = "https://ocsi.uk/left-behind-neighbourhoods/",
        target = "_blank",
        "here."
      ),
      " An interactive map to visualise these areas can be found",
      tags$a(
        href = "https://britishredcross.shinyapps.io/left-behind-areas/",
        target = "_blank",
        "here."
      )
    ),
    "imd_england", "summary_metrics", "IMD", tags$p(
      "The Indices of Multiple Deprivation (IMD) include a measure of health
        that measures the risk of premature death and the impairment of quality
        of life through poor physical or mental health. More information can be
        viewed ",
      tags$a(
        href = "https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019",
        target = "_blank",
        "here."
      )
    )
    
  )


usethis::use_data(list_indicators, internal = TRUE, overwrite = TRUE)
