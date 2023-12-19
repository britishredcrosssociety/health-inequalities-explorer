# ---- Load libraries ----
library(tibble)
library(shiny)

# ---- Build data ----
list_indicators <-
  tribble(
    
    # ----Column Names----
    ~id, ~type , ~indicator, ~geography, ~tag,
    
    # ----Summary metrics----
    "summary_intro", "summary_metrics", "intro", "all", tags$p(
      "These indicators summarise a selection of health metrics into a single
        score. They can be useful for comparing the overall health of different
        areas and are a good place to start. But, they should not be used in
        isolation to make judgements about all aspects of an area's health. For
        example, an area may score poorly in a summary metric, yet still excel
        in certain aspects of health."
    ),
    "health_index", "summary_metrics", "health_index", "England", tags$p(
      "The ONS Health Index provides an indication of health outcomes, risk
        factors, and the wider determinants of health. A detailed breakdown
        of the index can be viewed ",
      tags$a(
        href = "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/bulletins/healthinengland/2015to2021",
        target = "_blank",
        "here."
      )
    ),
    "lba", "summary_metrics", "left_behind_areas", "England", tags$p(
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
    "imd_england", "summary_metrics", "IMD", "England", tags$p(
      "The Indices of Multiple Deprivation (IMD) include a measure of health
        that measures the risk of premature death and the impairment of quality
        of life through poor physical or mental health. More information can be
        viewed ",
      tags$a(
        href = "https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019",
        target = "_blank",
        "here."
      )
    ),
    
    # ----Secondary Care----
    "second_care_intro_eng", "secondary_care", "intro", "England", tags$p(
      "Secondary care indicators report on the direct performance of the national
         health service. Most secondary care statistics are reported only at
         the Trust level. To be able to view these statistics at the local authority
         level, we have aggregated them using catchment population data.
         For more information on how we have done this, see ",
      tags$a(
        href = "https://britishredcrosssociety.github.io/resilience-index-book/technical.html#health-capacity---england",
        target = "_blank",
        "here."
      )
    ),
    "iapt", "secondary_care", "IAPT", "England", tags$p(
      "The Improving Access to Pyschological Therapies (IAPT) programme offers
        talking therapies for mental health problems. To address both access to
        mental health services and the success rate of their interventions, the
        percentage of referrals that were able to access a service within 18
        weeks and also finished the first course of treatment are presented.
        More detailed statistics can be accessed ",
      tags$a(
        href = "https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-sets/improving-access-to-psychological-therapies-data-set/improving-access-to-psychological-therapies-data-set-reports",
        target = "_blank",
        "here."
      )
    ),
    "discharged_eng", "secondary_care", "Discharged beds", "England", tags$p(
      "Discharged beds indicates the total number of patients discharged from
        beds, and the percentage this makes up of all beds. More detailed
        breakdowns can be viewed ",
      tags$a(
        href = "https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-acute-data/",
        target = "_blank",
        "here."
      )
    ),
    "crit_reside_eng", "secondary_care", "Beds not meeting criteria to reside", "England", tagList(
      tags$p(
      "Beds not meeting criteria to reside shows the number of patients who
        are no longer eligible to occupy a bed, and the percentage of these of
        all beds. This indicator is often a good proxy for where social care is
        low. For a more detailed breakdown, see",
      tags$a(
        href = "https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-acute-data/",
        target = "_blank",
        "here."
      )
    ),
    tags$p(
      "In addition to the bed availability indicator presented above, our team
         has also produced a NHS England winter situation report explorer, with
         detailed breakdowns by type of bed, which can be seen ",
      tags$a(
        href = "https://britishredcross.shinyapps.io/sitrep-explorer/",
        target = "_blank",
        "here."
        )
      )
    ),
    
    # ----Demographics----
    "demog_ONS", "demographics", "Demographics", "England", tags$p(
      "These indicators can be used alongside other indicators to understand
        the population breakdowns of the areas being assessed. All data come from
        the latest ",
      tags$a(
        href = "https://www.ons.gov.uk/census",
        target = "_blank",
        "2021 census."
      )
    )
    
    
  )


usethis::use_data(list_indicators, internal = TRUE, overwrite = TRUE)
