# ---- Load libraries ----
library(tibble)
library(shiny)

# ---- Build data ----
list_descriptions <-
  tribble(

    # ----Column Names----
    ~id, ~type, ~indicator, ~geography, ~tag,

    # ----Summary metrics----
    "summary_intro", "summary_metrics", "/", "All", tags$p(
      "These indicators summarise a selection of health metrics into a single
        score. They can be useful for comparing the overall health of different
        areas and are a good place to start. But, they should not be used in
        isolation to make judgements about all aspects of an area's health. For
        example, an area may score poorly in a summary metric, yet still excel
        in certain aspects of health."
    ),
    "health_index", "summary_metrics", "Health Index", "England", tags$p(
      "The ONS Health Index provides an indication of health outcomes, risk
        factors, and the wider determinants of health. A detailed breakdown
        of the index can be viewed ",
      tags$a(
        href = "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/bulletins/healthinengland/2015to2021",
        target = "_blank",
        "here."
      )
    ),
    "health_index_devolved", "summary_metrics", "Health Index", "Devolved Nations", tags$p(
      "The ",
      tags$a(
        href = "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/bulletins/healthinengland/2015to2021",
        target = "_blank",
        "ONS Health Index"
      ),
      " is England specific and provides an indication of health outcomes,
            risk factors, and the wider determinants of health. As part of the
            British Red Cross Resilience Index, an equivalent version
            was created for devolved nations. More details of this index can
            be viewed ",
      tags$a(
        href = "https://github.com/britishredcrosssociety/resilience-index",
        target = "_blank",
        "here."
      )
    ),
    "lba", "summary_metrics", "Left-behind Areas", "All", tags$p(
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
    "imd_scotland", "summary_metrics", "IMD", "Scotland", tags$p(
      "The Scottish Indices of Multiple Deprivation (IMD) include a measure
            of health that measures the risk of premature death and the impairment
            of quality of life through poor mental health and drug and alcohol
            misuse. More
            information can be viewed ",
      tags$a(
        href = "https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/",
        target = "_blank",
        "here."
      )
    ),
    "imd_ni", "summary_metrics", "IMD", "Northern Ireland", tags$p(
      "The Indices of Multiple Deprivation (IMD) include a measure of health
        that measures the risk of premature death and the impairment of quality
        of life through poor physical or mental health. More information can be
        viewed ",
      tags$a(
        href = "https://www.nisra.gov.uk/statistics/deprivation/northern-ireland-multiple-deprivation-measure-2017-nimdm2017",
        target = "_blank",
        "here."
      )
    ),
    "imd_wales", "summary_metrics", "IMD", "Wales", tags$p(
      "The Indices of Multiple Deprivation (IMD) include a measure of health
        that measures the risk of premature death and the impairment of quality
        of life through poor physical or mental health. More information can be
        viewed ",
      tags$a(
        href = "https://statswales.gov.wales/Catalogue/Community-Safety-and-Social-Inclusion/Welsh-Index-of-Multiple-Deprivation#:~:text=The%20Welsh%20Index%20of%20Multiple,to%201%2C909%20(least%20deprived)",
        target = "_blank",
        "here."
      )
    ),
    "depahri", "summary_metrics", "Access to Healthcare", "England, Scotland, Wales", tags$p(
      "Access to Healthcare indicates the overall difficulty of accessing healthcare
      services. Digital access includes measures of broadband connectivity, demography and
      deprivation. Physical access includes measures of  road distances to GPs and hospitals,
      demography and deprivation. More information available ",
      tags$a(
        href = "https://github.com/humaniverse/DEPAHRI",
        target = "_blank",
        "here."
      )
    ),
    "loneliness_eng", "summary_metrics", "Loneliness", "England", tags$p(
      "Loneliness scores for England indicate responses to a direct question on
      loneliness asked in the Community Life Survey 2020-21. The question is
      'How often do you feel lonely?' and the percentage represents responses of
      'Often/always' or 'some of the time'. More information on the survey ",
      tags$a(
        href = "https://www.gov.uk/government/statistical-data-sets/dcms-community-life-survey-ad-hoc-statistical-releases",
        target = "_blank",
        "here."
      )
    ),
    "loneliness_excl_eng", "summary_metrics", "Loneliness", "Scotland, Wales, Northern Ireland", tags$p(
      "Loneliness scores for Scotland, Wales and Northern Ireland indicate
      loneliness risk levels. It is calculated from GP prescriptions for
      conditions related to loneliness: Alzheimer's disease, depression,
      hypertension, insomnia, addiction, social anxiety, diabetes and
      cardiovascular disease. It is based on an approach developed by the Office
      for National Statistics, available ",
      tags$a(
        href = "https://datasciencecampus.ons.gov.uk/developing-a-loneliness-prescription-index/",
        target = "_blank",
        "here."
      )
    ),

    # ----Secondary Care----
    "second_care_intro_eng", "secondary_care", "/", "England", tags$p(
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
    "aande_eng", "secondary_care", "A&E waits over four hours", "England", tags$p(
      "Accident and emergency attendances (A&E) show the number of patients
        for all A&E types, including Minor Injury Units and Walk-in Centres,
        that are discharged, admitted or transferred after over four hours of arrival.
        For a more detailed breakdown, see",
      tags$a(
        href = "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2023-24/",
        target = "_blank",
        "here."
      )
    ),
    "iapt", "secondary_care", "IAPT", "England", tags$p(
      "The Improving Access to Psychological Therapies (IAPT) programme offers
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
    "delayed_discharged_scotland", "secondary_care", "Delayed discharges", "Scotland", tags$p(
      "Delayed discharges indicate the average number of hospital bed days
            occupied by patients who were clinically ready for discharge. More
            information can be viewed ",
      tags$a(
        href = "https://www.opendata.nhs.scot/dataset/delayed-discharges-in-nhsscotland",
        target = "_blank",
        "here."
      )
    ),
    "rtt_scotland", "secondary_care", "Referral to treatment waiting times", "Scotland", tags$p(
      "Referral to treatment waiting times show the number of people waiting
            over 18 weeks from their initial referral to the start of their
            treatment. The Scottish Government determined that at least
            90% of patients should be seen within at least 18 weeks. More
            information can be viewed ",
      tags$a(
        href = "https://www.opendata.nhs.scot/dataset/18-weeks-referral-to-treatment",
        target = "_blank",
        "here."
      )
    ),
    "rtt_ni", "secondary_care", "Referral to treatment waiting times", "Northern Ireland", tags$p(
      "Referral to treatment waiting times show the number of people waiting
            over 18 weeks from their initial referral to the start of their
            treatment. More information can be viewed ",
      tags$a(
        href = "https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-inpatient-and-day-case-waiting-times-december-2022",
        target = "_blank",
        "here"
      ),
      " for inpatient and day case waiting times, and ",
      tags$a(
        href = "https://www.health-ni.gov.uk/publications/northern-ireland-waiting-time-statistics-outpatient-waiting-times-december-2022",
        target = "_blank",
        "here"
      ),
      " for outpatient waiting times."
    ),
    "rtt_wales", "secondary_care", "Referral to treatment waiting times", "Wales", tags$p(
      "Referral to treatment waiting times show the number of people waiting
            over 18 weeks from their initial referral to the start of their
            treatment. More information can be viewed ",
      tags$a(
        href = "https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Waiting-Times/Referral-to-Treatment",
        target = "_blank",
        "here."
      )
    ),
    "bed_availability_scotland", "secondary_care", "Bed availability", "Scotland", tags$p(
      "Bed availability shows the number of available staffed beds across
            all specialties. More information can be viewed ",
      tags$a(
        href = "https://www.opendata.nhs.scot/dataset/hospital-beds-information",
        target = "_blank",
        "here."
      )
    ),
    "bed_availability_ni", "secondary_care", "Bed availability", "Northern Ireland", tags$p(
      "Bed availability shows the number of available staffed beds across
            all specialties. More information can be viewed ",
      tags$a(
        href = "https://www.health-ni.gov.uk/publications/hospital-statistics-inpatient-and-day-case-activity-202122",
        target = "_blank",
        "here."
      )
    ),
    "bed_availability_wales", "secondary_care", "Bed availability", "Wales", tags$p(
      "Bed availability shows the number of available staffed beds across
            all specialties. More information can be viewed ",
      tags$a(
        href = "https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Activity/NHS-Beds/nhsbeds-by-organisation-specialty-month",
        target = "_blank",
        "here."
      )
    ),
    "unpaid_care", "secondary_care", "Provision of unpaid care", "Northern Ireland", tags$p(
      "Provision of unpaid care covers looking after, giving help or support
            to anyone because they have long-term physical or mental health
            conditions or illnesses, or problems related to old age. It excludes
            any activities carried out in paid employment. More information can
            be viewed ",
      tags$a(
        href = "https://www.nisra.gov.uk/publications/census-2021-main-statistics-health-disability-and-unpaid-care-tables",
        target = "_blank",
        "here."
      )
    ),


    # ----Demographics----
    "demog_ONS", "demographics", "Demographics", "England & Wales", tags$p(
      "These indicators can be used alongside other indicators to understand
        the population breakdowns of the areas being assessed. All data come from
        the latest ",
      tags$a(
        href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationandhouseholdestimatesenglandandwales/census2021unroundeddata#:~:text=On%20Census%20Day%2C%2021%20March,census%20in%20England%20and%20Wales",
        target = "_blank",
        "2021 census."
      )
    ),
    "demog_scotland", "demographics", "Demographics", "Scotland", tags$p(
      "These indicators can be used alongside other indicators to understand
        the population breakdowns of the areas being assessed. All data come from
        the latest ",
      tags$a(
        href = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2021",
        target = "_blank",
        "mid-2021 population estimates."
      )
    ),
    "demog_ni", "demographics", "Demographics", "Northern Ireland", tags$p(
      "These indicators can be used alongside other indicators to understand
        the population breakdowns of the areas being assessed. All data come from
        the latest ",
      tags$a(
        href = "https://www.nisra.gov.uk/statistics/census/2021-census",
        target = "_blank",
        "2021 census."
      )
    )
  )


usethis::use_data(list_descriptions, internal = TRUE, overwrite = TRUE)
