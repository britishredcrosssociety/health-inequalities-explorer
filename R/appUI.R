ui <- function(request) {
  grid_page(

    # ---- Non-grid elements ----
    includeCSS("inst/www/styles.css"),
    use_cicerone(),

    # ---- Layout specified in R/gridConfig.R ----
    layout = grid_config,

    # ---- Header & Intro ----
    grid_card(
      "header",
      tags$h1(tags$b("Health Inequalities Explorer")),
      has_border = FALSE
    ),
    grid_card(
      "intro",
      tags$h4(
        tags$b(
          "Use this interactive tool to explore health statistics and demographics
         in your local area."
        )
      ),
      tags$p(
        "Compare different areas by first selecting a geography (e.g.,
        Local Authorities) and then selecting up to three areas using the map
        or", tags$i("Select areas"), "box."
      ),
      tags$p(
        "Data in the plots will then be populated and are presented on a scale
        from -1 to 1 to allow different indicators to be compared side-by-side,
        while maintaining their underlying distributions. Hover over individudal
        points on each plot to see their actual non-scaled values."
      ),
      tags$p(
        tags$em(
          "This is a new tool under development and currently has limited
          functionality. Other geographical areas, nations, and datasets
          will be added shortly. Please provide feedback or bugs to 
          mpage@redcross.org.uk"
        )
      ),
      has_border = FALSE,
      class = "intro"
    ),

    # ---- Selection ----
    grid_card(
      "select_geography",
      tags$h4("Select a geography"),
      selectGeographyUI("geography"),
      has_border = TRUE,
      scrollable = FALSE,
      class = "select-box"
    ),
    grid_card(
      "select_areas",
      tags$h4("Select areas to compare"),
      selectAreasUI("areas"),
      has_border = TRUE,
      scrollable = FALSE,
      class = "select-box"
    ),
    grid_card(
      "map",
      mapUI("leafletMap"),
      has_border = TRUE,
      class = "map"
    ),

    # ---- Summary indicators ----
    grid_card(
      "summary_intro",
      tags$h4(tags$b("Summary Indicators")),
      tags$p(
        "These indicators summarise a selection of health metrics into a single
        score. They can be useful for comparing the overall health of different
        areas and are a good place to start. But, they should not be used in
        isolation to make judgements about all aspects of an areas health. For
        example, an area may score poorly in a summary metric, yet still excel
        in certain aspects of health."
      ),
      tags$p(
        "The ONS Health Index provides an indication of health outcomes, risk
        factors, and the wider determinants of health. A detailed breakdown
        of the index can be viewed ",
        tags$a(
          href = "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/datasets/healthindexscoresengland",
          target = "_blank",
          "here."
        )
      ),
      tags$p(
        "Left-behind areas are places high in deprivation and socio-economic
        challenges, and low in social infrastructure and investment to meet those
        challenges. Research has shown they are associated with with health
        inequalities. More information and an interactive map of these areas
        can be found ",
        tags$a(
          href = "https://britishredcross.shinyapps.io/left-behind-areas/",
          target = "_blank",
          "here."
        )
      ),
      tags$p(
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
      tags$p(
        "For all of the summary indicators below, higher scores equal higher
        (i.e., worse) health inequalities."
      ),
      has_border = FALSE,
      class = "summary-intro"
    ),
    grid_card(
      "summary_metrics",
      # title = "Summary metrics",
      jitterPlotUI("summaryPlot"),
      has_border = FALSE
    ),

    # ---- Secondary care ----
    grid_card(
      "secondary_intro",
      tags$h4(tags$b("Secondary Care Indicators")),
      tags$p(
        "Secondary care indicators report on the direct performance of the national
         health service. Most secondary care statistics are reported only at
         the Trust level. This means that if you are viewing these statistics at
         a different geography, we have aggregated these statistics using 
         catchment population data. Fore more information on how we have done 
         this, see ",
        tags$a(
          href = "https://britishredcrosssociety.github.io/resilience-index-book/technical.html#health-capacity---england",
          target = "_blank",
          "here."
        )
      ),
      tags$p(
        "The Improving Access to Pyschological Therapies (IAPT) programme offers
        talking therapies for mental health problems. To address both access to
        mental health services and the success rate of their interventions, the
        percentage of referrals to were able to access a service within 18 weeks
        and also finished the first course of treatment, are presented below.
        More detailed statistics can be accessed ",
        tags$a(
          href = "https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-sets/improving-access-to-psychological-therapies-data-set/improving-access-to-psychological-therapies-data-set-reports",
          target = "_blank",
          "here."
        )
      ),
      tags$p(
        "Discharged beds indicates the total number of patients discharged from
        beds, and the percentage this makes up of all beds. More detailed
        breakdowns can be viewed ",
        tags$a(
          href = "https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-acute-data/",
          target = "_blank",
          "here."
        )
      ),
      tags$p(
        "Beds not meeting criteria to reside shows the number of patients who
        are no longer eligible to occupy a bed, and the percentage of these of
        all beds. This indicator is often a good proxy for where social care is
        low. For a more detailed breakdown, see",
        tags$a(
          href = "https://britishredcross.shinyapps.io/trust-discharge-criteria/",
          target = "_blank",
          "here."
        )
      ),
      tags$p(
        "In addition to the bed availability indicator presented below, our team
         has also produced a NHS England winter situation report explorer, with
         detailed breakdowns by type of bed, which can be seen ",
        tags$a(
          href = "https://britishredcross.shinyapps.io/sitrep-explorer/",
          target = "_blank",
          "here."
        )
      ),
      has_border = FALSE
    ),
    grid_card(
      "secondary_care",
      # title = "Secondary Care",
      jitterPlotUI("secondaryCarePlot"),
      has_border = FALSE
    ),
    grid_card(
      "demographics_intro",
      tags$h4(tags$b("Demographic Indicators")),
      tags$p(
        "These indicators can be used alongside other indicators to understand
        the population breakdowns of the areas being assesed. All data come from
        the latest ",
        tags$a(
          href = "https://www.ons.gov.uk/census",
          target = "_blank",
          "2021 census."
        )
      ),
      has_border = FALSE
    ),

    # ---- Demographics ----
    grid_card(
      "demographics",
      # title = "Demographics",
      jitterPlotUI("demographicsPlot"),
      has_border = FALSE
    ),
    grid_card(
      "footer",
      tags$h4(tags$b("Additional Information")),
      tags$p(
        "This app can remember your selections and activity so that you can share
        your findings with others. Click the bookmark button below to generate
        a shareable link:"
      ),
      bookmarkButton(),
      tags$p(
        "This app is open-source and uses open datasets. Click ",
        tags$a(
          href = "https://github.com/britishredcrosssociety/health-inequalities-explorer",
          target = "_blank",
          "here"
        ),
        " for licenses and more information."
      ),
      has_border = FALSE,
      class = "footer"
    )
  )
}
