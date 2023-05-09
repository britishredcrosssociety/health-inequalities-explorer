ui <- function(request) {
  grid_page(

    # `grid_card()` notes:
    # - `area` arg is used in R/gridConfig.R
    # - `class` arg is used in inst/www/styles.css
    # - To make some cards act like accordions (which are hidden by default),
    #   they are piped into calls to `tagAppendAttributes(class = "collapsed")`
    #   and provided special card titles (from R/utils.R)

    # ---- Non-grid elements ----
    includeCSS("inst/www/styles.css"),

    # ---- Layout specified in R/gridConfig.R ----
    layout = grid_config,

    # ---- Header, Intro & User Guide ----
    grid_card(
      area = "header",
      has_border = FALSE,
      class = "header",
      tags$h1(tags$b("Health Inequalities Explorer"))
    ),
    grid_card(
      area = "intro",
      has_border = FALSE,
      class = "intro",
      tags$p(
        tags$span(class = "phase-banner", "ALPHA"),
        "This is a new service - new data will be added soon."
      ),
      tags$h4(
        tags$b(
          "Use this interactive tool to explore health statistics and
           demographics in your local area."
        )
      )
    ),
    grid_card(
      area = "user_guide",
      has_border = FALSE,
      tags$a(
        href = "https://medium.com/insight-and-improvement-at-british-red-cross/health-inequalities-explorer-f77025a2f1a3",
        target = "_blank",
        tags$button(class = "guide-banner", "SEE QUICK USER GUIDE")
      )
    ),

    # ---- Selection ----
    grid_card(
      area = "select_geography",
      has_border = TRUE,
      scrollable = FALSE,
      class = "select-box",
      tags$h4("Select a geography"),
      selectGeographyUI("geography")
    ),
    grid_card(
      area = "select_areas",
      has_border = TRUE,
      scrollable = FALSE,
      class = "select-box",
      tags$h4("Select up to five areas"),
      selectAreasUI("areas")
    ),
    grid_card(
      area = "map",
      has_border = TRUE,
      class = "map",
      mapUI("leafletMap")
    ),

    # ---- Summary indicators ----
    grid_card(
      area = "summary_title",
      has_border = FALSE,
      class = "summary-title",
      tags$h4(tags$b("Summary Indicators"))
    ),
    grid_card(
      area = "summary_note",
      has_border = FALSE,
      tags$p(
        tags$span(class = "note-banner", "NOTE"),
        "Clusters of points have similar values. See the help button for more
        info."
      )
    ),
    grid_card(
      area = "summary_metrics",
      has_border = FALSE,
      jitterPlotUI("summaryPlot")
    ),
    grid_card(
      area = "help_button_summary",
      has_border = FALSE,
      helpButtonUI("help_summary")
    ),
    grid_card(
      area = "summary_descriptions",
      has_border = FALSE,
      class = "indicator-details",
      collapsible = TRUE,
      title = title_collapsible("Show indicator details"),
      tags$p(
        "These indicators summarise a selection of health metrics into a single
        score. They can be useful for comparing the overall health of different
        areas and are a good place to start. But, they should not be used in
        isolation to make judgements about all aspects of an area's health. For
        example, an area may score poorly in a summary metric, yet still excel
        in certain aspects of health."
      ),
      tags$p(
        "The ONS Health Index provides an indication of health outcomes, risk
        factors, and the wider determinants of health. A detailed breakdown
        of the index can be viewed ",
        tags$a(
          href = "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/bulletins/healthinengland/2015to2020",
          target = "_blank",
          "here."
        )
      ),
      tags$p(
        "Left-behind areas are places high in deprivation and socio-economic
        challenges, and low in social infrastructure and investment to meet those
        challenges. Research has shown they are associated with high health
        inequalities. More information on these areas can be found ",
        tags$a(
          href = "https://ocsi.uk/left-behind-neighbourhoods/",
          target = "_blank",
          "here."
        ),
        " An interactice map to visualise these areas can be found",
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
      )
    ) |>
      tagAppendAttributes(class = "collapsed"),

    # ---- Secondary care ----
    grid_card(
      area = "secondary_title",
      has_border = FALSE,
      tags$h4(tags$b("Secondary Care Indicators"))
    ),
    grid_card(
      area = "secondary_note",
      has_border = FALSE,
      tags$p(
        tags$span(class = "note-banner", "NOTE"),
        "Clusters of points have similar values. See the help button for more
        info."
      )
    ),
    grid_card(
      area = "secondary_care",
      has_border = FALSE,
      jitterPlotUI("secondaryCarePlot")
    ),
    grid_card(
      area = "help_button_secondary",
      has_border = FALSE,
      helpButtonUI("help_secondary")
    ),
    grid_card(
      area = "secondary_descriptions",
      has_border = FALSE,
      class = "indicator-details",
      collapsible = TRUE,
      title = title_collapsible("Show indicator details"),
      tags$p(
        "Secondary care indicators report on the direct performance of the national
         health service. Most secondary care statistics are reported only at
         the Trust level. This means that if you are viewing these statistics at
         a different geography, we have aggregated these statistics using
         catchment population data. For more information on how we have done
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
        percentage of referrals that were able to access a service within 18
        weeks and also finished the first course of treatment are presented.
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
    ) |>
      tagAppendAttributes(class = "collapsed"),

    # ---- Demographics ----
    grid_card(
      area = "demographics_title",
      has_border = FALSE,
      tags$h4(tags$b("Demographic Indicators"))
    ),
    grid_card(
      area = "demographics_note",
      has_border = FALSE,
      tags$p(
        tags$span(class = "note-banner", "NOTE"),
        "Clusters of points have similar values. See the help button for more
        info."
      )
    ),
    grid_card(
      area = "demographics",
      has_border = FALSE,
      jitterPlotUI("demographicsPlot")
    ),
    grid_card(
      area = "help_button_demographics",
      has_border = FALSE,
      helpButtonUI("help_demographics")
    ),
    grid_card(
      area = "demographics_descriptions",
      has_border = FALSE,
      class = "indicator-details",
      collapsible = TRUE,
      title = title_collapsible("Show indicator details"),
      tags$p(
        "These indicators can be used alongside other indicators to understand
        the population breakdowns of the areas being assesed. All data come from
        the latest ",
        tags$a(
          href = "https://www.ons.gov.uk/census",
          target = "_blank",
          "2021 census."
        )
      )
    ) |>
      tagAppendAttributes(class = "collapsed"),

    # ---- Footer ----
    grid_card(
      area = "footer",
      has_border = FALSE,
      class = "footer",
      tags$p(
        "This app is open-source and uses open datasets. Click ",
        tags$a(
          href = "https://github.com/britishredcrosssociety/health-inequalities-explorer",
          target = "_blank",
          "here"
        ),
        " to see the underlying code, data, and licenses."
      )
    )
  )
}
