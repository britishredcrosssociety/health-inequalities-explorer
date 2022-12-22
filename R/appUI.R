ui <- function(request) {
  grid_page(
    includeCSS("inst/www/styles.css"),
    layout = grid_config,

    # Card layout specified in R/gridConfig.R
    grid_card(
      "left_margin",
      has_border = FALSE
    ),
    grid_card(
      "right_margin",
      has_border = FALSE
    ),
    grid_card(
      "github",
      tags$a(
        href = "https://github.com/britishredcrosssociety/health-inequalities-explorer",
        target = "_blank",
        icon("github", "fa-2x")
      ),
      has_border = FALSE
    ),
    grid_card(
      "bookmark",
      bookmarkButton(),
      has_border = FALSE
    ),
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
        "Compare different areas by first selecting a statistical geography (e.g.,
        Local Authorities) and then selecting up to three areas using the map
        or", tags$i("Select areas"), "box."
      ),
      tags$p(
        "Data in the plots will then be populated and are presented on a scale
        from -1 to 1 to allow different indicators to be compared side-by-side,
        while maintaining their underlying distributions. Hover over indivudal
        points on each plot to see their actual non-scaled values."
      ),
      has_border = FALSE,
      class = "intro"
    ),
    grid_card(
      "select_geography",
      tags$h4(tags$b("Select a statistical geography")),
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
    grid_card(
      "summary_intro",
      tags$h4("Summary Indicators"),
      tags$p(
        "These indicators summarise a selection of health metrics into a single
        score. They can be useful for comparing the overall health of different
        areas and are a good place to start. But, They should not be used in 
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
      has_border = FALSE
    ),
    grid_card(
      "summary_metrics",
      # title = "Summary metrics",
      jitterPlotUI("summaryPlot"),
      has_border = FALSE
    ),
    # grid_card(
    #   "secondary_intro",
    #   tags$h4(tags$b("Secondary Care Indicators")),
    #   tags$p(
    #     "The indicators report system performance."
    #   ),
    #   tags$p(
    #     "",
    #     tags$a(
    #         href = "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/datasets/healthindexscoresengland",
    #         target = "_blank",
    #         "here."
    #       )
    #   ),
    #   tags$p(
    #     "",
    #     tags$a(
    #         href = "https://britishredcross.shinyapps.io/left-behind-areas/",
    #         target = "_blank",
    #         "here."
    #       )
    #   ),
    #   tags$p(
    #     "",
    #     tags$a(
    #         href = "https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019",
    #         target = "_blank",
    #         "here."
    #       )
    #   ),
    #   has_border = FALSE
    # ),
    grid_card(
      "secondary_care",
      # title = "Secondary Care",
      jitterPlotUI("secondaryCarePlot"),
      has_border = FALSE
    ),
    # grid_card(
    #   "demographics_intro",
    #   tags$h4(tags$b("Demographic Indicators")),
    #   tags$p(
    #     "These indicators can be used alongside other indicators to understand
    # the populations being studied?"
    #   ),
    #   tags$p(
    #     "",
    #     tags$a(
    #         href = "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/datasets/healthindexscoresengland",
    #         target = "_blank",
    #         "here."
    #       )
    #   ),
    #   tags$p(
    #     "",
    #     tags$a(
    #         href = "https://britishredcross.shinyapps.io/left-behind-areas/",
    #         target = "_blank",
    #         "here."
    #       )
    #   ),
    #   tags$p(
    #     "",
    #     tags$a(
    #         href = "https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019",
    #         target = "_blank",
    #         "here."
    #       )
    #   ),
    #   has_border = FALSE
    # ),
    grid_card(
      "demographics",
      # title = "Demographics",
      jitterPlotUI("demographicsPlot"),
      has_border = FALSE
    ),
    grid_card(
      "footer",
       tags$p(
        "This app is open-source and uses open datasets. Click the GitHub
        logo in the top-left corner for license information and more."
      ),
      tags$p(
        tags$em(
          "This is a new tool under development. Please provide feedback or bugs
          to mpage@redcross.org.uk"
        )
      ),
      has_border = FALSE,
      class = "footer"
    )
  )
}
