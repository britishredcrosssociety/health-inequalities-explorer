ui <- function() {
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
        style = "color: #1d70b8",
        tags$i("New Updates:")
      ),
      tags$p(
        tags$ul(
          tags$li(
            style = "color: #1d70b8",
            tags$i(
              "March 2024: ONS Health Index Domains and Sub-domains added for England Local Authorities; BRC Regional filters added"
            )
          )
        )
      ),
      tags$p(
        tags$span(class = "phase-banner", "ALPHA"),
        "This is a new service - new data and features are being added."
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
        tags$button(class = "guide-button", "SEE QUICK USER GUIDE")
      )
    ),


    # ---- Selection ----
    grid_card(
      area = "select_region",
      has_border = TRUE,
      scrollable = FALSE,
      class = "select-box",
      tags$h4("Select a nation/region"),
      selectRegionUI("region")
    ),
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
      tags$h4("Select up to eight areas"),
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
      indicatorDescriptionsUI("summaryDescriptions")
    ) |>
      tagAppendAttributes(class = "collapsed"),

    # ---- Health Index ----
    grid_card(
      area = "hi_title",
      has_border = FALSE,
      tags$h4(tags$b("Health Index"))
    ),
    grid_card(
      area = "hi_note",
      has_border = FALSE,
      tags$p(
        tags$span(class = "note-banner", "NOTE"),
        "Clusters of points have similar values. See the help button for more
        info."
      )
    ),
    grid_card(
      area = "hi_plot",
      has_border = FALSE,
      jitterPlotUI("healthindexPlot")
    ),
    grid_card(
      area = "help_button_health_index",
      has_border = FALSE,
      helpButtonUI("help_health_index")
    ),
    grid_nested(
      "hi_domain",
      layout = c(
        "hi_domain_title help_hi_domains help_hi_domains",
        "hi_text hi_text hi_text",
        "people_title places_title lives_title",
        "people_domain  places_domain lives_domain",
        "peoplesub_title placessub_title livessub_title",
        "people_subdomain  places_subdomain lives_subdomain",
        "subdomain_text subdomain_text subdomain_text",
        "hi_source hi_source hi_source"
        
      ),
      # Link to ONS HI page
      grid_card(
        area = "hi_source",
        has_border = FALSE,
        tags$p(
          "For more information on the ONS Health Index, its underlying methodology and indicators, please see",
          tags$a(
            href = "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/methodologies/healthindexindicatorsanddefinitions#the-health-index-and-what-it-covers",
            target = "_blank",
            "here."
          )
        )
      ),
      # Overall title
      grid_card("hi_domain_title",
        has_border = FALSE,
        tags$h6(tags$b("Health Index Domains and Sub-Domains"))
      ),
      grid_card(
        area = "help_hi_domains",
        has_border = FALSE,
        helpButtonHIUI("help_hi_domains")
      ),

      # Explanatory text
      grid_card(
        area = "hi_text",
        has_border = FALSE,
        collapsible = TRUE,
        tags$p(
          "The ONS Health Index score can be broken down into three areas of health,
          known as domains - Health Outcomes (People), Preventable Risk Factors (Lives) and
          Social Determinants of Health (Places). Each domain contains several indicators, or subdomains
          that represent overarching topics related to their respective domain."
        ),
        tags$p(
          "Click the help (?) button above for information on interpreting the scores."
        )
      ),

      # Domain titles
      grid_card("people_title",
        has_border = FALSE,
        conditionalPanel(
          condition = "['england_ltla_shp', 'brc_central_shp', 'brc_london_shp', 'brc_north_shp', 'brc_south_shp', 'brc_southeast_shp'].includes(input['geography-selectGeography'])",
          # condition = "input['geography-selectGeography'] == 'england_ltla_shp'",
          tags$h6(tags$b("Health Outcomes Domain"))
        )
      ),
      grid_card("lives_title",
        has_border = FALSE,
        conditionalPanel(
          condition = "['england_ltla_shp', 'brc_central_shp', 'brc_london_shp', 'brc_north_shp', 'brc_south_shp', 'brc_southeast_shp'].includes(input['geography-selectGeography'])",
          # condition = "input['geography-selectGeography'] == 'england_ltla_shp'",
          tags$h6(tags$b("Preventable Risk Factors Domain"))
        )
      ),
      grid_card("places_title",
        has_border = FALSE,
        conditionalPanel(
          condition = "['england_ltla_shp', 'brc_central_shp', 'brc_london_shp', 'brc_north_shp', 'brc_south_shp', 'brc_southeast_shp'].includes(input['geography-selectGeography'])",
          # condition = "input['geography-selectGeography'] == 'england_ltla_shp'",
          tags$h6(tags$b("Social Determinants of Health Domain"))
        )
      ),
      # Domain plots
      grid_card("people_domain",
        has_border = FALSE,
        conditionalPanel(
          condition = "['england_ltla_shp', 'brc_central_shp', 'brc_london_shp', 'brc_north_shp', 'brc_south_shp', 'brc_southeast_shp'].includes(input['geography-selectGeography'])",
          # condition = "input['geography-selectGeography'] == 'england_ltla_shp'",
          barPlotUI("hioutcomesPlot")
        )
      ),
      grid_card("lives_domain",
        has_border = FALSE,
        conditionalPanel(
          condition = "['england_ltla_shp', 'brc_central_shp', 'brc_london_shp', 'brc_north_shp', 'brc_south_shp', 'brc_southeast_shp'].includes(input['geography-selectGeography'])",
          # condition = "input['geography-selectGeography'] == 'england_ltla_shp'",
          barPlotUI("hiriskfactorsPlot")
        )
      ),
      grid_card("places_domain",
        has_border = FALSE,
        conditionalPanel(
          condition = "['england_ltla_shp', 'brc_central_shp', 'brc_london_shp', 'brc_north_shp', 'brc_south_shp', 'brc_southeast_shp'].includes(input['geography-selectGeography'])",
          # condition = "input['geography-selectGeography'] == 'england_ltla_shp'",
          barPlotUI("hisocialdeterminantsPlot")
        )
      ),
      # Sub-titles
      grid_card("peoplesub_title",
        has_border = FALSE,
        conditionalPanel(
          condition = "['england_ltla_shp', 'brc_central_shp', 'brc_london_shp', 'brc_north_shp', 'brc_south_shp', 'brc_southeast_shp'].includes(input['geography-selectGeography'])",
          # condition = "input['geography-selectGeography'] == 'england_ltla_shp'",
          tags$h6(tags$b("Health Outcomes Sub-Domains"))
        )
      ),
      grid_card("livessub_title",
        has_border = FALSE,
        conditionalPanel(
          condition = "['england_ltla_shp', 'brc_central_shp', 'brc_london_shp', 'brc_north_shp', 'brc_south_shp', 'brc_southeast_shp'].includes(input['geography-selectGeography'])",
          # condition = "input['geography-selectGeography'] == 'england_ltla_shp'",
          tags$h6(tags$b("Preventable Risk Factors Sub-Domains"))
        )
      ),
      grid_card("placessub_title",
        has_border = FALSE,
        conditionalPanel(
          condition = "['england_ltla_shp', 'brc_central_shp', 'brc_london_shp', 'brc_north_shp', 'brc_south_shp', 'brc_southeast_shp'].includes(input['geography-selectGeography'])",
          # condition = "input['geography-selectGeography'] == 'england_ltla_shp'",
          tags$h6(tags$b("Social Determinants of Health Sub-Domains"))
        )
      ),
      # Sub-domain explnatory text
      grid_card(
        area = "subdomain_text",
        has_border = FALSE,
        tags$p(
          "Each subdomain contains a number of indicators that measure a particular aspect of health.
    Low numbers mean worse health and high numbers mean better health. Boxes in ",
          tags$span(
            "red",
            style = "color: white; background-color: #ee2a24; padding: 0 4px; border-radius: 4px;"
          ),
          " represent the area's worst performing indicator by subdomain."
        )
      ),
      
      
      # Sub-domain tables
      grid_card("people_subdomain",
        has_border = FALSE,
        scrollable = TRUE,
        style = "overflow-x: auto; overflow-y: hidden;", # only enable horizontal scroll
        
        conditionalPanel(
          condition = "['england_ltla_shp', 'brc_central_shp', 'brc_london_shp', 'brc_north_shp', 'brc_south_shp', 'brc_southeast_shp'].includes(input['geography-selectGeography'])",
          # condition = "input['geography-selectGeography'] == 'england_ltla_shp'",
          tableUI("peopleSubdomainTable")
        )
      ),
      grid_card("lives_subdomain",
        has_border = FALSE, scrollable = TRUE,
        style = "overflow-x: auto; overflow-y: hidden;",
        
        conditionalPanel(
          condition = "['england_ltla_shp', 'brc_central_shp', 'brc_london_shp', 'brc_north_shp', 'brc_south_shp', 'brc_southeast_shp'].includes(input['geography-selectGeography'])",
          # condition = "input['geography-selectGeography'] == 'england_ltla_shp'",
          tableUI("livesSubdomainTable")
        )
      ), # only enable horizontal scroll
      grid_card("places_subdomain",
        has_border = FALSE,
        scrollable = TRUE,
        style = "overflow-x: auto; overflow-y: hidden;", # only enable horizontal scroll
        
        conditionalPanel(
          condition = "['england_ltla_shp', 'brc_central_shp', 'brc_london_shp', 'brc_north_shp', 'brc_south_shp', 'brc_southeast_shp'].includes(input['geography-selectGeography'])",
          # condition = "input['geography-selectGeography'] == 'england_ltla_shp'",
          tableUI("placesSubdomainTable")
        )
      )
    ),


    # ---- Secondary care ----
    # Secondary care in Wales LTLA does not have data
    # So note content & plot appearance is conditional on geography selected
    grid_card(
      area = "secondary_title",
      has_border = FALSE,
      tags$h4(tags$b("Secondary Care Indicators"))
    ),
    grid_card(
      area = "secondary_note",
      has_border = FALSE,
      conditionalPanel(
        condition = "input['geography-selectGeography'] == 'wales_ltla_shp'",
        tags$p(
          tags$span(class = "note-banner", "NOTE"),
          "No data is available at Local Authorities level, please refer to
          the Local Health Boards view for information on secondary care
          indicators in Wales"
        )
      ),
      conditionalPanel(
        condition = "input['geography-selectGeography'] != 'wales_ltla_shp'",
        tags$p(
          tags$span(class = "note-banner", "NOTE"),
          "Clusters of points have similar values. See the help button for more
        info."
        )
      )
    ),
    grid_card(
      area = "secondary_care",
      has_border = FALSE,
      conditionalPanel(
        condition = "input['geography-selectGeography'] != 'wales_ltla_shp'",
        jitterPlotUI("secondaryCarePlot")
      )
    ),
    grid_card(
      area = "help_button_secondary",
      has_border = FALSE,
      conditionalPanel(
        condition = "input['geography-selectGeography'] != 'wales_ltla_shp'",
        helpButtonUI("help_secondary")
      )
    ),
    grid_card(
      area = "secondary_descriptions",
      has_border = FALSE,
      class = "indicator-details",
      collapsible = TRUE,
      title = title_collapsible("Show indicator details"),
      conditionalPanel(
        condition = "input['geography-selectGeography'] != 'wales_ltla_shp'",
        indicatorDescriptionsUI("secondaryCareDescriptions")
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
      indicatorDescriptionsUI("demographicsDescriptions")
    ) |>
      tagAppendAttributes(class = "collapsed"),

    # ---- Footer ----
    grid_card(
      area = "placeholder",
      has_border = FALSE,
      tags$p(
        "  "
      )
    ),
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
