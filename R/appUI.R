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
        tags$button(class = "guide-button", "SEE QUICK USER GUIDE")
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
      indicatorDescriptionsUI("summaryDescriptions")
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
      indicatorDescriptionsUI("secondaryCareDescriptions")
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
