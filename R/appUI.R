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
      tags$h3("Health Inequalities Explorer"),
      alignment = "center",
      has_border = FALSE
    ),
    grid_card(
      "select_geography",
      title = "Select type of geography",
      selectGeographyUI("geography"),
      has_border = TRUE,
      scrollable = FALSE
    ),
    grid_card(
      "select_areas",
      title = "Select areas",
      selectAreasUI("areas"),
      has_border = TRUE,
      scrollable = FALSE
    ),
    grid_card(
      "map",
      mapUI("leafletMap"),
      has_border = TRUE
    ),
    grid_card(
      "summary_metrics",
      title = "Summary metrics",
      jitterPlotUI("summaryPlot"),
      has_border = TRUE
    ),
    grid_card(
      "secondary_care",
      title = "Secondary Care",
      jitterPlotUI("secondaryCarePlot"),
      has_border = TRUE
    ),
    grid_card(
      "demographics",
      title = "Demographics",
      jitterPlotUI("demographicsPlot"),
      has_border = TRUE
    )
  )
}
