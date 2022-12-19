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
      tags$h1("Health Inequalities Explorer"),
      tags$h5(
        "Use this interactive tool to explore health statistics in your local
        area."
      ),
      tags$p(
        "Select a geography type and then select up to three areas in that
      geography by clicking the map or using the", tags$i("Select areas"),
        "box."
      ),
      tags$p(
        "Data in the plots are presented on a normalised range from -1 to 1 to
        allow statistics to be compared side-by-side and to see the underlying
        distributions in the data. Hover over points on the plots to find the
        relative (percent) and abolsute (number) values of the statistics."
      ),
      tags$p(
        "This app is open-source, and based on open datasets. Click the GitHub
        logo in the top-left corner for more information."
      ),
      tags$p(
        tags$i(
          "This is a new tool under development. Please provide feedback or bugs
          to mpage@redcross.org.uk"
        )
      ),
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
