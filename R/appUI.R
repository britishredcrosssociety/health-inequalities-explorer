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
        "Use this interactive tool to explore health statistics and demographics
         in your local area."
      ),
      tags$p(
        "Compare different areas by selecting a statistical geography (e.g., 
        Local Authorities) and then selecting up to three areas using the map 
        or", tags$i("Select areas"), "box."
      ),
      tags$p(
        "Data in the plots are presented on a scale from -1 to 1 to allow
        different indicators to be compared side-by-side while maintaining their
        underlying distributions. Hover over indivudal points on each plot to
        see their actual non-scaled values."
      ),
      tags$p(
        "This app is open-source, and based on open datasets. Click the GitHub
        logo in the top-left corner for licenses and more information."
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
      tags$h4("Select a statistical geography"),
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
      has_border = TRUE
    ),
    grid_card(
      "summary_metrics",
      # title = "Summary metrics",
      jitterPlotUI("summaryPlot"),
      has_border = FALSE
    ),
    grid_card(
      "secondary_care",
      # title = "Secondary Care",
      jitterPlotUI("secondaryCarePlot"),
      has_border = FALSE
    ),
    grid_card(
      "demographics",
      # title = "Demographics",
      jitterPlotUI("demographicsPlot"),
      has_border = FALSE
    )
  )
}
