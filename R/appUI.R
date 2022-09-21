ui <- function(request) {
  grid_page(
    includeCSS("inst/www/styles.css"),
    layout = grid_config,
    grid_card(
      "header",
      title = "Header",
      tags$h3("Health Inequalities Explorer"),
      alignment = "center",
      has_border = TRUE
    ),
    grid_card(
      "bookmark",
      title = "Bookmark button",
      bookmarkButton()
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
      jitterPlotUI("jitterPlotVulnerability"),
      has_border = TRUE
    ),
    grid_card(
      "demographics",
      title = "Demographics",
      has_border = TRUE
    ),
    grid_card(
      "secondary_care",
      title = "Secondary Care",
      has_border = TRUE
    )
  )
}
