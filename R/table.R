tableUI <- function(id) {
  gt::gt_output(NS(id, "table"))
}

tableServer <- function(id, selected) {
  moduleServer(id, function(input, output, session) {
    valid_geographies <- c(
      "england_ltla_shp",
      "brc_central_shp",
      "brc_central_icb_shp",
      "brc_london_shp",
      "brc_london_icb_shp",
      "brc_north_shp",
      "brc_north_icb_shp",
      "brc_south_shp",
      "brc_south_icb_shp",
      "brc_southeast_shp",
      "brc_southeast_icb_shp",
      "england_icb_shp",
      "scotland_ltla_shp",
      "northern_ireland_ltla_shp",
      "wales_ltla_shp"
    )

    # Select dataset based on geographical selection and type of data
    dataset <- reactive({
      if (
        selected$geography %in%
          c(
            "england_ltla_shp",
            "brc_central_shp",
            "brc_london_shp",
            "brc_north_shp",
            "brc_south_shp",
            "brc_southeast_shp"
          )
      ) {
        combine_subdomains(
          england_ltla_hi_outcomes_sub,
          england_ltla_hi_risk_factors_sub,
          england_ltla_hi_social_determinants_sub
        )
      } else if (
        selected$geography %in% 
          c(
            "england_icb_shp",
            "brc_central_icb_shp",
            "brc_london_icb_shp",
            "brc_north_icb_shp",
            "brc_south_icb_shp",
            "brc_southeast_icb_shp"
          )
        ) {
        combine_subdomains(
          england_icb_hi_outcomes_sub,
          england_icb_hi_risk_factors_sub,
          england_icb_hi_social_determinants_sub
        )
      } else if (selected$geography == "scotland_ltla_shp") {
        combine_subdomains(
          scotland_ltla_hi_outcomes_sub,
          scotland_ltla_hi_risk_factors_sub,
          scotland_ltla_hi_social_determinants_sub
        )
      } else if (selected$geography == "northern_ireland_ltla_shp") {
        combine_subdomains(
          northern_ireland_ltla_hi_outcomes_sub,
          northern_ireland_ltla_hi_risk_factors_sub,
          northern_ireland_ltla_hi_social_determinants_sub
        )
      } else if (selected$geography == "wales_ltla_shp") {
        combine_subdomains(
          wales_ltla_hi_outcomes_sub,
          wales_ltla_hi_risk_factors_sub,
          wales_ltla_hi_social_determinants_sub
        )
      }
    })

    output$table <- render_gt({
      if (selected$geography %in% valid_geographies) {
        table_selected(
          data = dataset(),
          selected_areas = selected$areas
        ) |>
          tab_style(
            style = cell_text(color = "transparent"),
            locations = cells_body(columns = -1)
            
          )
      } else {
        table_null()
      }
    })
  })
}

tableTest <- function() {
  ui <- fluidPage(
    tableUI("test")
  )

  server <- function(input, output, session) {
    selected <- reactiveValues(
      areas = c("Lincolnshire", "Norfolk and Waveney"),
      geography = "england_icb_shp"
    )
    tableServer("test", selected)
  }

  shinyApp(ui, server)
}

# Examples
tableTest()
