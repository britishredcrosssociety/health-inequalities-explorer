indicatorDescriptionsUI <- function(id) {
  uiOutput(NS(id, "indicatorDescriptions"))
}

indicatorDescriptionsServer <- function(id, selected, type) {
  moduleServer(id, function(input, output, session) {
    output$indicatorDescriptions <- renderUI({
      # ---- england_ltla_summary_metrics ----
      if (selected$geography == "england_ltla_shp" & type == "summary_metrics") {
        tagList(
          list_descriptions[list_descriptions$id == "summary_intro",]$tag,
          list_descriptions[list_descriptions$id == "health_index",]$tag,
          list_descriptions[list_descriptions$id == "lba",]$tag,
          list_descriptions[list_descriptions$id == "imd_england",]$tag
        )

        # ---- england_ltla_secondary_care ----
      } else if (selected$geography == "england_ltla_shp" & type == "secondary_care") {
        tagList(
          list_descriptions[list_descriptions$id == "second_care_intro_eng",]$tag,
          list_descriptions[list_descriptions$id == "iapt",]$tag,
          list_descriptions[list_descriptions$id == "discharged_eng",]$tag,
          list_descriptions[list_descriptions$id == "crit_reside_eng",]$tag
        )

        # ---- england_ltla_demographics ----
      } else if (selected$geography == "england_ltla_shp" & type == "demographics") {
        tagList(
          list_descriptions[list_descriptions$id == "demog_ONS",]$tag
        )

        # ---- england_icb_summary_metrics ----
      } else if (selected$geography == "england_icb_shp" & type == "summary_metrics") {
        tagList(
          list_descriptions[list_descriptions$id == "summary_intro",]$tag,
          list_descriptions[list_descriptions$id == "health_index",]$tag,
          list_descriptions[list_descriptions$id == "lba",]$tag,
          list_descriptions[list_descriptions$id == "imd_england",]$tag
        )

        # ---- england_icb_secondary_care ----
      } else if (selected$geography == "england_icb_shp" & type == "secondary_care") {
        tagList(
          list_descriptions[list_descriptions$id == "second_care_intro_eng",]$tag,
          list_descriptions[list_descriptions$id == "iapt",]$tag,
          list_descriptions[list_descriptions$id == "discharged_eng",]$tag,
          list_descriptions[list_descriptions$id == "crit_reside_eng",]$tag
        )

        # ---- england_icb_demographics ----
      } else if (selected$geography == "england_icb_shp" & type == "demographics") {
        tagList(
          list_descriptions[list_descriptions$id == "demog_ONS",]$tag
        )

        # ---- scotland_ltla_summary_metrics ----
      } else if (selected$geography == "scotland_ltla_shp" & type == "summary_metrics") {
        tagList(
          list_descriptions[list_descriptions$id == "summary_intro",]$tag,
          list_descriptions[list_descriptions$id == "health_index_devolved",]$tag,
          list_descriptions[list_descriptions$id == "lba",]$tag,
          list_descriptions[list_descriptions$id == "imd_scotland",]$tag
        )

        # ---- scotland_ltla_secondary_care ----
      } else if (selected$geography == "scotland_ltla_shp" & type == "secondary_care") {
        tagList(
          list_descriptions[list_descriptions$id == "delayed_discharged_scotland",]$tag
        )

        # ---- scotland_ltla_demographics ----
      } else if (selected$geography == "scotland_ltla_shp" & type == "demographics") {
        tagList(
          list_descriptions[list_descriptions$id == "demog_scotland",]$tag
        )

        # ---- scotland_hb_summary_metrics ----
      } else if (selected$geography == "scotland_hb_shp" & type == "summary_metrics") {
        tagList(
          list_descriptions[list_descriptions$id == "summary_intro",]$tag,
          list_descriptions[list_descriptions$id == "health_index_devolved",]$tag,
          list_descriptions[list_descriptions$id == "lba",]$tag,
          list_descriptions[list_descriptions$id == "imd_scotland",]$tag
        )

        # ---- scotland_hb_secondary_care ----
      } else if (selected$geography == "scotland_hb_shp" & type == "secondary_care") {
        tagList(
          list_descriptions[list_descriptions$id == "rtt_scotland",]$tag,
          list_descriptions[list_descriptions$id == "delayed_discharged_scotland",]$tag,
          list_descriptions[list_descriptions$id == "bed_availability_scotland",]$tag,
        )

        # ---- scotland_hb_demographics ----
      } else if (selected$geography == "scotland_hb_shp" & type == "demographics") {
        tagList(
          list_descriptions[list_descriptions$id == "demog_scotland",]$tag
        )

        # ---- northern_ireland_ltla_summary_metrics ----
      } else if (selected$geography == "northern_ireland_ltla_shp" & type == "summary_metrics") {
        tagList(
          list_descriptions[list_descriptions$id == "summary_intro",]$tag,
          list_descriptions[list_descriptions$id == "health_index_devolved",]$tag,
          list_descriptions[list_descriptions$id == "lba",]$tag,
          list_descriptions[list_descriptions$id == "imd_ni",]$tag
        )

        # ---- northern_ireland_ltla_secondary_care ----
      } else if (selected$geography == "northern_ireland_ltla_shp" & type == "secondary_care") {
        tagList(
          list_descriptions[list_descriptions$id == "unpaid_care",]$tag
        )

        # ---- northern_ireland_ltla_demographics ----
      } else if (selected$geography == "northern_ireland_ltla_shp" & type == "demographics") {
        tagList(
          list_descriptions[list_descriptions$id == "demog_ni",]$tag
        )

        # ---- northern_ireland_hsct_summary_metrics ----
      } else if (selected$geography == "northern_ireland_hsct_shp" & type == "summary_metrics") {
        tagList(
          list_descriptions[list_descriptions$id == "summary_intro",]$tag,
          list_descriptions[list_descriptions$id == "health_index_devolved",]$tag,
          list_descriptions[list_descriptions$id == "lba",]$tag,
          list_descriptions[list_descriptions$id == "imd_ni",]$tag
        )

        # ---- northern_ireland_hsct_secondary_care ----
      } else if (selected$geography == "northern_ireland_hsct_shp" & type == "secondary_care") {
        tagList(
          list_descriptions[list_descriptions$id == "rtt_ni",]$tag,
          list_descriptions[list_descriptions$id == "bed_availability_ni",]$tag,
        )

        # ---- northern_ireland_hsct_demographics ----
      } else if (selected$geography == "northern_ireland_hsct_shp" & type == "demographics") {
        tagList(
          list_descriptions[list_descriptions$id == "demog_ni",]$tag
        )

        # ---- wales_ltla_summary_metrics ----
      } else if (selected$geography == "wales_ltla_shp" & type == "summary_metrics") {
        tagList(
          list_descriptions[list_descriptions$id == "summary_intro",]$tag,
          list_descriptions[list_descriptions$id == "health_index_devolved",]$tag,
          list_descriptions[list_descriptions$id == "lba",]$tag,
          list_descriptions[list_descriptions$id == "imd_wales",]$tag,
        )

        # ---- wales_ltla_secondary_care ----
        # No data available
        
        # ---- wales_ltla_demographics ----
      } else if (selected$geography == "wales_ltla_shp" & type == "demographics") {
        tagList(
          list_descriptions[list_descriptions$id == "demog_ONS",]$tag
        )

        # ---- wales_lhb_summary_metrics ----
      } else if (selected$geography == "wales_lhb_shp" & type == "summary_metrics") {
        tagList(
          list_descriptions[list_descriptions$id == "summary_intro",]$tag,
          list_descriptions[list_descriptions$id == "health_index_devolved",]$tag,
          list_descriptions[list_descriptions$id == "lba",]$tag,
          list_descriptions[list_descriptions$id == "imd_wales",]$tag,
        )

        # ---- wales_lhb_secondary_care ----
      } else if (selected$geography == "wales_lhb_shp" & type == "secondary_care") {
        tagList(
          list_descriptions[list_descriptions$id == "rtt_wales",]$tag,
          list_descriptions[list_descriptions$id == "bed_availability_wales",]$tag,
        )

        # ---- wales_lhb_demographics ----
      } else if (selected$geography == "wales_lhb_shp" & type == "demographics") {
        tagList(
          list_descriptions[list_descriptions$id == "demog_ONS",]$tag
        )
      }
    })
  })
}

indicatorDescriptionsTest <- function() {
  ui <- fluidPage(
    indicatorDescriptionsUI("test")
  )
  server <- function(input, output, session) {
    selected <- reactiveValues(
      areas = vector(), geography = "northern_ireland_ltla_shp"
    )
    indicatorDescriptionsServer("test", selected, type = "summary_metrics")
  }
  shinyApp(ui, server)
}

# Examples
# indicatorDescriptionsTest()
