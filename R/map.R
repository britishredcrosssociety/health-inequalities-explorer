mapUI <- function(id) {
  leafletOutput(
    NS(id, "map"),
    height = 450
  )
}

mapServer <- function(id, selected) {
  moduleServer(id, function(input, output, session) {
    # - Map -
    # Track which nation has been selected and set the map lat/long accordingly
    latitude <- reactive({
      if (grepl("^england_", selected$geography)) {
        52.75
      } else if (grepl("^scotland_", selected$geography)) {
        57
      }
      # else if (grepl("^wales_", selected$geography)) {
      #   52.13
      # } else if (grepl("^ni_", selected$geography)) {
      #   54.78
      # }
    })
    

    longitude <- reactive({
      if (grepl("^england_", selected$geography)) {
        -2.0
      } else if (grepl("^scotland_", selected$geography)) {
        -4.5
      }
      # else if (grepl("^wales_", selected$geography)) {
      #   -3.78
      # } else if (grepl("^ni_", selected$geography)) {
      #   -6.5
      # }
    })
    
    output$map <-
      renderLeaflet({
        # Create base map
        base_map <- leaflet() |>
          setView(lat = latitude(), lng = longitude(), zoom = 6) |>
          addProviderTiles(
            providers$CartoDB.Positron,
            options = providerTileOptions(minZoom = 6)
          ) |>
          setMaxBounds(-12, 49, 3.0, 61)

        # Add polygon layers
        base_map |>
          addPolygons(
            data = get(selected$geography),
            layerId = ~area_name,
            group = "base",
            label = ~area_name,
            weight = 0.7,
            opacity = 0.5,
            color = "#5C747A",
            dashArray = "0.1",
            fillOpacity = 0.2,
            highlight = highlightOptions(
              weight = 5,
              color = "#5C747A",
              dashArray = "",
              fillOpacity = 0.2,
              bringToFront = TRUE
            )
          ) |>
          addPolygons(
            data = get(selected$geography),
            layerId = ~area_code,
            # Create number of groups equal to the length of the number of areas
            # Match group names to layerId above so that group visibility can
            # be toggled on/off
            group = ~area_name,
            label = ~area_name,
            weight = 0.7,
            opacity = 0.5,
            color = "#193351",
            dashArray = "0.1",
            fillOpacity = 0.4,
            highlight = highlightOptions(
              weight = 5,
              color = "#193351",
              dashArray = "",
              fillOpacity = 0.4,
              bringToFront = TRUE
            )
          ) |>
          hideGroup(get(selected$geography)$area_name)
      })

    # - Interactivity logic -
    # Create two sets of reactive values. One contained to the module
    # namespace (`clicked`), and one to the global namespace (`selected`)

    # Contained to module namespace
    clicked <- reactiveValues(areas = vector())

    # Track click events and then update both the module and global reactive
    # values and add/remove polygons from the map. Limit the addition of
    # overlapping polygons to eight to match the select box.
    observeEvent(input$map_shape_click, {
      if (input$map_shape_click$group == "base" & length(clicked$areas) < 8) {
        selected$areas <- c(selected$areas, input$map_shape_click$id)
        clicked$areas <- c(clicked$areas, input$map_shape_click$id)
        leafletProxy("map") |> showGroup(input$map_shape_click$id)
        # Only hide non base group polygons
      } else if (input$map_shape_click$group != "base") {
        selected$areas <- setdiff(selected$areas, input$map_shape_click$group)
        clicked$areas <- setdiff(clicked$areas, input$map_shape_click$group)
        leafletProxy("map") |> hideGroup(input$map_shape_click$group)
      }
    })

    # Track differences in the module and global reactive values. If activity
    # has occured outside of this module, update the module reactive values to
    # match the global reactive values and update the map polygons
    observeEvent(selected$areas,
      {
        removed <- setdiff(clicked$areas, selected$areas)
        added <- setdiff(selected$areas, clicked$areas)

        leafletProxy("map") |>
          hideGroup(removed) |>
          showGroup(added)

        clicked$areas <- selected$areas
      },
      ignoreNULL = FALSE
    )
  })
}

mapTest <- function() {
  ui <- fluidPage(
    mapUI("test")
  )
  server <- function(input, output, session) {
    selected <- reactiveValues(
      areas = vector(), geography = "england_ltla_shp"
    )
    mapServer("test", selected)
  }
  shinyApp(ui, server)
}

# Examples
mapTest()
