mapUI <- function(id) {
  leafletOutput(
    NS(id, "map"),
    height = 450
  )
}

mapServer <- function(id, selected) {
  moduleServer(id, function(input, output, session) {

    # - Map -
    output$map <-
      renderLeaflet({

        # Create base map
        base_map <- leaflet() |>
          setView(lat = 52.75, lng = -2.0, zoom = 6) |>
          addProviderTiles(
            providers$CartoDB.Positron,
            options = providerTileOptions(minZoom = 6)
          ) |> 
          setMaxBounds(-12, 49, 3.0, 61 )

        # Depending upon the type of geography selected, use either markers (for
        # point geometries) or polygons (for multipoint geometries)
        if (selected$geography == "trusts_shp_england") {
          base_map |>
            addAwesomeMarkers(
              data = get(selected$geography),
              layerId = ~area_name,
              group = "base",
              label = ~area_name,
              options = markerOptions(
                opacity = .8,
                riseOnHover = TRUE
              ),
              icon = awesomeIcons(
                icon = "hospital-o",
                library = "fa",
                markerColor = "lightgray",
                iconColor = "#FFFFFF"
              )
            ) |>
            addAwesomeMarkers(
              data = get(selected$geography),
              layerId = ~area_code,
              # Create number of groups equal to the length of the number of areas
              # Match group names to layerId above so that group visibility can
              # be toggled on/off
              group = ~area_name,
              label = ~area_name,
              options = markerOptions(
                opacity = .8,
                riseOnHover = TRUE
              ),
              icon = awesomeIcons(
                icon = "hospital-o",
                library = "fa",
                markerColor = "cadetblue",
                iconColor = "#FFFFFF"
              )
            ) |>
            hideGroup(get(selected$geography)$area_name)
        } else {
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
        }
      })

    # - Interactivity logic -
    # Create two sets of reactive values. One contained to the module
    # namespace (`clicked`), and one to the global namespace (`selected`)

    # Contained to module namespace
    clicked <- reactiveValues(areas = vector())

    # - Track click events and then update both the module and global reactive
    #   values and add/remove polygons from the map. Limit the addition of
    #   overlapping polygons/markers to five to match the select box.
    # - Depending upon the geography selected, change the object event name
    #   (between map_marker_click and map_shape_click)
    observeEvent(
      if (selected$geography == "trusts_shp_england") {
        input$map_marker_click
      } else {
        input$map_shape_click
      },
      {
        if (selected$geography == "trusts_shp_england") {
          if (input$map_marker_click$group == "base" & length(clicked$areas) < 5) {
            selected$areas <- c(selected$areas, input$map_marker_click$id)
            clicked$areas <- c(clicked$areas, input$map_marker_click$id)
            leafletProxy("map") |> showGroup(input$map_marker_click$id)
            # Only hide non base group polygons
          } else if (input$map_marker_click$group != "base") {
            selected$areas <- setdiff(selected$areas, input$map_marker_click$group)
            clicked$areas <- setdiff(clicked$areas, input$map_marker_click$group)
            leafletProxy("map") |> hideGroup(input$map_marker_click$group)
          }
        } else {
          if (input$map_shape_click$group == "base" & length(clicked$areas) < 5) {
            selected$areas <- c(selected$areas, input$map_shape_click$id)
            clicked$areas <- c(clicked$areas, input$map_shape_click$id)
            leafletProxy("map") |> showGroup(input$map_shape_click$id)
            # Only hide non base group polygons
          } else if (input$map_shape_click$group != "base") {
            selected$areas <- setdiff(selected$areas, input$map_shape_click$group)
            clicked$areas <- setdiff(clicked$areas, input$map_shape_click$group)
            leafletProxy("map") |> hideGroup(input$map_shape_click$group)
          }
        }
      }
    )

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
      areas = vector(), geography = "ltla_shp_england"
    )
    mapServer("test", selected)
  }
  shinyApp(ui, server)
}

# Examples
# mapTest()
