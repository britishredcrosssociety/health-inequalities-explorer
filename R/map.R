mapUI <- function(id) {
  leafletOutput(
    NS(id, "map"),
    height = 600
  )
}

mapServer <- function(id, data, selected) {
  moduleServer(id, function(input, output, session) {
    output$map <-
      renderLeaflet({
        leaflet() |>
          setView(lat = 52.75, lng = -2.0, zoom = 6) |>
          addProviderTiles(providers$CartoDB.Positron) |>
          addPolygons(
            data = data,
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
            data = data,
            layerId = ~area_code,
            # Create number of groups equal to the length of the number of areas
            # Match group names to layerId above so that group visibility can
            # be toggled on/off
            group = ~area_name,
            label = ~area_name,
            weight = 0.7,
            opacity = 0.5,
            color = "#D0021B",
            dashArray = "0.1",
            fillOpacity = 0.4,
            highlight = highlightOptions(
              weight = 5,
              color = "#D0021B",
              dashArray = "",
              fillOpacity = 0.4,
              bringToFront = TRUE
            )
          ) |>
          hideGroup(data$area_name)
      })

    # Logic: create two sets of reactive values. One contained to the module
    # namespace (`clicked`), and one to the global namespace (`selected`)

    # Contained to local namespace
    clicked <- reactiveValues(areas = vector())

    # As polygons are clicked, update both the local and global reactive values
    observeEvent(input$map_shape_click, {
      if (input$map_shape_click$group == "base") {
        selected$areas <- c(selected$areas, input$map_shape_click$id)
        clicked$areas <- c(clicked$areas, input$map_shape_click$id)
        leafletProxy("map") |> showGroup(input$map_shape_click$id)
      } else {
        selected$areas <- setdiff(selected$areas, input$map_shape_click$group)
        clicked$areas <- setdiff(clicked$areas, input$map_shape_click$group)
        leafletProxy("map") |> hideGroup(input$map_shape_click$group)
      }
    })

    # Track differences in the local and global reactive values. If activity has
    # occured outside of this module, update the local reactive values to match
    # the global reactive values and update the map polygons

    observeEvent(selected$areas, {
      removed <- setdiff(clicked$areas, selected$areas)
      added <- setdiff(selected$areas, clicked$areas)

      leafletProxy("map") |>
        hideGroup(removed) |>
        showGroup(added)

      clicked$areas <- selected$areas
    })

    # observeEvent(selected$areas, {
    #   removed <- setdiff(clicked$areas, selected$areas)
    #   added <- setdiff(selected$areas, clicked$areas)

    #   if (length(removed) > 0) {
    #     clicked$areas <- selected$areas
    #     leafletProxy("map") |> hideGroup(group = removed)
    #   }

    #   if (length(added) > 0) {
    #     clicked$areas <- selected$areas
    #     leafletProxy("map") |> showGroup(group = added)
    #   }
    # })
  })
}

mapTest <- function(data) {
  ui <- fluidPage(
    mapUI("test")
  )
  server <- function(input, output, session) {
    selected <- reactiveValues(areas = vector())
    mapServer("test", data, selected)
  }
  shinyApp(ui, server)
}

# Examples
# mapTest(data = boundaries_ltla21_england)