mapUI <- function(id) {
  leafletOutput(
    NS(id, "map"),
    height = 600
  )
}

mapServer <- function(id, data, selected) {
  moduleServer(id, function(input, output, session) {

    # Create static elemets
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
            # and match to click event ids from base layer
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

    observeEvent(input$map_shape_click, {
      if (input$map_shape_click$group == "base") {
        selected$areas <- c(selected$areas, input$map_shape_click$id)
        leafletProxy("map") |> showGroup(input$map_shape_click$id)
      } else {
        selected$areas <- setdiff(selected$areas, input$map_shape_click$group)
        leafletProxy("map") |> hideGroup(input$map_shape_click$group)
      }
    })
  })
}

mapTest <- function(data) {
  ui <- fluidPage(
    mapUI("test")
  )
  server <- function(input, output, session) {
    selected <- reactiveValues(areas = NULL)
    mapServer("test", data, selected)
    observe({
      print(selected$areas)
    })
  }
  shinyApp(ui, server)
}

# Examples
# mapTest(data = boundaries_ltla21_england)