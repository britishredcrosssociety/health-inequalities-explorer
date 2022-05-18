mapUI <- function(id) {
  leafletOutput(
    NS(id, "map"),
    height = 1200
  )
}

mapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$map <-
      renderLeaflet({
        leaflet() |>
          setView(lat = 52.75, lng = -2.0, zoom = 6) |>
          addProviderTiles(providers$CartoDB.Positron) |>
          addPolygons(
            data = boundaries_ltla21_england,
            layerId = ~ltla21_code,
            weight = 0.7,
            opacity = 0.5,
            color = "#5C747A",
            dashArray = "0.1",
            fillOpacity = 0.4,
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE
            ),
            label = boundaries_ltla21_england$ltl2a_name
          )
      })
  })
}

mapTest <- function() {
  ui <- fluidPage(
    mapUI("test")
  )

  server <- function(input, output, session) {
    mapServer("test")
  }

  shinyApp(ui, server)
