selectBoxUI <- function(id, data) {
  selectizeInput(
    NS(id, "selectbox"),
    label = NULL,
    choices = c("Select area(s)" = "", sort(unique(data$area_name))),
    multiple = TRUE
  )
}

selectBoxServer <- function(id, selected_areas) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$selectbox,
      {
        input$selectbox |>
          selected_areas()
      },
      ignoreInit = TRUE
    )

    # This sits in its own observer because it needs to track any changes to
    # the global `selected_area` reactive value, not just the selectbox (above)
    observeEvent(selected_areas(), {
      updateSelectizeInput(
        session,
        "selectbox",
        selected = selected_areas()
      )
    })
  })
}

selectBoxTest <- function(data) {
  ui <- fluidPage(
    selectBoxUI("test", data)
  )
  server <- function(input, output, session) {
    selected_areas <- reactiveVal()
    selectBoxServer("test", selected_areas)
  }
  shinyApp(ui, server)
}

# Examples
# selectBoxTest(data = boundaries_ltla21_england)