guide <- Cicerone$
  new()$
  step(
    el = "select_geography",
    title = "Geographies",
    description = "Test geographies"
  )$
  step(
    el = "select_areas",
    title = "Areas",
    description = "Test areas"
  )