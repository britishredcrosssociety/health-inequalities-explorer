main_layout <- c(
  "      1fr              1fr             ",
  "120px header           bookmark        ",
  "auto  select_geography select_areas    ",
  "1fr   map              summary_metrics ",
  "1fr   demographics     secondary_care  "
)

mobile_layout <- c(
  "      1fr             ",
  "120px header          ",
  "120px bookmark        ",
  "100px select_geography",
  "100px select_areas    ",
  "1fr   map             ",
  "1fr   summary_metrics ",
  "1fr   demographics    ",
  "1fr   secondary_care  "
)

grid_config <- new_gridlayout(
  main_layout,
  alternate_layouts = list(
    list(
      layout = mobile_layout,
      width_bounds = c(max = 600)
    )
  )
)
