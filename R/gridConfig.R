main_layout <- c(
  "1rem   1fr          500px            500px            1fr         ",
  "100px  github       header           header           bookmark    ",
  "150px  left_margin  map              select_geography right_margin",
  "150px  left_margin  map              select_areas     right_margin",
  "300px  left_margin  summary_metrics  summary_metrics  right_margin",
  "300px  left_margin  secondary_care   secondary_care   right_margin",
  "450px  left_margin  demographics     demographics     right_margin"
)

mobile_layout <- c(
  "1rem  5px         1fr              5px         ",
  "120px left_margin header           right_margin",
  "100px left_margin bookmark         right_margin",
  "100px left_margin github           right_margin",
  "100px left_margin select_geography right_margin",
  "100px left_margin select_areas     right_margin",
  "1fr   left_margin map              right_margin",
  "1fr   left_margin summary_metrics  right_margin",
  "1fr   left_margin secondary_care   right_margin",
  "1fr   left_margin demographics     right_margin"
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
