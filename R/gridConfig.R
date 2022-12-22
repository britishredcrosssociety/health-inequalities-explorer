grid_config <- new_gridlayout(
  c(
    "1rem   1fr          200px            300px            300px            200px            1fr         ",
    "auto   github       blank1           header           header           blank2           bookmark    ",
    "auto   left_margin  blank1           intro            intro            blank2           right_margin",
    "175px  left_margin  blank1           select_geography map              blank2           right_margin",
    "175px  left_margin  blank1           select_areas     map              blank2           right_margin",
    "auto   left_margin  blank1           summary_intro    summary_intro    blank2           right_margin",
    "350px  left_margin  summary_metrics  summary_metrics  summary_metrics  summary_metrics  right_margin",
    "350px  left_margin  secondary_care   secondary_care   secondary_care   secondary_care   right_margin",
    "850px  left_margin  demographics     demographics     demographics     demographics     right_margin",
    "auto   left_margin  blank3           footer           footer           blank4           right_margin"
  )
)
# mobile_layout <- c(
#   "1rem  5px         1fr              5px         ",
#   "120px left_margin header           right_margin",
#   "100px left_margin bookmark         right_margin",
#   "100px left_margin github           right_margin",
#   "100px left_margin select_geography right_margin",
#   "100px left_margin select_areas     right_margin",
#   "1fr   left_margin map              right_margin",
#   "1fr   left_margin summary_metrics  right_margin",
#   "1fr   left_margin secondary_care   right_margin",
#   "1fr   left_margin demographics     right_margin"
# )

# grid_config <- new_gridlayout(
#   main_layout,
#   alternate_layouts = list(
#     list(
#       layout = mobile_layout,
#       width_bounds = c(max = 600)
#     )
#   )
# )
