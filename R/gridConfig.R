# Note: the blank* elements do not have a matching card in appUI
grid_config <- new_gridlayout(
  c(
    "1rem   1fr     200px            300px                     300px                     200px            1fr   ",
    "auto   blank1  blank3           header                    header                    blank4           blank2",
    "auto   blank1  blank3           intro                     intro                     blank4           blank2",
    "175px  blank1  blank3           select_geography          map                       blank4           blank2",
    "175px  blank1  blank3           select_areas              map                       blank4           blank2",
    "auto   blank1  blank3           summary_title             summary_title             blank4           blank2",
    "auto   blank1  blank3           help_button               help_button               blank4           blank2",
    "350px  blank1  summary_metrics  summary_metrics           summary_metrics           summary_metrics  blank2",
    "auto   blank1  blank5           summary_descriptions      summary_descriptions      blank6           blank2",
    "auto   blank1  blank5           secondary_title           secondary_title           blank6           blank2",
    "450px  blank1  secondary_care   secondary_care            secondary_care            secondary_care   blank2",
    "auto   blank1  blank7           secondary_descriptions    secondary_descriptions    blank8           blank2",
    "auto   blank1  blank7           demographics_title        demographics_title        blank8           blank2",
    "850px  blank1  demographics     demographics              demographics              demographics     blank2",
    "auto   blank1  blank9           demographics_descriptions demographics_descriptions blank10          blank2",
    "auto   blank1  blank9           footer                    footer                    blank10          blank2"
  )
)

# The elements in mobile_layout must match those in main_layout
# mobile_layout <- c(
#   "1rem  5px     1fr              5px   ",
#   "auto  blank_1 header           blank2",
#   "100px blank_1 select_geography blank2",
#   "100px blank_1 select_areas     blank2",
#   "250px blank_1 map              blank2",
#   "1fr   blank_1 summary_metrics  blank2",
#   "1fr   blank_1 secondary_care   blank2",
#   "1fr   blank_1 demographics     blank2"
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
