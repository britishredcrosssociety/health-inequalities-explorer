# Note: the blank* elements do not have a matching card in appUI
# Note: plot heights are determined using the following schema:
#         - Calculate a starting point by multiplying the number of
#           indicators in the plot by 130.
#         - Iteratively change the heights until they all look approximately
#           equal on the screen

grid_config <- new_gridlayout(
  c(
    "1rem    1fr     10px            175px            300px                     300px                     175px           10px                     1fr   ",
    "auto    blank1  blank11         blank3           header                    header                    blank4          blank15                  blank2",
    "auto    blank1  blank11         blank3           intro                     intro                     blank4          blank15                  blank2",
    "auto    blank1  blank11         blank3           user_guide                user_guide                blank4          blank15                  blank2",
    "180px   blank1  blank11         blank3           select_geography          map                       blank4          blank15                  blank2",
    "270px   blank1  blank11         blank3           select_areas              map                       blank4          blank15                  blank2",
    "auto    blank1  blank11         blank3           summary_title             summary_title             blank4          blank15                  blank2",
    "auto    blank1  blank11         blank3           summary_note              summary_note              blank4          blank15                  blank2",
    "auto    blank1  summary_metrics summary_metrics  summary_metrics           summary_metrics           summary_metrics help_button_summary      blank2",
    "auto    blank1  blank12         blank5           summary_descriptions      summary_descriptions      blank6          blank16                  blank2",
    "auto    blank1  blank12         blank5           hi_title                  hi_title                  hi_title        hi_title                 blank2",
    "auto    blank1  blank12         blank5           hi_note                   hi_note                   blank8          blank17                  blank2",
    "auto    blank1  hi_plot         hi_plot          hi_plot                   hi_plot                   hi_plot         hi_plot                  blank2",
    "auto    blank1  hi_domain       hi_domain        hi_domain                 hi_domain                 hi_domain       hi_domain                blank2",
    "auto    blank1  blank13         blank7           secondary_title           secondary_title           secondary_title secondary_title          blank2",
    "auto    blank1  blank13         blank7           secondary_note            secondary_note            blank10         blank18                  blank2",
    "auto    blank1  secondary_care  secondary_care   secondary_care            secondary_care            secondary_care  help_button_secondary    blank2",
    "auto    blank1  blank14         blank9           secondary_descriptions    secondary_descriptions    secondary_descriptio secondary_description blank2",
    "auto    blank1  blank14         blank9           demographics_title        demographics_title        demographics_title  demographics_title  blank2",
    "auto    blank1  blank14         blank9           demographics_note         demographics_note         demographics_note demographics_note     blank2",
    "auto    blank1  demographics    demographics     demographics              demographics              demographics    help_button_demographics blank2",
    "auto    blank1  placeholder     placeholder      demographics_descriptions demographics_descriptions demographics_descriptions   demographics_descriptions blank2",
    "auto    blank1  placeholder     placeholder      footer                    footer                    blank           blank                    blank2"
    #
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
