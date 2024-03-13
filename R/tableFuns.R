# ---- Prepare selected data ----
table_prep <- function(data, selected_areas) {
  
  # if (is.null(selected_areas)) {
  #   # User hasn't selected a geographical area so just show national mean
  #   
  #   
  # } else {
  #   
  # }
  
  # If selected_areas is NULL then this will just return "variable" and "National Mean"
  data <- data[, c("variable", "National Mean", selected_areas)]
  
  data[, c("National Mean", selected_areas)] <- round(data[, c("National Mean", selected_areas)], 0)
  
  # data <- data |>
  #   tidyr::pivot_longer(cols = -variable) |>
  #   tidyr::pivot_wider(names_from = "variable", values_from = "value")
  
  # data$area_name <- string_wrap(data$area_name)
  # 
  # data$selected <- ifelse(data$area_name %in% string_wrap(selected_areas), data$area_name, "not selected")
  # 
  # data$alpha <- ifelse(data$selected != "not selected", 1, 0.1)
  # 
  # data$selected <- factor(data$selected)
  # 
  # if ("not selected" %in% levels(data$selected)) {
  #   data$selected <- relevel(data$selected, ref = "not selected")
  # }
  
  return(data)
}

# ---- Only show national score while waiting for selection ----
# table_null <- function(data) {
#   indicator_count <- length(unique(data$variable))
#   
#   table <- formattable(
#     data
#   )
#   
#   plot <- ggplot(
#     data,
#     aes(
#       x = scaled_1_1,
#       y = variable,
#       text = label
#     )
#   ) +
#     geom_point(
#       position = position_jitter(height = 0.25, width = 0.1, seed = 123),
#       size = 4,
#       shape = 21,
#       alpha = 0.1,
#       fill = "#717171",
#       colour = "#262626"
#     ) +
#     annotate(
#       "segment",
#       x = 0,
#       xend = 0,
#       y = 0.5,
#       yend = indicator_count + 0.5,
#       colour = "#262626",
#       linetype = "dashed",
#       alpha = .5,
#       size = .5
#     ) +
#     theme_minimal() +
#     labs(x = NULL, y = NULL) +
#     theme(text = element_text(size = 12))
#   
#   # Group demographics by type (age and gender vs. ethnicity)
#   if (
#     any(grepl("Younger", unique(data$variable), fixed = TRUE)) &&
#     as.character(length(unique(data$variable))) == 8
#   ) {
#     plot <- plot +
#       scale_y_discrete(
#         limits = c(
#           "White",
#           "Other ethnic \ngroup",
#           "Mixed or Multiple \nethnic group",
#           "Black, Black British, \nBlack Welsh, \nCaribbean or African",
#           "Asian, Asian \nBritish or \nAsian Welsh",
#           "Older \npeople (65+)",
#           "Working \nage (18-65)",
#           "Younger \npeople (< 18)"
#         )
#       ) +
#       annotate(geom = "text", x = 0.94, y = 8.4, label = "Age", colour = "#717171") +
#       geom_hline(yintercept = 5.5, size = 0.1, linetype = "dotted") +
#       annotate(geom = "text", x = 0.99, y = 5.4, label = "Ethnicity", colour = "#717171")
#   } else if (
#     any(grepl("Younger", unique(data$variable), fixed = TRUE)) &&
#     as.character(length(unique(data$variable))) == 11
#   ) {
#     if (
#       any(grepl("Younger \nfemales (< 20)", unique(data$variable), fixed = TRUE)) # Northern Ireland LTLA demographics
#     ) {
#       plot <- plot +
#         scale_y_discrete(
#           limits = c(
#             "White",
#             "Other ethnic \ngroup",
#             "Mixed or Multiple \nethnic groups",
#             "Black",
#             "Asian",
#             "Older \nmales (65+)",
#             "Working age \nmales (20-64)",
#             "Younger \nmales (< 20)",
#             "Older \nfemales (65+)",
#             "Working age \nfemales (20-64)",
#             "Younger \nfemales (< 20)"
#           )
#         ) +
#         annotate(geom = "text", x = 0.94, y = 11.4, label = "Age & Gender", colour = "#717171") +
#         geom_hline(yintercept = 5.5, size = 0.1, linetype = "dotted") +
#         annotate(geom = "text", x = 0.99, y = 5.4, label = "Ethnicity", colour = "#717171")
#     } else if (
#       any(grepl("Younger \nfemales (< 16)", unique(data$variable), fixed = TRUE)) # Northern Ireland HSCT demographics
#     ) {
#       plot <- plot +
#         scale_y_discrete(
#           limits = c(
#             "White",
#             "Other ethnic \ngroup",
#             "Mixed or Multiple \nethnic groups",
#             "Black",
#             "Asian",
#             "Older \nmales (65+)",
#             "Working age \nmales (16-64)",
#             "Younger \nmales (< 16)",
#             "Older \nfemales (65+)",
#             "Working age \nfemales (16-64)",
#             "Younger \nfemales (< 16)"
#           )
#         ) +
#         annotate(geom = "text", x = 0.94, y = 11.4, label = "Age & Gender", colour = "#717171") +
#         geom_hline(yintercept = 5.5, size = 0.1, linetype = "dotted") +
#         annotate(geom = "text", x = 0.99, y = 5.4, label = "Ethnicity", colour = "#717171")
#     } else {
#       plot <- plot +
#         scale_y_discrete(
#           limits = c(
#             "White",
#             "Other ethnic \ngroup",
#             "Mixed or Multiple \nethnic groups",
#             "Black, Black British, \nBlack Welsh, \nCaribbean or African",
#             "Asian, Asian \nBritish or \nAsian Welsh",
#             "Older \nmales (65+)",
#             "Working age \nmales (18-65)",
#             "Younger \nmales (< 18)",
#             "Older \nfemales (65+)",
#             "Working age \nfemales (18-65)",
#             "Younger \nfemales (< 18)"
#           )
#         ) +
#         annotate(geom = "text", x = 0.94, y = 11.4, label = "Age & Gender", colour = "#717171") +
#         geom_hline(yintercept = 5.5, size = 0.1, linetype = "dotted") +
#         annotate(geom = "text", x = 0.99, y = 5.4, label = "Ethnicity", colour = "#717171")
#     }
#   } else if (
#     any(grepl("Younger", unique(data$variable), fixed = TRUE)) &&
#     as.character(length(unique(data$variable))) == 6
#   ) {
#     plot <- plot +
#       scale_y_discrete(
#         limits = c(
#           "Older \nmales (65+)",
#           "Working age \nmales (18-65)",
#           "Younger \nmales (< 18)",
#           "Older \nfemales (65+)",
#           "Working age \nfemales (18-65)",
#           "Younger \nfemales (< 18)"
#         )
#       ) +
#       annotate(geom = "text", x = 0.94, y = 6.4, label = "Age & Gender", colour = "#717171")
#   }
#   
#   # Set plot annotations to higher/lower if the data is demographics, else set
#   # to better/worse
#   if (any(grepl("Younger", unique(data$variable), fixed = TRUE))) {
#     ggplotly_default(plot, indicator_count) |>
#       add_annotations(
#         x = -0.75,
#         y = indicator_count + 0.6,
#         text = "◄ Lower than mean",
#         showarrow = F
#       ) |>
#       add_annotations(
#         x = 0.75,
#         y = indicator_count + 0.6,
#         text = "Higher than mean ►",
#         showarrow = F
#       )
#   } else {
#     ggplotly_default(plot, indicator_count) |>
#       add_annotations(
#         x = -0.75,
#         y = indicator_count + 0.6,
#         text = "◄ Worse than mean",
#         showarrow = F
#       ) |>
#       add_annotations(
#         x = 0.75,
#         y = indicator_count + 0.6,
#         text = "Better than mean ►",
#         showarrow = F
#       )
#   }
# }

# ---- Show table for selected areas (or just national mean if no areas have been selected) ----
table_selected <- function(data, selected_areas) {
  if (ncol(data) >= 3) {
    # User has selected at least one area; highlight the minimum values for each sub-domain in the selected areas
    formattable(
      data,
      
      # For each of the areas selected by the user (columns 3 and beyond), use a <span> tag to highlight the minimum values in red
      lapply(data[, 3:ncol(data)], function(x) {
        formatter("span",
                  style = x ~ formattable::style(
                    display = "block",
                    padding = "0 4px", 
                    `border-radius` = "4px",
                    color = ifelse(x == min(x, na.rm = TRUE), "white", NA),
                    `background-color` = ifelse(x == min(x, na.rm = TRUE), "#ee2a24", NA)
                  ))
      })
    )
  } else {
    # User hasn't selected any areas; just show the sub-domains and the national mean
    formattable(
      data
    )
  }
}
