# ---- Prepare selected data ----
table_prep <- function(data, selected_areas) {
  # If selected_areas is NULL then this will just return "variable" and "National Mean"
  data <- data[, c("variable", "National Mean", selected_areas)]
  
  data[, c("National Mean", selected_areas)] <- round(data[, c("National Mean", selected_areas)], 0)
  
  return(data)
}

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