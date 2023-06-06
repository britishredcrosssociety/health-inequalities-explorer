#' Currently, `grid_card()` can only be collapsed using the arrow icon.
#' This function also allows cards to be collapsed by clicking their title.
title_collapsible <- function(title) {
  span(
    "onclick" = '
    var card = this.parentElement.parentElement;
    var card_classes = card.classList;
    if (card_classes.contains("collapsed")) {
      card_classes.remove("collapsed");
    } else {
      card_classes.add("collapsed");
    }
    window.dispatchEvent(new Event("resize"));
    ',
    title
  )
}

#' Wrap strings in the jitter plot
string_wrap_unvectorized <- function(string) {
  paste(strwrap(string, width = 20), collapse = "\n")
}

string_wrap <- Vectorize(string_wrap_unvectorized, USE.NAMES = FALSE)
