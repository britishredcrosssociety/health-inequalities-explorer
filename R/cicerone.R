# https://github.com/JohnCoene/cicerone/issues/27

guide <- Cicerone$
  new(allow_close = FALSE)$
  step(
  el = "card_header",
  title = " ",
  description =
    "This short guided tour will highlight the key functionality of the the
    Health Inequalities Explorer, an interactive tool to compare and explore
    health statistics and demographics across different geographical areas."
)$step(
  el = "card_select_geography",
  title = " ",
  description =
    "First, start by selecting the type of geography you are interested in. You
    can choose between Local Authorities, Integrated Care Systems, and NHS
    Trusts."
)$step(
  el = "card_select_areas",
  title = " ",
  description =
    "Then, use the search box to select up to three areas to compare."
)$step(
  el = "card_map",
  title = " ",
  description =
    "You can also select/deselect areas by clicking the map."
)$step(
  el = "card_summary_intro",
  title = " ",
  description =
    "You will then be presented with statistical indicators, grouped by section
    (here, summary statistics). Each section has a short narrative describing
    the indicators, with links to further resources, followed by an interactive
    plot."
)$step(
  el = "card_summary_metrics",
  title = " ",
  description =
    "The interactive plots compare each indicator by the national mean. All the
    indicators are normalised in a range of -1 to 1 to standardise the different
    units used and allow them to be compared side-by-side.
    </br>
    </br>
    Click and drag the cursor around a set of points to zoom in to the plot.
    Double-click the mouse in the plot area to return back to the original plot.
    </br>
    </br>
    Hover over points to bring up an interactive pop-up with more information."
)$step(
  el = "card_footer",
  title = " ",
  description =
    "Additional information and a button to bookmark the current state of the
    app can be found here.
    </br>
    </br>
    Please note, This is a new tool under development and currently has limited
    functionality. Other geographical areas, nations, and datasets will be added
    shortly. Please provide feedback or bugs to mpage@redcross.org.uk"
)
