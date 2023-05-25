## README

- With the excpetion of `-shp` files, each `.R` file in `/data-raw` prepares a
  set of indicators for a single plot in a single geography (e.g., summary
  indicators for LTLAs or secondary careindicators for ICBs). Each exported data
  set should contain the following information:
  - **area_name `<chr>`**: geograpichal area name. These act as the primary key
    across datasets and must match 
  - **data_type `<chr>`**: the type of data set, one of "Summary metrics", 
    "Secondary care" or "Demographics"
  - **variable `<chr>`**: the name of the variable as will be printed on the y-axis
    of each plot 
  - **number `<dbl>`**: the absolute number or count of the variable being 
    measured (e.g., a score or rank)      
  - **percent `<dbl>`**: a relative statistics (i.e, a percentage), can be `NA`
  if unavailable 
  - **scaled_1_1 `<dbl>`**: normalised values scaled from -1 to 1, to be 
    plotted (taken from the number column). It is important that indicators are 
    polarised in the same direction (by multiplying those that aren't by -1) so
    that higher scores equal better health (or a higher score in the case of 
    demographics), before being normalised
- `census-raw/` contains 2021 census data from:
  https://www.nomisweb.co.uk/sources/census_2021_ts used in `-demographics`
  scripts. The NOMIS API has been unreliable so deferring to manual downloads.
- `-shp` files export the boundary data for use in the maps and the area
  selection boxes. Each exported `-shp` data set should contains the following
  information:
  - **area_name `<chr>`**: geograpichal area name. These act as the primary key
    across datasets and must match
  - **area_code `<chr>`**: geographical area codes. These are used as layer ID's
    in the maps
  - **geometry `<MULTIPOLYGON [°]>`**: geographical boundaries. These should be
    as simplified as possible to reduce load times. Use `rmapshaper::ms_simplify()`