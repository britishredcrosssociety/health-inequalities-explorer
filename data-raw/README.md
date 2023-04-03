## README

- Each `.R` file in `/data-raw` prepares a set of indicators for a single plot
in a single geography (e.g., summary indicators for LTLAs or secondary care
indicators for ICBs)
- Each exported data set should contain the following information:
  - **area_name `<chr>`**: geograpichal area name. These act as the primary key
  across datasets and must match.    
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
  demographics), before being normalised.
