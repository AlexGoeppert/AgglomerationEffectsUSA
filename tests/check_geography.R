# Area and employment must use the same member counties for each MSA.
area <- haven::read_dta("MSA_analysis_data.dta", col_select = tidyselect::all_of(
  c("msafips", "year", "msaarea", "ch_land_acres", "ch_units_expected")))
known <- is.finite(area$ch_land_acres) & area$ch_units_expected > 0
expected <- area$ch_land_acres[known] * 4046.8
relative_error <- abs(area$msaarea[known] - expected) / pmax(1, abs(expected))
if (!all(is.finite(relative_error)) || any(relative_error > 1e-7))
  stop("MSA area does not match the land area of the CH member counties. Rebuild the MSA data before deployment.")
if (!any(known)) stop("MSA member-county areas are unavailable.")
cat("MSA land areas agree with the CH county membership.\n")
