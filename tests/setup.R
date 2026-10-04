repo <- normalizePath(Sys.getenv("APP_REPO", unset = "."), mustWork = TRUE)
work <- Sys.getenv("APP_TEST_OUTPUT", unset = tempdir())
dir.create(work, recursive = TRUE, showWarnings = FALSE)
setwd(repo)
app <- new.env(parent = globalenv())
for (expr in parse("app.R")) {
  if (is.call(expr) && as.character(expr[[1]]) %in% c("tryCatch", "shinyApp")) next
  eval(expr, app)
}
for (generic in c("coef", "vcov", "nobs", "residuals", "fitted", "confint"))
  registerS3method(generic, "ch_model", app[[paste0(generic, ".ch_model")]], envir = asNamespace("stats"))
# Fixed numerical comparison specifications, including the original unadjusted
# County specification. Live interface defaults are checked in check_interface.R.
defaults <- list(analysis_level="MSA", year_modern="2010", analysis_type="IV", use_fe=TRUE,
  fe_type="historical", sample_scope="states_only", approach="density", instrument_form="levels", msa_density_measure="average",
  msa_sectors="1", msa_sample_year="1790", msa_iv_year="1840", msa_instrument_type="overlap",
  msa_overlap_pct="5", msa_controls=c("water_1820", "railroads_1840"), msa_schooling_adj="3",
  msa_apply_college_adj=TRUE, msa_college_coeff="0.75", msa_se_spec="cluster_instrument",
  msa_mining_filter_active=FALSE, msa_mining_threshold="0.01", msa_spatial_cutoff="100",
  county_sectors="1", county_sample_year="1790", county_iv_year="1840", county_instrument_type="max_density_overlap",
  county_overlap_threshold="5", county_controls=character(), county_schooling_adj="0",
  county_apply_college_adj=FALSE, county_college_coeff="0.75", county_se_spec="cluster_instrument",
  county_mining_filter_active=FALSE, county_mining_threshold="0.01", county_spatial_cutoff="100",
  county_msa_restriction=FALSE, geo_controls=character(), water_controls=character(), water_year="1820",
  map_size=75, show_map=FALSE)
fixest::setFixest_notes(FALSE)
fixture_loader <- app$make_app_data_loader()
for (level in c("MSA", "County")) {
  app[[if (level == "MSA") "msa_data" else "county_data"]] <- fixture_loader$get_year(level, 2010)
}
app$state_data <- fixture_loader$get_state()
rm(fixture_loader)
# Model fixtures can replace or modify these frames without using the production cache.
app$get_app_year <- function(level, year) {
  data <- app[[if (level == "MSA") "msa_data" else "county_data"]]
  data[!is.na(data$year) & data$year == as.integer(year), , drop = FALSE]
}
stopifnot(nrow(app$state_data) == 48L * 22L, !anyDuplicated(app$state_data[c("statefips", "year")]))
original <- list(MSA = app$msa_data, County = app$county_data)

# Keep the affected geographic identities available when a model check stops.
identity_gaps <- list()
for (level in names(original)) {
  data <- original[[level]]
  id <- if (level == "MSA") "msafips" else "geofips"
  name <- if (level == "MSA") "msaname" else "geoname"
  outcome <- if (level == "MSA") data$ln_output_worker else log(data$gcp_total / data$employment_total)
  jobs <- if (level == "MSA") data$employment else data$employment_total
  area <- if (level == "MSA") data$msaarea else data$area_acre
  for (field in c("modern_state_fe", grep("^hist_state_fe_", names(data), value = TRUE))) {
    value <- trimws(as.character(data[[field]]))
    missing <- is.na(value) | value == "" | value == "0"
    if (!any(missing)) next
    year <- if (field == "modern_state_fe") NA_integer_ else as.integer(sub("^hist_state_fe_([0-9]{4}).*$", "\\1", field))
    suffix <- if (grepl("_s$", field)) "_s" else ""
    population <- function(prefix) {
      if (is.na(year)) return(rep(NA_real_, nrow(data)))
      data[[paste0(prefix, "pop_", year, suffix)]]
    }
    identity_gaps[[length(identity_gaps) + 1L]] <- data.frame(
      level, unit_id = as.character(data[[id]][missing]), unit_name = as.character(data[[name]][missing]),
      field, historical_year = year, modern_year = data$year[missing],
      modern_outcome_available = is.finite(outcome[missing]),
      modern_density_available = is.finite(jobs[missing]) & jobs[missing] > 0 & is.finite(area[missing]) & area[missing] > 0,
      gg_population = population("GG")[missing], aw_population = population("AW")[missing],
      stringsAsFactors = FALSE)
  }
}
if (length(identity_gaps)) write.csv(do.call(rbind, identity_gaps),
  file.path(work, "fixed_effect_identity_gaps.csv"), row.names = FALSE, na = "")
rm(identity_gaps, data, outcome, jobs, area)
