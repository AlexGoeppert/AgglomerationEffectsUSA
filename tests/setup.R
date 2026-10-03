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
for (level in c("MSA", "County")) {
  filename <- if (level == "MSA") "MSA_analysis_data.dta" else "master_county_build.dta"
  dataset <- app$read_app_data(file.path(repo, filename), level)
  app[[if (level == "MSA") "msa_data" else "county_data"]] <- dataset[dataset$year %in% 2010, ]
  rm(dataset)
  gc()
}
app$state_data <- haven::read_dta(file.path(repo, "State_CH_analysis_data.dta"))
stopifnot(nrow(app$state_data) == 48L * 22L, !anyDuplicated(app$state_data[c("statefips", "year")]))
original <- list(MSA = app$msa_data, County = app$county_data)
