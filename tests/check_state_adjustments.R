# Integration checks use the production loader, retaining one county year at a time.
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
loader <- app$make_app_data_loader()
app$get_app_year <- loader$get_year
app$state_data <- loader$get_state()
fixest::setFixest_notes(FALSE)
settings <- list(analysis_level = "State", year_modern = "2010", analysis_type = "IV",
  state_ch_mode = "county_adjusted", state_adjustment_type = "geography",
  state_sample_year = "1900", state_iv_year = "1900", state_instrument_type = "area_population",
  state_county_instrument = "area_population", sample_scope = "states_territories",
  instrument_form = "levels", state_county_area = TRUE,
  state_geo_controls = c("terrain", "crop", "climate"),
  state_water_controls = c("shoreline", "historical_access", "distance", "portage"),
  state_transport_controls = c("water_1820", "railroads_1840"), state_water_year = "1820",
  state_schooling_efficiency = TRUE, state_college_externality = TRUE, state_college_coefficient = .75,
  approach = "density", msa_density_measure = "average", msa_instrument_type = "overlap",
  county_instrument_type = "max_density_overlap", use_fe = FALSE, show_map = FALSE)

checks <- 0L
records <- list()
check_result <- function(input, result) {
  model <- result$models[[1L]]
  stopifnot(inherits(model, "ch_model"), all(is.finite(coef(model))),
    all(is.finite(vcov(model))), nobs(model) >= 3L, nobs(model) <= 48L,
    result$state_ch_mode == "county_adjusted", result$se_spec == "cluster_state",
    result$fe_type == "No", is.null(model$anderson_rubin),
    isTRUE(all.equal(unname(model$full_covariance), unname(crossprod(model$state_influence)), tolerance = 1e-10)))
  if (input$state_adjustment_type != "human_capital") {
    calibration <- model$county_calibration
    stopifnot(calibration$calibrated, calibration$nobs > nobs(model),
      calibration$county_fe, identical(calibration$method, input$analysis_type),
      max(abs(model$calibration_state_influence)) > 0,
      all(names(calibration$gamma) %in% result$county_geo_vars),
      !any(grepl("^\\.fe_", names(calibration$gamma))),
      !"ln_area" %in% names(calibration$gamma))
  } else stopifnot(!model$county_calibration$calibrated,
    model$county_calibration$nobs == 0L, all(model$calibration_state_influence == 0))
  if (input$analysis_type == "IV") {
    stopifnot(model$stock_wright$status == "available", model$instrument_relevance$status == "available",
      is.finite(model$stock_wright$p_value), abs(model$iv_moment) < 1e-7,
      model$instrument_relevance$conditional_on_calibration,
      model$stock_wright$null_elasticity == 0)
    if (input$state_adjustment_type != "geography") stopifnot(any(abs(model$null_index_value) > 1e-7))
  } else stopifnot(is.null(model$stock_wright), is.null(model$instrument_relevance))
  checks <<- checks + 1L
  records[[length(records) + 1L]] <<- data.frame(modern_year = input$year_modern,
    historical_year = input$state_iv_year, state_instrument = input$state_instrument_type,
    county_instrument = input$state_county_instrument, mode = input$state_adjustment_type,
    method = input$analysis_type, n = nobs(model), county_n = model$county_calibration$nobs,
    elasticity = coef(model)[["ch_elasticity"]], se = sqrt(vcov(model)["ch_elasticity", "ch_elasticity"]),
    S = if (input$analysis_type == "IV") model$stock_wright$statistic else NA_real_,
    instrument_F = if (input$analysis_type == "IV") model$instrument_relevance$statistic else NA_real_)
  cat("Checked", input$year_modern, input$state_iv_year, input$state_adjustment_type,
      input$analysis_type, input$state_instrument_type, "N =", nobs(model), "\n")
  invisible(model)
}

# The standard path must be unchanged by the addition of the new mode.
standard_input <- modifyList(settings, list(state_ch_mode = "standard"))
standard_before <- app$state_ch_result(standard_input, app$state_data)$models[[1L]]
baseline <- NULL
for (year in c("2010", "2020")) for (mode in c("geography", "human_capital", "both"))
  for (method in c("IV", "OLS")) {
    input <- modifyList(settings, list(year_modern = year, state_adjustment_type = mode, analysis_type = method))
    result <- app$state_ch_result(input, app$state_data)
    check_result(input, result)
    if (year == "2010" && mode == "both" && method == "IV") baseline <- result
  }
standard_after <- app$state_ch_result(standard_input, app$state_data)$models[[1L]]
stopifnot(identical(standard_before$coefficients, standard_after$coefficients),
  identical(standard_before$covariance, standard_after$covariance))

for (instrument in c("county_population", "historical_ch")) {
  input <- modifyList(settings, list(state_instrument_type = instrument,
    state_county_instrument = "county_population", state_adjustment_type = "both"))
  check_result(input, app$state_ch_result(input, app$state_data))
}
input <- modifyList(settings, list(state_county_area = FALSE))
without_area <- app$state_ch_result(input, app$state_data)
check_result(input, without_area)
stopifnot(!"ln_area" %in% without_area$models[[1L]]$county_calibration$nuisance)
states_only_input <- modifyList(settings, list(sample_scope = "states_only", state_adjustment_type = "both"))
states_only_result <- app$state_ch_result(states_only_input, app$state_data)
check_result(states_only_input, states_only_result)
stopifnot(all(states_only_result$data_for_fs$AWvalid_1900_s == 1))

# Early samples may have no interior IV root. They must return the adjusted S
# diagnostic with an explicit estimation error, never a silent substitute fit.
early_input <- modifyList(settings, list(state_sample_year = "1790", state_iv_year = "1790",
  state_geo_controls = "terrain", state_water_controls = character(), state_transport_controls = character()))
early <- tryCatch(app$state_ch_result(early_input, app$state_data), error = identity)
if (inherits(early, "error")) {
  stopifnot(inherits(early, "ch_fit_error"), !is.null(early$stock_wright),
    early$stock_wright$status == "available")
} else check_result(early_input, early)

# The fixed human-capital mode must not request county GDP or population data.
original_county_loader <- app$get_app_year
app$get_app_year <- function(...) stop("County data must not be loaded for fixed human-capital coefficients.")
hc_input <- modifyList(settings, list(state_adjustment_type = "human_capital"))
check_result(hc_input, app$state_ch_result(hc_input, app$state_data))
app$get_app_year <- original_county_loader

# Deliberately missing one active shifter removes the entire affected state and
# removes that state's counties from the linked calibration.
original_static_loader <- app$get_state_adjustment_inputs
static <- as.data.frame(original_static_loader())
selected_state <- baseline$models[[1L]]$state_ids[1L]
row <- which(static$statefips == as.numeric(selected_state))[1L]
changed <- static
changed$rugged_mean[row] <- NA_real_
app$get_state_adjustment_inputs <- function(...) changed
missing <- app$state_ch_result(modifyList(settings, list(state_adjustment_type = "both")), app$state_data)
stopifnot(nobs(missing$models[[1L]]) == nobs(baseline$models[[1L]]) - 1L,
  !selected_state %in% missing$models[[1L]]$state_ids,
  !selected_state %in% missing$models[[1L]]$county_calibration$states)
app$get_state_adjustment_inputs <- original_static_loader

# The rendered controls, regression output and exports all follow the new mode.
ui <- htmltools::renderTags(app$ui)$html
stopifnot(grepl('id="state_ch_mode"', ui, fixed = TRUE),
  grepl('value="county_adjusted"', ui, fixed = TRUE),
  grepl('value="human_capital"', ui, fixed = TRUE),
  grepl('download_county_calibration', ui, fixed = TRUE))
shiny::testServer(app$server, {
  do.call(session$setInputs, c(modifyList(settings, list(state_adjustment_type = "both")), list(run_analysis = 1L)))
  session$flushReact()
  result <- isolate(analysis_output())
  if (!is.null(result$error)) stop(result$error)
  check_result(modifyList(settings, list(state_adjustment_type = "both")), result)
  html <- output$results_table$html
  stopifnot(grepl("Stock–Wright LM S", html, fixed = TRUE),
    grepl("Instrument Wald F", html, fixed = TRUE),
    grepl("County-adjusted CH", html, fixed = TRUE),
    grepl("conditional", html, ignore.case = TRUE),
    !grepl("Anderson.{0,12}Rubin", html, ignore.case = TRUE))
  csv <- read.csv(output$download_coefficients)
  stopifnot(all(csv$state_ch_specification == "county_adjusted"),
    all(csv$county_adjustment == "both"), all(csv$schooling_enters_ch_power),
    all(csv$county_calibration_n == result$county_calibration$nobs),
    all(csv$geography_estimation_uncertainty == "Joint county-state covariance"))
  calibration <- read.csv(output$download_county_calibration)
  stopifnot("county_elasticity" %in% calibration$term,
    !any(calibration$enters_state_index[calibration$term %in% c("county_elasticity", "ln_area")]),
    all(calibration$enters_state_index[calibration$term %in% result$county_geo_vars]),
    all(calibration$county_state_fixed_effects))
  saved_html <- paste(readLines(output$download_results, warn = FALSE), collapse = "\n")
  stopifnot(grepl("County-adjusted state CH", saved_html, fixed = TRUE),
    grepl("Stock–Wright LM S", saved_html, fixed = TRUE))
  session$setInputs(state_adjustment_type = "human_capital", run_analysis = 2L)
  session$flushReact()
  hc_result <- isolate(analysis_output())
  if (!is.null(hc_result$error)) stop(hc_result$error)
  hc_csv <- read.csv(output$download_coefficients)
  stopifnot(all(hc_csv$county_calibration_n == 0L), all(is.na(hc_csv$county_calibration_method)),
    all(is.na(hc_csv$county_calibration_instrument)), all(is.na(hc_csv$county_calibration_scale)),
    all(hc_csv$geography_estimation_uncertainty == "No estimated geography coefficients"),
    !grepl("County calibration observations", output$analysis_details$html, fixed = TRUE))
  session$setInputs(state_adjustment_type = "both", analysis_type = "OLS", run_analysis = 3L)
  session$flushReact()
  ols_result <- isolate(analysis_output())
  if (!is.null(ols_result$error)) stop(ols_result$error)
  ols_csv <- read.csv(output$download_coefficients)
  stopifnot(all(ols_csv$county_calibration_method == "OLS"),
    all(is.na(ols_csv$county_calibration_instrument)), all(is.na(ols_csv$county_calibration_scale)),
    !grepl("County calibration instrument:", output$analysis_details$html, fixed = TRUE),
    !grepl('<tr[^>]*><td[^>]*>Instrument Wald F</td>', output$results_table$html, perl = TRUE))
})
write.csv(do.call(rbind, records), file.path(work, "state_adjustment_checks.csv"), row.names = FALSE)
cat("County-adjusted state CH integration checks passed:", checks, "estimated specifications.\n")
