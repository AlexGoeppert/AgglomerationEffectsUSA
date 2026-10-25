# Check matching controls and both fixed-instrument routes through the production app.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
source("tests/setup.R", encoding = "UTF-8")
base <- modifyList(defaults, list(analysis_type = "IV", use_fe = FALSE,
  sample_scope = "states_territories", msa_density_measure = "CH", msa_ch_mode = "standard",
  msa_sample_year = "1900", msa_iv_year = "1900", msa_instrument_type = "historical_ch",
  msa_schooling_adj = "0", msa_apply_college_adj = FALSE, msa_se_spec = "robust", msa_controls = character(),
  msa_hch_coverage = "none", msa_hch_variant = "historical", msa_hch_density_basis = "modern",
  msa_hch_method = "derivative", msa_hch_gmm_steps = "1", state_ch_mode = "standard",
  state_sample_year = "1900", state_iv_year = "1900", state_instrument_type = "historical_ch",
  state_hch_coverage = "none", state_hch_variant = "historical", state_hch_density_basis = "modern",
  state_hch_method = "derivative", state_hch_gmm_steps = "1", show_map = FALSE))

choice <- function(level, method, steps = "1") {
  x <- base
  x$analysis_level <- level
  x[[paste0(tolower(level), "_hch_method")]] <- method
  x[[paste0(tolower(level), "_hch_gmm_steps")]] <- steps
  x
}

results <- list()
shiny::testServer(app$server, {
  run <- 0L
  for (level in c("State", "MSA")) for (method in c("derivative", "nls")) for (steps in c("1", "2")) {
    settings <- choice(level, method, steps)
    do.call(session$setInputs, settings)
    run <- run + 1L
    session$setInputs(run_analysis = run)
    session$flushReact()
    result <- isolate(analysis_output())
    if (!is.null(result$error)) stop(result$error)
    model <- result$models[[1]]
    stopifnot(result$analysis_level == level, result$instrument_type == "historical_ch",
      result$state_hch_method == method, model$instrument_method == method,
      model$gmm_steps == as.integer(steps), is.finite(model$theta),
      length(model$historical_instrument) == nrow(result$data_for_fs))
    if (method == "nls") {
      stopifnot(is.finite(model$pilot_theta), all(app$app_model_rows(model) == model$pilot_rows),
        !grepl("Stock–Wright LM S", output$results_table$html, fixed = TRUE),
        grepl("Conditional on the NLS", output$analysis_details$html, fixed = TRUE))
    } else stopifnot(is.na(model$pilot_theta))
    coefficients <- read.csv(output$download_coefficients)
    diagnostics <- read.csv(output$download_hch_diagnostics)
    stopifnot(all(coefficients$historical_ch_method == method), all(coefficients$gmm_steps == as.integer(steps)),
      tolower(level) %in% diagnostics$record_type)
    if (method == "nls") stopifnot("fitted_instrument" %in% diagnostics$record_type)
    results[[paste(level, method, steps)]] <<- c(theta = model$theta, pilot = model$pilot_theta, n = model$nobs)
  }
  for (level in c("State", "MSA")) for (method in c("derivative", "nls"))
    stopifnot(isTRUE(all.equal(results[[paste(level, method, "1")]], results[[paste(level, method, "2")]], tolerance = 1e-10)))

  # Options are stored with results, even when the user changes controls afterwards.
  session$setInputs(msa_hch_method = "derivative", msa_hch_gmm_steps = "1")
  stopifnot(isolate(analysis_output())$state_hch_method == "nls")
  stopifnot(all(read.csv(output$download_coefficients)$historical_ch_method == "nls"))
  stopifnot(is.null(isolate(map_output())$src))

  # Both county-adjusted routes preserve the selected aggregate instrument.
  for (level in c("State", "MSA")) for (adjustment in c("human_capital", "geography")) {
    settings <- choice(level, "nls")
    prefix <- tolower(level)
    settings[[paste0(prefix, "_ch_mode")]] <- "county_adjusted"
    settings[[paste0(prefix, "_adjustment_mode")]] <- adjustment
    settings[[paste0(prefix, "_geo_controls")]] <- "terrain"
    settings[[paste0(prefix, "_water_controls")]] <- character()
    settings[[paste0(prefix, "_transport_controls")]] <- character()
    settings[[paste0(prefix, "_county_instrument")]] <- "area_population"
    settings[[paste0(prefix, "_county_area")]] <- FALSE
    if (level == "MSA") {
      settings$use_fe <- TRUE
      settings$fe_type <- "modern"
      settings$msa_se_spec <- "cluster_state"
    }
    do.call(session$setInputs, settings)
    run <- run + 1L
    session$setInputs(run_analysis = run)
    session$flushReact()
    result <- isolate(analysis_output())
    if (!is.null(result$error)) stop(result$error)
    model <- result$models[[1]]
    stopifnot(result$state_ch_mode == "county_adjusted", model$instrument_method == "nls",
      is.finite(model$pilot_theta), all(app$app_model_rows(model) == model$pilot_rows),
      !grepl("Stock–Wright LM S", output$results_table$html, fixed = TRUE))
    results[[paste(level, adjustment)]] <<- c(theta = model$theta, pilot = model$pilot_theta, n = model$nobs)
  }

  # The historical basis uses original source pieces in the nonlinear instrument.
  for (level in c("State", "MSA")) {
    settings <- choice(level, "nls")
    settings[[paste0(tolower(level), "_hch_density_basis")]] <- "source"
    do.call(session$setInputs, settings)
    run <- run + 1L
    session$setInputs(run_analysis = run)
    session$flushReact()
    result <- isolate(analysis_output())
    if (!is.null(result$error)) stop(result$error)
    model <- result$models[[1]]
    stopifnot(is.finite(model$pilot_theta), is.finite(model$theta), model$instrument_method == "nls")
    results[[paste(level, "source")]] <<- c(theta = model$theta, pilot = model$pilot_theta, n = model$nobs)
  }

  # National prediction coefficients agree across levels, including Washington's county input.
  predicted <- list()
  for (level in c("State", "MSA")) {
    settings <- choice(level, "nls")
    prefix <- tolower(level)
    settings[[paste0(prefix, "_hch_variant")]] <- "ab"
    settings[[paste0(prefix, "_hch_coverage")]] <- "80"
    settings[[paste0(prefix, "_hch_prediction_geo_controls")]] <- c("terrain", "crop", "climate")
    settings[[paste0(prefix, "_hch_prediction_water_controls")]] <- c("shoreline", "historical_access", "distance", "portage")
    settings[[paste0(prefix, "_hch_prediction_transport_controls")]] <- c("water_1820", "railroads_1840")
    settings[[paste0(prefix, "_hch_prediction_water_year")]] <- "1820"
    do.call(session$setInputs, settings)
    run <- run + 1L
    session$setInputs(run_analysis = run)
    session$flushReact()
    result <- isolate(analysis_output())
    if (!is.null(result$error)) stop(result$error)
    prediction <- result$hch_prediction_diagnostics[["1900"]]
    predicted[[level]] <- prediction$diagnostics$coefficients
    stopifnot(length(result$hch_prediction_geo_vars) == 22L)
    if (level == "MSA") {
      counties <- prediction$counties
      stopifnot(any(counties$geofips == 11001), !any(counties$training[counties$geofips == 11001]),
        all(counties$support[counties$geofips == 11001]))
      diagnostics <- read.csv(output$download_hch_diagnostics)
      stopifnot("msafips" %in% names(diagnostics), !"statefips" %in% names(diagnostics))
    }
    model <- result$models[[1]]
    results[[paste(level, "predicted")]] <<- c(theta = model$theta, pilot = model$pilot_theta, n = model$nobs)
  }
  stopifnot(isTRUE(all.equal(predicted$MSA, predicted$State, tolerance = 1e-12)))
})
print(do.call(rbind, results))
cat("MSA and State CH interface checks passed: derivative/NLS instruments, GMM weights, pilot samples, exports and stored choices.\n")
