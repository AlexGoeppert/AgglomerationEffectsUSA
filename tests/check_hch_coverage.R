check_hch_coverage_options <- function(app) {
  settings <- list(analysis_level = "State", year_modern = "2010", analysis_type = "IV",
    state_ch_mode = "standard", state_sample_year = "1790", state_iv_year = "1790",
    state_instrument_type = "historical_ch", sample_scope = "states_only", instrument_form = "levels",
    show_map = FALSE, use_fe = FALSE)
  expected_ids <- function(choice, scope) {
    panel <- app$state_data[app$state_data$year==2010 & app$state_data$sample_year==1790 &
      app$state_data$scope==app$historical_region_scope(scope),,drop=FALSE]
    panel <- app$attach_historical_region_instruments(panel,1790,scope,1790)
    suffix <- ""
    population <- panel[[paste0("HCHobs_1790", suffix)]]
    numerator <- panel[[paste0("HCHnumer_1790", suffix)]]
    coverage <- panel[[paste0("HCHcover_1790", suffix)]]
    minimum <- switch(choice, `80` = .80, `95` = .95, none = 0)
    keep <- is.finite(population) & population > 0 & is.finite(numerator) &
      is.finite(numerator / population) & is.finite(coverage) & coverage >= minimum &
      is.finite(panel$LHS) & is.finite(panel$RHS) & panel$ch_complete==1L
    sort(as.character(panel$region_id[keep]))
  }
  shiny::testServer(app$server, {
    do.call(session$setInputs, settings)
    previous <- NULL
    run <- 0L
    for (scope in c("states_only", "states_territories")) for (choice in c("80", "95", "none")) {
      session$setInputs(sample_scope = scope, state_hch_coverage = choice)
      if (!is.null(previous)) stopifnot(identical(isolate(analysis_output())$state_hch_coverage, previous))
      run <- run + 1L
      session$setInputs(run_analysis = run)
      session$flushReact()
      result <- isolate(analysis_output())
      if (!is.null(result$error)) stop(result$error)
      stopifnot(identical(sort(as.character(result$data_for_fs$region_id)), expected_ids(choice, scope)),
        identical(result$state_hch_coverage, choice),
        identical(result$hch_coverage_required, switch(choice, `80` = .80, `95` = .95, none = 0)))
      label <- switch(choice, `80` = "80%", `95` = "95%", none = "No coverage filter")
      stopifnot(grepl(paste0("Known-population area coverage:</strong> ", label), output$analysis_details$html, fixed = TRUE),
        grepl(paste0("Known-population area coverage: ", label), output$results_table$html, fixed = TRUE))
      csv <- read.csv(output$download_coefficients)
      stopifnot(all(csv$historical_ch_coverage == label),
        all(csv$historical_ch_coverage_required == result$hch_coverage_required))
      previous <- choice
    }
    html <- paste(readLines(output$download_results, warn = FALSE), collapse = "\n")
    stopifnot(grepl("<dt>Known-population area coverage</dt><dd>No coverage filter</dd>", html, fixed = TRUE))
    for (type in c("area_population", "county_population")) for (choice in c("none","80","95")) {
      run <- run + 1L
      session$setInputs(analysis_type = "OLS", sample_scope = "states_only", state_instrument_type = type,
        state_hch_coverage = choice, run_analysis = run)
      population_result <- isolate(analysis_output())
      if (!is.null(population_result$error)) stop(population_result$error)
      expected <- switch(choice, none=13L, `80`=12L, `95`=10L)
      inputs <- read.csv(output$download_state_historical_inputs)
      csv <- read.csv(output$download_coefficients)
      stopifnot(population_result$n_obs == expected, nrow(inputs) == 13L, sum(inputs$in_estimation) == expected,
        all(csv$known_area_coverage_required == switch(choice, none=0, `80`=.8, `95`=.95)),
        identical(population_result$state_hch_coverage, choice), !"historical_ch_coverage" %in% names(csv),
        all(inputs$known_area_coverage[inputs$in_estimation] >= csv$known_area_coverage_required[1]),
        grepl("Coverage and exclusions by historical group", output$analysis_details$html,fixed=TRUE))
      if (choice == "80") stopifnot(identical(inputs$region_name[!inputs$in_estimation], "Georgia"),
        grepl("1790: Known-population area 14.4% is below 80%", inputs$exclusion_reason[!inputs$in_estimation], fixed=TRUE))
      if (type == "county_population") stopifnot(abs(inputs$observed_population[inputs$region_name=="North Carolina"] - 388011) < 1e-7)
      run <- run + 1L
      session$setInputs(analysis_type = "IV", run_analysis = run)
      failed <- isolate(analysis_output())
      stopifnot(grepl("No positive-theta",failed$error,fixed=TRUE),
        failed$state_diagnostics$historical_inputs_usable_n == expected,
        identical(failed$stock_wright$status, "available"), is.null(failed$models),
        identical(output$state_fit_unavailable, "yes"), identical(output$historical_input_export, "population"),
        grepl(paste(expected,"groups entered the estimator"),output$results_table$html,fixed=TRUE))
      diagnostic <- read.csv(output$download_state_historical_inputs)
      stopifnot(sum(diagnostic$assessed_for_fit)==expected, all(is.na(diagnostic$in_estimation)),
        grepl("Fit assessment",output$analysis_details$html,fixed=TRUE))
    }
  })
  # The same regional selection precedes both county-adjusted routes.
  for (mode in c("human_capital", "geography")) {
    adjusted <- app$state_ch_result(modifyList(settings, list(analysis_type = "OLS",
      state_ch_mode = "county_adjusted", state_adjustment_mode = mode, state_hch_coverage = "none",
      state_geo_controls = "terrain", state_water_controls = character(),
      state_transport_controls = character(), state_water_year = "1820")), app$state_data)
    stopifnot(identical(sort(as.character(adjusted$data_for_fs$region_id)), expected_ids("none", "states_only")),
      adjusted$state_hch_coverage == "none", adjusted$hch_coverage_required == 0,
      all(adjusted$models[[1]]$region_ids %in% as.character(expected_ids("none", "states_only"))))
  }
  cat("Historical CH coverage options passed: 80/95/no-filter samples, stored configuration, exports, and county-adjusted routes.\n")
}
