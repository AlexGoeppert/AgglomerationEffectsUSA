check_hch_coverage_options <- function(app) {
  settings <- list(analysis_level = "State", year_modern = "2010", analysis_type = "IV",
    state_ch_mode = "standard", state_sample_year = "1790", state_iv_year = "1790",
    state_instrument_type = "historical_ch", sample_scope = "states_only", instrument_form = "levels",
    show_map = FALSE, use_fe = FALSE)
  panel <- app$state_data[app$state_data$year == 2010, , drop = FALSE]
  expected_ids <- function(choice, scope) {
    suffix <- if (scope == "states_only") "_s" else ""
    population <- panel[[paste0("HCHobs_1790", suffix)]]
    numerator <- panel[[paste0("HCHnumer_1790", suffix)]]
    coverage <- panel[[paste0("HCHcover_1790", suffix)]]
    minimum <- switch(choice, `80` = .80, `95` = .95, none = 0)
    keep <- is.finite(population) & population > 0 & is.finite(numerator) &
      is.finite(numerator / population) & is.finite(coverage) & coverage >= minimum &
      is.finite(panel$LHS) & is.finite(panel$RHS)
    sort(as.integer(panel$statefips[keep]))
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
      stopifnot(identical(sort(as.integer(result$data_for_fs$statefips)), expected_ids(choice, scope)),
        identical(result$state_hch_coverage, choice),
        identical(result$hch_coverage_required, switch(choice, `80` = .80, `95` = .95, none = 0)))
      label <- switch(choice, `80` = "80%", `95` = "95%", none = "No coverage filter")
      stopifnot(grepl(paste0("Historical CH coverage:</strong> ", label), output$analysis_details$html, fixed = TRUE),
        grepl(paste0("Historical CH coverage: ", label), output$results_table$html, fixed = TRUE))
      csv <- read.csv(output$download_coefficients)
      stopifnot(all(csv$historical_ch_coverage == label),
        all(csv$historical_ch_coverage_required == result$hch_coverage_required))
      previous <- choice
    }
    html <- paste(readLines(output$download_results, warn = FALSE), collapse = "\n")
    stopifnot(grepl("<dt>Historical CH coverage</dt><dd>No coverage filter</dd>", html, fixed = TRUE))
    session$setInputs(state_instrument_type = "area_population", state_hch_coverage = "unused", run_analysis = run + 1L)
    population_result <- isolate(analysis_output())
    if (!is.null(population_result$error)) stop(population_result$error)
    stopifnot(is.null(population_result$state_hch_coverage),
      !"historical_ch_coverage" %in% names(read.csv(output$download_coefficients)))
  })
  # The same regional selection precedes both county-adjusted routes.
  for (mode in c("human_capital", "geography")) {
    adjusted <- app$state_ch_result(modifyList(settings, list(analysis_type = "OLS",
      state_ch_mode = "county_adjusted", state_adjustment_mode = mode, state_hch_coverage = "none",
      state_geo_controls = "terrain", state_water_controls = character(),
      state_transport_controls = character(), state_water_year = "1820")), app$state_data)
    stopifnot(identical(sort(as.integer(adjusted$data_for_fs$statefips)), expected_ids("none", "states_only")),
      adjusted$state_hch_coverage == "none", adjusted$hch_coverage_required == 0,
      all(adjusted$models[[1]]$region_ids %in% as.character(expected_ids("none", "states_only"))))
  }
  cat("Historical CH coverage options passed: 80/95/no-filter samples, stored configuration, exports, and county-adjusted routes.\n")
}
