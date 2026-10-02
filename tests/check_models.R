source("tests/setup.R")
run_checked <- function(settings, exports = FALSE) {
  result <- NULL
  shiny::testServer(app$server, {
    do.call(session$setInputs, c(modifyList(defaults, settings), list(run_analysis = 1)))
    session$flushReact()
    result <<- isolate(analysis_output())
    if (!is.null(result$error)) stop(result$error)
    stopifnot(length(result$models) > 0L, length(result$model_warnings) == length(unique(result$model_warnings)))
    for (name in names(result$models)) {
      model <- result$models[[name]]
      stopifnot(nobs(model) > 0L, all(is.finite(coef(model))), all(is.finite(vcov(model))))
      if (!inherits(model, "ch_model") && result$analysis_type == "IV") {
        stopifnot(nobs(model) == nobs(result$first_stage_models[[name]]))
        dep <- if (name == "Same Return Adj.") "LHS_adj1" else if (name == "Specific Return Adj.") "LHS_adj2" else "LHS"
        controls <- if (length(result$controls_vec)) paste("+", paste(result$controls_vec, collapse = " + ")) else ""
        formula <- as.formula(glue::glue("{dep} ~ 1 {controls} | {result$fe_part} | RHS ~ instrument"))
        independent <- fixest::feols(formula, data = result$data_for_fs, vcov = result$vcov_arg)
        stopifnot(isTRUE(all.equal(coef(model), coef(independent), tolerance = 1e-10)),
          isTRUE(all.equal(vcov(model), vcov(independent), tolerance = 1e-10)))
      }
    }
    stopifnot(grepl("Observations", output$results_table$html))
    table <- app$model_coefficients(result)
    stopifnot(nrow(table) > 0L, all(is.finite(table$estimate)))
    stopifnot(inherits(app$result_plot(result), "ggplot"))
    if (exports) {
      csv <- read.csv(output$download_coefficients)
      stopifnot(all(csv$geography == result$analysis_level), all(csv$instrument_year == result$iv_year),
        all(csv$instrument_construction == app$instrument_name(result$instrument_type, result$approach)))
      html <- paste(readLines(output$download_results, warn = FALSE), collapse = "\n")
      stopifnot(grepl('<h2>Geographic controls</h2>', html, fixed = TRUE),
        grepl(app$instrument_name(result$instrument_type, result$approach), html, fixed = TRUE))
    }
  })
  cat("Passed", settings$analysis_level, settings$analysis_type, settings$msa_iv_year,
    settings$county_iv_year, settings$msa_instrument_type, settings$county_instrument_type, "\n")
  result
}

for (level in c("MSA", "County")) for (year in c(1790, 1870, 1880, 1890, 1900)) {
  prefix <- tolower(level)
  methods <- if (level == "MSA") c("overlap", "county_population", "area_population") else
    c("max_density_overlap", "weighted_density_overlap", "county_population", "area_population")
  scopes <- if (year == 1790) "states_only" else c("states_only", "states_territories")
  approaches <- if (year == 1790) "density" else c("density", "employment")
  for (method in methods) for (scope in scopes) for (approach in approaches) {
    if (approach == "employment" && method == "weighted_density_overlap") next
    settings <- list(analysis_level = level, analysis_type = "IV", sample_scope = scope, approach = approach)
    settings[[paste0(prefix, "_sample_year")]] <- as.character(year)
    settings[[paste0(prefix, "_iv_year")]] <- as.character(year)
    settings[[paste0(prefix, "_instrument_type")]] <- method
    run_checked(settings, exports = year == 1900 && scope == "states_only" && approach == "density" && method == "area_population")
  }
}
for (scope in c("states_only", "states_territories")) for (year in c(1870, 1880, 1890, 1900)) {
  for (type in c("county_population", "area_population")) {
    run_checked(list(analysis_level = "MSA", analysis_type = "IV", msa_density_measure = "CH",
      msa_sample_year = "1900", msa_iv_year = as.character(year), msa_instrument_type = type,
      sample_scope = scope))
  }
}
for (level in c("MSA", "County")) for (method in c("OLS", "First-stage Regression")) {
  settings <- list(analysis_level = level, analysis_type = method)
  settings[[paste0(tolower(level), "_sample_year")]] <- "1900"
  settings[[paste0(tolower(level), "_iv_year")]] <- "1900"
  settings[[paste0(tolower(level), "_instrument_type")]] <- "area_population"
  run_checked(settings)
}
cat("All model checks passed.\n")
