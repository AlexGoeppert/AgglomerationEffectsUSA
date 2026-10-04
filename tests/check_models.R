source("tests/setup.R")
assert_ch_diagnostics_hidden <- function(html = NULL, csv = NULL) {
  if (!is.null(html)) stopifnot(!grepl("Anderson.{0,12}Rubin|Local instrument|Local relevance", html, ignore.case = TRUE))
  if (!is.null(csv)) stopifnot(!any(grepl("^(anderson_rubin_|ch_local_)", names(csv))))
}
assert_ch_diagnostics_hidden(htmltools::renderTags(app$ui)$html)
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
      if (is.finite(result$schooling_reference[[name]])) {
        used <- result$data_for_fs[app$app_model_rows(model), , drop = FALSE]
        stopifnot(abs(result$schooling_reference[[name]] - mean(used$avg_schooling)) < 1e-10)
      }
      if (inherits(model, "ch_model") && result$analysis_type == "IV") {
        stopifnot(model$stock_wright$status == "available", model$stock_wright$nobs == nobs(model),
          grepl("Stock–Wright LM S", output$results_table$html, fixed = TRUE),
          grepl("Stock–Wright p-value", output$results_table$html, fixed = TRUE),
          !grepl("Wald p-value", output$results_table$html, fixed = TRUE),
          model$instrument_relevance$status == "available",
          model$anderson_rubin$status == "available", model$anderson_rubin$nobs == nobs(model),
          model$anderson_rubin$null_elasticity == 0, model$anderson_rubin$df == 1L,
          model$instrument_relevance$nobs == nobs(model),
          model$instrument_relevance$evaluated_theta == model$theta)
        assert_ch_diagnostics_hidden(output$results_table$html)
      } else stopifnot(is.null(model$stock_wright), is.null(model$anderson_rubin), is.null(model$instrument_relevance))
      if (inherits(model, "ch_model"))
        stopifnot(!grepl("Instrument Wald F", output$results_table$html, fixed = TRUE))
      if (!inherits(model, "ch_model") && result$analysis_type == "IV") {
        stopifnot(nobs(model) == nobs(result$first_stage_models[[name]]))
        dep <- if (name == "Same Return Adj.") "LHS_adj1" else if (name == "Specific Return Adj.") "LHS_adj2" else "LHS"
        controls <- if (length(result$controls_vec)) paste("+", paste(result$controls_vec, collapse = " + ")) else ""
        formula <- as.formula(glue::glue("{dep} ~ 1 {controls} | {result$fe_part} | RHS ~ instrument"))
        independent <- fixest::feols(formula, data = result$data_for_fs, vcov = result$vcov_arg, fixef.rm = "singletons")
        stopifnot(isTRUE(all.equal(coef(model), coef(independent), tolerance = 1e-10)),
          isTRUE(all.equal(vcov(model), vcov(independent), tolerance = 1e-10)))
      }
    }
    stopifnot(grepl("Observations", output$results_table$html))
    table <- app$model_coefficients(result)
    stopifnot(nrow(table) > 0L, all(is.finite(table$estimate)))
    assert_ch_diagnostics_hidden(csv = table)
    if (!inherits(result$models[[1]], "ch_model") && result$analysis_type %in% c("IV", "First-stage Regression"))
      stopifnot(grepl("Instrument Wald F", output$results_table$html, fixed = TRUE))
    stopifnot(inherits(app$result_plot(result), "ggplot"))
    if (exports) {
      csv <- read.csv(output$download_coefficients)
      stopifnot(all(csv$geography == result$analysis_level), all(csv$instrument_year == result$iv_year),
        all(csv$instrument_construction == app$instrument_name(result$instrument_type, result$approach)))
      if (inherits(result$models[[1]], "ch_model") && result$analysis_type == "IV") {
        stopifnot(all(c("stock_wright_lm_s", "stock_wright_p_value", "stock_wright_df", "stock_wright_null_elasticity") %in% names(csv)))
        for (name in names(result$models)) {
          main <- csv[csv$model == name & csv$term == "ch_elasticity", ]
          diagnostic <- result$models[[name]]$stock_wright
          stopifnot(nrow(main) == 1L,
            isTRUE(all.equal(main$stock_wright_lm_s, diagnostic$statistic, tolerance = 1e-10)),
            isTRUE(all.equal(main$stock_wright_p_value, diagnostic$p_value, tolerance = 1e-10)),
            main$stock_wright_null_elasticity == 0, main$stock_wright_df == 1L,
            !"wald_p_value" %in% names(csv))
        }
      }
      assert_ch_diagnostics_hidden(csv = csv)
      stopifnot(all(csv$instrument_units == app$instrument_units(result)),
        all(csv$schooling_adjustment == app$schooling_adjustment_name(result$schooling_adj)))
      html <- paste(readLines(output$download_results, warn = FALSE), collapse = "\n")
      stopifnot(grepl('<h2>Geographic controls</h2>', html, fixed = TRUE),
        grepl(app$instrument_name(result$instrument_type, result$approach), html, fixed = TRUE))
      assert_ch_diagnostics_hidden(html)
      if (inherits(result$models[[1]], "ch_model") && result$analysis_type == "IV")
        stopifnot(grepl("Stock–Wright LM S", html, fixed = TRUE), grepl("Stock–Wright p-value", html, fixed = TRUE),
          !grepl("Instrument Wald F", html, fixed = TRUE))
      if (!inherits(result$models[[1]], "ch_model") && result$analysis_type %in% c("IV", "First-stage Regression"))
        stopifnot(grepl("Instrument Wald F", html, fixed = TRUE))
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
      sample_scope = scope), exports = year == 1900 && scope == "states_only")
  }
}
for (level in c("MSA", "County")) for (method in c("OLS", "First-stage Regression")) {
  settings <- list(analysis_level = level, analysis_type = method)
  settings[[paste0(tolower(level), "_sample_year")]] <- "1900"
  settings[[paste0(tolower(level), "_iv_year")]] <- "1900"
  settings[[paste0(tolower(level), "_instrument_type")]] <- "area_population"
  run_checked(settings)
}
for (level in c("MSA", "County")) for (type in c("area_population", if (level == "MSA") "overlap" else "max_density_overlap")) {
  prefix <- tolower(level)
  settings <- list(analysis_level = level, analysis_type = "First-stage Regression", instrument_form = "log")
  settings[[paste0(prefix, "_sample_year")]] <- "1900"
  settings[[paste0(prefix, "_iv_year")]] <- "1900"
  settings[[paste0(prefix, "_instrument_type")]] <- type
  settings[[paste0(prefix, "_schooling_adj")]] <- "3"
  result <- run_checked(settings, exports = TRUE)
  stopifnot(result$instrument_form == "log", all(is.finite(result$data_for_fs$instrument)))
}
both <- run_checked(list(analysis_level = "County", analysis_type = "OLS", county_schooling_adj = "3",
  county_sample_year = "1900", county_iv_year = "1900", county_instrument_type = "area_population"))
stopifnot(identical(names(both$models), c("Same Return Adj.", "Specific Return Adj.")))
cat("All model checks passed.\n")
