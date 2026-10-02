source("tests/check_state_core.R")
source("tests/setup.R")
required <- unlist(lapply(app$historical_census_years, function(year)
  paste0(rep(c("AWpop_", "GGpop_", "AWvalid_", "GGvalid_"), each = 2L), year, c("", "_s"))))
missing <- setdiff(required, names(app$state_data))
if (length(missing)) stop(paste("State panel is missing required historical fields:", paste(missing, collapse = ", ")))
stopifnot(identical(sort(unique(as.integer(app$state_data$year))), 2001:2022),
  all(table(app$state_data$year) == 48L), !any(app$state_data$statefips %in% c(2, 11, 15)))
ui <- htmltools::renderTags(app$ui)$html
select_values <- function(id) {
  select <- regmatches(ui, regexec(paste0('<select[^>]*id="', id, '"[^>]*>[\\s\\S]*?</select>'), ui, perl = TRUE))[[1]][1]
  options <- regmatches(select, gregexpr('<option value="[^"]+"', select, perl = TRUE))[[1]]
  as.integer(sub('^<option value="([^"]+)"$', '\\1', options))
}
for (id in c("state_sample_year", "state_iv_year", "msa_sample_year", "msa_iv_year", "county_sample_year", "county_iv_year"))
  stopifnot(identical(select_values(id), as.integer(app$historical_census_years)))
stopifnot(identical(select_values("water_year"), as.integer(seq(1790, 1860, 10))),
  grepl('value="State"', ui, fixed = TRUE), grepl("at least 95% geographic coverage", app$help_state_ch, fixed = TRUE))

# Reconstruct the estimating equations and covariance directly from county inputs.
check_estimate <- function(result) {
  model <- result$models[[1]]
  d <- result$data_for_fs[model$rows, , drop = FALSE]
  ncols <- grep("^nc[0-9]+$", names(d), value = TRUE)
  n <- as.matrix(d[ncols])
  a <- as.matrix(d[sub("^nc", "ac", ncols)])
  theta <- unname(coef(model)[["ch_elasticity"]]) + 1
  weight <- n^theta * a^(1 - theta)
  log_density <- matrix(0, nrow(n), ncol(n))
  log_density[n > 0] <- log(n[n > 0] / a[n > 0])
  index <- log(rowSums(weight) / rowSums(n))
  derivative <- rowSums(weight * log_density) / rowSums(weight)
  intercept <- mean(d$LHS - index)
  residual <- d$LHS - index - intercept
  D <- cbind(derivative, 1)
  Z <- if (result$analysis_type == "IV") cbind(as.numeric(scale(d$instrument)), 1) else D
  moment <- colSums(Z * residual) / sqrt(colSums(Z^2))
  stopifnot(max(abs(moment)) < 1e-5, abs(coef(model)[["(Intercept)"]] - intercept) < 1e-8)
  influence <- (Z * residual) %*% t(solve(crossprod(Z, D)))
  correction <- if (result$analysis_type == "OLS") nrow(d) / (nrow(d) - 2L) else 1
  covariance <- crossprod(influence) * correction
  stopifnot(isTRUE(all.equal(unname(vcov(model)), unname(covariance), tolerance = 1e-7)))
}
years <- app$historical_census_years
rows <- list()
for (year in years) for (construction in c('county_population', 'area_population')) for (scope in c('states_only', 'states_territories')) for (method in c('OLS', 'IV')) {
  result <- NULL
  shiny::testServer(app$server, {
    session$setInputs(analysis_level = 'State', year_modern = '2010', analysis_type = method,
      state_sample_year = as.character(year), state_iv_year = as.character(year),
      state_instrument_type = construction, sample_scope = scope,
      msa_density_measure = 'average', msa_instrument_type = 'overlap',
      county_instrument_type = 'max_density_overlap', approach = 'employment',
      geo_controls = c('terrain', 'crop', 'climate'), use_fe = TRUE, show_map = FALSE,
      run_analysis = 1)
    session$flushReact()
    result <<- isolate(analysis_output())
    if (!is.null(result$error)) stop(result$error)
    stopifnot(result$analysis_level == 'State', result$density_measure == 'CH',
      result$fe_type == 'No', length(result$controls_vec) == 0L, result$approach == 'density',
      !result$apply_college_adj, result$schooling_adj == 0L)
    model <- result$models[[1]]
    check_estimate(result)
    prefix <- if (construction == 'county_population') 'GG' else 'AW'
    suffix <- if (scope == 'states_only') '_s' else ''
    stopifnot(isTRUE(all.equal(result$data_for_fs$instrument,
      as.numeric(result$data_for_fs[[paste0(prefix, 'pop_', year, suffix)]]) / 1000, check.attributes = FALSE)),
      all(result$data_for_fs[[paste0(prefix, 'valid_', year, suffix)]] == 1L))
    stopifnot(all(is.finite(coef(model))), all(is.finite(vcov(model))), nobs(model) <= 48)
    if (method == 'IV') {
      used <- result$data_for_fs[model$rows, ]
      score <- (used$LHS - mean(used$LHS)) * (used$instrument - mean(used$instrument))
      expected_s <- sum(score)^2 / sum(score^2)
      stopifnot(model$stock_wright$status == 'available', model$stock_wright$nobs == nobs(model),
        isTRUE(all.equal(model$stock_wright$statistic, expected_s, tolerance = 1e-10)),
        grepl('Stock–Wright LM S', output$results_table$html, fixed = TRUE),
        grepl('Anderson–Rubin Wald χ²', output$results_table$html, fixed = TRUE),
        grepl('Anderson–Rubin p-value', output$results_table$html, fixed = TRUE),
        grepl('Local instrument Wald F', output$results_table$html, fixed = TRUE),
        grepl('Local instrument partial R²', output$results_table$html, fixed = TRUE),
        !grepl('Wald p-value', output$results_table$html, fixed = TRUE),
        model$instrument_relevance$status == 'available',
        model$anderson_rubin$status == 'available', model$anderson_rubin$nobs == nobs(model),
        model$anderson_rubin$null_elasticity == 0, model$anderson_rubin$df == 1L,
        model$instrument_relevance$nobs == nobs(model),
        model$instrument_relevance$evaluated_theta == model$theta,
        model$instrument_relevance$covariance == 'HC0')
      if (year == 1900 && scope == 'states_only') {
        expected_s <- if (construction == 'area_population') 1.4868677788548883 else 1.2606478772765684
        stopifnot(abs(model$stock_wright$statistic - expected_s) < 1e-10)
      }
    } else stopifnot(is.null(model$stock_wright), is.null(model$anderson_rubin), is.null(model$instrument_relevance),
      !grepl('Stock–Wright LM S', output$results_table$html, fixed = TRUE),
      !grepl('Anderson–Rubin Wald χ²', output$results_table$html, fixed = TRUE),
      !grepl('Local instrument Wald F', output$results_table$html, fixed = TRUE))
    stopifnot(grepl('Ciccone', output$results_table$html), grepl('State', output$analysis_details$html),
      output$data_notes_heading == 'State data and method')
    stopifnot(grepl('at least 95% geographic coverage', output$data_notes$html, fixed = TRUE))
    if (year == 1900) {
      csv <- read.csv(output$download_coefficients)
      stopifnot(all(csv$geography == 'State'), all(csv$density_measure == 'CH'),
        all(csv$instrument_units == 'Thousands of people'), all(csv$instrument_year == year),
        all(csv$sample_year == year), all(csv$standard_errors == 'robust'),
        !'water_year' %in% names(csv), !'overlap_threshold_pct' %in% names(csv))
      if (method == 'IV') {
        main <- csv[csv$term == 'ch_elasticity', ]
        diagnostic <- model$instrument_relevance
        stopifnot(nrow(main) == 1L,
          isTRUE(all.equal(main$anderson_rubin_wald_chi_squared, model$anderson_rubin$statistic, tolerance = 1e-10)),
          isTRUE(all.equal(main$anderson_rubin_p_value, model$anderson_rubin$p_value, tolerance = 1e-10)),
          main$anderson_rubin_null_elasticity == 0, main$anderson_rubin_covariance == 'HC0',
          isTRUE(all.equal(main$ch_local_instrument_wald_f, diagnostic$statistic, tolerance = 1e-10)),
          isTRUE(all.equal(main$ch_local_partial_r2, diagnostic$partial_r2, tolerance = 1e-10)),
          main$ch_local_covariance == 'HC0', !'wald_p_value' %in% names(csv))
      } else stopifnot(!'ch_local_instrument_wald_f' %in% names(csv), !'anderson_rubin_p_value' %in% names(csv))
      html <- paste(readLines(output$download_results, warn = FALSE), collapse = '\n')
      stopifnot(grepl('<h2>State data</h2>', html, fixed = TRUE),
        grepl('at least 95% geographic coverage', html, fixed = TRUE),
        grepl('Known unallocated population', html, fixed = TRUE),
        grepl(app$instrument_name(construction), html, fixed = TRUE),
        !grepl('<h2>Geographic controls</h2>', html, fixed = TRUE),
        !grepl('Water-access year', html, fixed = TRUE))
      if (method == 'IV') stopifnot(all(csv$stock_wright_df == 1L), all(csv$stock_wright_null_elasticity == 0),
        all(abs(csv$stock_wright_lm_s - model$stock_wright$statistic) < 1e-10),
        all(abs(csv$stock_wright_p_value - model$stock_wright$p_value) < 1e-10),
        grepl('Stock–Wright LM S', html, fixed = TRUE))
      else stopifnot(!'stock_wright_lm_s' %in% names(csv))
    }
    prior_header <- output$results_header
    session$setInputs(analysis_level = 'County', state_iv_year = '1790')
    stopifnot(identical(prior_header, output$results_header))
  })
  model <- result$models[[1]]
  rows[[length(rows) + 1L]] <- data.frame(year, construction, scope, method,
    elasticity = coef(model)[['ch_elasticity']], se = sqrt(vcov(model)['ch_elasticity','ch_elasticity']),
    n = nobs(model))
  cat('State app passed:', year, construction, scope, method, 'N', nobs(model), '\n')
}
for (construction in c('county_population', 'area_population')) for (scope in c('states_only', 'states_territories')) {
  result <- app$state_ch_result(list(year_modern = '2010', analysis_type = 'IV',
    state_sample_year = '1870', state_iv_year = '1900', state_instrument_type = construction,
    sample_scope = scope), app$state_data)
  prefix <- if (construction == 'county_population') 'GG' else 'AW'
  suffix <- if (scope == 'states_only') '_s' else ''
  stopifnot(all(result$data_for_fs[[paste0(prefix, 'valid_1870', suffix)]] == 1L),
    all(result$data_for_fs[[paste0(prefix, 'valid_1900', suffix)]] == 1L))
  check_estimate(result)
}
for (scope in c('states_only', 'states_territories')) for (iv_year in if (scope == 'states_only') c(1830, 1840, 1850) else c(1830, 1840)) {
  shiny::testServer(app$server, {
    session$setInputs(analysis_level = 'State', year_modern = '2010', analysis_type = 'IV',
      state_sample_year = '1790', state_iv_year = as.character(iv_year), state_instrument_type = 'area_population',
      sample_scope = scope, msa_density_measure = 'average', approach = 'density', show_map = FALSE, run_analysis = 1)
    session$flushReact()
    result <- isolate(analysis_output())
    if (!is.null(result$error)) stop(result$error)
    stopifnot(length(result$model_warnings) == 1L,
      grepl('multiple solutions', result$model_warnings, fixed = TRUE),
      grepl(result$model_warnings, output$sample_note$html, fixed = TRUE))
    if (scope == 'states_territories' && iv_year == 1830) {
      csv <- read.csv(output$download_coefficients)
      html <- paste(readLines(output$download_results, warn = FALSE), collapse = '\n')
      stopifnot(all(csv$estimation_notes == result$model_warnings),
        grepl(result$model_warnings, html, fixed = TRUE))
    }
  })
}
shiny::testServer(app$server, {
  session$setInputs(analysis_level = 'State', year_modern = '2010', analysis_type = 'IV',
    state_sample_year = '1900', state_iv_year = '1910', state_instrument_type = 'area_population',
    sample_scope = 'states_only', msa_density_measure = 'average', approach = 'density', show_map = FALSE, run_analysis = 1)
  session$flushReact()
  stopifnot(grepl('Choose a historical census year', isolate(analysis_output())$error, fixed = TRUE),
    output$results_header == 'Analysis Error', grepl('Choose a historical census year', output$results_table$html, fixed = TRUE))
})
local({
  original_state <- app$state_data
  on.exit({app$state_data <- original_state})
  bad <- original_state
  retained <- which(bad$year == 2010)
  ncols <- grep('^nc[0-9]+$', names(bad), value = TRUE)
  bad[retained, ncols] <- 0
  bad[retained, sub('^nc', 'ac', ncols)] <- 1
  density <- seq(1, 4, length.out = length(retained))
  bad$nc1[retained] <- exp(density)
  bad$LHS[retained] <- -10 * density
  bad$RHS[retained] <- density
  bad$ch_complete[retained] <- 1L
  bad$AWpop_1900_s[retained] <- density * 1000
  bad$AWvalid_1900_s[retained] <- 1L
  app$state_data <- bad
  shiny::testServer(app$server, {
    session$setInputs(analysis_level = 'State', year_modern = '2010', analysis_type = 'IV',
      state_sample_year = '1900', state_iv_year = '1900', state_instrument_type = 'area_population',
      sample_scope = 'states_only', msa_density_measure = 'average', approach = 'density', show_map = FALSE, run_analysis = 1)
    session$flushReact()
    failed <- isolate(analysis_output())
    stopifnot(!is.null(failed$error), failed$stock_wright$status == 'available',
      grepl('Stock–Wright LM S', output$results_table$html, fixed = TRUE),
      grepl('CH elasticity = 0', output$results_table$html, fixed = TRUE))
  })
})
results <- dplyr::bind_rows(rows)
jsonlite::write_json(results, file.path(work, 'state_app_results.json'), pretty = TRUE, digits = 15)
print(results, row.names = FALSE)
