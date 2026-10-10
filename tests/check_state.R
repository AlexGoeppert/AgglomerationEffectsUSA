source("tests/check_state_core.R")
source("tests/setup.R")
assert_ch_diagnostics_hidden <- function(html = NULL, csv = NULL) {
  if (!is.null(html)) stopifnot(!grepl("Anderson.{0,12}Rubin|Local instrument Wald F|Local instrument partial R|partial R-squared|partial R²", html, ignore.case = TRUE))
  if (!is.null(csv)) stopifnot(!any(grepl("^anderson_rubin_|^ch_local_partial_r2$", names(csv))))
}
assert_f_row <- function(html, expected) {
  row <- regmatches(html, regexec('<tr[^>]*><td[^>]*>Instrument Wald F</td>(.*?)</tr>', html, perl = TRUE))[[1]]
  stopifnot(length(row) == 2L)
  cells <- regmatches(row[2], gregexpr('<td[^>]*>[^<]*</td>', row[2], perl = TRUE))[[1]]
  shown <- sub('<td[^>]*>([^<]*)</td>', '\\1', cells, perl = TRUE)
  formatted <- ifelse(is.finite(expected), sprintf('%.2f', expected), '—')
  stopifnot(identical(shown, unname(formatted)))
}
required <- unlist(lapply(app$historical_census_years, function(year)
  paste0(rep(c("AWpop_", "GGpop_", "AWvalid_", "GGvalid_", "HCH_", "HCHpop_", "HCHvalid_"), each = 2L), year, c("", "_s"))))
missing <- setdiff(required, names(app$state_data))
if (length(missing)) stop(paste("State panel is missing required historical fields:", paste(missing, collapse = ", ")))
stopifnot(identical(sort(unique(as.integer(app$state_data$year))), 2001:2022),
  all(table(app$state_data$year) == 48L), !any(app$state_data$statefips %in% c(2, 11, 15)))
ui <- htmltools::renderTags(app$ui)$html
assert_ch_diagnostics_hidden(ui)
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
        grepl('Stock–Wright p-value', output$results_table$html, fixed = TRUE),
        grepl('Instrument Wald F', output$results_table$html, fixed = TRUE),
        !grepl('Wald p-value', output$results_table$html, fixed = TRUE),
        model$instrument_relevance$status == 'available',
        model$anderson_rubin$status == 'available', model$anderson_rubin$nobs == nobs(model),
        model$anderson_rubin$null_elasticity == 0, model$anderson_rubin$df == 1L,
        model$instrument_relevance$nobs == nobs(model),
        model$instrument_relevance$evaluated_theta == model$theta,
        model$instrument_relevance$covariance == 'HC0')
    } else stopifnot(is.null(model$stock_wright), is.null(model$anderson_rubin), is.null(model$instrument_relevance),
      !grepl('Stock–Wright LM S', output$results_table$html, fixed = TRUE),
      !grepl('Anderson–Rubin Wald χ²', output$results_table$html, fixed = TRUE),
      !grepl('Local instrument Wald F', output$results_table$html, fixed = TRUE))
    assert_ch_diagnostics_hidden(output$results_table$html)
    if (method == 'IV') assert_f_row(output$results_table$html, model$instrument_relevance$statistic)
    if (method == 'OLS') stopifnot(!grepl('<tr[^>]*><td[^>]*>Instrument Wald F</td>', output$results_table$html, perl = TRUE))
    stopifnot(grepl('Ciccone', output$results_table$html), grepl('State', output$analysis_details$html),
      output$data_notes_heading == 'State data and method')
    stopifnot(grepl('at least 95% geographic coverage', output$data_notes$html, fixed = TRUE))
    assert_ch_diagnostics_hidden(output$data_notes$html)
    if (year == 1900) {
      csv <- read.csv(output$download_coefficients)
      stopifnot(all(csv$geography == 'State'), all(csv$density_measure == 'CH'),
        all(csv$instrument_units == 'Thousands of people'), all(csv$instrument_year == year),
        all(csv$sample_year == year), all(csv$standard_errors == 'robust'),
        !'water_year' %in% names(csv), !'overlap_threshold_pct' %in% names(csv))
      if (method == 'IV') {
        main <- csv[csv$term == 'ch_elasticity', ]
        relevance <- model$instrument_relevance
        stopifnot(nrow(main) == 1L,
          all(c('stock_wright_lm_s', 'stock_wright_p_value', 'stock_wright_df', 'stock_wright_null_elasticity',
            'ch_local_instrument_wald_f', 'ch_local_evaluated_theta', 'ch_local_covariance', 'ch_local_relevance_note') %in% names(csv)),
          isTRUE(all.equal(main$ch_local_instrument_wald_f, relevance$statistic, tolerance = 1e-10)),
          isTRUE(all.equal(main$ch_local_evaluated_theta, relevance$evaluated_theta, tolerance = 1e-10)),
          main$ch_local_covariance == relevance$covariance, nzchar(main$ch_local_relevance_note),
          !'wald_p_value' %in% names(csv))
      }
      assert_ch_diagnostics_hidden(csv = csv)
      html <- paste(readLines(output$download_results, warn = FALSE), collapse = '\n')
      stopifnot(grepl('<h2>State data</h2>', html, fixed = TRUE),
        grepl('at least 95% geographic coverage', html, fixed = TRUE),
        grepl('Known unallocated population', html, fixed = TRUE),
        grepl(app$instrument_name(construction), html, fixed = TRUE),
        !grepl('<h2>Geographic controls</h2>', html, fixed = TRUE),
        !grepl('Water-access year', html, fixed = TRUE))
      assert_ch_diagnostics_hidden(html)
      if (method == 'IV') assert_f_row(html, model$instrument_relevance$statistic)
      if (method == 'OLS') stopifnot(!grepl('<tr[^>]*><td[^>]*>Instrument Wald F</td>', html, perl = TRUE))
      if (method == 'IV') stopifnot(all(csv$stock_wright_df == 1L), all(csv$stock_wright_null_elasticity == 0),
        all(abs(csv$stock_wright_lm_s - model$stock_wright$statistic) < 1e-10),
        all(abs(csv$stock_wright_p_value - model$stock_wright$p_value) < 1e-10),
        grepl('Stock–Wright LM S', html, fixed = TRUE), grepl('Stock–Wright p-value', html, fixed = TRUE),
        grepl('Instrument Wald F', html, fixed = TRUE))
      else stopifnot(!'stock_wright_lm_s' %in% names(csv), !'ch_local_instrument_wald_f' %in% names(csv))
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
# Test the reported 2020/1900 case using the rebuilt data and actual app controls.
local({
  original_state <- app$state_data
  on.exit({app$state_data <- original_state})
  options <- list(
    AW_levels = list(construction = 'area_population', scale = 'levels', prefix = 'AW'),
    AW_log = list(construction = 'area_population', scale = 'log', prefix = 'AW'),
    GG_levels = list(construction = 'county_population', scale = 'levels', prefix = 'GG'),
    GG_log = list(construction = 'county_population', scale = 'log', prefix = 'GG'),
    HCH_levels = list(construction = 'historical_ch', scale = 'levels', prefix = 'HCH'),
    HCH_log = list(construction = 'historical_ch', scale = 'log', prefix = 'HCH'))
  used_data <- function(result) result$data_for_fs[result$models[[1]]$rows, , drop = FALSE]
  state_ids <- function(result) sort(as.integer(used_data(result)$statefips))
  estimates <- list()
  members <- list()
  scope_members <- list()
  run_option <- function(option, scope, sample) {
    result <- NULL
    suffix <- if (scope == 'states_only') '_s' else ''
    index <- paste0(if (option$prefix == 'HCH') 'HCH_' else paste0(option$prefix, 'pop_'), '1900', suffix)
    valid <- paste0(option$prefix, 'valid_1900', suffix)
    panel <- app$state_data[app$state_data$year == 2020, , drop = FALSE]
    expected <- panel[[valid]] == 1L & is.finite(panel[[index]]) &
      is.finite(panel$LHS) & is.finite(panel$RHS) & panel$ch_complete == 1L
    if (option$prefix == 'HCH') expected <- expected &
      is.finite(panel[[paste0('HCHpop_1900', suffix)]]) & panel[[paste0('HCHpop_1900', suffix)]] > 0
    else expected <- expected & if (option$scale == 'log') panel[[index]] > 0 else panel[[index]] >= 0
    expected_ids <- sort(as.integer(panel$statefips[which(expected)]))
    shiny::testServer(app$server, {
      session$setInputs(analysis_level = 'State', year_modern = '2020', analysis_type = 'IV',
        state_sample_year = '1900', state_iv_year = '1900', state_instrument_type = option$construction,
        sample_scope = scope, instrument_form = option$scale, msa_density_measure = 'average',
        approach = 'density', use_fe = TRUE, geo_controls = 'terrain', show_map = FALSE, run_analysis = 1)
      session$flushReact()
      session$setInputs(msa_density_measure = 'CH')
      session$flushReact()
      result <<- isolate(analysis_output())
      if (!is.null(result$error)) stop(result$error)
      check_estimate(result)
      stopifnot(identical(state_ids(result), expected_ids), result$fe_type == 'No',
        length(result$controls_vec) == 0L, result$year_modern == 2020L)
      map <- isolate(map_output())
      expected_map_title <- paste('Instrument Map:', app$instrument_name(option$construction))
      if (option$prefix != 'HCH') expected_map_title <- paste(expected_map_title, '\u00b7',
        if (option$scale == 'log') 'Natural log of population (people)' else 'Thousands of people')
      stopifnot(is.null(map$src),
        identical(map$title, expected_map_title),
        identical(map$message, 'A map is not available for state instruments.'))
      used <- used_data(result)
      original_instrument <- as.numeric(used[[index]])
      expected_instrument <- if (option$prefix == 'HCH') original_instrument else
        if (option$scale == 'log') log(original_instrument) else original_instrument / 1000
      stopifnot(isTRUE(all.equal(as.numeric(used$instrument), expected_instrument, tolerance = 1e-12)))
      if (option$prefix == 'HCH') stopifnot(any(used$instrument < 0),
        result$instrument_form == 'historical_ch', result$log_nonpositive_n == 0L,
        grepl('theta0 → 1 limit of the normalized historical CH index', output$results_table$html, fixed = TRUE))
      else stopifnot(result$instrument_form == option$scale)
      model <- result$models[[1]]
      yr <- used$LHS - mean(used$LHS)
      zr <- used$instrument - mean(used$instrument)
      score <- yr * zr
      expected_s <- sum(score)^2 / sum(score^2)
      slope <- sum(zr * yr) / sum(zr^2)
      variance <- sum((zr * (yr - slope * zr))^2) / sum(zr^2)^2
      expected_ar <- slope^2 / variance
      stopifnot(abs(model$stock_wright$statistic - expected_s) < 1e-9,
        abs(model$anderson_rubin$statistic - expected_ar) < 1e-9)
      csv <- read.csv(output$download_coefficients)
      assert_ch_diagnostics_hidden(output$results_table$html, csv)
      stopifnot(all(csv$instrument_form == result$instrument_form),
        all(csv$instrument_units == app$instrument_units(result)),
        all(csv$instrument_year == 1900), all(csv$sample_year == 1900),
        all(abs(csv$stock_wright_lm_s - expected_s) < 1e-9),
        all(abs(csv$ch_local_instrument_wald_f - model$instrument_relevance$statistic) < 1e-9),
        grepl('Stock–Wright LM S', output$results_table$html, fixed = TRUE),
        grepl('Instrument Wald F', output$results_table$html, fixed = TRUE))
    })
    model <- result$models[[1]]
    label <- paste(option$prefix, option$scale, sep = '_')
    estimates[[length(estimates) + 1L]] <<- data.frame(scope, sample, instrument = label,
      n = nobs(model), elasticity = coef(model)[['ch_elasticity']],
      se = sqrt(vcov(model)['ch_elasticity', 'ch_elasticity']),
      S = model$stock_wright$statistic, S_p = model$stock_wright$p_value,
      AR = model$anderson_rubin$statistic, AR_p = model$anderson_rubin$p_value)
    used <- used_data(result)
    members[[length(members) + 1L]] <<- data.frame(scope, sample, instrument = label,
      statefips = as.integer(used$statefips), state = used$state_name, value = used$instrument)
    result
  }
  for (scope in c('states_only', 'states_territories')) {
    app$state_data <- original_state
    native <- lapply(options, run_option, scope = scope, sample = 'available')
    scope_members[[scope]] <- lapply(native, state_ids)
    expected_n <- if (scope == 'states_only') 42L else 43L
    for (name in c('AW_levels', 'AW_log', 'HCH_levels', 'HCH_log'))
      stopifnot(nobs(native[[name]]$models[[1]]) == expected_n,
        all(c(20L, 29L) %in% state_ids(native[[name]]))) # Kansas and Missouri
    for (pair in list(c('AW_levels', 'AW_log'), c('GG_levels', 'GG_log'), c('HCH_levels', 'HCH_log')))
      stopifnot(identical(state_ids(native[[pair[1]]]), state_ids(native[[pair[2]]])))
    stopifnot(isTRUE(all.equal(coef(native$HCH_levels$models[[1]]), coef(native$HCH_log$models[[1]]), tolerance = 1e-12)),
      isTRUE(all.equal(vcov(native$HCH_levels$models[[1]]), vcov(native$HCH_log$models[[1]]), tolerance = 1e-12)))
    common_ids <- Reduce(intersect, lapply(native, state_ids))
    stopifnot(length(common_ids) >= 4L)
    app$state_data <- original_state[original_state$year != 2020 | original_state$statefips %in% common_ids, , drop = FALSE]
    common <- lapply(options, run_option, scope = scope, sample = 'common')
    reference <- used_data(common[[1]])
    fields <- c('statefips', 'LHS', 'RHS', grep('^(nc|ac)[0-9]+$', names(reference), value = TRUE))
    reference <- reference[order(reference$statefips), fields]
    for (result in common) {
      stopifnot(identical(state_ids(result), sort(common_ids)))
      used <- used_data(result)
      stopifnot(isTRUE(all.equal(reference, used[order(used$statefips), fields], check.attributes = FALSE)))
    }
  }
  for (name in c('AW_levels', 'HCH_levels')) {
    state_ids_only <- scope_members$states_only[[name]]
    state_ids_all <- scope_members$states_territories[[name]]
    stopifnot(all(state_ids_only %in% state_ids_all),
      identical(as.integer(setdiff(state_ids_all, state_ids_only)), 35L)) # New Mexico
  }
  write.csv(do.call(rbind, estimates), file.path(work, 'state_2020_1900_options.csv'), row.names = FALSE)
  write.csv(do.call(rbind, members), file.path(work, 'state_2020_1900_option_samples.csv'), row.names = FALSE)
  cat('State 2020/1900 checks passed: native/common samples, exact logs, signed HCH and restored Kansas/Missouri coverage.\n')
})
# Construct two known IV roots rather than depending on an old geographic sample.
local({
  original_state <- app$state_data
  on.exit({app$state_data <- original_state})
  fixture <- original_state
  retained <- which(fixture$year == 2010)
  i <- seq_along(retained)
  n <- cbind(exp(1 + i / 25), exp(.5 + sin(i * .31)))
  a <- cbind(rep(1, length(i)), rep(2, length(i)))
  direct_index <- function(theta) log(rowSums(n^theta * a^(1 - theta)) / rowSums(n))
  first <- direct_index(1.05)
  second <- direct_index(1.5)
  z <- qr.resid(qr(cbind(1, second - first)), sin(i * .73))
  noise <- qr.resid(qr(cbind(1, z)), cos(i * .41))
  y <- 9 + first + .01 * noise
  stopifnot(sum(z^2) > 1e-4,
    abs(sum(z * (y - first))) < 1e-10, abs(sum(z * (y - second))) < 1e-10)
  ncols <- grep('^nc[0-9]+$', names(fixture), value = TRUE)
  fixture[retained, ncols] <- 0
  fixture[retained, sub('^nc', 'ac', ncols)] <- 1
  fixture$nc1[retained] <- n[, 1]
  fixture$nc2[retained] <- n[, 2]
  fixture$ac2[retained] <- a[, 2]
  fixture$LHS[retained] <- y
  fixture$RHS[retained] <- log(rowSums(n) / rowSums(a))
  fixture$ch_complete[retained] <- 1L
  fixture$AWpop_1900_s[retained] <- (z - min(z) + 1) * 1000
  fixture$AWvalid_1900_s[retained] <- 1L
  app$state_data <- fixture
  shiny::testServer(app$server, {
    session$setInputs(analysis_level = 'State', year_modern = '2010', analysis_type = 'IV',
      state_sample_year = '1900', state_iv_year = '1900', state_instrument_type = 'area_population',
      sample_scope = 'states_only', instrument_form = 'levels', msa_density_measure = 'average',
      approach = 'density', show_map = FALSE, run_analysis = 1)
    session$flushReact()
    result <- isolate(analysis_output())
    if (!is.null(result$error)) stop(result$error)
    stopifnot(length(result$model_warnings) == 1L,
      grepl('multiple solutions', result$model_warnings, fixed = TRUE),
      grepl(result$model_warnings, output$sample_note$html, fixed = TRUE))
    csv <- read.csv(output$download_coefficients)
    html <- paste(readLines(output$download_results, warn = FALSE), collapse = '\n')
    stopifnot(all(csv$estimation_notes == result$model_warnings),
      grepl(result$model_warnings, html, fixed = TRUE))
    assert_ch_diagnostics_hidden(html, csv)
  })
})
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
    assert_ch_diagnostics_hidden(output$results_table$html)
  })
})
results <- dplyr::bind_rows(rows)
jsonlite::write_json(results, file.path(work, 'state_app_results.json'), pretty = TRUE, digits = 15)
print(results, row.names = FALSE)
