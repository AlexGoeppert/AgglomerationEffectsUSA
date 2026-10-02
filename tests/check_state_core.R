# This check uses only base R, so the model and filters can also be checked offline.
local({
  core <- new.env(parent = globalenv())
  functions <- c("historical_census_years", "ch_index_terms", "stock_wright_lm_s",
    "auxiliary_instrument_wald", "ch_instrument_relevance", "anderson_rubin_wald", "fit_ch_model", "state_ch_result",
    "coef.ch_model", "vcov.ch_model", "nobs.ch_model", "residuals.ch_model", "fitted.ch_model", "confint.ch_model")
  for (expr in parse("app.R")) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]]) %in% functions) eval(expr, core)
  }
  for (generic in c("coef", "vcov", "nobs", "residuals", "fitted", "confint"))
    registerS3method(generic, "ch_model", core[[paste0(generic, ".ch_model")]], envir = asNamespace("stats"))
  settings <- list(year_modern = "2010", state_sample_year = "1790", state_iv_year = "1900",
    analysis_type = "IV", state_instrument_type = "area_population", sample_scope = "states_only")
  i <- seq_len(48)
  data <- data.frame(year = 2010L, statefips = i, state_name = paste("State", i), ch_complete = 1L,
    nc1 = 200 + i^2 * 18, nc2 = 160 + i^2 * 11, nc3 = 100 + i^2 * 6,
    ac1 = 35 + i %% 7, ac2 = 60 + i %% 5, ac3 = 25 + i %% 3)
  n <- as.matrix(data[c("nc1", "nc2", "nc3")])
  a <- as.matrix(data[c("ac1", "ac2", "ac3")])
  theta <- 1.12
  data$LHS <- 9 + log(rowSums(n^theta * a^(1 - theta)) / rowSums(n))
  data$RHS <- log(rowSums(n) / rowSums(a))
  for (year in core$historical_census_years) for (prefix in c("AW", "GG")) for (suffix in c("", "_s")) {
    data[[paste0(prefix, "pop_", year, suffix)]] <- (i + 5)^2 * (year - 1700) * if (suffix == "_s") 0.8 else 1
    data[[paste0(prefix, "valid_", year, suffix)]] <- 1L
  }
  checks <- 0L
  for (year in core$historical_census_years) for (method in c("IV", "OLS"))
    for (construction in c("county_population", "area_population")) for (scope in c("states_only", "states_territories")) {
      input <- modifyList(settings, list(state_sample_year = as.character(year), state_iv_year = as.character(year),
        analysis_type = method, state_instrument_type = construction, sample_scope = scope))
      result <- core$state_ch_result(input, data)
      model <- result$models[[1]]
      prefix <- if (construction == "county_population") "GG" else "AW"
      suffix <- if (scope == "states_only") "_s" else ""
      stopifnot(abs(coef(model)[["ch_elasticity"]] - (theta - 1)) < 1e-6,
        abs(coef(model)[["(Intercept)"]] - 9) < 1e-6, nobs(model) == 48L,
        identical(result$data_for_fs$instrument, data[[paste0(prefix, "pop_", year, suffix)]] / 1000),
        result$fe_type == "No", result$se_spec == "robust", result$schooling_adj == 0L)
      checks <- checks + 1L
    }
  for (year in 2001:2022) {
    current <- data
    current$year <- year
    result <- core$state_ch_result(modifyList(settings, list(year_modern = as.character(year))), current)
    stopifnot(result$year_modern == year, result$n_obs == 48L)
  }
  for (construction in c("county_population", "area_population")) {
    changed <- data
    prefix <- if (construction == "county_population") "GG" else "AW"
    sample <- paste0(prefix, "pop_1790_s")
    instrument <- paste0(prefix, "pop_1900_s")
    changed[1, c(sample, instrument)] <- 0
    changed[2, sample] <- NA_real_
    changed[3, instrument] <- NA_real_
    changed[4, instrument] <- -1
    changed[5, paste0(prefix, "valid_1790_s")] <- 0L
    changed[6, paste0(prefix, "valid_1900_s")] <- 0L
    changed$ch_complete[7] <- 0L
    result <- core$state_ch_result(modifyList(settings, list(analysis_type = "OLS", state_instrument_type = construction)), changed)
    stopifnot(result$population_missing_n == 5L, result$ch_missing_n == 1L, result$n_obs == 42L,
      result$data_for_fs$instrument[result$data_for_fs$statefips == 1L] == 0,
      !any(2:6 %in% result$data_for_fs$statefips),
      !7L %in% result$data_for_fs$statefips[result$models[[1]]$rows])
  }
  expect_error <- function(input = settings, changed = data, message) {
    error <- tryCatch({core$state_ch_result(input, changed); NULL}, error = conditionMessage)
    stopifnot(is.character(error), grepl(message, error, fixed = TRUE))
  }
  for (value in list(NULL, "First-stage Regression", NA_character_))
    expect_error(modifyList(settings, list(analysis_type = value)), message = "Choose IV or OLS")
  expect_error(modifyList(settings, list(state_instrument_type = "overlap")), message = "Choose a state population")
  expect_error(modifyList(settings, list(sample_scope = "unknown")), message = "Choose a historical territory scope")
  expect_error(modifyList(settings, list(year_modern = "2000")), message = "Choose a modern year")
  for (field in c("state_sample_year", "state_iv_year")) for (value in list(NULL, "1910", "not a year")) {
    invalid <- settings
    invalid[field] <- list(value)
    expect_error(invalid, message = "Choose a historical census year")
  }
  expect_error(changed = data[FALSE, ], message = "State data are unavailable")
  expect_error(changed = data[setdiff(names(data), "AWvalid_1900_s")], message = "State data fields are missing")
  expect_error(changed = data[setdiff(names(data), "ac1")], message = "CH model variables are unavailable")
  changed <- data
  changed$ch_complete <- 0L
  expect_error(changed = changed, message = "No observations have complete county components")
  changed <- data
  changed$AWpop_1900_s <- 0
  expect_error(changed = changed, message = "historical instrument has no variation")
  cat("State core checks passed:", checks, "known-coefficient specifications, 22 modern years, source zeroes, validity flags and errors.\n")
})
