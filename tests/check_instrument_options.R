# Base-R checks for instrument units, source scope and nonlinear estimation.
local({
  core <- new.env(parent = globalenv())
  for (expr in parse("app.R")) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        (is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function")) ||
         identical(as.character(expr[[2]]), "historical_census_years"))) eval(expr, core)
  }
  for (generic in c("coef", "vcov", "nobs", "residuals", "fitted", "confint"))
    registerS3method(generic, "ch_model", core[[paste0(generic, ".ch_model")]], envir = asNamespace("stats"))
  values <- c(0, -1, 1, 1000, NA, Inf)
  level <- core$transform_historical_instrument(values, "levels", population = TRUE)
  logged <- core$transform_historical_instrument(values, "log", population = TRUE)
  density <- core$transform_historical_instrument(values, "log", population = FALSE)
  index <- core$transform_historical_instrument(c(-2, 0, 3), "log", historical_ch = TRUE)
  stopifnot(identical(level$keep, c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)),
    identical(level$values[3:4], c(.001, 1)), identical(logged$keep, c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)),
    logged$nonpositive == 2L, identical(logged$values[3:4], log(c(1, 1000))),
    identical(logged$values, density$values), identical(index$values, c(-2, 0, 3)), all(index$keep),
    index$form == "historical_ch",
    core$control_label("instrument", "county_population", instrument_form = "log") == "Log historical population",
    core$control_label("instrument", "overlap", instrument_form = "log") == "Log historical density")
  # NAICS 11 output and employment exclusions cover the same sectors.
  sector4 <- core$sector_exclusion_outcome(c(100, 100, 100, 1), c(10, NA, 10, 2),
    c(20, 20, 20, 1), c(5, 5, 20, 2))
  sector5 <- core$sector_exclusion_outcome(100, 10, 20, 5, 10, 1)
  stopifnot(abs(sector4[1] - log(90 / 15)) < 1e-14, all(is.na(sector4[2:4])),
    abs(sector5 - log(80 / 14)) < 1e-14)

  # Historical FE identities use the sample year even when a later instrument is selected.
  scope_data <- data.frame(state_id = c("VA", "TN", "PA"), hist_state_fe_1790 = c("Virginia", "Southwest Territory", "Pennsylvania"),
    hist_state_fe_1790_s = c("Virginia", NA, "Pennsylvania"), hist_state_fe_1900 = c("West Virginia", "Tennessee", "Pennsylvania"))
  scoped <- core$historical_sample_filter(scope_data, 1790, "states_only", "overlap", TRUE, "historical")
  stopifnot(nrow(scoped) == 2L, identical(scoped$hist_state_fe_1790_s, c("Virginia", "Pennsylvania")))
  expect_error <- function(code, pattern) {
    result <- tryCatch(force(code), error = conditionMessage)
    stopifnot(is.character(result), length(result) == 1L, grepl(pattern, result, fixed = TRUE))
  }
  expect_error(core$historical_sample_filter(scope_data[setdiff(names(scope_data), "hist_state_fe_1790")],
    1790, "states_only", "overlap", FALSE, "historical"), "identifiers are missing")
  scope_data$hist_state_fe_1900[1] <- NA
  scoped <- core$historical_sample_filter(scope_data, 1900, "states_territories", "overlap", TRUE, "historical")
  stopifnot(identical(scoped$state_id, c("TN", "PA")), attr(scoped, "fe_missing_n") == 1L,
    length(core$sample_exclusion_notes(list(fe_missing_n = attr(scoped, "fe_missing_n")))) == 1L)
  expect_error(core$historical_sample_filter(scope_data[setdiff(names(scope_data), "hist_state_fe_1900")],
    1900, "states_territories", "county_population", TRUE, "historical"), "identifiers are missing")

  # Schooling references exclude missing controls, invalid outcomes and linear FE singletons.
  sample <- data.frame(y = c(1, 2, 3, Inf, 4, 5), control = c(1, 2, NA, 3, 4, 5),
    state = c("A", "A", "B", "B", "C", "D"), schooling = c(10, 12, 30, 40, 50, 60))
  mask <- core$model_sample_mask(sample, c("y", "control", "schooling"), "state", TRUE)
  stopifnot(identical(which(mask), 1:2), mean(sample$schooling[mask]) == 11)

  # A boundary NLS optimum must not block an interior, identified IV solution.
  set.seed(707)
  size <- 80L
  z <- rnorm(size)
  noise <- qr.resid(qr(cbind(1, z)), rnorm(size))
  density <- .6 * z + noise
  boundary <- data.frame(nc1 = exp(density), ac1 = 1, instrument = z,
    y = 9 + .12 * density - 10 * noise, ch_complete = 1L)
  fit <- core$fit_ch_model(boundary, "y", method = "IV", se_spec = "robust")
  stopifnot(abs(fit$theta - 1.12) < 1e-7, fit$stock_wright$status == "available")
  expect_error(core$fit_ch_model(boundary, "y", method = "OLS", se_spec = "robust"), "search boundary")

  # State HCH may be negative; it is neither population-scaled nor logged again.
  i <- seq_len(24)
  state <- data.frame(year = 2010, statefips = i, state_name = paste("State", i), ch_complete = 1L,
    nc1 = 10 + i^2, nc2 = 5 + i, ac1 = 500, ac2 = 600)
  state$RHS <- log((state$nc1 + state$nc2) / 1100)
  state$LHS <- 9 + core$ch_index_terms(1.08, as.matrix(state[c("nc1", "nc2")]), as.matrix(state[c("ac1", "ac2")]))$value
  for (suffix in c("", "_s")) for (year in c(1790, 1900)) {
    state[[paste0("HCH_", year, suffix)]] <- state$RHS - 2
    state[[paste0("HCHvalid_", year, suffix)]] <- 1L
    state[[paste0("HCHpop_", year, suffix)]] <- 100 + i
    for (prefix in c("GG", "AW")) {
      state[[paste0(prefix, "pop_", year, suffix)]] <- 100 + i^2
      state[[paste0(prefix, "valid_", year, suffix)]] <- 1L
    }
  }
  settings <- list(year_modern = "2010", state_sample_year = "1790", state_iv_year = "1900",
    analysis_type = "IV", state_instrument_type = "historical_ch", sample_scope = "states_only", instrument_form = "log")
  result <- core$state_ch_result(settings, state)
  stopifnot(result$n_obs == 24L, all(result$data_for_fs$instrument < 0),
    identical(result$data_for_fs$instrument, state$HCH_1900_s), result$instrument_form == "historical_ch",
    abs(coef(result$models[[1]])[["ch_elasticity"]] - .08) < 1e-7)
  for (construction in c("county_population", "area_population")) {
    result <- core$state_ch_result(modifyList(settings, list(state_instrument_type = construction)), state)
    stopifnot(identical(result$data_for_fs$instrument, log(100 + i^2)), result$instrument_form == "log",
      abs(coef(result$models[[1]])[["ch_elasticity"]] - .08) < 1e-7)
  }
  state$HCHpop_1900_s[1] <- 0
  state$HCHvalid_1790_s[2] <- 0
  result <- core$state_ch_result(settings, state)
  stopifnot(result$n_obs == 22L, result$population_missing_n == 2L)
  cat("Instrument options checks passed: exact logs, HCH state input, historical scope, final samples and IV boundary recovery.\n")
})
