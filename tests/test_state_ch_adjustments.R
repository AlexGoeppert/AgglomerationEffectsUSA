# Run from the application directory: Rscript tests/test_state_ch_adjustments.R
source("state_ch_adjustments.R")

close_enough <- function(a, b, tolerance = 1e-7) {
  stopifnot(length(a) == length(b), all(is.finite(a)), all(is.finite(b)),
            max(abs(a - b)) < tolerance)
}

set.seed(89245)
S <- 24L; C <- 8L
state <- rep(seq_len(S), each = C)
state_z <- rnorm(S)
county_z <- exp(.6 * state_z[state] + rnorm(S * C, sd = .4))
g <- .4 * state_z[state] + rnorm(S * C)
x <- 2 + .7 * log(county_z) + .1 * g + rnorm(S * C, sd = .2)
school <- 11 + .4 * x + rnorm(S * C, sd = .5)
college <- plogis(-2 + .25 * x + rnorm(S * C, sd = .4))
area <- exp(rnorm(S * C, 3, .3)); employment <- area * exp(x)
county <- data.frame(county_id = seq_len(S * C), state_id = state,
  x = x, y = 10 + rnorm(S, sd = .1)[state] + .06 * x + .04 * g +
    .07 * 1.06 * school + .15 * college + rnorm(S * C, sd = .035),
  instrument = county_z, schooling = school, college = college, geo = g)
components <- data.frame(county_id = county$county_id, state_id = state,
  employment = employment, area = area, schooling = school, college = college, geo = g)
state_levels <- as.character(seq_len(S))
index <- sca_make_index(components, state_levels, "geo", .04, .07, .15, 0)
d <- data.frame(state_id = seq_len(S), LHS = 10 + index(1.09)$value + rnorm(S, sd = .025),
  instrument = as.vector(rowsum(county_z, state, reorder = FALSE)))
settings <- list(mode = "both", geo_vars = "geo", beta_school = .07,
  kappa_college = .15, county_fe = TRUE)

# The long-form calculation equals the stated CH formula.
t <- 1.13
direct <- vapply(seq_len(S), function(s) {
  a <- components[components$state_id == s, ]
  log(sum(a$employment * exp(.04 * a$geo + .15 * a$college + t * .07 * a$schooling) *
    (a$employment / a$area)^(t - 1)) / sum(a$employment))
}, numeric(1))
close_enough(index(t)$value, direct)
h <- 1e-5
close_enough(index(t)$derivative, (index(t + h)$value - index(t - h)$value) / (2 * h))
close_enough(index(t)$second_derivative,
  (index(t + h)$derivative - index(t - h)$derivative) / (2 * h))
index_plus <- sca_make_index(components, state_levels, "geo", .04 + h, .07, .15, 0)
index_minus <- sca_make_index(components, state_levels, "geo", .04 - h, .07, .15, 0)
close_enough(as.vector(index(t)$gamma_derivative),
  (index_plus(t)$value - index_minus(t)$value) / (2 * h))
close_enough(as.vector(index(t)$mixed_derivative),
  (index_plus(t)$derivative - index_minus(t)$derivative) / (2 * h))
stopifnot(any(abs(index(1)$value) > .01))

fit <- fit_state_adjusted_ch(d, county, components, settings)
stopifnot(inherits(fit, "ch_model"), fit$nobs == S,
  abs(fit$iv_moment) < 1e-8, fit$stock_wright$status == "available",
  fit$instrument_relevance$status == "available", fit$county_calibration$nobs == S * C,
  fit$instrument_relevance$conditional_on_calibration,
  min(eigen(fit$full_covariance, symmetric = TRUE)$values) > -1e-10)
close_enough(fit$full_covariance, crossprod(fit$state_influence))
close_enough(fit$state_influence,
  fit$direct_state_influence + fit$calibration_state_influence)
stopifnot(max(abs(fit$calibration_state_influence)) > 1e-7)

# Changing the common schooling reference affects only the fitted intercept.
fit_ref <- fit_state_adjusted_ch(d, county, components, modifyList(settings, list(school_reference = 0)))
close_enough(fit$theta, fit_ref$theta, 1e-7)
close_enough(fit$covariance[1L, 1L], fit_ref$covariance[1L, 1L], 1e-7)
close_enough(fit$stock_wright$statistic, fit_ref$stock_wright$statistic, 1e-7)
close_enough(fit$fitted.values, fit_ref$fitted.values, 1e-7)

# Perturb both stages by the fitted residuals of one shared state. The
# numerical parameter derivative must equal the joint influence for that state.
s <- 7L; epsilon <- 1e-5
county_perturbed <- county
county_perturbed$y[state == s] <- county_perturbed$y[state == s] +
  epsilon * fit$county_calibration$residuals[state == s]
d_perturbed <- d
d_perturbed$LHS[s] <- d_perturbed$LHS[s] + epsilon * fit$residuals[s]
fit_perturbed <- fit_state_adjusted_ch(d_perturbed, county_perturbed, components, settings)
close_enough((fit_perturbed$full_coefficients - fit$full_coefficients) / epsilon,
  fit$state_influence[s, ], 2e-5)

# The adjusted-null score has the same shared calibration derivative. At
# theta=1 the geography and schooling index must still be subtracted.
calibrated_index <- sca_make_index(components, state_levels, "geo",
  fit$county_calibration$gamma, .07, .15, fit$adjustment$school_reference)
perturbed_index <- sca_make_index(components, state_levels, "geo",
  fit_perturbed$county_calibration$gamma, .07, .15, fit$adjustment$school_reference)
zr <- d$instrument - mean(d$instrument)
zr <- zr / sqrt(sum(zr^2))
null_residual <- d$LHS - calibrated_index(1)$value
null_residual <- null_residual - mean(null_residual)
null_y_perturbed <- d$LHS
null_y_perturbed[s] <- null_y_perturbed[s] + epsilon * null_residual[s]
q0 <- sum(zr * (d$LHS - calibrated_index(1)$value))
q0_perturbed <- sum(zr * (null_y_perturbed - perturbed_index(1)$value))
close_enough((q0_perturbed - q0) / epsilon, fit$stock_wright$score[s], 2e-5)
close_enough(fit$stock_wright$statistic,
  q0^2 / sum(fit$stock_wright$score^2), 1e-7)

# Missing one selected county shifter removes the complete state, rather than
# quietly changing the state composition. Missing an unused shifter is harmless.
incomplete <- components
incomplete$geo[1L] <- NA_real_
fit_missing <- fit_state_adjusted_ch(d, county, incomplete, settings)
stopifnot(fit_missing$nobs == S - 1L, !"1" %in% fit_missing$state_ids,
  fit_missing$county_calibration$nobs == (S - 1L) * C,
  fit_missing$excluded_states$state_id == "1")
hc_settings <- modifyList(settings, list(mode = "human_capital"))
fit_hc <- fit_state_adjusted_ch(d, county, incomplete, hc_settings)
stopifnot(fit_hc$nobs == S, length(fit_hc$adjustment$geo_vars) == 0L,
  all(fit_hc$calibration_state_influence == 0),
  !fit_hc$county_calibration$calibrated)
county_unidentified <- county
county_unidentified$instrument <- 1
fit_hc_unidentified <- fit_state_adjusted_ch(d, county_unidentified, incomplete, hc_settings)
close_enough(fit_hc$full_coefficients, fit_hc_unidentified$full_coefficients)

zero_weight <- incomplete
zero_weight$employment[1L] <- 0
zero_weight$schooling[1L] <- NA_real_
fit_zero_weight <- fit_state_adjusted_ch(d, county, zero_weight, settings)
stopifnot(fit_zero_weight$nobs == S)

# Geography that varies only by state cannot be estimated with county state FE.
collinear_county <- county
collinear_components <- components
collinear_county$geo <- state
collinear_components$geo <- state
error <- tryCatch(fit_state_adjusted_ch(d, collinear_county, collinear_components, settings), error = identity)
stopifnot(inherits(error, "error"), grepl("not separately identified", conditionMessage(error)))

# The OLS path uses the same exact index and propagates calibration uncertainty.
fit_ols <- fit_state_adjusted_ch(d, county, components, modifyList(settings, list(method = "OLS")))
stopifnot(is.finite(fit_ols$theta), is.null(fit_ols$stock_wright),
  all(is.finite(fit_ols$covariance)), max(abs(fit_ols$calibration_state_influence)) > 0)
county_ols_perturbed <- county
county_ols_perturbed$y[state == s] <- county_ols_perturbed$y[state == s] +
  epsilon * fit_ols$county_calibration$residuals[state == s]
d_ols_perturbed <- d
d_ols_perturbed$LHS[s] <- d_ols_perturbed$LHS[s] + epsilon * fit_ols$residuals[s]
fit_ols_perturbed <- fit_state_adjusted_ch(d_ols_perturbed, county_ols_perturbed, components,
  modifyList(settings, list(method = "OLS")))
close_enough((fit_ols_perturbed$full_coefficients - fit_ols$full_coefficients) / epsilon,
  fit_ols$state_influence[s, ], 3e-4)
cat("County-calibrated state CH checks passed.\n")
