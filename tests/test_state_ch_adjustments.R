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
components$region_id <- components$state_id
state_levels <- as.character(seq_len(S))
index <- sca_make_index(components, state_levels, "geo", .04)
d <- data.frame(state_id = seq_len(S), LHS = 10 + index(1.09)$value + rnorm(S, sd = .025),
  instrument = as.vector(rowsum(county_z, state, reorder = FALSE)))
settings <- list(mode = "geography", geo_vars = "geo", county_fe = TRUE)

# The long-form calculation equals the stated CH formula.
t <- 1.13
direct <- vapply(seq_len(S), function(s) {
  a <- components[components$state_id == s, ]
  log(sum(a$employment * exp(.04 * a$geo) *
    (a$employment / a$area)^(t - 1)) / sum(a$employment))
}, numeric(1))
close_enough(index(t)$value, direct)
h <- 1e-5
close_enough(index(t)$derivative, (index(t + h)$value - index(t - h)$value) / (2 * h))
close_enough(index(t)$second_derivative,
  (index(t + h)$derivative - index(t - h)$derivative) / (2 * h))
index_plus <- sca_make_index(components, state_levels, "geo", .04 + h)
index_minus <- sca_make_index(components, state_levels, "geo", .04 - h)
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

# County schooling and college are inactive, including missing values.
unused <- county
unused$schooling <- NA_real_; unused$college <- NA_real_
unused_parts <- components
unused_parts$schooling <- NA_real_; unused_parts$college <- NA_real_
fit_unused <- fit_state_adjusted_ch(d, unused, unused_parts, settings)
close_enough(fit$full_coefficients, fit_unused$full_coefficients)
close_enough(fit$full_covariance, fit_unused$full_covariance)

# Setting both human-capital coefficients to zero recovers geography alone,
# even when the unused education inputs are absent.
both_zero <- fit_state_adjusted_ch(d, unused, unused_parts,
  modifyList(settings, list(mode = "both", beta_school = 0, kappa_college = 0)))
close_enough(fit$full_coefficients, both_zero$full_coefficients)
close_enough(fit$full_covariance, both_zero$full_covariance)

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
  fit$state_influence[as.character(s), ], 2e-5)

# The adjusted-null score has the same shared calibration derivative. At
# theta=1 the geography index must still be subtracted.
calibrated_index <- sca_make_index(components, state_levels, "geo",
  fit$county_calibration$gamma)
perturbed_index <- sca_make_index(components, state_levels, "geo",
  fit_perturbed$county_calibration$gamma)
zr <- d$instrument - mean(d$instrument)
zr <- zr / sqrt(sum(zr^2))
null_residual <- d$LHS - calibrated_index(1)$value
null_residual <- null_residual - mean(null_residual)
null_y_perturbed <- d$LHS
null_y_perturbed[s] <- null_y_perturbed[s] + epsilon * null_residual[s]
q0 <- sum(zr * (d$LHS - calibrated_index(1)$value))
q0_perturbed <- sum(zr * (null_y_perturbed - perturbed_index(1)$value))
close_enough((q0_perturbed - q0) / epsilon, fit$stock_wright$score[match(as.character(s), fit$joint_cluster_levels)], 2e-5)
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
  fit_ols$state_influence[as.character(s), ], 3e-4)
cat("County-calibrated state CH checks passed.\n")

msa_parts <- components
msa_parts$region_id <- rep(seq_len(32), each = 6)
msa_parts$employment <- components$employment
msa_county <- county
msa_county$joint_cluster <- paste0("m", msa_parts$region_id)
# Nonmetro counties in a represented state still belong to the county calibration.
msa_parts <- msa_parts[msa_parts$region_id != 32, ]
msa_index <- sca_make_index(msa_parts, as.character(seq_len(31)), "geo", .04)
msa <- data.frame(region_id = seq_len(31), state_id = rep(seq_len(8), length.out = 31),
  joint_cluster = paste0("m", seq_len(31)), LHS = 10 + msa_index(1.1)$value + rnorm(31, sd=.01),
  instrument = as.vector(rowsum(county_z[seq_len(186)], msa_parts$region_id)))
msa_fit <- fit_state_adjusted_ch(msa, msa_county, msa_parts, modifyList(settings, list(state_fe="state_id")))
stopifnot(msa_fit$nobs == 31L, msa_fit$county_calibration$nobs == 192L,
  "m32" %in% msa_fit$joint_cluster_levels,
  all(msa_fit$direct_state_influence["m32", ] == 0),
  any(abs(msa_fit$calibration_state_influence["m32", ]) > 0))
close_enough(msa_fit$full_covariance, crossprod(msa_fit$state_influence))
msa_incomplete <- msa_parts
msa_incomplete$geo[1L] <- NA_real_
msa_missing <- fit_state_adjusted_ch(msa, msa_county, msa_incomplete,
  modifyList(settings, list(state_fe="state_id")))
stopifnot(msa_missing$nobs == 30L, !"1" %in% msa_missing$region_ids,
  msa_missing$county_calibration$nobs == 192L)
cat("MSA full-constituent-state calibration and shared-cluster checks passed.\n")

# Human capital shifts county productivity inside the additional index. The
# density variable remains log employment per unit of land area.
both_settings <- modifyList(settings, list(mode = "both", beta_school = .07,
  kappa_college = .15, schooling_reference = 0))
both_index <- sca_make_index(components, state_levels, "geo", .04,
  mode = "both", beta_school = .07, kappa_college = .15)
both_direct <- vapply(seq_len(S), function(s) {
  a <- components[components$state_id == s, ]
  log(sum(a$employment * exp(.04 * a$geo + .15 * a$college + .07 * a$schooling) *
    (a$employment / a$area)^(t - 1)) / sum(a$employment))
}, numeric(1))
close_enough(both_index(t)$value, both_direct)
close_enough(both_index(t)$derivative,
  (both_index(t + h)$value - both_index(t - h)$value) / (2 * h))
close_enough(both_index(t)$second_derivative,
  (both_index(t + h)$derivative - both_index(t - h)$derivative) / (2 * h))
both_plus <- sca_make_index(components, state_levels, "geo", .04 + h, "both", .07, .15)
both_minus <- sca_make_index(components, state_levels, "geo", .04 - h, "both", .07, .15)
close_enough(as.vector(both_index(t)$gamma_derivative),
  (both_plus(t)$value - both_minus(t)$value) / (2 * h))
close_enough(as.vector(both_index(t)$mixed_derivative),
  (both_plus(t)$derivative - both_minus(t)$derivative) / (2 * h))

# With one county per region the index reduces to the ordinary county equation.
single_parts <- components[!duplicated(components$state_id), ]
single_index <- sca_make_index(single_parts, state_levels, "geo", .04, "both", .07, .15)
single_log_density <- log(single_parts$employment / single_parts$area)
close_enough(single_index(t)$value, .04 * single_parts$geo + .07 * single_parts$schooling +
  .15 * single_parts$college + (t - 1) * single_log_density)
close_enough(single_index(t)$derivative, single_log_density)

both_d <- d
both_d$LHS <- 10 + both_index(1.09)$value + rnorm(S, sd = .025)
both_fit <- fit_state_adjusted_ch(both_d, county, components, both_settings)
stopifnot(both_fit$nobs == S, both_fit$adjustment$mode == "both",
  both_fit$adjustment$beta_school == .07, both_fit$adjustment$kappa_college == .15,
  abs(both_fit$iv_moment) < 1e-8,
  both_fit$stock_wright$status == "available",
  abs(both_fit$county_calibration$gamma - fit$county_calibration$gamma) > 1e-5)

# The county calibration uses the human-capital-adjusted outcome and ordinary
# log density, with geography effects reestimated for that specification.
manual_county <- county
manual_county$joint_cluster <- manual_county$state_id
manual_county$y <- county$y - .07 * county$schooling - .15 * county$college
manual_county$x <- county$x
manual_calibration <- sca_county_fit(manual_county, "geo", character(), "IV", TRUE)
close_enough(both_fit$county_calibration$coefficients, manual_calibration$coefficients)
close_enough(both_fit$county_calibration$covariance, manual_calibration$covariance)
close_enough(both_fit$full_covariance, crossprod(both_fit$state_influence))

# Changing the common schooling reference changes only the aggregate intercept.
centered_index <- sca_make_index(components, state_levels, "geo", .04, "both", .07, .15, 12)
close_enough(centered_index(t)$value, both_index(t)$value - .07 * 12)
close_enough(centered_index(t)$derivative, both_index(t)$derivative)
centered_fit <- fit_state_adjusted_ch(both_d, county, components,
  modifyList(both_settings, list(schooling_reference = 12)))
close_enough(both_fit$theta, centered_fit$theta)
close_enough(both_fit$county_calibration$gamma, centered_fit$county_calibration$gamma)
close_enough(both_fit$covariance["ch_elasticity", "ch_elasticity"],
  centered_fit$covariance["ch_elasticity", "ch_elasticity"])
close_enough(centered_fit$full_coefficients["(Intercept)"],
  both_fit$full_coefficients["(Intercept)"] + .07 * 12)

# Missing schooling or college in one positive-employment county removes its
# whole region only from the combined model. Zero-employment counties need no HC.
for (variable in c("schooling", "college")) {
  missing_hc <- components
  missing_hc[[variable]][1] <- NA_real_
  hc_missing_fit <- fit_state_adjusted_ch(both_d, county, missing_hc, both_settings)
  stopifnot(hc_missing_fit$nobs == S - 1L, !"1" %in% hc_missing_fit$region_ids)
  missing_hc$employment[1] <- 0
  hc_zero_fit <- fit_state_adjusted_ch(both_d, county, missing_hc, both_settings)
  stopifnot(hc_zero_fit$nobs == S)
}
missing_county_hc <- county
missing_county_hc$schooling[1] <- NA_real_
calibration_missing <- fit_state_adjusted_ch(both_d, missing_county_hc, components, both_settings)
stopifnot(calibration_missing$nobs == S, calibration_missing$county_calibration$nobs == S * C - 1L)

# Joint uncertainty must retain the county calibration effect in both IV and
# nonlinear least squares. Perturb one shared cluster in both regressions.
for (method in c("IV", "OLS")) {
  current_settings <- modifyList(both_settings, list(method = method))
  current_fit <- fit_state_adjusted_ch(both_d, county, components, current_settings)
  altered_county <- county
  altered_county$y[state == s] <- county$y[state == s] +
    epsilon * current_fit$county_calibration$residuals[state == s]
  altered_data <- both_d
  altered_data$LHS[s] <- both_d$LHS[s] + epsilon * current_fit$residuals[s]
  altered_fit <- fit_state_adjusted_ch(altered_data, altered_county, components, current_settings)
  close_enough((altered_fit$full_coefficients - current_fit$full_coefficients) / epsilon,
    current_fit$state_influence[as.character(s), ], if (method == "IV") 2e-5 else 3e-4)
  if (method == "IV") {
    null_index <- sca_make_index(components, state_levels, "geo", current_fit$county_calibration$gamma,
      "both", .07, .15)
    changed_null_index <- sca_make_index(components, state_levels, "geo", altered_fit$county_calibration$gamma,
      "both", .07, .15)
    null_y <- both_d$LHS
    null_residual <- null_y - null_index(1)$value
    null_residual <- null_residual - mean(null_residual)
    null_y[s] <- null_y[s] + epsilon * null_residual[s]
    original_moment <- sum(zr * (both_d$LHS - null_index(1)$value))
    changed_moment <- sum(zr * (null_y - changed_null_index(1)$value))
    close_enough((changed_moment - original_moment) / epsilon,
      current_fit$stock_wright$score[match(as.character(s), current_fit$joint_cluster_levels)], 2e-5)
    close_enough(current_fit$stock_wright$statistic,
      original_moment^2 / sum(current_fit$stock_wright$score^2))
  }
}

# The same combined calculation works with MSA fixed effects, cross-state
# membership, and calibration counties outside the retained MSAs.
msa_both_index <- sca_make_index(msa_parts, as.character(seq_len(31)), "geo", .04, "both", .07, .15)
msa_both_data <- msa
msa_both_data$LHS <- 10 + msa_both_index(1.1)$value + rnorm(31, sd = .01)
msa_both_settings <- modifyList(both_settings, list(state_fe = "state_id"))
msa_both_fit <- fit_state_adjusted_ch(msa_both_data, msa_county, msa_parts, msa_both_settings)
stopifnot(msa_both_fit$nobs == 31L, msa_both_fit$county_calibration$nobs == 192L,
  "m32" %in% msa_both_fit$joint_cluster_levels,
  all(msa_both_fit$direct_state_influence["m32", ] == 0),
  any(abs(msa_both_fit$calibration_state_influence["m32", ]) > 0))
close_enough(msa_both_fit$full_covariance, crossprod(msa_both_fit$state_influence))
msa_both_missing <- msa_parts
msa_both_missing$college[1] <- NA_real_
msa_both_missing_fit <- fit_state_adjusted_ch(msa_both_data, msa_county, msa_both_missing, msa_both_settings)
stopifnot(msa_both_missing_fit$nobs == 30L, !"1" %in% msa_both_missing_fit$region_ids)
cat("Combined county geography, schooling and college CH checks passed.\n")
