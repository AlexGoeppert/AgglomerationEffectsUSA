# County geography effects enter before MSA or state CH aggregation.
# The combined mode also places fixed schooling and college effects in the index.

sca_design <- function(data, controls = character(), fe = NULL, prefix = ".fe_") {
  if (is.null(fe) || identical(fe, "0")) {
    X <- matrix(1, nrow(data), 1L, dimnames = list(NULL, "(Intercept)"))
  } else {
    f <- factor(as.character(data[[fe]]))
    X <- diag(nlevels(f))[as.integer(f), , drop = FALSE]
    colnames(X) <- paste0(prefix, levels(f))
  }
  if (length(controls)) X <- cbind(X, as.matrix(data[controls]))
  storage.mode(X) <- "double"
  scales <- sqrt(colSums(X^2))
  scales[scales == 0] <- 1
  Xs <- sweep(X, 2L, scales, "/")
  q <- qr(Xs, tol = 1e-10)
  if (q$rank != ncol(X)) {
    missing <- colnames(X)[q$pivot[seq.int(q$rank + 1L, ncol(X))]]
    stop(paste("The selected county/state controls are not separately identified:",
               paste(missing, collapse = ", "),
               "Remove redundant controls; the omitted geography effect cannot be assigned a zero value."))
  }
  list(X = X, Xs = Xs, scales = scales, qr = q)
}

sca_complete <- function(data, variables) {
  absent <- setdiff(variables, names(data))
  if (length(absent)) stop(paste("County-calibrated CH inputs are missing:", paste(absent, collapse = ", ")))
  ok <- rep(TRUE, nrow(data))
  for (v in unique(variables)) {
    a <- data[[v]]
    ok <- ok & if (is.numeric(a)) is.finite(a) else !is.na(a) & nzchar(trimws(as.character(a)))
  }
  ok[is.na(ok)] <- FALSE
  ok
}

sca_cluster_sum <- function(value, cluster, levels) {
  value <- as.matrix(value)
  result <- matrix(0, length(levels), ncol(value), dimnames = list(levels, colnames(value)))
  if (!ncol(value)) return(result)
  sums <- rowsum(value, as.character(cluster), reorder = FALSE)
  result[match(rownames(sums), levels), ] <- sums
  result
}

sca_relevance <- function(response, instrument, W, cluster = NULL, theta = NA_real_) {
  zr <- qr.resid(W$qr, instrument)
  dr <- qr.resid(W$qr, response)
  zz <- sum(zr^2); dd <- sum(dr^2)
  groups <- if (is.null(cluster)) length(response) else length(unique(cluster))
  unavailable <- function(reason) list(statistic = NA_real_, partial_r2 = NA_real_,
    coefficient = NA_real_, std_error = NA_real_, nobs = length(response), clusters = groups,
    covariance = "CR0", evaluated_theta = theta, status = "unavailable", reason = reason,
    conditional_on_calibration = TRUE)
  if (!is.finite(zz) || zz <= 1e-20 || dd <= 1e-20)
    return(unavailable("No residual variation remains in the instrument or fitted index derivative."))
  b <- sum(zr * dr) / zz
  score <- zr * (dr - b * zr) / zz
  sums <- if (is.null(cluster)) score else as.numeric(rowsum(score, cluster, reorder = FALSE))
  variance <- sum(sums^2)
  if (!is.finite(variance) || variance <= 0) return(unavailable("The auxiliary variance is zero."))
  list(statistic = b^2 / variance, partial_r2 = min(1, max(0, sum(zr * dr)^2 / zz / dd)),
       coefficient = b, std_error = sqrt(variance), nobs = length(response), clusters = groups,
       covariance = "CR0", evaluated_theta = theta, status = "available", reason = "",
       conditional_on_calibration = TRUE,
       note = "Relevance of the fitted CH derivative, conditional on the fitted parameters; not a weak-instrument test.")
}

sca_county_fit <- function(county, geo_vars, nuisance, method, county_fe = TRUE) {
  required <- c("county_id", "state_id", "joint_cluster", "y", "x", geo_vars, nuisance,
                if (method == "IV") "instrument")
  county <- county[sca_complete(county, required), , drop = FALSE]
  if (anyDuplicated(as.character(county$county_id))) stop("County calibration contains repeated county identifiers.")
  if (!nrow(county)) stop("No counties have complete inputs for the linked calibration.")
  y <- county$y
  x <- county$x
  W <- sca_design(county, unique(c(geo_vars, nuisance)), if (county_fe) "state_id" else NULL)
  X <- cbind(county_elasticity = x, W$X)
  scales <- sqrt(colSums(X^2)); scales[scales == 0] <- 1
  Xs <- sweep(X, 2L, scales, "/")
  if (nrow(X) <= ncol(X) || qr(Xs, tol = 1e-10)$rank != ncol(X))
    stop("The county calibration has too few observations or no density variation after controls and state fixed effects.")
  if (method == "IV") {
    zscale <- max(abs(county$instrument))
    if (!is.finite(zscale) || zscale == 0) stop("The county population instrument has no variation.")
    Z <- cbind(county$instrument / zscale, W$Xs)
  } else Z <- Xs
  bread <- crossprod(Z, Xs)
  if (rcond(bread) < 1e-13) stop("The county calibration is not identified by the selected population instrument and controls.")
  inverse <- solve(bread)
  coefficient <- as.vector(inverse %*% crossprod(Z, y)) / scales
  names(coefficient) <- colnames(X)
  residual <- y - as.vector(X %*% coefficient)
  influence <- sweep((Z * residual) %*% t(inverse), 2L, scales, "/")
  colnames(influence) <- names(coefficient)
  state_levels <- sort(unique(as.character(county$state_id)))
  cluster_levels <- sort(unique(as.character(county$joint_cluster)))
  cluster_influence <- sca_cluster_sum(influence, county$joint_cluster, cluster_levels)
  covariance <- crossprod(cluster_influence)
  gamma <- coefficient[geo_vars]
  gamma_influence <- cluster_influence[, geo_vars, drop = FALSE]
  list(calibrated = TRUE, coefficients = coefficient, covariance = covariance, gamma = gamma,
       gamma_influence = gamma_influence, influence = cluster_influence,
       residuals = residual, nobs = nrow(county), states = state_levels,
       county_ids = as.character(county$county_id), method = method,
       geo_vars = geo_vars, nuisance = nuisance,
       observation_influence = influence, joint_cluster = county$joint_cluster,
       county_fe = county_fe,
       instrument_relevance = if (method == "IV")
         sca_relevance(x, county$instrument, W, county$joint_cluster) else NULL)
}

sca_human_capital <- function(data, mode, beta_school, kappa_college, schooling_reference) {
  h <- college_effect <- rep(0, nrow(data))
  if (mode == "both") {
    required <- c(if (beta_school != 0) "schooling", if (kappa_college != 0) "college")
    sca_complete(data, required)
    if (beta_school != 0) h <- beta_school * (data$schooling - schooling_reference)
    if (kappa_college != 0) college_effect <- kappa_college * data$college
  }
  list(h = h, college_effect = college_effect)
}

sca_make_index <- function(components, state_levels, geo_vars, gamma, mode = "geography",
                           beta_school = .10262306034564972, kappa_college = .75,
                           schooling_reference = 0) {
  # Zero-employment counties remain in the membership check, but have no
  # contribution to the index and need no productivity shifter.
  components <- components[components$employment > 0, , drop = FALSE]
  group <- match(as.character(components$region_id), state_levels)
  ordering <- order(group)
  components <- components[ordering, , drop = FALSE]
  group <- group[ordering]
  n <- components$employment; a <- components$area
  positive <- n > 0
  x <- rep(0, length(n)); x[positive] <- log(n[positive] / a[positive])
  G <- if (length(geo_vars)) as.matrix(components[geo_vars]) else matrix(0, nrow(components), 0L)
  geo <- if (length(geo_vars)) as.vector(G %*% gamma) else rep(0, length(n))
  hc <- sca_human_capital(components, mode, beta_school, kappa_college, schooling_reference)
  base <- rep(-Inf, length(n))
  base[positive] <- log(n[positive]) + geo[positive] + hc$h[positive] + hc$college_effect[positive]
  q <- x + hc$h
  total <- as.vector(rowsum(n, group, reorder = FALSE))
  if (any(total <= 0) || length(total) != length(state_levels)) stop("A region has no complete positive-employment county components.")
  function(theta) {
    logw <- base + (theta - 1) * q
    largest <- as.vector(tapply(logw, group, max))
    w <- exp(logw - largest[group])
    denominator <- as.vector(rowsum(w, group, reorder = FALSE))
    p <- w / denominator[group]
    derivative <- as.vector(rowsum(p * q, group, reorder = FALSE))
    second <- as.vector(rowsum(p * (q - derivative[group])^2, group, reorder = FALSE))
    K <- if (length(geo_vars)) rowsum(G * p, group, reorder = FALSE) else matrix(0, length(state_levels), 0L)
    mixed <- if (length(geo_vars)) rowsum(G * (p * (q - derivative[group])), group, reorder = FALSE) else K
    colnames(K) <- colnames(mixed) <- geo_vars
    list(value = largest + log(denominator) - log(total), derivative = derivative,
         second_derivative = second, gamma_derivative = K, mixed_derivative = mixed)
  }
}

sca_s_test <- function(y, z, W, terms, gamma_influence, cluster, cluster_levels) {
  zr <- qr.resid(W$qr, z)
  residual <- qr.resid(W$qr, y - terms$value)
  moment <- sum(zr * residual)
  derivative <- -as.vector(crossprod(zr, terms$gamma_derivative))
  score <- as.vector(sca_cluster_sum(zr * residual, cluster, cluster_levels))
  if (length(derivative)) score <- score + as.vector(gamma_influence %*% derivative)
  variance <- sum(score^2)
  available <- is.finite(variance) && variance > 1e-25 && sum(zr^2) > 1e-20
  statistic <- if (available) moment^2 / variance else NA_real_
  list(statistic = statistic, p_value = if (available) pchisq(statistic, 1L, lower.tail = FALSE) else NA_real_,
       df = 1L, null_elasticity = 0, nobs = length(y), clusters = length(cluster_levels),
       status = if (available) "available" else "unavailable",
       reason = if (available) "" else "The adjusted null has no estimable moment variance.",
       calibration_uncertainty = ncol(gamma_influence) > 0L, moment = moment, score = score,
       note = if (ncol(gamma_influence) > 0L)
         "Adjusted-null S test includes the county geography calibration; validity also requires a regularly identified county calibration." else
         "The zero-elasticity S test uses the geography-adjusted index at theta = 1.")
}

fit_state_adjusted_ch <- function(data, county, components, settings = list()) {
  defaults <- list(dep_var = "LHS", method = "IV", mode = "geography", geo_vars = character(),
    county_fe = TRUE, county_controls = character(), state_controls = character(), state_fe = "0",
    se_spec = "cluster_state", county_instrument_label = "Area-weighted historical population",
    beta_school = .10262306034564972, kappa_college = .75, schooling_reference = 0)
  for (name in names(defaults)) if (is.null(settings[[name]])) settings[[name]] <- defaults[[name]]
  method <- match.arg(settings$method, c("IV", "OLS"))
  mode <- match.arg(settings$mode, c("geography", "both"))
  for (name in c("beta_school", "kappa_college", "schooling_reference")) {
    value <- settings[[name]]
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value))
      stop(paste("The fixed human-capital setting must be one finite number:", name))
  }
  beta_school <- if (mode == "both") settings$beta_school else 0
  kappa_college <- if (mode == "both") settings$kappa_college else 0
  hc_vars <- c(if (beta_school != 0) "schooling", if (kappa_college != 0) "college")
  geo_vars <- unique(settings$geo_vars)
  if (!length(geo_vars)) stop("Select at least one geography or transport control for the county adjustment.")
  if (!"region_id" %in% names(data)) data$region_id <- data$state_id
  if (!"region_id" %in% names(components)) components$region_id <- components$state_id
  if (!"joint_cluster" %in% names(data)) data$joint_cluster <- data$state_id
  if (!"joint_cluster" %in% names(county)) county$joint_cluster <- county$state_id
  if (anyDuplicated(as.character(data$region_id))) stop("CH estimation requires one row per region.")
  if (anyDuplicated(paste(components$region_id, components$county_id, sep = ":")))
    stop("CH components contain repeated county identifiers within a region.")
  required <- c(settings$dep_var, "region_id", "joint_cluster", settings$state_controls,
    if (settings$state_fe != "0") settings$state_fe, if (method == "IV") "instrument")
  valid <- sca_complete(data, required)
  if ("ch_complete" %in% names(data)) valid <- valid & !is.na(data$ch_complete) & data$ch_complete == 1
  complete_component <- sca_complete(components, c("region_id", "state_id", "county_id", "employment", "area")) &
    components$employment >= 0 & components$area > 0 &
    (components$employment == 0 | sca_complete(components, c(geo_vars, hc_vars)))
  bad_states <- unique(as.character(components$region_id[!complete_component]))
  available_states <- unique(as.character(components$region_id))
  valid <- valid & as.character(data$region_id) %in% setdiff(available_states, bad_states)
  valid[is.na(valid)] <- FALSE
  rows <- which(valid)
  d <- data[rows, , drop = FALSE]
  if (nrow(d) < 3L) stop("Fewer than three regions have complete components and selected adjustment inputs.")
  state_levels <- as.character(d$region_id)
  components <- components[as.character(components$region_id) %in% state_levels, , drop = FALSE]
  # All constituent states contribute calibration counties, including nonmetro counties.
  county <- county[as.character(county$state_id) %in% unique(as.character(components$state_id)), , drop = FALSE]
  hc <- sca_human_capital(county, mode, beta_school, kappa_college, settings$schooling_reference)
  county$y <- county$y - hc$h - hc$college_effect
  county$x <- county$x + hc$h
  calibration <- sca_county_fit(county, geo_vars, settings$county_controls, method, isTRUE(settings$county_fe))
  calibration$human_capital <- list(mode = mode, beta_school = beta_school,
    kappa_college = kappa_college, schooling_reference = settings$schooling_reference,
    coefficients_fixed = TRUE)
  cluster_levels <- sort(unique(c(as.character(d$joint_cluster), rownames(calibration$gamma_influence))))
  if (length(unique(d$joint_cluster)) < 3L)
    stop("Fewer than three independent aggregate clusters remain. Choose a wider sample or a different clustering specification.")
  gamma_influence <- matrix(0, length(cluster_levels), length(geo_vars), dimnames = list(cluster_levels, geo_vars))
  gamma_influence[match(rownames(calibration$gamma_influence), cluster_levels), ] <- calibration$gamma_influence
  index <- sca_make_index(components, state_levels, geo_vars, calibration$gamma, mode,
    beta_school, kappa_college, settings$schooling_reference)
  W <- sca_design(d, settings$state_controls, if (settings$state_fe == "0") NULL else settings$state_fe)
  y <- d[[settings$dep_var]]
  n <- length(y); k <- ncol(W$X) + 1L
  if (n <= k) stop("Too few regions remain after aggregate controls or fixed effects.")
  partial <- function(v) qr.resid(W$qr, v)
  yr <- partial(y)
  zr <- NULL
  stock_wright <- NULL
  if (method == "IV") {
    zscale <- max(abs(d$instrument))
    if (!is.finite(zscale) || zscale == 0) stop("The historical aggregate instrument has no variation.")
    z <- d$instrument / zscale
    zr <- partial(z)
    if (sqrt(sum(zr^2)) <= 1e-10 * sqrt(sum(z^2))) stop("The aggregate instrument has no variation after the selected controls.")
    zr <- zr / sqrt(sum(zr^2))
    stock_wright <- sca_s_test(y, zr, W, index(1), gamma_influence, d$joint_cluster, cluster_levels)
  }
  fail <- function(message) stop(errorCondition(message, class = "ch_fit_error", stock_wright = stock_wright))
  objective <- function(theta) sum((yr - partial(index(theta)$value))^2)
  grid <- sort(unique(c(1e-7, seq(0.05, 3, .05), 4, 8, 16, 32)))
  best <- which.min(vapply(grid, objective, numeric(1)))
  boundary <- best %in% c(1L, length(grid))
  if (boundary && method == "OLS") fail("The adjusted CH least-squares solution is at the search boundary.")
  theta <- if (boundary) grid[best] else optimize(objective, grid[c(best - 1L, best + 1L)], tol = 1e-11)$minimum
  roots <- theta
  if (method == "IV") {
    moment <- function(t) sum(zr * (yr - partial(index(t)$value)))
    moment_derivative <- function(t) -sum(zr * partial(index(t)$derivative))
    grid <- sort(unique(c(grid, seq(.01, 3, .01), seq(3.05, 8, .05), seq(8.2, 32, .2))))
    deriv <- vapply(grid, moment_derivative, numeric(1))
    turns <- which(head(deriv, -1L) * tail(deriv, -1L) < 0)
    if (length(turns)) grid <- sort(unique(c(grid, vapply(turns, function(j)
      uniroot(moment_derivative, grid[c(j, j + 1L)], tol = 1e-11)$root, numeric(1)))))
    values <- vapply(grid, moment, numeric(1))
    crossings <- which(head(values, -1L) * tail(values, -1L) <= 0)
    if (!length(crossings)) fail("No positive-theta adjusted CH IV solution was found between 0 and 32.")
    roots <- vapply(crossings, function(j) uniroot(moment, grid[c(j, j + 1L)], tol = 1e-11)$root, numeric(1))
    roots <- roots[!duplicated(round(roots, 8L))]
    if (length(roots) > 1L) warning("Adjusted CH IV has multiple solutions; the solution closest to the least-squares estimate is displayed.", call. = FALSE)
    theta <- roots[which.min(abs(roots - theta))]
  }
  terms <- index(theta)
  beta <- qr.coef(W$qr, y - terms$value) / W$scales
  fitted <- terms$value + as.vector(W$X %*% beta)
  residual <- y - fitted
  J <- cbind(ch_elasticity = terms$derivative, W$X)
  scales <- sqrt(colSums(J^2)); scales[scales == 0] <- 1
  Js <- sweep(J, 2L, scales, "/")
  if (qr(Js, tol = 1e-10)$rank != k) fail("The adjusted CH elasticity is not identified separately from the selected aggregate controls.")
  if (method == "IV") {
    Z <- cbind(zr, W$Xs)
    bread <- crossprod(Z, Js)
    gamma_bread <- crossprod(Z, terms$gamma_derivative)
    scores <- Z * residual
  } else {
    bread <- crossprod(Js)
    bread[1L, 1L] <- bread[1L, 1L] - sum(residual * terms$second_derivative) / scales[1L]^2
    gamma_bread <- crossprod(Js, terms$gamma_derivative)
    if (length(geo_vars)) gamma_bread[1L, ] <- gamma_bread[1L, ] -
      as.vector(crossprod(residual, terms$mixed_derivative)) / scales[1L]
    scores <- Js * residual
  }
  if (rcond(bread) < 1e-12) fail("The adjusted CH derivative is too weak for a reliable Wald covariance matrix.")
  inverse <- solve(bread)
  direct_influence <- sca_cluster_sum(sweep(scores %*% t(inverse), 2L, scales, "/"), d$joint_cluster, cluster_levels)
  calibration_influence <- if (length(geo_vars))
    sweep((-gamma_influence %*% t(gamma_bread)) %*% t(inverse), 2L, scales, "/") else matrix(0, length(cluster_levels), k)
  influence <- direct_influence + calibration_influence
  colnames(influence) <- colnames(J)
  full_vcov <- crossprod(influence)
  dimnames(full_vcov) <- list(colnames(J), colnames(J))
  full_coef <- c(ch_elasticity = theta - 1, beta)
  exposed <- !startsWith(names(full_coef), ".fe_")
  instrument_relevance <- if (method == "IV") sca_relevance(terms$derivative, d$instrument, W, d$joint_cluster, theta) else NULL
  excluded <- data.frame(state_id = as.character(data$region_id[!valid]),
    reason = ifelse(as.character(data$region_id[!valid]) %in% bad_states,
      "At least one county component is missing a selected adjustment input.",
      ifelse(!as.character(data$region_id[!valid]) %in% available_states,
        "Complete county components are unavailable.", "The aggregate outcome, instrument or selected aggregate controls are missing.")), stringsAsFactors = FALSE)
  result <- list(coefficients = full_coef[exposed], covariance = full_vcov[exposed, exposed, drop = FALSE],
    theta = theta, residuals = residual, fitted.values = fitted, rows = rows,
    nobs = as.integer(n), df.residual = Inf, parameter_count = k, method = method,
    se_spec = settings$se_spec, clusters = as.integer(length(unique(d$joint_cluster))), collin.var = character(), singleton_n = 0L,
    stock_wright = stock_wright, anderson_rubin = NULL, instrument_relevance = instrument_relevance,
    inference = paste("Joint CR0 covariance includes county geography estimation and shared county/aggregate observations; asymptotic normal inference.",
      if (mode == "both") "Schooling and college effects enter the county index with fixed coefficients; their calibration uncertainty is not estimated."
      else "Human capital, when selected, adjusts only the aggregate outcome."),
    full_coefficients = full_coef, full_covariance = full_vcov,
    index_value = terms$value, index_derivative = terms$derivative,
    objective = sum(residual^2), iv_moment = if (method == "IV") moment(theta) else NULL,
    county_calibration = calibration, excluded_states = excluded,
    adjustment = list(mode = mode, geo_vars = geo_vars, beta_school = beta_school,
      kappa_college = kappa_college, schooling_reference = settings$schooling_reference,
      human_capital_coefficients_fixed = TRUE,
      county_instrument_label = settings$county_instrument_label,
      county_controls_not_aggregated = calibration$nuisance,
      county_fe_not_aggregated = calibration$county_fe),
    state_ids = state_levels, region_ids = state_levels, joint_cluster_levels = cluster_levels, state_influence = influence,
    direct_state_influence = direct_influence, calibration_state_influence = calibration_influence,
    index_gamma_derivative = terms$gamma_derivative, null_index_value = index(1)$value,
    roots = roots)
  class(result) <- "ch_model"
  result
}
