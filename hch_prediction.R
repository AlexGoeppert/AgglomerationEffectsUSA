# Historical CH instruments and county predictions use the same county moments.
get_hch_county_moments <- local({
  cached <- NULL
  signature <- NULL
  function(path = "State_HCH_county_moments.dta") {
    if (!file.exists(path)) stop("Historical CH county moments are unavailable. Run the State data build.")
    info <- file.info(path)
    current <- paste(normalizePath(path, winslash = "/"), info$size, as.numeric(info$mtime), sep = "|")
    if (!identical(current, signature)) {
      value <- as.data.frame(haven::read_dta(path))
      required <- c("geofips", "statefips", "historical_year", "modern_area_km2",
        "pop", "numer", "known_area_km2", "pop_s", "numer_s", "known_area_km2_s")
      if (!all(required %in% names(value)) || anyDuplicated(value[c("geofips", "historical_year")]))
        stop("Historical CH county moments have missing fields or repeated counties.")
      cached <<- value
      signature <<- current
    }
    cached
  }
})

hch_prediction_year <- function(county_moments, modern_components, geo_data = NULL,
                                historical_year, scope = "states_territories",
                                basis = "source", variant = "historical",
                                geo_vars = character(), minimum = .80) {
  choice <- function(value, allowed, description) {
    if (length(value) != 1L || is.na(value) || !value %in% allowed)
      stop(paste("Choose a valid", description, "."))
    as.character(value)
  }
  scope <- choice(scope, c("states_only", "states_territories"), "historical scope")
  basis <- choice(basis, c("source", "modern"), "historical density basis")
  variant <- choice(variant, c("historical", "a", "b", "ab"), "historical CH variant")
  if (length(historical_year) != 1L || !is.finite(historical_year) ||
      !historical_year %in% seq(1790, 1900, 10)) stop("Choose a historical census year from 1790 to 1900.")
  if (length(minimum) != 1L || !is.finite(minimum) || !minimum %in% c(0, .80, .95))
    stop("Historical CH coverage must be 80%, 95%, or No coverage filter.")
  suffix <- if (scope == "states_only") "_s" else ""
  required <- c("geofips", "statefips", "historical_year", "modern_area_km2",
    paste0(c("pop", "numer", "known_area_km2"), suffix))
  if (!all(required %in% names(county_moments))) stop("Historical CH county moment fields are missing.")
  d <- as.data.frame(county_moments[county_moments$historical_year == historical_year, required, drop = FALSE])
  if (!nrow(d) || anyNA(d$geofips) || anyNA(d$statefips) || anyDuplicated(d$geofips))
    stop("Historical CH needs one identified row per modern county unit.")
  names(d)[match(paste0(c("pop", "numer", "known_area_km2"), suffix), names(d))] <-
    c("population", "numerator", "known_area_km2")
  for (v in c("population", "numerator", "known_area_km2", "modern_area_km2"))
    if (!is.numeric(d[[v]])) stop(paste("Historical CH field must be numeric:", v))
  if (any(!is.finite(d$modern_area_km2) | d$modern_area_km2 <= 0))
    stop("Historical CH needs the full modern polygon area for every county unit.")
  if (any(is.finite(d$population) & d$population < 0) ||
      any(is.finite(d$known_area_km2) & d$known_area_km2 < 0))
    stop("Historical CH population and known area cannot be negative.")
  if (any(is.finite(d$population) & d$population == 0 &
          is.finite(d$numerator) & d$numerator != 0))
    stop("Zero historical population must have a zero historical density numerator.")
  d <- d[order(d$statefips, d$geofips), , drop = FALSE]
  rownames(d) <- NULL
  d$land_area_km2 <- NA_real_
  d$employment <- NA_real_
  prediction <- variant != "historical"
  if (prediction || basis == "modern") {
    needed <- c("geofips", "statefips", "nc", "ac")
    if (!all(needed %in% names(modern_components)) || anyDuplicated(modern_components$geofips))
      stop("Modern CH components need one row per county with employment and land area.")
    member <- match(d$geofips, modern_components$geofips)
    found <- !is.na(member)
    if (any(found & d$statefips != modern_components$statefips[member], na.rm = TRUE))
      stop("Historical and modern county units disagree on their state.")
    d$land_area_km2 <- as.numeric(modern_components$ac[member]) * 4046.8 / 1e6
    d$employment <- as.numeric(modern_components$nc[member])
  }
  moments_known <- is.finite(d$population) & d$population >= 0 &
    is.finite(d$numerator) & is.finite(d$known_area_km2)
  positive <- moments_known & d$population > 0
  d$historical_log_density <- NA_real_
  if (basis == "source") d$historical_log_density[positive] <-
    d$numerator[positive] / d$population[positive]
  else {
    positive <- positive & is.finite(d$land_area_km2) & d$land_area_km2 > 0
    d$historical_log_density[positive] <- log(d$population[positive]) - log(d$land_area_km2[positive])
  }
  positive <- positive & is.finite(d$historical_log_density)
  d$predicted_log_density <- NA_real_
  d$training <- FALSE
  d$support <- FALSE
  d$coverage_support <- FALSE
  d$normalized_weight <- 0
  d$log_weight <- NA_real_
  coefficients <- numeric()
  rank <- 0L
  n_training <- NA_integer_
  dropped <- character()
  training_states <- numeric()
  rmse <- NA_real_
  if (!prediction) {
    # A known zero population contributes zero and still contributes known area.
    d$support <- positive
    d$coverage_support <- moments_known & (d$population == 0 | positive)
    if (basis == "modern") d$coverage_support <- d$coverage_support &
      is.finite(d$land_area_km2) & d$land_area_km2 > 0
    geo_vars <- character()
  } else {
    geo_vars <- unique(as.character(geo_vars))
    if (anyNA(geo_vars) || any(!nzchar(geo_vars)) ||
        any(geo_vars %in% c("(Intercept)", "historical_log_density")))
      stop("County prediction geography variable names are invalid.")
    X <- cbind("(Intercept)" = 1, historical_log_density = d$historical_log_density)
    geo_complete <- rep(TRUE, nrow(d))
    if (length(geo_vars)) {
      if (is.null(geo_data) || !all(c("geofips", geo_vars) %in% names(geo_data)) ||
          anyDuplicated(geo_data$geofips)) stop("County prediction geography inputs are missing or repeated.")
      if (any(!vapply(geo_data[geo_vars], is.numeric, logical(1))))
        stop("County prediction geography inputs must be numeric.")
      geo <- as.matrix(geo_data[match(d$geofips, geo_data$geofips), geo_vars, drop = FALSE])
      geo_complete <- rowSums(!is.finite(geo)) == 0L
      X <- cbind(X, geo)
    }
    candidates <- positive & geo_complete & is.finite(d$land_area_km2) & d$land_area_km2 > 0
    y <- rep(NA_real_, nrow(d))
    observed <- is.finite(d$employment) & d$employment > 0 &
      is.finite(d$land_area_km2) & d$land_area_km2 > 0
    y[observed] <- log(d$employment[observed]) - log(d$land_area_km2[observed])
    train <- candidates & is.finite(y)
    n_training <- sum(train)
    if (n_training < 3L) stop("Too few counties have complete inputs for the density prediction.")
    # Scaling improves numerical conditioning; coefficients below use original units.
    center <- colMeans(X[train, , drop = FALSE])
    center[1L] <- 0
    spread <- sqrt(colSums(sweep(X[train, , drop = FALSE], 2, center, "-")^2) / (n_training - 1))
    spread[1L] <- 1
    spread[!is.finite(spread) | spread == 0] <- 1
    scaled <- sweep(sweep(X, 2, center, "-"), 2, spread, "/")
    fit <- stats::lm.fit(scaled[train, , drop = FALSE], y[train], tol = 1e-10)
    rank <- fit$rank
    if (n_training <= rank) stop("The county density prediction needs more observations than estimated coefficients.")
    fitted_coefficients <- fit$coefficients
    dropped <- names(fitted_coefficients)[is.na(fitted_coefficients)]
    fitted_coefficients[is.na(fitted_coefficients)] <- 0
    coefficients <- fitted_coefficients / spread
    coefficients[1L] <- fitted_coefficients[1L] - sum(coefficients[-1L] * center[-1L])
    d$predicted_log_density[candidates] <- as.vector(scaled[candidates, , drop = FALSE] %*% fitted_coefficients)
    estimable <- candidates
    if (rank < ncol(scaled)) {
      # A prediction must respect dependencies found in the training regressors.
      retained <- fit$qr$pivot[seq_len(rank)]
      map <- qr.coef(qr(scaled[train, retained, drop = FALSE], tol = 1e-10), scaled[train, , drop = FALSE])
      possible <- which(candidates)
      reconstructed <- scaled[possible, retained, drop = FALSE] %*% map
      discrepancy <- apply(abs(reconstructed - scaled[possible, , drop = FALSE]), 1, max)
      scale <- 1 + apply(abs(scaled[possible, , drop = FALSE]), 1, max)
      estimable[possible] <- is.finite(discrepancy) & discrepancy <= 1e-7 * scale
    }
    d$training <- train
    d$support <- estimable & is.finite(d$predicted_log_density)
    d$coverage_support <- d$support
    training_states <- sort(unique(d$statefips[train]))
    rmse <- sqrt(mean(fit$residuals^2))
  }
  supported <- which(d$support)
  if (variant %in% c("b", "ab"))
    d$log_weight[supported] <- log(d$land_area_km2[supported]) + d$predicted_log_density[supported]
  else d$log_weight[supported] <- log(d$population[supported])
  contribution <- if (variant %in% c("a", "ab")) d$predicted_log_density else d$historical_log_density
  rows <- lapply(sort(unique(d$statefips)), function(state) {
    all <- which(d$statefips == state)
    used <- all[d$support[all] & is.finite(d$log_weight[all]) & is.finite(contribution[all])]
    total_area <- sum(d$modern_area_km2[all])
    covered_area <- sum(d$known_area_km2[all[d$coverage_support[all]]])
    coverage <- covered_area / total_area
    population <- sum(d$population[used])
    source_population <- sum(d$population[all][is.finite(d$population[all]) & d$population[all] > 0])
    value <- NA_real_
    log_weight_sum <- NA_real_
    if (length(used)) {
      largest <- max(d$log_weight[used])
      weight <- exp(d$log_weight[used] - largest)
      weight <- weight / sum(weight)
      d$normalized_weight[used] <<- weight
      value <- sum(weight * contribution[used])
      log_weight_sum <- largest + log(sum(exp(d$log_weight[used] - largest)))
    }
    keep <- is.finite(value) && is.finite(population) && population > 0 &&
      is.finite(coverage) && coverage >= minimum
    data.frame(statefips = state, index = value, coverage = coverage, keep = keep,
      population = population, source_population = source_population,
      support_n = length(used), known_area_km2 = covered_area, total_area_km2 = total_area,
      log_weight_sum = log_weight_sum, training_n = sum(d$training[all]))
  })
  states <- do.call(rbind, rows)
  list(states = states,
    diagnostics = list(coefficients = coefficients, nobs = n_training, geo_vars = geo_vars,
      rank = rank, omitted_regressors = dropped, training_states = training_states,
      rmse = rmse, basis = basis, variant = variant, historical_year = historical_year,
      scope = scope, coverage_required = minimum,
      prediction_estimated = prediction,
      prediction_equation = if (prediction) "OLS: log modern employment per square kilometre on historical log density and selected geography" else NULL,
      inference = if (prediction) "Aggregate inference conditions on the estimated county density prediction." else NULL),
    counties = d)
}
