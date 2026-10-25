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
                                geo_vars = character(), minimum = .80, membership = NULL) {
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
    train <- candidates & is.finite(y) & d$statefips != 11
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
  # Fit nationally before assigning counties to the requested regional units.
  if (is.null(membership)) {
    d$region_id <- d$statefips
    d <- d[d$statefips != 11, , drop = FALSE]
  } else {
    if (!all(c("geofips", "region_id") %in% names(membership)) ||
        anyNA(membership[c("geofips", "region_id")]) || anyDuplicated(membership$geofips))
      stop("Historical CH membership needs one identified region per county unit.")
    member_row <- match(membership$geofips, d$geofips)
    if (anyNA(member_row))
      stop("Historical CH county moments are missing a constituent county. Rebuild the historical CH inputs.")
    d <- d[member_row, , drop = FALSE]
    d$region_id <- membership$region_id
  }
  contribution <- if (variant %in% c("a", "ab")) d$predicted_log_density else d$historical_log_density
  d$contribution <- contribution
  rows <- lapply(sort(unique(d$region_id)), function(region) {
    all <- which(d$region_id == region)
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
    data.frame(region_id = region, statefips = region, index = value, coverage = coverage, keep = keep,
      population = population, source_population = source_population,
      support_n = length(used), known_area_km2 = covered_area, total_area_km2 = total_area,
      log_weight_sum = log_weight_sum, training_n = sum(d$training[all]))
  })
  states <- do.call(rbind, rows)
  list(states = states, regions = states,
    diagnostics = list(coefficients = coefficients, nobs = n_training, geo_vars = geo_vars,
      rank = rank, omitted_regressors = dropped, training_states = training_states,
      rmse = rmse, basis = basis, variant = variant, historical_year = historical_year,
      scope = scope, coverage_required = minimum,
      prediction_estimated = prediction,
      prediction_equation = if (prediction) "OLS: log modern employment per square kilometre on historical log density and selected geography" else NULL,
      inference = if (prediction) "Aggregate inference conditions on the estimated county density prediction." else NULL),
    counties = d)
}

# The nonlinear historical index needs source-level densities, not their mean.
get_hch_source_pieces <- local({
  cached <- NULL
  signature <- NULL
  function(path = "Historical_CH_source_pieces.dta") {
    if (!file.exists(path)) stop("Historical CH source pieces are unavailable. Rebuild the historical CH inputs.")
    info <- file.info(path)
    current <- paste(normalizePath(path, winslash = "/"), info$size, as.numeric(info$mtime), sep = "|")
    if (!identical(current, signature)) {
      value <- as.data.frame(haven::read_dta(path))
      required <- c("geofips", "historical_year", "source_log_density", "allocated_population", "in_historical_state")
      if (!all(required %in% names(value))) stop("Historical CH source-piece fields are missing.")
      if (any(!is.finite(value$allocated_population) | value$allocated_population <= 0) ||
          any(!is.finite(value$source_log_density))) stop("Historical CH source pieces need positive known population and finite density.")
      cached <<- value
      signature <<- current
    }
    cached
  }
})

hch_fixed_instrument <- function(hch_year, theta, source_pieces = NULL) {
  if (length(theta) != 1L || !is.finite(theta) || theta <= 0)
    stop("The historical CH instrument requires a finite positive NLS pilot theta.")
  d <- hch_year$counties
  needed <- c("geofips", "region_id", "support", "log_weight", "contribution", "population")
  if (is.null(d) || !all(needed %in% names(d)))
    stop("Historical county contributions are required for the NLS-based instrument.")
  d <- d[d$support & is.finite(d$log_weight) & is.finite(d$contribution), , drop = FALSE]
  if (!nrow(d)) stop("No historical county contributions remain for the NLS-based instrument.")
  meta <- hch_year$diagnostics
  if (identical(meta$variant, "historical") && identical(meta$basis, "source")) {
    if (is.null(source_pieces)) source_pieces <- get_hch_source_pieces()
    pieces <- source_pieces[source_pieces$historical_year == meta$historical_year, , drop = FALSE]
    if (identical(meta$scope, "states_only"))
      pieces <- pieces[!is.na(pieces$in_historical_state) & pieces$in_historical_state == 1, , drop = FALSE]
    member <- match(pieces$geofips, d$geofips)
    pieces <- pieces[!is.na(member), , drop = FALSE]
    member <- member[!is.na(member)]
    pop <- rowsum(pieces$allocated_population, pieces$geofips, reorder = FALSE)
    expected <- d$population[match(rownames(pop), as.character(d$geofips))]
    if (!setequal(rownames(pop), as.character(d$geofips)) ||
        any(abs(pop[, 1] - expected) > 1e-7 * (1 + abs(expected))))
      stop("Historical source pieces do not reproduce the selected county populations.")
    work <- data.frame(region_id = d$region_id[member],
      density = pieces$source_log_density, log_weight = log(pieces$allocated_population))
  } else {
    work <- data.frame(region_id = d$region_id, density = d$contribution, log_weight = d$log_weight)
  }
  rho <- theta - 1
  # Dividing by fixed rho only rescales an IV. Centering avoids cancellation at zero.
  value <- vapply(split(work, as.character(work$region_id)), function(x) {
    w <- exp(x$log_weight - max(x$log_weight)); w <- w / sum(w)
    mu <- sum(w * x$density)
    centered <- x$density - mu
    if (rho == 0) return(mu)
    if (max(abs(rho * centered)) < .01)
      return(mu + log1p(sum(w * expm1(rho * centered))) / rho)
    v <- log(w) + rho * centered
    m <- max(v)
    mu + (m + log(sum(exp(v - m)))) / rho
  }, numeric(1))
  if (any(!is.finite(value))) stop("The NLS-based historical CH instrument is not finite.")
  value
}

fit_ch_with_historical_instrument <- function(data, fit, hch_year, region_ids,
                                              instrument_method = "derivative", gmm_steps = 1L,
                                              method = "IV") {
  if (identical(instrument_method, "nls_fixed")) instrument_method <- "nls"
  if (length(instrument_method) != 1L || !instrument_method %in% c("derivative", "nls"))
    stop("Choose the derivative instrument or the NLS-based instrument.")
  gmm_steps <- suppressWarnings(as.integer(gmm_steps))
  if (length(gmm_steps) != 1L || is.na(gmm_steps) || !gmm_steps %in% c(1L, 2L))
    stop("Choose one-step or two-step GMM.")
  if (length(region_ids) != nrow(data) || anyNA(region_ids) || anyDuplicated(region_ids))
    stop("The CH instrument needs one identified row per region.")
  pilot <- NULL
  fixed <- data$instrument
  if (method == "IV" && instrument_method == "nls") {
    # Both stages use the same outcomes, controls, counties and regional sample.
    eligible <- which(is.finite(data$instrument))
    pilot <- fit(data[eligible, , drop = FALSE], "NLS_pilot")
    rows <- eligible[pilot$rows]
    if (!length(rows) || anyNA(rows) || anyDuplicated(rows)) stop("The NLS pilot returned an invalid sample.")
    generated <- hch_fixed_instrument(hch_year, pilot$theta)
    fixed <- rep(NA_real_, nrow(data))
    fixed[rows] <- generated[as.character(region_ids[rows])]
    if (any(!is.finite(fixed[rows]))) stop("An NLS pilot region lacks the historical CH instrument.")
    final_data <- data[rows, , drop = FALSE]
    final_data$instrument <- fixed[rows]
    result <- tryCatch(fit(final_data, "IV"), error = function(e) {
      e$stock_wright <- NULL; e$anderson_rubin <- NULL
      stop(e)
    })
    if (!identical(as.integer(result$rows), seq_along(rows)))
      stop("The NLS pilot and GMM estimation samples differ.")
    result$rows <- rows
    if (!is.null(pilot$excluded_states)) result$excluded_states <- pilot$excluded_states
    result$stock_wright <- NULL
    result$anderson_rubin <- NULL
    result$pilot_theta <- pilot$theta
    result$pilot_rows <- rows
    result$instrument_inference <- paste("Inference conditions on the NLS pilot theta and the constructed instrument.",
      "The pilot uses the outcome, so the fixed-instrument Stock-Wright S test is not reported for this method.")
    result$inference <- paste(result$inference, result$instrument_inference)
  } else {
    result <- fit(data, method)
    result$pilot_theta <- NA_real_
    result$pilot_rows <- integer()
    result$instrument_inference <- "The derivative instrument does not require a preliminary CH elasticity."
  }
  result$instrument_method <- instrument_method
  result$historical_instrument <- fixed
  result$gmm_steps <- gmm_steps
  result$gmm_identification <- "Exactly identified: one excluded historical instrument and one CH elasticity."
  result$gmm_weighting <- if (gmm_steps == 1L) "One-step GMM" else
    "Two-step GMM: the exact moment solution and robust covariance equal one-step GMM."
  result
}
