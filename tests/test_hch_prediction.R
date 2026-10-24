source("hch_prediction.R")

near <- function(a, b, tolerance = 1e-9) {
  stopifnot(length(a) == length(b), all(is.finite(a)), all(is.finite(b)), max(abs(a - b)) < tolerance)
}
fails <- function(expr, pattern) {
  error <- tryCatch({ force(expr); NULL }, error = conditionMessage)
  stopifnot(is.character(error), grepl(pattern, error, fixed = TRUE))
}

set.seed(273)
n <- 24L
state <- rep(1:4, each = 6)
county <- 1000 * state + rep(1:6, 4)
area <- exp(rnorm(n, 1, .3))
historical <- rnorm(n, 1, .7)
geo <- rnorm(n)
population <- exp(historical) * area * exp(rnorm(n, 0, .2))
modern_density <- 2 + .7 * historical + .15 * geo + rnorm(n, 0, .1)
moments <- data.frame(geofips = county, statefips = state, historical_year = 1790,
  modern_area_km2 = area, pop = population, numer = population * historical,
  known_area_km2 = area * rep(c(1, .9, .7, .4), each = 6),
  pop_s = population * .8, numer_s = population * .8 * (historical - .2),
  known_area_km2_s = area * rep(c(.99, .85, .6, .3), each = 6))
components <- data.frame(geofips = county, statefips = state,
  nc = area * exp(modern_density), ac = area * 1e6 / 4046.8)
geography <- data.frame(geofips = county, geo = geo)
run <- function(basis = "source", variant = "historical", minimum = .8,
                scope = "states_territories", m = moments, c = components, g = geography,
                vars = "geo")
  hch_prediction_year(m, c, g, 1790, scope, basis, variant, vars, minimum)

# Reconstruct all four instruments directly from an independent OLS fit.
for (basis in c("source", "modern")) for (scope in c("states_only", "states_territories")) {
  p <- if (scope == "states_only") moments$pop_s else moments$pop
  L <- if (basis == "modern") log(p / area) else if (scope == "states_only") historical - .2 else historical
  fit <- lm(modern_density ~ L + geo)
  fitted <- unname(fitted(fit))
  for (variant in c("historical", "a", "b", "ab")) {
    actual <- run(basis, variant, scope = scope)
    weights <- if (variant %in% c("b", "ab")) area * exp(fitted) else p
    value <- if (variant %in% c("a", "ab")) fitted else L
    expected <- vapply(1:4, function(s) sum(weights[state == s] * value[state == s]) / sum(weights[state == s]), numeric(1))
    near(actual$states$index, expected)
    near(actual$states$total_area_km2, as.numeric(rowsum(area, state)))
    near(as.numeric(rowsum(actual$counties$normalized_weight, actual$counties$statefips)), rep(1, 4))
    if (variant != "historical") {
      near(unname(actual$diagnostics$coefficients), unname(coef(fit)))
      stopifnot(actual$diagnostics$nobs == n, identical(actual$diagnostics$training_states, 1:4),
        all(actual$states$support_n == 6L), all(actual$counties$training))
    }
  }
}

# Changing the state cutoff does not refit the national county regression.
for (variant in c("historical", "a", "b", "ab")) {
  low <- run(variant = variant, minimum = 0)
  mid <- run(variant = variant, minimum = .8)
  high <- run(variant = variant, minimum = .95)
  near(low$states$index, high$states$index)
  stopifnot(identical(low$diagnostics$coefficients, high$diagnostics$coefficients),
    identical(high$states$keep, c(TRUE, FALSE, FALSE, FALSE)),
    identical(mid$states$keep, c(TRUE, TRUE, FALSE, FALSE)), all(low$states$keep))
}

# Geography and current employment do not alter the historical baseline.
bad_geo <- geography; bad_geo$geo <- NA_real_
bad_modern <- components; bad_modern$nc <- NA_real_
near(run(g = bad_geo, c = bad_modern)$states$index, run()$states$index)
near(run(basis = "modern", g = bad_geo, c = bad_modern)$states$index, run(basis = "modern")$states$index)

# A, B and A+B use the same county support and the full state polygon denominator.
partial <- geography; partial$geo[1] <- NA_real_
fits <- lapply(c("a", "b", "ab"), function(v) run(variant = v, g = partial))
for (f in fits) {
  stopifnot(f$diagnostics$nobs == n - 1L, !f$counties$support[1])
  near(f$states$total_area_km2, run()$states$total_area_km2)
  near(f$states$coverage[1], sum(moments$known_area_km2[2:6]) / sum(area[1:6]))
}
stopifnot(identical(fits[[1]]$counties$support, fits[[2]]$counties$support),
  identical(fits[[2]]$counties$support, fits[[3]]$counties$support))

# Zero population is distinct from unknown population. Baselines retain its area.
zero <- moments; zero$pop[1] <- 0; zero$numer[1] <- 0
historical_zero <- run(m = zero)
near(historical_zero$states$coverage, run()$states$coverage)
stopifnot(!historical_zero$counties$support[1], historical_zero$counties$coverage_support[1])
prediction_zero <- run(m = zero, variant = "a")
stopifnot(!prediction_zero$counties$support[1], !prediction_zero$counties$coverage_support[1])
unknown <- moments; unknown$pop[1] <- NA_real_
stopifnot(!run(m = unknown)$counties$coverage_support[1])

# Constant and duplicated controls are omitted without changing fitted values.
redundant <- geography; redundant$constant <- 2; redundant$twice <- 2 * redundant$geo
simple <- run(variant = "ab")
singular <- run(variant = "ab", g = redundant, vars = c("geo", "constant", "twice"))
near(simple$states$index, singular$states$index)
stopifnot(length(singular$diagnostics$omitted_regressors) == 2L,
  singular$diagnostics$rank == simple$diagnostics$rank)

# Zero current employment can be predicted, but an unidentified extrapolation cannot.
new_target <- components; new_target$nc[1] <- 0
estimable <- run(variant = "a", c = new_target, g = redundant, vars = c("geo", "constant", "twice"))
stopifnot(!estimable$counties$training[1], estimable$counties$support[1])
outside <- redundant; outside$constant[1] <- 4
not_estimable <- run(variant = "a", c = new_target, g = outside, vars = c("geo", "constant", "twice"))
stopifnot(!not_estimable$counties$support[1])

# The normalization remains finite when predicted employment would overflow.
extreme <- data.frame(geofips = 1:4, statefips = c(1, 1, 1, 2), historical_year = 1790,
  modern_area_km2 = 1, pop = exp(c(-1, 0, 1, 20)),
  numer = exp(c(-1, 0, 1, 20)) * c(-1, 0, 1, 20), known_area_km2 = 1)
extreme_modern <- data.frame(geofips = 1:4, statefips = c(1, 1, 1, 2),
  nc = c(exp(c(500, 600, 700)), NA_real_), ac = 1e6 / 4046.8)
extreme_fit <- hch_prediction_year(extreme, extreme_modern, historical_year = 1790, variant = "ab", minimum = 0)
near(extreme_fit$states$index[2], 2600, 1e-7)
stopifnot(extreme_fit$states$keep[2], is.finite(extreme_fit$states$log_weight_sum[2]))
extreme_b <- hch_prediction_year(extreme, extreme_modern, historical_year = 1790, variant = "b", minimum = 0)
near(extreme_b$states$index[2], 20)

# Unit shifts change fitted density levels but not normalized predicted weights.
shifted <- components; shifted$nc <- shifted$nc * exp(3)
near(run(variant = "b", c = shifted)$states$index, run(variant = "b")$states$index)
near(run(variant = "ab", c = shifted)$states$index, run(variant = "ab")$states$index + 3)

fails(run(variant = "unknown"), "variant")
fails(run(minimum = .7), "coverage")
duplicates <- moments[c(1, 1, 2:n), ]
fails(run(m = duplicates), "one identified row")
tiny <- moments[1:2, ]; fails(run(m = tiny, variant = "a"), "Too few counties")
cat("Historical CH prediction unit tests passed.\n")
