source("hch_prediction.R")
near <- function(a, b, tolerance = 1e-10) stopifnot(length(a) == length(b),
  all(is.finite(a)), all(is.finite(b)), max(abs(a - b)) < tolerance)
pieces <- data.frame(geofips = rep(c(1001, 1003, 4001, 4003), each = 2),
  historical_year = 1900, source_log_density = c(-2, 4, 0, 2, -1, 3, 1, 5),
  allocated_population = c(10, 30, 20, 10, 40, 20, 30, 50), in_historical_state = 1)
pop <- rowsum(pieces$allocated_population, pieces$geofips)
num <- rowsum(pieces$allocated_population * pieces$source_log_density, pieces$geofips)
moments <- data.frame(geofips = as.integer(rownames(pop)), statefips = c(1, 1, 4, 4),
  historical_year = 1900, modern_area_km2 = 10, pop = pop[, 1], numer = num[, 1],
  known_area_km2 = 10, pop_s = pop[, 1], numer_s = num[, 1], known_area_km2_s = 10)
modern <- data.frame(geofips = moments$geofips, statefips = moments$statefips,
  nc = c(300, 250, 400, 700), ac = 10 * 1e6 / 4046.8)
base <- hch_prediction_year(moments, modern, historical_year = 1900)
for (theta in c(.2, .8, 1 - 1e-10, 1, 1 + 1e-10, 1.2, 3, 30)) {
  value <- hch_fixed_instrument(base, theta, pieces)
  expected <- vapply(split(pieces, floor(pieces$geofips / 1000)), function(x) {
    w <- x$allocated_population / sum(x$allocated_population)
    if (abs(theta - 1) < 1e-8) return(sum(w * x$source_log_density))
    log(sum(w * exp((theta - 1) * x$source_log_density))) / (theta - 1)
  }, numeric(1))
  near(value, expected, 1e-8)
}
# A nonlinear index must retain variation within a modern county.
exact <- hch_fixed_instrument(base, 1.3, pieces)
coarsened <- base; coarsened$diagnostics$basis <- "modern"
stopifnot(max(abs(exact - hch_fixed_instrument(coarsened, 1.3))) > .1)

# Region aggregation follows actual county membership, including interstate MSAs.
membership <- data.frame(geofips = moments$geofips, region_id = c(10, 20, 10, 20))
metro <- hch_prediction_year(moments, modern, historical_year = 1900, membership = membership)
near(metro$regions$total_area_km2, c(20, 20))
expected <- vapply(c(10, 20), function(id) {
  x <- pieces[pieces$geofips %in% membership$geofips[membership$region_id == id], ]
  log(sum(x$allocated_population * exp(.3 * x$source_log_density)) / sum(x$allocated_population)) / .3
}, numeric(1))
near(unname(hch_fixed_instrument(metro, 1.3, pieces)), expected)
near(unname(hch_fixed_instrument(metro, 1, pieces)), metro$regions$index)

# Inconsistent source populations cannot silently define a different instrument.
bad <- pieces; bad$allocated_population[1] <- 20
message <- tryCatch({hch_fixed_instrument(base, 1.2, bad); ""}, error = conditionMessage)
stopifnot(grepl("do not reproduce", message, fixed = TRUE))
message <- tryCatch({hch_prediction_year(moments[-1, ], modern, historical_year = 1900,
  membership = membership); ""}, error = conditionMessage)
stopifnot(grepl("missing a constituent county", message, fixed = TRUE))
cat("Fixed-NLS historical instrument passed: exact source aggregation, interstate membership, zero-elasticity limit and mismatched inputs.\n")
