# Base-R checks run before loading the large app datasets.
local({
  app <- new.env(parent = globalenv())
  functions <- c("stock_wright_lm_s", "stock_wright_text", "auxiliary_instrument_wald",
    "ch_instrument_relevance", "anderson_rubin_wald", "ch_index_terms", "fit_ch_model")
  for (expr in parse("app.R")) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]]) %in% functions) eval(expr, app)
  }
  same <- function(a, b) stopifnot(isTRUE(all.equal(a, b, tolerance = 1e-10)))
  i <- seq_len(80)
  x1 <- sin(i * 0.63)
  x2 <- cos(i * 0.27)
  fe <- factor(rep(seq_len(8), each = 10))
  cluster <- rep(seq_len(20), each = 4)
  z <- i / 30 + sin(i * 1.17) + 0.4 * x1
  y <- 0.23 * z + 0.7 * x1 - 0.4 * x2 + sin(i * 0.91) + as.numeric(fe) / 7
  X <- model.matrix(~ x1 + x2 + fe)
  # Explicit matrix projection and cluster sums give an independent reference.
  manual <- function(y, z, X, cluster = NULL) {
    residualise <- function(v) as.vector(v - X %*% solve(crossprod(X), crossprod(X, v)))
    score <- residualise(y) * residualise(z)
    sums <- if (is.null(cluster)) score else vapply(split(score, cluster), sum, numeric(1))
    sum(score)^2 / sum(sums^2)
  }
  for (design in list(matrix(1, length(i), 1L), cbind(1, x1, x2), X)) for (groups in list(NULL, cluster)) {
    diagnostic <- app$stock_wright_lm_s(y, z, design, cluster = groups)
    expected <- manual(y, z, design, groups)
    stopifnot(diagnostic$status == "available", diagnostic$nobs == 80L, diagnostic$df == 1L,
      diagnostic$null_elasticity == 0)
    same(diagnostic$statistic, expected)
    same(diagnostic$p_value, pchisq(expected, 1, lower.tail = FALSE))
  }
  robust <- app$stock_wright_lm_s(y, z, X)
  unique_clusters <- app$stock_wright_lm_s(y, z, X, cluster = i)
  same(robust$statistic, unique_clusters$statistic)
  same(robust$p_value, unique_clusters$p_value)
  for (scale in c(-1e12, -1000, 1e-12, 0.001, 1, 1e12)) {
    diagnostic <- app$stock_wright_lm_s(y, z * scale, X)
    same(diagnostic$statistic, robust$statistic)
    same(diagnostic$p_value, robust$p_value)
  }
  same(app$stock_wright_lm_s(y * 1e-8, z + 100, X)$statistic, robust$statistic)
  same(app$stock_wright_lm_s(y + 4, z + 100, X)$statistic, robust$statistic)
  collinear <- cbind(X, x1_again = 2 * x1)
  same(app$stock_wright_lm_s(y, z, collinear)$statistic, robust$statistic)
  zero_score <- app$stock_wright_lm_s(c(1, 1, -1, -1), c(1, -1, 1, -1))
  stopifnot(zero_score$status == "available", zero_score$statistic == 0, zero_score$p_value == 1)
  degenerate <- list(
    app$stock_wright_lm_s(y, rep(1, length(i)), X),
    app$stock_wright_lm_s(rep(1, length(i)), z, X),
    app$stock_wright_lm_s(y, z, X, cluster = rep(1, length(i))),
    app$stock_wright_lm_s(c(1, 1, -1, -1), c(1, -1, 1, -1), cluster = c(1, 1, 2, 2)))
  stopifnot(all(vapply(degenerate, function(d) d$status == "unavailable" &&
    is.na(d$statistic) && is.na(d$p_value) && nzchar(d$reason), logical(1))))

  # The attached test must use the exact CH sample and nuisance regressors.
  density <- 1 + i / 20
  data <- data.frame(LHS = 3 + 0.12 * density + x1 * 0.2 + as.numeric(fe) * 0.03 + sin(i * 0.91) * 0.01,
    instrument = density + cos(i) * 0.15, x1, fe, clusterID = cluster, state_id = cluster,
    nc1 = exp(density), ac1 = 1, ch_complete = 1L)
  data$LHS[1] <- NA_real_
  data$instrument[2] <- NA_real_
  data$nc1[3] <- NA_real_
  data$x1[4] <- NA_real_
  data$ch_complete[5] <- 0L
  data$clusterID[6] <- NA_integer_
  for (se_spec in c("robust", "cluster_instrument", "cluster_state")) {
    fit <- app$fit_ch_model(data, "LHS", controls_vec = "x1", fe_part = "fe", method = "IV", se_spec = se_spec)
    used <- data[fit$rows, ]
    design <- model.matrix(~ x1 + fe, used)
    groups <- if (se_spec == "robust") NULL else used[[if (se_spec == "cluster_instrument") "clusterID" else "state_id"]]
    same(fit$stock_wright$statistic, manual(used$LHS, used$instrument, design, groups))
    stopifnot(fit$stock_wright$nobs == fit$nobs, fit$stock_wright$df == 1L)
    scaled <- data
    scaled$instrument <- scaled$instrument / 1000
    refit <- app$fit_ch_model(scaled, "LHS", controls_vec = "x1", fe_part = "fe", method = "IV", se_spec = se_spec)
    same(fit$stock_wright$statistic, refit$stock_wright$statistic)
    same(fit$coefficients, refit$coefficients)
    stopifnot(is.null(app$fit_ch_model(data, "LHS", controls_vec = "x1", fe_part = "fe", method = "OLS", se_spec = se_spec)$stock_wright))
  }
  failed_data <- data.frame(LHS = -10 * density, instrument = density, nc1 = exp(density), ac1 = 1)
  failure <- tryCatch(app$fit_ch_model(failed_data, "LHS", method = "IV"), error = identity)
  stopifnot(inherits(failure, "ch_fit_error"), failure$stock_wright$status == "available",
    is.finite(failure$stock_wright$statistic), grepl("Stock–Wright LM S", app$stock_wright_text(failure$stock_wright), fixed = TRUE))
  same(failure$stock_wright$statistic, manual(failed_data$LHS, failed_data$instrument, matrix(1, 80, 1)))
  # References: Stata ivreg2 4.1.12 (14aug2024), e(sstat)/e(sstatp), without small.
  # ivreg2 y (endog=z), robust partial(_cons) ffirst
  # ivreg2 y x1 x2 (endog=z), robust partial(_all) ffirst
  # ivreg2 y x1 x2 i.fe (endog=z), robust partial(_all) ffirst
  # ivreg2 y x1 x2 i.fe (endog=z), cluster(cluster) partial(_all) ffirst
  # Also checked by residualizing y, z and endog on X, then running
  # ivreg2 yr (dr=zr), nocons robust/cluster(cluster) ffirst.
  fixture <- read.csv("tests/fixtures/stock_wright_fixture.csv", na.strings = c("", ".", "NA"))
  expected <- read.csv("tests/fixtures/stock_wright_expected.csv")
  for (j in seq_len(nrow(expected))) {
    reference <- expected[j, ]
    specification <- reference$specification
    used <- fixture[fixture[[paste0("sample_", specification)]] == 1, ]
    used$fe <- factor(used$fe)
    design <- if (specification == "intercept_robust") matrix(1, nrow(used), 1L)
      else if (specification == "controls_robust") model.matrix(~ x1 + x2, used)
      else model.matrix(~ x1 + x2 + fe, used)
    groups <- if (specification == "controls_fe_cluster") used$cluster else NULL
    diagnostic <- app$stock_wright_lm_s(used$y, used$z, design, cluster = groups)
    stopifnot(diagnostic$status == "available", diagnostic$nobs == reference$N)
    same(diagnostic$statistic, reference$S)
    same(diagnostic$p_value, reference$p)
  }
  cat("Stock–Wright LM S checks passed: robust/clustered scores, controls, FE, scale invariance, exact CH sample, OLS omission and fit failures.\n")
  cat("All four saved Stata ivreg2 S statistics, p-values and sample sizes match.\n")
})
