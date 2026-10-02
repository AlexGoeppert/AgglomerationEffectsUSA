# Anderson-Rubin tests the fixed CH null without using the fitted elasticity.
local({
  app <- new.env(parent = globalenv())
  required <- c("ch_index_terms", "stock_wright_lm_s", "auxiliary_instrument_wald",
    "ch_instrument_relevance", "anderson_rubin_wald", "anderson_rubin_text", "fit_ch_model")
  for (expr in parse("app.R")) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]]) %in% required) eval(expr, app)
  }
  stopifnot(all(vapply(required, exists, logical(1), envir = app, inherits = FALSE)))
  same <- function(actual, expected, tolerance = 1e-9) {
    stopifnot(isTRUE(all.equal(unname(actual), unname(expected), tolerance = tolerance)))
  }
  # Independent full-design OLS and sandwich covariance, without residualizing.
  manual <- function(y_null, instrument, X, cluster = NULL) {
    design <- cbind(instrument, X)
    bread <- solve(crossprod(design))
    coefficients <- bread %*% crossprod(design, y_null)
    residual <- as.vector(y_null - design %*% coefficients)
    scores <- design * residual
    if (!is.null(cluster)) scores <- rowsum(scores, cluster, reorder = FALSE)
    covariance <- bread %*% crossprod(scores) %*% bread
    as.numeric(coefficients[1]^2 / covariance[1, 1])
  }
  i <- seq_len(80)
  x1 <- sin(i * 0.63)
  x2 <- cos(i * 0.27)
  fe <- factor(rep(seq_len(8), each = 10))
  cluster <- rep(seq_len(20), each = 4)
  z <- i / 30 + sin(i * 1.17) + 0.4 * x1
  y <- 0.23 * z + 0.7 * x1 - 0.4 * x2 + sin(i * 0.91) + as.numeric(fe) / 7
  X <- model.matrix(~ x1 + x2 + fe)
  for (design in list(matrix(1, length(i), 1L), cbind(1, x1, x2), X)) {
    for (groups in list(NULL, cluster)) {
      result <- app$anderson_rubin_wald(y, z, design, cluster = groups)
      expected <- manual(y, z, design, groups)
      same(result$statistic, expected)
      same(result$p_value, pchisq(expected, 1, lower.tail = FALSE))
      stopifnot(result$status == "available", result$nobs == length(i),
        result$df == 1L, result$null_elasticity == 0,
        result$covariance == if (is.null(groups)) "HC0" else "CR0")
      qr_result <- app$anderson_rubin_wald(y, z, design_qr = qr(design), cluster = groups)
      same(qr_result$statistic, expected)
      same(qr_result$p_value, result$p_value)
    }
  }
  result <- app$anderson_rubin_wald(y, z, X, cluster)
  for (scale in c(-1e12, -1000, -0.001, 0.001, 1000, 1e12)) {
    scaled <- app$anderson_rubin_wald(y, z * scale, X, cluster)
    same(scaled$statistic, result$statistic)
    same(scaled$p_value, result$p_value)
  }
  shifted <- app$anderson_rubin_wald(y + 2 * x1 - 1, z + 3 * x2 + 5, X, cluster)
  same(shifted$statistic, result$statistic)
  zero <- app$anderson_rubin_wald(c(1, 1, -1, -1), c(1, -1, 1, -1))
  stopifnot(zero$status == "available", zero$statistic == 0, zero$p_value == 1)
  invalid <- list(
    app$anderson_rubin_wald(y, rep(1, length(i)), X),
    app$anderson_rubin_wald(rep(1, length(i)), z, X),
    app$anderson_rubin_wald(y, z, X, rep(1, length(i))),
    app$anderson_rubin_wald(replace(y, 1, NA_real_), z, X))
  stopifnot(all(vapply(invalid, function(d) d$status == "unavailable" &&
    is.na(d$statistic) && is.na(d$p_value) && nzchar(d$reason), logical(1))))

  # A nonzero CH null must be subtracted before running the auxiliary regression.
  n <- cbind(exp(1 + i / 25), exp(0.5 + i / 40 + sin(i * 0.3) / 5))
  a <- cbind(rep(1, length(i)), rep(2, length(i)))
  for (beta_null in c(-0.1, 0, 0.12)) {
    h_null <- log(rowSums(n^(1 + beta_null) * a^(-beta_null)) / rowSums(n))
    y_null <- y - h_null
    tested <- app$anderson_rubin_wald(y_null, z, X, cluster, null_elasticity = beta_null)
    same(tested$statistic, manual(y_null, z, X, cluster))
    stopifnot(tested$null_elasticity == beta_null)
  }

  # Values from Stata ivreg2 4.1.12 e(archi2)/e(archi2p), with partial(_all)
  # (partial(_cons) for the intercept-only case), robust/cluster and no small.
  fixture <- read.csv("tests/fixtures/stock_wright_fixture.csv", na.strings = c("", ".", "NA"))
  expected <- data.frame(
    specification = c("intercept_robust", "controls_robust", "controls_fe_robust", "controls_fe_cluster"),
    N = c(237, 236, 235, 234),
    AR = c(3.368271326457171, 0.1767781445583357, 0.8970365019745967, 1.591306781732709),
    p = c(0.0664634605094705, 0.6741567986927235, 0.3435775786532394, 0.207139530495789))
  for (j in seq_len(nrow(expected))) {
    reference <- expected[j, ]
    used <- fixture[fixture[[paste0("sample_", reference$specification)]] == 1, ]
    used$fe <- factor(used$fe)
    design <- if (reference$specification == "intercept_robust") matrix(1, nrow(used), 1L)
      else if (reference$specification == "controls_robust") model.matrix(~ x1 + x2, used)
      else model.matrix(~ x1 + x2 + fe, used)
    groups <- if (reference$specification == "controls_fe_cluster") used$cluster else NULL
    tested <- app$anderson_rubin_wald(used$y, used$z, design, groups)
    same(tested$statistic, reference$AR)
    same(tested$p_value, reference$p)
    stopifnot(tested$nobs == reference$N)
  }

  data <- data.frame(LHS = y + 3, instrument = z, x1 = x1, fe = fe,
    nc1 = n[, 1], nc2 = n[, 2], ac1 = a[, 1], ac2 = a[, 2],
    clusterID = cluster, state_id = rep(seq_len(10), each = 8), ch_complete = 1L)
  data$LHS <- 3 + app$ch_index_terms(1.12, n, a)$value + 0.2 * x1 +
    as.numeric(fe) / 20 + 0.01 * sin(i * 0.91)
  data$LHS[1] <- NA_real_
  data$instrument[2] <- NA_real_
  data$nc1[3] <- NA_real_
  data$x1[4] <- NA_real_
  data$ch_complete[5] <- 0L
  data$clusterID[6] <- NA_integer_
  for (se_spec in c("robust", "cluster_instrument", "cluster_state")) {
    fit <- app$fit_ch_model(data, "LHS", controls_vec = "x1", fe_part = "fe",
      method = "IV", se_spec = se_spec)
    used <- data[fit$rows, , drop = FALSE]
    design <- model.matrix(~ x1 + fe, used)
    groups <- if (se_spec == "robust") NULL else used[[if (se_spec == "cluster_instrument") "clusterID" else "state_id"]]
    same(fit$anderson_rubin$statistic, manual(used$LHS, used$instrument, design, groups))
    stopifnot(fit$anderson_rubin$nobs == fit$nobs, fit$anderson_rubin$null_elasticity == 0)
    ols <- app$fit_ch_model(data, "LHS", controls_vec = "x1", fe_part = "fe", method = "OLS", se_spec = se_spec)
    stopifnot(is.null(ols$anderson_rubin))
  }
  # An infeasible positive-theta estimate does not prevent testing a fixed null.
  density <- 1 + i / 20
  failure_data <- data.frame(LHS = -10 * density + 0.1 * sin(i), instrument = density,
    nc1 = exp(density), ac1 = 1)
  failure <- tryCatch(app$fit_ch_model(failure_data, "LHS", method = "IV"), error = identity)
  stopifnot(inherits(failure, "ch_fit_error"), failure$anderson_rubin$status == "available",
    is.finite(failure$anderson_rubin$statistic),
    grepl("Anderson", app$anderson_rubin_text(failure$anderson_rubin), fixed = TRUE))
  same(failure$anderson_rubin$statistic,
    manual(failure_data$LHS, failure_data$instrument, matrix(1, length(i), 1L)))
  cat("Anderson-Rubin Wald checks passed: fixed nonlinear nulls, full-design HC0/CR0 reference, four Stata statistics and p-values, exact CH sample, scale invariance, OLS omission and retained diagnostics after fit failure.\n")
})
