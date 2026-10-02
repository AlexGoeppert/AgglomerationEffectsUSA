# Local instrument relevance uses the fitted CH derivative and an auxiliary OLS fit.
local({
  app <- new.env(parent = globalenv())
  required <- c("ch_index_terms", "stock_wright_lm_s", "auxiliary_instrument_wald",
    "ch_instrument_relevance", "anderson_rubin_wald", "fit_ch_model")
  for (expr in parse("app.R")) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]]) %in% required) eval(expr, app)
  }
  stopifnot(all(vapply(required, exists, logical(1), envir = app, inherits = FALSE)))
  same <- function(actual, expected, tolerance = 1e-9) {
    stopifnot(isTRUE(all.equal(unname(actual), unname(expected), tolerance = tolerance)))
  }
  # A full OLS design and sandwich covariance provide an independent reference.
  manual <- function(derivative, instrument, X, cluster = NULL) {
    design <- cbind(instrument, X)
    bread <- solve(crossprod(design))
    coefficients <- bread %*% crossprod(design, derivative)
    residual <- as.vector(derivative - design %*% coefficients)
    scores <- design * residual
    if (!is.null(cluster)) scores <- rowsum(scores, cluster, reorder = FALSE)
    covariance <- bread %*% crossprod(scores) %*% bread
    partial <- function(value) as.vector(value - X %*% solve(crossprod(X), crossprod(X, value)))
    coefficient <- as.numeric(coefficients[1])
    se <- sqrt(covariance[1, 1])
    list(coefficient = coefficient, std_error = se, statistic = (coefficient / se)^2,
      partial_r2 = cor(partial(derivative), partial(instrument))^2)
  }
  compare <- function(result, reference) {
    stopifnot(result$status == "available")
    for (name in c("coefficient", "std_error", "statistic", "partial_r2"))
      same(result[[name]], reference[[name]])
    # No inferential p-value is assigned to this fitted-Jacobian diagnostic.
    stopifnot(!"p_value" %in% names(result), !"critical_value" %in% names(result))
  }
  i <- seq_len(84)
  x1 <- sin(i * 0.43)
  x2 <- cos(i * 0.71)
  fe <- factor(rep(seq_len(7), each = 12))
  cluster <- rep(seq_len(21), each = 4)
  z <- i / 30 + sin(i * 0.17) + 0.35 * x1
  derivative <- 0.2 * z + 0.5 * x1 - 0.3 * x2 + as.numeric(fe) / 10 + sin(i * 1.13)
  X <- model.matrix(~ x1 + x2 + fe)
  for (design in list(matrix(1, length(i), 1L), cbind(1, x1, x2), X)) {
    for (groups in list(NULL, cluster)) {
      result <- app$ch_instrument_relevance(derivative, z, X = design, cluster = groups, theta = 1.08)
      compare(result, manual(derivative, z, design, groups))
      stopifnot(result$nobs == length(i), result$evaluated_theta == 1.08,
        result$covariance == if (is.null(groups)) "HC0" else "CR0")
      if (!is.null(groups)) stopifnot(result$clusters == length(unique(groups)))
      qr_result <- app$ch_instrument_relevance(derivative, z,
        design_qr = qr(design), cluster = groups, theta = 1.08)
      compare(qr_result, result)
    }
  }
  robust <- app$ch_instrument_relevance(derivative, z, X)
  clustered <- app$ch_instrument_relevance(derivative, z, X, cluster)
  unique_clusters <- app$ch_instrument_relevance(derivative, z, X, i)
  compare(unique_clusters, robust)
  for (scale in c(-1000, -0.001, 0.001, 1000)) {
    scaled <- app$ch_instrument_relevance(derivative, z * scale, X, cluster)
    same(scaled$statistic, clustered$statistic)
    same(scaled$partial_r2, clustered$partial_r2)
    same(scaled$coefficient, clustered$coefficient / scale)
    same(scaled$std_error, clustered$std_error / abs(scale))
    shifted <- app$ch_instrument_relevance(derivative * scale + 2 * x1 + 1,
      z + 3 * x2 + 4, X, cluster)
    same(shifted$statistic, clustered$statistic)
    same(shifted$partial_r2, clustered$partial_r2)
    same(shifted$coefficient, clustered$coefficient * scale)
    same(shifted$std_error, clustered$std_error * abs(scale))
  }
  collinear <- app$ch_instrument_relevance(derivative, z, cbind(X, 2 * x1), cluster)
  compare(collinear, clustered)
  invalid <- list(
    app$ch_instrument_relevance(derivative, rep(1, length(i)), X),
    app$ch_instrument_relevance(rep(1, length(i)), z, X),
    app$ch_instrument_relevance(derivative, z, X, rep(1, length(i))),
    app$ch_instrument_relevance(derivative, z, X, replace(cluster, 1, NA_integer_)),
    app$ch_instrument_relevance(replace(derivative, 1, NA_real_), z, X),
    app$ch_instrument_relevance(derivative, replace(z, 1, Inf), X),
    app$ch_instrument_relevance(derivative, z, diag(length(i))))
  stopifnot(all(vapply(invalid, function(result) result$status == "unavailable" &&
    is.na(result$statistic) && nzchar(result$reason), logical(1))))

  # Check that model attachments use the fitted nonlinear derivative, exact model
  # rows, selected controls, fixed effects and covariance grouping.
  data <- data.frame(instrument = z, x1 = x1, fe = fe, clusterID = cluster,
    state_id = rep(seq_len(12), each = 7), nc1 = exp(2 + i / 35 + 0.4 * z),
    nc2 = exp(1 + i / 45 + 0.2 * cos(i * 0.23)), ac1 = 1, ac2 = 1.5,
    ch_complete = 1L)
  n <- as.matrix(data[c("nc1", "nc2")])
  a <- as.matrix(data[c("ac1", "ac2")])
  data$LHS <- 3 + app$ch_index_terms(1.12, n, a)$value + 0.2 * x1 +
    0.03 * as.numeric(fe) + 0.015 * sin(i * 0.91)
  data$LHS[1] <- NA_real_
  data$instrument[2] <- NA_real_
  data$nc1[3] <- NA_real_
  data$x1[4] <- NA_real_
  data$ch_complete[5] <- 0L
  data$clusterID[6] <- NA_integer_
  data$state_id[8] <- NA_integer_
  for (nuisance in c("intercept", "controls_and_fe")) for (se_spec in c("robust", "cluster_instrument", "cluster_state")) {
    controls <- if (nuisance == "intercept") character() else "x1"
    fixed_effect <- if (nuisance == "intercept") "0" else "fe"
    fit <- app$fit_ch_model(data, "LHS", controls_vec = controls,
      fe_part = fixed_effect, method = "IV", se_spec = se_spec)
    used <- data[fit$rows, , drop = FALSE]
    design <- if (nuisance == "intercept") matrix(1, nrow(used), 1L) else model.matrix(~ x1 + fe, used)
    groups <- if (se_spec == "robust") NULL else used[[if (se_spec == "cluster_instrument") "clusterID" else "state_id"]]
    compare(fit$instrument_relevance, manual(fit$index_derivative, used$instrument, design, groups))
    stopifnot(fit$instrument_relevance$nobs == fit$nobs,
      fit$instrument_relevance$evaluated_theta == fit$theta)
    # Direct differentiation of the CH sum verifies the derivative input.
    n <- as.matrix(used[c("nc1", "nc2")])
    a <- as.matrix(used[c("ac1", "ac2")])
    weights <- n^fit$theta * a^(1 - fit$theta)
    direct_derivative <- rowSums(weights * log(n / a)) / rowSums(weights)
    same(fit$index_derivative, direct_derivative)
    ols <- app$fit_ch_model(data, "LHS", controls_vec = controls,
      fe_part = fixed_effect, method = "OLS", se_spec = se_spec)
    stopifnot(is.null(ols$instrument_relevance))
  }
  cat("CH local instrument-relevance checks passed: auxiliary OLS Wald F, HC0/CR0 covariance, scale invariance, nuisance regressors, exact nonlinear CH sample and derivative, OLS omission and invalid inputs.\n")
})
