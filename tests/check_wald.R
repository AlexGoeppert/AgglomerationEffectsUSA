# These checks need only base R and leave the app datasets untouched.
local({
  required <- c("ch_index_terms", "stock_wright_lm_s", "fit_ch_model",
    "coef.ch_model", "vcov.ch_model", "confint.ch_model", "ch_coeftable",
    "ch_wald_diagnostic")
  for (expr in parse("app.R")) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]]) %in% required) eval(expr)
  }
  stopifnot(all(vapply(required, exists, logical(1), envir = environment(), inherits = FALSE)))
  same <- function(actual, expected, tolerance = 1e-9) {
    stopifnot(isTRUE(all.equal(unname(actual), unname(expected), tolerance = tolerance)))
  }
  fixture <- function(elasticity, se = 0.02, method = "IV") {
    covariance <- diag(c(se^2, 0.3^2))
    dimnames(covariance) <- list(c("ch_elasticity", "(Intercept)"),
      c("ch_elasticity", "(Intercept)"))
    structure(list(coefficients = c(ch_elasticity = elasticity, `(Intercept)` = 3),
      covariance = covariance, theta = 1 + elasticity, method = method,
      df.residual = Inf), class = "ch_model")
  }

  # The restriction is theta minus one = zero, including when theta itself is one.
  for (elasticity in c(-0.06, 0, 0.06)) {
    model <- fixture(elasticity)
    result <- ch_wald_diagnostic(model)
    stopifnot(result$status == "available", result$df == 1L,
      result$null_elasticity == 0, result$conf_level == 0.95)
    same(result$estimate, elasticity)
    same(result$std_error, 0.02)
    same(result$statistic, if (elasticity == 0) 0 else 9)
    same(result$p_value, if (elasticity == 0) 1 else 0.00269979606326019)
    same(c(result$conf_low, result$conf_high), elasticity + c(-1, 1) * 0.0391992796908011)
    same(result$p_value, ch_coeftable(model)["ch_elasticity", "Pr(>|t|)"])
    same(c(result$conf_low, result$conf_high),
      as.numeric(confint.ch_model(model, parm = "ch_elasticity")))
    ninety <- ch_wald_diagnostic(model, level = 0.90)
    same(c(ninety$conf_low, ninety$conf_high), elasticity + c(-1, 1) * 0.0328970725390294)
    same(ninety$p_value, result$p_value)
    same(ninety$statistic, result$statistic)
    stopifnot(ninety$conf_level == 0.90)
  }
  stopifnot(is.null(ch_wald_diagnostic(fixture(0.06, method = "OLS"))),
    is.null(ch_wald_diagnostic(lm(c(1, 3, 2, 5) ~ c(1, 2, 3, 4)))))
  for (variance in c(0, -0.01, NA_real_, Inf)) {
    model <- fixture(0.06)
    model$covariance["ch_elasticity", "ch_elasticity"] <- variance
    result <- ch_wald_diagnostic(model)
    stopifnot(result$status == "unavailable", nzchar(result$reason),
      is.na(result$statistic), is.na(result$p_value),
      is.na(result$conf_low), is.na(result$conf_high))
  }
  missing_covariance <- fixture(0.06)
  missing_covariance$covariance <- NULL
  stopifnot(ch_wald_diagnostic(missing_covariance)$status == "unavailable")
  for (level in list(0, 1, NA_real_, c(0.90, 0.95))) {
    failure <- tryCatch(ch_wald_diagnostic(fixture(0.06), level = level), error = identity)
    stopifnot(inherits(failure, "error"))
  }

  # With one county per unit, CH is linear in elasticity. Its exact IV solution
  # and sandwich covariance can be computed directly without the CH estimator.
  i <- seq_len(72)
  x <- cos(i * 0.53)
  z <- sin(i * 0.19) + i / 45
  density <- 1 + i / 35 + 0.6 * z + 0.3 * sin(i * 0.83)
  fe <- factor(rep(seq_len(6), each = 12))
  cluster <- rep(seq_len(24), each = 3)
  state <- rep(seq_len(9), each = 8)
  data <- data.frame(
    LHS = 3 + 0.09 * density + 0.21 * x + 0.04 * as.numeric(fe) +
      0.05 * sin(i * 1.17) + 0.03 * cos(cluster * 0.41),
    instrument = z, x = x, fe = fe, clusterID = cluster, state_id = state,
    nc1 = exp(density), ac1 = 1, ch_complete = 1L)
  data$LHS[2] <- NA_real_
  data$instrument[5] <- NA_real_
  data$clusterID[9] <- NA_integer_
  data$state_id[11] <- NA_integer_
  data$x[14] <- NA_real_
  data$ch_complete[17] <- 0L

  for (nuisance in c("intercept", "controls_and_fe")) {
    for (se_spec in c("robust", "cluster_instrument", "cluster_state")) {
      controls <- if (nuisance == "intercept") character() else "x"
      fixed_effect <- if (nuisance == "intercept") "0" else "fe"
      fit <- fit_ch_model(data, "LHS", controls_vec = controls,
        fe_part = fixed_effect, method = "IV", se_spec = se_spec)
      used <- data[fit$rows, , drop = FALSE]
      X <- if (nuisance == "intercept") matrix(1, nrow(used), 1L)
        else model.matrix(~ x + fe, used)
      D <- cbind(log(used$nc1 / used$ac1), X)
      Z <- cbind(used$instrument, X)
      inverse <- solve(crossprod(Z, D))
      coefficients <- inverse %*% crossprod(Z, used$LHS)
      residual <- as.vector(used$LHS - D %*% coefficients)
      scores <- Z * residual
      if (se_spec != "robust") {
        group <- used[[if (se_spec == "cluster_instrument") "clusterID" else "state_id"]]
        scores <- rowsum(scores, group, reorder = FALSE)
      }
      covariance <- inverse %*% crossprod(scores) %*% t(inverse)
      estimate <- as.numeric(coefficients[1])
      std_error <- sqrt(covariance[1, 1])
      result <- ch_wald_diagnostic(fit)
      same(result$estimate, estimate)
      same(result$std_error, std_error)
      same(result$statistic, (estimate / std_error)^2)
      same(result$p_value, 2 * pnorm(abs(estimate / std_error), lower.tail = FALSE))
      same(c(result$conf_low, result$conf_high), estimate + c(-1, 1) * qnorm(0.975) * std_error)
      same(result$p_value, ch_coeftable(fit)["ch_elasticity", "Pr(>|t|)"])
      same(c(result$conf_low, result$conf_high),
        as.numeric(confint.ch_model(fit, parm = "ch_elasticity")))
      ols <- fit_ch_model(data, "LHS", controls_vec = controls,
        fe_part = fixed_effect, method = "OLS", se_spec = se_spec)
      stopifnot(is.null(ch_wald_diagnostic(ols)))
    }
  }
  cat("CH Wald checks passed: theta-minus-one null, analytic p-values and intervals, robust and clustered IV covariance, controls and FE, OLS omission and unavailable variance.\n")
})
