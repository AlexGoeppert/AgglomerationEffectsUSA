# --- 1. Load Required Packages ------------------------------------------------

library(shiny)
library(fixest)
library(dplyr)
library(haven)
library(glue)
library(stringr)
library(shinycssloaders) # For withSpinner
library(DT)
library(ggplot2)

# --- 2. Load Data on Startup --------------------------------------------------

tryCatch({
  msa_data    <- haven::read_dta("MSA_analysis_data.dta")
  county_data <- haven::read_dta("master_county_build.dta")
}, error = function(e) {
  stop("Error: Make sure 'MSA_analysis_data.dta' and 'master_county_build.dta' are in the same directory as app.R")
})

# --- 3. Territory Filtering Function ------------------------------------------

get_territory_exclusions <- function(data, fe_column_name, id_column) {
  territory_patterns <- c("territory", "territoy", "territ", "organised", "organized", "district", "columbia", "\\bdc\\b")
  
  if (!fe_column_name %in% names(data)) {
    return(c()) # Return empty vector if column doesn't exist
  }
  
  territory_ids <- data %>%
    filter(
      if_any(all_of(fe_column_name), ~ str_detect(tolower(as.character(.)), 
                                                  paste(territory_patterns, collapse = "|")))
    ) %>%
    pull(!!sym(id_column)) %>%
    unique()
  
  return(territory_ids)
}

# --- 4. Standardized help‑text strings  --------------------------------------
# Master settings
help_analysis_level      <- "Pick the geography to study: metropolitan statistical areas (MSAs) or counties."
help_year_modern         <- "Year in which modern productivity and employment are measured."
help_analysis_type       <- paste(
  "Estimation method:",
  " • OLS – ordinary least squares regression of modern productivity on modern employment density. ",
  " • IV – instrumental variables regression using historical population density as an instrument for modern density. ",
  " • First-stage regression – ordinary least squares regression of modern employment density on historical population density.",
  sep = "\n")

# Fixed effects & scope
help_use_fe              <- "State fixed effects to eliminate state level factors affecting modern productivity."
help_fe_type             <- "Choose whether state fixed effects are used for modern states or historical states and territories."
help_sample_scope        <- paste(
  "Which modern places stay in the sample?",
  " • Historical States Only – includes only the territory covered by historical states. ",
  " • Historical States & Territories – also includes historical organized U.S. territories.",
  sep = "\n")

# Standardized settings (used for both MSA and County)
help_sectors             <- "Select whether modern productivity (output per worker) used as the left-hand-side outcome is measured for the whole economy, the private economy, etc. The Right-hand-side employment density always refers to total employment. "
help_sample_year         <- "Defines which modern geographic units are included in the empirical analysis."
help_iv_year             <- "Defines which historical census is used to obtain historical population density (the instrument for modern employment density)."
help_controls            <- "Add indicator variables for whether the geographic unit has access to waterways in 1820 (ocean, lakes, rivers, canals) or railroads in 1840, 1850, or 1861."
help_schooling_adj       <- paste(
  "Adjust productivity for schooling using Mincerian return to schooling:",
  " • None – no adjustment.",
  " • Same Return – employs nationwide Mincerian return to schooling. ",
  " • Specific Return – allows Mincerian return to vary with the employment density of the modern geographic unit.",
  " • Run Both – shows separate results for both adjustments",
  sep = "\n")
help_apply_college_adj   <- "Subtract coef × modern college‑educated share from productivity."
help_college_coeff       <- "Adjusts modern productivity for human capital spillovers related to the share of workers with a college degree based on Enrico Moretti, *Workers' education, spillovers, and productivity: evidence from plant-level production functions*, American Economic Review, 2004. Moretti obtains estimates between 0.5 and 0.7 in his most flexible specifications (Table 3, column 10)."
help_mining_filter_active<- "Exclude geographic units whose share of mining in GDP exceeds the threshold below."
help_mining_threshold    <- "Drops all geographic units with a share of mining in GDP above the chosen level."
help_se_spec             <- paste(
  "Standard‑error option:",
  " • Cluster by historical instrument – clusters by common historical instrument.",
  " • Cluster by state – clusters at the modern state level. ",
  " • Spatial correlation (Conley standard errors) – distance based correction using uniform kernel and a distance cutoff (distance cutoff chosen below).",
  " • Robust – heteroskedasticity robust standard errors.",
  sep = "\n")

# Spatial standard error settings
help_spatial_cutoff      <- "Distance in km beyond which spatial correlation is set to zero. Uses uniform kernel with constant weight within cutoff distance."
help_spatial_kernel      <- "Uniform kernel with constant weight within cutoff distance."

# MSA‑specific settings
help_msa_instrument_type     <- paste(
  "How to construct the historical density instrument:",
  " Overlap – maximum population density among all historical counties with at least X% of their territory overlapping with the modern geographic unit (X% is chosen below). ",
  sep = "\n")
help_msa_overlap_pct         <- "Minimum % of a historical county's area that must overlap with the MSA."

# County‑specific settings
help_county_msa_restriction  <- "Keep only counties that belong to a modern MSA."
help_county_instrument_type  <- paste(
  "Historical density instrument construction:",
  " • Max density overlap  – maximum population density among all historical counties with at least X% of their territory overlapping with the modern geographic unit (X% is chosen below). ",
  " • Weighted density overlap – average population density weighted by overlap using all historical counties with at least X% of their territory overlapping with the modern geographic unit (X% is chosen below)",
  sep = "\n")
help_county_overlap_threshold <- "Minimum % of a historical county's area that must overlap with the modern county"

# --- Helper for labelled inputs ----------------------------------------------

labeledInput <- function(inputId, labelText, inputUI, helpId, helpText) {
  div(
    class = "labeled-input-container",
    tags$label(labelText, `for` = inputId, class = "control-label"),
    div(class = "input-button-row", inputUI, actionButton(helpId, NULL, icon = icon("question-circle"), class = "help-btn")),
    conditionalPanel(
      condition = glue("input.{helpId} % 2 == 1"),
      div(class = "help-text", helpText)
    )
  )
}

# ============================================================================
# 5. User Interface (UI) -------------------------------------------------------
# ============================================================================
# Geographic controls use the names and units in the two prepared datasets.
format_estimate <- function(value) {
  if (!is.finite(value)) return("—")
  sprintf("%.4f", value)
}

scale_model_controls <- function(data, selected) {
  for (variable in intersect(selected, c("gaez_wheat_suit", "gaez_maize_suit")))
    data[[variable]] <- data[[variable]] / 1000
  data
}

resolve_geo_controls <- function(geo_groups = character(), water_groups = character(), water_year = 1820) {
  land <- list(terrain = c("rugged_mean", "elev_mean"),
               crop = c("gaez_wheat_suit", "gaez_maize_suit"),
               climate = c("tjan", "tjul", "precip"))
  water_year <- as.integer(water_year)
  if (length(water_year) != 1L || is.na(water_year) || !water_year %in% seq(1790, 1860, 10))
    stop("Choose a water-access year from 1790 to 1860, in ten-year steps.")
  water <- list(shoreline = c("ocean_access10", "lakes_access10"),
                historical_access = paste0(c("river_access_", "canal_access_"), water_year),
                distance = c("ln1p_dist_ocean", "ln1p_dist_lakes",
                             paste0(c("ln1p_dist_river_", "ln1p_dist_canal_"), water_year)),
                portage = "portage_access10")
  if (any(!geo_groups %in% names(land)) || any(!water_groups %in% names(water)))
    stop("An unknown geographic control group was selected.")
  unique(unname(unlist(c(land[geo_groups], water[water_groups]), use.names = FALSE)))
}

control_label <- function(variable) {
  if (variable == "ch_elasticity") return("Ciccone–Hall elasticity (theta − 1)")
  labels <- c(fit_RHS = "Log employment density (instrumented)", RHS = "Log employment density",
              instrument = "Historical density instrument", `(Intercept)` = "Constant",
              water_1820 = "Legacy water access, 1820", railroads_1840 = "Railroads, 1840",
              railroads_1850 = "Railroads, 1850", railroads_1861 = "Railroads, 1861",
              rugged_mean = "Terrain ruggedness (m)", elev_mean = "Mean elevation (m)",
              gaez_wheat_suit = "Wheat suitability (per 1,000 points)",
              gaez_maize_suit = "Maize suitability (per 1,000 points)",
              tjan = "January temperature (°C)", tjul = "July temperature (°C)",
              precip = "Annual precipitation (mm)", ocean_access10 = "Ocean access within 10 km",
              lakes_access10 = "Great Lakes access within 10 km",
              ln1p_dist_ocean = "Log(1 + ocean distance in km)",
              ln1p_dist_lakes = "Log(1 + Great Lakes distance in km)",
              portage_access10 = "Approximate portage access within 10 km")
  if (variable %in% names(labels)) return(unname(labels[[variable]]))
  legacy <- sub("1$", "", variable)
  if (legacy %in% c("water_1820", "railroads_1840", "railroads_1850", "railroads_1861"))
    return(unname(labels[[legacy]]))
  year <- sub(".*([0-9]{4})$", "\\1", variable)
  if (grepl("^river_access_", variable)) return(paste0("River access within 10 km, ", year))
  if (grepl("^canal_access_", variable)) return(paste0("Canal access within 10 km, ", year))
  if (grepl("^ln1p_dist_river_", variable)) return(paste0("Log(1 + river distance in km), ", year))
  if (grepl("^ln1p_dist_canal_", variable)) return(paste0("Log(1 + canal distance in km), ", year))
  variable
}

# The CH term uses county employment and land area within each MSA.
ch_index_terms <- function(theta, employment, area) {
  positive <- employment > 0
  log_density <- matrix(0, nrow(employment), ncol(employment))
  log_density[positive] <- log(employment[positive] / area[positive])
  log_weight <- matrix(-Inf, nrow(employment), ncol(employment))
  log_weight[positive] <- log(employment[positive]) + (theta - 1) * log_density[positive]
  largest <- apply(log_weight, 1L, max)
  weight <- exp(log_weight - largest)
  total <- rowSums(weight)
  derivative <- rowSums(weight * log_density) / total
  list(value = largest + log(total) - log(rowSums(employment)),
       derivative = derivative,
       second_derivative = rowSums(weight * (log_density - derivative)^2) / total)
}

fit_ch_model <- function(data, dep_var, controls_vec = character(), fe_part = "0",
                         method = c("IV", "OLS"), se_spec = "robust",
                         n_cols = grep("^nc[0-9]+$", names(data), value = TRUE),
                         a_cols = sub("^nc", "ac", n_cols)) {
  method <- match.arg(method)
  if (!se_spec %in% c("robust", "cluster_instrument", "cluster_state")) {
    stop("The CH model supports robust or clustered standard errors. Select one of these options.")
  }
  if (!length(n_cols) || length(n_cols) != length(a_cols)) stop("County components for the CH index are unavailable.")
  cluster_col <- switch(se_spec, cluster_instrument = "clusterID", cluster_state = "state_id", NULL)
  has_fe <- !is.null(fe_part) && !identical(as.character(fe_part), "0")
  variables <- unique(c(dep_var, controls_vec, n_cols, a_cols,
                        if (has_fe) fe_part, if (method == "IV") "instrument", cluster_col))
  absent <- setdiff(variables, names(data))
  if (length(absent)) stop(paste("CH model variables are unavailable:", paste(absent, collapse = ", ")))
  for (variable in controls_vec) {
    if (!is.numeric(data[[variable]])) {
      source <- trimws(as.character(data[[variable]]))
      converted <- suppressWarnings(as.numeric(source))
      if (any(is.na(converted) & !is.na(source) & !source %in% c("", "."))) {
        stop(paste("The CH control must contain numbers:", variable))
      }
      data[[variable]] <- converted
    }
  }
  valid <- complete.cases(data[variables])
  numeric_vars <- variables[vapply(data[variables], is.numeric, logical(1))]
  for (variable in numeric_vars) valid <- valid & is.finite(data[[variable]])
  employment <- as.matrix(data[n_cols])
  area <- as.matrix(data[a_cols])
  valid <- valid & rowSums(employment < 0, na.rm = TRUE) == 0 &
    rowSums(area <= 0, na.rm = TRUE) == 0 & rowSums(employment, na.rm = TRUE) > 0
  if ("ch_complete" %in% names(data)) valid <- valid & !is.na(data$ch_complete) & data$ch_complete == 1
  rows <- which(valid)
  if (!length(rows)) stop("No observations have complete county components and selected CH model variables.")
  d <- data[rows, , drop = FALSE]
  employment <- employment[rows, , drop = FALSE]
  area <- area[rows, , drop = FALSE]
  y <- d[[dep_var]]
  singleton_n <- 0L
  if (has_fe) {
    fixed_effect <- factor(d[[fe_part]])
    singleton_n <- as.integer(sum(table(fixed_effect) == 1L))
    X <- diag(nlevels(fixed_effect))[as.integer(fixed_effect), , drop = FALSE]
    colnames(X) <- paste0(".fe_", levels(fixed_effect))
  } else {
    X <- matrix(1, nrow(d), 1L, dimnames = list(NULL, "(Intercept)"))
  }
  X <- cbind(X, as.matrix(d[controls_vec]))
  storage.mode(X) <- "double"
  scales <- sqrt(colSums(X^2))
  scales[scales == 0] <- 1
  design_qr <- qr(sweep(X, 2L, scales, "/"), tol = 1e-10)
  selected <- sort(design_qr$pivot[seq_len(design_qr$rank)])
  omitted <- setdiff(colnames(X), colnames(X)[selected])
  if (length(omitted)) warning(paste("CH model omitted collinear controls:", paste(omitted, collapse = ", ")), call. = FALSE)
  X <- X[, selected, drop = FALSE]
  scales <- scales[selected]
  Xs <- sweep(X, 2L, scales, "/")
  design_qr <- qr(Xs, tol = 1e-10)
  n <- length(y)
  k <- ncol(X) + 1L
  if (n <= k) stop("Too few observations remain for the selected CH model and fixed effects.")
  partial <- function(x) qr.resid(design_qr, x)
  yr <- partial(y)
  index <- function(theta) ch_index_terms(theta, employment, area)
  objective <- function(theta) sum((yr - partial(index(theta)$value))^2)
  theta_grid <- sort(unique(c(1e-7, seq(0.05, 3, by = 0.05), 4, 8, 16, 32)))
  rss_grid <- vapply(theta_grid, objective, numeric(1))
  best <- which.min(rss_grid)
  if (best %in% c(1L, length(theta_grid))) stop("The CH least-squares solution is at the search boundary; this specification is not identified reliably.")
  nls_fit <- optimize(objective, interval = theta_grid[c(best - 1L, best + 1L)], tol = 1e-11)
  theta <- nls_fit$minimum
  if (method == "IV") {
    zr <- partial(d$instrument)
    if (sqrt(sum(zr^2)) <= 1e-10 * max(1, sqrt(sum(d$instrument^2)))) {
      stop("The historical instrument has no variation after the selected controls and fixed effects.")
    }
    zr <- zr / sqrt(sum(zr^2))
    moment <- function(theta) sum(zr * (yr - partial(index(theta)$value)))
    moment_grid <- vapply(theta_grid, moment, numeric(1))
    crossings <- which(head(moment_grid, -1L) * tail(moment_grid, -1L) <= 0)
    if (!length(crossings)) stop("The nonlinear IV moment has no positive-theta solution for this specification.")
    roots <- vapply(crossings, function(j) uniroot(moment, interval = theta_grid[c(j, j + 1L)], tol = 1e-11)$root, numeric(1))
    roots <- roots[!duplicated(round(roots, 8L))]
    if (length(roots) > 1L) warning("The nonlinear IV model has multiple solutions. The solution closest to the NLS estimate is shown.", call. = FALSE)
    theta <- roots[which.min(abs(roots - theta))]
  }
  terms <- index(theta)
  beta <- qr.coef(design_qr, y - terms$value) / scales
  fitted <- terms$value + as.vector(X %*% beta)
  residual <- y - fitted
  derivative <- cbind(ch_elasticity = terms$derivative, X)
  derivative_scale <- sqrt(colSums(derivative^2))
  Js <- sweep(derivative, 2L, derivative_scale, "/")
  if (qr(Js, tol = 1e-10)$rank < k) stop("The CH parameter is not identified separately from the selected controls and fixed effects.")
  if (method == "IV") {
    Z <- cbind(zr, Xs)
    bread <- crossprod(Z, Js)
    if (rcond(bread) < 1e-12) stop("The nonlinear IV derivative is too weak to estimate a reliable covariance matrix.")
    influence <- sweep((Z * residual) %*% t(solve(bread)), 2L, derivative_scale, "/")
  } else {
    bread <- crossprod(Js)
    influence <- sweep((Js * residual) %*% solve(bread), 2L, derivative_scale, "/")
  }
  groups <- NA_integer_
  if (!is.null(cluster_col)) {
    cluster <- factor(d[[cluster_col]])
    groups <- nlevels(cluster)
    if (groups < 2L) stop("At least two clusters are needed for clustered CH standard errors.")
    influence <- rowsum(influence, cluster, reorder = FALSE)
    correction <- if (method == "OLS") groups / (groups - 1) * (n - 1) / (n - k) else 1
    df <- if (method == "OLS") groups - 1L else Inf
  } else {
    correction <- if (method == "OLS") n / (n - k) else 1
    df <- if (method == "OLS") n - k else Inf
  }
  full_vcov <- crossprod(influence) * correction
  dimnames(full_vcov) <- list(colnames(derivative), colnames(derivative))
  full_coef <- c(ch_elasticity = theta - 1, beta)
  exposed <- !startsWith(names(full_coef), ".fe_")
  result <- list(coefficients = full_coef[exposed], covariance = full_vcov[exposed, exposed, drop = FALSE],
                 theta = theta, residuals = residual, fitted.values = fitted,
                 rows = rows, nobs = as.integer(n), df.residual = df,
                 parameter_count = k, method = method, se_spec = se_spec,
                 clusters = as.integer(groups), collin.var = omitted,
                 singleton_n = singleton_n,
                 inference = if (method == "IV") "Stata gmm: asymptotic normal inference; no small-sample covariance correction." else if (is.null(cluster_col)) "Stata nl: HC1 covariance; t inference with N minus parameter count degrees of freedom." else "Stata nl: cluster covariance with finite-sample correction; t inference with clusters minus one degrees of freedom.",
                 full_coefficients = full_coef, full_covariance = full_vcov,
                 index_value = terms$value, index_derivative = terms$derivative,
                 objective = sum(residual^2), iv_moment = if (method == "IV") moment(theta) else NULL)
  class(result) <- "ch_model"
  result
}

coef.ch_model <- function(object, ...) object$coefficients
vcov.ch_model <- function(object, ...) object$covariance
nobs.ch_model <- function(object, ...) object$nobs
residuals.ch_model <- function(object, ...) object$residuals
fitted.ch_model <- function(object, ...) object$fitted.values
confint.ch_model <- function(object, parm, level = 0.95, ...) {
  if (missing(parm)) parm <- names(coef(object))
  b <- coef(object)[parm]
  se <- sqrt(diag(vcov(object)))[parm]
  quantile <- if (is.finite(object$df.residual)) qt((1 + level) / 2, object$df.residual) else qnorm((1 + level) / 2)
  result <- cbind(b - quantile * se, b + quantile * se)
  colnames(result) <- paste0(format(100 * c((1 - level) / 2, (1 + level) / 2)), " %")
  result
}
ch_coeftable <- function(model) {
  estimate <- coef(model)
  se <- sqrt(diag(vcov(model)))
  statistic <- estimate / se
  p <- if (is.finite(model$df.residual)) 2 * pt(abs(statistic), df = model$df.residual, lower.tail = FALSE) else 2 * pnorm(abs(statistic), lower.tail = FALSE)
  cbind(Estimate = estimate, `Std. Error` = se, `t value` = statistic, `Pr(>|t|)` = p)
}


app_model_table <- function(model) {
  if (inherits(model, "ch_model")) return(ch_coeftable(model))
  fixest::coeftable(model)
}
app_model_rows <- function(model) {
  if (inherits(model, "ch_model")) return(model$rows)
  fixest::obs(model)
}
app_model_se <- function(model) sqrt(diag(stats::vcov(model)))
app_model_pvalue <- function(model) app_model_table(model)[, 4]
model_description <- function(details) {
  if (identical(details$density_measure, "CH")) {
    return(if (details$analysis_type == "IV") "Ciccone–Hall · nonlinear IV (GMM)" else "Ciccone–Hall · nonlinear least squares")
  }
  details$analysis_type
}

model_coefficients <- function(details) {
  rows <- lapply(names(details$models), function(model_name) {
    model <- details$models[[model_name]]
    table <- app_model_table(model)
    interval <- stats::confint(model, level = .95)
    terms <- rownames(table)
    data.frame(model = model_name, term = terms,
               label = vapply(terms, control_label, character(1)),
               estimate = table[, 1], std_error = table[, 2],
               statistic = table[, 3], p_value = table[, 4],
               conf_low = interval[terms, 1], conf_high = interval[terms, 2],
               observations = stats::nobs(model), geography = details$analysis_level,
               year = details$year_modern, method = model_description(details),
               density_measure = details$density_measure,
               stringsAsFactors = FALSE, row.names = NULL)
  })
  do.call(rbind, rows)
}

main_coefficient <- function(details) {
  if (identical(details$density_measure, "CH")) return("ch_elasticity")
  switch(details$analysis_type, IV = "fit_RHS", OLS = "RHS", "instrument")
}

result_plot <- function(details) {
  estimates <- model_coefficients(details)
  estimates <- estimates[estimates$term == main_coefficient(details), , drop = FALSE]
  if (!nrow(estimates)) stop("The density coefficient could not be estimated for this specification.")
  estimates$model <- factor(estimates$model, levels = rev(names(details$models)))
  ggplot2::ggplot(estimates, ggplot2::aes(x = estimate, y = model)) +
    ggplot2::geom_vline(xintercept = 0, color = "#999999", linetype = "dashed", linewidth = .5) +
    ggplot2::geom_segment(ggplot2::aes(x = conf_low, xend = conf_high, yend = model),
                          color = "#007BFF", linewidth = 1.5) +
    ggplot2::geom_point(color = "#000000", fill = "#ffffff", shape = 21, size = 4.5, stroke = 1.6) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = .65)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .12)) +
    ggplot2::labs(x = if (details$analysis_type == "First-stage Regression") "Historical density coefficient" else if (identical(details$density_measure, "CH")) "Ciccone–Hall elasticity (theta − 1)" else "Employment density coefficient",
                  y = NULL, title = paste(details$analysis_level, "·", details$year_modern, "·", model_description(details)),
                  subtitle = "Point estimates and 95% confidence intervals",
                  caption = "Intervals use the selected standard-error specification.") +
    ggplot2::theme_minimal(base_size = 14, base_family = "sans") +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                    panel.grid.major.x = ggplot2::element_line(color = "#e6e6e6"),
                    plot.title = ggplot2::element_text(face = "bold", color = "#000000", size = 17),
                    plot.subtitle = ggplot2::element_text(color = "#555555", margin = ggplot2::margin(b = 22)),
                    axis.text = ggplot2::element_text(color = "#222222"),
                    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 14)),
                    plot.caption = ggplot2::element_text(color = "#555555", hjust = 0, margin = ggplot2::margin(t = 20)),
                    plot.margin = ggplot2::margin(20, 22, 16, 14),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA))
}

ui <- fluidPage(title = "Agglomeration Effects USA",
  titlePanel(div(class = "beamer-title",
    h2("Regression Interface: MSA and County Level Analysis"),
    p(style="margin-top: 10px; font-size: 14px;", 
      "Code, data and maps available at: ",
      a(icon("github"), " GitHub",
        href="https://github.com/AlexGoeppert/AgglomerationEffectsUSA", 
        target="_blank", style="color: #007BFF;")),
    div(class = "presentation-option", checkboxInput("presentation_mode", "Presentation view", FALSE))
  ), windowTitle = "Agglomeration Effects USA"),
  
  # Page and table styles
  tags$head(tags$style(HTML("
    .labeled-input-container{margin-bottom:15px}
    .input-button-row{display:flex;align-items:center;gap:8px}
    .input-button-row .shiny-input-container{flex-grow:1;width:auto!important;margin-bottom:0}
    .labeled-input-container .shiny-options-group{padding-top:5px}
    .help-btn{background:none;border:none;color:#007BFF;font-size:20px;padding:0 5px;cursor:pointer;align-self:center;line-height:1;transition:color .2s ease,transform .2s ease}
    .help-btn:hover{color:#0056b3;transform:scale(1.1)}
    .help-btn:focus{outline:none;box-shadow:none}
    .help-text{background-color:#f8f9fa;border-left:4px solid #007BFF;padding:10px 15px;margin-top:8px;font-size:13px;color:#495057;border-radius:0 4px 4px 0}
    #instrument_map_render img{max-width:100%;height:auto;border:1px solid #ddd;border-radius:4px}
    

    .regression-table {
      font-family: 'Computer Modern', 'Times New Roman', 'Times', serif;
      font-size: 16px;
      margin: 20px auto;
      border-collapse: collapse;
      width: 100%;
      max-width: 900px;
      background-color: white;
    }
    .regression-table th {
      color: #000;
      font-weight: normal;
      text-align: center;
      padding: 10px 14px;
      border-top: 2px solid #000;
      border-bottom: 1px solid #000;
      border-left: none;
      border-right: none;
      background-color: white;
    }
    .regression-table td {
      padding: 5px 14px;
      text-align: center;
      border: none;
      background-color: white;
    }
    .regression-table .row-label {
      text-align: left;
      font-weight: normal;
      background-color: white;
      padding-left: 10px;
    }
    .regression-table .coefficient {
      font-weight: normal;
      color: #000;
      font-size: 16px;

    }
    .regression-table .se {
      font-weight: normal;
      color: #000;
      font-size: 16px;
    }
    .regression-table .stats {
      background-color: white;
      font-weight: normal;
      color: #000;
      font-size: 16px;
    }
    .regression-table .top-border {
      border-top: 1px solid #000;
    }
    .regression-table .bottom-border {
      border-bottom: 2px solid #000;
    }
    .table-notes {
      font-family: 'Computer Modern', 'Times New Roman', 'Times', serif;
      font-size: 12px;
      font-style: italic;
      margin-top: 5px;
      text-align: left;
      max-width: 900px;
      margin-left: auto;
      margin-right: auto;
    }
    .analysis-details {
      background-color: #F0F0F0;
      padding: 15px;
      border-radius: 5px;
      margin-top: 20px;
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      font-size: 14px;
      color: #495057;
      max-width: 900px;
      margin-left: auto;
      margin-right: auto;
    }
    .analysis-details h5 {
      margin-top: 0;
      color: #343a40;
      font-weight: 600;
    }

    body{background:#fff;color:#000}
    a{color:#007BFF}a:hover{color:#0056b3}
    .btn-primary{background-color:#007BFF;border-color:#007BFF}
    .btn-primary:hover,.btn-primary:focus,.btn-primary:active{background-color:#0056b3!important;border-color:#0056b3!important}
    input[type=checkbox]{accent-color:#007BFF}
    .irs-bar,.irs-bar-edge,.irs-single,.irs-from,.irs-to{background:#007BFF!important;border-color:#007BFF!important}
    .container-fluid{padding-left:25px;padding-right:25px}
    .beamer-title{border-bottom:2px solid #007BFF;padding-bottom:8px;margin-bottom:14px}
    .beamer-title h2{color:#000;font-family:Georgia,'Times New Roman',serif;font-weight:normal}
    .well{background:#fff;border-color:#ddd;box-shadow:none}
    .well h4{margin-top:19px;margin-bottom:14px;font-size:17px;font-family:Georgia,'Times New Roman',serif;color:#000;border-bottom:1px solid #ddd;padding-bottom:6px}
    .well h4:first-child{margin-top:0}
    .control-label{line-height:1.45}
    .presentation-option{margin:10px 0 16px;font-size:13px}
    .presentation-option .checkbox{margin:0}
    .presentation-option .shiny-input-container{margin:0}
    .geo-control-note,.sample-note,.estimate-caption{font-size:12px;line-height:1.6;color:#555;margin:10px 0}
    .sample-note{padding:8px 12px;background:#f8f9fa;border-left:3px solid #007BFF}
    .download-row{display:flex;flex-wrap:wrap;gap:8px;margin:15px 0 20px}
    .download-row .btn{font-size:12px;padding:6px 10px}
    .optional-chart{margin:20px 0}
    .optional-chart h4{font-size:17px}
    .data-notes-panel{margin:20px 0;font-size:13px;line-height:1.6}
    .data-notes-panel>summary{cursor:pointer;color:#007BFF;font-weight:600;margin-bottom:10px}
    .data-notes-panel .note-card{padding:12px;background:#f8f9fa;border:1px solid #eee;margin:10px 0}
    #results_table{overflow-x:auto}
    body.presentation-mode .original-sidebar{display:none}
    body.presentation-mode .original-results{width:100%}
    @media(max-width:767px){.container-fluid{padding-left:15px;padding-right:15px}.regression-table{font-size:14px}.regression-table th,.regression-table td{padding-left:8px;padding-right:8px}}
    @media print{.original-sidebar,.presentation-option,.download-row{display:none!important}.original-results{width:100%!important}.container-fluid{padding:0}.regression-table{page-break-inside:avoid}a[href]:after{content:none!important}}
  ")),
            tags$script(HTML("
    // Simple validation for text inputs used as numeric inputs
    $(document).ready(function() {
      $('#run_analysis').closest('.col-sm-4').addClass('original-sidebar');
      $('#results_table').closest('.col-sm-8').addClass('original-results');
      function updatePresentation() {
        document.body.classList.toggle('presentation-mode', $('#presentation_mode').prop('checked') === true);
        setTimeout(function() { $(window).trigger('resize'); }, 100);
      }
      $(document).on('change', '#presentation_mode', updatePresentation);
      $(document).on('shiny:connected', updatePresentation);
      // Add input validation and formatting
      var numericTextInputs = ['#msa_college_coeff', '#msa_mining_threshold', '#msa_spatial_cutoff', 
                               '#county_college_coeff', '#county_mining_threshold', '#county_spatial_cutoff'];
      
      numericTextInputs.forEach(function(selector) {
        $(document).on('input', selector, function() {
          var value = this.value;
          // Allow numbers, periods, and commas, but replace commas with periods
          value = value.replace(/[^0-9.,]/g, '').replace(/,/g, '.');
          // Ensure only one period
          var parts = value.split('.');
          if (parts.length > 2) {
            value = parts[0] + '.' + parts.slice(1).join('');
          }
          this.value = value;
        });
        
        $(document).on('blur', selector, function() {
          var value = parseFloat(this.value.replace(/,/g, '.'));
          if (!isNaN(value)) {
            this.value = value.toString();
          }
        });
      });
    });
  "))),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 # ── 1. Master Configuration ───────────────────────────
                 h4("1. Master Configuration"),
                 labeledInput("analysis_level", "Select Analysis Level:",
                              selectInput("analysis_level", NULL, c("MSA", "County")),
                              "help_analysis_level", help_analysis_level),
                 labeledInput("year_modern", "Select Modern Year:",
                              selectInput("year_modern", NULL, seq(2022, 2001, -1), 2010),
                              "help_year_modern", help_year_modern),
                 div(style="margin-top:20px;", actionButton("run_analysis", "Run Analysis", icon=icon("play-circle"), class="btn-primary btn-lg", width="100%")),
                 hr(),
                 
                 # Display options
                 h4("Display Options"),
                 checkboxInput("show_map", "Display Instrument Map", value = TRUE),
                 checkboxInput("show_coefficient_chart", "Display Coefficient Chart", value = FALSE),
                 conditionalPanel("input.show_map == true", sliderInput("map_size", "Map Size:", min = 25, max = 200, value = 75, post = "%")),
                 hr(),
                 
                 # ── 2. Analysis Settings ───────────────────────────────
                 h4("2. Analysis Settings"),
                 labeledInput("analysis_type", "Analysis Method:",
                              selectInput("analysis_type", NULL, c("IV", "OLS", "First-stage Regression"), "IV"),
                              "help_analysis_type", help_analysis_type),
                 hr(),
                 
                 # ── 3. Fixed Effects & Sample Scope ───────────────────
                 h4("3. Fixed Effects & Sample Scope"),
                 div(class="input-button-row", style="margin-bottom:10px;",
                     checkboxInput("use_fe", "Use State Fixed Effects", value = TRUE),
                     actionButton("help_use_fe", NULL, icon = icon("question-circle"), class = "help-btn")),
                 conditionalPanel("input.help_use_fe % 2 == 1", div(class="help-text", help_use_fe)),
                 conditionalPanel("input.use_fe == true",
                                  labeledInput("fe_type", "Fixed Effects Type:",
                                               selectInput("fe_type", NULL, c("Historical"="historical", "Modern"="modern"), "historical"),
                                               "help_fe_type", help_fe_type)),
                 labeledInput("sample_scope", "Historical Territory:",
                              selectInput("sample_scope", NULL, c("Historical States Only "="states_only", "HistoricalStates & Territories"="states_territories"), "states_only"),
                              "help_sample_scope", help_sample_scope),
                 hr(),
                 
                 # ── MSA‑specific panels ────────────────────────────────
                 conditionalPanel(
                   condition = "input.analysis_level == 'MSA'",
                   h4("4. MSA Specific Settings"),
                   selectInput("msa_density_measure", "Density measure:",
                     c("MSA average density" = "average", "Ciccone–Hall index" = "CH"), "average"),
                   conditionalPanel("input.msa_density_measure == 'CH'",
                     p(class = "help-block", "Uses employment density within each MSA's county units. OLS runs nonlinear least squares; IV runs nonlinear GMM.")),
                   labeledInput("msa_sectors", "Sector:",
                                selectInput("msa_sectors", NULL, c("All"=1, "Private"=2, "Manufacturing"=3, "Private non-farm"=4, "Private non-farm/mining"=5), 1),
                                "help_msa_sectors", help_sectors),
                   labeledInput("msa_sample_year", "Year of the Historical Territory:",
                                selectInput("msa_sample_year", NULL, seq(1790, 1860, 10), 1790),
                                "help_msa_sample_year", help_sample_year),
                   labeledInput("msa_iv_year", "Year of the Historical Population Density (Historical Census Year):",
                                selectInput("msa_iv_year", NULL, seq(1790, 1860, 10), 1840),
                                "help_msa_iv_year", help_iv_year),
                   labeledInput("msa_instrument_type", "Match of Historical to Modern Geographic Units:",
                                selectInput("msa_instrument_type", NULL, c("overlap")),
                                "help_msa_instrument_type", help_msa_instrument_type),
                   labeledInput("msa_overlap_pct", "Overlap %:",
                                selectInput("msa_overlap_pct", NULL, c(5,10,20,30,40,50,60,70,80,90), 5),
                                "help_msa_overlap_pct", help_msa_overlap_pct),
                   labeledInput("msa_controls", "Control Variables:",
                                checkboxGroupInput("msa_controls", NULL, c("Water Access 1820"="water_1820", "Railroads 1840"="railroads_1840", "Railroads 1850"="railroads_1850", "Railroads 1861"="railroads_1861"), c("water_1820", "railroads_1840")),
                                "help_msa_controls", help_controls),
                   labeledInput("msa_schooling_adj", "Modern Adjustment for Human Capital",
                                selectInput("msa_schooling_adj", NULL, c("None"=0, "Same Return"=1, "Specific Return"=2, "Run Both"=3), 3),
                                "help_msa_schooling_adj", help_schooling_adj),
                   div(class="input-button-row", style="margin-bottom:15px;",
                       checkboxInput("msa_apply_college_adj", "Apply College Share Adjustment", value = TRUE),
                       actionButton("help_msa_apply_college_adj", NULL, icon=icon("question-circle"), class="help-btn")),
                   conditionalPanel("input.help_msa_apply_college_adj % 2 == 1", div(class="help-text", help_apply_college_adj)),
                   labeledInput("msa_college_coeff", "College Share Coefficient:",
                                textInput("msa_college_coeff", NULL, "0.75", placeholder = "0.75"),
                                "help_msa_college_coeff", help_college_coeff),
                   div(class="input-button-row", style="margin-bottom:15px;",
                       checkboxInput("msa_mining_filter_active", "Filter by Mining Share", value = FALSE),
                       actionButton("help_msa_mining_filter_active", NULL, icon=icon("question-circle"), class="help-btn")),
                   conditionalPanel("input.help_msa_mining_filter_active % 2 == 1", div(class="help-text", help_mining_filter_active)),
                   labeledInput("msa_mining_threshold", "Max Mining Share:",
                                textInput("msa_mining_threshold", NULL, "0.01", placeholder = "0.01"),
                                "help_msa_mining_threshold", help_mining_threshold),
                   labeledInput("msa_se_spec", "Standard Errors:",
                                selectInput("msa_se_spec", NULL, c("Cluster on Instrument"="cluster_instrument", "Cluster on State"="cluster_state", "Spatial (Conley)"="spatial", "Robust"="robust"), "cluster_instrument"),
                                "help_msa_se_spec", help_se_spec),
                   conditionalPanel("input.msa_se_spec == 'spatial'",
                                    labeledInput("msa_spatial_cutoff", "Spatial Cutoff (km):",
                                                 textInput("msa_spatial_cutoff", NULL, "100", placeholder = "100"),
                                                 "help_msa_spatial_cutoff", help_spatial_cutoff))
                 ),
                 
                 # ── County‑specific panels ─────────────────────────────
                 conditionalPanel(
                   condition = "input.analysis_level == 'County'",
                   h4("4. County Specific Settings"),
                   labeledInput("county_sectors", "Sector:",
                                selectInput("county_sectors", NULL, c("All"=1, "Private"=2, "Manufacturing"=3, "Private non-farm"=4, "Private non-farm/mining"=5), 1),
                                "help_county_sectors", help_sectors),
                   div(class="input-button-row", style="margin-bottom:15px;",
                       checkboxInput("county_msa_restriction", "Restrict to counties in an MSA", value = FALSE),
                       actionButton("help_county_msa_restriction", NULL, icon=icon("question-circle"), class="help-btn")),
                   conditionalPanel("input.help_county_msa_restriction % 2 == 1", div(class="help-text", help_county_msa_restriction)),
                   labeledInput("county_sample_year", "Year of the Historical Territory:",
                                selectInput("county_sample_year", NULL, seq(1790, 1860, 10), 1790),
                                "help_county_sample_year", help_sample_year),
                   labeledInput("county_iv_year", "Year of the Historical Population Density (Historical Census Year):",
                                selectInput("county_iv_year", NULL, seq(1790, 1860, 10), 1840),
                                "help_county_iv_year", help_iv_year),
                   labeledInput("county_instrument_type", "Match of Historical to Modern Geographic Units:",
                                selectInput("county_instrument_type", NULL, c("Max Density Overlap"="max_density_overlap", "Weighted Density Overlap"="weighted_density_overlap"), "max_density_overlap"),
                                "help_county_instrument_type", help_county_instrument_type),
                   labeledInput("county_overlap_threshold", "Overlap Threshold %:",
                                selectInput("county_overlap_threshold", NULL, c(5,10,20,30,40,50,60,70,80,90), 5),
                                "help_county_overlap_threshold", help_county_overlap_threshold),
                   labeledInput("county_controls", "Control Variables:",
                                checkboxGroupInput("county_controls", NULL, c("Water Access 1820"="water_1820", "Railroads 1840"="railroads_1840", "Railroads 1850"="railroads_1850", "Railroads 1861"="railroads_1861")),
                                "help_county_controls", help_controls),
                   labeledInput("county_schooling_adj", "Modern Adjustment for Human Capital",
                                selectInput("county_schooling_adj", NULL, c("None"=0, "Same Return"=1, "Specific Return"=2, "Run Both"=3), 0),
                                "help_county_schooling_adj", help_schooling_adj),
                   div(class="input-button-row", style="margin-bottom:15px;",
                       checkboxInput("county_apply_college_adj", "Apply College Share Adjustment", value = FALSE),
                       actionButton("help_county_apply_college_adj", NULL, icon=icon("question-circle"), class="help-btn")),
                   conditionalPanel("input.help_county_apply_college_adj % 2 == 1", div(class="help-text", help_apply_college_adj)),
                   labeledInput("county_college_coeff", "College Share Coefficient:",
                                textInput("county_college_coeff", NULL, "0.75", placeholder = "0.75"),
                                "help_county_college_coeff", help_college_coeff),
                   div(class="input-button-row", style="margin-bottom:15px;",
                       checkboxInput("county_mining_filter_active", "Filter by Mining Share", value = FALSE),
                       actionButton("help_county_mining_filter_active", NULL, icon=icon("question-circle"), class="help-btn")),
                   conditionalPanel("input.help_county_mining_filter_active % 2 == 1", div(class="help-text", help_mining_filter_active)),
                   labeledInput("county_mining_threshold", "Max Mining Share:",
                                textInput("county_mining_threshold", NULL, "0.01", placeholder = "0.01"),
                                "help_county_mining_threshold", help_mining_threshold),
                   labeledInput("county_se_spec", "Standard Errors:",
                                selectInput("county_se_spec", NULL, c("Cluster on Instrument"="cluster_instrument", "Cluster on State"="cluster_state", "Spatial (Conley)"="spatial", "Robust"="robust"), "cluster_instrument"),
                                "help_county_se_spec", help_se_spec),
                   conditionalPanel("input.county_se_spec == 'spatial'",
                                    labeledInput("county_spatial_cutoff", "Spatial Cutoff (km):",
                                                 textInput("county_spatial_cutoff", NULL, "100", placeholder = "100"),
                                                 "help_county_spatial_cutoff", help_spatial_cutoff))
                 ),
                 hr(),
                 h4("Geographic Controls"),
                 checkboxGroupInput("geo_controls", "Terrain, Soil and Climate:",
                   c("Ruggedness and Elevation" = "terrain", "Wheat and Maize Suitability" = "crop", "Temperature and Precipitation" = "climate"), selected = character()),
                 checkboxGroupInput("water_controls", "Water Access:",
                   c("Ocean and Great Lakes Access" = "shoreline", "Historical River and Canal Access" = "historical_access", "Distance to Shores and Waterways" = "distance", "Approximate Portage Access" = "portage"), selected = character()),
                 conditionalPanel("(input.water_controls || []).indexOf('historical_access') >= 0 || (input.water_controls || []).indexOf('distance') >= 0",
                   selectInput("water_year", "Waterway Reference Year:", seq(1790, 1860, 10), 1820)),
                 p(class = "geo-control-note", "Terrain and crop controls use area averages. Climate covers 1991–2020. Access means within 10 km; shores use modern boundaries, while rivers and canals use historical dates. Portage access is approximate. Geographic coverage is mainly the contiguous U.S.")
    ), # sidebarPanel
    
    # Results
    mainPanel(
      h3(textOutput("results_header")),
      uiOutput("sample_note"),
      hr(),
      withSpinner(htmlOutput("results_table"), type = 6, color = "black"),
      conditionalPanel("input.run_analysis > 0",
        div(class = "download-row",
          downloadButton("download_coefficients", "Coefficients (CSV)"),
          downloadButton("download_results", "Results (HTML)"),
          conditionalPanel("input.show_coefficient_chart == true", downloadButton("download_plot", "Chart (PNG)"))
        )
      ),
      conditionalPanel("input.show_coefficient_chart == true && input.run_analysis > 0",
        div(class = "optional-chart",
          h4("Coefficient Estimates"),
          p(class = "estimate-caption", "Points show the estimated density effect; lines show 95% confidence intervals."),
          withSpinner(plotOutput("effect_plot", height = "360px"), type = 6, color = "#007BFF")
        )
      ),
      conditionalPanel("input.run_analysis > 0",
        div(class = "configuration-toggle", style = "margin: 16px 0;",
          actionButton("toggle_configuration", "Show regression configuration", icon = icon("list"), class = "btn-sm")),
        conditionalPanel("input.toggle_configuration % 2 == 1", htmlOutput("analysis_details"))),
      conditionalPanel("input.run_analysis > 0",
        tags$details(class = "data-notes-panel", tags$summary("Geographic data and sources"), uiOutput("data_notes"))
      ),
      conditionalPanel(condition = "input.show_map == true", hr(), h3(textOutput("map_header")), uiOutput("map_ui"))
    )
  )
)

server <- function(input, output, session) {
  
  analysis_output <- reactiveVal(NULL)
  map_output <- reactiveVal(NULL)

  observeEvent(list(input$msa_density_measure, input$analysis_level), {
    ch <- identical(input$analysis_level, "MSA") && identical(input$msa_density_measure, "CH")
    methods <- if (ch) c("IV", "OLS") else c("IV", "OLS", "First-stage Regression")
    selected <- isolate(input$analysis_type)
    if (is.null(selected) || !selected %in% methods) selected <- "IV"
    updateSelectInput(session, "analysis_type", choices = methods, selected = selected)
    standard_errors <- c("Cluster on Instrument" = "cluster_instrument",
      "Cluster on State" = "cluster_state", "Spatial (Conley)" = "spatial", "Robust" = "robust")
    if (ch) standard_errors <- standard_errors[standard_errors != "spatial"]
    selected_se <- isolate(input$msa_se_spec)
    if (is.null(selected_se) || !selected_se %in% standard_errors) selected_se <- "cluster_instrument"
    updateSelectInput(session, "msa_se_spec", choices = standard_errors, selected = selected_se)
  }, ignoreInit = TRUE)

  estimated_sample <- function(model, details = analysis_output()) {
    details$data_for_fs[app_model_rows(model), , drop = FALSE]
  }

  instrument_wald_f <- function(model) {
    b <- coef(model)["instrument"]
    uncertainty <- se(model)["instrument"]
    if (length(b) != 1L || !is.finite(b) || !is.finite(uncertainty) || uncertainty <= 0) return(NA_real_)
    unname((b / uncertainty)^2)
  }
  
  # Simple helper function to safely convert text inputs to numeric
  safe_numeric <- function(value, default = 0) {
    if (is.null(value) || value == "" || is.na(value)) return(default)
    
    # Replace commas with periods and convert to numeric
    clean_value <- gsub(",", ".", as.character(value))
    result <- suppressWarnings(as.numeric(clean_value))
    
    if (is.na(result)) return(default)
    return(result)
  }
  
  observeEvent(input$run_analysis, {
    
    # --- 1. Run Regression Analysis ---
    reg_results <- tryCatch({
      withProgress(message = 'Running Analysis...', value = 0, {
        
        isolate({
          analysis_level <- input$analysis_level
          year_modern <- as.numeric(input$year_modern)
          analysis_type <- input$analysis_type
          density_measure <- if (analysis_level == "MSA" && identical(input$msa_density_measure, "CH")) "CH" else "average"
          if (density_measure == "CH" && !analysis_type %in% c("IV", "OLS")) stop("Choose IV or OLS for the Ciccone–Hall model.")
          use_fe <- input$use_fe
          fe_type <- input$fe_type
          sample_scope <- input$sample_scope
        })
        
        incProgress(0.1, detail = "Configuring analysis...")
        
        suffix <- if (sample_scope == "states_only") "_s" else ""
        
        if (analysis_level == "MSA") {
          isolate({
            sectors <- as.numeric(input$msa_sectors)
            sample_year <- as.numeric(input$msa_sample_year)
            iv_year <- as.numeric(input$msa_iv_year)
            instrument_type <- input$msa_instrument_type
            overlap_pct <- input$msa_overlap_pct
            controls_vec <- input$msa_controls
            schooling_adj <- as.numeric(input$msa_schooling_adj)
            apply_college_adj <- input$msa_apply_college_adj
            college_coeff <- safe_numeric(input$msa_college_coeff, 0.75)
            se_spec <- input$msa_se_spec
            mining_filter_active <- input$msa_mining_filter_active
            mining_threshold <- safe_numeric(input$msa_mining_threshold, 0.01)
            spatial_cutoff <- safe_numeric(input$msa_spatial_cutoff, 100)
          })
          
          df <- msa_data %>%
            filter(year == year_modern) %>%
            rename(lat = lat_DD, lon = lon_DD, state_id = modern_state_fe,
                   avg_schooling = msa_schooling_09, college_share = college_share_09)
          
          # Simplified logic - only overlap for MSA
          sample_col <- glue("MSAol_{sample_year}_{overlap_pct}{suffix}")
          instr_col  <- glue("MSAol_{iv_year}_{overlap_pct}{suffix}")
          
          # Apply territory filtering for states-only analysis
          if (sample_scope == "states_only") {
            fe_var_name_full <- glue("hist_state_fe_{iv_year}")  # without _s suffix
            
            # Try to find MSA identifier column
            msa_id_col <- NULL
            potential_id_cols <- c("msafips", "msaid", "msa_code", "msaname", "msa_id", "cbsacode")
            for (col in potential_id_cols) {
              if (col %in% names(df)) {
                msa_id_col <- col
                break
              }
            }
            
            if (!is.null(msa_id_col) && fe_var_name_full %in% names(msa_data)) {
              territory_msa_ids <- get_territory_exclusions(
                data = msa_data %>% filter(year == year_modern),
                fe_column_name = fe_var_name_full,
                id_column = msa_id_col
              )
              
              if (length(territory_msa_ids) > 0) {
                df <- df %>%
                  filter(!.data[[msa_id_col]] %in% territory_msa_ids)
              }
            }
          }
          
          # Remove MSAs without fixed effects when FEs are activated
          if (use_fe) {
            if (fe_type == "modern") {
              # Filter out MSAs without modern state fixed effects
              df <- df %>%
                filter(!is.na(state_id) & state_id != "" & state_id != 0)
              
              # Also remove District of Columbia (not a state)
              df <- df %>%
                filter(!str_detect(tolower(as.character(state_id)), "district|columbia|\\bdc\\b"))
              
            } else {
              # Filter out MSAs without historical state fixed effects
              fe_var_name <- glue("hist_state_fe_{iv_year}{suffix}")
              if (fe_var_name %in% names(df)) {
                df <- df %>%
                  filter(!is.na(.data[[fe_var_name]]) & .data[[fe_var_name]] != "" & .data[[fe_var_name]] != 0)
                
                # Also remove District of Columbia (not a state)
                df <- df %>%
                  filter(!str_detect(tolower(as.character(.data[[fe_var_name]])), "district|columbia|\\bdc\\b"))
              }
            }
          }
          
          # MODIFIED: Use total employment (employment) for RHS across all sectors
          df <- switch(
            as.character(sectors),
            "1" = df %>% mutate(RHS = log(employment / (msaarea / 4046.8)), LHS = ln_output_worker),
            "2" = df %>% mutate(RHS = log(employment / (msaarea / 4046.8)), LHS = ln_output_worker_private),
            "3" = df %>% mutate(RHS = log(employment / (msaarea / 4046.8)), LHS = log(gmp_indstryid_12 / emply_indryid_500)),
            "4" = df %>% mutate(RHS = log(employment / (msaarea / 4046.8)), LHS = log((gmp_private_industry - gmp_agriculture) / employment_private_nonfarm)),
            "5" = df %>% mutate(RHS = log(employment / (msaarea / 4046.8)),
                                LHS = log((gmp_private_industry - gmp_agriculture - gmp_mining) / (employment_private_nonfarm - employment_mining))),
            stop("Invalid MSA sector specified.")
          )
          
          if (mining_filter_active) {
            df <- df %>%
              mutate(temp_mining_share = gmp_mining / gmp) %>%
              filter(temp_mining_share <= mining_threshold | is.na(temp_mining_share)) %>%
              select(-temp_mining_share)
          }
          
        } else { # County Logic
          isolate({
            sectors <- as.numeric(input$county_sectors)
            sample_year <- as.numeric(input$county_sample_year)
            iv_year <- as.numeric(input$county_iv_year)
            instrument_type <- input$county_instrument_type
            overlap_threshold <- input$county_overlap_threshold
            controls_vec <- input$county_controls
            schooling_adj <- as.numeric(input$county_schooling_adj)
            apply_college_adj <- input$county_apply_college_adj
            college_coeff <- safe_numeric(input$county_college_coeff, 0.75)
            se_spec <- input$county_se_spec
            mining_filter_active <- input$county_mining_filter_active
            mining_threshold <- safe_numeric(input$county_mining_threshold, 0.01)
            county_msa_restriction <- input$county_msa_restriction
            spatial_cutoff <- safe_numeric(input$county_spatial_cutoff, 100)
          })
          
          df <- county_data %>%
            filter(year == year_modern) %>%
            rename(state_id = modern_state_fe)
          
          df <- df %>%
            group_by(geofips) %>%
            mutate(avg_schooling = mean(avg_schooling_10_14, na.rm = TRUE)) %>%
            ungroup()
          
          if (county_msa_restriction) {
            if (!"county_partof_MSA" %in% names(df)) stop("Error: 'county_partof_MSA' column required for MSA restriction not found.")
            df <- df %>% filter(county_partof_MSA == 1)
          }
          
          # Apply territory filtering for states-only analysis
          if (sample_scope == "states_only") {
            fe_var_name_full <- glue("hist_state_fe_{iv_year}")  # without _s suffix
            
            # Try to find county identifier column
            county_id_col <- NULL
            potential_id_cols <- c("geofips", "fips", "county_fips", "countycode", "county_id")
            for (col in potential_id_cols) {
              if (col %in% names(df)) {
                county_id_col <- col
                break
              }
            }
            
            if (!is.null(county_id_col) && fe_var_name_full %in% names(county_data)) {
              territory_county_ids <- get_territory_exclusions(
                data = county_data %>% filter(year == year_modern),
                fe_column_name = fe_var_name_full,
                id_column = county_id_col
              )
              
              if (length(territory_county_ids) > 0) {
                df <- df %>%
                  filter(!.data[[county_id_col]] %in% territory_county_ids)
              }
            }
          }
          
          # Remove counties without fixed effects when FEs are activated
          if (use_fe) {
            if (fe_type == "modern") {
              # Filter out counties without modern state fixed effects
              df <- df %>%
                filter(!is.na(state_id) & state_id != "" & state_id != 0)
              
              # Also remove District of Columbia (not a state)
              df <- df %>%
                filter(!str_detect(tolower(as.character(state_id)), "district|columbia|\\bdc\\b"))
              
            } else {
              # Filter out counties without historical state fixed effects
              fe_var_name <- glue("hist_state_fe_{iv_year}{suffix}")
              if (fe_var_name %in% names(df)) {
                df <- df %>%
                  filter(!is.na(.data[[fe_var_name]]) & .data[[fe_var_name]] != "" & .data[[fe_var_name]] != 0)
                
                # Also remove District of Columbia (not a state)
                df <- df %>%
                  filter(!str_detect(tolower(as.character(.data[[fe_var_name]])), "district|columbia|\\bdc\\b"))
              }
            }
          }
          
          if (instrument_type == "weighted_density_overlap") {
            sample_col <- glue("iv_overlap_{overlap_threshold}_{sample_year}{suffix}")
            instr_col  <- glue("iv_overlap_{overlap_threshold}_{iv_year}{suffix}")
          } else if (instrument_type == "max_density_overlap") {
            sample_col <- glue("iv_overlap_max_{overlap_threshold}_{sample_year}{suffix}")
            instr_col  <- glue("iv_overlap_max_{overlap_threshold}_{iv_year}{suffix}")
          } else {
            stop("Error: Invalid instrument_type for County level specified.")
          }
          
          # MODIFIED: Use total employment (employment_total) for RHS across all sectors
          df <- switch(
            as.character(sectors),
            "1" = df %>% mutate(RHS = log(employment_total / area_acre), LHS = log(gcp_total / employment_total)),
            "2" = df %>% mutate(RHS = log(employment_total / area_acre), LHS = log(gcp_private_industry / (employment_private_nonfarm + emply_c_indryid_70))),
            "3" = df %>% rename(gcp_manufact = gcp_indstryid_12) %>%
              mutate(RHS = log(employment_total / area_acre), LHS = log(gcp_manufact / emply_c_indryid_500)),
            "4" = df %>% mutate(RHS = log(employment_total / area_acre), LHS = log((gcp_private_industry - gcp_agriculture) / employment_private_nonfarm)),
            "5" = df %>% mutate(RHS = log(employment_total / area_acre), LHS = log((gcp_private_industry - gcp_agriculture - gcp_mining) / (employment_private_nonfarm - employment_mining))),
            stop("Invalid County sector specified.")
          )
          
          if (mining_filter_active) {
            df <- df %>%
              mutate(temp_mining_share = gcp_mining / gcp_total) %>%
              filter(temp_mining_share <= mining_threshold | is.na(temp_mining_share)) %>%
              select(-temp_mining_share)
          }
        }
        
        incProgress(0.3, detail = "Finalizing data preparation...")
        
        new_controls <- resolve_geo_controls(input$geo_controls, input$water_controls, input$water_year)
        controls_vec <- unique(c(controls_vec, new_controls))
        req_cols <- c(sample_col, instr_col, controls_vec)
        missing_cols <- req_cols[!req_cols %in% names(df)]
        if (length(missing_cols) > 0) {
          stop(glue("Error: Required column(s) not found: {paste(missing_cols, collapse=', ')}. Check selections or data file."))
        }
        df <- scale_model_controls(df, controls_vec)
        
        df <- df %>% mutate(across(all_of(c(sample_col, instr_col)), as.numeric))
        df <- df %>%
          mutate(.sample_iv = .data[[sample_col]], instrument = .data[[instr_col]]) %>%
          filter(!is.na(.sample_iv) & !is.na(instrument)) %>%
          select(-.sample_iv)
        df$clusterID <- as.integer(as.factor(df$instrument))
        if (apply_college_adj) {
          if (!"college_share" %in% names(df)) stop("Error: 'college_share' column not found.")
          df <- df %>% mutate(LHS = LHS - college_coeff * college_share)
        }
        
        # Check for human capital adjustment columns before using them
        if (!"simple_gamma" %in% names(df) || !"Minc_return_calc" %in% names(df)) {
          stop("Error: 'simple_gamma' and/or 'Minc_return_calc' columns not found in the data. These are required for schooling adjustments.")
        }
        
        mean_schooling <- mean(df$avg_schooling, na.rm = TRUE)
        df <- df %>%
          mutate(
            LHS_adj1 = LHS - simple_gamma * (avg_schooling - mean_schooling),
            LHS_adj2 = LHS - Minc_return_calc * (avg_schooling - mean_schooling)
          )
        df <- df %>% filter(if_all(c(RHS, LHS, instrument), ~is.finite(.)))
        if(nrow(df) == 0){
          stop("After all filtering, 0 observations remain. Check selections for missing data in key variables.")
        }
        
        eligible_n <- nrow(df)
        geo_missing_n <- if (length(new_controls)) {
          sum(!Reduce(`&`, lapply(df[new_controls], is.finite)))
        } else 0L

        ch_missing_n <- 0L
        if (density_measure == "CH") {
          if (!"ch_complete" %in% names(df)) stop("The MSA data do not contain the Ciccone–Hall county inputs. Rebuild the MSA data first.")
          ch_missing_n <- sum(is.na(df$ch_complete) | df$ch_complete != 1)
        }

        incProgress(0.5, detail = "Building regression models...")
        
        dep_vars_map <- list(
          "0" = setNames("LHS", "No Schooling Adj."),
          "1" = setNames("LHS_adj1", "Same Return Adj."),
          "2" = setNames("LHS_adj2", "Specific Return Adj."),
          "3" = setNames(c("LHS", "LHS_adj1", "LHS_adj2"), c("No Adj.", "Same Return", "Specific Return"))
        )
        if (analysis_level == "MSA" && schooling_adj == 3) {
          dep_vars_map <- list("3" = setNames(c("LHS_adj1", "LHS_adj2"), c("Same Return Adj.", "Specific Return Adj.")))
        }
        dep_vars <- dep_vars_map[[as.character(schooling_adj)]]
        if (analysis_type == "First-stage Regression") {
          dep_vars <- setNames("RHS", "First stage")
        }
        controls_part <- if (length(controls_vec) > 0) paste("+", paste(controls_vec, collapse = " + ")) else ""
        fe_part <- "0"
        if (use_fe) {
          if (fe_type == "modern") {
            fe_part <- "state_id"
          } else {
            fe_var_name <- glue("hist_state_fe_{iv_year}{suffix}")
            if (!fe_var_name %in% names(df)) {
              stop(glue("Error: Historical FE variable '{fe_var_name}' not found."))
            }
            df[[fe_var_name]] <- as.factor(df[[fe_var_name]])
            fe_part <- fe_var_name
          }
        }
        
        # Determine vcov argument based on standard error specification
        if (se_spec == "spatial") {
          if (!is.finite(spatial_cutoff) || spatial_cutoff <= 0) stop("Choose a positive spatial distance cutoff.")
          vcov_arg <- vcov_conley(lat = "lat", lon = "lon", cutoff = spatial_cutoff, distance = "spherical")
        } else {
          vcov_arg <- switch(se_spec,
                             "robust" = "hetero",
                             "cluster_instrument" = ~clusterID,
                             "cluster_state" = ~state_id,
                             stop("Invalid 'se_spec' defined.")
          )
        }
        
        incProgress(0.7, detail = "Estimating models...")
        models <- list()
        first_stage_models <- list()
        model_warnings <- character()
        record_model_warning <- function(warning) {
          model_warnings <<- unique(c(model_warnings, conditionMessage(warning)))
          invokeRestart("muffleWarning")
        }
        
        for (i in seq_along(dep_vars)) {
          dep_var_name <- dep_vars[i]
          model_name <- names(dep_vars)[i]
          
          formula_str <- switch(analysis_type,
                                "OLS" = glue("{dep_var_name} ~ RHS {controls_part} | {fe_part}"),
                                "IV"  = glue("{dep_var_name} ~ 1 {controls_part} | {fe_part} | RHS ~ instrument"),
                                "First-stage Regression" = glue("RHS ~ instrument {controls_part} | {fe_part}"),
                                stop("Invalid 'analysis_type' specified.")
          )
          
          model <- withCallingHandlers(
            if (density_measure == "CH") {
              fit_ch_model(data = df, dep_var = dep_var_name, controls_vec = controls_vec,
                fe_part = fe_part, method = analysis_type, se_spec = se_spec)
            } else feols(as.formula(formula_str), data = df, vcov = vcov_arg),
            warning = record_model_warning
          )
          models[[model_name]] <- model
          
          if (analysis_type == "IV" && density_measure != "CH") {
            first_stage_models[[model_name]] <- withCallingHandlers(
              summary(model, stage = 1),
              warning = record_model_warning
            )
          }
        }
        
        model_n <- vapply(models, nobs, integer(1))
        cluster_column <- switch(se_spec, cluster_instrument = "clusterID", cluster_state = "state_id", NULL)
        model_clusters <- vapply(models, function(model) {
          if (is.null(cluster_column)) return(NA_integer_)
          values <- df[[cluster_column]][app_model_rows(model)]
          as.integer(length(unique(values[!is.na(values)])))
        }, integer(1))

        incProgress(0.9, detail = "Formatting results...")
        
        # Return both models and metadata for professional formatting
        list(
          models = models,
          first_stage_models = first_stage_models,
          analysis_type = analysis_type,
          density_measure = density_measure,
          ch_missing_n = ch_missing_n,
          analysis_level = analysis_level,
          year_modern = year_modern,
          sectors = sectors,
          sample_year = sample_year,
          iv_year = iv_year,
          instrument_type = if(analysis_level == "MSA") instrument_type else instrument_type,
          overlap_pct = if(analysis_level == "MSA") overlap_pct else overlap_threshold,
          schooling_adj = schooling_adj,
          apply_college_adj = apply_college_adj,
          college_coeff = if(apply_college_adj) college_coeff else NULL,
          mining_filter_active = mining_filter_active,
          mining_threshold = if(mining_filter_active) mining_threshold else NULL,
          county_msa_restriction = if(analysis_level == "County") county_msa_restriction else NULL,
          se_spec = se_spec,
          spatial_cutoff = if(se_spec == "spatial") spatial_cutoff else NULL,
          fe_type = if(use_fe) fe_type else "No",
          sample_scope = sample_scope,
          controls = if(length(controls_vec) > 0) paste(controls_vec, collapse = ", ") else "None",
          n_obs = unname(model_n[[1]]),
          eligible_n = eligible_n,
          geo_missing_n = geo_missing_n,
          model_n = model_n,
          model_clusters = model_clusters,
          model_warnings = model_warnings,
          geo_controls = input$geo_controls,
          water_controls = input$water_controls,
          water_year = as.integer(input$water_year),
          new_controls = new_controls,
          cluster_data = list(
            clusterID = if("clusterID" %in% names(df)) df$clusterID else NULL,
            state_id = if("state_id" %in% names(df)) df$state_id else NULL
          ),
          data_for_fs = df,
          controls_vec = controls_vec,
          fe_part = fe_part,
          vcov_arg = vcov_arg
        )
      })
    }, error = function(e) {
      list(error = paste("An error occurred during analysis:\n\n", e$message))
    })
    
    analysis_output(reg_results)
    
    # --- 2. Generate Map Path ---
    isolate({
      level <- input$analysis_level
      scope <- input$sample_scope
      scope_folder <- if (scope == "states_territories") "states_and_territories" else "states_only"
      
      map_path <- NULL
      map_title <- "Instrument Map: Historical density instrument map not available for the selected settings."
      
      if (level == "MSA") {
        year <- input$msa_iv_year
        pct <- input$msa_overlap_pct
        base_folder <- paste("www", "MSA Maps", scope_folder, sep="/")
        
        # Simplified logic - only overlap for MSA
        filename <- glue("overlap_{pct}pct_{year}.png")
        subfolder <- glue("{pct}pct")
        map_path <- paste(base_folder, "overlap", subfolder, filename, sep="/")
        map_title <- glue("Instrument Map: Maximum population density among {year} historical counties with {pct}% minimum overlap with modern MSA")
        
      } else { # County Logic
        type <- input$county_instrument_type
        year <- input$county_iv_year
        pct <- input$county_overlap_threshold
        base_folder <- paste("www", "County Maps", scope_folder, sep="/")
        
        if (type == "max_density_overlap") {
          filename <- glue("max_overlap_{pct}pct_{year}.png")
          subfolder <- glue("{pct}pct")
          map_path <- paste(base_folder, "max_overlap", subfolder, filename, sep="/")
          map_title <- glue("Instrument Map: Maximum population density among {year} historical counties with {pct}% minimum overlap with modern county")
        }
      }
      
      map_output(list(src = map_path, title = map_title))
    })
  })
  
  # --- 3. Professional Table Formatting Functions ---
  format_coefficient <- function(coef, se, stars = "") {
    coef_str <- format_estimate(coef)
    se_str <- paste0("(", format_estimate(se), ")")
    if (stars != "") coef_str <- paste0(coef_str, stars)
    return(list(coef = coef_str, se = se_str))
  }
  
  get_significance_stars <- function(p_value) {
    if (is.na(p_value)) return("")
    if (p_value < 0.01) return("***")
    if (p_value < 0.05) return("**")
    if (p_value < 0.1) return("*")
    return("")
  }
  
  create_professional_table <- function(models, analysis_type, se_spec = NULL, cluster_data = NULL,
                                        first_stage_models = NULL, data_for_fs = NULL,
                                        controls_vec = NULL, fe_part = NULL, vcov_arg = NULL,
                                        model_clusters = NULL) {
    if (!length(models)) return("")
    escape <- function(x) as.character(htmltools::htmlEscape(x))
    model_names <- names(models)
    all_vars <- unique(unlist(lapply(models, function(model) names(coef(model)))))
    is_ch <- inherits(models[[1]], "ch_model")
    main_var <- if (is_ch) "ch_elasticity" else switch(analysis_type, IV = "fit_RHS", OLS = "RHS", "instrument")
    ordered_vars <- unique(c(intersect(main_var, all_vars), setdiff(all_vars, main_var)))
    cells <- function(values, css = "stats") {
      paste0('<td class="', css, '">', values, '</td>', collapse = "")
    }
    row <- function(label, values, css = "stats", row_class = "") {
      paste0('<tr class="', row_class, '"><td class="row-label ', css, '">',
             escape(label), '</td>', cells(values, css), '</tr>')
    }
    parts <- c('<div class="regression-table-scroll"><table class="regression-table">',
               paste0('<thead><tr><th class="row-label">Variable</th>',
                      paste0('<th>', escape(model_names), '</th>', collapse = ""), '</tr></thead><tbody>'))
    for (variable in ordered_vars) {
      estimates <- vapply(models, function(model) {
        if (!variable %in% names(coef(model))) return("—")
        paste0(format_estimate(coef(model)[variable]), get_significance_stars(app_model_pvalue(model)[variable]))
      }, character(1))
      uncertainties <- vapply(models, function(model) {
        if (!variable %in% names(coef(model))) return("")
        paste0("(", format_estimate(app_model_se(model)[variable]), ")")
      }, character(1))
      parts <- c(parts, row(control_label(variable), estimates, "coefficient"), row("", uncertainties, "se"))
    }
    parts <- c(parts, row("Observations", vapply(models, function(model) format(nobs(model), big.mark = ","), character(1)), row_class = "top-border"))
    diagnostic_models <- if (analysis_type == "IV") first_stage_models else if (analysis_type == "First-stage Regression") models else NULL
    if (length(diagnostic_models)) {
      fstats <- vapply(model_names, function(name) {
        value <- instrument_wald_f(diagnostic_models[[name]])
        if (is.finite(value)) sprintf("%.2f", value) else "—"
      }, character(1))
      parts <- c(parts, row("Instrument Wald F", fstats))
    }
    if (!is.null(model_clusters) && any(!is.na(model_clusters))) {
      label <- if (se_spec == "cluster_instrument") "Instrument clusters" else "State clusters"
      parts <- c(parts, row(label, ifelse(is.na(model_clusters), "—", model_clusters)))
    }
    parts <- c(parts, '</tbody></table></div>')
    note <- 'Standard errors in parentheses. *** p&lt;0.01, ** p&lt;0.05, * p&lt;0.1.'
    if (length(diagnostic_models)) {
      note <- paste0(note, ' Instrument Wald F is the squared t statistic for the excluded instrument, using the selected standard errors and the model sample.')
    }
    if (is_ch) note <- paste0(note, ' Ciccone–Hall estimates theta in log[sum(n^theta a^(1−theta))/sum(n)], using county employment n and land area a. The reported elasticity is theta − 1. BEA combined county units are kept together.')
    if (is_ch) note <- paste0(note, ' ', escape(models[[1]]$inference))
    if (is_ch && any(vapply(models, function(m) m$singleton_n > 0L, logical(1)))) note <- paste0(note, ' As in Stata, CH retains states represented by one MSA; the linear model removes these observations.')
    if (se_spec == "spatial") note <- paste0(note, ' Conley standard errors use a uniform kernel.')
    parts <- c(parts, paste0('<div class="table-notes">', note, '</div>'))
    paste(parts, collapse = "\n")
  }

  # --- 4. Render Professional Outputs ---
  output$results_header <- renderText({
    req(analysis_output())
    if ("error" %in% names(analysis_output())) {
      "Analysis Error"
    } else {
      details <- analysis_output()
      paste(details$analysis_level, "results ·", model_description(details))
    }
  })
  
  output$results_table <- renderUI({
    req(analysis_output())
    
    if ("error" %in% names(analysis_output())) {
      div(class = "help-text", style = "color: #dc3545; border-left-color: #dc3545;",
          analysis_output()$error)
    } else {
      details <- analysis_output()
      HTML(create_professional_table(
        models = details$models, 
        analysis_type = details$analysis_type,
        se_spec = details$se_spec,
        cluster_data = details$cluster_data,
        first_stage_models = details$first_stage_models,
        data_for_fs = details$data_for_fs,
        controls_vec = details$controls_vec,
        fe_part = details$fe_part,
        vcov_arg = details$vcov_arg,
        model_clusters = details$model_clusters
      ))
    }
  })
  
  output$analysis_details <- renderUI({
    req(analysis_output())
    
    if ("error" %in% names(analysis_output())) {
      return(NULL)
    }
    
    details <- analysis_output()
    
    # Helper function to format sector names
    get_sector_name <- function(sector_num) {
      sector_names <- c("1" = "All", "2" = "Private", "3" = "Manufacturing", 
                        "4" = "Private non-farm", "5" = "Private non-farm/mining")
      return(sector_names[as.character(sector_num)])
    }
    
    # Helper function to format schooling adjustment
    get_schooling_adj_name <- function(adj_num) {
      adj_names <- c("0" = "None", "1" = "Same Return", "2" = "Specific Return", "3" = "Run Both")
      return(adj_names[as.character(adj_num)])
    }
    
    # Helper function to format instrument type
    get_instrument_name <- function(instr_type, level) {
      if (level == "MSA") {
        switch(instr_type,
               "overlap" = "Overlap",
               instr_type)
      } else {
        switch(instr_type,
               "max_density_overlap" = "Max Density Overlap",
               "weighted_density_overlap" = "Weighted Density Overlap", 
               instr_type)
      }
    }
    
    # Build comprehensive analysis details
    se_description <- details$se_spec
    if (details$se_spec == "cluster_instrument" && !is.null(details$cluster_data$clusterID)) {
      se_description <- "Cluster on historical instrument"
    } else if (details$se_spec == "cluster_state" && !is.null(details$cluster_data$state_id)) {
      se_description <- "Cluster on modern state"
    } else if (details$se_spec == "spatial") {
      spatial_cutoff_formatted <- sprintf("%.0f", as.numeric(details$spatial_cutoff))
      se_description <- paste0("Spatial (Conley). ", spatial_cutoff_formatted, " km cutoff. Uniform kernel")
    } else if (details$se_spec == "robust") {
      se_description <- "Robust (Heteroskedasticity-robust)"
    }
    
    # Create detailed information sections
    detail_sections <- list()
    
    # Core Analysis Settings
    detail_sections$core <- paste0(
      '<strong>Analysis Level:</strong> ', details$analysis_level, '<br>',
      '<strong>Modern Year:</strong> ', details$year_modern, '<br>',
      '<strong>Analysis Method:</strong> ', model_description(details), '<br>',
      '<strong>Sector:</strong> ', get_sector_name(details$sectors), '<br>'
    )
    
    # Sample and Instrument Settings  
    detail_sections$sample <- paste0(
      '<strong> Year of the Historical Territory:</strong> ', details$sample_year, '<br>',
      '<strong>Year of the Historical Population Density (Historical Census Year):</strong> ', details$iv_year, '<br>',
      '<strong>Match of Historical to Modern Geographic Units:</strong> ', get_instrument_name(details$instrument_type, details$analysis_level),
      if (!is.null(details$overlap_pct)) paste0(' (', details$overlap_pct, '% overlap)') else '', '<br>',
      '<strong>Historical territory:</strong> ', details$sample_scope, '<br>'
    )
    
    # Fixed Effects and Controls
    detail_sections$controls <- paste0(
      '<strong>State Fixed Effects:</strong> ', details$fe_type, '<br>',
      '<strong>Control variables:</strong> ', if (length(details$controls_vec)) paste(vapply(details$controls_vec, control_label, character(1)), collapse = ', ') else 'None', '<br>'
    )
    
    # Adjustments
    adjustment_text <- paste0('<strong> Modern Adjustment for Human capital:</strong> ', get_schooling_adj_name(details$schooling_adj))
    if (details$apply_college_adj && !is.null(details$college_coeff)) {
      college_coeff_formatted <- sprintf("%.2f", as.numeric(details$college_coeff))
      adjustment_text <- paste0(adjustment_text, '<br><strong>College Share Adjustment:</strong> Yes (coefficient = ', college_coeff_formatted, ' - Based on Moretti (2004))')
    } else {
      adjustment_text <- paste0(adjustment_text, '<br><strong>College Share Adjustment:</strong> No')
    }
    if (details$mining_filter_active && !is.null(details$mining_threshold)) {
      mining_threshold_formatted <- sprintf("%.3f", as.numeric(details$mining_threshold))
      adjustment_text <- paste0(adjustment_text, '<br><strong>Maximum mining share:</strong> Yes (max share = ', mining_threshold_formatted, ')')
    } else {
      adjustment_text <- paste0(adjustment_text, '<br><strong>Maximum mining share:</strong> No')
    }
    detail_sections$adjustments <- paste0(adjustment_text, '<br>')
    
    # Level-specific settings
    if (details$analysis_level == "County" && !is.null(details$county_msa_restriction)) {
      detail_sections$level_specific <- paste0('<strong>Restrict to MSA Counties:</strong> ', 
                                               if(details$county_msa_restriction) "Yes" else "No", '<br>')
    }
    
    # Standard Errors and Sample Size
    detail_sections$technical <- paste0(
      '<strong>Standard Errors:</strong> ', se_description, '<br>',
      '<strong>Observations used:</strong> ', if (length(unique(details$model_n)) == 1L) format(details$model_n[[1]], big.mark = ',') else paste(names(details$model_n), format(details$model_n, big.mark = ','), sep = ': ', collapse = '; '), '<br>',
      '<strong>Eligible before control and model exclusions:</strong> ', format(details$eligible_n, big.mark = ','), '<br>',
      '<strong>Missing selected geographic controls:</strong> ', format(details$geo_missing_n, big.mark = ','),
      if (identical(details$density_measure, 'CH')) paste0('<br><strong>Incomplete county inputs:</strong> ', details$ch_missing_n) else ''
    )
    
    # Combine all sections
    all_details <- paste(unlist(detail_sections), collapse = "")
    
    HTML(paste0(
      '<div class="analysis-details">',
      '<h5> Regression Analysis Configuration</h5>',
      all_details,
      '</div>'
    ))
  })
  
  output$map_header <- renderText({
    req(map_output())
    map_output()$title
  })
  
  output$map_ui <- renderUI({
    info <- map_output()
    req(info)
    
    if (!is.null(info$src) && file.exists(info$src)) {
      imageOutput("instrument_map_render", width = paste0(input$map_size, "%"), height = "auto")
    } else {
      div(class = "help-text",
          "Map image not found. Please check your selections and verify your file structure in the 'www' folder.")
    }
  })
  
  output$instrument_map_render <- renderImage({
    info <- map_output()
    req(info$src, file.exists(info$src))
    
    list(
      src = info$src,
      contentType = "image/png",
      alt = "Instrument Map"
    )
  }, deleteFile = FALSE)

  valid_result <- reactive({
    details <- analysis_output()
    req(details, is.null(details$error), length(details$models))
    details
  })

  observeEvent(input$toggle_configuration, {
    updateActionButton(session, "toggle_configuration", label =
      if (input$toggle_configuration %% 2L == 1L) "Hide regression configuration" else "Show regression configuration")
  }, ignoreInit = TRUE)

  output$results_summary <- renderUI({
    details <- analysis_output()
    if (is.null(details)) return(div(class = "empty-state",
      div(class = "empty-state-mark", icon("chart-line")),
      h3("Explore the geography of productivity"),
      p("Choose a sample and specification, then select Run analysis."),
      p(class = "empty-state-meta", paste(format(length(unique(msa_data$msafips)), big.mark = ","),
        "MSAs ·", format(length(unique(county_data$geofips)), big.mark = ","), "counties · 2001–2022"))))
    if (!is.null(details$error)) return(div(class = "note-card", role = "alert", h4("This specification could not be estimated"), p(details$error)))
    estimates <- model_coefficients(details)
    primary <- estimates[estimates$term == main_coefficient(details), , drop = FALSE]
    n <- vapply(details$models, stats::nobs, numeric(1))
    sample_text <- if (length(unique(n)) == 1) format(n[1], big.mark = ",") else paste(format(range(n), big.mark = ","), collapse = "–")
    metric <- function(label, value, note) div(class = "metric-card", div(class = "metric-label", label), div(class = "metric-value", value), div(class = "metric-detail", note))
    tagList(div(class = "metric-grid",
      metric(if (identical(details$density_measure, "CH")) "Ciccone–Hall elasticity" else "Density coefficient", if (nrow(primary)) sprintf("%.3f", primary$estimate[1]) else "Unavailable",
             if (nrow(primary)) primary$model[1] else "Not identified in this sample"),
      metric("95% interval", if (nrow(primary)) sprintf("%.3f to %.3f", primary$conf_low[1], primary$conf_high[1]) else "—", "Using the chosen standard errors"),
      metric("Observations", sample_text, paste(length(details$models), if (length(details$models) == 1) "model" else "models")),
      metric("Modern year", as.character(details$year_modern), paste(details$analysis_level, "·", model_description(details)))),
      div(class = "estimate-caption", "The chart compares the selected schooling adjustments. The table contains every estimated coefficient."))
  })

  output$effect_plot <- renderPlot({ result_plot(valid_result()) }, res = 120)

  output$sample_note <- renderUI({
    details <- analysis_output()
    if (is.null(details) || !is.null(details$error)) return(NULL)
    counts <- vapply(details$models, stats::nobs, numeric(1))
    eligible <- if (is.null(details$eligible_n)) details$n_obs else details$eligible_n
    missing_geo <- if (is.null(details$geo_missing_n)) 0L else details$geo_missing_n
    selected <- details$new_controls
    omitted <- unique(unlist(lapply(details$models, function(m) m$collin.var)))
    notices <- list()
    if (isTRUE(details$ch_missing_n > 0L)) notices <- c(notices, list(p(paste(details$ch_missing_n,
      "eligible observations have incomplete county employment or land area for the Ciccone–Hall model."))))
    if (missing_geo > 0) notices <- c(notices, list(p(paste(format(missing_geo, big.mark = ","),
      "eligible observations have missing values in the selected geographic controls. Missing values are excluded, never set to zero."))))
    if (length(unique(counts)) > 1) notices <- c(notices, list(p("The models use different sample sizes because their required values differ.")))
    if (length(omitted)) notices <- c(notices, list(p(paste("Omitted because of collinearity:", paste(vapply(omitted, control_label, character(1)), collapse = "; "), "."))))
    if (length(details$model_warnings)) notices <- c(notices, lapply(details$model_warnings, function(warning) p(paste("Estimation note:", warning))))
    if ("portage_access10" %in% selected) notices <- c(notices, list(p("Portage access uses approximate fall-line/river candidates, not a verified historical portage inventory.")))
    if (!length(notices)) return(NULL)
    div(class = "sample-note", notices)
  })

  output$data_notes <- renderUI({
    div(class = "data-notes",
      h4("Geographic measures"),
      tags$dl(
        tags$dt("Terrain"), tags$dd("Area-weighted Nunn–Puga ruggedness and PRISM elevation, in metres."),
        tags$dt("Crop suitability"), tags$dd("GAEZ wheat and maize indices, 0–10,000: rainfed production with low inputs, 1981–2010. Regression coefficients refer to a 1,000-point increase in each index."),
        tags$dt("Climate"), tags$dd("PRISM 1991–2020 normals: January and July temperature in °C, annual precipitation in mm."),
        tags$dt("Water"), tags$dd("Access means the geographic footprint lies within 10 km. River and canal dates follow Atack; shoreline uses modern Census geometry. Distances run from the geometric centroid and enter as log(1 + km)."),
        tags$dt("Coverage"), tags$dd("These new controls cover the contiguous US. Missing coverage and uncertain historical dates remain missing. Approximate portage candidates are optional; historical harbor depth is not included.")),
      p("Measures are calculated over county or MSA boundaries. The original water and railroad variables remain available under Historical infrastructure."))
  })

  output$download_coefficients <- downloadHandler(
    filename = function() {d <- valid_result(); paste0("agglomeration-", tolower(d$analysis_level), "-", d$year_modern, "-coefficients.csv")},
    content = function(file) {
      details <- valid_result()
      table <- model_coefficients(details)
      table$controls <- paste(details$controls_vec, collapse = "; ")
      table$water_year <- details$water_year
      table$standard_errors <- details$se_spec
      table$fixed_effects <- details$fe_type
      table$sample_year <- details$sample_year
      table$instrument_year <- details$iv_year
      utils::write.csv(table, file, row.names = FALSE, na = "")
    })

  output$download_plot <- downloadHandler(
    filename = function() {d <- valid_result(); paste0("agglomeration-", tolower(d$analysis_level), "-", d$year_modern, ".png")},
    content = function(file) {ggplot2::ggsave(file, result_plot(valid_result()), device = "png", width = 10, height = 5.5, dpi = 300, bg = "white")})

  output$download_results <- downloadHandler(
    filename = function() {d <- valid_result(); paste0("agglomeration-", tolower(d$analysis_level), "-", d$year_modern, "-results.html")},
    content = function(file) {
      details <- valid_result()
      chart_file <- tempfile(fileext = ".png")
      on.exit(unlink(chart_file))
      ggplot2::ggsave(chart_file, result_plot(details), device = "png", width = 9, height = 4.7, dpi = 180, bg = "white")
      chart_data <- base64enc::dataURI(file = chart_file, mime = "image/png")
      table <- create_professional_table(details$models, details$analysis_type, details$se_spec,
        details$cluster_data, details$first_stage_models, details$data_for_fs, details$controls_vec, details$fe_part, details$vcov_arg,
        model_clusters = details$model_clusters)
      esc <- htmltools::htmlEscape
      specifications <- c(Geography = details$analysis_level, Method = model_description(details), `Modern year` = details$year_modern,
        Sector = c("All", "Private", "Manufacturing", "Private non-farm", "Private non-farm/mining")[details$sectors],
        `Historical sample year` = details$sample_year, `Instrument year` = details$iv_year,
        `Historical territory` = details$sample_scope, `Instrument construction` = details$instrument_type,
        `Overlap threshold (%)` = details$overlap_pct, `State fixed effects` = details$fe_type,
        `Standard errors` = details$se_spec, `Schooling adjustment` = details$schooling_adj,
        `College adjustment` = if (details$apply_college_adj) details$college_coeff else "None",
        `Mining threshold` = if (details$mining_filter_active) details$mining_threshold else "None",
        `Spatial cutoff (km)` = if (details$se_spec == "spatial") details$spatial_cutoff else "Not used",
        `MSA counties only` = if (is.null(details$county_msa_restriction)) "Not applicable" else as.character(details$county_msa_restriction),
        `Water-access year` = details$water_year,
        Controls = if (length(details$controls_vec)) paste(vapply(details$controls_vec, control_label, character(1)), collapse = "; ") else "None")
      spec_html <- paste0("<dt>", esc(names(specifications)), "</dt><dd>", esc(as.character(specifications)), "</dd>", collapse = "")
      html <- paste0('<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>Agglomeration results</title>',
        '<style>body{font-family:Arial,sans-serif;color:#000000;max-width:1100px;margin:40px auto;padding:0 24px;line-height:1.5}h1{font-size:32px}img{max-width:100%}table{border-collapse:collapse;width:100%;font-size:14px}th,td{padding:7px 10px;text-align:right;border-bottom:1px solid #dddddd}.row-label{text-align:left}th{border-top:2px solid #000000}dt{font-weight:bold;margin-top:10px}dd{margin-left:0}.table-notes{font-size:12px;margin-top:15px}@media print{body{margin:0}}</style><body>',
        '<h1>Agglomeration effects in the United States</h1><p>', esc(paste(details$analysis_level, details$year_modern, model_description(details), sep = ' · ')),
        '</p><img alt="Density coefficient estimates and 95% confidence intervals" src="', chart_data, '">', table,
        '<h2>Specification</h2><dl>', spec_html, '</dl><p>New geographic controls cover the contiguous US. Missing values are excluded. Shoreline is a modern proxy; portage access is approximate.</p></body></html>')
      writeLines(html, file, useBytes = TRUE)
    })

}

shinyApp(ui = ui, server = server)
