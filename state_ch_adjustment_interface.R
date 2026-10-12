help_state_adjusted_ch <- paste(
  "County-adjusted CH links the state model to a county estimation in the selected modern year and eligible states.",
  "Geography: county IV or OLS estimates the selected geographic effects with modern state fixed effects and an optional log-area control. Only the geographic effects enter the state CH index; county fixed effects and the area coefficient do not enter it.",
  "Human capital: schooling changes effective employment using the common micro-estimated Mincer return. Effective employment enters the CH power theta. The chosen college-share coefficient is a direct productivity multiplier. The denominator remains actual employment. Both adjustments enter each county contribution before aggregation.",
  "The county calibration uses the selected county population construction, historical years and territory scope, with population in levels. The state instrument retains the separate state construction and scale settings.",
  "With a geography adjustment, standard errors and the zero-elasticity S test account for the estimated geographic coefficients and overlapping county and state observations. The S test then requires an identified county calibration. Human-capital-only adjustment needs no county regression. Inference conditions on the supplied micro schooling return and chosen college coefficient.",
  "All constituent counties are retained. A state is excluded if an active adjustment is missing for any county with positive employment. Combined BEA county/city units use explicitly aggregated geography and education inputs.",
  sep = "\n")

state_adjustment_settings_ui <- function() {
  tagList(
    labeledInput("state_ch_mode", "CH specification:",
      selectInput("state_ch_mode", NULL,
        c("Standard CH" = "standard", "County-adjusted CH" = "county_adjusted"), "standard"),
      "help_state_ch_mode", help_state_adjusted_ch),
    conditionalPanel("input.state_ch_mode == 'county_adjusted'",
      selectInput("state_adjustment_type", "Adjust the county contributions for:",
        c("Geography" = "geography", "Human capital" = "human_capital", "Geography and human capital" = "both"), "geography"),
      conditionalPanel("input.state_adjustment_type != 'human_capital'",
      conditionalPanel("input.analysis_type == 'IV'",
      labeledInput("state_county_instrument", "County calibration instrument:",
        selectInput("state_county_instrument", NULL,
          c("Area-weighted population" = "area_population", "Glaeser and Gottlieb (2009)" = "county_population"), "area_population"),
        "help_state_county_instrument",
        "The linked county estimation uses historical population in levels, with the state model's historical years and territory scope. This choice is separate from the state instrument.")),
      checkboxInput("state_county_area", "Include log county area in the calibration", TRUE)),
      conditionalPanel("input.state_adjustment_type != 'geography'",
        checkboxInput("state_schooling_efficiency", "Schooling as labor efficiency", TRUE),
        checkboxInput("state_college_externality", "College-share productivity adjustment", TRUE),
        conditionalPanel("input.state_college_externality == true",
          numericInput("state_college_coefficient", "College-share coefficient:", .75, min = -10, max = 10, step = .05)),
        p(class = "help-block", "Schooling uses the existing common micro return and one shared reference. The state model estimates the CH elasticity, not the schooling or college coefficients.")),
      conditionalPanel("input.state_adjustment_type != 'human_capital'",
        h4("County Geography Adjustment"),
        labeledInput("state_geo_controls", "Terrain, crop suitability and climate:",
          checkboxGroupInput("state_geo_controls", NULL,
            c("Ruggedness and Elevation" = "terrain", "Wheat and Maize Suitability" = "crop", "Temperature and Precipitation" = "climate"),
            selected = c("terrain", "crop", "climate")), "help_state_geo_controls", help_geo_controls),
        labeledInput("state_water_controls", "Separate water measures:",
          checkboxGroupInput("state_water_controls", NULL,
            c("Ocean and Great Lakes Access" = "shoreline", "Historical River and Canal Access" = "historical_access",
              "Distance to Shores and Waterways" = "distance", "Approximate Portage Access" = "portage"),
            selected = c("shoreline", "historical_access", "distance", "portage")),
          "help_state_water_controls", help_water_controls),
        labeledInput("state_transport_controls", "Water access and railroads:",
          checkboxGroupInput("state_transport_controls", NULL,
            c("Water Access Sum, 1820" = "water_1820", "Railroads, 1840" = "railroads_1840",
              "Railroads, 1850" = "railroads_1850", "Railroads, 1861" = "railroads_1861"),
            selected = c("water_1820", "railroads_1840")),
          "help_state_transport_controls", help_controls),
        labeledInput("state_water_year", "Waterway reference year:",
          selectInput("state_water_year", NULL, seq(1790, 1860, 10), 1820),
          "help_state_water_year", geographic_notes[["Waterway reference year"]]))))
}

get_state_adjustment_inputs <- local({
  cached <- NULL
  function(path = "State_CH_adjustment_counties_data.dta") {
    if (!is.null(cached)) return(cached)
    if (!file.exists(path)) stop("County-adjusted CH inputs are unavailable. Run the State CH adjustment data build.")
    value <- haven::read_dta(path)
    required <- c("statefips", "geofips", "ch_slot", "avg_schooling_10_14", "college_share", "simple_gamma")
    if (!all(required %in% names(value)) || anyDuplicated(value$geofips) ||
        anyDuplicated(value[c("statefips", "ch_slot")]))
      stop("County-adjusted CH inputs have invalid fields or county slots.")
    cached <<- value
    cached
  }
})

state_ch_adjusted_result <- function(input, data, population_missing_n, transformed) {
  year <- as.integer(input$year_modern)
  mode <- input$state_adjustment_type
  if (is.null(mode)) mode <- "geography"
  if (!mode %in% c("geography", "human_capital", "both")) stop("Choose a county adjustment.")
  geography <- mode %in% c("geography", "both")
  human <- mode %in% c("human_capital", "both")
  school <- human && isTRUE(input$state_schooling_efficiency)
  college <- human && isTRUE(input$state_college_externality)
  if (human && !school && !college && !geography) stop("Select a schooling or college-share adjustment.")
  water_year <- if (is.null(input$state_water_year)) 1820L else as.integer(input$state_water_year)
  geo_vars <- if (geography) unique(c(resolve_geo_controls(input$state_geo_controls,
    input$state_water_controls, water_year), input$state_transport_controls)) else character()
  allowed_transport <- c("water_1820", "railroads_1840", "railroads_1850", "railroads_1861")
  if (any(!input$state_transport_controls %in% allowed_transport)) stop("Unknown county transport control.")
  if (geography && !length(geo_vars)) stop("Select at least one geographic control for the county adjustment.")
  construction <- input$state_county_instrument
  if (is.null(construction)) construction <- "area_population"
  if (!construction %in% c("area_population", "county_population")) stop("Choose a county population construction.")
  prefix <- if (construction == "area_population") "AW" else "GG"
  suffix <- if (input$sample_scope == "states_only") "_s" else ""
  years <- unique(as.integer(c(input$state_sample_year, input$state_iv_year)))
  county <- data.frame(state_id = numeric(), county_id = numeric(), schooling = numeric())
  if (geography) {
    county <- as.data.frame(get_app_year("County", year))
    county$state_id <- floor(as.numeric(county$geofips) / 1000)
    county$county_id <- as.numeric(county$geofips)
    county <- county[county$state_id %in% data$state_id, , drop = FALSE]
    for (historical_year in years) {
    name <- paste0(prefix, "pop_", historical_year, suffix)
    if (!name %in% names(county)) stop(paste("County population input is missing:", name))
    valid <- is.finite(county[[name]]) & county[[name]] >= 0
    for (flag in c("miss", "gap")) {
      field <- paste0(prefix, flag, "_", historical_year, suffix)
      if (field %in% names(county)) valid <- valid & !is.na(county[[field]]) & county[[field]] == 0
    }
    valid_field <- paste0(prefix, "valid_", historical_year, suffix)
    if (valid_field %in% names(county)) valid <- valid & !is.na(county[[valid_field]]) & county[[valid_field]] == 1
      county <- county[valid, , drop = FALSE]
    }
    county$instrument <- as.numeric(county[[paste0(prefix, "pop_", input$state_iv_year, suffix)]]) / 1000
  }
  inputs <- as.data.frame(get_state_adjustment_inputs())
  needed <- unique(c(geo_vars, if (school) c("avg_schooling_10_14", "simple_gamma"), if (college) "college_share"))
  if (!all(needed %in% names(inputs)) || geography && !all(needed %in% names(county)))
    stop("Selected county adjustment fields are missing from the data build.")
  components <- inputs[inputs$statefips %in% data$state_id, , drop = FALSE]
  components$state_id <- as.numeric(components$statefips)
  components$county_id <- as.numeric(components$geofips)
  state_row <- match(components$state_id, data$state_id)
  components$employment <- vapply(seq_len(nrow(components)), function(i)
    as.numeric(data[[paste0("nc", components$ch_slot[i])]][state_row[i]]), numeric(1))
  components$area <- vapply(seq_len(nrow(components)), function(i)
    as.numeric(data[[paste0("ac", components$ch_slot[i])]][state_row[i]]), numeric(1))
  expected <- table(factor(components$state_id, levels = data$state_id))
  if (any(as.numeric(expected) != data$ch_units_expected)) stop("State county memberships differ from the adjustment input.")
  if (geography) {
    canonical <- match(county$county_id, components$county_id)
    county$area <- as.numeric(county$area_acre)
    match_area <- !is.na(canonical)
    county$area[match_area] <- components$area[canonical[match_area]]
    county$x <- log(as.numeric(county$employment_total) / county$area)
    county$ln_area <- log(county$area)
    county$y <- log(as.numeric(county$gcp_total) / as.numeric(county$employment_total))
    county$schooling <- as.numeric(county$avg_schooling_10_14)
    county$college <- as.numeric(county$college_share)
    county$schooling[match_area] <- as.numeric(components$avg_schooling_10_14[canonical[match_area]])
    county$college[match_area] <- as.numeric(components$college_share[canonical[match_area]])
    county <- scale_model_controls(county, geo_vars)
  }
  components$schooling <- as.numeric(components$avg_schooling_10_14)
  components$college <- as.numeric(components$college_share)
  components <- scale_model_controls(components, geo_vars)
  beta <- 0
  if (school) {
    beta_values <- unique(as.numeric(inputs$simple_gamma[is.finite(inputs$simple_gamma)]))
    if (length(beta_values) != 1L) stop("The common micro schooling return is missing or inconsistent.")
    beta <- beta_values
  }
  kappa <- if (college) as.numeric(input$state_college_coefficient) else 0
  if (length(kappa) != 1L || !is.finite(kappa) || abs(kappa) > 10)
    stop("Enter a finite college-share coefficient between -10 and 10.")
  county_area <- if (is.null(input$state_county_area)) TRUE else isTRUE(input$state_county_area)
  warnings <- character()
  model <- withCallingHandlers(fit_state_adjusted_ch(data, county, components, list(
    method = input$analysis_type, mode = mode, geo_vars = geo_vars,
    beta_school = beta, kappa_college = kappa, school_reference = NULL,
    county_controls = if (county_area) "ln_area" else character(),
    county_instrument_label = instrument_name(construction), county_fe = TRUE)),
    warning = function(w) {
      warnings <<- unique(c(warnings, conditionMessage(w)))
      invokeRestart("muffleWarning")
    })
  model_name <- switch(mode, geography = "CH: geography", human_capital = "CH: human capital", both = "CH: geography and human capital")
  excluded <- model$excluded_states
  if (nrow(excluded)) {
    labels <- data$state_name[match(excluded$state_id, as.character(data$state_id))]
    warnings <- c(warnings, paste("Incomplete selected county adjustments exclude whole states:", paste(labels, collapse = ", ")))
  }
  model$adjustment_mode <- mode
  list(models = setNames(list(model), model_name), first_stage_models = list(), analysis_type = input$analysis_type,
    approach = "density", density_measure = "CH", analysis_level = "State", year_modern = year,
    state_ch_mode = "county_adjusted", adjustment_mode = mode, county_calibration_instrument = construction,
    county_calibration = model$county_calibration, sectors = 1L,
    sample_year = as.integer(input$state_sample_year), iv_year = as.integer(input$state_iv_year),
    instrument_type = input$state_instrument_type, instrument_form = transformed$form,
    log_nonpositive_n = transformed$nonpositive, overlap_pct = NULL,
    schooling_adj = if (school) 1L else 0L, apply_college_adj = college, college_coeff = if (college) kappa else NULL,
    schooling_reference = setNames(model$adjustment$school_reference, model_name),
    mining_filter_active = FALSE, mining_threshold = NULL, county_msa_restriction = NULL,
    se_spec = "cluster_state", spatial_cutoff = NULL, fe_type = "No", sample_scope = input$sample_scope,
    controls = if (length(geo_vars)) paste(geo_vars, collapse = ", ") else "None",
    controls_vec = character(), county_geo_vars = geo_vars, n_obs = nobs(model), eligible_n = nrow(data),
    geo_missing_n = nrow(excluded), ch_missing_n = sum(is.na(data$ch_complete) | data$ch_complete != 1),
    population_missing_n = population_missing_n,
    model_n = setNames(nobs(model), model_name), model_clusters = setNames(model$clusters, model_name),
    model_warnings = warnings, geo_controls = if (geography) input$state_geo_controls else character(),
    water_controls = if (geography) input$state_water_controls else character(),
    water_year = if (geography) water_year else NA_integer_, new_controls = geo_vars,
    cluster_data = list(clusterID = data$clusterID, state_id = data$state_id),
    data_for_fs = data, fe_part = "0", vcov_arg = "hetero")
}
