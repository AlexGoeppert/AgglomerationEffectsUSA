help_state_adjusted_ch <- paste(
  "County-adjusted CH puts county geography, schooling or college effects inside the MSA or state CH index, according to the selected adjustment.",
  "The county equation relates log GDP per employment to log employment density, the selected geography variables, modern state fixed effects and optional log county area. IV uses historical population in levels for county density; OLS uses the same controls without an instrument.",
  "From the county regression, only the fitted geography and transport effects are transferred to the index. The county density coefficient, fixed effects and area coefficient are not inserted into the index. The aggregate model estimates its own CH elasticity.",
  "The Geography option uses geography and transport only. Existing MSA human-capital options then adjust the MSA outcome separately.",
  "The Geography + schooling and college option puts schooling and college share inside each county contribution. The fixed schooling return is the existing micro estimate (about 0.102623); the college-share coefficient is 0.75. Geography is reestimated after subtracting h + 0.75 times college share from county log GDP per employment, where h is the schooling return times years of schooling. The county density regressor remains log employment per unit of land area, as in the ordinary County IV model. The index contribution becomes n times exp(X gamma + h + 0.75 times college share) times density to the power theta minus one. The denominator remains actual employment. The aggregate outcome receives no further schooling or college adjustment.",
  "Schooling and college places the fixed county schooling and college effects inside CH without estimating geography coefficients or fitting a county calibration. Missing geography inputs do not affect this specification. The aggregate outcome is unadjusted.",
  "For Geography and the combined adjustment, calibration uses eligible counties in all constituent states, including nonmetropolitan counties. Every constituent county remains in the index, including counties outside the historical calibration sample. Missing a required geography or human-capital input in a positive-employment county excludes the whole MSA or state.",
  "When geography is estimated, joint standard errors and the zero-elasticity S test account for estimated geography and observations shared by the two stages. The county calibration must be identified. MSA clustering puts an MSA and its constituent counties together; counties outside represented MSAs retain their own MSA or county cluster. State clustering joins states connected by interstate MSAs to keep shared observations together.",
  sep = "\n")

state_adjustment_settings_ui <- function(level = "State", section = c("model", "controls")) {
  section <- match.arg(section, c("model", "controls"), several.ok = TRUE)
  prefix <- tolower(level)
  id <- function(x) paste0(prefix, "_", x)
  tagList(
    if ("model" %in% section) div(class = "ch-index-options",
      labeledInput(id("ch_mode"), "CH specification:",
        radioButtons(id("ch_mode"), NULL,
          c("Standard CH" = "standard", "County-adjusted CH" = "county_adjusted"), "standard"),
        id("help_ch_mode"), help_state_adjusted_ch),
      conditionalPanel(paste0("input.", id("ch_mode"), " == 'county_adjusted'"),
        labeledInput(id("adjustment_mode"), "County adjustments inside CH:",
          radioButtons(id("adjustment_mode"), NULL,
            c("Geography" = "geography", "Schooling and college" = "human_capital", "Geography + schooling and college" = "both"), "geography"),
          id("help_adjustment_mode"), help_state_adjusted_ch))),
    if ("controls" %in% section) conditionalPanel(paste0("input.", id("ch_mode"), " == 'county_adjusted' && input.", id("adjustment_mode"), " != 'human_capital'"),
      h4("County effects inside CH"),
      div(class = "calibration-status",
        tags$div(tags$strong("Sector: "), "All industries"),
        tags$div(tags$strong("Fixed effects: "), "Modern state fixed effects"),
        conditionalPanel("input.analysis_type == 'IV'",
          tags$div(tags$strong("Instrument scale: "), "Historical population in levels"))),
      conditionalPanel("input.analysis_type == 'IV'",
        labeledInput(id("county_instrument"), "County calibration instrument:",
          selectInput(id("county_instrument"), NULL,
            c("Area-weighted population" = "area_population", "Glaeser and Gottlieb (2009)" = "county_population"), "area_population"),
          id("help_county_instrument"),
          "The county IV uses population in levels, with the same historical years and territory scope as the aggregate model. The population matching method can be selected separately.")),
      div(class = "input-button-row",
        checkboxInput(id("county_area"), "Include log county area in the calibration", FALSE),
        pdf_help_button(id("help_county_area"), "Log county area in the calibration")),
      labeledInput(id("geo_controls"), "Terrain, crop suitability and climate:",
        checkboxGroupInput(id("geo_controls"), NULL,
          c("Ruggedness and Elevation" = "terrain", "Wheat and Maize Suitability" = "crop", "Temperature and Precipitation" = "climate"),
          selected = c("terrain", "crop", "climate")), id("help_geo_controls"), help_geo_controls),
      labeledInput(id("water_controls"), "Separate water measures:",
        checkboxGroupInput(id("water_controls"), NULL,
          c("Ocean and Great Lakes Access" = "shoreline", "Historical River and Canal Access" = "historical_access",
            "Distance to Shores and Waterways" = "distance", "Approximate Portage Access" = "portage"),
          selected = c("shoreline", "historical_access", "distance", "portage")), id("help_water_controls"), help_water_controls),
      labeledInput(id("transport_controls"), "Water access and railroads:",
        checkboxGroupInput(id("transport_controls"), NULL,
          c("Water Access Sum, 1820" = "water_1820", "Railroads, 1840" = "railroads_1840",
            "Railroads, 1850" = "railroads_1850", "Railroads, 1861" = "railroads_1861"),
          selected = c("water_1820", "railroads_1840")), id("help_transport_controls"), help_controls),
      labeledInput(id("water_year"), "Waterway reference year:",
        selectInput(id("water_year"), NULL, seq(1790, 1860, 10), 1820), id("help_water_year"), geographic_notes[["Waterway reference year"]])))
}

get_state_adjustment_inputs <- local({
  cached <- NULL
  function(path = "State_CH_adjustment_counties_data.dta") {
    if (!is.null(cached)) return(cached)
    if (!file.exists(path)) stop("County geography inputs are unavailable. Run the county adjustment data build.")
    value <- haven::read_dta(path)
    required <- c("statefips", "geofips", "ch_slot", "ac")
    if (!all(required %in% names(value)) || anyDuplicated(value$geofips) || anyDuplicated(value[c("statefips", "ch_slot")]))
      stop("County geography inputs have invalid fields or county slots.")
    cached <<- value
    cached
  }
})

get_msa_ch_membership <- local({
  cached <- NULL
  function(path = "MSA_CH_county_membership.dta") {
    if (!is.null(cached)) return(cached)
    if (!file.exists(path)) stop("MSA county membership is unavailable. Run the MSA data build.")
    value <- as.data.frame(haven::read_dta(path))
    if (!all(c("msafips", "geofips", "statefips", "ch_slot", "ac") %in% names(value)) ||
        anyDuplicated(value$geofips) || anyDuplicated(value[c("msafips", "ch_slot")]))
      stop("MSA county membership has invalid fields or repeated county slots.")
    cached <<- value
    cached
  }
})

get_msa_ch_cluster_membership <- local({
  cached <- NULL
  function(path = "MSA_CH_county_cluster_membership.dta") {
    if (!is.null(cached)) return(cached)
    if (!file.exists(path)) stop("MSA county clustering membership is unavailable. Run the MSA data build.")
    value <- as.data.frame(haven::read_dta(path))
    if (!all(c("msafips", "geofips") %in% names(value)) || anyDuplicated(value$geofips))
      stop("MSA county clustering membership contains invalid or repeated county identities.")
    cached <<- value
    cached
  }
})

sca_state_components <- function(membership) {
  states <- sort(unique(as.character(membership$statefips)))
  parent <- setNames(states, states)
  root <- function(s) { while (parent[[s]] != s) s <- parent[[s]]; s }
  for (parts in split(as.character(membership$statefips), membership$msafips)) {
    parts <- unique(parts)
    if (length(parts) > 1L) for (s in parts[-1L]) parent[[root(s)]] <- root(parts[1L])
  }
  setNames(vapply(states, root, character(1)), states)
}

prepare_county_geography <- function(input, data, level = "State", se_spec = "cluster_state") {
  get <- function(x) input[[paste0(tolower(level), "_", x)]]
  mode <- get("adjustment_mode")
  if (is.null(mode)) mode <- "geography"
  if (!mode %in% c("geography", "human_capital", "both")) stop("Choose Geography, Schooling and college, or Geography + schooling and college.")
  has_geo <- mode != "human_capital"
  has_hc <- mode %in% c("human_capital", "both")
  year <- as.integer(input$year_modern)
  water_year <- NA_integer_
  geo_vars <- character()
  construction <- NA_character_
  if (has_geo) {
    water_year <- if (is.null(get("water_year"))) 1820L else as.integer(get("water_year"))
    transport <- get("transport_controls")
    if (any(!transport %in% c("water_1820", "railroads_1840", "railroads_1850", "railroads_1861")))
      stop("Unknown county transport control.")
    geo_vars <- unique(c(resolve_geo_controls(get("geo_controls"), get("water_controls"), water_year), transport))
    if (!length(geo_vars)) stop("Select at least one county geography or transport control.")
    construction <- get("county_instrument")
    if (is.null(construction)) construction <- "area_population"
    if (!construction %in% c("area_population", "county_population")) stop("Choose a county population construction.")
  }
  static <- as.data.frame(get_state_adjustment_inputs())
  input_vars <- c(geo_vars, if (has_hc) c("avg_schooling_10_14", "college_share"))
  if (!all(input_vars %in% names(static)))
    stop("Selected county adjustment fields are missing from the data build.")
  county <- data.frame(county_id = numeric(), state_id = numeric(), joint_cluster = character())
  if (has_geo) {
    county <- as.data.frame(get_app_year("County", year))
    county$state_id <- floor(as.numeric(county$geofips) / 1000)
    county$county_id <- as.numeric(county$geofips)
    if (!all(input_vars %in% names(county))) stop("Selected county calibration fields are missing from the data build.")
  }
  beta <- 0
  if (has_hc) {
    if (!"simple_gamma" %in% names(static)) stop("The fixed micro schooling return is unavailable.")
    beta_values <- unique(as.numeric(static$simple_gamma[is.finite(static$simple_gamma)]))
    if (length(beta_values) != 1L) stop("The fixed micro schooling return is missing or inconsistent.")
    beta <- beta_values[[1L]]
  }
  # Canonical controls describe complete BEA units; DC uses source county data.
  if (has_geo) {
    canonical <- match(county$county_id, static$geofips)
    found <- !is.na(canonical)
    county$area <- as.numeric(county$area_acre)
    county$area[found] <- static$ac[canonical[found]]
    for (v in input_vars) county[[v]][found] <- static[[v]][canonical[found]]
  }
  if (level == "State") {
    components <- static[static$statefips %in% data$state_id, , drop = FALSE]
    components$region_id <- components$statefips
    data$region_id <- data$state_id
  } else {
    membership <- get_msa_ch_membership()
    members <- membership[membership$msafips %in% data$msafips, , drop = FALSE]
    components <- members
    canonical_member <- match(members$geofips, static$geofips)
    for (v in input_vars) components[[v]] <- static[[v]][canonical_member]
    if (anyNA(canonical_member)) {
      fallback <- if (has_geo) county else as.data.frame(get_app_year("County", year))
      if (!all(input_vars %in% names(fallback))) stop("Selected county component fields are missing from the data build.")
      missing <- which(is.na(canonical_member))
      source <- match(members$geofips[missing], fallback$geofips)
      for (v in input_vars) components[[v]][missing] <- fallback[[v]][source]
    }
    components$region_id <- components$msafips
    data$region_id <- data$msafips
  }
  components$state_id <- as.numeric(components$statefips)
  components$county_id <- as.numeric(components$geofips)
  region_row <- match(components$region_id, data$region_id)
  components$employment <- vapply(seq_len(nrow(components)), function(i)
    as.numeric(data[[paste0("nc", components$ch_slot[i])]][region_row[i]]), numeric(1))
  components$area <- vapply(seq_len(nrow(components)), function(i)
    as.numeric(data[[paste0("ac", components$ch_slot[i])]][region_row[i]]), numeric(1))
  # Match every active slot to a county; extra or missing membership is an error.
  slots <- as.integer(sub("^nc", "", grep("^nc[0-9]+$", names(data), value = TRUE)))
  actual <- as.integer(table(factor(components$region_id, levels = data$region_id)))
  for (i in seq_len(nrow(data))) {
    mapped <- sort(components$ch_slot[components$region_id == data$region_id[i]])
    if (!identical(as.integer(mapped), seq_len(actual[i])))
      stop("County membership contains missing or repeated CH slots.")
    # Unused wide-form slots are padded with n=0 and a=1 in the build.
    tail <- slots[slots > actual[i]]
    if (isTRUE(data$ch_complete[i] == 1) && length(tail) && any(vapply(tail, function(j)
        !isTRUE(data[[paste0("nc", j)]][i] == 0 && data[[paste0("ac", j)]][i] == 1), logical(1))))
      stop("An active CH county slot is missing from the membership file.")
  }
  known_area <- is.finite(components$area)
  if (any(abs(components$area[known_area] - components$ac[known_area]) > 1e-7 * (1 + abs(components$ac[known_area]))))
    stop("County membership order does not match the CH land-area slots.")
  if (level == "State") {
    data$joint_cluster <- paste0("s", data$state_id)
    if (has_geo) county$joint_cluster <- paste0("s", county$state_id)
  } else if (se_spec == "cluster_state" && !has_geo) {
    data$joint_cluster <- paste0("s", data$state_id)
  } else if (se_spec == "cluster_state") {
    connected <- sca_state_components(membership)
    county$joint_cluster <- paste0("s", county$state_id)
    match_state <- match(as.character(county$state_id), names(connected))
    county$joint_cluster[!is.na(match_state)] <- paste0("s", connected[match_state[!is.na(match_state)]])
    component_cluster <- connected[as.character(components$state_id)]
    region_cluster <- vapply(split(component_cluster, components$region_id), function(x) {
      x <- unique(x)
      if (length(x) != 1L || anyNA(x)) stop("An interstate MSA spans inconsistent connected-state clusters.")
      unname(x)
    }, character(1))
    aggregate_cluster <- region_cluster[as.character(data$region_id)]
    if (anyNA(aggregate_cluster)) stop("A selected MSA has no state cluster.")
    data$joint_cluster <- paste0("s", aggregate_cluster)
  } else {
    # Every aggregate and the counties used to form it share one score cluster.
    data$joint_cluster <- if (se_spec == "cluster_instrument") paste0("i", data$clusterID) else paste0("m", data$msafips)
    if (has_geo) {
      county$joint_cluster <- paste0("c", county$county_id)
      cluster_membership <- get_msa_ch_cluster_membership()
      member_row <- match(county$county_id, cluster_membership$geofips)
      metro <- !is.na(member_row)
      county$joint_cluster[metro] <- paste0("m", cluster_membership$msafips[member_row[metro]])
      represented <- match(cluster_membership$msafips[member_row], data$msafips)
      county$joint_cluster[!is.na(represented)] <- data$joint_cluster[represented[!is.na(represented)]]
    }
  }
  if (has_geo) {
    prefix <- if (construction == "area_population") "AW" else "GG"
    suffix <- if (input$sample_scope == "states_only") "_s" else ""
    for (historical_year in unique(as.integer(c(get("sample_year"), get("iv_year"))))) {
      name <- paste0(prefix, "pop_", historical_year, suffix)
      if (!name %in% names(county)) stop(paste("County population input is missing:", name))
      valid <- is.finite(county[[name]]) & county[[name]] >= 0
      for (flag in c("miss", "gap")) {
        field <- paste0(prefix, flag, "_", historical_year, suffix)
        if (field %in% names(county)) valid <- valid & !is.na(county[[field]]) & county[[field]] == 0
      }
      field <- paste0(prefix, "valid_", historical_year, suffix)
      if (field %in% names(county)) valid <- valid & !is.na(county[[field]]) & county[[field]] == 1
      county <- county[valid, , drop = FALSE]
    }
    county$instrument <- as.numeric(county[[paste0(prefix, "pop_", get("iv_year"), suffix)]]) / 1000
    county$x <- log(as.numeric(county$employment_total) / county$area)
    county$ln_area <- log(county$area)
    county$y <- log(as.numeric(county$gcp_total) / as.numeric(county$employment_total))
  }
  if (has_hc) {
    if (has_geo) {
      county$schooling <- as.numeric(county$avg_schooling_10_14)
      county$college <- as.numeric(county$college_share)
    }
    components$schooling <- as.numeric(components$avg_schooling_10_14)
    components$college <- as.numeric(components$college_share)
  }
  if (has_geo) county <- scale_model_controls(county, geo_vars)
  components <- scale_model_controls(components, geo_vars)
  area <- has_geo && isTRUE(get("county_area"))
  list(data = data, county = county, components = components, geo_vars = geo_vars,
    component_vars = c(geo_vars, if (has_hc) c("schooling", "college")),
    construction = construction, water_year = water_year, mode = mode,
    settings = list(method = input$analysis_type, mode = mode, geo_vars = geo_vars,
      beta_school = beta, kappa_college = if (has_hc) .75 else 0, schooling_reference = 0,
      county_controls = if (area) "ln_area" else character(), county_fe = has_geo,
      se_spec = se_spec, county_instrument_label = if (has_geo) instrument_name(construction) else NA_character_))
}

state_ch_adjusted_result <- function(input, data, population_missing_n, transformed, hch_year = NULL, hch_settings = NULL) {
  prepared <- prepare_county_geography(input, data)
  warnings <- character()
  fit <- function(data, method) fit_state_adjusted_ch(data, prepared$county, prepared$components,
    modifyList(prepared$settings,
      list(method = if (method == "NLS_pilot") "IV" else method, pilot = method == "NLS_pilot")))
  model <- withCallingHandlers(
    if (!is.null(hch_settings)) fit_ch_with_historical_instrument(prepared$data, fit = fit,
      hch_year = hch_year, region_ids = prepared$data$statefips,
      instrument_method = hch_settings$instrument_method, gmm_steps = hch_settings$gmm_steps,
      method = input$analysis_type) else fit(prepared$data, input$analysis_type),
    warning = function(w) { warnings <<- unique(c(warnings, conditionMessage(w))); invokeRestart("muffleWarning") })
  model$adjustment_mode <- prepared$mode
  model_name <- switch(prepared$mode, both = "CH: geography + human capital", human_capital = "CH: Schooling and college", geography = "CH: county geography")
  excluded <- model$excluded_states
  if (nrow(excluded)) warnings <- c(warnings, paste("Incomplete county adjustment inputs exclude whole states:",
    paste(data$state_name[match(excluded$state_id, as.character(data$state_id))], collapse = ", ")))
  list(models = setNames(list(model), model_name), first_stage_models = list(), analysis_type = input$analysis_type,
    approach = "density", density_measure = "CH", analysis_level = "State", year_modern = as.integer(input$year_modern),
    state_ch_mode = "county_adjusted", adjustment_mode = prepared$mode, county_calibration_instrument = prepared$construction,
    county_calibration = model$county_calibration, sectors = 1L,
    sample_year = as.integer(input$state_sample_year), iv_year = as.integer(input$state_iv_year),
    instrument_type = input$state_instrument_type, instrument_form = transformed$form,
    log_nonpositive_n = transformed$nonpositive, overlap_pct = NULL,
    schooling_adj = 0L, apply_college_adj = FALSE, college_coeff = NULL, schooling_reference = NULL,
    mining_filter_active = FALSE, mining_threshold = NULL, county_msa_restriction = NULL,
    se_spec = "cluster_state", spatial_cutoff = NULL, fe_type = "No", sample_scope = input$sample_scope,
    controls = paste(prepared$geo_vars, collapse = ", "), controls_vec = character(), county_geo_vars = prepared$geo_vars,
    n_obs = nobs(model), eligible_n = nrow(data), geo_missing_n = nrow(excluded),
    ch_missing_n = sum(is.na(data$ch_complete) | data$ch_complete != 1), population_missing_n = population_missing_n,
    model_n = setNames(nobs(model), model_name), model_clusters = setNames(model$clusters, model_name),
    model_warnings = warnings, geo_controls = if (prepared$mode != "human_capital") input$state_geo_controls else character(),
    water_controls = if (prepared$mode != "human_capital") input$state_water_controls else character(),
    water_year = prepared$water_year, new_controls = prepared$geo_vars,
    cluster_data = list(clusterID = data$clusterID, state_id = data$state_id), data_for_fs = data, fe_part = "0", vcov_arg = "hetero")
}
