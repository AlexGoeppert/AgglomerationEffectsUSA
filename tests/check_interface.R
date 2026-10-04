# Check the interface and switching without loading the production datasets.
library(shiny)
library(glue)
local({
  app <- new.env(parent = globalenv())
  for (expr in parse("app.R")) {
    if (!is.call(expr) || !identical(expr[[1]], as.name("<-"))) next
    name <- as.character(expr[[2]])
    function_definition <- is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))
    if (function_definition || grepl("^(help_|historical_census_years$|geographic_notes$|area_population_omissions$)", name)) eval(expr, app)
  }
  markup <- lapply(c("MSA", "County"), function(level)
    htmltools::renderTags(tagList(app$geography_settings_ui(level), app$geography_transport_ui(level)))$html)
  names(markup) <- c("MSA", "County")
  select_markup <- function(html, id) {
    hit <- regmatches(html, regexec(paste0('<select[^>]*id="', id, '"[^>]*>[\\s\\S]*?</select>'), html, perl = TRUE))[[1]]
    stopifnot(length(hit) == 1L)
    hit
  }
  option_values <- function(html, id) {
    tags <- regmatches(select_markup(html, id), gregexpr('<option[^>]*>', select_markup(html, id), perl = TRUE))[[1]]
    sub('.*value="([^"]+)".*', '\\1', tags)
  }
  selected_value <- function(html, id) {
    tags <- regmatches(select_markup(html, id), gregexpr('<option[^>]*>', select_markup(html, id), perl = TRUE))[[1]]
    sub('.*value="([^"]+)".*', '\\1', tags[grepl("selected", tags, fixed = TRUE)])
  }
  input_tag <- function(html, id) {
    hit <- regmatches(html, regexec(paste0('<input[^>]*id="', id, '"[^>]*>'), html, perl = TRUE))[[1]]
    stopifnot(length(hit) == 1L)
    hit
  }
  for (level in names(markup)) {
    html <- markup[[level]]
    prefix <- paste0(tolower(level), "_")
    stopifnot(grepl("4. Geographic Unit Settings", html, fixed = TRUE),
      identical(selected_value(html, paste0(prefix, "schooling_adj")), "3"),
      grepl("checked", input_tag(html, paste0(prefix, "apply_college_adj")), fixed = TRUE),
      !grepl("checked", input_tag(html, paste0(prefix, "mining_filter_active")), fixed = TRUE),
      identical(option_values(html, paste0(prefix, "iv_year")), as.character(app$historical_census_years)),
      grepl("Max Density Overlap", html, fixed = TRUE), grepl("Minimum Overlap (%)", html, fixed = TRUE))
    transport <- regmatches(html, gregexpr('<input[^>]*name="[^\"]+_controls"[^>]*>', html, perl = TRUE))[[1]]
    checked <- sub('.*value="([^"]+)".*', '\\1', transport[grepl("checked", transport, fixed = TRUE)])
    stopifnot(identical(checked, app$geography_settings_defaults()$controls))
    ids <- regmatches(html, gregexpr('<(?:input|select)[^>]*id="[^"]+"', html, perl = TRUE))[[1]]
    ids <- sub('.*id="([^"]+)"$', '\\1', ids)
    shared <- c("density_measure", "sectors", "sample_year", "iv_year", "instrument_type",
      if (level == "MSA") "overlap_pct" else "overlap_threshold", "schooling_adj", "apply_college_adj",
      "college_coeff", "mining_filter_active", "mining_threshold", "se_spec", "spatial_cutoff")
    stopifnot(identical(ids[seq_along(shared)], paste0(prefix, shared)))
  }
  stopifnot(identical(option_values(markup$County, "county_density_measure"), "average"),
    identical(option_values(markup$MSA, "msa_density_measure"), c("average", "CH")),
    !"weighted_density_overlap" %in% option_values(markup$MSA, "msa_instrument_type"),
    "weighted_density_overlap" %in% option_values(markup$County, "county_instrument_type"),
    grepl("historical county&#39;s area", markup$MSA, fixed = TRUE) || grepl("historical county's area", markup$MSA, fixed = TRUE),
    grepl("modern county&#39;s area", markup$County, fixed = TRUE) || grepl("modern county's area", markup$County, fixed = TRUE))

  settings <- list(analysis_level = "MSA", approach = "density", analysis_type = "IV",
    msa_density_measure = "average", county_density_measure = "average",
    msa_instrument_type = "overlap", county_instrument_type = "max_density_overlap",
    msa_overlap_pct = "20", county_overlap_threshold = "5", county_msa_restriction = TRUE)
  for (level in c("msa", "county")) {
    values <- app$geography_settings_defaults()
    values$overlap <- NULL
    names(values) <- paste0(level, "_", names(values))
    settings <- c(settings, values)
  }
  settings$msa_se_spec <- "spatial"
  settings$msa_sectors <- "3"
  settings$msa_iv_year <- "1900"
  settings$msa_schooling_adj <- "2"
  settings$msa_controls <- character()
  settings$msa_college_coeff <- "0.6"
  plan <- app$geography_switch_plan("MSA", "County", settings)
  stopifnot(plan$updates$county_sectors == "3", plan$updates$county_iv_year == "1900",
    plan$updates$county_schooling_adj == "2", plan$updates$county_college_coeff == "0.6",
    plan$updates$county_overlap_threshold == "20", plan$updates$county_se_spec == "spatial",
    identical(plan$updates$county_controls, character()), plan$updates$county_instrument_type == "max_density_overlap",
    !"county_msa_restriction" %in% names(plan$updates), !"county_density_measure" %in% names(plan$updates),
    length(app$geography_switch_plan("MSA", "State", settings)$updates) == 0L)
  for (method in c("county_population", "area_population")) {
    settings$county_instrument_type <- method
    stopifnot(app$geography_switch_plan("County", "MSA", settings)$updates$msa_instrument_type == method)
  }
  settings$county_instrument_type <- "weighted_density_overlap"
  retained <- app$geography_switch_plan("County", "MSA", settings)
  stopifnot(!"msa_instrument_type" %in% names(retained$updates), length(retained$messages) == 1L,
    grepl("MSA keeps Max Density Overlap", retained$messages, fixed = TRUE))
  settings$msa_density_measure <- "CH"
  settings$county_se_spec <- "spatial"
  fallback <- app$geography_switch_plan("County", "MSA", settings)
  stopifnot(fallback$updates$msa_se_spec == "robust", any(grepl("Conley", fallback$messages, fixed = TRUE)),
    !"msa_density_measure" %in% names(fallback$updates))

  # Record the messages sent to the browser by the actual observers.
  sent <- list()
  notifications <- character()
  app$showNotification <- function(ui, duration, session) notifications <<- c(notifications, as.character(ui))
  test_server <- function(input, output, session) {
    session$sendInputMessage <- function(inputId, message) sent[[inputId]] <<- message
    app$observe_geography_settings(input, session)
  }
  settings$msa_density_measure <- "average"
  settings$county_instrument_type <- "max_density_overlap"
  shiny::testServer(test_server, {
    do.call(session$setInputs, settings)
    session$setInputs(analysis_type = "First-stage Regression")
    session$setInputs(analysis_level = "State")
    stopifnot(sent$msa_se_spec$value == "spatial", grepl('value="spatial"', sent$msa_se_spec$options, fixed = TRUE),
      sent$analysis_type$value == "IV", any(grepl("Ciccone–Hall model offers IV", notifications, fixed = TRUE)))
    session$setInputs(analysis_level = "MSA", analysis_type = "IV")
    stopifnot(sent$msa_se_spec$value == "spatial", sent$analysis_type$value == "First-stage Regression")
    session$setInputs(analysis_level = "County")
    stopifnot(sent$county_sectors$value == "3", sent$county_schooling_adj$value == "2",
      sent$county_instrument_type$value == "max_density_overlap", sent$county_overlap_threshold$value == "20",
      identical(sent$county_controls$value, character()))
    # Browser updates are represented explicitly in this test session.
    session$setInputs(county_instrument_type = "weighted_density_overlap", msa_instrument_type = "area_population")
    session$setInputs(analysis_level = "MSA")
    stopifnot(any(grepl("MSA keeps Area-weighted population", notifications, fixed = TRUE)))
    session$setInputs(msa_density_measure = "CH", msa_se_spec = "spatial")
    stopifnot(sent$msa_se_spec$value == "robust", !grepl('value="spatial"', sent$msa_se_spec$options, fixed = TRUE),
      any(grepl("Conley standard errors are not implemented", notifications, fixed = TRUE)))
    session$setInputs(analysis_level = "County", analysis_type = "First-stage Regression", msa_se_spec = "robust")
    stopifnot(grepl("First-stage Regression", sent$analysis_type$options, fixed = TRUE),
      any(grepl("County uses average employment density", notifications, fixed = TRUE)))
    session$setInputs(approach = "employment")
    stopifnot(sent$county_instrument_type$value == "max_density_overlap",
      !grepl("weighted_density_overlap", sent$county_instrument_type$options, fixed = TRUE),
      any(grepl("available for Employment density only", notifications, fixed = TRUE)))
  })
  cat("Interface checks passed: common County/MSA controls and defaults, preserved settings, explicit model exceptions, and State/Conley switching.\n")
})
