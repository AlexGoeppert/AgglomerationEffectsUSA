# Check the interface and switching without loading the production datasets.
library(shiny)
library(glue)
library(shinycssloaders)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
options(encoding = "UTF-8")
local({
  app <- new.env(parent = globalenv())
  source("pdf_help.R", local = app, encoding = "UTF-8")
  parsed <- parse("app.R", encoding = "UTF-8")
  for (expr in parsed) {
    if (!is.call(expr) || !identical(expr[[1]], as.name("<-"))) next
    name <- as.character(expr[[2]])
    function_definition <- is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))
    if (function_definition || grepl("^(help_|historical_census_years$|geographic_notes$|area_population_omissions$)", name)) eval(expr, app)
  }
  # The shared MSA settings now include the county geography CH controls.
  # Defining this interface loads no production data or estimation engine.
  source("state_ch_adjustment_interface.R", local = app, encoding = "UTF-8")
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
  radio_tags <- function(html, id) {
    tags <- regmatches(html, gregexpr(paste0('<input(?=[^>]*name="', id, '")(?=[^>]*type="radio")[^>]*>'), html, perl = TRUE))[[1]]
    stopifnot(length(tags) >= 1L)
    tags
  }
  radio_values <- function(html, id) sub('.*value="([^"]+)".*', '\\1', radio_tags(html, id))
  radio_selected <- function(html, id) {
    tags <- radio_tags(html, id)
    sub('.*value="([^"]+)".*', '\\1', tags[grepl("checked", tags, fixed = TRUE)])
  }
  bound_ids <- function(html) {
    tags <- regmatches(html, gregexpr('<(?:input|select|textarea)[^>]*id="[^"]+"', html, perl = TRUE))[[1]]
    sub('.*id="([^"]+)"$', '\\1', tags)
  }
  # All levels use one navigation row. Changing a tab only hides controls;
  # no setting is recreated or duplicated when moving between sections.
  ui <- Filter(function(expr) is.call(expr) && identical(expr[[1]], as.name("<-")) &&
    identical(expr[[2]], as.name("ui")), parsed)
  stopifnot(length(ui) == 1L)
  eval(ui[[1]], app)
  full_html <- htmltools::renderTags(app$ui)$html
  nav <- regmatches(full_html, regexec('<ul[^>]*id="settings_section"[^>]*>[\\s\\S]*?</ul>', full_html, perl = TRUE))[[1]]
  stopifnot(length(nav) == 1L)
  tabs <- regmatches(nav, gregexpr('<a[^>]*data-value="[^"]+"[^>]*>', nav, perl = TRUE))[[1]]
  stopifnot(identical(sub('.*data-value="([^"]+)".*', '\\1', tabs),
    c("model", "instrument", "controls", "inference", "display")),
    grepl('class="active"[^>]*>[\\s\\S]*?data-value="model"', nav, perl = TRUE),
    identical(radio_values(full_html, "analysis_level"), c("County", "MSA", "State")),
    identical(radio_selected(full_html, "analysis_level"), "MSA"),
    identical(selected_value(full_html, "year_modern"), "2010"),
    regexpr('id="run_analysis"', full_html, fixed = TRUE)[1] < regexpr('id="settings_section"', full_html, fixed = TRUE)[1],
    regexpr('id="year_modern"', full_html, fixed = TRUE)[1] < regexpr('id="settings_section"', full_html, fixed = TRUE)[1],
    !grepl('class="help-block"', full_html, fixed = TRUE),
    !grepl('class="geo-control-note"', full_html, fixed = TRUE),
    !grepl('class="data-notes-panel"', full_html, fixed = TRUE))
  all_ids <- regmatches(full_html, gregexpr(' id="[^"]+"', full_html, perl = TRUE))[[1]]
  if (anyDuplicated(all_ids)) stop("Repeated UI id: ", paste(unique(all_ids[duplicated(all_ids)]), collapse = ", "))
  for (level in names(markup)) {
    html <- markup[[level]]
    prefix <- paste0(tolower(level), "_")
    stopifnot(identical(selected_value(html, paste0(prefix, "schooling_adj")), "3"),
      grepl("checked", input_tag(html, paste0(prefix, "apply_college_adj")), fixed = TRUE),
      !grepl("checked", input_tag(html, paste0(prefix, "mining_filter_active")), fixed = TRUE),
      identical(option_values(html, paste0(prefix, "iv_year")), as.character(app$historical_census_years)),
      grepl("Max Density Overlap", html, fixed = TRUE), grepl("Minimum Overlap (%)", html, fixed = TRUE))
    transport <- regmatches(html, gregexpr(paste0('<input[^>]*name="', prefix, 'controls"[^>]*>'), html, perl = TRUE))[[1]]
    checked <- sub('.*value="([^"]+)".*', '\\1', transport[grepl("checked", transport, fixed = TRUE)])
    stopifnot(identical(checked, app$geography_settings_defaults()$controls))
    ids <- bound_ids(html)
    shared <- c("density_measure", "sectors", "sample_year", "iv_year", "instrument_type",
      if (level == "MSA") "overlap_pct" else "overlap_threshold", "schooling_adj", "apply_college_adj",
      "college_coeff", "mining_filter_active", "mining_threshold", "se_spec", "spatial_cutoff")
    stopifnot(setequal(ids[ids %in% paste0(prefix, shared)], paste0(prefix, shared)), !anyDuplicated(ids))
    sections <- lapply(c("model", "instrument", "controls", "inference"), function(section)
      htmltools::renderTags(app$geography_settings_ui(level, section = section))$html)
    names(sections) <- c("model", "instrument", "controls", "inference")
    expected <- list(model = c("density_measure", "sectors"),
      instrument = c("sample_year", "iv_year", "instrument_type", if (level == "MSA") "overlap_pct" else "overlap_threshold"),
      controls = c("schooling_adj", "apply_college_adj", "college_coeff", "mining_filter_active", "mining_threshold"),
      inference = c("se_spec", "spatial_cutoff"))
    for (section in names(expected)) {
      own <- paste0(prefix, expected[[section]])
      other <- paste0(prefix, unlist(expected[names(expected) != section], use.names = FALSE))
      stopifnot(all(own %in% bound_ids(sections[[section]])), !any(other %in% bound_ids(sections[[section]])))
    }
    section_ids <- unlist(lapply(sections, bound_ids), use.names = FALSE)
    stopifnot(!anyDuplicated(section_ids), setequal(section_ids, bound_ids(htmltools::renderTags(app$geography_settings_ui(level))$html)),
      grepl(paste0("input.", prefix, "apply_college_adj"), sections$controls, fixed = TRUE),
      grepl(paste0("input.", prefix, "mining_filter_active"), sections$controls, fixed = TRUE))
  }
  # Check the additional CH controls explicitly at both aggregate levels.
  for (level in c("MSA", "State")) {
    html <- if (level == "MSA") markup$MSA else htmltools::renderTags(app$state_adjustment_settings_ui(level))$html
    prefix <- paste0(tolower(level), "_")
    stopifnot(identical(radio_values(html, paste0(prefix, "ch_mode")), c("standard", "county_adjusted")),
      identical(radio_selected(html, paste0(prefix, "ch_mode")), "standard"),
      identical(option_values(html, paste0(prefix, "county_instrument")), c("area_population", "county_population")),
      identical(selected_value(html, paste0(prefix, "county_instrument")), "area_population"),
      !grepl("checked", input_tag(html, paste0(prefix, "county_area")), fixed = TRUE),
      identical(selected_value(html, paste0(prefix, "water_year")), "1820"),
      grepl(paste0('id="', prefix, 'adjustment_mode"'), html, fixed = TRUE),
      identical(radio_values(html, paste0(prefix, "adjustment_mode")), c("geography", "both")),
      identical(radio_selected(html, paste0(prefix, "adjustment_mode")), "geography"),
      grepl("Geography + schooling and college", html, fixed = TRUE),
      !grepl(paste0('id="', prefix, 'schooling_efficiency"'), html, fixed = TRUE))
    index_model <- htmltools::renderTags(app$state_adjustment_settings_ui(level, section = "model"))$html
    index_controls <- htmltools::renderTags(app$state_adjustment_settings_ui(level, section = "controls"))$html
    stopifnot(identical(radio_values(index_model, paste0(prefix, "ch_mode")), c("standard", "county_adjusted")),
      identical(radio_values(index_model, paste0(prefix, "adjustment_mode")), c("geography", "both")),
      !grepl(paste0('id="', prefix, 'county_instrument"'), index_model, fixed = TRUE),
      !grepl(paste0('id="', prefix, 'ch_mode"'), index_controls, fixed = TRUE),
      grepl(paste0('id="', prefix, 'county_instrument"'), index_controls, fixed = TRUE),
      !grepl("<p", paste(index_model, index_controls), fixed = TRUE))
    groups <- list(geo_controls = c("terrain", "crop", "climate"),
      water_controls = c("shoreline", "historical_access", "distance", "portage"),
      transport_controls = c("water_1820", "railroads_1840"))
    for (group in names(groups)) {
      tags <- regmatches(html, gregexpr(paste0('<input[^>]*name="', prefix, group, '"[^>]*>'), html, perl = TRUE))[[1]]
      checked <- sub('.*value="([^"]+)".*', '\\1', tags[grepl("checked", tags, fixed = TRUE)])
      stopifnot(identical(checked, groups[[group]]))
    }
  }
  stopifnot(identical(option_values(markup$County, "county_density_measure"), "average"),
    identical(option_values(markup$MSA, "msa_density_measure"), c("average", "CH")),
    !"weighted_density_overlap" %in% option_values(markup$MSA, "msa_instrument_type"),
    "weighted_density_overlap" %in% option_values(markup$County, "county_instrument_type"))

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
  stopifnot(grepl("input.msa_adjustment_mode", markup$MSA, fixed = TRUE))
  cat("Interface checks passed: shared navigation, visible CH choices, unique controls, preserved defaults/settings, explicit model exceptions, and State/Conley switching.\n")
})
