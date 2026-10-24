# Exercise the production State routes without loading a County or MSA panel.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
app <- new.env(parent = globalenv())
for (expr in parse("app.R", encoding = "UTF-8")) {
  if (is.call(expr) && as.character(expr[[1]]) %in% c("tryCatch", "shinyApp")) next
  eval(expr, app)
}
for (generic in c("coef", "vcov", "nobs", "residuals", "fitted", "confint"))
  registerS3method(generic, "ch_model", app[[paste0(generic, ".ch_model")]], envir = asNamespace("stats"))
loader <- app$make_app_data_loader()
app$state_data <- loader$get_state()
app$get_app_year <- function(...) stop("The prediction must not load a full County or MSA panel.")
settings <- list(analysis_level = "State", year_modern = "2010", analysis_type = "IV",
  state_ch_mode = "standard", state_sample_year = "1790", state_iv_year = "1900",
  state_instrument_type = "historical_ch", state_hch_coverage = "80", sample_scope = "states_only",
  state_hch_variant = "historical", state_hch_density_basis = "source", instrument_form = "levels",
  state_hch_prediction_geo_controls = c("terrain", "crop", "climate"),
  state_hch_prediction_water_controls = c("shoreline", "historical_access", "distance", "portage"),
  state_hch_prediction_transport_controls = c("water_1820", "railroads_1840"),
  state_hch_prediction_water_year = "1820", show_map = FALSE, use_fe = FALSE)

baseline <- app$state_ch_result(settings, app$state_data)
stopifnot(baseline$state_hch_variant == "historical", baseline$state_hch_density_basis == "source",
  !length(baseline$hch_prediction_geo_vars), all(is.na(vapply(baseline$hch_prediction_diagnostics,
    function(x) x$diagnostics$nobs, integer(1)))))

results <- list()
for (basis in c("source", "modern")) for (variant in c("historical", "a", "b", "ab")) {
  chosen <- modifyList(settings, list(state_hch_variant = variant, state_hch_density_basis = basis))
  result <- app$state_ch_result(chosen, app$state_data)
  stopifnot(result$state_hch_variant == variant, result$state_hch_density_basis == basis,
    is.finite(coef(result$models[[1]])["ch_elasticity"]))
  sample <- result$hch_prediction_diagnostics[["1790"]]$states
  instrument <- result$hch_prediction_diagnostics[["1900"]]$states
  expected <- intersect(sample$statefips[sample$keep], instrument$statefips[instrument$keep])
  stopifnot(setequal(result$data_for_fs$statefips, expected),
    isTRUE(all.equal(result$data_for_fs$instrument,
      instrument$index[match(result$data_for_fs$statefips, instrument$statefips)], tolerance = 1e-12)))
  if (variant != "historical") {
    stopifnot(length(result$hch_prediction_geo_vars) == 22L,
      result$hch_prediction_diagnostics[["1900"]]$diagnostics$nobs > 2000L,
      grepl("conditional", app$hch_result_metadata(result)[["Prediction inference"]], fixed = TRUE))
  }
  results[[paste(basis, variant)]] <- result
}
for (basis in c("source", "modern")) {
  a <- results[[paste(basis, "a")]]$hch_prediction_diagnostics
  for (variant in c("b", "ab")) {
    b <- results[[paste(basis, variant)]]$hch_prediction_diagnostics
    for (year in names(a)) stopifnot(identical(a[[year]]$counties$support, b[[year]]$counties$support),
      isTRUE(all.equal(a[[year]]$diagnostics$coefficients, b[[year]]$diagnostics$coefficients)))
  }
}
# Changing final region support or coverage cannot refit the national prediction.
changed <- app$state_ch_result(modifyList(settings, list(state_hch_variant = "ab",
  state_hch_density_basis = "modern", state_sample_year = "1800", state_hch_coverage = "none")),
  app$state_data[app$state_data$statefips != 36, , drop = FALSE])
stopifnot(identical(changed$hch_prediction_diagnostics[["1900"]]$diagnostics$coefficients,
  results[["modern ab"]]$hch_prediction_diagnostics[["1900"]]$diagnostics$coefficients))

shiny::testServer(app$server, {
  do.call(session$setInputs, modifyList(settings, list(state_hch_variant = "ab", state_hch_density_basis = "modern")))
  session$setInputs(run_analysis = 1L)
  result <- isolate(analysis_output())
  if (!is.null(result$error)) stop(result$error)
  stopifnot(result$state_hch_variant == "ab", result$state_hch_density_basis == "modern",
    grepl("Predicted density and weights", output$analysis_details$html, fixed = TRUE),
    grepl("conditional on the fitted county prediction", output$results_table$html, fixed = TRUE))
  coefficients <- read.csv(output$download_coefficients, stringsAsFactors = FALSE)
  diagnostics <- read.csv(output$download_hch_diagnostics, stringsAsFactors = FALSE)
  stopifnot(all(coefficients$historical_ch_variant == "ab"),
    all(coefficients$historical_ch_density_basis == "modern"),
    setequal(diagnostics$historical_year, c(1790L, 1900L)),
    setequal(diagnostics$record_type, c("state", "prediction_coefficient")),
    all(diagnostics$historical_ch_variant == "ab"))
  session$setInputs(state_hch_variant = "historical", state_hch_density_basis = "source")
  stopifnot(isolate(analysis_output())$state_hch_variant == "ab")
  coefficients <- read.csv(output$download_coefficients, stringsAsFactors = FALSE)
  stopifnot(all(coefficients$historical_ch_variant == "ab"))
})

# The same selected instrument reaches a county-adjusted State CH specification.
hc <- app$state_ch_result(modifyList(settings, list(state_hch_variant = "a", state_hch_density_basis = "modern",
  state_ch_mode = "county_adjusted", state_adjustment_mode = "human_capital")), app$state_data)
stopifnot(hc$state_ch_mode == "county_adjusted", hc$state_hch_variant == "a",
  identical(hc$data_for_fs$instrument, results[["modern a"]]$data_for_fs$instrument))
cat("Historical CH prediction interface passed: eight constructions, both historical years, national training, stored configuration, exports and adjusted CH.\n")
