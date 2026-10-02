source("tests/setup.R")
run <- function(settings) {
  result <- NULL
  shiny::testServer(app$server, {
    do.call(session$setInputs, c(modifyList(defaults, settings), list(run_analysis = 1)))
    session$flushReact()
    result <<- isolate(analysis_output())
  })
  if (!is.null(result$error)) stop(result$error)
  result
}
model_ids <- function(result, name) {
  id <- if (result$analysis_level == 'MSA') 'msafips' else 'geofips'
  result$data_for_fs[[id]][app$app_model_rows(result$models[[name]])]
}
extract <- function(result, name) {
  model <- result$models[[name]]
  tab <- app$app_model_table(model)
  term <- if (inherits(model, 'ch_model')) 'ch_elasticity' else 'fit_RHS'
  if (!term %in% rownames(tab)) stop(paste('Missing term', term, paste(rownames(tab), collapse = ', ')))
  first <- result$first_stage_models[[name]]
  fs <- if (!is.null(first)) fixest::coeftable(first) else NULL
  f <- if (!is.null(fs) && 'instrument' %in% rownames(fs)) fs['instrument', 3]^2 else NA_real_
  c(coefficient = tab[term, 1], se = tab[term, 2], n = nobs(model), first_stage_f = f)
}
rows <- list()
failures <- list()
compare <- function(level, iv_year, scope = 'states_only', extra = list(), design = 'default') {
  data_key <- if (level == 'MSA') 'msa_data' else 'county_data'
  id <- if (level == 'MSA') 'msafips' else 'geofips'
  type_key <- if (level == 'MSA') 'msa_instrument_type' else 'county_instrument_type'
  year_key <- if (level == 'MSA') 'msa_iv_year' else 'county_iv_year'
  settings <- modifyList(list(analysis_level = level, sample_scope = scope), extra)
  settings[[year_key]] <- as.character(iv_year)
  on.exit({app[[data_key]] <- original[[level]]}, add = TRUE)
  app[[data_key]] <- original[[level]]
  native <- lapply(c(GG = 'county_population', AW = 'area_population'), function(type) {
    configured <- settings
    configured[[type_key]] <- type
    run(configured)
  })
  for (name in names(native$GG$models)) {
    ids <- intersect(model_ids(native$GG, name), model_ids(native$AW, name))
    app[[data_key]] <- original[[level]][original[[level]][[id]] %in% ids, ]
    common <- lapply(c(GG = 'county_population', AW = 'area_population'), function(type) {
      configured <- settings
      configured[[type_key]] <- type
      run(configured)
    })
    stopifnot(identical(sort(model_ids(common$GG, name)), sort(model_ids(common$AW, name))))
    a <- common$GG$data_for_fs
    b <- common$AW$data_for_fs
    shared_fields <- c(id, 'RHS', 'LHS', 'LHS_adj1', 'LHS_adj2', common$GG$controls_vec, common$GG$fe_part)
    stopifnot(isTRUE(all.equal(a[shared_fields], b[shared_fields], check.attributes = FALSE)))
    for (sample in c('native', 'common')) {
      pair <- if (sample == 'native') native else common
      gg <- extract(pair$GG, name)
      aw <- extract(pair$AW, name)
      rows[[length(rows)+1L]] <<- data.frame(level, design, iv_year, scope, adjustment = name, sample,
        gg = gg['coefficient'], aw = aw['coefficient'], delta = aw['coefficient'] - gg['coefficient'],
        relative_percent = 100 * (aw['coefficient'] - gg['coefficient']) / abs(gg['coefficient']),
        gg_se = gg['se'], aw_se = aw['se'], gg_n = gg['n'], aw_n = aw['n'],
        gg_f = gg['first_stage_f'], aw_f = aw['first_stage_f'], row.names = NULL)
    }
    app[[data_key]] <- original[[level]]
  }
  cat('Compared', level, iv_year, scope, design, '\n')
  flush.console()
}
safe_compare <- function(...) {
  specification <- list(...)
  tryCatch(do.call(compare, specification), error = function(error) {
    failures[[length(failures)+1L]] <<- list(specification = specification, error = conditionMessage(error))
    cat('FAILED', conditionMessage(error), '\n')
  })
}
for (level in c('MSA', 'County')) for (year in seq(1870, 1900, 10)) {
  safe_compare(level = level, iv_year = year)
  sample_setting <- list()
  sample_setting[[if (level == 'MSA') 'msa_sample_year' else 'county_sample_year']] <- '1900'
  safe_compare(level = level, iv_year = year, extra = sample_setting, design = 'sample_1900')
}
for (year in seq(1870, 1900, 10)) {
  safe_compare(level = 'MSA', iv_year = year, scope = 'states_territories',
    extra = list(msa_sample_year = '1900'), design = 'sample_1900')
  safe_compare(level = 'MSA', iv_year = year,
    extra = list(msa_sample_year = '1900', geo_controls = c('terrain','crop','climate'),
      water_controls = c('shoreline','historical_access','distance','portage')),
    design = 'sample_1900_all_geographic_controls')
}
results <- dplyr::bind_rows(rows)
jsonlite::write_json(list(results = results, failures = failures), file.path(work, 'coefficient_comparison.json'), pretty = TRUE, digits = 15, auto_unbox = TRUE, na = 'null')
print(results, row.names = FALSE)
cat('RESULT_ROWS', nrow(results), 'FAILURES', length(failures), '\n')

stopifnot(length(failures) == 0L, nrow(results) > 0L)
