source("tests/setup.R")
defaults$fe_type <- "modern"
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
  stopifnot('fit_RHS' %in% rownames(tab))
  first <- result$first_stage_models[[name]]
  fs <- fixest::coeftable(first)
  c(coefficient = tab['fit_RHS', 1], se = tab['fit_RHS', 2], n = nobs(model),
    first_stage_f = if ('instrument' %in% rownames(fs)) fs['instrument', 3]^2 else NA_real_)
}
years <- c(1790, 1870, 1880, 1890, 1900)
methods <- c(GG = 'county_population', AW = 'area_population')
rows <- list()
failures <- list()
compare_years <- function(level, scope = 'states_only') {
  data_key <- if (level == 'MSA') 'msa_data' else 'county_data'
  id <- if (level == 'MSA') 'msafips' else 'geofips'
  prefix <- tolower(level)
  grid <- expand.grid(iv_year = years, method = names(methods), stringsAsFactors = FALSE)
  settings <- lapply(seq_len(nrow(grid)), function(i) {
    options <- list(analysis_level = level, sample_scope = scope)
    options[[paste0(prefix, '_sample_year')]] <- as.character(grid$iv_year[i])
    options[[paste0(prefix, '_iv_year')]] <- as.character(grid$iv_year[i])
    options[[paste0(prefix, '_instrument_type')]] <- methods[[grid$method[i]]]
    options
  })
  on.exit({app[[data_key]] <- original[[level]]}, add = TRUE)
  app[[data_key]] <- original[[level]]
  native <- lapply(settings, run)
  for (name in names(native[[1]]$models)) {
    # The common sample is shared by all five years and both constructions.
    ids <- Reduce(intersect, lapply(native, model_ids, name = name))
    if (length(ids) < 5) stop(paste('Insufficient common sample:', level, name, length(ids)))
    app[[data_key]] <- original[[level]][original[[level]][[id]] %in% ids, ]
    common <- lapply(settings, run)
    reference <- common[[1]]
    fields <- c(id, 'RHS', 'LHS', 'LHS_adj1', 'LHS_adj2', reference$controls_vec, reference$fe_part)
    a <- reference$data_for_fs[order(reference$data_for_fs[[id]]), fields]
    for (result in common) {
      stopifnot(identical(sort(model_ids(reference, name)), sort(model_ids(result, name))))
      b <- result$data_for_fs[order(result$data_for_fs[[id]]), fields]
      stopifnot(isTRUE(all.equal(a, b, check.attributes = FALSE)))
    }
    for (sample in c('native', 'common_all_years_methods')) {
      estimates <- if (sample == 'native') native else common
      for (i in seq_along(estimates)) {
        value <- extract(estimates[[i]], name)
        baseline_i <- which(grid$method == grid$method[i] & grid$iv_year == 1790)
        baseline <- extract(estimates[[baseline_i]], name)
        rows[[length(rows) + 1L]] <<- data.frame(level, scope, adjustment = name, sample,
          method = grid$method[i], iv_year = grid$iv_year[i],
          coefficient = value['coefficient'], se = value['se'], n = value['n'],
          first_stage_f = value['first_stage_f'],
          delta_from_1790 = value['coefficient'] - baseline['coefficient'], row.names = NULL)
      }
    }
    app[[data_key]] <- original[[level]]
  }
  cat('Compared instrument years:', level, scope, '\n')
  flush.console()
}
for (level in c('MSA', 'County')) for (scope in c('states_only', 'states_territories')) {
  tryCatch(compare_years(level, scope), error = function(error) {
    failures[[length(failures) + 1L]] <<- list(level = level, scope = scope, error = conditionMessage(error))
    cat('FAILED', level, scope, conditionMessage(error), '\n')
  })
}
results <- dplyr::bind_rows(rows)
jsonlite::write_json(list(modern_year = 2010, fixed_effects = 'modern state',
  results = results, failures = failures), file.path(work, 'instrument_year_comparison.json'),
  pretty = TRUE, digits = 15, auto_unbox = TRUE, na = 'null')
print(results, row.names = FALSE)
cat('RESULT_ROWS', nrow(results), 'FAILURES', length(failures), '\n')

stopifnot(length(failures) == 0L, nrow(results) > 0L)
